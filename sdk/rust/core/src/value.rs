//! The canonical data model (docs/specs/canonical-encoding.md section 1).
//!
//! Every invariant is held by a type: [`Rational`] is reduced and never an integer, [`Map`] has unique keys in
//! canonical order and no null values, and [`Array`] and [`Map`] never nest deeper than [`MAX_DEPTH`]. Their fields
//! are private, so every constructible [`Value`] has exactly one encoding and decodes back to itself.

use alloc::collections::BTreeSet;
use alloc::string::String;
use alloc::vec::Vec;
use core::cmp::Ordering;
use core::fmt;

use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Signed, Zero};

use crate::error::CanonicalError;

/// The nesting limit for arrays and maps: a scalar has depth 0, an array or map one more than its deepest element.
pub const MAX_DEPTH: usize = 256;

/// A canonical value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    /// `null`.
    Null,
    /// `false` or `true`.
    Bool(bool),
    /// An integer of any size.
    Int(BigInt),
    /// A non-integer rational in lowest terms.
    Rational(Rational),
    /// Text: Unicode scalar values (Rust strings hold no surrogates).
    Text(String),
    /// A byte string.
    Bytes(Vec<u8>),
    /// An array.
    Array(Array),
    /// A map with text keys.
    Map(Map),
}

impl Value {
    /// Nesting depth: 0 for scalars, one more than the deepest element for arrays and maps (1 when empty).
    pub fn depth(&self) -> usize {
        match self {
            Self::Array(a) => a.depth,
            Self::Map(m) => m.depth,
            _ => 0,
        }
    }

    /// The integer `n`.
    pub fn int(n: impl Into<BigInt>) -> Self {
        Self::Int(n.into())
    }

    /// The text `s`.
    pub fn text(s: impl Into<String>) -> Self {
        Self::Text(s.into())
    }

    /// The byte string `b`.
    pub fn bytes(b: impl Into<Vec<u8>>) -> Self {
        Self::Bytes(b.into())
    }

    /// The number `numerator / denominator` in the single numeric domain: an integer when it is one, otherwise a
    /// reduced [`Rational`].
    pub fn number(numerator: &BigInt, denominator: &BigInt) -> Result<Self, DivisionByZero> {
        Rational::reduce(numerator, denominator).map(|r| match r {
            Reduced::Integer(n) => Self::Int(n),
            Reduced::Fraction(r) => Self::Rational(r),
        })
    }

    /// An array of `items`, if within the depth limit.
    pub fn array(items: Vec<Self>) -> Result<Self, CanonicalError> {
        Array::new(items).map(Self::Array)
    }

    /// A map of `entries`, dropping null values; duplicate keys and excess depth are errors.
    pub fn map(entries: impl IntoIterator<Item = (String, Self)>) -> Result<Self, CanonicalError> {
        Map::new(entries).map(Self::Map)
    }

    /// The kind of value, for messages.
    pub const fn kind(&self) -> &'static str {
        match self {
            Self::Null => "null",
            Self::Bool(_) => "bool",
            Self::Int(_) => "int",
            Self::Rational(_) => "rational",
            Self::Text(_) => "text",
            Self::Bytes(_) => "bytes",
            Self::Array(_) => "array",
            Self::Map(_) => "map",
        }
    }
}

/// A zero denominator.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DivisionByZero;

impl fmt::Display for DivisionByZero {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("division by zero")
    }
}

impl core::error::Error for DivisionByZero {}

/// A rational `n/d` with `d >= 2` and `gcd(|n|, d) = 1`: never an integer.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rational {
    numerator: BigInt,
    denominator: BigInt,
}

pub(crate) enum Reduced {
    Integer(BigInt),
    Fraction(Rational),
}

impl Rational {
    /// `n/d` exactly as given, if it already satisfies the invariants (as a decoder must check).
    pub fn new(numerator: BigInt, denominator: BigInt) -> Option<Self> {
        let valid = denominator > BigInt::one() && numerator.gcd(&denominator).is_one();
        valid.then_some(Self { numerator, denominator })
    }

    pub(crate) fn reduce(numerator: &BigInt, denominator: &BigInt) -> Result<Reduced, DivisionByZero> {
        if denominator.is_zero() {
            return Err(DivisionByZero);
        }
        let g = numerator.gcd(denominator);
        let sign = if denominator.is_negative() { -BigInt::one() } else { BigInt::one() };
        // g >= 1 here: the denominator is non-zero, so the divisions are exact and cannot fail
        let n = numerator.div_floor(&g) * &sign;
        let d = denominator.div_floor(&g) * sign;
        Ok(if d.is_one() { Reduced::Integer(n) } else { Reduced::Fraction(Self { numerator: n, denominator: d }) })
    }

    /// The numerator, carrying the sign.
    pub const fn numerator(&self) -> &BigInt {
        &self.numerator
    }

    /// The denominator, at least 2.
    pub const fn denominator(&self) -> &BigInt {
        &self.denominator
    }
}

impl fmt::Display for Rational {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}

/// An array whose nesting is within [`MAX_DEPTH`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Array {
    items: Vec<Value>,
    depth: usize,
}

impl Array {
    /// The array of `items`, if within the depth limit.
    pub fn new(items: Vec<Value>) -> Result<Self, CanonicalError> {
        let depth = items.iter().map(Value::depth).max().unwrap_or(0).saturating_add(1);
        if depth > MAX_DEPTH {
            Err(CanonicalError::DepthExceeded { limit: MAX_DEPTH })
        } else {
            Ok(Self { items, depth })
        }
    }

    /// The elements, in order.
    pub fn items(&self) -> &[Value] {
        &self.items
    }

    /// The elements, by value.
    pub fn into_items(self) -> Vec<Value> {
        self.items
    }
}

/// A map with unique text keys, no null values, keys in canonical order and nesting within [`MAX_DEPTH`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Map {
    entries: Vec<(String, Value)>,
    depth: usize,
}

/// The order of keys in an encoded map: their encodings compared bytewise (RFC 8949 section 4.2.1). For text keys
/// that is shorter UTF-8 first, then bytewise, because the head encodes the length in increasing order.
pub fn canonical_key_order(a: &str, b: &str) -> Ordering {
    a.len().cmp(&b.len()).then_with(|| a.as_bytes().cmp(b.as_bytes()))
}

impl Map {
    /// The map of `entries`: null values are dropped, a repeated key (even with a null value) is
    /// [`CanonicalError::DuplicateKey`], and excess depth is [`CanonicalError::DepthExceeded`].
    pub fn new(entries: impl IntoIterator<Item = (String, Value)>) -> Result<Self, CanonicalError> {
        let mut seen: BTreeSet<String> = BTreeSet::new();
        let mut kept: Vec<(String, Value)> = Vec::new();
        for (k, v) in entries {
            if seen.contains(&k) {
                return Err(CanonicalError::DuplicateKey { key: k });
            }
            seen.insert(k.clone());
            if v != Value::Null {
                kept.push((k, v));
            }
        }
        kept.sort_by(|(a, _), (b, _)| canonical_key_order(a, b));
        Self::from_sorted(kept)
    }

    /// For entries already unique, non-null and in canonical order (the strict decoder checks all three).
    pub(crate) fn from_sorted(entries: Vec<(String, Value)>) -> Result<Self, CanonicalError> {
        let depth = entries.iter().map(|(_, v)| v.depth()).max().unwrap_or(0).saturating_add(1);
        if depth > MAX_DEPTH {
            Err(CanonicalError::DepthExceeded { limit: MAX_DEPTH })
        } else {
            Ok(Self { entries, depth })
        }
    }

    /// The value at `key`, if present.
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.entries
            .binary_search_by(|(k, _)| canonical_key_order(k, key))
            .ok()
            .and_then(|i| self.entries.get(i))
            .map(|(_, v)| v)
    }

    /// The entries in canonical key order.
    pub fn entries(&self) -> &[(String, Value)] {
        &self.entries
    }

    /// The entries by value, in canonical key order.
    pub fn into_entries(self) -> Vec<(String, Value)> {
        self.entries
    }

    /// The number of entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Whether the map has no entries.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}
