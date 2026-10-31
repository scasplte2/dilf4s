//! The canonical data model (docs/specs/canonical-encoding.md section 1).
//!
//! Every invariant is held by a type: [`Rational`] is reduced and never an integer, [`Map`] has unique keys in
//! canonical order and no null values, and [`Array`] and [`Map`] never nest deeper than [`MAX_DEPTH`]. Their fields
//! are private, so every constructible [`Value`] has exactly one encoding and decodes back to itself.
//!
//! # Sharing
//!
//! Values are immutable and share structure: every variable-size payload sits behind an [`Arc`], so cloning a
//! [`Value`] is O(1) and allocation-free, and a value may occur many times inside another without being copied. Each
//! value's [`weight`](Value::weight) and [`depth`](Value::depth) are computed once, at construction, from its children's
//! cached ones, so reading either is O(1). There is no interior mutability: once built, a value never changes.
//!
//! Sharing makes a value's logical size independent of its memory: `[x, x]` nested `k` deep holds `2^k` leaves in
//! `O(k)` allocations. Anything that walks a value's contents ([`crate::cbor::encode`], [`crate::json::render`],
//! equality, hashing, commitments) costs O(weight), not O(memory), and the weight is the logical, unshared measure
//! that bounds it. Pointer identity is only ever a fast path for equality: equal pointers are equal values, and every
//! other decision is made on content.
//!
//! # Not for secrets
//!
//! A [`Value`] must never hold secret material. Its buffers are shared and freed by whichever clone drops last, so they
//! cannot be reliably zeroized; secret keys, nonces and seeds belong in dedicated types (as in `dilf4s-signing`).

use alloc::collections::BTreeSet;
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::borrow::Borrow;
use core::cmp::Ordering;
use core::fmt;
use core::ops::Deref;

use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Signed, Zero};

use crate::error::CanonicalError;

/// The nesting limit for arrays and maps: a scalar has depth 0, an array or map one more than its deepest element.
///
/// Every recursive walk of a [`Value`] relies on it: dropping the last reference to a value, [`PartialEq`], [`fmt::Debug`],
/// the CBOR encoder and the JSON renderer recurse at most `MAX_DEPTH` (256) frames deep, never by input size.
pub const MAX_DEPTH: usize = 256;

/// The greatest [`Value::weight`]: weights saturate here (the Scala reference's `Long.MaxValue`), never wrap.
pub const MAX_WEIGHT: u64 = i64::MAX.unsigned_abs();

/// `a + b`, saturating at [`MAX_WEIGHT`].
fn add_weight(a: u64, b: u64) -> u64 {
    a.saturating_add(b).min(MAX_WEIGHT)
}

/// One node plus `len` units (bytes of text or of a byte string).
fn sized_weight(len: usize) -> u64 {
    add_weight(1, u64::try_from(len).unwrap_or(MAX_WEIGHT))
}

/// Java's `BigInteger.bitLength` (the Scala reference's): the bits of the minimal two's-complement form without its
/// sign bit, which for a negative `n` is the bit length of `|n| - 1`. That is one less than `|n|`'s exactly when `|n|`
/// is a power of two (`-2^63` has 63).
fn bit_length(n: &BigInt) -> u64 {
    let bits = n.bits();
    let negative_power_of_two = n.is_negative() && n.magnitude().trailing_zeros() == Some(bits.saturating_sub(1));
    if negative_power_of_two { bits.saturating_sub(1) } else { bits }
}

/// The whole 64-bit limbs of `n`, as the weight counts them: `bitLength / 64`.
fn limbs(n: &BigInt) -> u64 {
    bit_length(n) / 64
}

/// A canonical value.
///
/// Cloning is O(1): see the [module documentation](self). Dropping, comparing and formatting recurse at most
/// [`MAX_DEPTH`] deep.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    /// `null`.
    Null,
    /// `false` or `true`.
    Bool(bool),
    /// An integer of any size.
    Int(Int),
    /// A non-integer rational in lowest terms.
    Rational(Rational),
    /// Text: Unicode scalar values (Rust strings hold no surrogates).
    Text(Text),
    /// A byte string.
    Bytes(Arc<[u8]>),
    /// An array.
    Array(Array),
    /// A map with text keys.
    Map(Map),
}

// Shared values cross threads (and SP1 guests, which lower atomics): keep them `Send + Sync`.
const _: fn() = || {
    fn send_sync<T: Send + Sync>() {}
    send_sync::<Value>();
};

impl Value {
    /// Nesting depth: 0 for scalars, one more than the deepest element for arrays and maps (1 when empty). Cached.
    pub fn depth(&self) -> usize {
        match self {
            Self::Array(a) => a.depth,
            Self::Map(m) => m.depth,
            _ => 0,
        }
    }

    /// A portable measure of the value's logical size, as the Scala reference's `CanonicalValue.weight`: one per node,
    /// plus the UTF-8 length of each text and map key, the length of each byte string, and the whole 64-bit limbs of
    /// each number (`bitLength / 64`, numerator and denominator). A shared sub-value counts every time it occurs, so
    /// the weight bounds the work of any full traversal; it saturates at [`MAX_WEIGHT`]. O(1): cached at construction.
    pub fn weight(&self) -> u64 {
        match self {
            Self::Null | Self::Bool(_) => 1,
            Self::Int(n) => n.weight,
            Self::Rational(r) => r.0.weight,
            Self::Text(t) => sized_weight(t.len()),
            Self::Bytes(b) => sized_weight(b.len()),
            Self::Array(a) => a.weight,
            Self::Map(m) => m.weight,
        }
    }

    /// The integer `n`.
    pub fn int(n: impl Into<BigInt>) -> Self {
        Self::Int(Int::new(n))
    }

    /// The text `s`.
    pub fn text(s: impl Into<Text>) -> Self {
        Self::Text(s.into())
    }

    /// The byte string `b`.
    pub fn bytes(b: impl Into<Arc<[u8]>>) -> Self {
        Self::Bytes(b.into())
    }

    /// The number `numerator / denominator` in the single numeric domain: an integer when it is one, otherwise a
    /// reduced [`Rational`].
    pub fn number(numerator: &BigInt, denominator: &BigInt) -> Result<Self, DivisionByZero> {
        Rational::reduce(numerator, denominator).map(|r| match r {
            Reduced::Integer(n) => Self::int(n),
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

    /// The text, if this is text.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Self::Text(t) => Some(t),
            _ => None,
        }
    }

    /// The bytes, if this is a byte string.
    pub fn as_bytes(&self) -> Option<&[u8]> {
        match self {
            Self::Bytes(b) => Some(b),
            _ => None,
        }
    }

    /// The integer, if this is one.
    pub fn as_int(&self) -> Option<&BigInt> {
        match self {
            Self::Int(n) => Some(n),
            _ => None,
        }
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

// ------------------------------------------------------------------------------------------------ text

/// Shared, immutable text: derefs to [`str`], and clones in O(1).
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Text(Arc<str>);

impl Text {
    /// The text.
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl Deref for Text {
    type Target = str;
    fn deref(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for Text {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for Text {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl fmt::Debug for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&*self.0, f)
    }
}

impl fmt::Display for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&*self.0, f)
    }
}

impl PartialEq<str> for Text {
    fn eq(&self, other: &str) -> bool {
        *self.0 == *other
    }
}

impl PartialEq<&str> for Text {
    fn eq(&self, other: &&str) -> bool {
        *self.0 == **other
    }
}

impl PartialEq<String> for Text {
    fn eq(&self, other: &String) -> bool {
        *self.0 == **other
    }
}

impl PartialEq<Text> for str {
    fn eq(&self, other: &Text) -> bool {
        *self == *other.0
    }
}

impl PartialEq<Text> for &str {
    fn eq(&self, other: &Text) -> bool {
        **self == *other.0
    }
}

impl PartialEq<Text> for String {
    fn eq(&self, other: &Text) -> bool {
        **self == *other.0
    }
}

impl From<&str> for Text {
    fn from(s: &str) -> Self {
        Self(Arc::from(s))
    }
}

impl From<&String> for Text {
    fn from(s: &String) -> Self {
        Self(Arc::from(s.as_str()))
    }
}

impl From<String> for Text {
    fn from(s: String) -> Self {
        Self(Arc::from(s))
    }
}

impl From<Arc<str>> for Text {
    fn from(s: Arc<str>) -> Self {
        Self(s)
    }
}

impl From<Text> for Arc<str> {
    fn from(t: Text) -> Self {
        t.0
    }
}

// ------------------------------------------------------------------------------------------------ numbers

/// A shared, immutable integer: derefs to [`BigInt`], clones in O(1), and caches its weight.
#[derive(Clone)]
pub struct Int {
    value: Arc<BigInt>,
    weight: u64,
}

impl Int {
    /// The integer `n`.
    pub fn new(n: impl Into<BigInt>) -> Self {
        let value = n.into();
        let weight = add_weight(1, limbs(&value));
        Self { value: Arc::new(value), weight }
    }

    /// The integer.
    pub fn as_bigint(&self) -> &BigInt {
        &self.value
    }

    /// The integer by value; it is copied only when shared.
    pub fn into_bigint(self) -> BigInt {
        Arc::unwrap_or_clone(self.value)
    }

    /// `1 + bitLength / 64` (see [`Value::weight`]).
    pub const fn weight(&self) -> u64 {
        self.weight
    }
}

impl From<BigInt> for Int {
    fn from(n: BigInt) -> Self {
        Self::new(n)
    }
}

/// `T::try_from(&int)` as for `&BigInt`, so a matched `Value::Int(n)` converts directly.
macro_rules! try_from_int {
    ($($t:ty),*) => {$(
        impl TryFrom<&Int> for $t {
            type Error = num_bigint::TryFromBigIntError<()>;
            fn try_from(n: &Int) -> Result<Self, Self::Error> {
                Self::try_from(&*n.value)
            }
        }
    )*};
}

try_from_int!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

impl Deref for Int {
    type Target = BigInt;
    fn deref(&self) -> &BigInt {
        &self.value
    }
}

impl AsRef<BigInt> for Int {
    fn as_ref(&self) -> &BigInt {
        &self.value
    }
}

impl PartialEq for Int {
    fn eq(&self, other: &Self) -> bool {
        // `Arc<T: Eq>` compares pointers first, then content
        self.value == other.value
    }
}

impl Eq for Int {}

impl PartialEq<BigInt> for Int {
    fn eq(&self, other: &BigInt) -> bool {
        *self.value == *other
    }
}

impl PartialOrd for Int {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Int {
    fn cmp(&self, other: &Self) -> Ordering {
        self.value.cmp(&other.value)
    }
}

impl fmt::Debug for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&*self.value, f)
    }
}

impl fmt::Display for Int {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&*self.value, f)
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

/// A rational `n/d` with `d >= 2` and `gcd(|n|, d) = 1`: never an integer. Shared: clones in O(1).
#[derive(Clone, PartialEq, Eq)]
pub struct Rational(Arc<RationalParts>);

/// Equality is derived from the numerator and denominator; the weight follows from them.
#[derive(PartialEq, Eq)]
struct RationalParts {
    numerator: BigInt,
    denominator: BigInt,
    weight: u64,
}

pub(crate) enum Reduced {
    Integer(BigInt),
    Fraction(Rational),
}

impl Rational {
    /// The only way to build one: callers have checked the invariants.
    fn checked(numerator: BigInt, denominator: BigInt) -> Self {
        let weight = add_weight(add_weight(2, limbs(&numerator)), limbs(&denominator));
        Self(Arc::new(RationalParts { numerator, denominator, weight }))
    }

    /// `n/d` exactly as given, if it already satisfies the invariants (as a decoder must check).
    pub fn new(numerator: BigInt, denominator: BigInt) -> Option<Self> {
        let valid = denominator > BigInt::one() && numerator.gcd(&denominator).is_one();
        valid.then(|| Self::checked(numerator, denominator))
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
        Ok(if d.is_one() { Reduced::Integer(n) } else { Reduced::Fraction(Self::checked(n, d)) })
    }

    /// The numerator, carrying the sign.
    pub fn numerator(&self) -> &BigInt {
        &self.0.numerator
    }

    /// The denominator, at least 2.
    pub fn denominator(&self) -> &BigInt {
        &self.0.denominator
    }

    /// `2 + bitLength(n) / 64 + bitLength(d) / 64` (see [`Value::weight`]).
    pub fn weight(&self) -> u64 {
        self.0.weight
    }
}

impl fmt::Debug for Rational {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Rational")
            .field("numerator", self.numerator())
            .field("denominator", self.denominator())
            .finish()
    }
}

impl fmt::Display for Rational {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.numerator(), self.denominator())
    }
}

// ------------------------------------------------------------------------------------------------ containers

/// An array whose nesting is within [`MAX_DEPTH`]. Shared: clones in O(1); its depth and weight are cached.
#[derive(Clone)]
pub struct Array {
    items: Arc<[Value]>,
    depth: usize,
    weight: u64,
}

impl Array {
    /// The array of `items`, if within the depth limit.
    pub fn new(items: Vec<Value>) -> Result<Self, CanonicalError> {
        let depth = items.iter().map(Value::depth).max().unwrap_or(0).saturating_add(1);
        if depth > MAX_DEPTH {
            Err(CanonicalError::DepthExceeded { limit: MAX_DEPTH })
        } else {
            let weight = items.iter().fold(1, |w, v| add_weight(w, v.weight()));
            Ok(Self { items: Arc::from(items), depth, weight })
        }
    }

    /// The elements, in order.
    pub fn items(&self) -> &[Value] {
        &self.items
    }

    /// The elements, by value: O(n) reference-count increments, never a deep copy.
    pub fn into_items(self) -> Vec<Value> {
        self.items.to_vec()
    }

    /// The number of elements.
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Whether the array has no elements.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    /// See [`Value::depth`].
    pub const fn depth(&self) -> usize {
        self.depth
    }

    /// See [`Value::weight`].
    pub const fn weight(&self) -> u64 {
        self.weight
    }
}

impl PartialEq for Array {
    fn eq(&self, other: &Self) -> bool {
        // the depth and weight follow from the items; `Arc<[Value]>` compares pointers first, then content
        self.items == other.items
    }
}

impl Eq for Array {}

impl fmt::Debug for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Array")
            .field("items", &&*self.items)
            .field("depth", &self.depth)
            .field("weight", &self.weight)
            .finish()
    }
}

/// A map with unique text keys, no null values, keys in canonical order and nesting within [`MAX_DEPTH`]. Shared:
/// clones in O(1); its depth and weight are cached.
#[derive(Clone)]
pub struct Map {
    entries: Arc<[(String, Value)]>,
    depth: usize,
    weight: u64,
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
            let weight =
                entries.iter().fold(1, |w, (k, v)| add_weight(w, add_weight(sized_weight(k.len()), v.weight())));
            Ok(Self { entries: Arc::from(entries), depth, weight })
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

    /// The entries by value, in canonical key order: the keys are copied, the values are O(1) clones.
    pub fn into_entries(self) -> Vec<(String, Value)> {
        self.entries.to_vec()
    }

    /// The number of entries.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Whether the map has no entries.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// See [`Value::depth`].
    pub const fn depth(&self) -> usize {
        self.depth
    }

    /// See [`Value::weight`].
    pub const fn weight(&self) -> u64 {
        self.weight
    }
}

impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        // the depth and weight follow from the entries; `Arc<[_]>` compares pointers first, then content
        self.entries == other.entries
    }
}

impl Eq for Map {}

impl fmt::Debug for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Map")
            .field("entries", &&*self.entries)
            .field("depth", &self.depth)
            .field("weight", &self.weight)
            .finish()
    }
}
