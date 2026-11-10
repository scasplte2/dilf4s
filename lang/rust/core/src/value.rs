//! The canonical data model (docs/specs/canonical-encoding.md section 1).
//!
//! Every invariant is held by a type: [`Int`] is no wider than [`MAX_INTEGER_BITS`], [`Rational`] is reduced, never an
//! integer and no wider than [`MAX_RATIONAL_BITS`], [`Map`] has unique keys in
//! canonical order and no null values, and [`Array`] and [`Map`] never nest deeper than [`MAX_DEPTH`]. Their fields
//! are private, so every constructible [`Value`] has exactly one encoding and decodes back to itself.
//!
//! # Sharing
//!
//! Values are immutable and share structure: every variable-size payload sits behind an [`Arc`], so cloning a
//! [`Value`] is O(1) and allocation-free, and a value may occur many times inside another without being copied. Each
//! value's [`weight`](Value::weight) and [`depth`](Value::depth) are computed once, at construction, from its children's
//! cached ones, so reading either is O(1). A [`Map`] is a persistent tree: an update is O(log n) and shares the rest.
//! There is no interior mutability: once built, a value never changes.
//!
//! Sharing makes a value's logical size independent of its memory: `[x, x]` nested `k` deep holds `2^k` leaves in
//! `O(k)` allocations, and its weight (the logical, unshared measure) is about `2^k`. So:
//! - anything that expands a value ([`crate::cbor::encode`], [`crate::json::render_within`], hashing, commitments, decoding
//!   into typed models) costs O(weight), not O(memory): bound the weight of a value not known to be small first, or
//!   use the budgeted [`crate::cbor::encode_within`] and [`crate::cbor::encoded_len_within`];
//! - equality never expands: it records the pairs of shared parts it has proven equal and skips them when they recur;
//! - [`fmt::Debug`] shows at most [`DEBUG_BUDGET`] units of weight and [`DEBUG_DEPTH`] levels, and elides the rest as `…`.
//!
//! Values decoded from bytes are trees, not shared, so their weight is bounded by their input: every canonical
//! encoding is at least as long as its value's weight (see [`crate::cbor::encode_within`]).
//!
//! Pointer identity is only ever a fast path for equality: equal pointers are equal values, and every other decision
//! is made on content.
//!
//! # Not for secrets
//!
//! A [`Value`] must never hold secret material. Its buffers are shared and freed by whichever clone drops last, so they
//! cannot be reliably zeroized; secret keys, nonces and seeds belong in dedicated types (as in `dilf4s-signing`).

use alloc::boxed::Box;
use alloc::collections::BTreeSet;
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::borrow::Borrow;
use core::cmp::Ordering;
use core::fmt;
use core::ops::Deref;

use num_bigint::{BigInt, BigUint};
use num_integer::Integer;
use num_traits::{One, Signed, Zero};

use crate::error::CanonicalError;

mod code_points;
mod map;

pub use map::{Entries, Map, MapShape};

/// The nesting limit for arrays and maps: a scalar has depth 0, an array or map one more than its deepest element.
///
/// No walk of a [`Value`] recurses on the native stack by its depth or size, so every one is safe on a small stack
/// (an SP1 guest's): [`PartialEq`], the CBOR encoder and decoder, and the JSON renderer and parser keep explicit
/// stacks of at most `MAX_DEPTH` (256) open containers; a map's entries come from an explicit stack of at most its
/// tree height (under 92, see [`Map`]); dropping releases parts from an explicit worklist; and [`fmt::Debug`], which
/// recurses through the formatter, stops at [`DEBUG_DEPTH`] levels.
pub const MAX_DEPTH: usize = crate::generated::limits::as_usize::CANONICAL_MAX_DEPTH;

/// The widest numerator or denominator a [`Rational`] has, in Java `bitLength` bits (docs/specs/canonical-encoding.md
/// section 1): the integer bound, [`MAX_INTEGER_BITS`], and far past every JLVM number (4096 bits). Checking lowest
/// terms takes a gcd, superlinear in its operands, so a decoder refuses a wider part
/// before any gcd: a rational's decoding then costs at most one gcd of two numbers this wide, about 4 KiB each, and
/// stays linear in its input. Every constructor refuses a wider rational too, so no value is built that no decoder
/// accepts.
pub const MAX_RATIONAL_BITS: u64 = crate::generated::limits::CANONICAL_MAX_RATIONAL_BITS;

/// The widest [`Int`], in Java `bitLength` bits (docs/specs/canonical-encoding.md section 1): the widest whose every
/// integer has at most 10,000 digits (`2^33219 < 10^10000`), so every integer renders as JSON; `8e9999` is an integer
/// and `9e9999` is not. Far past every JLVM number (4096 bits), and the width of a rational's parts too. [`Int::new`] refuses a
/// wider one, and a decoder refuses a wider bignum from its length, before it builds the integer: no value is built that
/// no decoder accepts, and none is so wide that what reads it (rendering, arithmetic, hashing) is unbounded.
pub const MAX_INTEGER_BITS: u64 = crate::generated::limits::CANONICAL_MAX_INTEGER_BITS;

/// The greatest [`Value::weight`]: weights saturate here (the Scala reference's `Long.MaxValue`), never wrap.
pub const MAX_WEIGHT: u64 = crate::generated::limits::CANONICAL_MAX_WEIGHT;

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
pub(crate) fn bit_length(n: &BigInt) -> u64 {
    let bits = n.bits();
    let negative_power_of_two = n.is_negative() && n.magnitude().trailing_zeros() == Some(bits.saturating_sub(1));
    if negative_power_of_two { bits.saturating_sub(1) } else { bits }
}

/// A canonical value.
///
/// Cloning is O(1), comparing never expands shared parts, and formatting is budgeted: see the [module
/// documentation](self). No walk of it recurses by its depth or size (see [`MAX_DEPTH`]).
#[derive(Clone)]
pub enum Value {
    /// `null`.
    Null,
    /// `false` or `true`.
    Bool(bool),
    /// An integer within [`MAX_INTEGER_BITS`].
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
            Self::Map(m) => m.depth(),
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
            Self::Map(m) => m.weight(),
        }
    }

    /// The integer `n`, of a primitive type: always within [`MAX_INTEGER_BITS`]. A [`BigInt`] goes through
    /// [`Value::integer`], which checks the width.
    pub fn int(n: impl Into<Int>) -> Self {
        Self::Int(n.into())
    }

    /// The integer `n`, or [`IntegerTooWide`] when it is wider than [`MAX_INTEGER_BITS`].
    pub fn integer(n: BigInt) -> Result<Self, IntegerTooWide> {
        Int::new(n).map(Self::Int)
    }

    /// The text `s`.
    pub fn text(s: impl Into<Text>) -> Self {
        Self::Text(s.into())
    }

    /// The byte string `b`.
    pub fn bytes(b: impl Into<Arc<[u8]>>) -> Self {
        Self::Bytes(b.into())
    }

    /// `unscaled / 10^scale`, reduced with only the primes 2 and 5 (a JSON decimal's denominator has no others), or
    /// [`CanonicalError::RationalTooWide`] for a rational wider than [`MAX_RATIONAL_BITS`] (a JSON decimal can be:
    /// `0.000...1` at a scale of 10,000 has the denominator `10^10000`), or `IntegerTooWide` for an integer wider than
    /// [`MAX_INTEGER_BITS`].
    pub(crate) fn decimal(unscaled: &BigInt, scale: usize) -> Result<Self, CanonicalError> {
        match Rational::reduce_decimal(unscaled, scale) {
            Some(Reduced::Integer(n)) => Ok(Self::integer(n)?),
            Some(Reduced::Fraction(r)) => Ok(Self::Rational(r)),
            None => Err(CanonicalError::RationalTooWide { limit: MAX_RATIONAL_BITS }),
        }
    }

    /// The number `numerator / denominator` in the single numeric domain: an integer when it is one, refused when
    /// wider than [`MAX_INTEGER_BITS`], otherwise a reduced [`Rational`], refused when its numerator or denominator is
    /// wider than [`MAX_RATIONAL_BITS`].
    pub fn number(numerator: &BigInt, denominator: &BigInt) -> Result<Self, NumberError> {
        match Rational::reduce(numerator, denominator)? {
            Reduced::Integer(n) => Ok(Self::integer(n)?),
            Reduced::Fraction(r) => Ok(Self::Rational(r)),
        }
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

/// Shared, immutable text: derefs to [`str`], and clones in O(1). Its length in UTF-16 code units (the Scala
/// reference's `String.length`) is counted once, when it is made, and read in O(1).
#[derive(Clone)]
pub struct Text {
    text: Arc<str>,
    utf16: u64,
}

impl Text {
    fn of(text: Arc<str>) -> Self {
        let utf16 = text.chars().fold(0u64, |n, c| n.saturating_add(if c.len_utf16() == 2 { 2 } else { 1 }));
        Self { text, utf16 }
    }

    /// The text.
    pub fn as_str(&self) -> &str {
        &self.text
    }

    /// The length in UTF-16 code units. O(1): counted when the text was made.
    pub const fn utf16_len(&self) -> u64 {
        self.utf16
    }
}

// equality, order and hashing are the text's: the cached length follows from it
impl PartialEq for Text {
    fn eq(&self, other: &Self) -> bool {
        self.text == other.text
    }
}

impl Eq for Text {}

impl PartialOrd for Text {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Text {
    fn cmp(&self, other: &Self) -> Ordering {
        self.text.cmp(&other.text)
    }
}

impl core::hash::Hash for Text {
    fn hash<S: core::hash::Hasher>(&self, state: &mut S) {
        self.text.hash(state);
    }
}

impl Deref for Text {
    type Target = str;
    fn deref(&self) -> &str {
        &self.text
    }
}

impl AsRef<str> for Text {
    fn as_ref(&self) -> &str {
        &self.text
    }
}

impl Borrow<str> for Text {
    fn borrow(&self) -> &str {
        &self.text
    }
}

impl fmt::Debug for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&*self.text, f)
    }
}

impl fmt::Display for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&*self.text, f)
    }
}

impl PartialEq<str> for Text {
    fn eq(&self, other: &str) -> bool {
        *self.text == *other
    }
}

impl PartialEq<&str> for Text {
    fn eq(&self, other: &&str) -> bool {
        *self.text == **other
    }
}

impl PartialEq<String> for Text {
    fn eq(&self, other: &String) -> bool {
        *self.text == **other
    }
}

impl PartialEq<Text> for str {
    fn eq(&self, other: &Text) -> bool {
        *self == *other.text
    }
}

impl PartialEq<Text> for &str {
    fn eq(&self, other: &Text) -> bool {
        **self == *other.text
    }
}

impl PartialEq<Text> for String {
    fn eq(&self, other: &Text) -> bool {
        **self == *other.text
    }
}

impl From<&str> for Text {
    fn from(s: &str) -> Self {
        Self::of(Arc::from(s))
    }
}

impl From<&String> for Text {
    fn from(s: &String) -> Self {
        Self::of(Arc::from(s.as_str()))
    }
}

impl From<String> for Text {
    fn from(s: String) -> Self {
        Self::of(Arc::from(s))
    }
}

impl From<Arc<str>> for Text {
    fn from(s: Arc<str>) -> Self {
        Self::of(s)
    }
}

impl From<Text> for Arc<str> {
    fn from(t: Text) -> Self {
        t.text
    }
}

// ------------------------------------------------------------------------------------------------ numbers

/// A shared, immutable integer within [`MAX_INTEGER_BITS`]: derefs to [`BigInt`], clones in O(1), and caches its
/// weight. Built by [`Int::new`], which checks the width, or from a primitive integer, which always fits.
#[derive(Clone)]
pub struct Int {
    value: Arc<BigInt>,
    bit_length: u64,
    weight: u64,
}

/// An integer wider than [`MAX_INTEGER_BITS`]: no [`Int`] holds one.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IntegerTooWide;

impl fmt::Display for IntegerTooWide {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "an integer wider than {MAX_INTEGER_BITS} bits")
    }
}

impl core::error::Error for IntegerTooWide {}

impl From<IntegerTooWide> for CanonicalError {
    fn from(_: IntegerTooWide) -> Self {
        Self::IntegerTooWide { limit: MAX_INTEGER_BITS }
    }
}

impl Int {
    /// The integer `n`, or [`IntegerTooWide`] when it is wider than [`MAX_INTEGER_BITS`]: the only way to build a
    /// wide one.
    pub fn new(n: BigInt) -> Result<Self, IntegerTooWide> {
        let bits = bit_length(&n);
        if bits > MAX_INTEGER_BITS {
            Err(IntegerTooWide)
        } else {
            Ok(Self { value: Arc::new(n), bit_length: bits, weight: add_weight(1, bits / 64) })
        }
    }

    /// A primitive integer, at most 128 bits: always within [`MAX_INTEGER_BITS`].
    fn small(n: BigInt) -> Self {
        let bits = bit_length(&n);
        Self { value: Arc::new(n), bit_length: bits, weight: add_weight(1, bits / 64) }
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

    /// Java's `BigInteger.bitLength` (the Scala reference's): the bits of the minimal two's-complement form without
    /// its sign bit. O(1): computed when the integer was made.
    pub const fn bit_length(&self) -> u64 {
        self.bit_length
    }
}

/// Every primitive integer is at most 128 bits wide, so converting one is total.
macro_rules! int_from_primitive {
    ($($t:ty),*) => {$(
        impl From<$t> for Int {
            fn from(n: $t) -> Self {
                Self::small(BigInt::from(n))
            }
        }
    )*};
}

int_from_primitive!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

impl TryFrom<BigInt> for Int {
    type Error = IntegerTooWide;
    fn try_from(n: BigInt) -> Result<Self, IntegerTooWide> {
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

/// Why [`Value::number`] has no value.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumberError {
    /// A zero denominator.
    DivisionByZero,
    /// A rational, in lowest terms, whose numerator or denominator is wider than [`MAX_RATIONAL_BITS`].
    RationalTooWide,
    /// A number that reduces to an integer wider than [`MAX_INTEGER_BITS`].
    IntegerTooWide,
}

impl From<IntegerTooWide> for NumberError {
    fn from(_: IntegerTooWide) -> Self {
        Self::IntegerTooWide
    }
}

impl fmt::Display for NumberError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::DivisionByZero => f.write_str("division by zero"),
            Self::RationalTooWide => write!(f, "a rational part wider than {MAX_RATIONAL_BITS} bits"),
            Self::IntegerTooWide => write!(f, "an integer wider than {MAX_INTEGER_BITS} bits"),
        }
    }
}

impl core::error::Error for NumberError {}

/// The parts of a rational are within [`MAX_RATIONAL_BITS`].
fn within_rational_bits(numerator: &BigInt, denominator: &BigInt) -> bool {
    bit_length(numerator) <= MAX_RATIONAL_BITS && bit_length(denominator) <= MAX_RATIONAL_BITS
}

/// A rational `n/d` with `d >= 2`, `gcd(|n|, d) = 1` and both within [`MAX_RATIONAL_BITS`]: never an integer. Shared:
/// clones in O(1).
#[derive(Clone, PartialEq, Eq)]
pub struct Rational(Arc<RationalParts>);

/// Equality is derived from the numerator and denominator; the weight follows from them.
#[derive(PartialEq, Eq)]
struct RationalParts {
    numerator: BigInt,
    denominator: BigInt,
    numerator_bits: u64,
    denominator_bits: u64,
    weight: u64,
}

/// `n` without its factors of 5, at most `cap` of them, and how many were removed (see [`Rational::reduce_decimal`]).
fn strip_fives(n: BigUint, cap: usize) -> (BigUint, usize) {
    let powers: Vec<(usize, BigUint)> =
        core::iter::successors(Some((1usize, BigUint::from(5u8))), |(e, p)| e.checked_mul(2).map(|e2| (e2, p * p)))
            .take_while(|(e, _)| *e <= cap)
            .collect();
    powers.iter().rev().fold((n, 0usize), |(n, count), (e, p)| {
        let within = count.checked_add(*e).is_some_and(|c| c <= cap);
        if within && (&n % p).is_zero() { (n / p, count.saturating_add(*e)) } else { (n, count) }
    })
}

pub(crate) enum Reduced {
    Integer(BigInt),
    Fraction(Rational),
}

impl Rational {
    /// The only way to build one: callers have checked that it is reduced and not an integer, and it checks the width
    /// itself, so no wider rational exists.
    fn checked(numerator: BigInt, denominator: BigInt) -> Option<Self> {
        let (numerator_bits, denominator_bits) = (bit_length(&numerator), bit_length(&denominator));
        let within = numerator_bits <= MAX_RATIONAL_BITS && denominator_bits <= MAX_RATIONAL_BITS;
        let weight = add_weight(add_weight(2, numerator_bits / 64), denominator_bits / 64);
        within
            .then(|| Self(Arc::new(RationalParts { numerator, denominator, numerator_bits, denominator_bits, weight })))
    }

    /// `n/d` exactly as given, if it already satisfies the invariants (as a decoder must check): `d >= 2`, both within
    /// [`MAX_RATIONAL_BITS`] (checked first, so a wide one runs no gcd), and in lowest terms.
    pub fn new(numerator: BigInt, denominator: BigInt) -> Option<Self> {
        let valid = denominator > BigInt::one()
            && within_rational_bits(&numerator, &denominator)
            && numerator.gcd(&denominator).is_one();
        valid.then(|| Self::checked(numerator, denominator)).flatten()
    }

    /// `unscaled / 10^scale` in lowest terms. `10^scale`'s only primes are 2 and 5, so the common factor is
    /// `2^min(v2, scale) 5^min(v5, scale)`: the twos go in one shift, and the fives by trial division by `5^(2^i)` from
    /// the largest `2^i <= scale` down, which builds `min(v5, scale)` bit by bit (about 14 divisions for a scale of
    /// 10000). The same result as [`Rational::reduce`] against `10^scale`, without its general gcd.
    /// `None` for a rational wider than [`MAX_RATIONAL_BITS`].
    pub(crate) fn reduce_decimal(unscaled: &BigInt, scale: usize) -> Option<Reduced> {
        let magnitude = unscaled.magnitude();
        if magnitude.is_zero() {
            return Some(Reduced::Integer(BigInt::zero()));
        }
        let twos = magnitude.trailing_zeros().map_or(0, |t| usize::try_from(t).unwrap_or(usize::MAX)).min(scale);
        let (rest, fives) = strip_fives(magnitude >> twos, scale);
        let numerator = BigInt::from_biguint(unscaled.sign(), rest);
        let denominator = BigInt::from(
            num_traits::pow(BigUint::from(5u8), scale.saturating_sub(fives)) << scale.saturating_sub(twos),
        );
        if denominator.is_one() {
            Some(Reduced::Integer(numerator))
        } else {
            Self::checked(numerator, denominator).map(Reduced::Fraction)
        }
    }

    pub(crate) fn reduce(numerator: &BigInt, denominator: &BigInt) -> Result<Reduced, NumberError> {
        if denominator.is_zero() {
            return Err(NumberError::DivisionByZero);
        }
        let g = numerator.gcd(denominator);
        let sign = if denominator.is_negative() { -BigInt::one() } else { BigInt::one() };
        // g >= 1 here: the denominator is non-zero, so the divisions are exact and cannot fail
        let n = numerator.div_floor(&g) * &sign;
        let d = denominator.div_floor(&g) * sign;
        if d.is_one() {
            Ok(Reduced::Integer(n))
        } else {
            Self::checked(n, d).map(Reduced::Fraction).ok_or(NumberError::RationalTooWide)
        }
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

    /// The numerator's Java `bitLength` (see [`Int::bit_length`]). O(1).
    pub fn numerator_bit_length(&self) -> u64 {
        self.0.numerator_bits
    }

    /// The denominator's Java `bitLength`. O(1).
    pub fn denominator_bit_length(&self) -> u64 {
        self.0.denominator_bits
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
    /// Always present but while the array is dropped: the storage is taken out then, so its last owner can be found
    /// atomically (`Arc::into_inner`, which needs a sized payload, hence the box) and released without recursion.
    items: Option<Arc<Box<[Value]>>>,
    depth: usize,
    weight: u64,
}

impl Array {
    /// The array of `items`, if within the depth limit.
    pub fn new(items: Vec<Value>) -> Result<Self, CanonicalError> {
        let depth = items.iter().map(Value::depth).max().unwrap_or(0).saturating_add(1);
        if depth > MAX_DEPTH {
            Err(CanonicalError::DepthExceeded { limit: crate::error::size(MAX_DEPTH) })
        } else {
            let weight = items.iter().fold(1, |w, v| add_weight(w, v.weight()));
            Ok(Self { items: Some(Arc::new(items.into_boxed_slice())), depth, weight })
        }
    }

    /// The elements, in order.
    pub fn items(&self) -> &[Value] {
        self.items.as_deref().map_or(&[], |b| b)
    }

    /// The address of the shared storage: a key for the equality memo, never a decision by itself.
    fn address(&self) -> usize {
        self.items.as_ref().map_or(0, |b| Arc::as_ptr(b).addr())
    }

    /// The elements, by value: O(n) reference-count increments, never a deep copy.
    pub fn into_items(self) -> Vec<Value> {
        self.items().to_vec()
    }

    /// The number of elements.
    pub fn len(&self) -> usize {
        self.items().len()
    }

    /// Whether the array has no elements.
    pub fn is_empty(&self) -> bool {
        self.items().is_empty()
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

/// The order of keys in an encoded map: their encodings compared bytewise (RFC 8949 section 4.2.1). For text keys
/// that is shorter UTF-8 first, then bytewise, because the head encodes the length in increasing order.
pub fn canonical_key_order(a: &str, b: &str) -> Ordering {
    a.len().cmp(&b.len()).then_with(|| a.as_bytes().cmp(b.as_bytes()))
}

// ------------------------------------------------------------------------------------------------ equality

// Equality is on content (the canonical encoding is the identity), and costs O(the distinct pairs of shared payloads
// compared), never O(weight): a comparison keeps the address pairs of containers, and of large texts, byte strings and
// numbers, that it has proven equal, and skips them when they recur. An address is only ever a fast path: equal
// addresses are the same payload, and a pair is recorded only after its contents compared equal, while both values
// are borrowed (so neither address is freed or reused). Small scalars are compared outright: cheaper than the memo.

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        let mut c = Comparison::default();
        c.value(self, other) && c.finish()
    }
}

impl Eq for Value {}

impl PartialEq for Array {
    fn eq(&self, other: &Self) -> bool {
        let mut c = Comparison::default();
        c.array(self, other) && c.finish()
    }
}

impl Eq for Array {}

impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        let mut c = Comparison::default();
        c.map(self, other) && c.finish()
    }
}

impl Eq for Map {}

/// The address of a shared slice payload.
fn address<T>(items: &[T]) -> usize {
    items.as_ptr().addr()
}

/// The address of a shared payload.
fn at<T>(item: &T) -> usize {
    core::ptr::from_ref(item).addr()
}

/// A kind of payload and the addresses of two of them.
type Pair = (u8, usize, usize);

const ARRAY: u8 = 0;
const MAP: u8 = 1;
const INT: u8 = 2;
const RATIONAL: u8 = 3;
const TEXT: u8 = 4;
const BYTES: u8 = 5;

/// The weight from which a text or byte string is memoised (bytes: one unit each).
const LARGE_STRING: u64 = 64;
/// The weight from which a number is memoised (limbs: eight bytes each).
const LARGE_NUMBER: u64 = 8;

/// Two containers whose elements are being compared, and the address pair to record once they are all equal.
enum Frame<'a> {
    Items { a: &'a [Value], b: &'a [Value], next: usize, pair: Pair },
    Entries { a: Entries<'a>, b: Entries<'a>, pair: Pair },
}

/// One comparison, with an explicit stack (at most [`MAX_DEPTH`] frames) and the pairs proven equal so far.
#[derive(Default)]
struct Comparison<'a> {
    stack: Vec<Frame<'a>>,
    proven: BTreeSet<Pair>,
}

impl<'a> Comparison<'a> {
    /// False when `a` and `b` differ; true when they are equal scalars, or containers now open on the stack.
    fn value(&mut self, a: &'a Value, b: &'a Value) -> bool {
        if a.weight() != b.weight() || a.depth() != b.depth() {
            return false;
        }
        match (a, b) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            (Value::Int(x), Value::Int(y)) => {
                let pair = (INT, at(x.as_bigint()), at(y.as_bigint()));
                self.scalar(pair, a.weight() >= LARGE_NUMBER, || x == y)
            }
            (Value::Rational(x), Value::Rational(y)) => {
                let pair = (RATIONAL, at(x.numerator()), at(y.numerator()));
                self.scalar(pair, a.weight() >= LARGE_NUMBER, || x == y)
            }
            (Value::Text(x), Value::Text(y)) => {
                let pair = (TEXT, address(x.as_bytes()), address(y.as_bytes()));
                self.scalar(pair, a.weight() >= LARGE_STRING, || x == y)
            }
            (Value::Bytes(x), Value::Bytes(y)) => {
                self.scalar((BYTES, address(x), address(y)), a.weight() >= LARGE_STRING, || x == y)
            }
            (Value::Array(x), Value::Array(y)) => self.array(x, y),
            (Value::Map(x), Value::Map(y)) => self.map(x, y),
            _ => false,
        }
    }

    /// Two scalars of one kind: the same payload, a large pair already proven equal, or compared now (and a large
    /// pair recorded when equal).
    fn scalar(&mut self, pair: Pair, large: bool, equal: impl FnOnce() -> bool) -> bool {
        if pair.1 == pair.2 || (large && self.proven.contains(&pair)) {
            return true;
        }
        let same = equal();
        if same && large {
            self.proven.insert(pair);
        }
        same
    }

    fn array(&mut self, a: &'a Array, b: &'a Array) -> bool {
        if a.weight != b.weight || a.depth != b.depth || a.len() != b.len() {
            return false;
        }
        let pair = (ARRAY, a.address(), b.address());
        if pair.1 != pair.2 && !self.proven.contains(&pair) {
            self.stack.push(Frame::Items { a: a.items(), b: b.items(), next: 0, pair });
        }
        true
    }

    fn map(&mut self, a: &'a Map, b: &'a Map) -> bool {
        if a.weight() != b.weight() || a.depth() != b.depth() || a.len() != b.len() {
            return false;
        }
        let pair = (MAP, a.address(), b.address());
        if pair.1 != pair.2 && !self.proven.contains(&pair) {
            self.stack.push(Frame::Entries { a: a.entries(), b: b.entries(), pair });
        }
        true
    }

    /// Compares every open container to the end.
    fn finish(mut self) -> bool {
        loop {
            let Some(frame) = self.stack.last_mut() else {
                return true;
            };
            let next: Option<(&'a Value, &'a Value, bool)> = match frame {
                Frame::Items { a, b, next, .. } => {
                    let (a, b): (&'a [Value], &'a [Value]) = (a, b);
                    let i = *next;
                    *next = i.saturating_add(1);
                    a.get(i).zip(b.get(i)).map(|(x, y)| (x, y, true))
                }
                // in-order entries: the trees' shapes may differ, their contents decide
                Frame::Entries { a, b, .. } => a.next().zip(b.next()).map(|((ka, x), (kb, y))| (x, y, ka == kb)),
            };
            match next {
                Some((x, y, same_key)) => {
                    if !same_key || !self.value(x, y) {
                        return false;
                    }
                }
                None => {
                    if let Some(Frame::Items { pair, .. } | Frame::Entries { pair, .. }) = self.stack.pop() {
                        self.proven.insert(pair);
                    }
                }
            }
        }
    }
}

// ------------------------------------------------------------------------------------------------ formatting

/// How much of a value [`fmt::Debug`] shows, in units of [`Value::weight`]; the rest is elided as `…`. Formatting any
/// value is then bounded by this budget, however often it repeats a shared part.
pub const DEBUG_BUDGET: u64 = 1 << 16;

/// How many arrays and maps deep [`fmt::Debug`] shows; deeper ones are elided as `…`. Formatting recurses through the
/// formatter once per level shown, so this, not [`MAX_DEPTH`], bounds its native stack.
pub const DEBUG_DEPTH: usize = 16;

/// The budget left in one `fmt` call, and how deep it is. The cells live on that call's stack, never in a value.
struct Left {
    units: core::cell::Cell<u64>,
    depth: core::cell::Cell<usize>,
}

impl Left {
    const fn new(units: u64) -> Self {
        Self { units: core::cell::Cell::new(units), depth: core::cell::Cell::new(0) }
    }

    /// `show` one level deeper, or `…` past [`DEBUG_DEPTH`] or the budget.
    fn nested(
        &self,
        f: &mut fmt::Formatter<'_>,
        show: impl FnOnce(&mut fmt::Formatter<'_>) -> fmt::Result,
    ) -> fmt::Result {
        let depth = self.depth.get();
        // an elided container still costs a unit, so the output stays within the budget
        if !spend(self, 1) || depth >= DEBUG_DEPTH {
            return fmt::Debug::fmt(&Elided, f);
        }
        self.depth.set(depth.saturating_add(1));
        let shown = show(f);
        self.depth.set(depth);
        shown
    }
}

fn spend(left: &Left, weight: u64) -> bool {
    let l = left.units.get();
    let affordable = weight <= l;
    left.units.set(if affordable { l.saturating_sub(weight) } else { 0 });
    affordable
}

/// What the budget did not reach.
struct Elided;

impl fmt::Debug for Elided {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("…")
    }
}

/// A part of a value, formatted within a budget.
struct Shown<'a, T: ?Sized>(&'a T, &'a Left);

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Shown(self, &Left::new(DEBUG_BUDGET)).fmt(f)
    }
}

impl fmt::Debug for Array {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Shown(self, &Left::new(DEBUG_BUDGET)).fmt(f)
    }
}

impl fmt::Debug for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Shown(self, &Left::new(DEBUG_BUDGET)).fmt(f)
    }
}

impl fmt::Debug for Shown<'_, Value> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(v, left) = *self;
        match v {
            Value::Array(a) => f.debug_tuple("Array").field(&Shown(a, left)).finish(),
            Value::Map(m) => f.debug_tuple("Map").field(&Shown(m, left)).finish(),
            _ if !spend(left, v.weight()) => Elided.fmt(f),
            Value::Null => f.write_str("Null"),
            Value::Bool(b) => f.debug_tuple("Bool").field(b).finish(),
            Value::Int(n) => f.debug_tuple("Int").field(n).finish(),
            Value::Rational(r) => f.debug_tuple("Rational").field(r).finish(),
            Value::Text(t) => f.debug_tuple("Text").field(t).finish(),
            Value::Bytes(b) => f.debug_tuple("Bytes").field(b).finish(),
        }
    }
}

impl fmt::Debug for Shown<'_, Array> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(a, left) = *self;
        left.nested(f, |f| {
            f.debug_struct("Array")
                .field("items", &Shown(a.items(), left))
                .field("depth", &a.depth)
                .field("weight", &a.weight)
                .finish()
        })
    }
}

impl fmt::Debug for Shown<'_, Map> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(m, left) = *self;
        left.nested(f, |f| {
            f.debug_struct("Map")
                .field("entries", &ShownEntries(m, left))
                .field("depth", &m.depth())
                .field("weight", &m.weight())
                .finish()
        })
    }
}

impl fmt::Debug for Shown<'_, [Value]> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(items, left) = *self;
        let mut list = f.debug_list();
        for v in items {
            if left.units.get() == 0 {
                list.entry(&Elided);
                break;
            }
            list.entry(&Shown(v, left));
        }
        list.finish()
    }
}

/// A map's entries, formatted within a budget.
struct ShownEntries<'a>(&'a Map, &'a Left);

impl fmt::Debug for ShownEntries<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self(m, left) = *self;
        let mut list = f.debug_list();
        for (k, v) in m.entries() {
            if !spend(left, sized_weight(k.len())) {
                list.entry(&Elided);
                break;
            }
            list.entry(&(k, Shown(v, left)));
        }
        list.finish()
    }
}

#[cfg(test)]
mod decimal_tests {
    #![allow(clippy::unwrap_used, clippy::panic, reason = "tests fail by panicking")]

    use super::{BigInt, Value};

    /// Every shape of shared factor: none, only twos, only fives, both past the scale, and the sign.
    #[test]
    fn reducing_by_two_and_five_is_the_gcd_reduction() {
        let ten = BigInt::from(10u8);
        let numerators = [0i64, 1, -1, 2, 5, 10, 16, 25, 125, 640, 3125, -7_812_500, 999, 1_000_000, 2_048_000_000]
            .into_iter()
            .map(BigInt::from)
            .chain([num_traits::pow(BigInt::from(5u8), 20_000), num_traits::pow(BigInt::from(2u8), 20_000) * 3]);
        for n in numerators {
            for scale in [0usize, 1, 2, 3, 7, 8, 9, 64, 1000, 10_000] {
                // the same value, or both refused as too wide (5^20000 over a small scale)
                let general = Value::number(&n, &num_traits::pow(ten.clone(), scale)).ok();
                assert_eq!(Value::decimal(&n, scale).ok(), general, "{} / 10^{scale}", n.bits());
            }
        }
    }
}
