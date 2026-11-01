//! Value-level semantics shared by every operation (`Values.scala`): truthiness, numbers, weight, bounds, paths,
//! ordering, text and deduplication.

use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::cmp::Ordering;

use dilf4s_core::value::Text;
use dilf4s_core::{BigInt, Value, cbor};
use num_traits::{Signed, Zero};

use crate::error::JlvmError;
use crate::op::Op;
use crate::ratio::Ratio;

/// The largest number, in bits of the numerator and of the denominator.
pub const MAX_NUMBER_BITS: u64 = 4096;

/// The largest weight an operation may produce.
pub const MAX_WEIGHT: u64 = 1 << 24;

/// Keys and path segments count once more per block of this many UTF-16 units.
pub const KEY_BLOCK: u64 = 32;

static NULL: Value = Value::Null;

/// Java's `BigInteger.bitLength`: the bits of the minimal two's-complement form, without the sign bit. For a negative
/// `n` that is the bit length of `-n - 1`, so `-2^k` has `k` bits and `2^k` has `k + 1`. Without allocating; it
/// scans the magnitude's limbs for a power of two, so it is only for numbers this crate computed from bounded operands.
/// A value's own numbers carry their bit lengths, cached (`Int::bit_length`).
pub(crate) fn bit_length(n: &BigInt) -> u64 {
    let bits = n.bits();
    let negative_power_of_two = n.is_negative() && n.magnitude().trailing_zeros() == Some(bits.saturating_sub(1));
    if negative_power_of_two { bits.saturating_sub(1) } else { bits }
}

/// A value's number's bit lengths (numerator, denominator), cached: O(1) whatever its size.
fn bit_lengths(v: &Value) -> Option<(u64, u64)> {
    match v {
        Value::Int(i) => Some((i.bit_length(), 1)),
        Value::Rational(r) => Some((r.numerator_bit_length(), r.denominator_bit_length())),
        _ => None,
    }
}

/// The length of `s` in UTF-16 code units (the reference's `String.length`).
/// Every UTF-8 lead byte starts one code point, one UTF-16 unit, and a 4-byte lead (`0xF0` and above) one more.
pub fn utf16_len(s: &str) -> u64 {
    // ASCII, the common case for keys and path segments: one unit per byte
    if s.is_ascii() {
        return usize_u64(s.len());
    }
    let units = s.chars().count();
    let pairs = s.bytes().filter(|&b| b >= 0xf0).count();
    usize_u64(units).saturating_add(usize_u64(pairs))
}

fn usize_u64(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

/// `JsonLogic` truthiness: null, false, 0, "", empty bytes, [] and {} are falsy.
pub fn truthy(v: &Value) -> bool {
    match v {
        Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(i) => !i.is_zero(),
        Value::Rational(_) => true,
        Value::Text(s) => !s.is_empty(),
        Value::Bytes(b) => !b.is_empty(),
        Value::Array(a) => !a.items().is_empty(),
        Value::Map(m) => !m.is_empty(),
    }
}

/// The kind name `typeof` reports. There is one number kind.
pub const fn kind(v: &Value) -> &'static str {
    match v {
        Value::Null => "null",
        Value::Bool(_) => "boolean",
        Value::Int(_) | Value::Rational(_) => "number",
        Value::Text(_) => "string",
        Value::Bytes(_) => "bytes",
        Value::Array(_) => "array",
        Value::Map(_) => "object",
    }
}

pub(crate) fn is_number(v: &Value) -> bool {
    matches!(v, Value::Int(_) | Value::Rational(_))
}

fn fits(r: &Ratio) -> bool {
    bit_length(r.numerator()) <= MAX_NUMBER_BITS && bit_length(r.denominator()) <= MAX_NUMBER_BITS
}

pub(crate) fn too_large(op: Op) -> JlvmError {
    JlvmError::limit(alloc::format!("{}: a number exceeds {MAX_NUMBER_BITS} bits", op.wire()))
}

/// A number operand, within [`MAX_NUMBER_BITS`].
/// The bound is checked on the borrowed number, before it is copied out: an operation charged by its base alone (`abs`,
/// `pow`) must not copy a huge input to find it too large.
pub(crate) fn numeric(op: Op, v: &Value) -> Result<Ratio, JlvmError> {
    let (n, d) = bit_lengths(v)
        .ok_or_else(|| JlvmError::type_error(op.wire(), alloc::format!("expected a number, got {}", kind(v))))?;
    if n <= MAX_NUMBER_BITS && d <= MAX_NUMBER_BITS {
        Ratio::from_value(v).ok_or_else(|| too_large(op))
    } else {
        Err(too_large(op))
    }
}

/// An integer operand, within [`MAX_NUMBER_BITS`].
pub(crate) fn integer(op: Op, v: &Value) -> Result<&BigInt, JlvmError> {
    match v {
        Value::Int(i) if i.bit_length() <= MAX_NUMBER_BITS => Ok(i),
        Value::Int(_) => Err(too_large(op)),
        other => Err(JlvmError::type_error(op.wire(), alloc::format!("expected an integer, got {}", kind(other)))),
    }
}

/// A number result, within [`MAX_NUMBER_BITS`].
pub(crate) fn num(op: Op, r: &Ratio) -> Result<Value, JlvmError> {
    if fits(r) { Ok(r.to_value()) } else { Err(too_large(op)) }
}

/// An integer result, within [`MAX_NUMBER_BITS`].
pub(crate) fn int(op: Op, i: BigInt) -> Result<Value, JlvmError> {
    if bit_length(&i) <= MAX_NUMBER_BITS { Ok(Value::int(i)) } else { Err(too_large(op)) }
}

/// A count or a length: always small.
pub(crate) fn count(n: u64) -> Value {
    Value::int(n)
}

pub(crate) fn count_usize(n: usize) -> Value {
    count(usize_u64(n))
}

/// `v`, if its weight is within [`MAX_WEIGHT`]. `place` names the construct: an operation, `array` or `object`.
pub(crate) fn bounded(place: &str, v: Value) -> Result<Value, JlvmError> {
    // O(1): the weight is cached at construction, from the children's cached weights
    if v.weight() <= MAX_WEIGHT {
        Ok(v)
    } else {
        Err(JlvmError::limit(alloc::format!("{place}: the result exceeds weight {MAX_WEIGHT}")))
    }
}

/// A text result. Rust text is always well-formed, so only the weight is checked.
pub(crate) fn text(op: Op, s: String) -> Result<Value, JlvmError> {
    bounded(op.wire(), Value::text(s))
}

/// An array result: depth, then weight.
pub(crate) fn array(place: &str, vs: Vec<Value>) -> Result<Value, JlvmError> {
    let v = Value::array(vs).map_err(|e| JlvmError::limit(alloc::format!("{place}: {e}")))?;
    bounded(place, v)
}

/// A map result: duplicate keys and depth, then weight. Null values are dropped.
pub(crate) fn map(place: &str, entries: Vec<(String, Value)>) -> Result<Value, JlvmError> {
    let v = Value::map(entries).map_err(|e| JlvmError::limit(alloc::format!("{place}: {e}")))?;
    bounded(place, v)
}

/// The value at `path`: an object segment is a key; an array segment is a decimal index without leading zeros.
/// Anything absent is null.
pub fn at<'v, S: AsRef<str>>(v: &'v Value, path: &[S]) -> &'v Value {
    let mut here = v;
    for seg in path {
        let next = match here {
            Value::Map(m) => m.get(seg.as_ref()),
            Value::Array(a) => index(seg.as_ref()).and_then(|i| a.items().get(i)),
            _ => None,
        };
        match next {
            Some(x) => here = x,
            None => return &NULL,
        }
    }
    here
}

/// `0|[1-9][0-9]{0,8}`, as an index.
fn index(seg: &str) -> Option<usize> {
    let b = seg.as_bytes();
    let well_formed = match b.split_first() {
        Some((b'0', rest)) => rest.is_empty(),
        Some((first, rest)) => (b'1'..=b'9').contains(first) && rest.len() <= 8 && rest.iter().all(u8::is_ascii_digit),
        None => false,
    };
    if well_formed { seg.parse().ok() } else { None }
}

/// Text compares by Unicode code point (the order of UTF-8 bytes); numbers numerically; anything else cannot be
/// ordered.
pub(crate) fn compare(op: Op, a: &Value, b: &Value) -> Result<Ordering, JlvmError> {
    match (a, b) {
        (Value::Text(x), Value::Text(y)) => Ok(x.as_str().cmp(y.as_str())),
        _ if is_number(a) && is_number(b) => {
            let x = numeric(op, a)?;
            let y = numeric(op, b)?;
            Ok(x.cmp(&y))
        }
        _ => Err(JlvmError::type_error(op.wire(), alloc::format!("cannot order {} and {}", kind(a), kind(b)))),
    }
}

// ---- text

/// The Unicode `White_Space` property (PropList.txt), listed explicitly.
fn white_space(c: char) -> bool {
    matches!(
        u32::from(c),
        0x09..=0x0d
            | 0x20
            | 0x85
            | 0xa0
            | 0x1680
            | 0x2000..=0x200a
            | 0x2028
            | 0x2029
            | 0x202f
            | 0x205f
            | 0x3000
    )
}

/// Trims Unicode `White_Space` from both ends.
pub(crate) fn trim(s: &str) -> &str {
    s.trim_matches(white_space)
}

/// The bits of a number's decimal expansion, bounded from its size alone (`Values.renderedBits`); 0 for anything else.
pub(crate) fn rendered_bits(v: &Value) -> u64 {
    match v {
        Value::Int(i) => i.bit_length(),
        Value::Rational(r) => {
            r.numerator_bit_length().saturating_add(r.denominator_bit_length().saturating_mul(7).saturating_add(2) / 3)
        }
        _ => 0,
    }
}

/// A path segment's count: 1, plus one per [`KEY_BLOCK`] of its UTF-16 length.
pub(crate) fn segment_count(s: &str) -> u64 {
    (utf16_len(s) / KEY_BLOCK).saturating_add(1)
}

/// [`segment_count`] of shared text, from its cached UTF-16 length: O(1).
pub(crate) fn text_segment_count(t: &Text) -> u64 {
    (t.utf16_len() / KEY_BLOCK).saturating_add(1)
}

/// The blocks of text used as a key; 0 for anything else.
pub(crate) fn key_blocks(v: &Value) -> u64 {
    match v {
        Value::Text(s) => s.utf16_len() / KEY_BLOCK,
        _ => 0,
    }
}

/// Top-level size: entries of arrays and maps, 1 for anything else.
pub(crate) fn elements(v: &Value) -> u64 {
    match v {
        Value::Array(a) => usize_u64(a.items().len()),
        Value::Map(m) => usize_u64(m.len()),
        _ => 1,
    }
}

/// Whether `needle` occurs in `haystack`: Knuth-Morris-Pratt, linear in both lengths.
pub(crate) fn contains_slice(haystack: &[u8], needle: &[u8]) -> bool {
    if needle.is_empty() {
        return true;
    }
    // failure[i]: the length of the longest proper prefix of needle[..=i] that is also its suffix
    let mut failure: Vec<usize> = Vec::with_capacity(needle.len());
    failure.push(0);
    let mut k = 0usize;
    for &c in needle.iter().skip(1) {
        while k > 0 && needle.get(k) != Some(&c) {
            k = failure.get(k.saturating_sub(1)).copied().unwrap_or(0);
        }
        if needle.get(k) == Some(&c) {
            k = k.saturating_add(1);
        }
        failure.push(k);
    }
    let mut k = 0usize;
    for &c in haystack {
        while k > 0 && needle.get(k) != Some(&c) {
            k = failure.get(k.saturating_sub(1)).copied().unwrap_or(0);
        }
        if needle.get(k) == Some(&c) {
            k = k.saturating_add(1);
            if k == needle.len() {
                return true;
            }
        }
    }
    false
}

// ---- an order for deduplicating

/// A sort key for deduplicating: kinds first; scalars then compare structurally, and arrays and maps by their canonical
/// encoding, computed once. Total and consistent with equality; nothing is hashed, so values a program chooses cannot
/// be made to collide.
/// An upper bound on a canonical encoding's bytes per unit of the value's weight: a head is at most 9 bytes for a node
/// of weight at least 1, a text or byte string adds its length (which its weight counts), and a number of `l` whole
/// limbs takes at most `8 (l + 1)` bytes and a tag besides, for weight at least `1 + l`.
const ENCODING_PER_WEIGHT: u64 = 32;

struct SortKey<'v> {
    rank: u8,
    value: &'v Value,
    encoding: Vec<u8>,
}

impl<'v> SortKey<'v> {
    /// A collection's encoding is budgeted by its weight, which the operation has paid for: no encoding is longer than
    /// [`ENCODING_PER_WEIGHT`] bytes per unit of weight, so the budget only bounds the walk locally and is never hit.
    fn of(op: Op, value: &'v Value) -> Result<Self, JlvmError> {
        let encoded = |v: &Value| {
            let budget = v.weight().saturating_mul(ENCODING_PER_WEIGHT);
            cbor::encode_within(v, usize::try_from(budget).unwrap_or(usize::MAX))
                .map_err(|e| JlvmError::limit(alloc::format!("{}: {e}", op.wire())))
        };
        let (rank, encoding) = match value {
            Value::Null => (0, Vec::new()),
            Value::Bool(_) => (1, Vec::new()),
            Value::Int(_) => (2, Vec::new()),
            Value::Rational(_) => (3, Vec::new()),
            Value::Text(_) => (4, Vec::new()),
            Value::Bytes(_) => (5, Vec::new()),
            Value::Array(_) => (6, encoded(value)?),
            Value::Map(_) => (7, encoded(value)?),
        };
        Ok(Self { rank, value, encoding })
    }

    fn compare(&self, other: &Self) -> Ordering {
        self.rank.cmp(&other.rank).then_with(|| match (self.value, other.value) {
            (Value::Bool(x), Value::Bool(y)) => x.cmp(y),
            (Value::Int(x), Value::Int(y)) => x.cmp(y),
            (Value::Rational(x), Value::Rational(y)) => {
                x.numerator().cmp(y.numerator()).then_with(|| x.denominator().cmp(y.denominator()))
            }
            (Value::Text(x), Value::Text(y)) => x.cmp(y),
            (Value::Bytes(x), Value::Bytes(y)) => x.cmp(y),
            _ => self.encoding.cmp(&other.encoding),
        })
    }
}

/// `vs` without repeats, keeping each value's first occurrence, in order: a stable sort and a scan, O(n log n).
pub(crate) fn distinct(op: Op, vs: Vec<Value>) -> Result<Vec<Value>, JlvmError> {
    let mut keyed: Vec<(SortKey<'_>, usize)> = vs
        .iter()
        .map(|v| SortKey::of(op, v))
        .zip(0usize..)
        .map(|(k, i)| k.map(|k| (k, i)))
        .collect::<Result<_, _>>()?;
    keyed.sort_by(|a, b| a.0.compare(&b.0));
    let mut firsts: Vec<usize> = Vec::new();
    let mut previous: Option<&SortKey<'_>> = None;
    for (key, i) in &keyed {
        if previous.is_none_or(|p| p.compare(key) != Ordering::Equal) {
            firsts.push(*i);
        }
        previous = Some(key);
    }
    firsts.sort_unstable();
    drop(keyed);
    let mut keep = firsts.into_iter().peekable();
    Ok(vs.into_iter().zip(0usize..).filter(|(_, i)| keep.next_if_eq(i).is_some()).map(|(v, _)| v).collect())
}

/// Whether every element of `xs` is in `ys`: a binary search of `ys` in that order, O((m + n) log n).
pub(crate) fn subset(op: Op, xs: &[Value], ys: &[Value]) -> Result<bool, JlvmError> {
    let mut sorted: Vec<SortKey<'_>> = ys.iter().map(|y| SortKey::of(op, y)).collect::<Result<_, _>>()?;
    sorted.sort_by(SortKey::compare);
    xs.iter().try_fold(true, |all, x| {
        if !all {
            return Ok(false);
        }
        let k = SortKey::of(op, x)?;
        Ok(sorted.binary_search_by(|y| y.compare(&k)).is_ok())
    })
}

// ---- the JSON surface's bytes

/// The bytes a string denotes in the JSON surface, if it is `0x` followed by an even number of lowercase hex digits.
pub(crate) fn hex_bytes(s: &str) -> Option<Vec<u8>> {
    let digits = s.strip_prefix("0x")?;
    let lower = digits.bytes().all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b));
    if lower && digits.len() % 2 == 0 { dilf4s_core::hex::decode(digits).ok() } else { None }
}

/// Every leaf of `v` through `f`, rebuilding the containers: an explicit stack, no recursion on the value. `f` keeps
/// nulls null and maps to scalars only, so every rebuilt container is as valid as the original.
pub(crate) fn map_leaves(v: Value, f: &impl Fn(Value) -> Value) -> Value {
    enum Open {
        Array(Vec<Value>, alloc::vec::IntoIter<Value>),
        Map(Vec<(String, Value)>, String, alloc::vec::IntoIter<(String, Value)>),
    }
    let mut stack: Vec<Open> = Vec::new();
    // the value being delivered to the innermost open container
    let mut current = Some(v);
    loop {
        if let Some(v) = current.take() {
            match v {
                Value::Array(a) => {
                    stack.push(Open::Array(Vec::new(), a.into_items().into_iter()));
                }
                Value::Map(m) => {
                    stack.push(Open::Map(Vec::new(), String::new(), m.into_entries().into_iter()));
                }
                scalar => {
                    let done = f(scalar);
                    match stack.last_mut() {
                        Some(Open::Array(out, _)) => out.push(done),
                        Some(Open::Map(out, key, _)) => out.push((core::mem::take(key), done)),
                        None => return done,
                    }
                }
            }
        }
        // advance the innermost open container
        let next = match stack.last_mut() {
            Some(Open::Array(_, rest)) => rest.next(),
            Some(Open::Map(_, key, rest)) => rest.next().map(|(k, x)| {
                *key = k;
                x
            }),
            None => return Value::Null,
        };
        if next.is_some() {
            current = next;
            continue;
        }
        let built = match stack.pop() {
            Some(Open::Array(out, _)) => Value::array(out).unwrap_or(Value::Null),
            Some(Open::Map(out, _, _)) => Value::map(out).unwrap_or(Value::Null),
            None => return Value::Null,
        };
        match stack.last_mut() {
            Some(Open::Array(out, _)) => out.push(built),
            Some(Open::Map(out, key, _)) => out.push((core::mem::take(key), built)),
            None => return built,
        }
    }
}

/// Strings in `0x`-hex form become bytes, everywhere in the value (`JsonSurface.fromCanonicalJson`).
pub(crate) fn from_canonical_json(v: Value) -> Value {
    map_leaves(v, &|x| match x {
        Value::Text(s) => hex_bytes(&s).map_or(Value::Text(s), Value::bytes),
        other => other,
    })
}

/// Integers that fit 64 bits, as text (`i.toLong.toString`), for a computed path's segments.
pub(crate) fn small_integer_text(i: &BigInt) -> Option<String> {
    i64::try_from(i).ok().map(|n| n.to_string())
}
