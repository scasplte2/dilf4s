//! The dilf4s deterministic CBOR profile (docs/specs/canonical-encoding.md section 2).

use alloc::vec::Vec;
use core::fmt;

use num_bigint::{BigInt, Sign};
use num_integer::Integer;
use num_traits::One;

use crate::error::{CanonicalError, NonCanonicalReason, UnsupportedItem};
use crate::value::{Array, MAX_DEPTH, MAX_WEIGHT, Map, Rational, Text, Value, bit_length};

const TAG_POSITIVE_BIGNUM: u64 = 2;
const TAG_NEGATIVE_BIGNUM: u64 = 3;
const TAG_RATIONAL: u64 = 30;

const MAJOR_UNSIGNED: u8 = 0;
const MAJOR_NEGATIVE: u8 = 1;
const MAJOR_BYTES: u8 = 2;
const MAJOR_TEXT: u8 = 3;
const MAJOR_ARRAY: u8 = 4;
const MAJOR_MAP: u8 = 5;
const MAJOR_TAG: u8 = 6;
const MAJOR_SIMPLE: u8 = 7;

// ------------------------------------------------------------------------------------------------ encoding

/// The canonical encoding of `value`. Total: every [`Value`] has exactly one.
///
/// It costs O([`Value::weight`]) time and memory, which for a value built with shared parts (`[x, x]` nested `k`
/// deep weighs `2^k`) can be far more than the value occupies. Encode a value not known to be small with
/// [`encode_within`], or check its weight first.
pub fn encode(value: &Value) -> Vec<u8> {
    let mut out = Vec::new();
    match write_value(&mut out, value) {
        Ok(()) => out,
        Err(never) => match never {},
    }
}

/// A value whose canonical encoding is longer than a limit.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TooLarge {
    /// The limit, in bytes.
    pub limit: usize,
    /// A lower bound on the encoding's length, above the limit (counting stops at the first byte past it), or `None`
    /// when the length is not even countable: over `usize::MAX`, or `2^63 - 1` bytes or more (a weight that saturates).
    pub at_least: Option<usize>,
}

impl fmt::Display for TooLarge {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.at_least {
            Some(n) => write!(f, "the encoding is at least {n} bytes, over {}", self.limit),
            None => write!(f, "the encoding is too long to count, over {}", self.limit),
        }
    }
}

impl core::error::Error for TooLarge {}

/// The canonical encoding of `value`, if it is at most `max_bytes` long; it never holds more than `max_bytes`.
///
/// O(min(weight, `max_bytes`)), and no allocation beyond the output: every encoding is at least as long as the value's
/// [`Value::weight`] (each node's head is a byte, text and byte strings add their length, a number of `l` whole limbs
/// takes at least `l` bytes more), so a weight over the limit is refused at once; each node visited writes a byte
/// before its children, so the walk stops within `max_bytes + 1` nodes; and a number's length is known from its bit
/// length, so it is checked against the budget before its bytes are streamed out limb by limb.
pub fn encode_within(value: &Value, max_bytes: usize) -> Result<Vec<u8>, TooLarge> {
    let mut out = Vec::new();
    within(value, max_bytes, countable(), Some(&mut out))?;
    Ok(out)
}

/// The length of `value`'s canonical encoding, if it is at most `max_bytes`, counted without encoding it, with the
/// bounds of [`encode_within`].
pub fn encoded_len_within(value: &Value, max_bytes: usize) -> Result<usize, TooLarge> {
    within(value, max_bytes, countable(), None)
}

/// The longest length counted: below the saturated weight `2^63 - 1`, and within `usize`. Every count is checked
/// against it, so no counter clamps or wraps into a false success.
fn countable() -> usize {
    usize::try_from(MAX_WEIGHT.saturating_sub(1)).unwrap_or(usize::MAX)
}

/// [`encode_within`] and [`encoded_len_within`], counting lengths up to `countable` (a parameter so tests can stand
/// in for a 32-bit `usize`).
fn within(value: &Value, max: usize, countable: usize, out: Option<&mut Vec<u8>>) -> Result<usize, TooLarge> {
    match usize::try_from(value.weight()).ok().filter(|&w| w <= countable) {
        None => return Err(TooLarge { limit: max, at_least: None }),
        Some(w) if w > max => return Err(TooLarge { limit: max, at_least: Some(w) }),
        Some(_) => {}
    }
    let mut sink = Budget { out, len: 0, max, countable };
    write_value(&mut sink, value).map_err(|Over(at_least)| TooLarge { limit: max, at_least })?;
    Ok(sink.len)
}

/// Where encoded bytes go.
trait Sink {
    type Error;
    fn put(&mut self, bytes: &[u8]) -> Result<(), Self::Error>;
    /// Whether `n` more bytes fit, checked before they are produced.
    fn room(&mut self, n: u64) -> Result<(), Self::Error>;
}

impl Sink for Vec<u8> {
    type Error = core::convert::Infallible;
    fn put(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        self.extend_from_slice(bytes);
        Ok(())
    }
    fn room(&mut self, _: u64) -> Result<(), Self::Error> {
        Ok(())
    }
}

/// A byte budget, optionally writing: its buffer never grows past `max`, and its count never past `countable`.
struct Budget<'a> {
    out: Option<&'a mut Vec<u8>>,
    len: usize,
    max: usize,
    countable: usize,
}

/// The length the output would have reached, or `None` past what is countable.
struct Over(Option<usize>);

impl Budget<'_> {
    /// The length after `n` more bytes, if within the budget.
    fn reach(&self, n: u64) -> Result<usize, Over> {
        let next = usize::try_from(n)
            .ok()
            .and_then(|n| self.len.checked_add(n))
            .filter(|&next| next <= self.countable)
            .ok_or(Over(None))?;
        if next > self.max { Err(Over(Some(next))) } else { Ok(next) }
    }
}

impl Sink for Budget<'_> {
    type Error = Over;
    fn put(&mut self, bytes: &[u8]) -> Result<(), Over> {
        let next = self.reach(length(bytes.len()))?;
        if let Some(out) = self.out.as_deref_mut() {
            if out.capacity() < next {
                let target = next.max(out.capacity().saturating_mul(2)).min(self.max);
                out.reserve_exact(target.saturating_sub(out.len()));
            }
            out.extend_from_slice(bytes);
        }
        self.len = next;
        Ok(())
    }
    fn room(&mut self, n: u64) -> Result<(), Over> {
        self.reach(n).map(|_| ())
    }
}

/// Recursion is bounded: a [`Value`] never nests deeper than [`MAX_DEPTH`]. Every node writes its first byte before
/// its children (the budget relies on it).
fn write_value<S: Sink>(out: &mut S, value: &Value) -> Result<(), S::Error> {
    match value {
        Value::Null => out.put(&[0xf6]),
        Value::Bool(false) => out.put(&[0xf4]),
        Value::Bool(true) => out.put(&[0xf5]),
        Value::Int(n) => write_integer(out, n),
        Value::Rational(r) => {
            head(out, MAJOR_TAG, TAG_RATIONAL)?;
            head(out, MAJOR_ARRAY, 2)?;
            write_integer(out, r.numerator())?;
            write_integer(out, r.denominator())
        }
        Value::Text(s) => write_text(out, s),
        Value::Bytes(b) => {
            head(out, MAJOR_BYTES, length(b.len()))?;
            out.put(b)
        }
        Value::Array(a) => {
            head(out, MAJOR_ARRAY, length(a.items().len()))?;
            a.items().iter().try_for_each(|v| write_value(out, v))
        }
        Value::Map(m) => {
            // entries are held in canonical key order
            head(out, MAJOR_MAP, length(m.len()))?;
            m.entries().try_for_each(|(k, v)| {
                write_text(out, k)?;
                write_value(out, v)
            })
        }
    }
}

fn write_text<S: Sink>(out: &mut S, s: &str) -> Result<(), S::Error> {
    head(out, MAJOR_TEXT, length(s.len()))?;
    out.put(s.as_bytes())
}

/// A length as a CBOR argument; `usize` is at most 64 bits on every supported target.
fn length(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

/// An integer, without materialising it: CBOR carries `n` for `n >= 0` and `-1 - n` (that is `|n| - 1`) for `n < 0`,
/// read here limb by limb from `|n|`'s own digits, so nothing the size of `n` is allocated.
fn write_integer<S: Sink>(out: &mut S, n: &BigInt) -> Result<(), S::Error> {
    let negative = n.sign() == Sign::Minus;
    let magnitude = n.magnitude();
    // |n| - 1 borrows through |n|'s zero limbs below its lowest non-zero one, which loses 1
    let lowest = if negative { magnitude.trailing_zeros().unwrap_or(0) / 64 } else { 0 };
    let limb = |i: u64, digit: u64| {
        if !negative {
            return digit;
        }
        match i.cmp(&lowest) {
            core::cmp::Ordering::Less => u64::MAX,
            core::cmp::Ordering::Equal => digit.wrapping_sub(1),
            core::cmp::Ordering::Greater => digit,
        }
    };
    let bits = bit_length(n);
    let major = if negative { MAJOR_NEGATIVE } else { MAJOR_UNSIGNED };
    if bits <= 64 {
        return head(out, major, magnitude.iter_u64_digits().next().map_or(0, |d| limb(0, d)));
    }
    let bytes = bits.div_ceil(8);
    let limbs = bits.div_ceil(64);
    head(out, MAJOR_TAG, if negative { TAG_NEGATIVE_BIGNUM } else { TAG_POSITIVE_BIGNUM })?;
    head(out, MAJOR_BYTES, bytes)?;
    out.room(bytes)?;
    // the top limb carries only its significant bytes; a limb above it (|n| = 2^(64k), borrowed to zero) carries none
    let top_bytes = bytes.saturating_sub(limbs.saturating_sub(1).saturating_mul(8));
    let skip = usize::try_from(8u64.saturating_sub(top_bytes)).unwrap_or(0);
    for (i, digit) in magnitude.iter_u64_digits().enumerate().rev() {
        let i = u64::try_from(i).unwrap_or(u64::MAX);
        if i < limbs {
            let from = if i.saturating_add(1) == limbs { skip } else { 0 };
            out.put(limb(i, digit).to_be_bytes().get(from..).unwrap_or(&[]))?;
        }
    }
    Ok(())
}

/// The shortest-form initial byte and argument.
fn head<S: Sink>(out: &mut S, major: u8, argument: u64) -> Result<(), S::Error> {
    let mt = major.wrapping_shl(5);
    let [b0, b1, b2, b3, b4, b5, b6, b7] = argument.to_be_bytes();
    if argument < 24 {
        out.put(&[mt | b7])
    } else if argument < 0x100 {
        out.put(&[mt | 0x18, b7])
    } else if argument < 0x1_0000 {
        out.put(&[mt | 0x19, b6, b7])
    } else if argument < 0x1_0000_0000 {
        out.put(&[mt | 0x1a, b4, b5, b6, b7])
    } else {
        out.put(&[mt | 0x1b, b0, b1, b2, b3, b4, b5, b6, b7])
    }
}

// ------------------------------------------------------------------------------------------------ decoding

/// Strictly decodes one canonical value: any other encoding of a value, trailing bytes, or nesting deeper than
/// [`MAX_DEPTH`] is an error. When it succeeds, `encode(&decode(b)?) == b`.
pub fn decode(bytes: &[u8]) -> Result<Value, CanonicalError> {
    decode_with_limit(bytes, MAX_DEPTH)
}

/// [`decode`] with a lower nesting limit (it never exceeds [`MAX_DEPTH`]). The decoder keeps its own frame stack,
/// so input of any nesting is a typed error, never a stack overflow.
pub fn decode_with_limit(bytes: &[u8], max_depth: usize) -> Result<Value, CanonicalError> {
    Decoder { input: bytes, max_depth: max_depth.min(MAX_DEPTH), stack: Vec::new() }.run()
}

struct Head {
    major: u8,
    argument: u64,
    end: usize,
}

enum Item {
    Scalar(Value),
    OpenArray(u64),
    OpenMap(u64),
}

/// A key read, waiting for its value: the key and the span of its encoding.
struct PendingKey {
    key: Text,
    start: usize,
    end: usize,
}

enum Frame {
    Array { remaining: u64, items: Vec<Value> },
    Map { remaining: u64, entries: Vec<(Text, Value)>, last_key: Option<(usize, usize)>, pending: Option<PendingKey> },
}

struct Decoder<'a> {
    input: &'a [u8],
    max_depth: usize,
    stack: Vec<Frame>,
}

/// What to do after a value is complete: read the next item at an offset, or finish.
enum Next {
    Read(usize),
    Done(Value),
}

impl Decoder<'_> {
    fn run(mut self) -> Result<Value, CanonicalError> {
        let mut pos = 0;
        loop {
            let (item, end) = self.read_item(pos)?;
            let value = match item {
                Item::Scalar(v) => v,
                _ if self.stack.len() >= self.max_depth => {
                    return Err(CanonicalError::DepthExceeded { limit: self.max_depth });
                }
                Item::OpenArray(0) => Value::Array(Array::new(Vec::new())?),
                Item::OpenMap(0) => Value::Map(Map::from_sorted(Vec::new())?),
                Item::OpenArray(n) => {
                    self.stack.push(Frame::Array { remaining: n, items: Vec::new() });
                    pos = end;
                    continue;
                }
                Item::OpenMap(n) => {
                    self.stack.push(Frame::Map { remaining: n, entries: Vec::new(), last_key: None, pending: None });
                    pos = end;
                    continue;
                }
            };
            match self.deliver(value, pos, end)? {
                Next::Read(next) => pos = next,
                Next::Done(v) => return Ok(v),
            }
        }
    }

    /// Hands a complete value (encoded at `start..end`) to the enclosing frame, closing every frame it completes.
    fn deliver(&mut self, mut value: Value, mut start: usize, end: usize) -> Result<Next, CanonicalError> {
        loop {
            let Some(frame) = self.stack.last_mut() else {
                return if end == self.input.len() {
                    Ok(Next::Done(value))
                } else {
                    Err(CanonicalError::TrailingBytes { offset: end })
                };
            };
            let complete = match frame {
                Frame::Array { remaining, items } => {
                    items.push(value);
                    *remaining = remaining.saturating_sub(1);
                    *remaining == 0
                }
                Frame::Map { pending: pending @ None, last_key, .. } => {
                    let Value::Text(key) = value else {
                        return Err(CanonicalError::Unsupported { offset: start, item: UnsupportedItem::NonTextKey });
                    };
                    let ordered = last_key.is_none_or(|(ls, le)| self.input.get(ls..le) < self.input.get(start..end));
                    if !ordered {
                        return Err(CanonicalError::NonCanonical {
                            offset: start,
                            reason: NonCanonicalReason::KeyOrder,
                        });
                    }
                    *pending = Some(PendingKey { key, start, end });
                    return Ok(Next::Read(end));
                }
                Frame::Map { remaining, entries, last_key, pending } => {
                    if value == Value::Null {
                        return Err(CanonicalError::NonCanonical {
                            offset: start,
                            reason: NonCanonicalReason::NullMapValue,
                        });
                    }
                    if let Some(k) = pending.take() {
                        *last_key = Some((k.start, k.end));
                        entries.push((k.key, value));
                    }
                    *remaining = remaining.saturating_sub(1);
                    *remaining == 0
                }
            };
            if !complete {
                return Ok(Next::Read(end));
            }
            value = match self.stack.pop() {
                Some(Frame::Array { items, .. }) => Value::Array(Array::new(items)?),
                Some(Frame::Map { entries, .. }) => Value::Map(Map::from_sorted(entries)?),
                None => return Err(CanonicalError::UnexpectedEnd { offset: end }),
            };
            // the start of a closed container is not tracked; only a key's start is ever used, and a container
            // delivered as a key is rejected before its offset matters
            start = end;
        }
    }

    fn read_item(&self, pos: usize) -> Result<(Item, usize), CanonicalError> {
        let Head { major, argument, end } = self.read_head(pos)?;
        match major {
            MAJOR_UNSIGNED => Ok((Item::Scalar(Value::int(argument)), end)),
            MAJOR_NEGATIVE => Ok((Item::Scalar(Value::int(-BigInt::from(argument) - BigInt::one())), end)),
            MAJOR_BYTES => self.span(end, argument).map(|(b, e)| (Item::Scalar(Value::bytes(b)), e)),
            MAJOR_TEXT => {
                let (b, e) = self.span(end, argument)?;
                let text = core::str::from_utf8(b).map_err(|_| CanonicalError::InvalidUtf8 { offset: end })?;
                Ok((Item::Scalar(Value::text(text)), e))
            }
            MAJOR_ARRAY => self.count(end, argument, 1).map(|n| (Item::OpenArray(n), end)),
            MAJOR_MAP => self.count(end, argument, 2).map(|n| (Item::OpenMap(n), end)),
            MAJOR_TAG => self.read_tagged(pos, argument, end).map(|(v, e)| (Item::Scalar(v), e)),
            _ => match argument {
                20 => Ok((Item::Scalar(Value::Bool(false)), end)),
                21 => Ok((Item::Scalar(Value::Bool(true)), end)),
                22 => Ok((Item::Scalar(Value::Null), end)),
                s => Err(CanonicalError::Unsupported {
                    offset: pos,
                    item: UnsupportedItem::SimpleValue(u8::try_from(s).unwrap_or(u8::MAX)),
                }),
            },
        }
    }

    fn read_head(&self, pos: usize) -> Result<Head, CanonicalError> {
        let initial = *self.input.get(pos).ok_or(CanonicalError::UnexpectedEnd { offset: pos })?;
        let (major, info) = (initial >> 5, initial & 0x1f);
        let next = pos.saturating_add(1);
        let sized = |len: u64, minimum: u64| -> Result<Head, CanonicalError> {
            let (b, end) = self.span(next, len)?;
            let argument = b.iter().fold(0u64, |acc, &x| acc.wrapping_shl(8) | u64::from(x));
            if argument < minimum {
                Err(CanonicalError::NonCanonical { offset: pos, reason: NonCanonicalReason::NonShortestArgument })
            } else {
                Ok(Head { major, argument, end })
            }
        };
        if info < 24 {
            Ok(Head { major, argument: u64::from(info), end: next })
        } else if major == MAJOR_SIMPLE {
            Err(CanonicalError::Unsupported { offset: pos, item: UnsupportedItem::FloatOrSimple(info) })
        } else {
            match info {
                24 => sized(1, 24),
                25 => sized(2, 0x100),
                26 => sized(4, 0x1_0000),
                27 => sized(8, 0x1_0000_0000),
                31 => Err(CanonicalError::Unsupported { offset: pos, item: UnsupportedItem::IndefiniteLength }),
                _ => Err(CanonicalError::Unsupported { offset: pos, item: UnsupportedItem::ReservedInfo(info) }),
            }
        }
    }

    fn read_tagged(&self, pos: usize, tag: u64, end: usize) -> Result<(Value, usize), CanonicalError> {
        match tag {
            TAG_POSITIVE_BIGNUM | TAG_NEGATIVE_BIGNUM => {
                self.read_bignum(pos, tag, end).map(|(n, e)| (Value::int(n), e))
            }
            TAG_RATIONAL => self.read_rational(pos, end),
            _ => Err(CanonicalError::Unsupported { offset: pos, item: UnsupportedItem::Tag(tag) }),
        }
    }

    fn read_bignum(&self, pos: usize, tag: u64, end: usize) -> Result<(BigInt, usize), CanonicalError> {
        let body = self.read_head(end)?;
        if body.major != MAJOR_BYTES {
            return Err(CanonicalError::NonCanonical { offset: pos, reason: NonCanonicalReason::BignumNotBytes });
        }
        let (b, e) = self.span(body.end, body.argument)?;
        if b.first().is_none_or(|&first| first == 0) {
            Err(CanonicalError::NonCanonical { offset: pos, reason: NonCanonicalReason::BignumLeadingZero })
        } else if b.len() <= 8 {
            Err(CanonicalError::NonCanonical { offset: pos, reason: NonCanonicalReason::BignumInRange })
        } else {
            let magnitude = BigInt::from_bytes_be(Sign::Plus, b);
            Ok((if tag == TAG_POSITIVE_BIGNUM { magnitude } else { -magnitude - BigInt::one() }, e))
        }
    }

    fn read_integer(&self, pos: usize) -> Result<(BigInt, usize), CanonicalError> {
        let Head { major, argument, end } = self.read_head(pos)?;
        match (major, argument) {
            (MAJOR_UNSIGNED, n) => Ok((BigInt::from(n), end)),
            (MAJOR_NEGATIVE, n) => Ok((-BigInt::from(n) - BigInt::one(), end)),
            (MAJOR_TAG, t @ (TAG_POSITIVE_BIGNUM | TAG_NEGATIVE_BIGNUM)) => self.read_bignum(pos, t, end),
            _ => Err(CanonicalError::NonCanonical { offset: pos, reason: NonCanonicalReason::RationalNotInteger }),
        }
    }

    fn read_rational(&self, pos: usize, end: usize) -> Result<(Value, usize), CanonicalError> {
        let non_canonical = |reason| CanonicalError::NonCanonical { offset: pos, reason };
        let pair = self.read_head(end)?;
        if pair.major != MAJOR_ARRAY || pair.argument != 2 {
            return Err(non_canonical(NonCanonicalReason::RationalNotPair));
        }
        let (numerator, after_numerator) = self.read_integer(pair.end)?;
        let (denominator, after) = self.read_integer(after_numerator)?;
        if denominator < BigInt::from(2) {
            return Err(non_canonical(NonCanonicalReason::RationalDenominator));
        }
        if !numerator.gcd(&denominator).is_one() {
            return Err(non_canonical(NonCanonicalReason::RationalNotReduced));
        }
        Rational::new(numerator, denominator)
            .map(|r| (Value::Rational(r), after))
            .ok_or_else(|| non_canonical(NonCanonicalReason::RationalNotReduced))
    }

    /// `len` bytes from `pos`, and the offset after them.
    fn span(&self, pos: usize, len: u64) -> Result<(&[u8], usize), CanonicalError> {
        let truncated = CanonicalError::UnexpectedEnd { offset: pos };
        let available = self.input.len().saturating_sub(pos);
        let len = usize::try_from(len).ok().filter(|&l| l <= available).ok_or_else(|| truncated.clone())?;
        let end = pos.saturating_add(len);
        self.input.get(pos..end).map(|b| (b, end)).ok_or(truncated)
    }

    /// Every element needs at least one byte, so a count beyond the remaining input is truncated, not allocated.
    fn count(&self, pos: usize, n: u64, per_entry: u64) -> Result<u64, CanonicalError> {
        let available = length(self.input.len().saturating_sub(pos));
        if n > available.checked_div(per_entry).unwrap_or(0) {
            Err(CanonicalError::UnexpectedEnd { offset: pos })
        } else {
            Ok(n)
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use super::{TooLarge, Value, within};

    fn doubled(leaf: Value, k: usize) -> Value {
        (0..k).fold(leaf, |v, _| Value::array(vec![v.clone(), v]).unwrap())
    }

    /// A 32-bit `usize` stands in as a small countable bound: no count clamps or wraps into a success.
    #[test]
    fn lengths_past_what_is_countable_are_refused_not_clamped() {
        // 2^32 - 1 nodes of null encode in 2^33 - 1 bytes: over a 32-bit count, refused by weight alone
        let nulls = doubled(Value::Null, 32);
        let bound = usize::try_from(u32::MAX).unwrap();
        assert_eq!(within(&nulls, bound, bound, None), Err(TooLarge { limit: bound, at_least: None }), "32-bit");
        // within the countable weight but over it in bytes (u64::MAX: weight 2, 9 bytes): 383 weight, 1279 bytes
        let ints = doubled(Value::int(u64::MAX), 7);
        assert_eq!(ints.weight(), 383);
        assert_eq!(super::encode(&ints).len(), 1279);
        assert_eq!(within(&ints, 1000, 1000, None), Err(TooLarge { limit: 1000, at_least: None }), "past the count");
        let mut out = vec![];
        assert_eq!(
            within(&ints, 1000, 1000, Some(&mut out)).map(|_| ()),
            Err(TooLarge { limit: 1000, at_least: None })
        );
        assert!(out.capacity() <= 1000, "the buffer stays within the budget");
        // under a smaller limit, the first length past it is reported
        let e = within(&ints, 500, 1000, None).unwrap_err();
        assert!(e.at_least.is_some_and(|n| n > 500 && n <= 1000), "{e:?}");
        // and a countable length within the limit is exact
        assert_eq!(within(&ints, 1279, 2000, None), Ok(1279));
    }
}
