//! The dilf4s deterministic CBOR profile (docs/specs/canonical-encoding.md section 2).

use alloc::vec::Vec;
use core::fmt;

use num_bigint::{BigInt, Sign};
use num_traits::One;

use crate::error::{CanonicalError, NonCanonicalReason, UnsupportedItem};
use crate::hash::{Hasher, Hashing};
use crate::value::{Array, Entries, MAX_DEPTH, MAX_RATIONAL_BITS, MAX_WEIGHT, Map, Rational, Text, Value, bit_length};

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

/// Streams the canonical encoding of `value` into `hashing`, as [`encode`] would write it, without building it: the
/// same walk, writing into the hash instead of a buffer. O([`Value::weight`]) time, as [`encode`]; the memory is the
/// hash's state, [`STAGED_BYTES`] staged and the walk's stacks (one entry per level of nesting, and a map's own height),
/// never the encoding.
pub fn encode_into<H: Hasher>(hashing: &mut Hashing<H>, value: &Value) {
    let mut staged = Staged::new(hashing);
    match write_value(&mut staged, value) {
        Ok(()) => staged.flush(),
        Err(never) => match never {},
    }
}

/// A value whose canonical encoding or JSON is longer than a limit. It carries the limit only: how far past it the output
/// goes is never measured (docs/specs/conformance.md).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TooLarge {
    /// The limit, in bytes.
    pub limit: usize,
}

impl fmt::Display for TooLarge {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "the encoding is over {} bytes", self.limit)
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
    let refused = TooLarge { limit: max };
    if !usize::try_from(value.weight()).is_ok_and(|w| w <= countable && w <= max) {
        return Err(refused);
    }
    let mut sink = Budget { out, len: 0, max, countable };
    write_value(&mut sink, value).map_err(|Over| refused)?;
    Ok(sink.len)
}

/// Where encoded bytes go.
pub(crate) trait Sink {
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

/// The most bytes a [`Staged`] sink holds: the walk's small pieces (heads, keys, limbs) reach the hash this many at a
/// time instead of one call each.
pub const STAGED_BYTES: usize = 1024;

/// Writes into a hash: small pieces are staged in a fixed array and absorbed together; a piece that does not fit goes
/// to the hash in place. It holds at most [`STAGED_BYTES`] of the encoding; call [`Staged::flush`] when done.
pub(crate) struct Staged<'a, H: Hasher> {
    into: &'a mut Hashing<H>,
    buf: [u8; STAGED_BYTES],
    len: usize,
}

impl<'a, H: Hasher> Staged<'a, H> {
    pub(crate) const fn new(into: &'a mut Hashing<H>) -> Self {
        Self { into, buf: [0; STAGED_BYTES], len: 0 }
    }

    /// Absorbs what is staged.
    pub(crate) fn flush(&mut self) {
        if let Some(staged) = self.buf.get(..self.len) {
            self.into.update(staged);
        }
        self.len = 0;
    }
}

impl<H: Hasher> Sink for Staged<'_, H> {
    type Error = core::convert::Infallible;
    fn put(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        let end = self.len.saturating_add(bytes.len());
        if let Some(room) = self.buf.get_mut(self.len..end) {
            room.copy_from_slice(bytes);
            self.len = end;
        } else {
            self.flush();
            if let Some(room) = self.buf.get_mut(..bytes.len()) {
                room.copy_from_slice(bytes);
                self.len = bytes.len();
            } else {
                self.into.update(bytes);
            }
        }
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

/// The output would pass the budget, or what is countable.
struct Over;

impl Budget<'_> {
    /// The length after `n` more bytes, if within the budget.
    fn reach(&self, n: u64) -> Result<usize, Over> {
        let next = usize::try_from(n)
            .ok()
            .and_then(|n| self.len.checked_add(n))
            .filter(|&next| next <= self.countable)
            .ok_or(Over)?;
        if next > self.max { Err(Over) } else { Ok(next) }
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

/// An array or map being written: the elements still to come.
enum Open<'a> {
    Items(core::slice::Iter<'a, Value>),
    Entries(Entries<'a>),
}

/// Never recursive: an explicit stack holds one open container per level of nesting (at most [`MAX_DEPTH`]), and a
/// map's entries come from its own explicit stack. Every node writes its first byte before its children (the budget
/// relies on it).
pub(crate) fn write_value<S: Sink>(out: &mut S, value: &Value) -> Result<(), S::Error> {
    let mut stack: Vec<Open<'_>> = Vec::new();
    let mut next = Some(value);
    loop {
        match next.take() {
            Some(Value::Array(a)) => {
                head(out, MAJOR_ARRAY, length(a.items().len()))?;
                stack.push(Open::Items(a.items().iter()));
            }
            Some(Value::Map(m)) => {
                // entries come in canonical key order
                head(out, MAJOR_MAP, length(m.len()))?;
                stack.push(Open::Entries(m.entries()));
            }
            Some(Value::Null) => out.put(&[0xf6])?,
            Some(Value::Bool(false)) => out.put(&[0xf4])?,
            Some(Value::Bool(true)) => out.put(&[0xf5])?,
            Some(Value::Int(n)) => write_integer(out, n)?,
            Some(Value::Rational(r)) => {
                head(out, MAJOR_TAG, TAG_RATIONAL)?;
                head(out, MAJOR_ARRAY, 2)?;
                write_integer(out, r.numerator())?;
                write_integer(out, r.denominator())?;
            }
            Some(Value::Text(s)) => write_text(out, s)?,
            Some(Value::Bytes(b)) => {
                head(out, MAJOR_BYTES, length(b.len()))?;
                out.put(b)?;
            }
            None => {}
        }
        let Some(open) = stack.last_mut() else {
            return Ok(());
        };
        match open {
            Open::Items(items) => next = items.next(),
            Open::Entries(entries) => {
                if let Some((k, v)) = entries.next() {
                    write_text(out, k)?;
                    next = Some(v);
                }
            }
        }
        if next.is_none() {
            stack.pop();
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
    decode_with(bytes, max_depth, &Rational::new)
}

/// What builds a decoded rational from parts within [`MAX_RATIONAL_BITS`]: [`Rational::new`], which runs the one gcd
/// that checks lowest terms. A test passes one that counts its calls and the widths it sees.
type Reduce<'a> = &'a dyn Fn(BigInt, BigInt) -> Option<Rational>;

/// [`decode_with_limit`], building rationals with `reduce`.
fn decode_with(bytes: &[u8], max_depth: usize, reduce: Reduce<'_>) -> Result<Value, CanonicalError> {
    Decoder { input: bytes, max_depth: max_depth.min(MAX_DEPTH), stack: Vec::new(), reduce }.run()
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
    reduce: Reduce<'a>,
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

    /// In order: the pair, the numerator and its width, the denominator and its width, the denominator's range, and
    /// last the one gcd that checks lowest terms (docs/specs/conformance.md section 5).
    fn read_rational(&self, pos: usize, end: usize) -> Result<(Value, usize), CanonicalError> {
        let non_canonical = |reason| CanonicalError::NonCanonical { offset: pos, reason };
        let pair = self.read_head(end)?;
        if pair.major != MAJOR_ARRAY || pair.argument != 2 {
            return Err(non_canonical(NonCanonicalReason::RationalNotPair));
        }
        let (numerator, after_numerator) = self.read_part(pair.end)?;
        let (denominator, after) = self.read_part(after_numerator)?;
        if denominator < BigInt::from(2) {
            return Err(non_canonical(NonCanonicalReason::RationalDenominator));
        }
        (self.reduce)(numerator, denominator)
            .map(|r| (Value::Rational(r), after))
            .ok_or_else(|| non_canonical(NonCanonicalReason::RationalNotReduced))
    }

    /// A rational's numerator or denominator, refused when wider than [`MAX_RATIONAL_BITS`]: before any gcd, so a wide
    /// part costs no more than reading it.
    fn read_part(&self, pos: usize) -> Result<(BigInt, usize), CanonicalError> {
        let (n, end) = self.read_integer(pos)?;
        if bit_length(&n) > MAX_RATIONAL_BITS {
            Err(CanonicalError::RationalTooWide { limit: MAX_RATIONAL_BITS })
        } else {
            Ok((n, end))
        }
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
    use alloc::vec::Vec;
    use core::cell::Cell;

    use num_bigint::BigInt;
    use num_traits::One;

    use super::{MAX_RATIONAL_BITS, Rational, TooLarge, Value, decode, decode_with, encode, within};
    use crate::error::{CanonicalError, NonCanonicalReason};
    use crate::value::{MAX_DEPTH, bit_length};

    /// The consecutive Fibonacci numbers `(F(k + 1), F(k))` with `F(k + 1)` exactly `bits` wide: coprime, and the
    /// pair whose gcd takes the most steps for its width.
    fn fibonacci(bits: u64) -> (BigInt, BigInt) {
        let (mut a, mut b) = (BigInt::one(), BigInt::one());
        while b.bits() < bits {
            let next = &a + &b;
            a = core::mem::replace(&mut b, next);
        }
        (b, a)
    }

    fn rational(n: &BigInt, d: &BigInt) -> Value {
        let v = Value::number(n, d).unwrap();
        assert!(matches!(v, Value::Rational(_)), "{} / {} bits is a rational", bit_length(n), bit_length(d));
        v
    }

    /// Decodes `bytes` through a [`Rational::new`] that counts its calls (each one gcd) and the widest part it was given.
    fn counted(bytes: &[u8]) -> (Result<Value, CanonicalError>, usize, u64) {
        let (calls, widest) = (Cell::new(0usize), Cell::new(0u64));
        let reduce = |n: BigInt, d: BigInt| {
            calls.set(calls.get().saturating_add(1));
            widest.set(widest.get().max(bit_length(&n)).max(bit_length(&d)));
            Rational::new(n, d)
        };
        let decoded = decode_with(bytes, MAX_DEPTH, &reduce);
        (decoded, calls.get(), widest.get())
    }

    fn too_wide() -> Result<Value, CanonicalError> {
        Err(CanonicalError::RationalTooWide { limit: MAX_RATIONAL_BITS })
    }

    /// No rational is built that a decoder refuses: every constructor checks the width, after reducing.
    #[test]
    fn constructors_refuse_what_decoding_refuses() {
        let edge = BigInt::one() << MAX_RATIONAL_BITS;
        let three = BigInt::from(3u8);
        assert_eq!(Value::number(&edge, &three), Err(crate::value::NumberError::RationalTooWide));
        assert_eq!(Value::number(&three, &edge), Err(crate::value::NumberError::RationalTooWide));
        assert_eq!(Rational::new(edge.clone(), three.clone()), None);
        // reduced first: (2^33221 / 6) is 2^33220 / 3, still too wide; 2^33221 / 2^33220 is the integer 2
        assert_eq!(Value::number(&(&edge * 2u8), &(&three * 2u8)), Err(crate::value::NumberError::RationalTooWide));
        assert_eq!(Value::number(&(&edge * 2u8), &edge), Ok(Value::int(2u8)));
        // integers are unbounded
        assert_eq!(decode(&encode(&Value::int(&edge * &edge))), Ok(Value::int(&edge * &edge)));
        let at_bound = rational(&(&edge - BigInt::one()), &BigInt::from(2u8));
        assert_eq!(decode(&encode(&at_bound)), Ok(at_bound));
    }

    #[test]
    fn the_rational_bound_is_the_width_of_ten_to_the_digit_bound() {
        let ten_to_the_bound = num_traits::pow(BigInt::from(10u8), crate::json::MAX_DIGITS);
        assert_eq!(bit_length(&ten_to_the_bound), MAX_RATIONAL_BITS);
    }

    /// Decoding `k` maximal rationals runs exactly `k` gcds, each of parts within the bound: its work is at most `k`
    /// gcds of [`MAX_RATIONAL_BITS`]-bit numbers, linear in the input (each takes over 8 KiB). A wider part is refused
    /// before any gcd.
    #[test]
    fn decoding_a_rational_runs_one_gcd_of_bounded_parts() {
        let (n, d) = fibonacci(MAX_RATIONAL_BITS);
        assert_eq!((bit_length(&n), bit_length(&d)), (MAX_RATIONAL_BITS, MAX_RATIONAL_BITS - 1));
        let k = 8;
        let maximal = Value::array((0..k).map(|_| rational(&n, &d)).collect()).unwrap();
        let bytes = encode(&maximal);
        let (decoded, calls, widest) = counted(&bytes);
        assert_eq!(decoded.as_ref().ok(), Some(&maximal), "maximal rationals decode");
        assert_eq!(calls, k, "one gcd per rational");
        assert_eq!(widest, MAX_RATIONAL_BITS, "of parts within the bound");
        assert!(bytes.len() > k * 8300, "each takes {} bytes", bytes.len() / k);

        // one bit wider, numerator or denominator, and not in lowest terms either: refused with no gcd
        let wide = BigInt::one() << MAX_RATIONAL_BITS;
        let two = BigInt::from(2u8);
        for (numerator, denominator) in [
            (&wide * &two, &wide + &two),
            (two.clone(), &wide * &two),
            (-(&wide) - BigInt::one(), two.clone()),
            (BigInt::one(), -(&wide) * &two),
        ] {
            let bytes = rational_bytes(&numerator, &denominator);
            let (decoded, calls, _) = counted(&bytes);
            assert_eq!(decoded, too_wide(), "{} / {} bits", bit_length(&numerator), bit_length(&denominator));
            assert_eq!(calls, 0, "no gcd");
        }
    }

    /// Tag 30 around `[n, d]`, whatever `n` and `d` are.
    fn rational_bytes(n: &BigInt, d: &BigInt) -> Vec<u8> {
        let mut out = vec![0xd8, 0x1e, 0x82];
        super::write_integer(&mut out, n).unwrap();
        super::write_integer(&mut out, d).unwrap();
        out
    }

    #[test]
    fn the_bound_is_on_java_bit_length_for_either_sign() {
        let edge = BigInt::one() << MAX_RATIONAL_BITS;
        let (two, three) = (BigInt::from(2u8), BigInt::from(3u8));
        // 2^33220 - 1 and -2^33220 are 33220 bits wide; 2^33220 and -2^33220 - 1 are 33221
        for (n, d) in
            [(&edge - BigInt::one(), two.clone()), (-(&edge), three.clone()), (two.clone(), &edge - BigInt::one())]
        {
            assert_eq!(
                decode(&rational_bytes(&n, &d)),
                Ok(rational(&n, &d)),
                "{} / {} bits",
                bit_length(&n),
                bit_length(&d)
            );
        }
        for (n, d) in
            [(edge.clone(), three.clone()), (-(&edge) - BigInt::one(), three.clone()), (three.clone(), edge.clone())]
        {
            assert_eq!(decode(&rational_bytes(&n, &d)), too_wide(), "{} / {} bits", bit_length(&n), bit_length(&d));
        }
    }

    #[test]
    fn a_wide_numerator_is_refused_before_the_denominator_is_read() {
        let mut bytes = vec![0xd8, 0x1e, 0x82];
        super::write_integer(&mut bytes, &(BigInt::one() << MAX_RATIONAL_BITS)).unwrap();
        bytes.extend_from_slice(&[0x61, 0x61]);
        assert_eq!(decode(&bytes), too_wide());
        // within the bound, the same denominator is not an integer
        let mut narrow = vec![0xd8, 0x1e, 0x82, 0x01, 0x61, 0x61];
        assert!(matches!(
            decode(&narrow),
            Err(CanonicalError::NonCanonical { reason: NonCanonicalReason::RationalNotInteger, .. })
        ));
        narrow.truncate(4);
        assert!(matches!(decode(&narrow), Err(CanonicalError::UnexpectedEnd { .. })));
    }

    fn doubled(leaf: Value, k: usize) -> Value {
        (0..k).fold(leaf, |v, _| Value::array(vec![v.clone(), v]).unwrap())
    }

    /// A 32-bit `usize` stands in as a small countable bound: no count clamps or wraps into a success.
    #[test]
    fn lengths_past_what_is_countable_are_refused_not_clamped() {
        // 2^32 - 1 nodes of null encode in 2^33 - 1 bytes: over a 32-bit count, refused by weight alone
        let nulls = doubled(Value::Null, 32);
        let bound = usize::try_from(u32::MAX).unwrap();
        assert_eq!(within(&nulls, bound, bound, None), Err(TooLarge { limit: bound }), "32-bit");
        // within the countable weight but over it in bytes (u64::MAX: weight 2, 9 bytes): 383 weight, 1279 bytes
        let ints = doubled(Value::int(u64::MAX), 7);
        assert_eq!(ints.weight(), 383);
        assert_eq!(super::encode(&ints).len(), 1279);
        assert_eq!(within(&ints, 1000, 1000, None), Err(TooLarge { limit: 1000 }), "past the count");
        let mut out = vec![];
        assert_eq!(within(&ints, 1000, 1000, Some(&mut out)).map(|_| ()), Err(TooLarge { limit: 1000 }));
        assert!(out.capacity() <= 1000, "the buffer stays within the budget");
        // under a smaller limit, the refusal names that limit
        assert_eq!(within(&ints, 500, 1000, None), Err(TooLarge { limit: 500 }));
        // and a countable length within the limit is exact
        assert_eq!(within(&ints, 1279, 2000, None), Ok(1279));
    }

    /// What the walk hands a sink: how many bytes, and the longest piece it made itself (a head, or an integer's
    /// limb). Any other piece must be a text or byte string of the value, lent in place.
    struct Pieces<'v> {
        total: usize,
        longest_made: usize,
        lent: &'v [&'v [u8]],
    }

    impl super::Sink for Pieces<'_> {
        type Error = core::convert::Infallible;
        fn put(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
            self.total = self.total.saturating_add(bytes.len());
            // only a long piece is looked up among the lent ones: the bound is on those
            let long = bytes.len() > 9;
            if long && !self.lent.iter().any(|l| core::ptr::eq(l.as_ptr(), bytes.as_ptr()) && l.len() == bytes.len()) {
                self.longest_made = self.longest_made.max(bytes.len());
            }
            Ok(())
        }
        fn room(&mut self, _: u64) -> Result<(), Self::Error> {
            Ok(())
        }
    }

    /// The pieces of `value`'s encoding: (total, longest piece made by the walk rather than lent from the value).
    fn pieces(value: &Value, lent: &[&[u8]]) -> (usize, usize) {
        let mut p = Pieces { total: 0, longest_made: 0, lent };
        match super::write_value(&mut p, value) {
            Ok(()) => (p.total, p.longest_made),
            Err(never) => match never {},
        }
    }

    /// The streamed hashing path holds no part of the encoding: whatever the value's size, every piece the walk makes
    /// is at most 9 bytes (a head or a limb), and text and byte strings are lent from the value, never copied. The
    /// hashing sink then stages at most [`super::STAGED_BYTES`] and the hash a block, so memory is independent of the
    /// encoding's length.
    #[test]
    fn the_hashing_path_buffers_nothing_proportional_to_the_value() {
        use alloc::vec::Vec;
        use num_bigint::BigInt;
        for n in [10usize, 1_000, 100_000] {
            let wide = Value::array((0..n).map(|i| Value::int(u64::try_from(i).unwrap() << 40)).collect()).unwrap();
            let keyed =
                Value::map((0..n).map(|i| (alloc::format!("k{i}"), Value::Bool(i % 2 == 0))).collect::<Vec<_>>())
                    .unwrap();
            let big = Value::int(BigInt::from(1) << (n * 8));
            let text = Value::text("é".repeat(n));
            let bytes = Value::bytes(vec![0x5a; n]);
            let lent_text = [text.as_str().unwrap().as_bytes()];
            let lent_bytes = [bytes.as_bytes().unwrap()];
            let Value::Map(m) = &keyed else { panic!("a map") };
            let lent_keys: Vec<&[u8]> = m.keys().map(|k| k.as_str().as_bytes()).collect();
            for (name, v, lent) in [
                ("wide", &wide, &[][..]),
                ("keyed", &keyed, &lent_keys[..]),
                ("big", &big, &[][..]),
                ("text", &text, &lent_text[..]),
                ("bytes", &bytes, &lent_bytes[..]),
            ] {
                let (total, longest_made) = pieces(v, lent);
                assert_eq!(total, super::encode(v).len(), "{name} of {n}: every byte, once");
                assert!(longest_made <= 9, "{name} of {n}: the walk made a piece of {longest_made} bytes");
            }
        }
    }
}
