//! The JSON mapping (docs/specs/canonical-encoding.md section 3).
//!
//! [`parse`] is a strict RFC 8259 parser that reads numbers exactly from their source text (never through a float),
//! rejects duplicate member names and unpaired surrogates, and keeps its own frame stack, so nesting is a typed error
//! rather than a stack overflow. [`render_within`] is the display view, within a byte limit the caller names, and fails
//! for values JSON cannot represent.

use alloc::collections::BTreeSet;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::fmt::Write as _;

use num_bigint::BigInt;
use num_traits::{One, Signed, Zero, pow};

use crate::error::{CanonicalError, DigitsOutOfRange, JsonSyntaxError, NotJson, NumberBound, ScaleOutOfRange};
use crate::preview::Preview;
use crate::value::{Array, MAX_DEPTH, Map, Rational, Text, Value};

/// The largest accepted `|fraction digits - exponent|` of a JSON number, bounding the `10^scale` it materialises.
pub const DEFAULT_MAX_SCALE: u32 = crate::generated::limits::as_u32::CANONICAL_MAX_SCALE;

/// The most significant digits a JSON number's significand (integer and fraction digits, leading zeros aside) or
/// exponent may have, read or written. Decimal conversion is superlinear, so the bound is checked on the text before
/// any conversion; it equals the largest scale, so every decimal of an accepted scale has a form within it.
pub const MAX_DIGITS: usize = crate::generated::limits::as_usize::CANONICAL_MAX_DIGITS;

/// Parses JSON text into a canonical value with the default scale bound.
pub fn parse(text: &str) -> Result<Value, CanonicalError> {
    parse_with_max_scale(text, DEFAULT_MAX_SCALE)
}

/// Parses JSON text into a canonical value, rejecting numbers whose scale exceeds `max_scale`.
pub fn parse_with_max_scale(text: &str, max_scale: u32) -> Result<Value, CanonicalError> {
    Parser { text, bytes: text.as_bytes(), max_scale }.run()
}

/// A text kept as generalised UTF-8 (WTF-8) while parsing: `\u` escapes may leave an unpaired surrogate, which is
/// only an error once the string (or, for a member name, its object) is complete, as in the reference parser.
type Wtf8 = Vec<u8>;

enum Frame {
    InArray(Vec<Value>),
    InObject { entries: Vec<(Wtf8, Value)>, seen: BTreeSet<Wtf8>, key: Wtf8 },
}

enum State {
    ExpectValue,
    ExpectKey,
    Deliver(Value),
}

enum Delivered {
    Next(State, usize),
    Done(Value),
}

struct Parser<'a> {
    text: &'a str,
    bytes: &'a [u8],
    max_scale: u32,
}

fn syntax(offset: usize, reason: JsonSyntaxError) -> CanonicalError {
    CanonicalError::JsonSyntax { offset, reason }
}

impl Parser<'_> {
    fn at(&self, i: usize) -> Option<u8> {
        self.bytes.get(i).copied()
    }

    fn skip_ws(&self, mut i: usize) -> usize {
        while matches!(self.at(i), Some(b' ' | b'\t' | b'\n' | b'\r')) {
            i = i.saturating_add(1);
        }
        i
    }

    fn digits(&self, mut i: usize) -> usize {
        while self.at(i).is_some_and(|b| b.is_ascii_digit()) {
            i = i.saturating_add(1);
        }
        i
    }

    fn run(&self) -> Result<Value, CanonicalError> {
        let mut stack: Vec<Frame> = Vec::new();
        let mut pos = 0usize;
        let mut state = State::ExpectValue;
        loop {
            (state, pos) = match state {
                State::ExpectValue => self.expect_value(&mut stack, pos)?,
                State::ExpectKey => self.expect_key(&mut stack, pos)?,
                State::Deliver(v) => match self.deliver(&mut stack, pos, v)? {
                    Delivered::Next(state, next) => (state, next),
                    Delivered::Done(v) => return Ok(v),
                },
            };
        }
    }

    fn expect_value(&self, stack: &mut Vec<Frame>, pos: usize) -> Result<(State, usize), CanonicalError> {
        let p = self.skip_ws(pos);
        match self.at(p) {
            // a container opened inside MAX_DEPTH open ones is refused at its bracket, an empty one too
            // (canonical-encoding.md section 3): the first fault in reading order
            Some(b'{' | b'[') if stack.len() >= MAX_DEPTH => Err(CanonicalError::DepthExceeded { limit: MAX_DEPTH }),
            Some(open @ (b'{' | b'[')) => {
                let q = self.skip_ws(p.saturating_add(1));
                let is_object = open == b'{';
                if self.at(q) == Some(if is_object { b'}' } else { b']' }) {
                    let empty = if is_object {
                        Value::Map(Map::new(Vec::new())?)
                    } else {
                        Value::Array(Array::new(Vec::new())?)
                    };
                    Ok((State::Deliver(empty), q.saturating_add(1)))
                } else if is_object {
                    stack.push(Frame::InObject { entries: Vec::new(), seen: BTreeSet::new(), key: Vec::new() });
                    Ok((State::ExpectKey, q))
                } else {
                    stack.push(Frame::InArray(Vec::new()));
                    Ok((State::ExpectValue, q))
                }
            }
            Some(_) => self.scalar(p).map(|(v, next)| (State::Deliver(v), next)),
            None => Err(syntax(p, JsonSyntaxError::ExpectedValue)),
        }
    }

    fn expect_key(&self, stack: &mut [Frame], pos: usize) -> Result<(State, usize), CanonicalError> {
        let p = self.skip_ws(pos);
        if self.at(p) != Some(b'"') {
            return Err(syntax(p, JsonSyntaxError::ExpectedMemberName));
        }
        let (name, next) = self.string(p)?;
        let Some(Frame::InObject { seen, key, .. }) = stack.last_mut() else {
            return Err(syntax(p, JsonSyntaxError::ExpectedValue));
        };
        if seen.contains(&name) {
            return Err(CanonicalError::DuplicateKey { key: Preview::of_wtf8(&name) });
        }
        let colon = self.skip_ws(next);
        if self.at(colon) != Some(b':') {
            return Err(syntax(colon, JsonSyntaxError::ExpectedColon));
        }
        seen.insert(name.clone());
        *key = name;
        Ok((State::ExpectValue, colon.saturating_add(1)))
    }

    /// Hands a complete value to the enclosing array or object, closing it when its bracket follows.
    fn deliver(&self, stack: &mut Vec<Frame>, pos: usize, v: Value) -> Result<Delivered, CanonicalError> {
        let p = self.skip_ws(pos);
        let after = p.saturating_add(1);
        match (stack.last_mut(), self.at(p)) {
            (None, _) if p == self.bytes.len() => Ok(Delivered::Done(v)),
            (None, _) => Err(syntax(p, JsonSyntaxError::TrailingContent)),
            (Some(Frame::InArray(items)), Some(b',')) => {
                items.push(v);
                Ok(Delivered::Next(State::ExpectValue, after))
            }
            (Some(Frame::InArray(_)), Some(b']')) => {
                let Some(Frame::InArray(mut items)) = stack.pop() else {
                    return Err(syntax(p, JsonSyntaxError::ExpectedCommaOrBracket));
                };
                items.push(v);
                Ok(Delivered::Next(State::Deliver(Value::Array(Array::new(items)?)), after))
            }
            (Some(Frame::InArray(_)), _) => Err(syntax(p, JsonSyntaxError::ExpectedCommaOrBracket)),
            (Some(Frame::InObject { entries, key, .. }), Some(b',')) => {
                entries.push((core::mem::take(key), v));
                Ok(Delivered::Next(State::ExpectKey, after))
            }
            (Some(Frame::InObject { .. }), Some(b'}')) => {
                let Some(Frame::InObject { mut entries, key, .. }) = stack.pop() else {
                    return Err(syntax(p, JsonSyntaxError::ExpectedCommaOrBrace));
                };
                entries.push((key, v));
                Ok(Delivered::Next(State::Deliver(Value::Map(object(entries)?)), after))
            }
            (Some(Frame::InObject { .. }), _) => Err(syntax(p, JsonSyntaxError::ExpectedCommaOrBrace)),
        }
    }

    fn scalar(&self, p: usize) -> Result<(Value, usize), CanonicalError> {
        let rest = self.text.get(p..).unwrap_or("");
        match self.at(p) {
            Some(b'"') => {
                let (s, next) = self.string(p)?;
                String::from_utf8(s).map(|t| (Value::text(t), next)).map_err(|_| CanonicalError::UnpairedSurrogate)
            }
            Some(b't') if rest.starts_with("true") => Ok((Value::Bool(true), p.saturating_add(4))),
            Some(b'f') if rest.starts_with("false") => Ok((Value::Bool(false), p.saturating_add(5))),
            Some(b'n') if rest.starts_with("null") => Ok((Value::Null, p.saturating_add(4))),
            Some(c) if c == b'-' || c.is_ascii_digit() => self.number(p),
            _ => Err(syntax(p, JsonSyntaxError::ExpectedValue)),
        }
    }

    /// `-? (0 | [1-9][0-9]*) (. [0-9]+)? ([eE] [+-]? [0-9]+)?`, converted exactly. Its digits are counted, then its
    /// scale checked, before any conversion. A refusal describes the number by its length and byte offset, never its
    /// text, whose size the caller chose.
    fn number(&self, p: usize) -> Result<(Value, usize), CanonicalError> {
        let negative = self.at(p) == Some(b'-');
        let int_start = if negative { p.saturating_add(1) } else { p };
        let int_end = self.digits(int_start);
        let has_frac = self.at(int_end) == Some(b'.');
        let frac_end = if has_frac { self.digits(int_end.saturating_add(1)) } else { int_end };
        let has_exp = matches!(self.at(frac_end), Some(b'e' | b'E'));
        let exp_sign = self.at(frac_end.saturating_add(1));
        let exp_start = if has_exp && matches!(exp_sign, Some(b'+' | b'-')) {
            frac_end.saturating_add(2)
        } else {
            frac_end.saturating_add(1)
        };
        let end = if has_exp { self.digits(exp_start) } else { frac_end };
        let slice = |a: usize, b: usize| self.text.get(a..b).unwrap_or("");
        let invalid = |bound| CanonicalError::InvalidNumber { length: end.saturating_sub(p), offset: p, bound };
        let bounded = |bound| invalid(Some(bound));
        let int_digits = slice(int_start, int_end);
        let frac_digits = if has_frac { slice(int_end.saturating_add(1), frac_end) } else { "" };
        let exp_digits = if has_exp { slice(exp_start, end) } else { "0" };
        let malformed = int_digits.is_empty()
            || (int_digits.len() > 1 && int_digits.starts_with('0'))
            || (has_frac && frac_digits.is_empty())
            || (has_exp && exp_digits.is_empty());
        if malformed {
            return Err(invalid(None));
        }
        // the integer digits have no leading zeros but a lone 0, after which the fraction's leading zeros are not counted
        let significand = if int_digits == "0" {
            frac_digits.trim_start_matches('0').len()
        } else {
            int_digits.len().saturating_add(frac_digits.len())
        };
        let exponent_digits = exp_digits.trim_start_matches('0').len();
        if let Some(digits) = [significand, exponent_digits].into_iter().find(|d| *d > MAX_DIGITS) {
            return Err(bounded(NumberBound::Digits(DigitsOutOfRange { digits, max_digits: MAX_DIGITS })));
        }
        // saturating: any exponent past i128 is far outside every accepted scale
        let magnitude = exp_digits
            .bytes()
            .fold(0i128, |acc, d| acc.saturating_mul(10).saturating_add(i128::from(d.wrapping_sub(b'0'))));
        let exponent = if has_exp && exp_sign == Some(b'-') { magnitude.saturating_neg() } else { magnitude };
        let scale = i128::try_from(frac_digits.len()).unwrap_or(i128::MAX).saturating_sub(exponent);
        let max = i128::from(self.max_scale);
        if scale.saturating_abs() > max {
            return Err(bounded(NumberBound::Scale(ScaleOutOfRange { scale, max_scale: self.max_scale })));
        }
        let mut all_digits = String::with_capacity(int_digits.len().saturating_add(frac_digits.len()));
        all_digits.push_str(int_digits);
        all_digits.push_str(frac_digits);
        let unscaled = BigInt::parse_bytes(all_digits.as_bytes(), 10).ok_or_else(|| invalid(None))?;
        let unscaled = if negative { -unscaled } else { unscaled };
        let magnitude = usize::try_from(scale.unsigned_abs()).map_err(|_| invalid(None))?;
        // a decimal's denominator is 10^scale: reduced by its primes 2 and 5 alone, never by a general gcd
        let value = if scale >= 0 {
            Value::decimal(&unscaled, magnitude)?
        } else {
            // a positive exponent can take an integer past MAX_INTEGER_BITS (9e9999): refused, as every producer does
            Value::integer(unscaled * pow(BigInt::from(10u8), magnitude))?
        };
        Ok((value, end))
    }

    /// A JSON string from its opening quote, as WTF-8, and the offset after the closing quote.
    fn string(&self, open: usize) -> Result<(Wtf8, usize), CanonicalError> {
        let mut out = Wtf8Builder::default();
        let mut i = open.saturating_add(1);
        loop {
            let Some(b) = self.at(i) else {
                return Err(syntax(i, JsonSyntaxError::UnterminatedString));
            };
            match b {
                b'"' => return Ok((out.finish(), i.saturating_add(1))),
                0x00..=0x1f => return Err(syntax(i, JsonSyntaxError::ControlCharacter)),
                b'\\' => {
                    let simple = match self.at(i.saturating_add(1)) {
                        Some(b'"') => Some(b'"'),
                        Some(b'\\') => Some(b'\\'),
                        Some(b'/') => Some(b'/'),
                        Some(b'b') => Some(0x08),
                        Some(b'f') => Some(0x0c),
                        Some(b'n') => Some(b'\n'),
                        Some(b'r') => Some(b'\r'),
                        Some(b't') => Some(b'\t'),
                        Some(b'u') => None,
                        _ => return Err(syntax(i, JsonSyntaxError::InvalidEscape)),
                    };
                    if let Some(c) = simple {
                        out.byte(c);
                        i = i.saturating_add(2);
                    } else {
                        let hex = self
                            .bytes
                            .get(i.saturating_add(2)..i.saturating_add(6))
                            .ok_or_else(|| syntax(i, JsonSyntaxError::InvalidEscape))?;
                        let unit = hex
                            .iter()
                            .try_fold(0u16, |acc, &h| hex_digit(h).map(|d| acc.wrapping_shl(4) | u16::from(d)));
                        out.unit(unit.ok_or_else(|| syntax(i, JsonSyntaxError::InvalidEscape))?);
                        i = i.saturating_add(6);
                    }
                }
                _ => {
                    out.byte(b);
                    i = i.saturating_add(1);
                }
            }
        }
    }
}

fn hex_digit(h: u8) -> Option<u8> {
    char::from(h).to_digit(16).and_then(|d| u8::try_from(d).ok())
}

/// Builds WTF-8: a high surrogate escape followed directly by a low one is a pair; any other surrogate stays unpaired.
#[derive(Default)]
struct Wtf8Builder {
    out: Vec<u8>,
    high: Option<u16>,
}

impl Wtf8Builder {
    fn flush(&mut self) {
        if let Some(h) = self.high.take() {
            self.lone(h);
        }
    }

    /// The 3-byte generalised UTF-8 form of a surrogate code unit.
    fn lone(&mut self, s: u16) {
        let [hi, lo] = s.to_be_bytes();
        self.out.extend_from_slice(&[0xe0 | (hi >> 4), 0x80 | ((hi & 0x0f) << 2) | (lo >> 6), 0x80 | (lo & 0x3f)]);
    }

    fn byte(&mut self, b: u8) {
        self.flush();
        self.out.push(b);
    }

    fn unit(&mut self, u: u16) {
        let is_high = (0xd800..=0xdbff).contains(&u);
        let is_low = (0xdc00..=0xdfff).contains(&u);
        match self.high.take() {
            Some(h) if is_low => {
                // the 20-bit payload is added to 0x10000, not ORed: payloads from U+20000 up have bit 16 set
                let code = ((u32::from(h & 0x3ff) << 10) | u32::from(u & 0x3ff)).wrapping_add(0x1_0000);
                self.char(code);
                return;
            }
            Some(h) => self.lone(h),
            None => {}
        }
        if is_high {
            self.high = Some(u);
        } else if is_low {
            self.lone(u);
        } else {
            self.char(u32::from(u));
        }
    }

    fn char(&mut self, code: u32) {
        let c = char::from_u32(code).unwrap_or(char::REPLACEMENT_CHARACTER);
        let mut buf = [0u8; 4];
        self.out.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
    }

    fn finish(mut self) -> Wtf8 {
        self.flush();
        self.out
    }
}

/// An object's members as a map, checked in member order as the reference implementation does: an unpaired
/// surrogate in a name, then the depth limit.
fn object(entries: Vec<(Wtf8, Value)>) -> Result<Map, CanonicalError> {
    let named = entries
        .into_iter()
        .map(|(k, v)| String::from_utf8(k).map(|k| (k, v)).map_err(|_| CanonicalError::UnpairedSurrogate))
        .collect::<Result<Vec<_>, _>>()?;
    Map::new(named)
}

// ------------------------------------------------------------------------------------------------ render

/// Why a value was not rendered: JSON cannot represent it, or its JSON is longer than the limit.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RenderError {
    /// Bytes, a rational with no terminating decimal, or a number past the bounds [`parse`] accepts.
    Canonical(CanonicalError),
    /// The JSON is longer than the limit (docs/specs/conformance.md: the limit alone, nothing measured).
    TooLarge(crate::cbor::TooLarge),
}

impl core::fmt::Display for RenderError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Canonical(e) => e.fmt(f),
            Self::TooLarge(t) => write!(f, "the JSON is over {} bytes", t.limit),
        }
    }
}

impl core::error::Error for RenderError {}

impl From<CanonicalError> for RenderError {
    fn from(e: CanonicalError) -> Self {
        Self::Canonical(e)
    }
}

/// Compact JSON with object members in code-point order of their names, if it is at most `max_bytes` long in UTF-8.
/// Fails for bytes, non-terminating rationals and over-long numbers, and with `TooLarge` at the first part found past
/// the limit, whichever comes first. Never recursive: its explicit stack holds at most [`MAX_DEPTH`] open containers.
///
/// O(min(weight, `max_bytes`)) apart from sorting object members: a value's JSON is at least as long in UTF-8 as its
/// weight (each node writes at least a byte, a text or key its UTF-8 and quotes, a number of `l` whole limbs more than
/// `l` digits), so each part is refused in O(1) when its weight is over what is left, and every part written fits;
/// escaped text is counted before it is written. There is no render without a limit.
pub fn render_within(value: &Value, max_bytes: usize) -> Result<String, RenderError> {
    let mut out = Budget { out: String::new(), max: max_bytes };
    write_json(&mut out, value)?;
    Ok(out.out)
}

/// The output so far and its limit, in UTF-8 bytes.
struct Budget {
    out: String,
    max: usize,
}

impl Budget {
    fn refused(&self) -> RenderError {
        RenderError::TooLarge(crate::cbor::TooLarge { limit: self.max })
    }

    fn left(&self) -> usize {
        self.max.saturating_sub(self.out.len())
    }

    /// Whether a part of at least `weight` bytes can still fit.
    fn admits(&self, weight: u64) -> Result<(), RenderError> {
        if usize::try_from(weight).is_ok_and(|w| w <= self.left()) { Ok(()) } else { Err(self.refused()) }
    }

    /// Writes `piece` if it fits.
    fn push(&mut self, piece: &str) -> Result<(), RenderError> {
        if piece.len() > self.left() {
            return Err(self.refused());
        }
        self.out.push_str(piece);
        Ok(())
    }

    /// Writes `s` quoted and escaped, counted before it is built.
    fn quote(&mut self, s: &str) -> Result<(), RenderError> {
        if quoted_len(s) > self.left() {
            return Err(self.refused());
        }
        quote(&mut self.out, s);
        Ok(())
    }
}

/// An array or object being rendered: the elements still to come, and whether one has been written.
enum Open<'a> {
    Items { items: core::slice::Iter<'a, Value>, started: bool },
    Members { members: alloc::vec::IntoIter<(&'a Text, &'a Value)>, started: bool },
}

/// Never recursive: an explicit stack holds one open container per level of nesting (at most [`MAX_DEPTH`]).
fn write_json(out: &mut Budget, value: &Value) -> Result<(), RenderError> {
    let mut stack: Vec<Open<'_>> = Vec::new();
    let mut next = Some(value);
    loop {
        if let Some(v) = next {
            out.admits(v.weight())?;
        }
        match next.take() {
            Some(Value::Null) => out.push("null")?,
            Some(Value::Bool(b)) => out.push(if *b { "true" } else { "false" })?,
            Some(Value::Int(n)) => out.push(&integer_text(n))?,
            Some(Value::Rational(r)) => {
                out.push(&decimal(r).map_err(|kind| CanonicalError::NotJsonRepresentable { kind })?)?;
            }
            Some(Value::Text(s)) => out.quote(s)?,
            Some(Value::Bytes(_)) => return Err(CanonicalError::NotJsonRepresentable { kind: NotJson::Bytes }.into()),
            Some(Value::Array(a)) => {
                out.push("[")?;
                stack.push(Open::Items { items: a.items().iter(), started: false });
            }
            Some(Value::Map(m)) => {
                out.push("{")?;
                // Rust orders str by bytes, which for UTF-8 is code-point order
                let mut members: Vec<(&Text, &Value)> = m.entries().collect();
                members.sort_by(|(a, _), (b, _)| a.as_str().cmp(b.as_str()));
                stack.push(Open::Members { members: members.into_iter(), started: false });
            }
            None => {}
        }
        let Some(open) = stack.last_mut() else {
            return Ok(());
        };
        let closed = match open {
            Open::Items { items, started } => match items.next() {
                None => Some("]"),
                Some(v) => {
                    if *started {
                        out.push(",")?;
                    }
                    *started = true;
                    next = Some(v);
                    None
                }
            },
            Open::Members { members, started } => match members.next() {
                None => Some("}"),
                Some((k, v)) => {
                    if *started {
                        out.push(",")?;
                    }
                    *started = true;
                    out.quote(k)?;
                    out.push(":")?;
                    next = Some(v);
                    None
                }
            },
        };
        if let Some(close) = closed {
            out.push(close)?;
            stack.pop();
        }
    }
}

/// An integer's decimal, which [`parse`] reads back: total, since every integer is within
/// [`crate::value::MAX_INTEGER_BITS`], so it has at most [`MAX_DIGITS`] digits and converting it is bounded work.
fn integer_text(n: &crate::value::Int) -> String {
    n.to_string()
}

/// `text`, a plain decimal, when its significant digits and scale are within the bounds [`parse`] accepts.
fn within(text: String) -> Option<String> {
    let unsigned = text.strip_prefix('-').unwrap_or(&text);
    let (int_part, frac_part) = unsigned.split_once('.').unwrap_or((unsigned, ""));
    let significand = if int_part == "0" {
        frac_part.trim_start_matches('0').len()
    } else {
        int_part.trim_start_matches('0').len().saturating_add(frac_part.len())
    };
    let scale_ok = u32::try_from(frac_part.len()).is_ok_and(|s| s <= DEFAULT_MAX_SCALE);
    (significand <= MAX_DIGITS && scale_ok).then_some(text)
}

/// The length of `s` quoted and escaped as [`quote`] writes it, in UTF-8 bytes.
fn quoted_len(s: &str) -> usize {
    s.chars().fold(2usize, |n, c| {
        let w = match c {
            '"' | '\\' | '\n' | '\r' | '\t' | '\u{8}' | '\u{c}' => 2,
            c if c < ' ' => 6,
            c => c.len_utf8(),
        };
        n.saturating_add(w)
    })
}

fn quote(out: &mut String, s: &str) {
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{8}' => out.push_str("\\b"),
            '\u{c}' => out.push_str("\\f"),
            c if c < ' ' => {
                let _ = write!(out, "\\u{:04x}", u32::from(c));
            }
            c => out.push(c),
        }
    }
    out.push('"');
}

/// `log2(5)` in millionths, rounded down: the bits per factor of five.
const MICRO_BITS_PER_FIVE: u64 = 2_321_928;

/// `Some((a, b))` when `d = 2^a 5^b`, so that `1/d` terminates, and the big-number divisions and exponentiations it
/// took: at most four, whatever `d`. The twos are the low zero bits; the power of five is estimated from the bit length
/// (`5^b` has `floor(b log2 5) + 1` bits, and the neighbours absorb any rounding) and checked by exponentiation, never
/// found one division per factor (14,306 of them for `5^14306`, the widest a rational's denominator is).
pub(crate) fn twos_and_fives(d: &BigInt) -> (Option<(u64, u64)>, u32) {
    let twos = d.trailing_zeros().unwrap_or(0);
    let rest: BigInt = d >> twos;
    if rest.is_one() {
        return (Some((twos, 0)), 0);
    }
    if !(&rest % 5u8).is_zero() {
        return (None, 1);
    }
    let estimate =
        rest.bits().saturating_sub(1).saturating_mul(1_000_000).checked_div(MICRO_BITS_PER_FIVE).unwrap_or(0);
    // b log2 5 lies in [bits - 1, bits), under half a factor wide, so b is the estimate's ceiling but for rounding
    let candidates = [estimate.saturating_add(1), estimate, estimate.saturating_sub(1)];
    let five = BigInt::from(5u8);
    let (found, steps) = candidates.iter().fold((None, 1u32), |(found, steps), &b| match (found, usize::try_from(b)) {
        (Some(f), _) => (Some(f), steps),
        (None, Ok(e)) if b > 0 => ((pow(five.clone(), e) == rest).then_some(b), steps.saturating_add(1)),
        (None, _) => (None, steps),
    });
    (found.map(|b| (twos, b)), steps)
}

/// The exact decimal of `r`, within the bounds [`parse`] accepts: [`NotJson::NonTerminatingRational`] when its
/// denominator has prime factors other than 2 and 5, [`NotJson::OverLong`] past the bounds, both found from the
/// denominator's factors ([`twos_and_fives`]) before any digit is produced.
pub fn decimal(r: &Rational) -> Result<String, NotJson> {
    let d = r.denominator();
    let (twos, fives) = twos_and_fives(d).0.ok_or(NotJson::NonTerminatingRational)?;
    let scale = usize::try_from(twos.max(fives))
        .ok()
        .filter(|s| u32::try_from(*s).is_ok_and(|s| s <= DEFAULT_MAX_SCALE))
        .ok_or(NotJson::OverLong)?;
    // 10^scale is a multiple of d, so the division is exact
    let scaled = r.numerator() * (pow(BigInt::from(10u8), scale) / d);
    let digits = scaled.abs().to_string();
    let mut padded = "0".repeat(scale.saturating_add(1).saturating_sub(digits.len()));
    padded.push_str(&digits);
    let (int_part, frac_part) = padded.split_at(padded.len().saturating_sub(scale));
    let sign = if scaled.is_negative() { "-" } else { "" };
    within(alloc::format!("{sign}{int_part}.{frac_part}")).ok_or(NotJson::OverLong)
}

#[cfg(test)]
mod tests {
    use num_bigint::BigInt;
    use num_traits::{One, pow};

    use super::twos_and_fives;

    /// Factoring a denominator takes at most four big-number divisions and exponentiations, whatever its width:
    /// 2^33219, 5^14306 or 3 5^14000 is factored (or found not to terminate) at once, never one division per factor,
    /// and the scale bound then refuses 1/2^33218 and 1/5^14306, the widest of each, before any digit is produced.
    #[test]
    fn factoring_a_denominator_is_bounded_whatever_its_factors() {
        let (two, five) = (BigInt::from(2u8), BigInt::from(5u8));
        let cases = [
            (pow(two.clone(), 33_219), Some((33_219, 0))),
            (pow(five.clone(), 14_306), Some((0, 14_306))),
            (pow(two.clone(), 3) * pow(five.clone(), 9_000), Some((3, 9_000))),
            (pow(five.clone(), 14_000) * 3u8, None),
            (pow(two, 100) * 3u8, None),
            (five.clone(), Some((0, 1))),
            (BigInt::one(), Some((0, 0))),
        ];
        for (d, expected) in cases {
            let (factors, steps) = twos_and_fives(&d);
            assert_eq!(factors, expected, "{} bits", d.bits());
            assert!(steps <= 4, "{steps} divisions and exponentiations for {} bits", d.bits());
        }
        // every power of five up to the widest denominator is found from its estimate
        for b in (1..=300).chain([1_000, 4_096, 9_999, 10_000, 10_001, 14_306]) {
            let (factors, steps) = twos_and_fives(&pow(five.clone(), b));
            assert_eq!(factors, Some((0, u64::try_from(b).unwrap())), "5^{b}");
            assert!(steps <= 4, "{steps} steps for 5^{b}");
        }
        // one division and one exponentiation when the estimate is right
        assert_eq!(twos_and_fives(&pow(five.clone(), 14_306)).1, 2);
        // past the scale bound, refused from the factors, before any digit is produced
        for d in [pow(BigInt::from(2u8), 33_218), pow(five, 14_306)] {
            let Ok(crate::Value::Rational(r)) = crate::Value::number(&BigInt::one(), &d) else { panic!("a rational") };
            assert_eq!(super::decimal(&r), Err(crate::error::NotJson::OverLong), "{} bits", d.bits());
        }
    }
}
