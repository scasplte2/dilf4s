//! Small pure functions the codecs and the JLVM rest on: hex, integer bit lengths, previews, the canonical key order,
//! operation names and the choice of a schema alternative.

use core::cmp::Ordering;

use dilf4s_core::codec::{Alternative, select_alternative};
use dilf4s_core::value::canonical_key_order;
use dilf4s_core::{Int, Preview, Value, cbor, hex};
use dilf4s_jlvm::{Op, OpClass};

use crate::{Broken, holds};

/// `hex::encode(bytes)` is two lowercase digits per byte, and `hex::decode` reads it back.
pub fn hex_round_trip(bytes: &[u8]) -> Result<(), Broken> {
    let text = hex::encode(bytes);
    let lower = text.bytes().all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b));
    holds(lower && Some(text.len()) == bytes.len().checked_mul(2), Broken::Hex)?;
    holds(hex::decode(&text).as_deref() == Ok(bytes), Broken::Hex)
}

/// `hex::decode(text)`: what it reads is half as long as `text`, which is its encoding up to the case of its digits.
pub fn hex_decode(text: &str) -> Result<(), Broken> {
    let Ok(bytes) = hex::decode(text) else { return Ok(()) };
    holds(Some(text.len()) == bytes.len().checked_mul(2), Broken::Hex)?;
    holds(hex::encode(&bytes).eq_ignore_ascii_case(text), Broken::Hex)
}

/// Java's `BigInteger.bitLength`, by its definition: the bits of the minimal two's-complement form without its sign
/// bit, which for a negative `n` are those of `!n = -n - 1`.
fn java_bit_length(n: i128) -> u64 {
    let magnitude = if n < 0 { !n } else { n };
    u64::from(i128::BITS.saturating_sub(magnitude.leading_zeros()))
}

/// The integer `n` as a value: its bit length is Java's `bitLength` (the reference's), and its weight is
/// `1 + bitLength / 64` (canonical-encoding.md section 1).
pub fn bit_length(n: i128) -> Result<(), Broken> {
    let int = Int::from(n);
    let bits = java_bit_length(n);
    holds(int.bit_length() == bits, Broken::BitLength)?;
    holds(int.weight() == (bits / 64).saturating_add(1) && Value::int(n).weight() == int.weight(), Broken::BitLength)
}

/// The text `text` as a value: its cached UTF-16 length (the reference's `String.length`, which map searches and
/// paths are charged by) is `text`'s count of UTF-16 code units, and its weight is one plus its UTF-8 length.
pub fn text_measures(text: &str) -> Result<(), Broken> {
    let value = Value::text(text);
    let utf16 = u64::try_from(text.encode_utf16().count()).ok();
    let units = match &value {
        Value::Text(t) => Some(t.utf16_len()),
        _ => None,
    };
    holds(units.is_some() && units == utf16, Broken::TextMeasures)?;
    holds(u64::try_from(text.len()).ok().and_then(|l| l.checked_add(1)) == Some(value.weight()), Broken::TextMeasures)
}

/// `Preview::of(text)`: its head is `text`'s first [`dilf4s_core::preview::MAX_CODE_POINTS`] code points, it is
/// truncated exactly when `text` has more, and its length is `text`'s.
pub fn preview(text: &str) -> Result<(), Broken> {
    let p = Preview::of(text);
    let max = dilf4s_core::preview::MAX_CODE_POINTS;
    let head_end = text.char_indices().nth(max).map_or(text.len(), |(i, _)| i);
    holds(Some(p.head()) == text.get(..head_end), Broken::Preview)?;
    holds(p.is_truncated() == (text.chars().count() > max) && p.length() == text.len(), Broken::Preview)
}

/// `canonical_key_order(a, b)` is the order of the keys' canonical encodings, bytewise (RFC 8949 section 4.2.1), which
/// is the order a map's keys are encoded in and the decoder checks.
pub fn key_order(a: &str, b: &str) -> Result<(), Broken> {
    let encoded: Ordering = cbor::encode(&Value::text(a)).cmp(&cbor::encode(&Value::text(b)));
    holds(canonical_key_order(a, b) == encoded, Broken::KeyOrder)
}

/// `Op::from_wire(name)` is the operation whose wire name is `name`, if any, and `OpClass::from_name` reads the same
/// but for its four aliases. (That every operation's wire name reads back as it is a finite claim: `tests/` checks it.)
pub fn op_wire(name: &str) -> Result<(), Broken> {
    let wire = Op::from_wire(name);
    holds(wire.is_none_or(|op| op.wire() == name), Broken::OpWire)?;
    let alias = matches!(name, "getKey" | "setKey" | "deleteKey" | "size");
    holds(alias || <Op as OpClass>::from_name(name) == wire, Broken::OpWire)
}

/// Whether a map record with these `required` and `allowed` keys fits `value`.
fn fits(value: &Value, required: &[&str], allowed: &[&str]) -> bool {
    match value {
        Value::Map(m) => required.iter().all(|r| m.get(r).is_some()) && m.keys().all(|k| allowed.contains(&k.as_str())),
        _ => false,
    }
}

/// `select_alternative(value, records, texts)`: a map is the first record whose required keys it has and which allows
/// every key it has; a text is the first text alternative it equals; anything else, or a value no alternative fits, is
/// refused.
pub fn alternative(value: &Value, records: &[(&[&str], &[&str])], texts: &[&str]) -> Result<(), Broken> {
    let expected = match value {
        Value::Map(_) => records.iter().position(|(r, a)| fits(value, r, a)).map(Alternative::Record),
        Value::Text(t) => texts.iter().position(|x| *x == t.as_str()).map(Alternative::Text),
        _ => None,
    };
    holds(select_alternative(value, records, texts).ok() == expected, Broken::Alternative)
}
