//! The canonical data model's codecs (`dilf4s-core`, docs/specs/canonical-encoding.md): the strict CBOR decoder, the
//! JSON parser and the JSON renderer.

use dilf4s_core::cbor::{self, TooLarge};
use dilf4s_core::json::{self, RenderError};
use dilf4s_core::{MAX_DEPTH, Value};

use crate::{Broken, holds};

// Values are compared by their canonical encodings, which are their identity (canonical-encoding.md section 2), and
// cost a model checker far less than `Value`'s own equality (`tests/` checks the two agree).

/// Whether `value` weighs at most `len`.
fn weighs_at_most(value: &Value, len: usize) -> bool {
    usize::try_from(value.weight()).is_ok_and(|w| w <= len)
}

/// `cbor::decode(bytes)`. When it decodes a value:
/// - the value's canonical encoding is `bytes`: one encoding per value, and no other accepted (section 2);
/// - it weighs at most `bytes.len()`, the bound `encode_within` refuses by before it encodes anything;
/// - `encode_within` and `encoded_len_within` within `bytes.len()` give the encoding and its length, and within a byte
///   less refuse with that limit.
pub fn cbor_decode(bytes: &[u8]) -> Result<(), Broken> {
    let Ok(value) = cbor::decode(bytes) else { return Ok(()) };
    let len = bytes.len();
    holds(cbor::encode(&value) == bytes, Broken::CborRoundTrip)?;
    holds(weighs_at_most(&value, len), Broken::CborWeight)?;
    holds(cbor::encoded_len_within(&value, len) == Ok(len), Broken::CborWithin)?;
    holds(cbor::encode_within(&value, len).as_deref() == Ok(bytes), Broken::CborWithin)?;
    // every encoding is at least a byte long
    let short = len.checked_sub(1).ok_or(Broken::CborWithin)?;
    holds(cbor::encoded_len_within(&value, short) == Err(TooLarge { limit: short }), Broken::CborWithin)
}

/// `cbor::decode_with_limit(bytes, max_depth)` succeeds exactly when `cbor::decode(bytes)` does with a value at most
/// `max_depth` deep (the limit never exceeds [`MAX_DEPTH`]), and then with the same value: the one `bytes` encode.
pub fn cbor_depth_limit(bytes: &[u8], max_depth: usize) -> Result<(), Broken> {
    let limited = cbor::decode_with_limit(bytes, max_depth);
    let within = cbor::decode(bytes).is_ok_and(|v| v.depth() <= max_depth.min(MAX_DEPTH));
    holds(limited.is_ok() == within, Broken::CborDepthLimit)?;
    limited.map_or(Ok(()), |v| holds(cbor::encode(&v) == bytes, Broken::CborDepthLimit))
}

/// `json::parse_with_max_scale(text, max_scale)`. When it parses a value:
/// - the value renders, and its rendering parses back to it under the same scale bound (so rendering is canonical:
///   rendering that value again gives the same text);
/// - the rendering is at least as long as the value's weight, the bound `render_within` refuses by before it writes,
///   and within a byte less it is refused with that limit.
pub fn json_parse(text: &str, max_scale: u32) -> Result<(), Broken> {
    let Ok(value) = json::parse_with_max_scale(text, max_scale) else { return Ok(()) };
    let rendered = json::render_within(&value, usize::MAX).map_err(|_| Broken::JsonRoundTrip)?;
    let reparsed = json::parse_with_max_scale(&rendered, max_scale).map_err(|_| Broken::JsonRoundTrip)?;
    holds(cbor::encode(&reparsed) == cbor::encode(&value), Broken::JsonRoundTrip)?;
    holds(weighs_at_most(&value, rendered.len()), Broken::JsonWithin)?;
    // every rendering is at least a byte long
    let short = rendered.len().checked_sub(1).ok_or(Broken::JsonWithin)?;
    holds(
        json::render_within(&value, short) == Err(RenderError::TooLarge(TooLarge { limit: short })),
        Broken::JsonWithin,
    )
}

/// `json::render_within(value, max)` of the value `bytes` decode to (any canonical value, bytes and rationals with no
/// terminating decimal included). The limit only ever refuses: a rendering within it is the unlimited rendering; a
/// `TooLarge` carries `max`, and is given only when the unlimited rendering is longer or itself refused (the limit can
/// be met before the part JSON cannot represent); any other refusal is the unlimited one's. And an unlimited rendering
/// parses back to the value (the one `bytes` encode).
pub fn json_render(bytes: &[u8], max: usize) -> Result<(), Broken> {
    let Ok(value) = cbor::decode(bytes) else { return Ok(()) };
    let full = json::render_within(&value, usize::MAX);
    let within = json::render_within(&value, max);
    let agrees = match (&within, &full) {
        (Ok(w), Ok(f)) => w == f && w.len() <= max,
        (Err(RenderError::TooLarge(t)), Ok(f)) => t.limit == max && f.len() > max,
        (Err(RenderError::TooLarge(t)), Err(RenderError::Canonical(_))) => t.limit == max,
        (Err(RenderError::Canonical(w)), Err(RenderError::Canonical(f))) => w == f,
        _ => false,
    };
    holds(agrees, Broken::JsonBudget)?;
    match full {
        Ok(f) => holds(json::parse(&f).is_ok_and(|v| cbor::encode(&v) == bytes), Broken::JsonRoundTrip),
        Err(_) => Ok(()),
    }
}
