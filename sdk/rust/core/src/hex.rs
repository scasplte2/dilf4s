//! Hexadecimal text, as the conformance vectors write bytes.

use alloc::string::String;
use alloc::vec::Vec;

use crate::hash::HashError;

const DIGITS: &[u8; 16] = b"0123456789abcdef";

/// Lowercase hexadecimal of `bytes`.
pub fn encode(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len().saturating_mul(2));
    for &b in bytes {
        for nibble in [b >> 4, b & 0x0f] {
            out.push(char::from(DIGITS.get(usize::from(nibble)).copied().unwrap_or(b'0')));
        }
    }
    out
}

/// The bytes of hexadecimal text: ASCII digits and `a`-`f` in either case, two per byte. Refused as
/// [`HashError::InvalidHex`] at the first byte that is not a hex digit, or else, for an odd number of digits, at the
/// text's length: every byte before either is ASCII, so the offset is the same in UTF-8 bytes, UTF-16 units and code
/// points (docs/specs/canonical-encoding.md section 4).
pub fn decode(text: &str) -> Result<Vec<u8>, HashError> {
    let bytes = text.as_bytes();
    let invalid = |at: usize| HashError::InvalidHex { offset: u64::try_from(at).unwrap_or(u64::MAX) };
    if let Some(at) = bytes.iter().position(|b| !b.is_ascii_hexdigit()) {
        return Err(invalid(at));
    }
    let (pairs, rest) = bytes.as_chunks::<2>();
    if !rest.is_empty() {
        return Err(invalid(bytes.len()));
    }
    pairs
        .iter()
        .enumerate()
        .map(|(i, [h, l])| {
            nibble(*h).zip(nibble(*l)).map(|(h, l)| (h << 4) | l).ok_or_else(|| invalid(i.saturating_mul(2)))
        })
        .collect()
}

fn nibble(c: u8) -> Option<u8> {
    char::from(c).to_digit(16).and_then(|d| u8::try_from(d).ok())
}
