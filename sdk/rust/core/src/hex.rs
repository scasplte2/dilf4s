//! Hexadecimal text, as the conformance vectors write bytes.

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

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

/// The bytes of hexadecimal text (either case), which must have an even number of digits.
pub fn decode(text: &str) -> Result<Vec<u8>, HexError> {
    let digits = text.as_bytes();
    if digits.len() % 2 != 0 {
        return Err(HexError::OddLength);
    }
    digits
        .chunks_exact(2)
        .enumerate()
        .map(|(i, pair)| match pair {
            [h, l] => nibble(*h)
                .zip(nibble(*l))
                .map(|(h, l)| (h << 4) | l)
                .ok_or(HexError::InvalidDigit { offset: i.saturating_mul(2) }),
            _ => Err(HexError::OddLength),
        })
        .collect()
}

fn nibble(c: u8) -> Option<u8> {
    char::from(c).to_digit(16).and_then(|d| u8::try_from(d).ok())
}

/// Text that is not hexadecimal.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HexError {
    /// An odd number of digits.
    OddLength,
    /// A character that is not a hex digit, in the pair at this offset.
    InvalidDigit {
        /// Offset of the pair.
        offset: usize,
    },
}

impl fmt::Display for HexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OddLength => f.write_str("odd number of hex digits"),
            Self::InvalidDigit { offset } => write!(f, "invalid hex digit near offset {offset}"),
        }
    }
}

impl core::error::Error for HexError {}
