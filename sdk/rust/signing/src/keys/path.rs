//! Child indices and derivation paths (`signing.md` section 3).

use alloc::format;
use alloc::vec::Vec;

use crate::error::KeyError;

/// The first hardened index, `2^31`.
pub const HARDENED_OFFSET: u32 = 0x8000_0000;

/// A child index below `2^31`; hardened indices are serialised with bit 31 set. The fields are private, so an index
/// is only built through [`ChildIndex::soft`] or [`ChildIndex::hardened`], which check the range: a soft index with
/// bit 31 set would serialise as a hardened one.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ChildIndex {
    hardened: bool,
    index: u32,
}

impl ChildIndex {
    /// A soft index `i < 2^31`.
    pub fn soft(i: u32) -> Result<Self, KeyError> {
        if i < HARDENED_OFFSET {
            Ok(Self { hardened: false, index: i })
        } else {
            Err(KeyError::InvalidDerivation { reason: format!("index {i} out of range") })
        }
    }

    /// A hardened index `i < 2^31`.
    pub fn hardened(i: u32) -> Result<Self, KeyError> {
        if i < HARDENED_OFFSET {
            Ok(Self { hardened: true, index: i })
        } else {
            Err(KeyError::InvalidDerivation { reason: format!("index {i} out of range") })
        }
    }

    /// The index without its hardening bit.
    pub const fn value(self) -> u32 {
        self.index
    }

    /// Whether the index is hardened.
    pub const fn is_hardened(self) -> bool {
        self.hardened
    }

    /// The serialised index: bit 31 set when hardened.
    pub const fn serialized(self) -> u32 {
        if self.hardened { self.index | HARDENED_OFFSET } else { self.index }
    }

    /// `BE32(serialized)`.
    pub const fn be32(self) -> [u8; 4] {
        self.serialized().to_be_bytes()
    }

    /// `LE32(serialized)`.
    pub const fn le32(self) -> [u8; 4] {
        self.serialized().to_le_bytes()
    }
}

/// The most significant digits a segment may have once its leading zeros are stripped: `2^31 - 1` has ten.
pub const MAX_SEGMENT_DIGITS: usize = 10;

/// Parses `m/44'/0'/0'/0/7` (or `h` in place of `'`), relative to the master key. `m` alone is the empty path.
/// Segments are ASCII decimal digits with an optional hardening mark; anything else, an empty segment (`m/1/`,
/// `m//1`) or surrounding whitespace included, is `InvalidDerivation`. Leading zeros are stripped and a segment with
/// more than [`MAX_SEGMENT_DIGITS`] digits left is refused before any conversion, so parsing is linear in the path. An
/// error names a segment by its position (0 is the first after `m`) and, past the bound, its significant digit
/// count; never by its text.
pub fn parse(path: &str) -> Result<Vec<ChildIndex>, KeyError> {
    let segments: Vec<&str> = path.split('/').collect();
    let Some((&"m", rest)) = segments.split_first() else {
        return Err(KeyError::InvalidDerivation { reason: "path must start with m".into() });
    };
    rest.iter().enumerate().map(|(at, segment)| parse_segment(segment, at)).collect()
}

fn parse_segment(segment: &str, at: usize) -> Result<ChildIndex, KeyError> {
    let (digits, hardened) = match segment.strip_suffix('\'').or_else(|| segment.strip_suffix('h')) {
        Some(d) => (d, true),
        None => (segment, false),
    };
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        return Err(KeyError::InvalidDerivation { reason: format!("path segment {at} is malformed") });
    }
    let significant = digits.trim_start_matches('0');
    if significant.len() > MAX_SEGMENT_DIGITS {
        return Err(KeyError::InvalidDerivation {
            reason: format!(
                "path segment {at} has {} significant digits, more than {MAX_SEGMENT_DIGITS}",
                significant.len()
            ),
        });
    }
    // at most ten ASCII digits: the fold cannot overflow a u64, and checked arithmetic says so without a panic
    let out_of_range = || KeyError::InvalidDerivation { reason: format!("path segment {at} is out of range") };
    let index = significant
        .bytes()
        .try_fold(0u64, |acc, b| acc.checked_mul(10)?.checked_add(u64::from(b.wrapping_sub(b'0'))))
        .and_then(|i| u32::try_from(i).ok())
        .filter(|&i| i < HARDENED_OFFSET)
        .ok_or_else(out_of_range)?;
    Ok(ChildIndex { hardened, index })
}
