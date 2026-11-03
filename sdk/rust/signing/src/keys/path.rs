//! Child indices and derivation paths (`signing.md` section 3).

use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;

use crate::error::{KeyError, PathError, PathRefusal};

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

/// The most digits a segment may have: `2^31 - 1` has ten.
pub const MAX_SEGMENT_DIGITS: usize = dilf4s_core::generated::limits::as_usize::PATH_MAX_SEGMENT_DIGITS;

/// The most segments a path may have after `m`: BIP32 serialises the depth in one byte.
pub const MAX_DEPTH: usize = dilf4s_core::generated::limits::as_usize::PATH_MAX_DEPTH;

/// Parses `m/44'/0'/0'/0/7` (or `h` in place of `'`), relative to the master key. `m` alone is the empty path.
/// A segment is `0` or a nonzero ASCII digit and more digits, then an optional hardening mark, so each index has one
/// spelling; anything else, a leading zero, an empty segment (`m/1/`, `m//1`) or surrounding whitespace included, is
/// a [`PathError`], typed by the first rule broken (a [`PathRefusal`]). A segment of more than [`MAX_SEGMENT_DIGITS`]
/// digits is refused before any conversion, and the scan stops at segment [`MAX_DEPTH`], so parsing is linear in the
/// path and never holds more than [`MAX_DEPTH`] indices. The message names a segment by its position (0 is the first
/// after `m`) and, past the digit bound, its length; never by its text.
pub fn parse(path: &str) -> Result<Vec<ChildIndex>, PathError> {
    let mut segments = path.split('/');
    if segments.next() != Some("m") {
        return Err(refuse(PathRefusal::MissingMaster, "path must start with m".into()));
    }
    // collecting into a Result stops at the first error, so a long path is never split past segment MAX_DEPTH
    segments
        .enumerate()
        .map(|(at, segment)| {
            if at < MAX_DEPTH {
                parse_segment(segment, at)
            } else {
                Err(refuse(PathRefusal::TooDeep, format!("path has more than {MAX_DEPTH} segments")))
            }
        })
        .collect()
}

fn parse_segment(segment: &str, at: usize) -> Result<ChildIndex, PathError> {
    let (digits, hardened) = match segment.strip_suffix('\'').or_else(|| segment.strip_suffix('h')) {
        Some(d) => (d, true),
        None => (segment, false),
    };
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        return Err(refuse(PathRefusal::Malformed, format!("path segment {at} is malformed")));
    }
    if digits != "0" && digits.starts_with('0') {
        return Err(refuse(PathRefusal::LeadingZero, format!("path segment {at} has a leading zero")));
    }
    if digits.len() > MAX_SEGMENT_DIGITS {
        return Err(refuse(
            PathRefusal::TooManyDigits,
            format!("path segment {at} has {} digits, more than {MAX_SEGMENT_DIGITS}", digits.len()),
        ));
    }
    // at most ten ASCII digits: the fold cannot overflow a u64, and checked arithmetic says so without a panic
    let out_of_range = || refuse(PathRefusal::OutOfRange, format!("path segment {at} is out of range"));
    let index = digits
        .bytes()
        .try_fold(0u64, |acc, b| acc.checked_mul(10)?.checked_add(u64::from(b.wrapping_sub(b'0'))))
        .and_then(|i| u32::try_from(i).ok())
        .filter(|&i| i < HARDENED_OFFSET)
        .ok_or_else(out_of_range)?;
    Ok(ChildIndex { hardened, index })
}

const fn refuse(refusal: PathRefusal, reason: String) -> PathError {
    PathError { refusal, reason }
}
