//! Child indices and derivation paths (`signing.md` section 3).

use alloc::format;
use alloc::vec::Vec;

use crate::error::KeyError;

/// The first hardened index, `2^31`.
pub const HARDENED_OFFSET: u32 = 0x8000_0000;

/// A child index below `2^31`; hardened indices are serialised with bit 31 set.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ChildIndex {
    /// A soft (normal) index.
    Soft(u32),
    /// A hardened index.
    Hardened(u32),
}

impl ChildIndex {
    /// A soft index `i < 2^31`.
    pub fn soft(i: u32) -> Result<Self, KeyError> {
        if i < HARDENED_OFFSET {
            Ok(Self::Soft(i))
        } else {
            Err(KeyError::InvalidDerivation { reason: format!("index {i} out of range") })
        }
    }

    /// A hardened index `i < 2^31`.
    pub fn hardened(i: u32) -> Result<Self, KeyError> {
        if i < HARDENED_OFFSET {
            Ok(Self::Hardened(i))
        } else {
            Err(KeyError::InvalidDerivation { reason: format!("index {i} out of range") })
        }
    }

    /// The index without its hardening bit.
    pub const fn value(self) -> u32 {
        match self {
            Self::Soft(i) | Self::Hardened(i) => i,
        }
    }

    /// Whether the index is hardened.
    pub const fn is_hardened(self) -> bool {
        matches!(self, Self::Hardened(_))
    }

    /// The serialised index: bit 31 set when hardened.
    pub const fn serialized(self) -> u32 {
        match self {
            Self::Soft(i) => i,
            Self::Hardened(i) => i | HARDENED_OFFSET,
        }
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

/// Parses `m/44'/0'/0'/0/7` (or `h` in place of `'`), relative to the master key. `m` alone is the empty path.
/// Segments are ASCII decimal digits with an optional hardening mark; anything else, an empty segment (`m/1/`,
/// `m//1`) or surrounding whitespace included, is `InvalidDerivation`.
pub fn parse(path: &str) -> Result<Vec<ChildIndex>, KeyError> {
    let segments: Vec<&str> = path.split('/').collect();
    let Some((&"m", rest)) = segments.split_first() else {
        return Err(KeyError::InvalidDerivation { reason: "path must start with m".into() });
    };
    rest.iter()
        .map(|segment| {
            let (digits, hardened) = match segment.strip_suffix('\'').or_else(|| segment.strip_suffix('h')) {
                Some(d) => (d, true),
                None => (*segment, false),
            };
            if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
                return Err(KeyError::InvalidDerivation { reason: format!("bad path segment '{segment}'") });
            }
            let i: u32 = digits
                .parse()
                .map_err(|_| KeyError::InvalidDerivation { reason: format!("index {digits} out of range") })?;
            if hardened { ChildIndex::hardened(i) } else { ChildIndex::soft(i) }
        })
        .collect()
}
