//! State keys, namespaces and version keys (docs/specs/versioned-trie.md §2).

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

use dilf4s_core::Preview;

use super::VersionedTrieError;

pub use dilf4s_core::generated::codec_errors::{StateKeyError, StateKeyErrorKind};

/// A refusal's input, previewed; `None` for one refused before it was read (empty, too long, or not UTF-8).
pub fn input(e: &StateKeyError) -> Option<&Preview> {
    match e {
        StateKeyError::Empty | StateKeyError::TooLong { .. } | StateKeyError::NotUtf8 => None,
        StateKeyError::TooManySegments { key, .. }
        | StateKeyError::InvalidSegment { key, .. }
        | StateKeyError::SegmentTooLong { key, .. } => Some(key),
        StateKeyError::NamespaceFull { namespace } => Some(namespace),
    }
}

/// A size or position as a refusal holds it.
fn size(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

/// A key of a versioned trie's state: `segment *("/" segment)`, each segment `[a-z0-9][a-z0-9._-]*` of at most 64
/// characters, at most 16 segments and 256 characters. Its trie key is its UTF-8 bytes; the alphabet is ASCII, so byte
/// order is string order.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StateKey(String);

impl StateKey {
    /// The longest segment.
    pub const MAX_SEGMENT_LENGTH: usize = dilf4s_core::generated::limits::as_usize::STATE_KEY_MAX_SEGMENT_LENGTH;
    /// The most segments.
    pub const MAX_SEGMENTS: usize = dilf4s_core::generated::limits::as_usize::STATE_KEY_MAX_SEGMENTS;
    /// The longest key.
    pub const MAX_LENGTH: usize = dilf4s_core::generated::limits::as_usize::STATE_KEY_MAX_LENGTH;

    /// Parses a key, refusing in the order of versioned-trie.md section 2: empty; over [`Self::MAX_LENGTH`] UTF-8
    /// bytes; over [`Self::MAX_SEGMENTS`] segments; then each segment in order, its grammar before its length. The
    /// length is checked before the text is split, so a refusal costs O([`Self::MAX_LENGTH`]) whatever the input; it
    /// names a segment by its position, never its text.
    pub fn parse(s: &str) -> Result<Self, StateKeyError> {
        if s.is_empty() {
            return Err(StateKeyError::Empty);
        }
        // refused by its length alone, as Scala and TypeScript, whose previews would measure the whole text
        if s.len() > Self::MAX_LENGTH {
            return Err(StateKeyError::TooLong { limit: size(Self::MAX_LENGTH) });
        }
        if s.split('/').count() > Self::MAX_SEGMENTS {
            return Err(StateKeyError::TooManySegments { key: Preview::of(s), limit: size(Self::MAX_SEGMENTS) });
        }
        let lower_digit = |b: u8| b.is_ascii_lowercase() || b.is_ascii_digit();
        let grammar = |segment: &str| {
            segment.as_bytes().split_first().is_some_and(|(&h, t)| {
                lower_digit(h) && t.iter().all(|&b| lower_digit(b) || matches!(b, b'.' | b'_' | b'-'))
            })
        };
        // the grammar is ASCII, so a segment that passes it has as many characters as bytes
        let refusal = s.split('/').enumerate().find_map(|(segment, text)| {
            if !grammar(text) {
                Some(StateKeyError::InvalidSegment { key: Preview::of(s), segment: size(segment) })
            } else if text.len() > Self::MAX_SEGMENT_LENGTH {
                Some(StateKeyError::SegmentTooLong {
                    key: Preview::of(s),
                    segment: size(segment),
                    limit: size(Self::MAX_SEGMENT_LENGTH),
                })
            } else {
                None
            }
        });
        refusal.map_or_else(|| Ok(Self(String::from(s))), Err)
    }

    /// A key from its trie key (UTF-8 bytes). Bytes longer than [`Self::MAX_LENGTH`] are refused by their length,
    /// before they are read as text; then bytes that are not UTF-8; then [`Self::parse`].
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, StateKeyError> {
        if bytes.len() > Self::MAX_LENGTH {
            return Err(StateKeyError::TooLong { limit: size(Self::MAX_LENGTH) });
        }
        core::str::from_utf8(bytes).map_err(|_| StateKeyError::NotUtf8).and_then(Self::parse)
    }

    /// The key text.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// The key as a diagnostic names it (docs/specs/conformance.md section 8).
    pub fn preview(&self) -> Preview {
        Preview::of(&self.0)
    }

    /// The trie key: the UTF-8 bytes.
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }
}

impl fmt::Display for StateKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

/// A namespace `ns`, naming the keys below `ns/`. Its prefix is `utf8(ns + "/")`, so `fiber` never matches `fiberx/…`;
/// it leaves room for one more segment (at most 254 characters and 15 segments).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StateNamespace(String);

impl StateNamespace {
    /// The longest namespace.
    pub const MAX_LENGTH: usize = dilf4s_core::generated::limits::as_usize::STATE_NAMESPACE_MAX_LENGTH;

    /// Parses a namespace.
    pub fn parse(s: &str) -> Result<Self, StateKeyError> {
        let k = StateKey::parse(s)?;
        let slashes = k.0.bytes().filter(|&b| b == b'/').count();
        if k.0.len() <= Self::MAX_LENGTH && slashes < StateKey::MAX_SEGMENTS - 1 {
            Ok(Self(k.0))
        } else {
            Err(StateKeyError::NamespaceFull { namespace: Preview::of(s) })
        }
    }

    /// The namespace text.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// The namespace as a diagnostic names it (docs/specs/conformance.md section 8).
    pub fn preview(&self) -> Preview {
        Preview::of(&self.0)
    }

    /// The trie prefix, `utf8(ns + "/")`.
    pub fn prefix(&self) -> Vec<u8> {
        let mut p = self.0.as_bytes().to_vec();
        p.push(b'/');
        p
    }

    /// Whether `key` is under the namespace.
    pub fn contains(&self, key: &StateKey) -> bool {
        key.as_bytes().starts_with(&self.prefix())
    }
}

/// How a version becomes a history-trie key: a fixed width and an unsigned big-endian encoding, so comparing versions
/// and comparing their keys as unsigned bytes always agree. The width is configuration a verifier trusts.
pub trait VersionKey {
    /// The version type.
    type Version: Clone + fmt::Debug + PartialEq;
    /// The width `w` in bytes, `1..=1024` (the history trie is `fixed(w)`).
    fn width(&self) -> u16;
    /// `vkey(v)`.
    fn encode(&self, version: &Self::Version) -> Result<Vec<u8>, VersionedTrieError>;
    /// The version a key encodes.
    fn decode(&self, bytes: &[u8]) -> Result<Self::Version, VersionedTrieError>;
}

fn invalid(reason: String) -> VersionedTrieError {
    VersionedTrieError::InvalidVersion { reason }
}

/// Non-negative integers as `w` unsigned big-endian bytes, `1 <= w <= 8`. The default is 8 bytes, the JLVM's
/// `int_to_bytes [v, 8]`. As Scala's `VersionKey.unsignedLong`, the largest version is `2^(8w) - 1`, capped at
/// `2^63 - 1` for `w = 8`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct U64Key {
    width: u8,
    max: u64,
}

impl Default for U64Key {
    fn default() -> Self {
        Self::DEFAULT
    }
}

impl U64Key {
    /// 8 bytes.
    pub const DEFAULT: Self = Self { width: 8, max: i64::MAX.unsigned_abs() };

    /// The key of `width` bytes.
    pub fn new(width: u8) -> Result<Self, VersionedTrieError> {
        if !(1..=8).contains(&width) {
            return Err(invalid(alloc::format!("an integer version key is 1..8 bytes, not {width}")));
        }
        let max = if width == 8 {
            Self::DEFAULT.max
        } else {
            1u64.checked_shl(u32::from(width).saturating_mul(8)).map_or(u64::MAX, |x| x.saturating_sub(1))
        };
        Ok(Self { width, max })
    }

    /// The largest version.
    pub const fn max(self) -> u64 {
        self.max
    }
}

impl VersionKey for U64Key {
    type Version = u64;

    fn width(&self) -> u16 {
        u16::from(self.width)
    }

    fn encode(&self, version: &u64) -> Result<Vec<u8>, VersionedTrieError> {
        if *version > self.max {
            return Err(invalid(alloc::format!("version {version} is outside 0..{}", self.max)));
        }
        let all = version.to_be_bytes();
        let skip = 8usize.saturating_sub(usize::from(self.width));
        Ok(all.iter().skip(skip).copied().collect())
    }

    fn decode(&self, bytes: &[u8]) -> Result<u64, VersionedTrieError> {
        if bytes.len() != usize::from(self.width) {
            return Err(invalid(alloc::format!("a version key is {} bytes, got {}", self.width, bytes.len())));
        }
        let v = bytes.iter().fold(0u64, |acc, &b| acc.checked_shl(8).unwrap_or(0) | u64::from(b));
        if v > self.max {
            return Err(invalid(alloc::format!("version {} exceeds {}", dilf4s_core::hex::encode(bytes), self.max)));
        }
        Ok(v)
    }
}

/// Versions that are already `w`-byte keys, for any width `1..=1024`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BytesKey {
    width: u16,
}

impl BytesKey {
    /// The key of `width` bytes.
    pub fn new(width: u16) -> Result<Self, VersionedTrieError> {
        if (1..=crate::trie::MAX_FIXED_BYTES).contains(&width) {
            Ok(Self { width })
        } else {
            Err(invalid(alloc::format!("a version key is 1..{} bytes, not {width}", crate::trie::MAX_FIXED_BYTES)))
        }
    }
}

impl VersionKey for BytesKey {
    type Version = Vec<u8>;

    fn width(&self) -> u16 {
        self.width
    }

    fn encode(&self, version: &Vec<u8>) -> Result<Vec<u8>, VersionedTrieError> {
        self.decode(version)
    }

    fn decode(&self, bytes: &[u8]) -> Result<Vec<u8>, VersionedTrieError> {
        if bytes.len() == usize::from(self.width) {
            Ok(bytes.to_vec())
        } else {
            Err(invalid(alloc::format!("a version key is {} bytes, got {}", self.width, bytes.len())))
        }
    }
}
