//! State keys, namespaces and version keys (docs/specs/versioned-trie.md §2).

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

use super::VersionedTrieError;

/// Why a state key or namespace was rejected.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StateKeyError {
    /// The input (hex for bytes that are not UTF-8).
    pub input: String,
    /// Why.
    pub reason: String,
}

impl fmt::Display for StateKeyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "invalid state key {:?}: {}", self.input, self.reason)
    }
}

impl core::error::Error for StateKeyError {}

/// A key of a versioned trie's state: `segment *("/" segment)`, each segment `[a-z0-9][a-z0-9._-]*` of at most 64
/// characters, at most 16 segments and 256 characters. Its trie key is its UTF-8 bytes; the alphabet is ASCII, so byte
/// order is string order.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StateKey(String);

impl StateKey {
    /// The longest segment.
    pub const MAX_SEGMENT_LENGTH: usize = 64;
    /// The most segments.
    pub const MAX_SEGMENTS: usize = 16;
    /// The longest key.
    pub const MAX_LENGTH: usize = 256;

    /// Parses a key.
    pub fn parse(s: &str) -> Result<Self, StateKeyError> {
        let bad = |reason: String| StateKeyError { input: String::from(s), reason };
        if s.is_empty() {
            return Err(bad(String::from("empty key")));
        }
        if s.len() > Self::MAX_LENGTH {
            return Err(bad(alloc::format!("longer than {} characters", Self::MAX_LENGTH)));
        }
        if s.split('/').count() > Self::MAX_SEGMENTS {
            return Err(bad(alloc::format!("more than {} segments", Self::MAX_SEGMENTS)));
        }
        for segment in s.split('/') {
            if segment.len() > Self::MAX_SEGMENT_LENGTH {
                return Err(bad(alloc::format!(
                    "segment '{segment}' is longer than {} characters",
                    Self::MAX_SEGMENT_LENGTH
                )));
            }
            let lower_digit = |b: u8| b.is_ascii_lowercase() || b.is_ascii_digit();
            let ok = segment.as_bytes().split_first().is_some_and(|(&h, t)| {
                lower_digit(h) && t.iter().all(|&b| lower_digit(b) || matches!(b, b'.' | b'_' | b'-'))
            });
            if !ok {
                return Err(bad(alloc::format!("segment '{segment}' does not match [a-z0-9][a-z0-9._-]*")));
            }
        }
        Ok(Self(String::from(s)))
    }

    /// A key from its trie key (UTF-8 bytes).
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, StateKeyError> {
        core::str::from_utf8(bytes)
            .map_err(|_| StateKeyError { input: dilf4s_core::hex::encode(bytes), reason: String::from("not UTF-8") })
            .and_then(Self::parse)
    }

    /// The key text.
    pub fn as_str(&self) -> &str {
        &self.0
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
    pub const MAX_LENGTH: usize = StateKey::MAX_LENGTH - 2;

    /// Parses a namespace.
    pub fn parse(s: &str) -> Result<Self, StateKeyError> {
        let k = StateKey::parse(s)?;
        let slashes = k.0.bytes().filter(|&b| b == b'/').count();
        if k.0.len() <= Self::MAX_LENGTH && slashes < StateKey::MAX_SEGMENTS - 1 {
            Ok(Self(k.0))
        } else {
            Err(StateKeyError {
                input: String::from(s),
                reason: String::from("a namespace must leave room for one more segment"),
            })
        }
    }

    /// The namespace text.
    pub fn as_str(&self) -> &str {
        &self.0
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
