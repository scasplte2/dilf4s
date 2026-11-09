//! The versioned trie (docs/specs/versioned-trie.md): state keys and namespaces, version keys, the top trie that
//! binds a head to one root, proof decoding and every check of §6, the chunked snapshot reader, and a replica that
//! follows deltas (§7).

use alloc::vec::Vec;
use core::fmt;

use dilf4s_core::cbor;
use dilf4s_core::hash::{Digest, Hasher};
use dilf4s_core::value::Value;

use crate::tag::hex;
use crate::trie::{KeyMode, TrieHashing, root_from_entries};

mod keys;
mod replica;
mod snapshot;
mod trie_map;
mod verifier;
mod wire;

pub use keys::{BytesKey, StateKey, StateKeyError, StateKeyErrorKind, StateNamespace, U64Key, VersionKey, input};
pub use replica::Replica;
pub use snapshot::{Snapshot, SnapshotChunk, SnapshotReader};
pub use verifier::VerifiedState;
pub use wire::{At, Delta, VersionedTrieProof};

/// Typed failures of a versioned trie: generated from `shared/schemas/proofs-errors.cddl`, with the reference's and the
/// TypeScript SDK's kinds and fields. Versions appear as their version keys' full lowercase hex ([`version_hex`]); a
/// recomputed root's component is a [`VtRootComponent`]. `Empty`, `VersionConflict`, `CommitRewrite`, `NotRetained` and
/// `Store` are the reference store's: this SDK has no store, so they never arise here.
pub use dilf4s_core::generated::proofs_errors::{
    VersionKeyHex, VersionedTrieError, VersionedTrieErrorKind, VtRootComponent,
};

/// A version key as a refusal names it: its full lowercase hex, never truncated. Every key reaching a refusal has been
/// checked at its version key's width (at most [`crate::trie::MAX_FIXED_BYTES`]), so it is at most twice that long.
pub fn version_hex(key: &[u8]) -> VersionKeyHex {
    dilf4s_core::hex::encode(key)
}

pub(crate) fn malformed(reason: impl fmt::Display) -> VersionedTrieError {
    VersionedTrieError::Malformed { reason: alloc::format!("{reason}") }
}

pub(crate) fn mismatch<H: Hasher>(
    component: VtRootComponent,
    expected: &Digest<H>,
    computed: &Digest<H>,
) -> VersionedTrieError {
    VersionedTrieError::RootMismatch { component, expected: hex(expected), computed: hex(computed) }
}

/// The largest value a key may hold, as canonical CBOR (1 MiB).
pub const MAX_VALUE_BYTES: u64 = dilf4s_core::generated::limits::VERSIONED_MAX_VALUE_BYTES;
/// At most this many entries or versions in one snapshot chunk.
pub const MAX_CHUNK_ENTRIES: usize = dilf4s_core::generated::limits::as_usize::VERSIONED_MAX_CHUNK_ENTRIES;
/// The largest snapshot chunk, as canonical CBOR (4 MiB): refused before it is decoded.
pub const MAX_CHUNK_BYTES: u64 = dilf4s_core::generated::limits::VERSIONED_MAX_CHUNK_BYTES;
/// The deepest value a key may hold: a delta or snapshot chunk wraps it in a map, an array and a map.
pub const MAX_VALUE_DEPTH: usize = dilf4s_core::generated::limits::as_usize::VERSIONED_MAX_VALUE_DEPTH;
/// The state trie's key limit: a state key's 256 bytes.
pub const STATE_KEY_BYTES: u64 = dilf4s_core::generated::limits::STATE_KEY_MAX_LENGTH;

/// The top trie's slots.
pub const VERSION_SLOT: u8 = 0x00;
/// The state root's slot.
pub const STATE_SLOT: u8 = 0x01;
/// The history root's slot.
pub const HISTORY_SLOT: u8 = 0x02;

/// What one root commits to: the head version, its state root and the history root.
pub struct Head<V, H: Hasher> {
    /// The head version.
    pub version: V,
    /// The state trie's root at it.
    pub state_root: Digest<H>,
    /// The history trie's root, which maps every committed version up to and including it to its state root.
    pub history_root: Digest<H>,
}

/// A committed version and its state root.
pub type Version<V, H> = (V, Digest<H>);

impl<V: Clone, H: Hasher> Clone for Head<V, H> {
    fn clone(&self) -> Self {
        Self {
            version: self.version.clone(),
            state_root: self.state_root.clone(),
            history_root: self.history_root.clone(),
        }
    }
}

impl<V: PartialEq, H: Hasher> PartialEq for Head<V, H> {
    fn eq(&self, other: &Self) -> bool {
        self.version == other.version && self.state_root == other.state_root && self.history_root == other.history_root
    }
}

impl<V: Eq, H: Hasher> Eq for Head<V, H> {}

impl<V: fmt::Debug, H: Hasher> fmt::Debug for Head<V, H> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Head")
            .field("version", &self.version)
            .field("state_root", &self.state_root)
            .field("history_root", &self.history_root)
            .finish()
    }
}

/// A value every wire format can carry: at most [`MAX_VALUE_DEPTH`] deep and [`MAX_VALUE_BYTES`] long.
///
/// O(min(weight, [`MAX_VALUE_BYTES`])) and allocation-free: the encoding is measured, not built, and the measure stops
/// at the limit, so a compact value with a vast shared expansion is refused without expanding it.
pub fn check_value(key: &StateKey, value: &Value) -> Result<(), VersionedTrieError> {
    let what = || alloc::format!("the value of {}", key.preview().short());
    if value.depth() > MAX_VALUE_DEPTH {
        return Err(VersionedTrieError::TooDeep {
            what: what(),
            depth: u64::try_from(value.depth()).unwrap_or(u64::MAX),
            limit: dilf4s_core::generated::limits::VERSIONED_MAX_VALUE_DEPTH,
        });
    }
    let max = usize::try_from(MAX_VALUE_BYTES).unwrap_or(usize::MAX);
    cbor::encoded_len_within(value, max)
        .map(|_| ())
        .map_err(|_| VersionedTrieError::TooLarge { what: what(), limit: MAX_VALUE_BYTES })
}

/// A versioned trie's configuration: the version key (its width is part of what a verifier trusts) and the trie
/// hashing. Every check and decoder is a method.
pub struct VersionedTrie<K: VersionKey, H: Hasher> {
    key: K,
    hashing: TrieHashing<H>,
    top: KeyMode,
    state: KeyMode,
    history: KeyMode,
}

impl<K: VersionKey + Clone, H: Hasher> Clone for VersionedTrie<K, H> {
    fn clone(&self) -> Self {
        Self {
            key: self.key.clone(),
            hashing: self.hashing.clone(),
            top: self.top,
            state: self.state,
            history: self.history,
        }
    }
}

impl<K: VersionKey, H: Hasher> VersionedTrie<K, H> {
    /// The configuration for version key `key`.
    pub fn new(key: K) -> Result<Self, VersionedTrieError> {
        let history = KeyMode::fixed(u64::from(key.width()))?;
        Ok(Self {
            key,
            hashing: TrieHashing::new(),
            top: KeyMode::fixed(1)?,
            state: KeyMode::variable(STATE_KEY_BYTES)?,
            history,
        })
    }

    /// The version key.
    pub const fn version_key(&self) -> &K {
        &self.key
    }

    /// The trie hashing.
    pub const fn hashing(&self) -> &TrieHashing<H> {
        &self.hashing
    }

    /// The top trie's mode, `fixed(1)`.
    pub const fn top_mode(&self) -> KeyMode {
        self.top
    }

    /// The state trie's mode, `variable(256)`.
    pub const fn state_mode(&self) -> KeyMode {
        self.state
    }

    /// The history trie's mode, `fixed(w)`.
    pub const fn history_mode(&self) -> KeyMode {
        self.history
    }

    /// `vkey(v)`, checked: the version key's bytes, refused as `InvalidVersion` unless they are exactly its width (which
    /// [`VersionedTrie::new`] bounds by [`crate::trie::MAX_FIXED_BYTES`]). Every path of this module encodes through it, so
    /// a caller's own version key never lengthens the key a refusal names ([`version_hex`]).
    pub fn vkey(&self, version: &K::Version) -> Result<Vec<u8>, VersionedTrieError> {
        let k = self.key.encode(version)?;
        if k.len() == usize::from(self.key.width()) {
            Ok(k)
        } else {
            Err(VersionedTrieError::InvalidVersion {
                reason: alloc::format!("a version key is {} bytes, got {}", self.key.width(), k.len()),
            })
        }
    }

    /// The hash a trie commits for a value: `valueHash(commit(v))`.
    pub fn value_hash(&self, value: &Value) -> Digest<H> {
        self.hashing.value_of(value)
    }

    /// The hash a trie commits for a byte string (top slots and history entries): `valueHash(0x00 || b)`, which is
    /// `value_hash(&Value::Bytes(b))`.
    pub fn bytes_hash(&self, bytes: &[u8]) -> Digest<H> {
        let mut committed = Vec::with_capacity(bytes.len().saturating_add(1));
        committed.push(dilf4s_core::commitment::BYTES_TAG);
        committed.extend_from_slice(bytes);
        self.hashing.value(&committed)
    }

    /// The commitment root of `head`: the root of its top trie (§3).
    pub fn root(&self, head: &Head<K::Version, H>) -> Result<Digest<H>, VersionedTrieError> {
        let vkey = self.vkey(&head.version)?;
        let slots = [
            (VERSION_SLOT, self.bytes_hash(&vkey)),
            (STATE_SLOT, self.bytes_hash(head.state_root.as_bytes())),
            (HISTORY_SLOT, self.bytes_hash(head.history_root.as_bytes())),
        ];
        let keys = [[VERSION_SLOT], [STATE_SLOT], [HISTORY_SLOT]];
        Ok(root_from_entries(&self.hashing, self.top, keys.iter().zip(slots).map(|(k, (_, h))| (k.as_slice(), h)))?)
    }
}
