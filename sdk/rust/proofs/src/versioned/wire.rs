//! Proofs and deltas, decoded from their canonical values (`shared/schemas/versioned-trie.cddl`).

use alloc::vec::Vec;

use dilf4s_core::codec::Model;
use dilf4s_core::generated::versioned_trie as w;
use dilf4s_core::hash::{Digest, Hasher};
use dilf4s_core::value::Value;

use super::{Head, StateKey, VersionKey, VersionedTrie, VersionedTrieError, malformed};
use crate::trie::TrieProof;

/// A version `u` asked about, and its state root if it was committed (`None` claims it was not).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct At<V, H: Hasher> {
    /// The version.
    pub version: V,
    /// Its state root, if committed.
    pub state_root: Option<Digest<H>>,
}

/// A proof against one commitment root (docs/specs/versioned-trie.md §5).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VersionedTrieProof<V, H: Hasher> {
    /// The head the top trie binds to the root.
    pub head: Head<V, H>,
    /// The whole top trie.
    pub top: TrieProof<H>,
    /// The version asked about, if any.
    pub at: Option<At<V, H>>,
    /// A proof over the head's history root.
    pub history: Option<TrieProof<H>>,
    /// A proof over the state root of `at`, or of the head.
    pub state: Option<TrieProof<H>>,
}

/// One version's change: the head before it (none for the first commit), the head after it, and the writes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Delta<V, H: Hasher> {
    /// The parent head.
    pub parent: Option<Head<V, H>>,
    /// The new head.
    pub head: Head<V, H>,
    /// Keys written, with their values.
    pub upserts: Vec<(StateKey, Value)>,
    /// Keys removed.
    pub removes: Vec<StateKey>,
}

fn digest<H: Hasher>(bytes: &[u8]) -> Result<Digest<H>, VersionedTrieError> {
    Digest::<H>::from_slice(bytes).map_err(malformed)
}

impl<K: VersionKey, H: Hasher> VersionedTrie<K, H> {
    /// A head from its wire record.
    pub fn head_from_wire(&self, wire: &w::VersionedTrieHead) -> Result<Head<K::Version, H>, VersionedTrieError> {
        Ok(Head { version: self.key.decode(&wire.v)?, state_root: digest(&wire.s)?, history_root: digest(&wire.h)? })
    }

    /// A head's wire record.
    pub fn head_to_wire(&self, head: &Head<K::Version, H>) -> Result<w::VersionedTrieHead, VersionedTrieError> {
        Ok(w::VersionedTrieHead {
            v: self.vkey(&head.version)?,
            s: head.state_root.as_bytes().to_vec(),
            h: head.history_root.as_bytes().to_vec(),
        })
    }

    /// Decodes a `versioned-trie-proof` value.
    pub fn decode_proof(&self, value: &Value) -> Result<VersionedTrieProof<K::Version, H>, VersionedTrieError> {
        let p = w::VersionedTrieProof::from_value(value).map_err(malformed)?;
        let at = match &p.at {
            None => None,
            Some(a) => {
                Some(At { version: self.key.decode(&a.v)?, state_root: a.s.as_deref().map(digest).transpose()? })
            }
        };
        Ok(VersionedTrieProof {
            head: self.head_from_wire(&p.head)?,
            top: TrieProof::from_records(&p.top)?,
            at,
            history: p.history.as_deref().map(TrieProof::from_records).transpose()?,
            state: p.state.as_deref().map(TrieProof::from_records).transpose()?,
        })
    }

    /// Decodes a `versioned-trie-delta` value.
    pub fn decode_delta(&self, value: &Value) -> Result<Delta<K::Version, H>, VersionedTrieError> {
        let d = w::VersionedTrieDelta::from_value(value).map_err(malformed)?;
        Ok(Delta {
            parent: d.parent.as_ref().map(|h| self.head_from_wire(h)).transpose()?,
            head: self.head_from_wire(&d.head)?,
            upserts: d
                .upserts
                .into_iter()
                .map(|u| Ok((StateKey::parse(&u.k)?, u.v)))
                .collect::<Result<_, VersionedTrieError>>()?,
            removes: d
                .removes
                .iter()
                .map(|k| StateKey::parse(k).map_err(VersionedTrieError::from))
                .collect::<Result<_, _>>()?,
        })
    }
}
