//! A replica (docs/specs/versioned-trie.md §7): kept in memory, started empty or from a snapshot checked against a
//! trusted head, and following deltas, recomputing both roots itself.
//!
//! The roots are rebuilt from the entries on every delta, which costs time linear in the state's size.

use alloc::collections::{BTreeMap, BTreeSet};
use alloc::vec::Vec;

use dilf4s_core::hash::{Digest, Hasher};
use dilf4s_core::value::Value;

use super::snapshot::{Snapshot, SnapshotChunk};
use super::wire::Delta;
use super::{
    Head, MAX_CHUNK_ENTRIES, StateKey, VersionKey, VersionedTrie, VersionedTrieError, check_value, malformed, mismatch,
};
use crate::trie::root_from_sorted;

/// A replica of a versioned trie.
pub struct Replica<K: VersionKey, H: Hasher> {
    vt: VersionedTrie<K, H>,
    head: Option<Head<K::Version, H>>,
    state: BTreeMap<Vec<u8>, Digest<H>>,
    history: BTreeMap<Vec<u8>, Digest<H>>,
    values: BTreeMap<StateKey, Value>,
}

fn none_or<H: Hasher>(d: Option<&Digest<H>>) -> alloc::string::String {
    d.map_or_else(|| alloc::string::String::from("none"), crate::tag::hex)
}

impl<K: VersionKey, H: Hasher> Replica<K, H> {
    /// An empty replica, before the first commit.
    pub const fn empty(vt: VersionedTrie<K, H>) -> Self {
        Self { vt, head: None, state: BTreeMap::new(), history: BTreeMap::new(), values: BTreeMap::new() }
    }

    /// A replica of `snapshot`, which must pass every check of the snapshot reader against `trusted` and rebuild
    /// its state root and history root. The snapshot's own head is only a claim.
    pub fn from_snapshot(
        vt: VersionedTrie<K, H>,
        trusted: Head<K::Version, H>,
        snapshot: &Snapshot<K::Version, H>,
    ) -> Result<Self, VersionedTrieError> {
        let mut chunks = alloc::vec![SnapshotChunk::Head(trusted.clone())];
        chunks.extend(snapshot.entries.chunks(MAX_CHUNK_ENTRIES).map(|c| SnapshotChunk::State(c.to_vec())));
        chunks.extend(snapshot.history.chunks(MAX_CHUNK_ENTRIES).map(|c| SnapshotChunk::History(c.to_vec())));
        let checked = vt.read_snapshot(trusted.clone(), chunks)?;
        let state: BTreeMap<Vec<u8>, Digest<H>> =
            checked.entries.iter().map(|(k, v)| (k.as_bytes().to_vec(), vt.value_hash(v))).collect();
        let mut history = BTreeMap::new();
        for (u, s) in &checked.history {
            history.insert(vt.vkey(u)?, vt.bytes_hash(s.as_bytes()));
        }
        let state_root = root_from_sorted(vt.hashing(), vt.state_mode(), &state)?;
        if state_root != trusted.state_root {
            return Err(mismatch("state", &trusted.state_root, &state_root));
        }
        let history_root = root_from_sorted(vt.hashing(), vt.history_mode(), &history)?;
        if history_root != trusted.history_root {
            return Err(mismatch("history", &trusted.history_root, &history_root));
        }
        let values = checked.entries.into_iter().collect();
        Ok(Self { vt, head: Some(trusted), state, history, values })
    }

    /// The head, if anything was committed.
    pub const fn head(&self) -> Option<&Head<K::Version, H>> {
        self.head.as_ref()
    }

    /// The commitment root of the head.
    pub fn root(&self) -> Result<Option<Digest<H>>, VersionedTrieError> {
        self.head.as_ref().map(|h| self.vt.root(h)).transpose()
    }

    /// The current values.
    pub const fn values(&self) -> &BTreeMap<StateKey, Value> {
        &self.values
    }

    /// The configuration.
    pub const fn versioned_trie(&self) -> &VersionedTrie<K, H> {
        &self.vt
    }

    /// Applies `delta` if it chains from the head, its version is greater, no key is both written and removed (or
    /// written twice), every value is within the limits, and the head it recomputes is the delta's head. A refused
    /// delta leaves the replica unchanged.
    pub fn apply_delta(&mut self, delta: &Delta<K::Version, H>) -> Result<(), VersionedTrieError> {
        let parent = self.root()?;
        let claimed = delta.parent.as_ref().map(|h| self.vt.root(h)).transpose()?;
        if parent != claimed {
            return Err(VersionedTrieError::RootMismatch {
                component: "parent",
                expected: none_or(parent.as_ref()),
                computed: none_or(claimed.as_ref()),
            });
        }
        let nk = self.vt.vkey(&delta.head.version)?;
        if let Some(h) = &self.head {
            let hk = self.vt.vkey(&h.version)?;
            if nk <= hk {
                return Err(VersionedTrieError::VersionNotIncreasing { head: hk, next: nk });
            }
        }
        let written: BTreeSet<&StateKey> = delta.upserts.iter().map(|(k, _)| k).collect();
        if delta.removes.iter().any(|k| written.contains(k)) {
            return Err(malformed("a key is both written and removed"));
        }
        if written.len() != delta.upserts.len() {
            return Err(malformed("a key is written twice"));
        }
        for (k, v) in &delta.upserts {
            if *v == Value::Null {
                return Err(malformed(alloc::format!("{k} is upserted as null")));
            }
            check_value(k, v)?;
        }
        let mut state = self.state.clone();
        for k in &delta.removes {
            state.remove(k.as_bytes());
        }
        for (k, v) in &delta.upserts {
            state.insert(k.as_bytes().to_vec(), self.vt.value_hash(v));
        }
        let state_root = root_from_sorted(self.vt.hashing(), self.vt.state_mode(), &state)?;
        if state_root != delta.head.state_root {
            return Err(mismatch("state", &delta.head.state_root, &state_root));
        }
        let mut history = self.history.clone();
        history.insert(nk, self.vt.bytes_hash(state_root.as_bytes()));
        let history_root = root_from_sorted(self.vt.hashing(), self.vt.history_mode(), &history)?;
        if history_root != delta.head.history_root {
            return Err(mismatch("history", &delta.head.history_root, &history_root));
        }
        for k in &delta.removes {
            self.values.remove(k);
        }
        for (k, v) in &delta.upserts {
            self.values.insert(k.clone(), v.clone());
        }
        self.state = state;
        self.history = history;
        self.head = Some(delta.head.clone());
        Ok(())
    }
}
