//! A replica (docs/specs/versioned-trie.md §7): kept in memory, started empty or from a snapshot checked against a
//! trusted head, and following deltas, recomputing both roots itself.
//!
//! The state and the history are each one persistent trie that holds the entries and caches every node's hash, so a
//! delta re-hashes only the paths of the keys it changes: O(changed keys × key length), whatever the state's size. No
//! key a peer sends is ever hashed into a table: keys are walked bit by bit, or compared.

use alloc::collections::BTreeSet;
use alloc::vec::Vec;

use dilf4s_core::hash::{Digest, Hasher};
use dilf4s_core::value::Value;

use super::snapshot::{Snapshot, SnapshotChunk};
use super::trie_map::TrieMap;
use super::wire::Delta;
use super::{
    Head, MAX_CHUNK_ENTRIES, StateKey, VersionKey, VersionedTrie, VersionedTrieError, check_value, malformed, mismatch,
};

/// A replica of a versioned trie.
pub struct Replica<K: VersionKey, H: Hasher> {
    vt: VersionedTrie<K, H>,
    head: Option<Head<K::Version, H>>,
    state: TrieMap<(StateKey, Value), H>,
    history: TrieMap<(), H>,
}

fn none_or<H: Hasher>(d: Option<&Digest<H>>) -> alloc::string::String {
    d.map_or_else(|| alloc::string::String::from("none"), crate::tag::hex)
}

impl<K: VersionKey, H: Hasher> Replica<K, H> {
    /// An empty replica, before the first commit.
    pub const fn empty(vt: VersionedTrie<K, H>) -> Self {
        let state = TrieMap::empty(vt.state_mode());
        let history = TrieMap::empty(vt.history_mode());
        Self { vt, head: None, state, history }
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
        // the reader has checked that keys and versions strictly increase, which the builder requires
        let entries = checked.entries.into_iter().map(|(k, v)| {
            let hash = vt.value_hash(&v);
            (k.as_bytes().to_vec(), (k, v), hash)
        });
        let state = TrieMap::from_sorted(vt.hashing(), vt.state_mode(), entries)?;
        let state_root = state.root(vt.hashing());
        if state_root != trusted.state_root {
            return Err(mismatch("state", &trusted.state_root, &state_root));
        }
        let mut versions = Vec::with_capacity(checked.history.len());
        for (u, s) in &checked.history {
            versions.push((vt.vkey(u)?, (), vt.bytes_hash(s.as_bytes())));
        }
        let history = TrieMap::from_sorted(vt.hashing(), vt.history_mode(), versions)?;
        let history_root = history.root(vt.hashing());
        if history_root != trusted.history_root {
            return Err(mismatch("history", &trusted.history_root, &history_root));
        }
        Ok(Self { vt, head: Some(trusted), state, history })
    }

    /// The head, if anything was committed.
    pub const fn head(&self) -> Option<&Head<K::Version, H>> {
        self.head.as_ref()
    }

    /// The commitment root of the head.
    pub fn root(&self) -> Result<Option<Digest<H>>, VersionedTrieError> {
        self.head.as_ref().map(|h| self.vt.root(h)).transpose()
    }

    /// The current value of `key`, if any: one walk down its path.
    pub fn get(&self, key: &StateKey) -> Option<&Value> {
        self.state.get(key.as_bytes()).map(|(_, v)| v)
    }

    /// Every current entry, in key order.
    pub fn entries(&self) -> impl Iterator<Item = (&StateKey, &Value)> {
        self.state.iter().map(|(k, v)| (k, v))
    }

    /// The number of current entries.
    pub const fn len(&self) -> usize {
        self.state.len()
    }

    /// Whether the state is empty.
    pub const fn is_empty(&self) -> bool {
        self.state.len() == 0
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
                return Err(malformed(alloc::format!("{} is upserted as null", k.preview().short())));
            }
            check_value(k, v)?;
        }
        // new tries sharing every untouched subtree with the current ones, which stay as they are until both roots match
        let hashing = self.vt.hashing();
        let mut state = self.state.clone();
        for k in &delta.removes {
            state = state.remove(hashing, k.as_bytes())?;
        }
        for (k, v) in &delta.upserts {
            state = state.insert(hashing, k.as_bytes(), (k.clone(), v.clone()), self.vt.value_hash(v))?;
        }
        let state_root = state.root(hashing);
        if state_root != delta.head.state_root {
            return Err(mismatch("state", &delta.head.state_root, &state_root));
        }
        let history = self.history.insert(hashing, &nk, (), self.vt.bytes_hash(state_root.as_bytes()))?;
        let history_root = history.root(hashing);
        if history_root != delta.head.history_root {
            return Err(mismatch("history", &delta.head.history_root, &history_root));
        }
        self.state = state;
        self.history = history;
        self.head = Some(delta.head.clone());
        Ok(())
    }
}
