//! Snapshots (docs/specs/versioned-trie.md §7): chunks decoded from canonical CBOR, and a reader that checks them one
//! by one against a head the caller already trusts.

use alloc::string::String;
use alloc::vec::Vec;

use dilf4s_core::Preview;
use dilf4s_core::cbor;
use dilf4s_core::codec::Model;
use dilf4s_core::generated::versioned_trie as w;
use dilf4s_core::hash::{Digest, Hasher};
use dilf4s_core::value::Value;

use super::{
    Head, MAX_CHUNK_BYTES, MAX_CHUNK_ENTRIES, StateKey, VersionKey, VersionedTrie, VersionedTrieError, check_value,
    malformed,
};

/// One chunk of a snapshot stream.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SnapshotChunk<V, H: Hasher> {
    /// The head, first.
    Head(Head<V, H>),
    /// Current entries, in strictly increasing key order across all state chunks.
    State(Vec<(StateKey, Value)>),
    /// Committed versions with their state roots, strictly increasing, ending with the head version.
    History(Vec<(V, Digest<H>)>),
}

/// A whole versioned trie as a snapshot carries it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Snapshot<V, H: Hasher> {
    /// The head.
    pub head: Head<V, H>,
    /// Every current entry, in key order.
    pub entries: Vec<(StateKey, Value)>,
    /// Every committed version with its state root, in order.
    pub history: Vec<(V, Digest<H>)>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Stage {
    Head,
    State,
    History,
    Done,
}

/// Checks a snapshot chunk by chunk against a trusted head, refusing it at the first chunk that breaks a rule. A
/// refused chunk leaves the reader unchanged. The roots, which need every entry, are checked by
/// [`Replica::from_snapshot`](super::Replica::from_snapshot).
pub struct SnapshotReader<'a, K: VersionKey, H: Hasher> {
    vt: &'a VersionedTrie<K, H>,
    trusted: Head<K::Version, H>,
    stage: Stage,
    last_key: Option<Vec<u8>>,
    last_version: Option<Vec<u8>>,
    entries: Vec<(StateKey, Value)>,
    history: Vec<(K::Version, Digest<H>)>,
}

fn refuse<T>(reason: &str) -> Result<T, VersionedTrieError> {
    Err(malformed(alloc::format!("snapshot: {reason}")))
}

fn sized(n: usize, what: &str) -> Result<(), VersionedTrieError> {
    if (1..=MAX_CHUNK_ENTRIES).contains(&n) {
        Ok(())
    } else {
        refuse(&alloc::format!("a {what} chunk holds 1..{MAX_CHUNK_ENTRIES} items, not {n}"))
    }
}

/// Keys in strictly increasing order, each named by `show` when one does not follow: a state key by a preview of its
/// hex, a version by its key's full hex ([`super::version_hex`], bounded by the width).
fn increasing(
    previous: Option<&Vec<u8>>,
    keys: &[Vec<u8>],
    what: &str,
    show: fn(&[u8]) -> String,
) -> Result<(), VersionedTrieError> {
    let mut last = previous;
    for k in keys {
        if let Some(a) = last
            && a >= k
        {
            return refuse(&alloc::format!("{what} {} does not follow {}", show(k), show(a)));
        }
        last = Some(k);
    }
    Ok(())
}

impl<K: VersionKey, H: Hasher> VersionedTrie<K, H> {
    /// A snapshot chunk from canonical CBOR, refused by size before it is decoded. Order is the reader's to check.
    pub fn decode_chunk(&self, bytes: &[u8]) -> Result<SnapshotChunk<K::Version, H>, VersionedTrieError> {
        if !u64::try_from(bytes.len()).is_ok_and(|n| n <= MAX_CHUNK_BYTES) {
            return Err(VersionedTrieError::TooLarge {
                what: alloc::string::String::from("a snapshot chunk"),
                limit: MAX_CHUNK_BYTES,
            });
        }
        let value = cbor::decode(bytes).map_err(malformed)?;
        Ok(match w::VersionedTrieSnapshotChunk::from_value(&value).map_err(malformed)? {
            w::VersionedTrieSnapshotChunk::VersionedTrieSnapshotHead(h) => {
                SnapshotChunk::Head(self.head_from_wire(&h.head)?)
            }
            w::VersionedTrieSnapshotChunk::VersionedTrieSnapshotState(s) => {
                SnapshotChunk::State(s.state.into_iter().map(|u| Ok((StateKey::parse(&u.k)?, u.v))).collect::<Result<
                    _,
                    VersionedTrieError,
                >>(
                )?)
            }
            w::VersionedTrieSnapshotChunk::VersionedTrieSnapshotHistory(h) => SnapshotChunk::History(
                h.history
                    .iter()
                    .map(|x| Ok((self.key.decode(&x.v)?, Digest::<H>::from_slice(&x.s).map_err(malformed)?)))
                    .collect::<Result<_, VersionedTrieError>>()?,
            ),
        })
    }

    /// A reader of a snapshot of `trusted`.
    pub fn snapshot_reader(&self, trusted: Head<K::Version, H>) -> SnapshotReader<'_, K, H> {
        SnapshotReader {
            vt: self,
            trusted,
            stage: Stage::Head,
            last_key: None,
            last_version: None,
            entries: Vec::new(),
            history: Vec::new(),
        }
    }

    /// Reads a whole chunk sequence against a trusted head: every check of [`SnapshotReader`], then completeness.
    pub fn read_snapshot(
        &self,
        trusted: Head<K::Version, H>,
        chunks: impl IntoIterator<Item = SnapshotChunk<K::Version, H>>,
    ) -> Result<Snapshot<K::Version, H>, VersionedTrieError> {
        let mut reader = self.snapshot_reader(trusted);
        for c in chunks {
            reader.feed(c)?;
        }
        reader.finish()
    }
}

impl<K: VersionKey, H: Hasher> SnapshotReader<'_, K, H> {
    /// Checks and takes the next chunk.
    pub fn feed(&mut self, chunk: SnapshotChunk<K::Version, H>) -> Result<(), VersionedTrieError> {
        match (self.stage, chunk) {
            (Stage::Head, SnapshotChunk::Head(head)) => {
                if head == self.trusted {
                    self.stage = Stage::State;
                    Ok(())
                } else {
                    refuse("its head is not the trusted head")
                }
            }
            (Stage::Head, _) => refuse("it starts with its head"),
            (_, SnapshotChunk::Head(_)) => refuse("it has one head"),
            (Stage::State, SnapshotChunk::State(es)) => {
                sized(es.len(), "state")?;
                for (k, v) in &es {
                    if *v == Value::Null {
                        return refuse(&alloc::format!("{} is null", k.preview().short()));
                    }
                    check_value(k, v)?;
                }
                let keys: Vec<Vec<u8>> = es.iter().map(|(k, _)| k.as_bytes().to_vec()).collect();
                increasing(self.last_key.as_ref(), &keys, "key", |k| {
                    alloc::string::ToString::to_string(&Preview::of(&dilf4s_core::hex::encode(k)).short())
                })?;
                if let Some(k) = keys.last() {
                    self.last_key = Some(k.clone());
                }
                self.entries.extend(es);
                Ok(())
            }
            (Stage::Done, _) => refuse("nothing follows the head version"),
            (Stage::History, SnapshotChunk::State(_)) => refuse("state entries precede the history"),
            (_, SnapshotChunk::History(vs)) => {
                sized(vs.len(), "history")?;
                let keys = vs.iter().map(|(u, _)| self.vt.vkey(u)).collect::<Result<Vec<_>, _>>()?;
                let hk = self.vt.vkey(&self.trusted.version)?;
                increasing(self.last_version.as_ref(), &keys, "version", super::version_hex)?;
                for (k, (_, s)) in keys.iter().zip(&vs) {
                    if *k > hk {
                        return refuse(&alloc::format!("version {} is after the head", super::version_hex(k)));
                    }
                    if *k == hk && *s != self.trusted.state_root {
                        return refuse("the head version has another state root");
                    }
                }
                let last = keys.last().cloned();
                self.stage = if last.as_ref() == Some(&hk) { Stage::Done } else { Stage::History };
                if last.is_some() {
                    self.last_version = last;
                }
                self.history.extend(vs);
                Ok(())
            }
        }
    }

    /// The snapshot, once its history has reached the head version.
    pub fn finish(self) -> Result<Snapshot<K::Version, H>, VersionedTrieError> {
        if self.stage == Stage::Done {
            Ok(Snapshot { head: self.trusted, entries: self.entries, history: self.history })
        } else {
            refuse("it ends before the head version")
        }
    }
}
