//! A replica's trie: an in-memory binary Patricia trie (docs/specs/trie.md) that keeps a payload with every value
//! hash and caches every node's hash, so it is both the replica's ordered map and its root.
//!
//! It is persistent: an update copies the nodes on the changed key's path and shares every other subtree, so it
//! hashes at most `depth + 2` nodes (the path, a lowered or merged node, and a new leaf), however many entries the trie
//! holds, and the trie it was applied to is unchanged. Keys are only ever walked bit by bit, never hashed or compared
//! with other keys. Walks, builds, iteration and dropping use explicit stacks, so depth never consumes call stack.

use alloc::sync::Arc;
use alloc::vec::Vec;

use dilf4s_core::hash::{Digest, Hasher};

use crate::bits::Bits;
use crate::trie::{KeyMode, TrieError, TrieHashing};

/// A value: the caller's payload and the value hash the trie commits.
struct Slot<P, H: Hasher> {
    payload: P,
    hash: Digest<H>,
}

/// A node: a leaf has a value and no children; a branch keeps the branch invariant (two children, or one and a value).
struct Node<P, H: Hasher> {
    segment: Bits,
    value: Option<Arc<Slot<P, H>>>,
    left: Option<Arc<Node<P, H>>>,
    right: Option<Arc<Node<P, H>>>,
    hash: Digest<H>,
}

type Link<P, H> = Option<Arc<Node<P, H>>>;
type Value<P, H> = Option<Arc<Slot<P, H>>>;

impl<P, H: Hasher> Node<P, H> {
    const fn child(&self, bit: bool) -> Option<&Arc<Self>> {
        if bit { self.right.as_ref() } else { self.left.as_ref() }
    }
}

impl<P, H: Hasher> Drop for Node<P, H> {
    /// Releases the subtrees this node alone holds one at a time, rather than by nested drops as deep as the trie.
    fn drop(&mut self) {
        let mut todo: Vec<Arc<Self>> = self.left.take().into_iter().chain(self.right.take()).collect();
        while let Some(n) = todo.pop() {
            if let Some(mut owned) = Arc::into_inner(n) {
                todo.extend(owned.left.take());
                todo.extend(owned.right.take());
            }
        }
    }
}

/// A node with its hash; `value` must be present when there are no children.
fn node<P, H: Hasher>(
    hashing: &TrieHashing<H>,
    segment: Bits,
    value: Option<Arc<Slot<P, H>>>,
    left: Link<P, H>,
    right: Link<P, H>,
) -> Arc<Node<P, H>> {
    let hash = match (&value, &left, &right) {
        (Some(v), None, None) => hashing.leaf(&segment, &v.hash),
        _ => hashing.branch(
            &segment,
            value.as_ref().map(|v| &v.hash),
            left.as_ref().map(|n| &n.hash),
            right.as_ref().map(|n| &n.hash),
        ),
    };
    Arc::new(Node { segment, value, left, right, hash })
}

/// `n` with its `bit` child replaced.
fn with_child<P, H: Hasher>(hashing: &TrieHashing<H>, n: &Node<P, H>, bit: bool, child: Link<P, H>) -> Link<P, H> {
    let (left, right) = if bit { (n.left.clone(), child) } else { (child, n.right.clone()) };
    normalize(hashing, n.segment.clone(), n.value.clone(), left, right)
}

/// The node for these fields under the branch invariant: nothing for no value and no children, a leaf for a value
/// alone, and a lone child merged into its parent's segment (one more hash: the child's own children are kept).
fn normalize<P, H: Hasher>(
    hashing: &TrieHashing<H>,
    segment: Bits,
    value: Option<Arc<Slot<P, H>>>,
    left: Link<P, H>,
    right: Link<P, H>,
) -> Link<P, H> {
    match (value, left, right) {
        (None, None, None) => None,
        (None, Some(only), None) => Some(merge(hashing, &segment, false, &only)),
        (None, None, Some(only)) => Some(merge(hashing, &segment, true, &only)),
        (value, left, right) => Some(node(hashing, segment, value, left, right)),
    }
}

fn merge<P, H: Hasher>(hashing: &TrieHashing<H>, segment: &Bits, bit: bool, child: &Node<P, H>) -> Arc<Node<P, H>> {
    let joined = segment.with_bit(bit).concat(&child.segment);
    node(hashing, joined, child.value.clone(), child.left.clone(), child.right.clone())
}

/// The trie: its key mode, its root, and how many entries it holds.
pub(crate) struct TrieMap<P, H: Hasher> {
    mode: KeyMode,
    root: Link<P, H>,
    len: usize,
}

impl<P, H: Hasher> Clone for TrieMap<P, H> {
    /// Shares every node: O(1).
    fn clone(&self) -> Self {
        Self { mode: self.mode, root: self.root.clone(), len: self.len }
    }
}

enum Work<P, H: Hasher> {
    /// Build the subtree of `entries[lo..hi]`, whose keys share their first `offset` bits.
    Build { lo: usize, hi: usize, offset: u64 },
    /// Join the subtrees its children left on the stack (right on top).
    Combine { segment: Bits, value: Option<Arc<Slot<P, H>>>, left: bool, right: bool },
}

impl<P, H: Hasher> TrieMap<P, H> {
    /// The empty trie.
    pub(crate) const fn empty(mode: KeyMode) -> Self {
        Self { mode, root: None, len: 0 }
    }

    /// The number of entries.
    pub(crate) const fn len(&self) -> usize {
        self.len
    }

    /// The root.
    pub(crate) fn root(&self, hashing: &TrieHashing<H>) -> Digest<H> {
        self.root.as_ref().map_or_else(|| hashing.empty(), |n| n.hash.clone())
    }

    /// The payload at `key`, if any: one walk down its path.
    pub(crate) fn get(&self, key: &[u8]) -> Option<&P> {
        let path = self.mode.path(key).ok()?;
        let mut at = self.root.as_deref()?;
        let mut offset = 0u64;
        loop {
            let seg = at.segment.len();
            if !path.range_eq(offset, &at.segment, 0, seg) {
                return None;
            }
            let pos = offset.saturating_add(seg);
            if pos == path.len() {
                return at.value.as_deref().map(|v| &v.payload);
            }
            at = at.child(path.get(pos)?)?;
            offset = pos.saturating_add(1);
        }
    }

    /// The trie with `key` holding `payload` committed as `hash`; this trie is unchanged.
    pub(crate) fn insert(
        &self,
        hashing: &TrieHashing<H>,
        key: &[u8],
        payload: P,
        hash: Digest<H>,
    ) -> Result<Self, TrieError> {
        let path = self.mode.path(key)?;
        let end = path.len();
        let slot = Arc::new(Slot { payload, hash });
        let leaf = |from: u64, slot: Arc<Slot<P, H>>| node(hashing, path.slice(from, end), Some(slot), None, None);
        let mut frames: Vec<(&Node<P, H>, bool)> = Vec::new();
        let mut at = self.root.as_deref();
        let mut offset = 0u64;
        let (replacement, added) = loop {
            let Some(n) = at else { break (leaf(offset, slot), true) };
            let seg = n.segment.len();
            let common = path.common_prefix_from(offset, &n.segment, 0);
            let pos = offset.saturating_add(common);
            if common < seg {
                // the key leaves this node's segment: split it, the node keeping the rest below the split
                let old_bit = n.segment.get(common).ok_or_else(invariant)?;
                let rest = node(
                    hashing,
                    n.segment.slice(common.saturating_add(1), seg),
                    n.value.clone(),
                    n.left.clone(),
                    n.right.clone(),
                );
                let shared = n.segment.slice(0, common);
                let split = match path.get(pos) {
                    None => {
                        let (l, r) = if old_bit { (None, Some(rest)) } else { (Some(rest), None) };
                        node(hashing, shared, Some(slot), l, r)
                    }
                    Some(new_bit) => {
                        let fresh = leaf(pos.saturating_add(1), slot);
                        let (l, r) = if new_bit { (rest, fresh) } else { (fresh, rest) };
                        node(hashing, shared, None, Some(l), Some(r))
                    }
                };
                break (split, true);
            }
            match path.get(pos) {
                None => {
                    let replaced = node(hashing, n.segment.clone(), Some(slot), n.left.clone(), n.right.clone());
                    break (replaced, n.value.is_none());
                }
                Some(bit) => {
                    if let Some(c) = n.child(bit) {
                        frames.push((n, bit));
                        at = Some(c);
                        offset = pos.saturating_add(1);
                    } else {
                        let fresh = leaf(pos.saturating_add(1), slot);
                        break (with_child(hashing, n, bit, Some(fresh)).ok_or_else(invariant)?, true);
                    }
                }
            }
        };
        let root = rebuild(hashing, frames, Some(replacement));
        let len = if added { self.len.saturating_add(1) } else { self.len };
        Ok(Self { mode: self.mode, root, len })
    }

    /// The trie without `key` (itself, shared, if the key is absent); this trie is unchanged.
    pub(crate) fn remove(&self, hashing: &TrieHashing<H>, key: &[u8]) -> Result<Self, TrieError> {
        let path = self.mode.path(key)?;
        let mut frames: Vec<(&Node<P, H>, bool)> = Vec::new();
        let mut at = self.root.as_deref();
        let mut offset = 0u64;
        let replacement = loop {
            let Some(n) = at else { return Ok(self.clone()) };
            let seg = n.segment.len();
            if !path.range_eq(offset, &n.segment, 0, seg) {
                return Ok(self.clone());
            }
            let pos = offset.saturating_add(seg);
            match path.get(pos) {
                None if n.value.is_some() => {
                    break normalize(hashing, n.segment.clone(), None, n.left.clone(), n.right.clone());
                }
                None => return Ok(self.clone()),
                Some(bit) => match n.child(bit) {
                    Some(c) => {
                        frames.push((n, bit));
                        at = Some(c);
                        offset = pos.saturating_add(1);
                    }
                    None => return Ok(self.clone()),
                },
            }
        };
        let root = rebuild(hashing, frames, replacement);
        Ok(Self { mode: self.mode, root, len: self.len.saturating_sub(1) })
    }

    /// The trie of `entries`, whose keys must strictly increase: each node hashed once.
    pub(crate) fn from_sorted(
        hashing: &TrieHashing<H>,
        mode: KeyMode,
        entries: impl IntoIterator<Item = (Vec<u8>, P, Digest<H>)>,
    ) -> Result<Self, TrieError> {
        let mut es: Vec<(Bits, Value<P, H>)> = Vec::new();
        for (key, payload, hash) in entries {
            let path = mode.path(&key)?;
            if es.last().is_some_and(|(previous, _)| !previous.key_lt(&path)) {
                return Err(TrieError::InvalidKey {
                    reason: alloc::string::String::from("keys must strictly increase"),
                });
            }
            es.push((path, Some(Arc::new(Slot { payload, hash }))));
        }
        let len = es.len();
        if len == 0 {
            return Ok(Self::empty(mode));
        }
        let mut work = alloc::vec![Work::Build { lo: 0, hi: len, offset: 0 }];
        let mut done: Vec<Arc<Node<P, H>>> = Vec::new();
        while let Some(w) = work.pop() {
            match w {
                Work::Build { lo, hi, offset } => {
                    let first = &es.get(lo).ok_or_else(invariant)?.0;
                    if hi.saturating_sub(lo) == 1 {
                        let segment = first.slice(offset, first.len());
                        let slot = es.get_mut(lo).and_then(|e| e.1.take());
                        done.push(node(hashing, segment, slot, None, None));
                        continue;
                    }
                    let last = &es.get(hi.saturating_sub(1)).ok_or_else(invariant)?.0;
                    let pos = offset.saturating_add(first.common_prefix_from(offset, last, offset));
                    let segment = first.slice(offset, pos);
                    // the key ending here sorts first; the rest split by the next bit, zeros first
                    let here = first.len() == pos;
                    let start = if here { lo.saturating_add(1) } else { lo };
                    let value = if here { es.get_mut(lo).and_then(|e| e.1.take()) } else { None };
                    let rest = es.get(start..hi).ok_or_else(invariant)?;
                    let split = start.saturating_add(rest.partition_point(|(k, _)| k.get(pos) != Some(true)));
                    let (left, right) = (split > start, hi > split);
                    work.push(Work::Combine { segment, value, left, right });
                    let next = pos.saturating_add(1);
                    if right {
                        work.push(Work::Build { lo: split, hi, offset: next });
                    }
                    if left {
                        work.push(Work::Build { lo: start, hi: split, offset: next });
                    }
                }
                Work::Combine { segment, value, left, right } => {
                    let r = if right { Some(done.pop().ok_or_else(invariant)?) } else { None };
                    let l = if left { Some(done.pop().ok_or_else(invariant)?) } else { None };
                    done.push(node(hashing, segment, value, l, r));
                }
            }
        }
        Ok(Self { mode, root: Some(done.pop().ok_or_else(invariant)?), len })
    }

    /// Every payload, in key order (a key before its extensions, then bit 0 before bit 1).
    pub(crate) fn iter(&self) -> Iter<'_, P, H> {
        Iter { todo: self.root.as_deref().into_iter().collect() }
    }
}

/// The ancestors re-hashed bottom-up around a replaced subtree, each normalised.
fn rebuild<P, H: Hasher>(
    hashing: &TrieHashing<H>,
    frames: Vec<(&Node<P, H>, bool)>,
    replacement: Link<P, H>,
) -> Link<P, H> {
    frames.into_iter().rev().fold(replacement, |child, (parent, bit)| with_child(hashing, parent, bit, child))
}

fn invariant() -> TrieError {
    TrieError::InvalidNode { reason: alloc::string::String::from("replica trie state") }
}

/// [`TrieMap::iter`]: a depth-first walk with an explicit stack.
pub(crate) struct Iter<'a, P, H: Hasher> {
    todo: Vec<&'a Node<P, H>>,
}

impl<'a, P, H: Hasher> Iterator for Iter<'a, P, H> {
    type Item = &'a P;

    fn next(&mut self) -> Option<&'a P> {
        loop {
            let n = self.todo.pop()?;
            self.todo.extend(n.right.as_deref());
            self.todo.extend(n.left.as_deref());
            if let Some(v) = n.value.as_deref() {
                return Some(&v.payload);
            }
        }
    }
}
