//! Partial tries (docs/specs/trie.md §4), their flat pre-order encoding (§5), verification and queries.
//!
//! A proof is held as an arena of nodes in record (pre-order) order, so every child sits after its parent. Verification
//! is a forward pass for depths and structure, and a backward pass for hashes; queries walk with explicit stacks.
//! Nothing recurses on the proof's depth.

use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;

use dilf4s_core::codec::Codec;
use dilf4s_core::generated::trie::{TRIE_PROOF, TrieBranch, TrieChildren, TrieLeaf, TrieRecord, TrieSegment, TrieStub};
use dilf4s_core::hash::{Digest, Hasher};
use dilf4s_core::value::Value;

use super::{KeyMode, Lookup, TrieError, TrieHashing, encoding, valid_branch};
use crate::bits::{Bits, intersects, within};
use crate::tag::hex;

/// A complete listing: `(key, valueHash)` pairs in key order.
pub type Listing<H> = Vec<(Vec<u8>, Digest<H>)>;

#[derive(Clone, Debug, PartialEq, Eq)]
enum Node<H: Hasher> {
    Stub(Digest<H>),
    Leaf { segment: Bits, value: Digest<H> },
    Branch { segment: Bits, value: Option<Digest<H>>, left: Option<usize>, right: Option<usize> },
}

/// A partial trie: the proof of a trie's keys, prefixes and ranges. No nodes is the proof of the empty trie.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TrieProof<H: Hasher> {
    nodes: Vec<Node<H>>,
}

/// A branch waiting for its children while decoding.
struct Open {
    index: usize,
    want_left: bool,
    want_right: bool,
    left_done: bool,
}

fn digest<H: Hasher>(bytes: &[u8]) -> Result<Digest<H>, TrieError> {
    Digest::<H>::from_slice(bytes).map_err(encoding)
}

/// A wire segment as bits: `len(b) == ceil(n / 8)` and zero padding.
fn segment(w: &TrieSegment) -> Result<Bits, TrieError> {
    Bits::from_padded(w.n, w.b.clone()).ok_or_else(|| {
        encoding(alloc::format!("segment of {} bits in {} bytes, or with non-zero padding bits", w.n, w.b.len()))
    })
}

fn wire_segment(bits: &Bits) -> TrieSegment {
    TrieSegment { n: bits.len(), b: bits.padded_bytes().to_vec() }
}

fn set_child<H: Hasher>(nodes: &mut [Node<H>], parent: usize, child: usize, left: bool) -> Result<(), TrieError> {
    match nodes.get_mut(parent) {
        Some(Node::Branch { left: l, right: r, .. }) => {
            *(if left { l } else { r }) = Some(child);
            Ok(())
        }
        _ => Err(encoding("decoder state: an open record is not a branch")),
    }
}

impl<H: Hasher> TrieProof<H> {
    /// The proof of the empty trie.
    pub const fn empty() -> Self {
        Self { nodes: Vec::new() }
    }

    /// The number of records.
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    /// Whether this is the proof of the empty trie.
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// Decodes a canonical `trie-proof` value (`shared/schemas/trie.cddl`).
    pub fn from_value(value: &Value) -> Result<Self, TrieError> {
        let records = TRIE_PROOF.decode(value).map_err(encoding)?;
        Self::from_records(&records)
    }

    /// Rebuilds the tree from pre-order records, with every check of docs/specs/trie.md §5: segment padding and
    /// length, digest lengths, masks against the branch invariant, missing children and trailing records.
    pub fn from_records(records: &[TrieRecord]) -> Result<Self, TrieError> {
        let mut nodes: Vec<Node<H>> = Vec::with_capacity(records.len());
        let mut stack: Vec<Open> = Vec::new();
        let mut finished = false;
        for record in records {
            if finished {
                return Err(encoding("trailing proof records"));
            }
            let index = nodes.len();
            match record {
                TrieRecord::TrieStub(TrieStub { h }) => nodes.push(Node::Stub(digest(h)?)),
                TrieRecord::TrieLeaf(TrieLeaf { s, v }) => {
                    nodes.push(Node::Leaf { segment: segment(s)?, value: digest(v)? });
                }
                TrieRecord::TrieBranch(TrieBranch { s, v, c }) => {
                    let (want_left, want_right) = match c {
                        TrieChildren::Left => (true, false),
                        TrieChildren::Right => (false, true),
                        TrieChildren::Both => (true, true),
                    };
                    let value = v.as_deref().map(digest).transpose()?;
                    if !valid_branch(value.is_some(), u8::from(want_left).saturating_add(u8::from(want_right))) {
                        return Err(encoding("branch needs two children, or one child and a value"));
                    }
                    nodes.push(Node::Branch { segment: segment(s)?, value, left: None, right: None });
                    stack.push(Open { index, want_left, want_right, left_done: false });
                    continue;
                }
            }
            // a finished node: give it to the innermost open branch, closing every branch it completes
            let mut child = index;
            loop {
                let Some(mut open) = stack.pop() else {
                    finished = true;
                    break;
                };
                if open.want_left && !open.left_done {
                    set_child(&mut nodes, open.index, child, true)?;
                    open.left_done = true;
                    if open.want_right {
                        stack.push(open);
                        break;
                    }
                } else {
                    set_child(&mut nodes, open.index, child, false)?;
                }
                child = open.index;
            }
        }
        if finished || records.is_empty() {
            Ok(Self { nodes })
        } else {
            Err(encoding("proof ended with unfinished branches"))
        }
    }

    /// The pre-order records.
    pub fn to_records(&self) -> Vec<TrieRecord> {
        self.nodes
            .iter()
            .map(|n| match n {
                Node::Stub(h) => TrieRecord::TrieStub(TrieStub { h: h.as_bytes().to_vec() }),
                Node::Leaf { segment, value } => {
                    TrieRecord::TrieLeaf(TrieLeaf { s: wire_segment(segment), v: value.as_bytes().to_vec() })
                }
                Node::Branch { segment, value, left, right } => TrieRecord::TrieBranch(TrieBranch {
                    s: wire_segment(segment),
                    v: value.as_ref().map(|d| d.as_bytes().to_vec()),
                    c: match (left, right) {
                        (Some(_), Some(_)) => TrieChildren::Both,
                        (Some(_), None) => TrieChildren::Left,
                        _ => TrieChildren::Right,
                    },
                }),
            })
            .collect()
    }

    /// The canonical `trie-proof` value.
    pub fn to_value(&self) -> Result<Value, TrieError> {
        TRIE_PROOF.encode(&self.to_records()).map_err(encoding)
    }

    /// Checks the structure against `mode` (docs/specs/trie.md §4 step 2), then recomputes the root. Every failure is
    /// an error, `RootMismatch` included.
    pub fn check(
        &self,
        hashing: &TrieHashing<H>,
        mode: KeyMode,
        root: &Digest<H>,
    ) -> Result<VerifiedProof<'_, H>, TrieError> {
        let computed = self.compute_root(hashing, mode)?;
        if &computed == root {
            Ok(VerifiedProof { proof: self })
        } else {
            Err(TrieError::RootMismatch { expected: hex(root), computed: hex(&computed) })
        }
    }

    /// As [`check`](Self::check), with a root mismatch as `None`: a well-formed proof of another trie.
    pub fn verify(
        &self,
        hashing: &TrieHashing<H>,
        mode: KeyMode,
        root: &Digest<H>,
    ) -> Result<Option<VerifiedProof<'_, H>>, TrieError> {
        match self.check(hashing, mode, root) {
            Ok(v) => Ok(Some(v)),
            Err(TrieError::RootMismatch { .. }) => Ok(None),
            Err(e) => Err(e),
        }
    }

    /// The root the partial trie hashes to, after the structural checks.
    pub fn compute_root(&self, hashing: &TrieHashing<H>, mode: KeyMode) -> Result<Digest<H>, TrieError> {
        if self.nodes.is_empty() {
            return Ok(hashing.empty());
        }
        let max = mode.max_bits();
        // forward: the bit depth each node starts at, and the structural rules, in pre-order
        let mut depth: Vec<u64> = vec![0; self.nodes.len()];
        for (i, node) in self.nodes.iter().enumerate() {
            let at = depth.get(i).copied().unwrap_or(0);
            match node {
                Node::Stub(_) => {}
                Node::Leaf { segment, .. } => {
                    let end = at.saturating_add(segment.len());
                    let ok = if mode.allows_branch_values() { end <= max && end % 8 == 0 } else { end == max };
                    if !ok {
                        return Err(TrieError::InvalidProof {
                            reason: alloc::format!("leaf ends at bit {end}, invalid for {mode}"),
                        });
                    }
                }
                Node::Branch { segment, value, left, right } => {
                    let here = at.saturating_add(segment.len());
                    let children = u8::from(left.is_some()).saturating_add(u8::from(right.is_some()));
                    if !valid_branch(value.is_some(), children) {
                        return Err(TrieError::InvalidProof {
                            reason: String::from("branch needs two children, or one child and a value"),
                        });
                    }
                    if value.is_some() && (!mode.allows_branch_values() || here % 8 != 0 || here > max) {
                        return Err(TrieError::InvalidProof {
                            reason: alloc::format!("branch value at bit {here} is invalid for {mode}"),
                        });
                    }
                    if here.saturating_add(1) > max {
                        return Err(TrieError::InvalidProof {
                            reason: alloc::format!("branch at bit {here} exceeds {mode}"),
                        });
                    }
                    for c in [left, right].into_iter().flatten() {
                        if let Some(d) = depth.get_mut(*c) {
                            *d = here.saturating_add(1);
                        }
                    }
                }
            }
        }
        // backward: every child's hash is ready before its parent's
        let mut hashes: Vec<Option<Digest<H>>> = vec![None; self.nodes.len()];
        for (i, node) in self.nodes.iter().enumerate().rev() {
            let h = match node {
                Node::Stub(h) => h.clone(),
                Node::Leaf { segment, value } => hashing.leaf(segment, value),
                Node::Branch { segment, value, left, right } => {
                    let mut take = |c: &Option<usize>| -> Result<Option<Digest<H>>, TrieError> {
                        c.map(|c| {
                            hashes
                                .get_mut(c)
                                .and_then(Option::take)
                                .ok_or_else(|| encoding("a child precedes its parent"))
                        })
                        .transpose()
                    };
                    let (l, r) = (take(left)?, take(right)?);
                    hashing.branch(segment, value.as_ref(), l.as_ref(), r.as_ref())
                }
            };
            if let Some(slot) = hashes.get_mut(i) {
                *slot = Some(h);
            }
        }
        hashes.into_iter().next().flatten().ok_or_else(|| encoding("no top node"))
    }
}

/// What a partial trie proves, once its root has been checked against a trusted one.
#[derive(Clone, Copy, Debug)]
pub struct VerifiedProof<'a, H: Hasher> {
    proof: &'a TrieProof<H>,
}

fn into_listing<H: Hasher>(entries: Vec<(Bits, Digest<H>)>) -> Listing<H> {
    // keys of a verified proof end on byte boundaries, so the padded bytes are the key
    entries.into_iter().map(|(k, v)| (k.into_padded_bytes(), v)).collect()
}

impl<'a, H: Hasher> VerifiedProof<'a, H> {
    fn node(&self, i: usize) -> Option<&'a Node<H>> {
        self.proof.nodes.get(i)
    }

    /// The answer for the key with bits `path` (docs/specs/trie.md §4). The key is not checked against the mode.
    pub fn lookup_bits(&self, path: &Bits) -> Lookup<H> {
        let mut i = 0usize;
        let mut pos = 0u64;
        loop {
            let Some(node) = self.node(i) else { return Lookup::Absent };
            match node {
                Node::Stub(_) => return Lookup::NotCovered,
                Node::Leaf { segment, value } => {
                    let exact = path.len().checked_sub(pos) == Some(segment.len())
                        && path.range_eq(pos, segment, 0, segment.len());
                    return if exact { Lookup::Present(value.clone()) } else { Lookup::Absent };
                }
                Node::Branch { segment, value, left, right } => {
                    if !path.range_eq(pos, segment, 0, segment.len()) {
                        return Lookup::Absent;
                    }
                    pos = pos.saturating_add(segment.len());
                    let Some(bit) = path.get(pos) else {
                        return value.clone().map_or(Lookup::Absent, Lookup::Present);
                    };
                    let Some(child) = (if bit { right } else { left }) else { return Lookup::Absent };
                    i = *child;
                    pos = pos.saturating_add(1);
                }
            }
        }
    }

    /// The answer for `key`. The key is not checked against the mode.
    pub fn lookup(&self, key: &[u8]) -> Lookup<H> {
        self.lookup_bits(&Bits::from_bytes(key))
    }

    /// Every entry of the subtree at `start` (whose path is `path`) in key order, keeping only subtrees that meet
    /// `bounds` if given; `None` if a stub is reached. One path buffer is shared by the whole walk.
    fn walk(
        &self,
        start: usize,
        mut path: Bits,
        bounds: Option<(&Bits, Option<&Bits>)>,
    ) -> Option<Vec<(Bits, Digest<H>)>> {
        let mut out = Vec::new();
        let keep = |key: &Bits| bounds.is_none_or(|(from, until)| within(key, from, until));
        // (node, the path length above it, the bit that leads to it)
        let mut todo: Vec<(usize, u64, Option<bool>)> = vec![(start, path.len(), None)];
        while let Some((i, len, bit)) = todo.pop() {
            path.truncate(len);
            if let Some(b) = bit {
                path.push(b);
            }
            match self.node(i)? {
                Node::Stub(_) => return None,
                Node::Leaf { segment, value } => {
                    path.extend(segment);
                    if keep(&path) {
                        out.push((path.clone(), value.clone()));
                    }
                }
                Node::Branch { segment, value, left, right } => {
                    path.extend(segment);
                    let here = path.len();
                    for (child, b) in [(right, true), (left, false)] {
                        let Some(c) = child else { continue };
                        let wanted = match bounds {
                            None => true,
                            Some((from, until)) => {
                                path.push(b);
                                let meets = intersects(&path, from, until);
                                path.truncate(here);
                                meets
                            }
                        };
                        if wanted {
                            todo.push((*c, here, Some(b)));
                        }
                    }
                    if let Some(v) = value {
                        if keep(&path) {
                            out.push((path.clone(), v.clone()));
                        }
                    }
                }
            }
        }
        Some(out)
    }

    /// Every `(path, valueHash)` under `prefix`, in key order, if the proof covers the whole subtree; `None` if any
    /// part of it is a stub. A covered listing is complete.
    pub fn prefix_bits(&self, prefix: &Bits) -> Option<Vec<(Bits, Digest<H>)>> {
        if self.proof.nodes.is_empty() {
            return Some(Vec::new());
        }
        // walk to the node whose subtree holds exactly the keys starting with `prefix`
        let mut i = 0usize;
        let mut consumed = Bits::new();
        let mut pos = 0u64;
        loop {
            let remaining = prefix.len().saturating_sub(pos);
            let node = self.node(i)?;
            let segment = match node {
                Node::Stub(_) => return None,
                Node::Leaf { segment, .. } | Node::Branch { segment, .. } => segment,
            };
            if segment.range_eq(0, prefix, pos, remaining) {
                return self.walk(i, consumed, None);
            }
            let Node::Branch { left, right, .. } = node else { return Some(Vec::new()) };
            if !prefix.range_eq(pos, segment, 0, segment.len()) {
                return Some(Vec::new());
            }
            let rest = pos.saturating_add(segment.len());
            let Some(bit) = prefix.get(rest) else { return Some(Vec::new()) };
            let Some(child) = (if bit { right } else { left }) else { return Some(Vec::new()) };
            consumed.extend(segment);
            consumed.push(bit);
            i = *child;
            pos = rest.saturating_add(1);
        }
    }

    /// [`prefix_bits`](Self::prefix_bits) for a byte prefix, with byte keys.
    pub fn prefix(&self, prefix: &[u8]) -> Option<Listing<H>> {
        self.prefix_bits(&Bits::from_bytes(prefix)).map(into_listing)
    }

    /// Every `(path, valueHash)` with `from <= path < until` (bitwise order, a prefix before its extensions; no
    /// `until` is unbounded), if no stub meets the range; `None` otherwise. A covered listing is complete, including
    /// an empty one.
    pub fn range_bits(&self, from: &Bits, until: Option<&Bits>) -> Option<Vec<(Bits, Digest<H>)>> {
        if self.proof.nodes.is_empty() || !intersects(&Bits::new(), from, until) {
            return Some(Vec::new());
        }
        self.walk(0, Bits::new(), Some((from, until)))
    }

    /// [`range_bits`](Self::range_bits) for byte bounds, with byte keys.
    pub fn range(&self, from: &[u8], until: Option<&[u8]>) -> Option<Listing<H>> {
        let until = until.map(Bits::from_bytes);
        self.range_bits(&Bits::from_bytes(from), until.as_ref()).map(into_listing)
    }
}
