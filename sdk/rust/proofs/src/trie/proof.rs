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
use core::cmp::Ordering;

use crate::bits::{Bits, intersects};
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

/// Where the keys starting with a prefix are in a proof.
enum Located {
    /// A stub is in the way: the proof does not say.
    Stub,
    /// No key starts with the prefix.
    Nowhere,
    /// The subtree of this node, whose path is given, holds exactly those keys.
    At(usize, Bits),
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

    /// Visits every entry of the subtree at `start` (whose path is `path`) in key order, keeping only subtrees that meet
    /// `bounds` if given; `None` if a stub is reached. One path buffer is shared by the whole walk, and each entry is
    /// handed to `visit` as that buffer, never copied.
    ///
    /// The comparisons with the bounds are incremental: each frame carries the length of the common prefix of its
    /// path with `from` and with `until`, extended only over the bits a node adds. So the whole walk is linear in the
    /// proof's size, however long the keys and however many entries share a long prefix.
    fn walk(
        &self,
        start: usize,
        mut path: Bits,
        bounds: Option<(&Bits, Option<&Bits>)>,
        visit: &mut dyn FnMut(&Bits, &Digest<H>),
    ) -> Option<()> {
        let empty = Bits::new();
        let (from, until) = bounds.unwrap_or((&empty, None));
        let bounded = bounds.is_some();
        let common = |path: &Bits, known: u64, upto: u64, bound: &Bits| advance(path, known, upto, bound);
        let start_from = common(&path, 0, 0, from);
        let start_until = until.map_or(0, |u| common(&path, 0, 0, u));
        // (node, the path length above it, the bit that leads to it, the common prefixes at that length)
        let mut todo: Vec<(usize, u64, Option<bool>, u64, u64)> =
            vec![(start, path.len(), None, start_from, start_until)];
        while let Some((i, len, bit, cf, cu)) = todo.pop() {
            path.truncate(len);
            if let Some(b) = bit {
                path.push(b);
            }
            let (cf, cu) = (common(&path, cf, len, from), until.map_or(0, |u| common(&path, cu, len, u)));
            let keep = |path: &Bits, cf: u64, cu: u64| {
                !bounded
                    || (order(path, cf, from) != Ordering::Less
                        && until.is_none_or(|u| order(path, cu, u) == Ordering::Less))
            };
            match self.node(i)? {
                Node::Stub(_) => return None,
                Node::Leaf { segment, value } => {
                    let above = path.len();
                    path.extend(segment);
                    let (cf, cu) = (common(&path, cf, above, from), until.map_or(0, |u| common(&path, cu, above, u)));
                    if keep(&path, cf, cu) {
                        visit(&path, value);
                    }
                }
                Node::Branch { segment, value, left, right } => {
                    let above = path.len();
                    path.extend(segment);
                    let here = path.len();
                    let (cf, cu) = (common(&path, cf, above, from), until.map_or(0, |u| common(&path, cu, above, u)));
                    for (child, b) in [(right, true), (left, false)] {
                        let Some(c) = child else { continue };
                        let wanted = !bounded || {
                            path.push(b);
                            let (f, u) =
                                (common(&path, cf, here, from), until.map_or(0, |x| common(&path, cu, here, x)));
                            let meets = meets(&path, f, from, until.map(|x| (x, u)));
                            path.truncate(here);
                            meets
                        };
                        if wanted {
                            todo.push((*c, here, Some(b), cf, cu));
                        }
                    }
                    if let Some(v) = value {
                        if keep(&path, cf, cu) {
                            visit(&path, v);
                        }
                    }
                }
            }
        }
        Some(())
    }

    /// Every entry `walk` visits, collected.
    fn collect(
        &self,
        start: usize,
        path: Bits,
        bounds: Option<(&Bits, Option<&Bits>)>,
    ) -> Option<Vec<(Bits, Digest<H>)>> {
        let mut out = Vec::new();
        self.walk(start, path, bounds, &mut |p, d| out.push((p.clone(), d.clone())))?;
        Some(out)
    }

    /// Whether the entries `walk` visits are exactly `claimed` (byte keys, in key order), compared as they are
    /// visited: nothing is listed, and comparing stops at the first difference, while the walk still goes on to find
    /// any stub (a stub anywhere in the query is `None`, whatever the claim).
    fn matches(
        &self,
        start: usize,
        path: Bits,
        bounds: Option<(&Bits, Option<&Bits>)>,
        claimed: &Listing<H>,
    ) -> Option<bool> {
        let mut next = 0usize;
        let mut equal = true;
        self.walk(start, path, bounds, &mut |p, d| {
            equal = equal
                && claimed.get(next).is_some_and(|(k, h)| {
                    u64::try_from(k.len()).ok().and_then(|n| n.checked_mul(8)) == Some(p.len())
                        && p.padded_bytes() == k.as_slice()
                        && h == d
                });
            next = next.saturating_add(1);
        })?;
        Some(equal && next == claimed.len())
    }

    /// Where the keys starting with `prefix` are.
    fn locate(&self, prefix: &Bits) -> Located {
        if self.proof.nodes.is_empty() {
            return Located::Nowhere;
        }
        let mut i = 0usize;
        let mut consumed = Bits::new();
        let mut pos = 0u64;
        loop {
            let remaining = prefix.len().saturating_sub(pos);
            let Some(node) = self.node(i) else { return Located::Stub };
            let segment = match node {
                Node::Stub(_) => return Located::Stub,
                Node::Leaf { segment, .. } | Node::Branch { segment, .. } => segment,
            };
            if segment.range_eq(0, prefix, pos, remaining) {
                return Located::At(i, consumed);
            }
            let Node::Branch { left, right, .. } = node else { return Located::Nowhere };
            if !prefix.range_eq(pos, segment, 0, segment.len()) {
                return Located::Nowhere;
            }
            let rest = pos.saturating_add(segment.len());
            let Some(bit) = prefix.get(rest) else { return Located::Nowhere };
            let Some(child) = (if bit { right } else { left }) else { return Located::Nowhere };
            consumed.extend(segment);
            consumed.push(bit);
            i = *child;
            pos = rest.saturating_add(1);
        }
    }

    /// Every `(path, valueHash)` under `prefix`, in key order, if the proof covers the whole subtree; `None` if any
    /// part of it is a stub. A covered listing is complete.
    pub fn prefix_bits(&self, prefix: &Bits) -> Option<Vec<(Bits, Digest<H>)>> {
        match self.locate(prefix) {
            Located::Stub => None,
            Located::Nowhere => Some(Vec::new()),
            Located::At(i, at) => self.collect(i, at, None),
        }
    }

    /// [`prefix_bits`](Self::prefix_bits) for a byte prefix, with byte keys.
    pub fn prefix(&self, prefix: &[u8]) -> Option<Listing<H>> {
        self.prefix_bits(&Bits::from_bytes(prefix)).map(into_listing)
    }

    /// Whether `claimed` is exactly the listing under `prefix`, without building the listing; `None` if a stub lies
    /// under the prefix.
    pub fn lists_prefix(&self, prefix: &[u8], claimed: &Listing<H>) -> Option<bool> {
        match self.locate(&Bits::from_bytes(prefix)) {
            Located::Stub => None,
            Located::Nowhere => Some(claimed.is_empty()),
            Located::At(i, at) => self.matches(i, at, None, claimed),
        }
    }

    /// Every `(path, valueHash)` with `from <= path < until` (bitwise order, a prefix before its extensions; no
    /// `until` is unbounded), if no stub meets the range; `None` otherwise. A covered listing is complete, including
    /// an empty one.
    pub fn range_bits(&self, from: &Bits, until: Option<&Bits>) -> Option<Vec<(Bits, Digest<H>)>> {
        if self.proof.nodes.is_empty() || !intersects(&Bits::new(), from, until) {
            return Some(Vec::new());
        }
        self.collect(0, Bits::new(), Some((from, until)))
    }

    /// [`range_bits`](Self::range_bits) for byte bounds, with byte keys.
    pub fn range(&self, from: &[u8], until: Option<&[u8]>) -> Option<Listing<H>> {
        let until = until.map(Bits::from_bytes);
        self.range_bits(&Bits::from_bytes(from), until.as_ref()).map(into_listing)
    }

    /// Whether `claimed` is exactly the listing of `[from, until)`, without building the listing; `None` if a stub
    /// meets the range.
    pub fn lists_range(&self, from: &[u8], until: Option<&[u8]>, claimed: &Listing<H>) -> Option<bool> {
        let (from, until) = (Bits::from_bytes(from), until.map(Bits::from_bytes));
        if self.proof.nodes.is_empty() || !intersects(&Bits::new(), &from, until.as_ref()) {
            return Some(claimed.is_empty());
        }
        self.matches(0, Bits::new(), Some((&from, until.as_ref())), claimed)
    }
}

/// The common prefix of `path` and `bound`, given `known`, its length over `path`'s first `upto` bits: extended over
/// the bits after `upto` only while the two have agreed so far.
fn advance(path: &Bits, known: u64, upto: u64, bound: &Bits) -> u64 {
    if known < upto { known } else { upto.saturating_add(path.common_prefix_from(upto, bound, upto)) }
}

/// `path` against `bound` in key order (a prefix before its extensions), from the length of their common prefix.
fn order(path: &Bits, common: u64, bound: &Bits) -> Ordering {
    match (path.get(common), bound.get(common)) {
        (None, None) => Ordering::Equal,
        (None, Some(_)) => Ordering::Less,
        (Some(_), None) => Ordering::Greater,
        (Some(a), Some(b)) => a.cmp(&b),
    }
}

/// Whether the keys starting with `prefix` meet `[from, until)` (`bits::intersects`), from the common prefixes.
fn meets(prefix: &Bits, cf: u64, from: &Bits, until: Option<(&Bits, u64)>) -> bool {
    // wholly below: `from` does not extend the prefix, and the prefix sorts before it
    let from_extends = cf == prefix.len();
    let wholly_below = !from_extends && order(prefix, cf, from) == Ordering::Less;
    // wholly above: `until <= prefix`
    let wholly_above = until.is_some_and(|(u, cu)| order(prefix, cu, u) != Ordering::Less);
    !wholly_below && !wholly_above
}
