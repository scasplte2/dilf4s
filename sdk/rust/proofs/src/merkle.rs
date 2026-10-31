//! The dilf4s Merkle tree (docs/specs/merkle.md): RFC 9162 trees under the tagged profile or the RFC 6962 profile,
//! inclusion and consistency verification, and proof decoding.
//!
//! The tagged profile takes leaves as canonical values, committed by `commit(v)`, or as leaf hashes already computed.
//! It never takes raw leaf bytes: only the RFC 6962 profile, for external logs, reads a leaf as its bytes.

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;
use core::marker::PhantomData;

use dilf4s_core::codec::Model;
use dilf4s_core::commitment::value_commitment;
use dilf4s_core::generated::merkle::{MerkleConsistency, MerkleInclusion};
use dilf4s_core::hash::{Digest, Hasher};
use dilf4s_core::value::Value;
use num_traits::ToPrimitive;

use crate::tag::{Tag, hex};

/// The tags of the tagged profile.
pub const LEAF_TAG: &str = "dilf4s/merkle/leaf/v1";
/// The node tag.
pub const NODE_TAG: &str = "dilf4s/merkle/node/v1";
/// The empty-tree tag.
pub const EMPTY_TAG: &str = "dilf4s/merkle/empty/v1";

/// Why a Merkle proof failed. `InvalidProof` and `RootMismatch` are a well-formed proof that proves something else;
/// the other kinds are malformed input.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MerkleError {
    /// A leaf index (or an old size) outside its tree.
    IndexOutOfRange {
        /// The index.
        index: u64,
        /// The tree size.
        size: u64,
    },
    /// Consistency sizes with `old > new`.
    InvalidSizes {
        /// The old size.
        old_size: u64,
        /// The new size.
        new_size: u64,
    },
    /// A path of the wrong length for its sizes, or an empty-tree case that does not hold.
    InvalidProof {
        /// Why.
        reason: &'static str,
    },
    /// The recomputed root differs from the trusted one.
    RootMismatch {
        /// The trusted root, hex.
        expected: String,
        /// The recomputed root, hex.
        computed: String,
    },
    /// A proof or head whose encoding does not decode.
    Encoding {
        /// Why.
        reason: String,
    },
}

impl fmt::Display for MerkleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::IndexOutOfRange { index, size } => write!(f, "index {index} out of range for size {size}"),
            Self::InvalidSizes { old_size, new_size } => write!(f, "invalid sizes {old_size} > {new_size}"),
            Self::InvalidProof { reason } => write!(f, "invalid proof: {reason}"),
            Self::RootMismatch { expected, computed } => {
                write!(f, "root mismatch: expected {expected}, computed {computed}")
            }
            Self::Encoding { reason } => write!(f, "encoding: {reason}"),
        }
    }
}

impl core::error::Error for MerkleError {}

impl MerkleError {
    /// The Scala case-class name of this kind.
    pub const fn kind(&self) -> &'static str {
        match self {
            Self::IndexOutOfRange { .. } => "IndexOutOfRange",
            Self::InvalidSizes { .. } => "InvalidSizes",
            Self::InvalidProof { .. } => "InvalidProof",
            Self::RootMismatch { .. } => "RootMismatch",
            Self::Encoding { .. } => "Encoding",
        }
    }

    /// Whether this is a verification failure (`false`) rather than malformed input.
    pub const fn is_invalid(&self) -> bool {
        matches!(self, Self::InvalidProof { .. } | Self::RootMismatch { .. })
    }
}

/// A Merkle profile: how leaves, nodes and the empty tree hash (docs/specs/merkle.md §2).
pub trait MerkleHashing<H: Hasher> {
    /// What a leaf is: a canonical value for the tagged profile, raw bytes for RFC 6962.
    type Leaf: ?Sized;
    /// The hash of a leaf.
    fn leaf_hash(&self, leaf: &Self::Leaf) -> Digest<H>;
    /// The hash of an interior node.
    fn node_hash(&self, left: &Digest<H>, right: &Digest<H>) -> Digest<H>;
    /// The root of the empty tree.
    fn empty_hash(&self) -> Digest<H>;
}

/// The dilf4s profile: tagged hashes per role, and value leaves committed by `commit(v)`.
pub struct Tagged<H: Hasher> {
    leaf: Tag<H>,
    node: Tag<H>,
    empty: Digest<H>,
}

impl<H: Hasher> Clone for Tagged<H> {
    fn clone(&self) -> Self {
        Self { leaf: self.leaf.clone(), node: self.node.clone(), empty: self.empty.clone() }
    }
}

impl<H: Hasher> Default for Tagged<H> {
    fn default() -> Self {
        Self::new()
    }
}

impl<H: Hasher> Tagged<H> {
    /// The profile, with its tag hashes computed.
    pub fn new() -> Self {
        Self { leaf: Tag::new(LEAF_TAG), node: Tag::new(NODE_TAG), empty: Tag::<H>::new(EMPTY_TAG).hash(&[]) }
    }
}

impl<H: Hasher> MerkleHashing<H> for Tagged<H> {
    type Leaf = Value;

    /// `leafHash(commit(v))`: O([`Value::weight`]), which the JLVM prices before calling (jlvm.md section 7).
    fn leaf_hash(&self, leaf: &Value) -> Digest<H> {
        self.leaf.hash(&[&value_commitment(leaf)])
    }

    fn node_hash(&self, left: &Digest<H>, right: &Digest<H>) -> Digest<H> {
        self.node.hash(&[left.as_bytes(), right.as_bytes()])
    }

    fn empty_hash(&self) -> Digest<H> {
        self.empty.clone()
    }
}

/// The RFC 6962 profile, for external logs: `H(0x00 || d)`, `H(0x01 || l || r)` and `H("")`, over raw leaf bytes.
pub struct Rfc6962<H: Hasher>(PhantomData<H>);

impl<H: Hasher> Clone for Rfc6962<H> {
    fn clone(&self) -> Self {
        Self::new()
    }
}

impl<H: Hasher> Default for Rfc6962<H> {
    fn default() -> Self {
        Self::new()
    }
}

impl<H: Hasher> Rfc6962<H> {
    /// The profile.
    pub const fn new() -> Self {
        Self(PhantomData)
    }
}

impl<H: Hasher> MerkleHashing<H> for Rfc6962<H> {
    type Leaf = [u8];

    fn leaf_hash(&self, leaf: &[u8]) -> Digest<H> {
        Digest::from_output(H::hash_parts(&[&[0x00], leaf]))
    }

    fn node_hash(&self, left: &Digest<H>, right: &Digest<H>) -> Digest<H> {
        Digest::from_output(H::hash_parts(&[&[0x01], left.as_bytes(), right.as_bytes()]))
    }

    fn empty_hash(&self) -> Digest<H> {
        Digest::from_output(H::hash_parts(&[]))
    }
}

fn encoding(reason: impl fmt::Display) -> MerkleError {
    MerkleError::Encoding { reason: alloc::format!("{reason}") }
}

fn digests<H: Hasher>(path: &[Vec<u8>]) -> Result<Vec<Digest<H>>, MerkleError> {
    path.iter().map(|b| Digest::<H>::from_slice(b).map_err(encoding)).collect()
}

/// An audit path for leaf `index` of a tree of `size` leaves, `index < size`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InclusionProof<H: Hasher> {
    index: u64,
    size: u64,
    path: Vec<Digest<H>>,
}

impl<H: Hasher> InclusionProof<H> {
    /// The proof, if `index < size`.
    pub fn new(index: u64, size: u64, path: Vec<Digest<H>>) -> Result<Self, MerkleError> {
        if index < size { Ok(Self { index, size, path }) } else { Err(MerkleError::IndexOutOfRange { index, size }) }
    }

    /// Decodes a `merkle-inclusion` value: the schema, digest lengths for `H`, and `index < size`.
    pub fn from_value(value: &Value) -> Result<Self, MerkleError> {
        let w = MerkleInclusion::from_value(value).map_err(encoding)?;
        Self::new(w.index, w.size, digests(&w.path)?)
    }

    /// The canonical `merkle-inclusion` value.
    pub fn to_value(&self) -> Result<Value, MerkleError> {
        MerkleInclusion {
            index: self.index,
            size: self.size,
            path: self.path.iter().map(|d| d.as_bytes().to_vec()).collect(),
        }
        .to_value()
        .map_err(encoding)
    }

    /// The leaf index.
    pub const fn index(&self) -> u64 {
        self.index
    }

    /// The tree size.
    pub const fn size(&self) -> u64 {
        self.size
    }

    /// The audit path, leaf first.
    pub fn path(&self) -> &[Digest<H>] {
        &self.path
    }
}

/// A consistency path between tree sizes `old_size <= new_size`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConsistencyProof<H: Hasher> {
    old_size: u64,
    new_size: u64,
    path: Vec<Digest<H>>,
}

impl<H: Hasher> ConsistencyProof<H> {
    /// The proof, if `old_size <= new_size`. Sizes out of order are `InvalidSizes`, as in Scala's
    /// `ConsistencyProof.from` and the verifier.
    pub fn new(old_size: u64, new_size: u64, path: Vec<Digest<H>>) -> Result<Self, MerkleError> {
        if old_size <= new_size {
            Ok(Self { old_size, new_size, path })
        } else {
            Err(MerkleError::InvalidSizes { old_size, new_size })
        }
    }

    /// Decodes a `merkle-consistency` value: the schema, digest lengths for `H`, and `old <= new`.
    pub fn from_value(value: &Value) -> Result<Self, MerkleError> {
        let w = MerkleConsistency::from_value(value).map_err(encoding)?;
        Self::new(w.old, w.new, digests(&w.path)?)
    }

    /// The canonical `merkle-consistency` value.
    pub fn to_value(&self) -> Result<Value, MerkleError> {
        MerkleConsistency {
            old: self.old_size,
            new: self.new_size,
            path: self.path.iter().map(|d| d.as_bytes().to_vec()).collect(),
        }
        .to_value()
        .map_err(encoding)
    }

    /// The old size.
    pub const fn old_size(&self) -> u64 {
        self.old_size
    }

    /// The new size.
    pub const fn new_size(&self) -> u64 {
        self.new_size
    }

    /// The path.
    pub fn path(&self) -> &[Digest<H>] {
        &self.path
    }
}

/// The root over `leaf_hashes`: pair adjacent nodes level by level, promoting an unpaired last node.
pub fn root<H: Hasher, M: MerkleHashing<H>>(profile: &M, leaf_hashes: &[Digest<H>]) -> Digest<H> {
    let mut level: Vec<Digest<H>> = leaf_hashes.to_vec();
    if level.is_empty() {
        return profile.empty_hash();
    }
    while level.len() > 1 {
        level = level
            .chunks(2)
            .filter_map(|pair| match pair {
                [l, r] => Some(profile.node_hash(l, r)),
                [single] => Some(single.clone()),
                _ => None,
            })
            .collect();
    }
    level.pop().unwrap_or_else(|| profile.empty_hash())
}

/// Right-shifts both until `fn` is odd or zero.
const fn shift_until_odd_or_zero(mut f: u64, mut s: u64) -> (u64, u64) {
    while f % 2 == 0 && f != 0 {
        f /= 2;
        s /= 2;
    }
    (f, s)
}

/// RFC 9162 §2.1.3.2, with the reason when it fails.
pub fn check_inclusion<H: Hasher, M: MerkleHashing<H>>(
    profile: &M,
    root: &Digest<H>,
    leaf_hash: &Digest<H>,
    proof: &InclusionProof<H>,
) -> Result<(), MerkleError> {
    if proof.index >= proof.size {
        return Err(MerkleError::IndexOutOfRange { index: proof.index, size: proof.size });
    }
    let mut f = proof.index;
    let mut s = proof.size.saturating_sub(1);
    let mut r = leaf_hash.clone();
    for p in &proof.path {
        if s == 0 {
            return Err(MerkleError::InvalidProof { reason: "path is too long for the tree size" });
        }
        if f % 2 == 1 || f == s {
            r = profile.node_hash(p, &r);
            (f, s) = shift_until_odd_or_zero(f, s);
        } else {
            r = profile.node_hash(&r, p);
        }
        f /= 2;
        s /= 2;
    }
    if s != 0 {
        return Err(MerkleError::InvalidProof { reason: "path is too short for the tree size" });
    }
    if &r == root { Ok(()) } else { Err(MerkleError::RootMismatch { expected: hex(root), computed: hex(&r) }) }
}

/// RFC 9162 §2.1.3.2 as a verdict: `false` for `InvalidProof` and `RootMismatch`, an error for malformed input.
pub fn verify_inclusion<H: Hasher, M: MerkleHashing<H>>(
    profile: &M,
    root: &Digest<H>,
    leaf_hash: &Digest<H>,
    proof: &InclusionProof<H>,
) -> Result<bool, MerkleError> {
    verdict(check_inclusion(profile, root, leaf_hash, proof))
}

/// RFC 9162 §2.1.4.2, with the reason when it fails.
pub fn check_consistency<H: Hasher, M: MerkleHashing<H>>(
    profile: &M,
    old_root: &Digest<H>,
    new_root: &Digest<H>,
    proof: &ConsistencyProof<H>,
) -> Result<(), MerkleError> {
    let (m, n) = (proof.old_size, proof.new_size);
    if m > n {
        return Err(MerkleError::InvalidSizes { old_size: m, new_size: n });
    }
    if m == 0 {
        return if proof.path.is_empty() && *old_root == profile.empty_hash() {
            Ok(())
        } else {
            Err(MerkleError::InvalidProof { reason: "size 0 needs an empty proof and the empty root" })
        };
    }
    if m == n {
        return if proof.path.is_empty() && old_root == new_root {
            Ok(())
        } else {
            Err(MerkleError::InvalidProof { reason: "equal sizes need an empty proof and equal roots" })
        };
    }
    let mut path = proof.path.iter();
    let first = if m.is_power_of_two() { Some(old_root) } else { path.next() };
    let Some(first) = first else {
        return Err(MerkleError::InvalidProof { reason: "empty consistency path" });
    };
    // shift while odd
    let (mut f, mut s) = (m.saturating_sub(1), n.saturating_sub(1));
    while f % 2 == 1 {
        f /= 2;
        s /= 2;
    }
    let mut fr = first.clone();
    let mut sr = first.clone();
    for c in path {
        if s == 0 {
            return Err(MerkleError::InvalidProof { reason: "path is too long for the sizes" });
        }
        if f % 2 == 1 || f == s {
            (f, s) = shift_until_odd_or_zero(f, s);
            fr = profile.node_hash(c, &fr);
            sr = profile.node_hash(c, &sr);
        } else {
            sr = profile.node_hash(&sr, c);
        }
        f /= 2;
        s /= 2;
    }
    if s != 0 {
        return Err(MerkleError::InvalidProof { reason: "path is too short for the sizes" });
    }
    if &fr != old_root {
        return Err(MerkleError::RootMismatch { expected: hex(old_root), computed: hex(&fr) });
    }
    if &sr == new_root {
        Ok(())
    } else {
        Err(MerkleError::RootMismatch { expected: hex(new_root), computed: hex(&sr) })
    }
}

/// RFC 9162 §2.1.4.2 as a verdict: `false` for `InvalidProof` and `RootMismatch`, an error for malformed input.
pub fn verify_consistency<H: Hasher, M: MerkleHashing<H>>(
    profile: &M,
    old_root: &Digest<H>,
    new_root: &Digest<H>,
    proof: &ConsistencyProof<H>,
) -> Result<bool, MerkleError> {
    verdict(check_consistency(profile, old_root, new_root, proof))
}

fn verdict(r: Result<(), MerkleError>) -> Result<bool, MerkleError> {
    match r {
        Ok(()) => Ok(true),
        Err(e) if e.is_invalid() => Ok(false),
        Err(e) => Err(e),
    }
}

/// A trusted tree head: a root and the size of the same tree (docs/specs/jlvm.md §6).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TreeHead<H: Hasher> {
    /// The root.
    pub root: Digest<H>,
    /// The number of leaves.
    pub size: u64,
}

impl<H: Hasher> TreeHead<H> {
    /// A head from its JLVM form `{"root": bytes, "size": n}`: exactly those keys, a digest of `H`, and a size in
    /// `0..2^63`.
    pub fn from_value(value: &Value) -> Result<Self, MerkleError> {
        let Value::Map(m) = value else { return Err(encoding("a tree head is {root, size}")) };
        if m.len() != 2 {
            return Err(encoding("a tree head is {root, size}"));
        }
        let root = match m.get("root") {
            Some(Value::Bytes(b)) => Digest::<H>::from_slice(b).map_err(encoding)?,
            _ => return Err(encoding("head.root is a digest")),
        };
        let size = match m.get("size") {
            Some(Value::Int(n)) => n.to_i64().and_then(|v| u64::try_from(v).ok()),
            _ => None,
        }
        .ok_or_else(|| encoding("head.size is a tree size"))?;
        Ok(Self { root, size })
    }
}

/// `merkle_verify [head, leaf, proof]`: the leaf (a value for the tagged profile, bytes for RFC 6962) is at the
/// proof's index of the tree `head` names. A proof whose size differs from the head's is `false`; an undecodable proof
/// is an error. O(the weight of the leaf and the proof), which the JLVM prices before calling (jlvm.md section 7).
pub fn merkle_verify<H: Hasher, M: MerkleHashing<H>>(
    profile: &M,
    head: &TreeHead<H>,
    leaf: &M::Leaf,
    proof: &Value,
) -> Result<bool, MerkleError> {
    let p = InclusionProof::<H>::from_value(proof)?;
    if p.size != head.size {
        return Ok(false);
    }
    verify_inclusion(profile, &head.root, &profile.leaf_hash(leaf), &p)
}

/// `merkle_consistency_verify [oldHead, newHead, proof]`: the new tree extends the old one. Sizes that differ from
/// the heads' are `false`.
pub fn merkle_consistency_verify<H: Hasher, M: MerkleHashing<H>>(
    profile: &M,
    old: &TreeHead<H>,
    new: &TreeHead<H>,
    proof: &Value,
) -> Result<bool, MerkleError> {
    let p = ConsistencyProof::<H>::from_value(proof)?;
    if p.old_size != old.size || p.new_size != new.size {
        return Ok(false);
    }
    verify_consistency(profile, &old.root, &new.root, &p)
}
