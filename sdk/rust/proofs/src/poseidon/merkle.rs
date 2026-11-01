//! The sparse Poseidon Merkle tree (docs/specs/zk.md §4): fixed depth `d`, positions in `[0, 2^d)`, empty leaves 0,
//! `node = poseidon([left, right])`, siblings root-first. Compatible with metakit and the SP1 shielded-transfer circuit.

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

use ark_ff::AdditiveGroup;
use dilf4s_core::codec::Model;
use dilf4s_core::generated::poseidon::PoseidonMerkleProof as Wire;
use dilf4s_core::value::Value;
use num_bigint::{BigInt, BigUint};

use super::{Fr, compress, fr_from_int, fr_to_int};

/// The deepest tree.
pub const MAX_DEPTH: usize = 256;

/// Why a Poseidon Merkle proof is malformed.
#[derive(Clone, PartialEq, Eq)]
pub enum PoseidonMerkleError {
    /// A position outside `[0, 2^depth)`, or more than 256 siblings.
    Position {
        /// The position.
        position: BigUint,
        /// The depth (the number of siblings).
        depth: usize,
    },
    /// A proof that does not decode, or a sibling outside `[0, r)`.
    Encoding {
        /// Why.
        reason: String,
    },
}

impl fmt::Display for PoseidonMerkleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Position { position, depth } => {
                write!(f, "a position of {} bits is out of range for depth {depth}", position.bits())
            }
            Self::Encoding { reason } => write!(f, "encoding: {reason}"),
        }
    }
}

// a caller's position can be any size: Debug, like Display, shows its bit length, never its digits
impl fmt::Debug for PoseidonMerkleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Position { position, depth } => {
                f.debug_struct("Position").field("position_bits", &position.bits()).field("depth", depth).finish()
            }
            Self::Encoding { reason } => f.debug_struct("Encoding").field("reason", reason).finish(),
        }
    }
}

impl core::error::Error for PoseidonMerkleError {}

impl PoseidonMerkleError {
    /// The Scala case-class name of this kind.
    pub const fn kind(&self) -> &'static str {
        match self {
            Self::Position { .. } => "Position",
            Self::Encoding { .. } => "Encoding",
        }
    }
}

/// A path of siblings, root-first, with `position < 2^depth` and at most 256 siblings.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PoseidonMerkleProof {
    position: BigUint,
    siblings: Vec<Fr>,
}

impl PoseidonMerkleProof {
    /// The proof, if `position < 2^siblings.len()` and there are at most 256 siblings.
    pub fn new(position: BigUint, siblings: Vec<Fr>) -> Result<Self, PoseidonMerkleError> {
        let depth = siblings.len();
        let fits = u64::try_from(depth).is_ok_and(|d| position.bits() <= d);
        if depth <= MAX_DEPTH && fits {
            Ok(Self { position, siblings })
        } else {
            Err(PoseidonMerkleError::Position { position, depth })
        }
    }

    /// Decodes a `poseidon-merkle-proof` value: the schema's shape and ranges, then `position < 2^depth`.
    pub fn from_value(value: &Value) -> Result<Self, PoseidonMerkleError> {
        let encoding = |reason: String| PoseidonMerkleError::Encoding { reason };
        let w = Wire::from_value(value).map_err(|e| encoding(alloc::format!("{e}")))?;
        let siblings = w
            .siblings
            .iter()
            .map(|s| fr_from_int(s).map_err(|e| encoding(alloc::format!("{e}"))))
            .collect::<Result<Vec<_>, _>>()?;
        let position = w.position.to_biguint().ok_or_else(|| encoding(String::from("a negative position")))?;
        Self::new(position, siblings)
    }

    /// The canonical `poseidon-merkle-proof` value.
    pub fn to_value(&self) -> Result<Value, PoseidonMerkleError> {
        Wire {
            position: BigInt::from(self.position.clone()),
            siblings: self.siblings.iter().map(|s| fr_to_int(*s)).collect(),
        }
        .to_value()
        .map_err(|e| PoseidonMerkleError::Encoding { reason: alloc::format!("{e}") })
    }

    /// The leaf position.
    pub const fn position(&self) -> &BigUint {
        &self.position
    }

    /// The siblings, root-first.
    pub fn siblings(&self) -> &[Fr] {
        &self.siblings
    }

    /// The depth: the number of siblings.
    pub fn depth(&self) -> usize {
        self.siblings.len()
    }
}

/// The root a leaf and its path fold to: at level `i` (from the leaf), bit `i` of the position chooses whether the
/// path node is the right child.
pub fn compute_root(leaf: Fr, proof: &PoseidonMerkleProof) -> Fr {
    let mut level = 0u64;
    let mut current = leaf;
    for sibling in proof.siblings.iter().rev() {
        current = if proof.position.bit(level) { compress(*sibling, current) } else { compress(current, *sibling) };
        level = level.saturating_add(1);
    }
    current
}

/// Whether `leaf` is at the proof's position of the tree with `root`.
pub fn verify_inclusion(leaf: Fr, proof: &PoseidonMerkleProof, root: Fr) -> bool {
    compute_root(leaf, proof) == root
}

/// Whether the proof's position is empty (holds 0) in the tree with `root`.
pub fn verify_absence(proof: &PoseidonMerkleProof, root: Fr) -> bool {
    verify_inclusion(Fr::ZERO, proof, root)
}

/// The empty subtree hashes `zero_0 = 0`, `zero_{i+1} = poseidon([zero_i, zero_i])`, for levels `0..=depth`.
pub fn zero_hashes(depth: usize) -> Vec<Fr> {
    let mut out = Vec::with_capacity(depth.saturating_add(1));
    let mut z = Fr::ZERO;
    out.push(z);
    for _ in 0..depth {
        z = compress(z, z);
        out.push(z);
    }
    out
}
