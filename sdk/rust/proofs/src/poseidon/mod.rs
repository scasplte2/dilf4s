//! Poseidon over the BN254 scalar field (docs/specs/zk.md §3), byte-compatible with circomlib and circomlibjs: x^5
//! S-box, 8 full rounds, width `t = inputs + 1` for 1 to 4 inputs, state `[0, x1, …, xn]`, output `state[0]`.
//!
//! [`hash`] evaluates the optimised form (the paper's Appendix B, as Scala and circomlibjs's `poseidon_opt` lay it
//! out); [`hash_reference`] evaluates the rounds as defined, over the base constants. The tests check they agree.
//!
//! **Public data only**: the arithmetic is variable time.

use ark_ff::{AdditiveGroup, BigInt, BigInteger, Field, PrimeField};
use core::fmt;

pub mod merkle;

#[rustfmt::skip]
pub mod constants;

/// An element of the BN254 scalar field.
pub type Fr = ark_bn254::Fr;

/// The most inputs.
pub const MAX_INPUTS: usize = 4;
/// Full rounds.
pub const FULL_ROUNDS: usize = 8;
const HALF: usize = FULL_ROUNDS / 2;

/// Why hashing or a field element failed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PoseidonError {
    /// Not 1 to 4 inputs.
    InputCount {
        /// The inputs given.
        count: usize,
        /// The most allowed.
        max: usize,
    },
    /// A value outside `[0, r)`: malformed, never reduced.
    NonCanonical,
}

impl fmt::Display for PoseidonError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InputCount { count, max } => write!(f, "{count} inputs, expected 1 to {max}"),
            Self::NonCanonical => f.write_str("not a canonical field element"),
        }
    }
}

impl core::error::Error for PoseidonError {}

/// One width's constants. `T` is the width, `RP` the partial rounds and `R = 8 + RP` the rounds.
///
/// The base form is `round_constants` (one row per round) and `mds`. The optimised form, with `h = 4`: `first` is added
/// before the first S-box; `full_first` after the S-boxes of rounds `1..h`; `opened` after round `h`'s, which
/// multiplies by `pre_sparse`; `partial` is one scalar per partial round, added to element 0 after its S-box, whose
/// sparse matrix has first row `sparse_w` and first column `sparse_v` (element 0 unused) below an identity; and
/// `full_last` after the S-boxes of the last full rounds but the final one, which adds none. Matrices act as
/// `new_i = Σ_j m_ij · s_j`.
pub struct Params<const T: usize, const RP: usize, const R: usize> {
    /// Round constants, per round.
    pub round_constants: [[Fr; T]; R],
    /// The MDS matrix.
    pub mds: [[Fr; T]; T],
    /// Added before the first S-box.
    pub first: [Fr; T],
    /// Added after the S-boxes of full rounds `1..4`.
    pub full_first: [[Fr; T]; 3],
    /// Added after the S-boxes of full round 4.
    pub opened: [Fr; T],
    /// One per partial round.
    pub partial: [Fr; RP],
    /// Added after the S-boxes of the last full rounds but the final one.
    pub full_last: [[Fr; T]; 3],
    /// The first row of each partial round's sparse matrix.
    pub sparse_w: [[Fr; T]; RP],
    /// The first column of each partial round's sparse matrix, below element 0 (which is 0).
    pub sparse_v: [[Fr; T]; RP],
    /// The matrix of full round 4.
    pub pre_sparse: [[Fr; T]; T],
}

fn pow5(x: Fr) -> Fr {
    let x2 = x.square();
    x2.square() * x
}

fn dot<const T: usize>(row: &[Fr; T], s: &[Fr; T]) -> Fr {
    row.iter().zip(s).fold(Fr::ZERO, |acc, (a, b)| acc + *a * b)
}

fn apply<const T: usize>(m: &[[Fr; T]; T], s: &[Fr; T]) -> [Fr; T] {
    m.each_ref().map(|row| dot(row, s))
}

fn add<const T: usize>(mut s: [Fr; T], c: &[Fr; T]) -> [Fr; T] {
    for (x, k) in s.iter_mut().zip(c) {
        *x += k;
    }
    s
}

fn full_round<const T: usize>(s: [Fr; T], m: &[[Fr; T]; T], c: &[Fr; T]) -> [Fr; T] {
    apply(m, &add(s.map(pow5), c))
}

fn partial_round<const T: usize>(mut s: [Fr; T], c: Fr, w: &[Fr; T], v: &[Fr; T]) -> [Fr; T] {
    let x0 = s.first().map_or(Fr::ZERO, |x| pow5(*x) + c);
    if let Some(first) = s.first_mut() {
        *first = x0;
    }
    let head = dot(w, &s);
    for (x, vk) in s.iter_mut().zip(v) {
        *x += x0 * vk;
    }
    if let Some(first) = s.first_mut() {
        *first = head;
    }
    s
}

impl<const T: usize, const RP: usize, const R: usize> Params<T, RP, R> {
    /// The permutation's output element 0, in the optimised form.
    pub fn permute(&self, state: [Fr; T]) -> Fr {
        let mut s = add(state, &self.first);
        for c in &self.full_first {
            s = full_round(s, &self.mds, c);
        }
        s = full_round(s, &self.pre_sparse, &self.opened);
        for ((c, w), v) in self.partial.iter().zip(&self.sparse_w).zip(&self.sparse_v) {
            s = partial_round(s, *c, w, v);
        }
        for c in &self.full_last {
            s = full_round(s, &self.mds, c);
        }
        // the last round adds no constant, and only element 0 of its output is read
        let boxed = s.map(pow5);
        self.mds.first().map_or(Fr::ZERO, |row| dot(row, &boxed))
    }

    /// The permutation as defined: each round adds its constants, applies the S-box to every element (full rounds)
    /// or to element 0 (partial rounds), and multiplies by the MDS matrix.
    pub fn permute_reference(&self, state: [Fr; T]) -> [Fr; T] {
        let mut s = state;
        for (r, c) in self.round_constants.iter().enumerate() {
            s = add(s, c);
            if r < HALF || r >= HALF.saturating_add(RP) {
                s = s.map(pow5);
            } else if let Some(x) = s.first_mut() {
                *x = pow5(*x);
            }
            s = apply(&self.mds, &s);
        }
        s
    }
}

fn state<const T: usize, const N: usize>(inputs: [Fr; N]) -> [Fr; T] {
    let mut s = [Fr::ZERO; T];
    for (x, i) in s.iter_mut().skip(1).zip(inputs) {
        *x = i;
    }
    s
}

/// `poseidon([a])`.
pub fn hash1(a: Fr) -> Fr {
    constants::WIDTH2.permute(state([a]))
}

/// `poseidon([a, b])`.
pub fn hash2(a: Fr, b: Fr) -> Fr {
    constants::WIDTH3.permute(state([a, b]))
}

/// `poseidon([a, b, c])`.
pub fn hash3(a: Fr, b: Fr, c: Fr) -> Fr {
    constants::WIDTH4.permute(state([a, b, c]))
}

/// `poseidon([a, b, c, d])`.
pub fn hash4(a: Fr, b: Fr, c: Fr, d: Fr) -> Fr {
    constants::WIDTH5.permute(state([a, b, c, d]))
}

/// Two-to-one compression for Merkle nodes: `poseidon([l, r])`.
pub fn compress(left: Fr, right: Fr) -> Fr {
    hash2(left, right)
}

/// `poseidon(inputs)` for 1 to 4 inputs.
pub fn hash(inputs: &[Fr]) -> Result<Fr, PoseidonError> {
    match *inputs {
        [a] => Ok(hash1(a)),
        [a, b] => Ok(hash2(a, b)),
        [a, b, c] => Ok(hash3(a, b, c)),
        [a, b, c, d] => Ok(hash4(a, b, c, d)),
        _ => Err(PoseidonError::InputCount { count: inputs.len(), max: MAX_INPUTS }),
    }
}

/// `poseidon(inputs)` through the rounds as defined, over the base constants.
pub fn hash_reference(inputs: &[Fr]) -> Result<Fr, PoseidonError> {
    let first = |s: &[Fr]| s.first().copied().unwrap_or(Fr::ZERO);
    match *inputs {
        [a] => Ok(first(&constants::WIDTH2.permute_reference(state([a])))),
        [a, b] => Ok(first(&constants::WIDTH3.permute_reference(state([a, b])))),
        [a, b, c] => Ok(first(&constants::WIDTH4.permute_reference(state([a, b, c])))),
        [a, b, c, d] => Ok(first(&constants::WIDTH5.permute_reference(state([a, b, c, d])))),
        _ => Err(PoseidonError::InputCount { count: inputs.len(), max: MAX_INPUTS }),
    }
}

/// A field element from 32 big-endian bytes, which must be below `r`.
pub fn fr_from_be_bytes(bytes: &[u8]) -> Result<Fr, PoseidonError> {
    let array: &[u8; 32] = bytes.try_into().map_err(|_| PoseidonError::NonCanonical)?;
    let mut limbs = [0u64; 4];
    for (limb, chunk) in limbs.iter_mut().zip(array.rchunks_exact(8)) {
        let mut word = [0u8; 8];
        word.copy_from_slice(chunk);
        *limb = u64::from_be_bytes(word);
    }
    Fr::from_bigint(BigInt::new(limbs)).ok_or(PoseidonError::NonCanonical)
}

/// A field element's 32 big-endian bytes.
pub fn fr_to_be_bytes(x: Fr) -> [u8; 32] {
    let mut out = [0u8; 32];
    let bytes = x.into_bigint().to_bytes_be();
    for (o, b) in out.iter_mut().rev().zip(bytes.iter().rev()) {
        *o = *b;
    }
    out
}

/// A field element from an integer, which must be in `[0, r)`.
pub fn fr_from_int(n: &num_bigint::BigInt) -> Result<Fr, PoseidonError> {
    let (sign, magnitude) = n.to_bytes_be();
    if sign == num_bigint::Sign::Minus || magnitude.len() > 32 {
        return Err(PoseidonError::NonCanonical);
    }
    let mut padded = [0u8; 32];
    for (o, b) in padded.iter_mut().rev().zip(magnitude.iter().rev()) {
        *o = *b;
    }
    fr_from_be_bytes(&padded)
}

/// A field element as an integer.
pub fn fr_to_int(x: Fr) -> num_bigint::BigInt {
    num_bigint::BigInt::from_bytes_be(num_bigint::Sign::Plus, &fr_to_be_bytes(x))
}
