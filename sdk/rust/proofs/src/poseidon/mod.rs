//! Poseidon over the BN254 scalar field (docs/specs/zk.md §3), byte-compatible with circomlib and circomlibjs: x^5
//! S-box, 8 full rounds, width `t = inputs + 1` for 1 to 4 inputs, state `[0, x1, …, xn]`, output `state[0]`.
//!
//! [`hash`] evaluates the optimised form (the paper's Appendix B, as Scala and circomlibjs's `poseidon_opt` lay it
//! out), and the crate holds only its tables. The base round constants feed nothing at run time, so, as in Scala, they
//! live in the tests: those derive them with the Grain LFSR, evaluate the rounds as defined, and check the forms agree.
//!
//! **Public data only**: the arithmetic is variable time.

use ark_ff::{AdditiveGroup, BigInt, BigInteger, Field, PrimeField};

pub mod merkle;

#[rustfmt::skip]
pub mod constants;

/// An element of the BN254 scalar field.
pub type Fr = ark_bn254::Fr;

/// The most inputs.
pub const MAX_INPUTS: usize = dilf4s_core::generated::limits::as_usize::POSEIDON_MAX_INPUTS;
/// Full rounds.
pub const FULL_ROUNDS: usize = 8;

/// Why hashing or a field element failed: generated from `shared/schemas/proofs-errors.cddl`, with the reference's and
/// the TypeScript SDK's kinds and fields.
pub use dilf4s_core::generated::proofs_errors::{PoseidonError, PoseidonErrorKind};

/// One width's optimised constants. `T` is the width, `RP` the partial rounds and `N = T - 1` the inputs (checked when
/// the width is evaluated).
///
/// With `h = 4`: `first` is added before the first S-box; `full_first` after the S-boxes of rounds `1..h`; `opened`
/// after round `h`'s, which multiplies by `pre_sparse`; `partial` is one scalar per partial round, added to element 0
/// after its S-box, whose sparse matrix has first row `sparse_w` and the rest of its first column `sparse_v` beside an
/// identity; and `full_last` after the S-boxes of the last full rounds but the final one, which adds none. Matrices
/// act as `new_i = Σ_j m_ij · s_j`. The full rounds multiply by `mds`, which is also the base form's matrix.
///
/// A width whose `N` is not `T - 1` does not compile once evaluated:
///
/// ```compile_fail,E0080
/// use dilf4s_proofs::poseidon::{Fr, Params};
/// let z = Fr::from(0u64);
/// let p: Params<3, 1, 1> = Params {
///     mds: [[z; 3]; 3],
///     first: [z; 3],
///     full_first: [[z; 3]; 3],
///     opened: [z; 3],
///     partial: [z; 1],
///     full_last: [[z; 3]; 3],
///     sparse_w: [[z; 3]; 1],
///     sparse_v: [[z; 1]; 1],
///     pre_sparse: [[z; 3]; 3],
/// };
/// p.permute([z; 3]);
/// ```
pub struct Params<const T: usize, const RP: usize, const N: usize> {
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
    /// The rest of the first column of each partial round's sparse matrix, below element 0.
    pub sparse_v: [[Fr; N]; RP],
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

fn partial_round<const T: usize, const N: usize>(mut s: [Fr; T], c: Fr, w: &[Fr; T], v: &[Fr; N]) -> [Fr; T] {
    let x0 = s.first().map_or(Fr::ZERO, |x| pow5(*x) + c);
    if let Some(first) = s.first_mut() {
        *first = x0;
    }
    let head = dot(w, &s);
    for (x, vk) in s.iter_mut().skip(1).zip(v) {
        *x += x0 * vk;
    }
    if let Some(first) = s.first_mut() {
        *first = head;
    }
    s
}

impl<const T: usize, const RP: usize, const N: usize> Params<T, RP, N> {
    /// The permutation's output element 0, in the optimised form.
    pub fn permute(&self, state: [Fr; T]) -> Fr {
        // `sparse_v` covers exactly the elements below element 0, or the width does not compile
        const { assert!(matches!(T.checked_sub(1), Some(n) if n == N), "N must be T - 1") };
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
        _ => Err(PoseidonError::InputCount { count: len64(inputs.len()), max: len64(MAX_INPUTS) }),
    }
}

/// A count as an error holds it (`usize` is at most 64 bits on every supported target).
fn len64(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

/// The bit length of the unsigned big-endian integer `bytes` encode: what a refusal names, never the value's digits.
fn be_bits(bytes: &[u8]) -> u64 {
    bytes.iter().position(|b| *b != 0).and_then(|i| bytes.get(i).map(|b| (i, *b))).map_or(0, |(i, b)| {
        let rest = len64(bytes.len().saturating_sub(i).saturating_sub(1));
        rest.saturating_mul(8).saturating_add(u64::from(8u32.saturating_sub(b.leading_zeros())))
    })
}

/// A field element from 32 big-endian bytes, which must be below `r`; a refusal names the integer's bit length.
pub fn fr_from_be_bytes(bytes: &[u8]) -> Result<Fr, PoseidonError> {
    let refused = || PoseidonError::NonCanonical { bits: be_bits(bytes) };
    let array: &[u8; 32] = bytes.try_into().map_err(|_| refused())?;
    let mut limbs = [0u64; 4];
    for (limb, chunk) in limbs.iter_mut().zip(array.rchunks_exact(8)) {
        let mut word = [0u8; 8];
        word.copy_from_slice(chunk);
        *limb = u64::from_be_bytes(word);
    }
    Fr::from_bigint(BigInt::new(limbs)).ok_or_else(refused)
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
/// A negative or wider integer is refused before it is converted.
pub fn fr_from_int(n: &num_bigint::BigInt) -> Result<Fr, PoseidonError> {
    if n.sign() == num_bigint::Sign::Minus || n.bits() > 256 {
        return Err(PoseidonError::NonCanonical { bits: n.bits() });
    }
    let (sign, magnitude) = n.to_bytes_be();
    if sign == num_bigint::Sign::Minus || magnitude.len() > 32 {
        return Err(PoseidonError::NonCanonical { bits: n.bits() });
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
