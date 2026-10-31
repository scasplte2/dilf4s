//! BN254 (`alt_bn128`) group operations on exact-width bytes, byte-compatible with the EVM precompiles (EIP-196/197,
//! docs/specs/zk.md §1), and Groth16 verification ([`groth16`], §2).
//!
//! - **Field elements** are 32 bytes big-endian and must be below `p`; a value `>= p` is malformed, never reduced.
//! - **G1** is `x || y`, 64 bytes; `(0, 0)` is the identity; any other point must be on `y^2 = x^3 + 3`.
//! - **G2** is `x.imag || x.real || y.imag || y.real`, 128 bytes; all zeros is the identity; any other point must be on
//!   the twist and in the order-`r` subgroup.
//!
//! A wrong length or a non-canonical coordinate is [`ZkError::Malformed`]; a point off the curve or twist, or outside
//! the subgroup, is [`ZkError::InvalidPoint`]. **Public data only**: the arithmetic is variable time.

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

use bn::{AffineG1, AffineG2, Fq, Fq2, Fr, G1, G2, Group, Gt};

pub mod groth16;

/// A G1 point's width.
pub const G1_BYTES: usize = 64;
/// A G2 point's width.
pub const G2_BYTES: usize = 128;
/// A field element's or scalar's width.
pub const WORD_BYTES: usize = 32;

/// Why a BN254 operation refused its input.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ZkError {
    /// A wrong length, or a coordinate not below `p`.
    Malformed {
        /// What was wrong.
        reason: String,
    },
    /// A point off the curve or twist, or a G2 point outside the order-`r` subgroup.
    InvalidPoint {
        /// What was wrong.
        reason: String,
    },
}

impl fmt::Display for ZkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Malformed { reason } => write!(f, "malformed: {reason}"),
            Self::InvalidPoint { reason } => write!(f, "invalid point: {reason}"),
        }
    }
}

fn malformed(reason: &str) -> ZkError {
    ZkError::Malformed { reason: reason.into() }
}

fn invalid(reason: &str) -> ZkError {
    ZkError::InvalidPoint { reason: reason.into() }
}

fn exact<'a, const N: usize>(bytes: &'a [u8], role: &str) -> Result<&'a [u8; N], ZkError> {
    bytes.try_into().map_err(|_| malformed(&alloc::format!("{role} must be {N} bytes, got {}", bytes.len())))
}

/// The four 32-byte words of a 128-byte G2 encoding, or the two of a 64-byte G1 encoding.
fn words<const N: usize, const W: usize>(bytes: &[u8; N]) -> [&[u8; WORD_BYTES]; W] {
    let mut out = [&[0u8; WORD_BYTES]; W];
    for (o, c) in out.iter_mut().zip(bytes.chunks_exact(WORD_BYTES)) {
        if let Ok(w) = <&[u8; WORD_BYTES]>::try_from(c) {
            *o = w;
        }
    }
    out
}

fn field(word: &[u8; WORD_BYTES], role: &str) -> Result<Fq, ZkError> {
    Fq::from_slice(word).map_err(|_| malformed(&alloc::format!("{role} is not a canonical field element")))
}

/// A field element's canonical 32 bytes, from its two 128-bit limbs (least significant first).
fn to_word(x: Fq) -> [u8; WORD_BYTES] {
    let bn::arith::U256([low, high]) = x.into_u256();
    let mut out = [0u8; WORD_BYTES];
    for (o, b) in out.iter_mut().zip(high.to_be_bytes().into_iter().chain(low.to_be_bytes())) {
        *o = b;
    }
    out
}

/// A G1 point from its 64 bytes.
pub fn decode_g1(bytes: &[u8]) -> Result<G1, ZkError> {
    let [x, y] = words::<G1_BYTES, 2>(exact::<G1_BYTES>(bytes, "a G1 point")?);
    let (x, y) = (field(x, "G1 x")?, field(y, "G1 y")?);
    if x.is_zero() && y.is_zero() {
        Ok(G1::zero())
    } else {
        AffineG1::new(x, y).map(G1::from).map_err(|_| invalid("G1 point is not on the curve"))
    }
}

/// A G2 point from its 128 bytes (EIP-197 order).
pub fn decode_g2(bytes: &[u8]) -> Result<G2, ZkError> {
    let [xi, xr, yi, yr] = words::<G2_BYTES, 4>(exact::<G2_BYTES>(bytes, "a G2 point")?);
    let (xi, xr, yi, yr) =
        (field(xi, "G2 x.imag")?, field(xr, "G2 x.real")?, field(yi, "G2 y.imag")?, field(yr, "G2 y.real")?);
    let (x, y) = (Fq2::new(xr, xi), Fq2::new(yr, yi));
    if x.is_zero() && y.is_zero() {
        Ok(G2::zero())
    } else {
        AffineG2::new(x, y).map(G2::from).map_err(|e| match e {
            bn::GroupError::NotOnCurve => invalid("G2 point is not on the twist"),
            bn::GroupError::NotInSubgroup => invalid("G2 point is not in the order-r subgroup"),
        })
    }
}

/// A G1 point's 64 bytes; the identity is all zeros.
pub fn encode_g1(p: G1) -> [u8; G1_BYTES] {
    let mut out = [0u8; G1_BYTES];
    if let Some(a) = AffineG1::from_jacobian(p) {
        for (o, b) in out.iter_mut().zip(to_word(a.x()).into_iter().chain(to_word(a.y()))) {
            *o = b;
        }
    }
    out
}

/// `a + b`.
pub fn add(a: &[u8], b: &[u8]) -> Result<[u8; G1_BYTES], ZkError> {
    let (p, q) = (decode_g1(a)?, decode_g1(b)?);
    Ok(encode_g1(p + q))
}

/// `k * p`, for any 256-bit `k` (G1 has prime order `r`, so this is `(k mod r) * p`).
pub fn mul(p: &[u8], k: &[u8]) -> Result<[u8; G1_BYTES], ZkError> {
    let point = decode_g1(p)?;
    let scalar = bn::arith::U256::from_slice(exact::<WORD_BYTES>(k, "a scalar")?)
        .map_err(|_| malformed("a scalar must be 32 bytes"))?;
    Ok(encode_g1(point * Fr::new_mul_factor(scalar)))
}

/// Whether the product of `e(g1, g2)` over the pairs is one; no pairs is true. Every point is decoded and checked
/// before any pairing is computed.
pub fn pairing(pairs: &[(&[u8], &[u8])]) -> Result<bool, ZkError> {
    let points = pairs.iter().map(|(a, b)| Ok((decode_g1(a)?, decode_g2(b)?))).collect::<Result<Vec<_>, ZkError>>()?;
    // e(O, Q) = e(P, O) = 1, so pairs with an identity drop out of the product
    let live: Vec<(G1, G2)> = points.into_iter().filter(|(p, q)| !p.is_zero() && !q.is_zero()).collect();
    Ok(live.is_empty() || bn::pairing_batch(&live) == Gt::one())
}
