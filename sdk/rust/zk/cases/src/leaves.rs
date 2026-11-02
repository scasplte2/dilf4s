//! Issue #33: a sigma leaf commitment `z·B − e·P` as two scalar multiplications (what `sigma.rs` `push_leaf` does)
//! and as one k256 `LinearCombination`, so the guest's cycles and the native time of each can be measured apart.

use alloc::string::String;
use alloc::vec::Vec;

use k256::elliptic_curve::Group;
use k256::elliptic_curve::PrimeField as _;
use k256::elliptic_curve::ops::{LinearCombination as _, Reduce};
use k256::elliptic_curve::sec1::ToEncodedPoint as _;
use k256::{FieldBytes, ProjectivePoint, PublicKey, Scalar, U256};
use serde::{Deserialize, Serialize};

/// The width of a sigma challenge.
const CHALLENGE_LENGTH: usize = 24;

/// One leaf of a sigma proof: `z·base − e·point`.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Leaf {
    /// The base, compressed SEC1; none for the generator (a dlog leaf).
    pub base: Option<Vec<u8>>,
    /// The point the challenge multiplies (`pk`, `u` or `v`), compressed SEC1.
    pub point: Vec<u8>,
    /// The leaf's 24-byte challenge.
    pub challenge: Vec<u8>,
    /// The leaf's 32-byte response `z`.
    pub response: Vec<u8>,
}

/// A leaf decoded, so the measurement holds only the multiplication.
#[derive(Debug, Clone, Copy)]
pub struct Prepared {
    base: ProjectivePoint,
    point: ProjectivePoint,
    z: Scalar,
    c: Scalar,
}

/// How to compute the commitments.
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Form {
    /// `z·B − e·P` as two scalar multiplications.
    TwoMuls,
    /// `z·B − e·P` as one linear combination.
    Lincomb,
}

/// Each leaf's commitment in `form`, compressed SEC1; with no form, each leaf's `P` encoded the same way. The guest's
/// cost of a form is its run's cycles less the no-form run's: decoding, encoding and I/O cancel out.
pub fn commit(form: Option<Form>, leaves: &[Leaf]) -> Result<Vec<Vec<u8>>, String> {
    let prepared = prepare(leaves)?;
    Ok(match form {
        None => encode(&prepared.iter().map(|l| l.point).collect::<Vec<_>>()),
        Some(Form::TwoMuls) => encode(&two_muls(&prepared)),
        Some(Form::Lincomb) => encode(&lincomb(&prepared)),
    })
}

fn point(bytes: &[u8]) -> Result<ProjectivePoint, String> {
    PublicKey::from_sec1_bytes(bytes).map(|k| k.to_projective()).map_err(|_| String::from("not a point"))
}

/// `sigma.rs` `challenge_scalar`: the challenge right-aligned in 32 bytes, reduced.
fn challenge(e: &[u8]) -> Result<Scalar, String> {
    if e.len() != CHALLENGE_LENGTH {
        return Err(String::from("challenge length"));
    }
    let mut wide = [0u8; 32];
    for (w, b) in wide.iter_mut().skip(32usize.saturating_sub(CHALLENGE_LENGTH)).zip(e) {
        *w = *b;
    }
    Ok(<Scalar as Reduce<U256>>::reduce_bytes(&FieldBytes::from(wide)))
}

fn response(z: &[u8]) -> Result<Scalar, String> {
    let bytes: [u8; 32] = z.try_into().map_err(|_| String::from("response length"))?;
    Option::from(Scalar::from_repr(FieldBytes::from(bytes))).ok_or_else(|| String::from("response out of range"))
}

/// Decodes each leaf.
pub fn prepare(leaves: &[Leaf]) -> Result<Vec<Prepared>, String> {
    leaves
        .iter()
        .map(|l| {
            Ok(Prepared {
                base: l.base.as_deref().map_or(Ok(<ProjectivePoint as Group>::generator()), point)?,
                point: point(&l.point)?,
                z: response(&l.response)?,
                c: challenge(&l.challenge)?,
            })
        })
        .collect()
}

/// `z·B − e·P` as `push_leaf` computes it today.
pub fn two_muls(leaves: &[Prepared]) -> Vec<ProjectivePoint> {
    leaves.iter().map(|l| l.base * l.z - l.point * l.c).collect()
}

/// `z·B − e·P` as one linear combination.
pub fn lincomb(leaves: &[Prepared]) -> Vec<ProjectivePoint> {
    leaves.iter().map(|l| ProjectivePoint::lincomb(&l.base, &l.z, &l.point, &-l.c)).collect()
}

/// Compressed SEC1, as sigma's transcript holds a commitment.
pub fn encode(points: &[ProjectivePoint]) -> Vec<Vec<u8>> {
    points.iter().map(|p| p.to_affine().to_encoded_point(true).as_bytes().to_vec()).collect()
}
