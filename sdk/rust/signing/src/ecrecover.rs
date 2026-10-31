//! Ethereum's signer recovery (`ecrecover`, yellow paper Appendix F) over secp256k1: the address whose key made an
//! ECDSA signature over a 32-byte digest (`jlvm.md` §5.6). Verification only: every input is public.
//!
//! Stricter than the EVM precompile, as `OpenZeppelin`'s `ECDSA.recover` is: a high-`s` signature (EIP-2) recovers
//! nothing, so a signature cannot be re-encoded into a second valid one. Written on k256's group arithmetic, so the
//! rules here are this crate's own and do not depend on a recovery routine's checks.

use k256::elliptic_curve::Group as _;
use k256::elliptic_curve::PrimeField as _;
use k256::elliptic_curve::ops::Reduce;
use k256::elliptic_curve::point::DecompressPoint as _;
use k256::elliptic_curve::scalar::IsHigh as _;
use k256::elliptic_curve::sec1::ToEncodedPoint as _;
use k256::elliptic_curve::subtle::Choice;
use k256::{AffinePoint, FieldBytes, ProjectivePoint, Scalar, U256};

use dilf4s_core::hash::{Hasher, Keccak256};

use crate::error::SigningError;
use crate::util::exact;

/// A digest's width.
pub const DIGEST_BYTES: usize = 32;
/// A signature's width: `r || s || v`.
pub const SIGNATURE_BYTES: usize = 65;
/// An address's width.
pub const ADDRESS_BYTES: usize = 20;

/// The signer's address of `signature` (`r || s || v`, `v` in {27, 28} or {0, 1}) over `digest`.
///
/// Wrong sizes or another `v` are malformed (`Err`). A well-formed signature that recovers no key is `Ok(None)`: `r`
/// or `s` zero or not below the order, a high `s`, an `r` that is no point's x-coordinate, or a recovered identity.
pub fn recover(digest: &[u8], signature: &[u8]) -> Result<Option<[u8; ADDRESS_BYTES]>, SigningError> {
    let digest: [u8; DIGEST_BYTES] = exact("ecrecover digest", digest)?;
    let signature: [u8; SIGNATURE_BYTES] = exact("ecrecover signature", signature)?;
    let (rs, v) = signature.split_at(64);
    let parity = match v {
        [0 | 27] => 0u8,
        [1 | 28] => 1u8,
        _ => {
            return Err(SigningError::InvalidEncoding {
                what: "ecrecover signature",
                reason: alloc::format!("v is 27 or 28 (or 0 or 1), got {v:?}"),
            });
        }
    };
    let (r, s) = rs.split_at(32);
    Ok(key(&digest, r, s, parity).map(|q| address(&q)))
}

/// `Q = r^-1 (s R - e G)`, where `R` has x-coordinate `r` and the given y parity; `None` where no key is recovered.
fn key(digest: &[u8; DIGEST_BYTES], r: &[u8], s: &[u8], parity: u8) -> Option<AffinePoint> {
    let r_bytes = FieldBytes::clone_from_slice(r);
    let r = Option::<Scalar>::from(Scalar::from_repr(r_bytes)).filter(|x| !bool::from(x.is_zero()))?;
    let s = Option::<Scalar>::from(Scalar::from_repr(FieldBytes::clone_from_slice(s)))
        .filter(|x| !bool::from(x.is_zero()) && !bool::from(x.is_high()))?;
    // r < n < p, so its bytes are a field element; it is an x-coordinate when x^3 + 7 is a square
    let big_r = Option::<AffinePoint>::from(AffinePoint::decompress(&r_bytes, Choice::from(parity)))?;
    let e = <Scalar as Reduce<U256>>::reduce_bytes(&FieldBytes::from(*digest));
    let r_inv = Option::<Scalar>::from(r.invert())?;
    let q = (ProjectivePoint::from(big_r) * (s * r_inv)) - (ProjectivePoint::GENERATOR * (e * r_inv));
    (!bool::from(q.is_identity())).then(|| q.to_affine())
}

/// An Ethereum address: the last 20 bytes of Keccak-256 over the uncompressed key's `x || y`.
fn address(key: &AffinePoint) -> [u8; ADDRESS_BYTES] {
    let encoded = key.to_encoded_point(false);
    let digest = Keccak256::hash_parts(&[encoded.as_bytes().get(1..).unwrap_or_default()]);
    let mut out = [0u8; ADDRESS_BYTES];
    for (o, b) in out.iter_mut().zip(digest.iter().skip(DIGEST_BYTES - ADDRESS_BYTES)) {
        *o = *b;
    }
    out
}

/// The address of a SEC1-encoded secp256k1 public key (33 or 65 bytes).
pub fn address_of_key(key: &[u8]) -> Result<[u8; ADDRESS_BYTES], SigningError> {
    let point = k256::PublicKey::from_sec1_bytes(key)
        .map_err(|_| SigningError::InvalidKey { reason: "not a secp256k1 public key".into() })?;
    Ok(address(point.as_affine()))
}
