//! BIP32 for secp256k1 (`signing.md` section 2.3): private derivation (`CKDpriv`) and public derivation (`CKDpub`).
//!
//! An index whose `I[0:32] >= n`, or whose derived key is zero (or the identity), is `InvalidDerivation`; the caller
//! moves to the next index.

use k256::elliptic_curve::PrimeField as _;
use k256::{FieldBytes, ProjectivePoint, Scalar};
use zeroize::Zeroizing;

use super::path::ChildIndex;
use super::{ExtendedKey, hmac_sha512, split_i};
use crate::ecdsa::secp256k1::{SigningKey, VerifyingKey};
use crate::error::KeyError;

fn invalid(reason: &str) -> KeyError {
    KeyError::InvalidDerivation { reason: reason.into() }
}

/// `parse256(I_L)`, which must be below `n`.
fn tweak(il: &[u8; 32]) -> Result<Zeroizing<Scalar>, KeyError> {
    Option::<Scalar>::from(Scalar::from_repr(FieldBytes::from(*il)))
        .map(Zeroizing::new)
        .ok_or_else(|| invalid("I_L is not below n (skip this index, per BIP32)"))
}

/// The master key: `I = HMAC-SHA512("Bitcoin seed", seed)` for a seed of 16 to 64 bytes.
pub fn master(seed: &[u8]) -> Result<ExtendedKey<SigningKey>, KeyError> {
    if !(16..=64).contains(&seed.len()) {
        return Err(invalid("seed must be 16..64 bytes"));
    }
    let (il, chain_code) = split_i(&hmac_sha512(b"Bitcoin seed", &[seed]));
    let key = SigningKey::from_bytes(&*il).map_err(|_| invalid("master key is not a valid scalar"))?;
    Ok(ExtendedKey { key, chain_code })
}

/// `CKDpriv`: hardened data `00 || k || BE32(i)`, normal data `serP(K) || BE32(i)`; `k' = I_L + k mod n`.
pub fn child(parent: &ExtendedKey<SigningKey>, index: ChildIndex) -> Result<ExtendedKey<SigningKey>, KeyError> {
    let k = parent.key.to_bytes();
    let i = if index.is_hardened() {
        hmac_sha512(&parent.chain_code, &[&[0], &*k, &index.be32()])
    } else {
        hmac_sha512(&parent.chain_code, &[parent.key.verifying_key().as_bytes(), &index.be32()])
    };
    let (il, chain_code) = split_i(&i);
    let sum = Zeroizing::new(*tweak(&il)? + **parent.key.scalar());
    let bytes: Zeroizing<[u8; 32]> = Zeroizing::new(sum.to_repr().into());
    let key =
        SigningKey::from_bytes(&*bytes).map_err(|_| invalid("derived key is zero (skip this index, per BIP32)"))?;
    Ok(ExtendedKey { key, chain_code })
}

/// The key at `path` from `seed`.
pub fn derive(seed: &[u8], path: &[ChildIndex]) -> Result<ExtendedKey<SigningKey>, KeyError> {
    path.iter().try_fold(master(seed)?, |key, i| child(&key, *i))
}

/// `CKDpub` (soft indices only): `I = HMAC-SHA512(cc, serP(K) || BE32(i))`, `K' = K + [I_L]G`. Returns the child key
/// and chain code.
pub fn public_child(
    key: &VerifyingKey,
    chain_code: &[u8; 32],
    index: ChildIndex,
) -> Result<(VerifyingKey, [u8; 32]), KeyError> {
    if index.is_hardened() {
        return Err(invalid("hardened children need the private key"));
    }
    let i = hmac_sha512(chain_code, &[key.as_bytes(), &index.be32()]);
    let (il, child_code) = split_i(&i);
    let point = ProjectivePoint::from(*key.as_affine()) + ProjectivePoint::GENERATOR * *tweak(&il)?;
    let child = VerifyingKey::from_affine(point.to_affine()).map_err(|_| invalid("derived key is the identity"))?;
    Ok((child, child_code))
}
