//! SLIP-10 for Ed25519 (`signing.md` section 2.2): hardened derivation only; each key is an RFC 8032 seed.

use super::path::ChildIndex;
use super::{ExtendedKey, hmac_sha512, split_i};
use crate::ed25519::SigningKey;
use crate::error::KeyError;

fn extended(i: &[u8; 64]) -> Result<ExtendedKey<SigningKey>, KeyError> {
    let (key, chain_code) = split_i(i);
    let key = SigningKey::from_bytes(&*key).map_err(|e| KeyError::InvalidDerivation { reason: e.to_string() })?;
    Ok(ExtendedKey { key, chain_code })
}

/// The master key: `I = HMAC-SHA512("ed25519 seed", seed)` for a seed of 16 to 64 bytes.
pub fn master(seed: &[u8]) -> Result<ExtendedKey<SigningKey>, KeyError> {
    if !(16..=64).contains(&seed.len()) {
        return Err(KeyError::InvalidDerivation { reason: "seed must be 16..64 bytes".into() });
    }
    extended(&hmac_sha512(b"ed25519 seed", &[seed]))
}

/// A hardened child: `I = HMAC-SHA512(cc, 00 || k || BE32(i))`. Soft indices are `InvalidDerivation`.
pub fn child(parent: &ExtendedKey<SigningKey>, index: ChildIndex) -> Result<ExtendedKey<SigningKey>, KeyError> {
    if !index.is_hardened() {
        return Err(KeyError::InvalidDerivation { reason: "SLIP-10 Ed25519 supports hardened indices only".into() });
    }
    extended(&hmac_sha512(&parent.chain_code, &[&[0], parent.key.as_bytes(), &index.be32()]))
}

/// The key at `path` from `seed`.
pub fn derive(seed: &[u8], path: &[ChildIndex]) -> Result<ExtendedKey<SigningKey>, KeyError> {
    path.iter().try_fold(master(seed)?, |key, i| child(&key, *i))
}

use alloc::string::ToString as _;
