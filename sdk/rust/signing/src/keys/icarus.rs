//! BIP32-Ed25519 (Khovratovich-Law), as used by Cardano Icarus and CIP-0003 (`signing.md` section 2.4).
//!
//! Secret-key arithmetic is fixed-width little-endian byte addition with no data-dependent branches.

use curve25519_dalek::edwards::{CompressedEdwardsY, EdwardsPoint};
use curve25519_dalek::scalar::Scalar;
use zeroize::Zeroizing;

use super::path::ChildIndex;
use super::{ExtendedKey, hmac_sha512};
use crate::ed25519::VerifyingKey;
use crate::ed25519_bip32::SigningKey;
use crate::error::KeyError;
use crate::util::halves;

fn invalid(reason: &str) -> KeyError {
    KeyError::InvalidDerivation { reason: reason.into() }
}

/// A root key from 96 bytes `kL || kR || cc`, clamped per CIP-0003 (`b[0] &= f8`, `b[31] &= 1f`, `b[31] |= 40`).
pub fn root(secret: &[u8]) -> Result<ExtendedKey<SigningKey>, KeyError> {
    let mut b: Zeroizing<[u8; 96]> =
        Zeroizing::new(secret.try_into().map_err(|_| invalid("root secret must be 96 bytes"))?);
    if let Some(first) = b.first_mut() {
        *first &= 0xf8;
    }
    if let Some(b31) = b.get_mut(31) {
        *b31 = (*b31 & 0x1f) | 0x40;
    }
    let (key, cc) = b.split_at(64);
    let key = SigningKey::from_bytes(key).map_err(|_| invalid("root kL is not clamped"))?;
    let chain_code: [u8; 32] = cc.try_into().map_err(|_| invalid("root secret must be 96 bytes"))?;
    Ok(ExtendedKey { key, chain_code })
}

/// The Icarus master key: `PBKDF2-HMAC-SHA512(passphrase, entropy, 4096, 96)`, clamped. The passphrase is its UTF-8
/// bytes (not normalised).
pub fn from_entropy(entropy: &[u8], passphrase: &str) -> Result<ExtendedKey<SigningKey>, KeyError> {
    let mut out = Zeroizing::new([0u8; 96]);
    pbkdf2::pbkdf2_hmac::<sha2::Sha512>(passphrase.as_bytes(), entropy, 4096, &mut *out);
    root(&*out)
}

/// `(a + b) mod 2^256`, little-endian, byte by byte with a carry.
fn add256(a: &[u8; 32], b: &[u8; 32]) -> [u8; 32] {
    let mut out = [0u8; 32];
    let mut carry = 0u16;
    for ((o, x), y) in out.iter_mut().zip(a).zip(b) {
        let [lo, hi] = u16::from(*x).wrapping_add(u16::from(*y)).wrapping_add(carry).to_le_bytes();
        *o = lo;
        carry = u16::from(hi);
    }
    out
}

/// `8 * LE(z[0:28])` as 32 little-endian bytes (it fits: 227 bits).
fn times8(z: &[u8; 32]) -> [u8; 32] {
    let mut out = [0u8; 32];
    let mut carry = 0u16;
    for (i, o) in out.iter_mut().enumerate() {
        let byte = if i < 28 { z.get(i).copied().unwrap_or(0) } else { 0 };
        let [lo, hi] = (u16::from(byte).wrapping_shl(3) | carry).to_le_bytes();
        *o = lo;
        carry = u16::from(hi);
    }
    out
}

/// A child key. Index `i` is `LE32`, and `A` the parent verifying key:
///
/// - hardened: `Z = HMAC-SHA512(cc, 00 || kL || kR || i)`, chain code from `01 || kL || kR || i`;
/// - soft: `Z = HMAC-SHA512(cc, 02 || A || i)`, chain code from `03 || A || i`;
///
/// then `kL' = 8 LE(Z[0:28]) + LE(kL)` and `kR' = (LE(Z[32:64]) + LE(kR)) mod 2^256`.
pub fn child(parent: &ExtendedKey<SigningKey>, index: ChildIndex) -> Result<ExtendedKey<SigningKey>, KeyError> {
    let (kl, kr) = (parent.key.kl(), parent.key.kr());
    let idx = index.le32();
    let a = parent.key.verifying_key();
    let (z, cc) = if index.is_hardened() {
        (
            hmac_sha512(&parent.chain_code, &[&[0x00], kl, kr, &idx]),
            hmac_sha512(&parent.chain_code, &[&[0x01], kl, kr, &idx]),
        )
    } else {
        (
            hmac_sha512(&parent.chain_code, &[&[0x02], a.as_bytes(), &idx]),
            hmac_sha512(&parent.chain_code, &[&[0x03], a.as_bytes(), &idx]),
        )
    };
    let (zl, zr) = halves(&z);
    let (zl, zr) = (Zeroizing::new(zl), Zeroizing::new(zr));
    let child_kl = Zeroizing::new(add256(&times8(&zl), kl));
    let child_kr = Zeroizing::new(add256(&zr, kr));
    let key = Zeroizing::new(crate::util::join::<32, 32, 64>(&child_kl, &child_kr));
    let key = SigningKey::from_bytes(&*key).map_err(|_| invalid("derived kL is not a valid BIP32-Ed25519 scalar"))?;
    let (_, chain_code) = halves(&cc);
    Ok(ExtendedKey { key, chain_code })
}

/// The key at `path` from `root`.
pub fn derive(root: &ExtendedKey<SigningKey>, path: &[ChildIndex]) -> Result<ExtendedKey<SigningKey>, KeyError> {
    path.iter().try_fold(root.clone(), |key, i| child(&key, *i))
}

/// Public (soft-only) derivation: `A' = A + [8 LE(Z[0:28])]B` from the verifying key and chain code. `A` must be the
/// canonical encoding: the HMAC input is its bytes, so a non-canonical twin would diverge from the private path.
pub fn public_child(
    key: &VerifyingKey,
    chain_code: &[u8; 32],
    index: ChildIndex,
) -> Result<(VerifyingKey, [u8; 32]), KeyError> {
    if index.is_hardened() {
        return Err(invalid("hardened children need the private key"));
    }
    let point = CompressedEdwardsY(*key.as_bytes())
        .decompress()
        .filter(|p| p.compress().to_bytes() == *key.as_bytes())
        .ok_or_else(|| invalid("verifying key is not a canonical point encoding"))?;
    let idx = index.le32();
    let z = hmac_sha512(chain_code, &[&[0x02], key.as_bytes(), &idx]);
    let (zl, _) = halves(&z);
    let child = EdwardsPoint::mul_base(&Scalar::from_bytes_mod_order(times8(&zl))) + point;
    let (_, child_code) = halves(&hmac_sha512(chain_code, &[&[0x03], key.as_bytes(), &idx]));
    Ok((VerifyingKey::from_point(child), child_code))
}
