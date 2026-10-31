//! Ed25519-BIP32 signing keys (Khovratovich-Law, as used by Icarus / CIP-0003; `signing.md` section 1.2).
//!
//! A signing key is the 64-byte extended scalar `kL || kR`. Signatures are ordinary Ed25519 signatures and verify
//! under [`crate::ed25519::verify`] (ZIP-215).

use core::fmt;

use curve25519_dalek::edwards::EdwardsPoint;
use curve25519_dalek::scalar::Scalar;
use subtle::ConstantTimeEq;
use zeroize::{Zeroize, ZeroizeOnDrop};

use crate::ed25519::{Signature, VerifyingKey, sign_expanded};
use crate::error::SigningError;
use crate::util::{exact, halves};

/// Signing key length (`kL || kR`).
pub const SIGNING_KEY_LENGTH: usize = 64;

/// An extended secret key `kL || kR`, with `kL` a multiple of 8 and bit 255 clear.
#[derive(Clone, Zeroize, ZeroizeOnDrop)]
pub struct SigningKey {
    kl: [u8; 32],
    kr: [u8; 32],
}

/// Whether `kL` is a multiple of 8 below 2^255 (clamped, and kept so by derivation).
pub(crate) fn valid_kl(kl: &[u8; 32]) -> bool {
    kl.first().is_some_and(|b| b.trailing_zeros() >= 3) && kl.last().is_some_and(|b| b & 0x80 == 0)
}

impl SigningKey {
    /// Decodes `kL || kR`: a wrong length is `InvalidEncoding`, an unclamped `kL` is `InvalidKey`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
        let bytes: [u8; 64] = exact("Ed25519-BIP32 signing key", bytes)?;
        let (kl, kr) = halves(&bytes);
        if valid_kl(&kl) {
            Ok(Self { kl, kr })
        } else {
            Err(SigningError::InvalidKey { reason: "kL must be a multiple of 8 with bit 255 clear".into() })
        }
    }

    /// A key with any `kL`, clamped or not. Only for interoperating with legacy test data whose keys are
    /// unclamped; such keys are not BIP32-Ed25519 keys and derivation from them is meaningless.
    #[doc(hidden)]
    pub const fn from_parts_unchecked(kl: [u8; 32], kr: [u8; 32]) -> Self {
        Self { kl, kr }
    }

    /// `kL || kR`.
    pub fn to_bytes(&self) -> zeroize::Zeroizing<[u8; 64]> {
        zeroize::Zeroizing::new(crate::util::join(&self.kl, &self.kr))
    }

    pub(crate) const fn kl(&self) -> &[u8; 32] {
        &self.kl
    }

    pub(crate) const fn kr(&self) -> &[u8; 32] {
        &self.kr
    }

    fn scalar(&self) -> Scalar {
        Scalar::from_bytes_mod_order(self.kl)
    }

    /// `A = [kL mod L]B`.
    pub fn verifying_key(&self) -> VerifyingKey {
        VerifyingKey::from_point(EdwardsPoint::mul_base(&self.scalar()))
    }

    /// `r = SHA-512(kR || M) mod L`, `R = [r]B`, `S = (r + SHA-512(R || A || M) kL) mod L`.
    pub fn sign(&self, message: &[u8]) -> Signature {
        let s = self.scalar();
        let a = EdwardsPoint::mul_base(&s).compress().to_bytes();
        Signature::from_array(sign_expanded(&s, &self.kr, &a, message))
    }
}

impl fmt::Debug for SigningKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("ed25519_bip32::SigningKey(<redacted>)")
    }
}

impl PartialEq for SigningKey {
    fn eq(&self, other: &Self) -> bool {
        (self.kl.ct_eq(&other.kl) & self.kr.ct_eq(&other.kr)).into()
    }
}

impl Eq for SigningKey {}
