//! Schnorr signatures over ristretto255 with the FROST(ristretto255, SHA-512) ciphersuite (RFC 9591;
//! `signing.md` section 1.5): the single-signer form of FROST, so a threshold group signature verifies under the same
//! rule.

use curve25519_dalek::ristretto::{CompressedRistretto, RistrettoPoint};
use curve25519_dalek::scalar::Scalar;
use curve25519_dalek::traits::IsIdentity as _;

use crate::error::SigningError;
use crate::util::{exact, halves, sha512};

/// Verifying key length.
pub const VERIFYING_KEY_LENGTH: usize = 32;
/// Signature length (`R || z`).
pub const SIGNATURE_LENGTH: usize = 64;
/// Secret key length (little-endian canonical scalar).
pub const SIGNING_KEY_LENGTH: usize = 32;
/// Nonce randomness length.
pub const RANDOMNESS_LENGTH: usize = 32;

const CONTEXT: &[u8] = b"FROST-RISTRETTO255-SHA512-v1";

/// `H2` (`"chal"`) or `H3` (`"nonce"`): `SHA-512(ctx || label || parts...) mod L`.
fn hash_to_scalar(label: &[u8], parts: &[&[u8]]) -> Scalar {
    let mut all: alloc::vec::Vec<&[u8]> = alloc::vec![CONTEXT, label];
    all.extend_from_slice(parts);
    Scalar::from_bytes_mod_order_wide(&sha512(&all))
}

/// RFC 9591 `DeserializeElement`: a canonical encoding of a non-identity element.
fn element(bytes: &[u8; 32]) -> Option<RistrettoPoint> {
    CompressedRistretto(*bytes).decompress().filter(|p| !p.is_identity())
}

/// A canonical, non-identity ristretto255 element.
#[derive(Clone, Copy, Debug)]
pub struct VerifyingKey {
    bytes: [u8; 32],
    point: RistrettoPoint,
}

impl VerifyingKey {
    /// Decodes 32 bytes: a wrong length is `InvalidEncoding`; a non-canonical encoding or the identity `InvalidKey`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
        let bytes: [u8; 32] = exact("ristretto255 verifying key", bytes)?;
        let point = element(&bytes).ok_or_else(|| SigningError::InvalidKey {
            reason: "ristretto255 verifying key is not a canonical non-identity element".into(),
        })?;
        Ok(Self { bytes, point })
    }

    /// The encoding.
    pub const fn as_bytes(&self) -> &[u8; 32] {
        &self.bytes
    }
}

impl PartialEq for VerifyingKey {
    fn eq(&self, other: &Self) -> bool {
        self.bytes == other.bytes
    }
}

impl Eq for VerifyingKey {}

/// A signature `R || z` with `R` a canonical non-identity element and `z` a canonical scalar.
#[derive(Clone, Copy, Debug)]
pub struct Signature {
    bytes: [u8; 64],
    r: RistrettoPoint,
    z: Scalar,
}

impl Signature {
    /// Decodes 64 bytes; anything but `R (canonical, non-identity) || z (canonical)` is `InvalidEncoding`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
        const WHAT: &str = "ristretto255 signature";
        let bytes: [u8; 64] = exact(WHAT, bytes)?;
        let (r_bytes, z_bytes) = halves(&bytes);
        let r = element(&r_bytes);
        let z: Option<Scalar> = Scalar::from_canonical_bytes(z_bytes).into();
        match (r, z) {
            (Some(r), Some(z)) => Ok(Self { bytes, r, z }),
            _ => Err(SigningError::InvalidEncoding {
                what: WHAT,
                reason: "expected R (non-identity element) || z (canonical scalar)".into(),
            }),
        }
    }

    /// The encoding.
    pub const fn as_bytes(&self) -> &[u8; 64] {
        &self.bytes
    }
}

impl PartialEq for Signature {
    fn eq(&self, other: &Self) -> bool {
        self.bytes == other.bytes
    }
}

impl Eq for Signature {}

/// `[z]G == R + [c]PK` with `c = H2(R || PK || M)`.
pub fn verify(key: &VerifyingKey, message: &[u8], signature: &Signature) -> bool {
    let (r_bytes, _) = halves(&signature.bytes);
    let c = hash_to_scalar(b"chal", &[&r_bytes, &key.bytes, message]);
    RistrettoPoint::mul_base(&signature.z) == signature.r + key.point * c
}

/// Verification over raw encodings: the key is decoded first, then the signature.
pub fn verify_bytes(key: &[u8], message: &[u8], signature: &[u8]) -> Result<bool, SigningError> {
    let key = VerifyingKey::from_bytes(key)?;
    let signature = Signature::from_bytes(signature)?;
    Ok(verify(&key, message, &signature))
}

#[cfg(feature = "sign")]
pub use signing::{SigningKey, from_seed};

#[cfg(feature = "sign")]
mod signing {
    use core::fmt;

    use curve25519_dalek::ristretto::RistrettoPoint;
    use curve25519_dalek::scalar::Scalar;
    use subtle::ConstantTimeEq as _;
    use zeroize::{Zeroize, ZeroizeOnDrop, Zeroizing};

    use super::{Signature, VerifyingKey, hash_to_scalar};
    use crate::error::SigningError;
    use crate::util::{exact, join, sha512};

    /// A canonical, non-zero scalar.
    #[derive(Clone, Zeroize, ZeroizeOnDrop)]
    pub struct SigningKey {
        scalar: Scalar,
    }

    impl SigningKey {
        /// Decodes 32 little-endian bytes: a wrong length is `InvalidEncoding`, a non-canonical or zero scalar
        /// `InvalidKey`.
        pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
            let bytes = Zeroizing::new(exact::<32>("ristretto255 signing key", bytes)?);
            let scalar: Option<Scalar> = Scalar::from_canonical_bytes(*bytes).into();
            scalar.filter(|s| *s != Scalar::ZERO).map(|scalar| Self { scalar }).ok_or_else(|| {
                SigningError::InvalidKey {
                    reason: "ristretto255 secret key must be a canonical non-zero scalar".into(),
                }
            })
        }

        /// The little-endian scalar.
        pub fn to_bytes(&self) -> Zeroizing<[u8; 32]> {
            Zeroizing::new(self.scalar.to_bytes())
        }

        /// `PK = [sk]G`.
        pub fn verifying_key(&self) -> VerifyingKey {
            let point = RistrettoPoint::mul_base(&self.scalar);
            VerifyingKey { bytes: point.compress().to_bytes(), point }
        }

        /// Signing with explicit nonce randomness `a` (32 bytes, fresh per signature in production):
        /// `k = H3(a || sk)`, `R = [k]G`, `c = H2(R || PK || M)`, `z = k + c sk`.
        pub fn sign(&self, message: &[u8], randomness: &[u8; 32]) -> Signature {
            let sk = self.to_bytes();
            let k = Zeroizing::new(hash_to_scalar(b"nonce", &[randomness, &*sk]));
            let r = RistrettoPoint::mul_base(&k);
            let r_bytes = r.compress().to_bytes();
            let pk = self.verifying_key();
            let c = hash_to_scalar(b"chal", &[&r_bytes, &pk.bytes, message]);
            let z = *k + c * self.scalar;
            Signature { bytes: join(&r_bytes, &z.to_bytes()), r, z }
        }

        /// Signing with nonce randomness drawn from `rng`.
        pub fn sign_with_rng<R: rand_core::CryptoRng + rand_core::RngCore + ?Sized>(
            &self,
            message: &[u8],
            rng: &mut R,
        ) -> Signature {
            let mut a = Zeroizing::new([0u8; 32]);
            rng.fill_bytes(&mut *a);
            self.sign(message, &a)
        }
    }

    impl fmt::Debug for SigningKey {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str("ristretto255::SigningKey(<redacted>)")
        }
    }

    impl PartialEq for SigningKey {
        fn eq(&self, other: &Self) -> bool {
            self.scalar.ct_eq(&other.scalar).into()
        }
    }

    impl Eq for SigningKey {}

    /// Seed derivation (`signing.md` section 1.7): `SHA-512(seed) mod L`, which must be non-zero.
    pub fn from_seed(seed: &[u8]) -> Result<(SigningKey, VerifyingKey), SigningError> {
        let seed = Zeroizing::new(exact::<32>("ristretto255 seed", seed)?);
        let wide = Zeroizing::new(sha512(&[&*seed]));
        let scalar = Scalar::from_bytes_mod_order_wide(&wide);
        let sk = SigningKey::from_bytes(&*Zeroizing::new(scalar.to_bytes()))?;
        let vk = sk.verifying_key();
        Ok((sk, vk))
    }
}
