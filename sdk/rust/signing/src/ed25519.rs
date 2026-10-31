//! Ed25519 (`signing.md` section 1.1): RFC 8032 signing, **ZIP-215** verification (the consensus rule), and
//! libsodium's `crypto_sign_ed25519_verify_detached` rule for compatibility (Cardano KES leaves).
//!
//! Both verifiers are written on curve25519-dalek, whose `CompressedEdwardsY::decompress` accepts non-canonical `y`
//! and the sign bit on `x = 0` (ZIP-215's decoding). Neither of ed25519-dalek's verifiers is either rule.

use curve25519_dalek::edwards::{CompressedEdwardsY, EdwardsPoint};
use curve25519_dalek::scalar::Scalar;
use curve25519_dalek::traits::IsIdentity;

use crate::error::SigningError;
use crate::util::{canonical_y, exact, halves, sha512};

/// Verifying key length.
pub const VERIFYING_KEY_LENGTH: usize = 32;
/// Signature length (`R || S`).
pub const SIGNATURE_LENGTH: usize = 64;
/// Secret key (seed) length.
pub const SIGNING_KEY_LENGTH: usize = 32;

/// A verifying key that decodes to a curve point under ZIP-215 (non-canonical encodings allowed).
#[derive(Clone, Copy, Debug)]
pub struct VerifyingKey {
    bytes: [u8; 32],
    point: EdwardsPoint,
}

impl VerifyingKey {
    /// Decodes 32 bytes: a wrong length is `InvalidEncoding`, a non-point `InvalidKey`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
        let bytes: [u8; 32] = exact("Ed25519 verifying key", bytes)?;
        let point = decode_zip215(&bytes)
            .ok_or_else(|| SigningError::InvalidKey { reason: "Ed25519 verifying key is not a valid point".into() })?;
        Ok(Self { bytes, point })
    }

    /// The encoding, exactly as given.
    pub const fn as_bytes(&self) -> &[u8; 32] {
        &self.bytes
    }

    #[cfg(feature = "sign")]
    pub(crate) fn from_point(point: EdwardsPoint) -> Self {
        Self { bytes: point.compress().to_bytes(), point }
    }
}

impl PartialEq for VerifyingKey {
    fn eq(&self, other: &Self) -> bool {
        self.bytes == other.bytes
    }
}

impl Eq for VerifyingKey {}

/// A 64-byte signature `R || S`. Only the length is checked: a non-canonical `S`, or an `R` that does not decode,
/// verifies `false`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Signature([u8; 64]);

impl Signature {
    /// Decodes 64 bytes.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
        exact("Ed25519 signature", bytes).map(Self)
    }

    /// The encoding.
    pub const fn as_bytes(&self) -> &[u8; 64] {
        &self.0
    }

    #[cfg(feature = "sign")]
    pub(crate) const fn from_array(bytes: [u8; 64]) -> Self {
        Self(bytes)
    }
}

/// ZIP-215 decoding: any `y` (reduced mod p) that is on the curve, any sign bit.
pub(crate) fn decode_zip215(bytes: &[u8; 32]) -> Option<EdwardsPoint> {
    CompressedEdwardsY(*bytes).decompress()
}

fn canonical_scalar(bytes: &[u8; 32]) -> Option<Scalar> {
    Scalar::from_canonical_bytes(*bytes).into()
}

/// `k = SHA-512(R || A || M) mod L`, over the encodings as given.
fn challenge(r: &[u8; 32], a: &[u8; 32], message: &[u8]) -> Scalar {
    Scalar::from_bytes_mod_order_wide(&sha512(&[r, a, message]))
}

/// ZIP-215: `S < L`, `A` and `R` decode, and `[8]([S]B - R - [k]A) = 0`.
pub fn verify(key: &VerifyingKey, message: &[u8], signature: &Signature) -> bool {
    let (r_bytes, s_bytes) = halves(&signature.0);
    let (Some(r), Some(s)) = (decode_zip215(&r_bytes), canonical_scalar(&s_bytes)) else {
        return false;
    };
    let k = challenge(&r_bytes, &key.bytes, message);
    let check = EdwardsPoint::vartime_double_scalar_mul_basepoint(&-k, &key.point, &s) - r;
    check.mul_by_cofactor().is_identity()
}

/// libsodium's rule: `S < L`; `A` with canonical `y` and not of small order; and `[S]B - [k]A`, computed
/// cofactorless, encodes exactly to `R` and is not of small order.
pub fn verify_libsodium(key: &VerifyingKey, message: &[u8], signature: &Signature) -> bool {
    let (r_bytes, s_bytes) = halves(&signature.0);
    if !canonical_y(&key.bytes) || key.point.is_small_order() {
        return false;
    }
    let Some(s) = canonical_scalar(&s_bytes) else {
        return false;
    };
    let k = challenge(&r_bytes, &key.bytes, message);
    let r = EdwardsPoint::vartime_double_scalar_mul_basepoint(&k, &-key.point, &s);
    r.compress().to_bytes() == r_bytes && !r.is_small_order()
}

/// ZIP-215 verification over raw encodings: the key is decoded first, then the signature.
pub fn verify_bytes(key: &[u8], message: &[u8], signature: &[u8]) -> Result<bool, SigningError> {
    let key = VerifyingKey::from_bytes(key)?;
    let signature = Signature::from_bytes(signature)?;
    Ok(verify(&key, message, &signature))
}

/// libsodium verification over raw encodings.
pub fn verify_libsodium_bytes(key: &[u8], message: &[u8], signature: &[u8]) -> Result<bool, SigningError> {
    let key = VerifyingKey::from_bytes(key)?;
    let signature = Signature::from_bytes(signature)?;
    Ok(verify_libsodium(&key, message, &signature))
}

#[cfg(feature = "sign")]
pub use signing::{SigningKey, from_seed};

#[cfg(feature = "sign")]
mod signing {
    use core::fmt;

    use curve25519_dalek::edwards::EdwardsPoint;
    use curve25519_dalek::scalar::Scalar;
    use subtle::ConstantTimeEq;
    use zeroize::{Zeroize, ZeroizeOnDrop};

    use super::{Signature, VerifyingKey};
    use crate::error::SigningError;
    use crate::util::{exact, join, sha512};

    /// An RFC 8032 secret key: the 32-byte seed.
    #[derive(Clone, Zeroize, ZeroizeOnDrop)]
    pub struct SigningKey {
        seed: [u8; 32],
    }

    impl SigningKey {
        /// Any 32 bytes are a seed; a wrong length is `InvalidEncoding`.
        pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
            exact("Ed25519 signing key", bytes).map(|seed| Self { seed })
        }

        /// The seed.
        pub const fn as_bytes(&self) -> &[u8; 32] {
            &self.seed
        }

        /// `A = [s]B` for the clamped scalar `s` of `SHA-512(seed)[0:32]`.
        pub fn verifying_key(&self) -> VerifyingKey {
            let (s, _) = expand(&self.seed);
            VerifyingKey::from_point(EdwardsPoint::mul_base(&s))
        }

        /// RFC 8032 section 5.1.6 (pure Ed25519, no context).
        pub fn sign(&self, message: &[u8]) -> Signature {
            let (s, prefix) = expand(&self.seed);
            let a = EdwardsPoint::mul_base(&s).compress().to_bytes();
            Signature::from_array(sign_expanded(&s, &prefix, &a, message))
        }
    }

    impl fmt::Debug for SigningKey {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str("ed25519::SigningKey(<redacted>)")
        }
    }

    impl PartialEq for SigningKey {
        fn eq(&self, other: &Self) -> bool {
            self.seed.ct_eq(&other.seed).into()
        }
    }

    impl Eq for SigningKey {}

    /// Ed25519 seed derivation (`signing.md` section 1.7): the seed is the key.
    pub fn from_seed(seed: &[u8]) -> Result<(SigningKey, VerifyingKey), SigningError> {
        let sk = SigningKey::from_bytes(seed)?;
        let vk = sk.verifying_key();
        Ok((sk, vk))
    }

    /// The clamped secret scalar (reduced mod L, which `[s]B` cannot tell apart) and the nonce prefix.
    pub(crate) fn expand(seed: &[u8; 32]) -> (Scalar, zeroize::Zeroizing<[u8; 32]>) {
        let h = zeroize::Zeroizing::new(sha512(&[seed]));
        let mut lo = zeroize::Zeroizing::new([0u8; 32]);
        let mut prefix = zeroize::Zeroizing::new([0u8; 32]);
        let (a, b) = h.split_at(32);
        lo.copy_from_slice(a);
        prefix.copy_from_slice(b);
        (Scalar::from_bytes_mod_order(clamp(*lo)), prefix)
    }

    /// RFC 8032 clamping.
    pub(crate) fn clamp(mut bytes: [u8; 32]) -> [u8; 32] {
        if let Some(first) = bytes.first_mut() {
            *first &= 0xf8;
        }
        if let Some(last) = bytes.last_mut() {
            *last = (*last & 0x7f) | 0x40;
        }
        bytes
    }

    /// `r = SHA-512(prefix || M) mod L`, `R = [r]B`, `S = r + SHA-512(R || A || M) s mod L`.
    pub(crate) fn sign_expanded(s: &Scalar, prefix: &[u8; 32], a: &[u8; 32], message: &[u8]) -> [u8; 64] {
        let r = Scalar::from_bytes_mod_order_wide(&sha512(&[prefix, message]));
        let r_point = EdwardsPoint::mul_base(&r).compress().to_bytes();
        let k = Scalar::from_bytes_mod_order_wide(&sha512(&[&r_point, a, message]));
        join(&r_point, &(r + k * s).to_bytes())
    }
}

#[cfg(feature = "sign")]
pub(crate) use signing::sign_expanded;
#[cfg(feature = "sign")]
#[allow(unused_imports, reason = "used by the VRF")]
pub(crate) use signing::{clamp, expand};
