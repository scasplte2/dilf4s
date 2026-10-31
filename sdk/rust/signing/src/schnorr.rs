//! BIP-340 Schnorr and dilf4s tagged Schnorr over secp256k1 (`signing.md` section 1.4).
//!
//! The two schemes share key encodings but are distinct types, and neither accepts the other's signatures:
//!
//! - [`bip340`] signs exactly 32 bytes (`&[u8; 32]`), byte-compatible with Bitcoin, Nostr and wallets;
//! - [`tagged`] signs `taggedHash_SHA256("dilf4s/sig/bip340/v1", message)` of a message of any length.
//!
//! Written on k256's group arithmetic: BIP-340's decoding rules (`r < p`, `s < n`, `s = 0` allowed) are applied here,
//! and malformed signature fields verify `false` rather than failing.

use k256::elliptic_curve::Group as _;
use k256::elliptic_curve::PrimeField as _;
use k256::elliptic_curve::ops::Reduce as _;
use k256::elliptic_curve::point::{AffineCoordinates as _, DecompactPoint as _};
use k256::{AffinePoint, FieldBytes, ProjectivePoint, Scalar};

use crate::error::SigningError;
use crate::util::{be_less_than, exact, halves, tagged_sha256};

/// The field prime `p` of secp256k1, big-endian.
const P: [u8; 32] = [
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe, 0xff, 0xff, 0xfc, 0x2f,
];

/// The dilf4s tag for [`tagged`] signatures.
pub const TAG: &str = "dilf4s/sig/bip340/v1";

/// A 32-byte x-only key that lifts to a curve point (with even `y`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct XOnly {
    x: [u8; 32],
    point: AffinePoint,
}

impl XOnly {
    fn decode(what: &'static str, bytes: &[u8]) -> Result<Self, SigningError> {
        let x: [u8; 32] = exact(what, bytes)?;
        let point: Option<AffinePoint> = AffinePoint::decompact(&FieldBytes::from(x)).into();
        point
            .map(|point| Self { x, point })
            .ok_or_else(|| SigningError::InvalidKey { reason: "x-only key does not lift to a curve point".into() })
    }
}

fn challenge(r: &[u8; 32], px: &[u8; 32], message: &[u8]) -> Scalar {
    Scalar::reduce(&FieldBytes::from(tagged_sha256(b"BIP0340/challenge", &[r, px, message])))
}

/// BIP-340 `Verify(pk, m, sig)`.
fn verify_raw(key: &XOnly, message: &[u8], signature: &[u8; 64]) -> bool {
    let (r, s) = halves(signature);
    if !be_less_than(&r, &P) {
        return false;
    }
    let Some(s) = Option::<Scalar>::from(Scalar::from_repr(FieldBytes::from(s))) else {
        return false;
    };
    let e = challenge(&r, &key.x, message);
    let big_r = ProjectivePoint::GENERATOR * s - ProjectivePoint::from(key.point) * e;
    if bool::from(big_r.is_identity()) {
        return false;
    }
    let big_r = big_r.to_affine();
    let x: [u8; 32] = big_r.x().into();
    !bool::from(big_r.y_is_odd()) && x == r
}

macro_rules! schnorr_scheme {
    ($label:literal, $message:ty, $prepare:path) => {
        use crate::error::SigningError;
        use crate::util::exact;

        /// Verifying key length (x-only).
        pub const VERIFYING_KEY_LENGTH: usize = 32;
        /// Signature length (`R.x || s`).
        pub const SIGNATURE_LENGTH: usize = 64;
        /// Secret key length (big-endian scalar).
        pub const SIGNING_KEY_LENGTH: usize = 32;

        /// An x-only verifying key.
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub struct VerifyingKey(super::XOnly);

        impl VerifyingKey {
            /// Decodes 32 bytes: a wrong length is `InvalidEncoding`, an `x` with no curve point `InvalidKey`.
            pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
                super::XOnly::decode(concat!($label, " verifying key"), bytes).map(Self)
            }

            /// The x-only encoding.
            pub const fn as_bytes(&self) -> &[u8; 32] {
                &self.0.x
            }
        }

        /// A 64-byte signature `R.x || s`. Only the length is checked: out-of-range fields verify `false`.
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub struct Signature([u8; 64]);

        impl Signature {
            /// Decodes 64 bytes.
            pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
                exact(concat!($label, " signature"), bytes).map(Self)
            }

            /// The encoding.
            pub const fn as_bytes(&self) -> &[u8; 64] {
                &self.0
            }
        }

        /// Verification.
        pub fn verify(key: &VerifyingKey, message: &$message, signature: &Signature) -> bool {
            super::verify_raw(&key.0, &$prepare(message), &signature.0)
        }

        #[cfg(feature = "sign")]
        mod signing {
            use core::fmt;

            use zeroize::{Zeroize, ZeroizeOnDrop};

            use super::{Signature, VerifyingKey};
            use crate::error::SigningError;
            use crate::util::exact;

            /// A secret scalar in `[1, n)`.
            #[derive(Clone, Zeroize, ZeroizeOnDrop)]
            pub struct SigningKey {
                bytes: [u8; 32],
            }

            impl SigningKey {
                /// Decodes 32 big-endian bytes: a wrong length is `InvalidEncoding`, a scalar outside `[1, n)`
                /// `InvalidKey`.
                pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
                    let bytes: [u8; 32] = exact(concat!($label, " signing key"), bytes)?;
                    let key = Self { bytes };
                    super::super::secret_scalar(&key.bytes).map(|_| key)
                }

                /// The big-endian scalar, exactly as given (not negated for an odd `y`).
                pub const fn as_bytes(&self) -> &[u8; 32] {
                    &self.bytes
                }

                /// The x-only public key.
                pub fn verifying_key(&self) -> Result<VerifyingKey, SigningError> {
                    super::super::x_only(&self.bytes).map(VerifyingKey)
                }

                /// BIP-340 signing with explicit 32-byte auxiliary randomness (fresh per signature in production).
                pub fn sign(&self, message: &$message, aux: &[u8; 32]) -> Result<Signature, SigningError> {
                    super::super::sign_raw(&self.bytes, &$prepare(message), aux).map(Signature)
                }

                /// Signing with auxiliary randomness drawn from `rng`.
                pub fn sign_with_rng<R: rand_core::CryptoRng + ?Sized>(
                    &self,
                    message: &$message,
                    rng: &mut R,
                ) -> Result<Signature, SigningError> {
                    let mut aux = zeroize::Zeroizing::new([0u8; 32]);
                    rng.fill_bytes(&mut *aux);
                    self.sign(message, &aux)
                }
            }

            impl fmt::Debug for SigningKey {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    f.write_str(concat!($label, " SigningKey(<redacted>)"))
                }
            }

            impl PartialEq for SigningKey {
                fn eq(&self, other: &Self) -> bool {
                    use subtle::ConstantTimeEq as _;
                    self.bytes.ct_eq(&other.bytes).into()
                }
            }

            impl Eq for SigningKey {}

            /// Seed derivation (`signing.md` section 1.7): the big-endian scalar, which must lie in `[1, n)`.
            pub fn from_seed(seed: &[u8]) -> Result<(SigningKey, VerifyingKey), SigningError> {
                let sk = SigningKey::from_bytes(seed)?;
                let vk = sk.verifying_key()?;
                Ok((sk, vk))
            }
        }

        #[cfg(feature = "sign")]
        pub use signing::{SigningKey, from_seed};
    };
}

fn exact_message(message: &[u8; 32]) -> [u8; 32] {
    *message
}

/// `taggedHash_SHA256("dilf4s/sig/bip340/v1", message)`.
pub fn tagged_digest(message: &[u8]) -> [u8; 32] {
    tagged_sha256(TAG.as_bytes(), &[message])
}

/// Standard BIP-340 over exactly 32 bytes.
pub mod bip340 {
    schnorr_scheme!("BIP-340", [u8; 32], crate::schnorr::exact_message);

    /// Verification over raw encodings: the key, then the signature, then the 32-byte message are decoded.
    pub fn verify_bytes(key: &[u8], message: &[u8], signature: &[u8]) -> Result<bool, SigningError> {
        let key = VerifyingKey::from_bytes(key)?;
        let signature = Signature::from_bytes(signature)?;
        let message: [u8; 32] = exact("BIP-340 message", message)?;
        Ok(verify(&key, &message, &signature))
    }
}

/// dilf4s tagged Schnorr: BIP-340 over [`tagged_digest`] of a message of any length.
pub mod tagged {
    schnorr_scheme!("secp256k1-tagged-Schnorr", [u8], crate::schnorr::tagged_digest);

    /// Verification over raw encodings: the key is decoded first, then the signature.
    pub fn verify_bytes(key: &[u8], message: &[u8], signature: &[u8]) -> Result<bool, SigningError> {
        let key = VerifyingKey::from_bytes(key)?;
        let signature = Signature::from_bytes(signature)?;
        Ok(verify(&key, message, &signature))
    }
}

#[cfg(feature = "sign")]
fn secret_scalar(bytes: &[u8; 32]) -> Result<k256::NonZeroScalar, SigningError> {
    Option::from(k256::NonZeroScalar::from_repr(FieldBytes::from(*bytes)))
        .ok_or_else(|| SigningError::InvalidKey { reason: "secp256k1 secret key must be a scalar in [1, n)".into() })
}

#[cfg(feature = "sign")]
fn x_only(secret: &[u8; 32]) -> Result<XOnly, SigningError> {
    let d = secret_scalar(secret)?;
    let point = (ProjectivePoint::GENERATOR * *d).to_affine();
    let x: [u8; 32] = point.x().into();
    // the even-y lift of x
    XOnly::decode("x-only key", &x)
}

/// BIP-340 `Sign(sk, m, a)`.
#[cfg(feature = "sign")]
fn sign_raw(secret: &[u8; 32], message: &[u8], aux: &[u8; 32]) -> Result<[u8; 64], SigningError> {
    use k256::elliptic_curve::subtle::ConditionallySelectable as _;
    use zeroize::Zeroizing;

    let d0 = secret_scalar(secret)?;
    let p = (ProjectivePoint::GENERATOR * *d0).to_affine();
    let d = Zeroizing::new(Scalar::conditional_select(&d0, &-*d0, p.y_is_odd()));
    let px: [u8; 32] = p.x().into();
    let mut t = Zeroizing::new(tagged_sha256(b"BIP0340/aux", &[aux]));
    let d_bytes: Zeroizing<[u8; 32]> = Zeroizing::new(d.to_repr().into());
    for (a, b) in t.iter_mut().zip(d_bytes.iter()) {
        *a ^= b;
    }
    let rand = Zeroizing::new(tagged_sha256(b"BIP0340/nonce", &[&*t, &px, message]));
    let k0 = Zeroizing::new(Scalar::reduce(&FieldBytes::from(*rand)));
    if bool::from(k0.is_zero()) {
        return Err(SigningError::InvalidKey { reason: "the derived nonce is zero; use other randomness".into() });
    }
    let big_r = (ProjectivePoint::GENERATOR * *k0).to_affine();
    let k = Zeroizing::new(Scalar::conditional_select(&k0, &-*k0, big_r.y_is_odd()));
    let rx: [u8; 32] = big_r.x().into();
    let e = challenge(&rx, &px, message);
    let s: [u8; 32] = (*k + e * *d).to_repr().into();
    Ok(crate::util::join(&rx, &s))
}
