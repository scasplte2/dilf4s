//! ECDSA over P-256 and secp256k1 (`signing.md` section 1.3): `SHA-256(message)`, RFC 6979 nonces, compact
//! `r || s`, low-S only.
//!
//! The decoders enforce low-S themselves (the high-S twin is `InvalidEncoding`), for both curves: `RustCrypto`'s
//! p256 accepts high-S and its k256 normalises, and neither choice is this rule.

macro_rules! ecdsa_scheme {
    ($krate:ident, $label:literal) => {
        use $krate::ecdsa::signature::hazmat::PrehashVerifier;
        use $krate::elliptic_curve::scalar::IsHigh;

        use crate::error::SigningError;
        use crate::util::{exact, halves, sha256};

        /// Verifying key length (compressed SEC1).
        pub const VERIFYING_KEY_LENGTH: usize = 33;
        /// Signature length (`r || s`, big-endian).
        pub const SIGNATURE_LENGTH: usize = 64;
        /// Secret key length (big-endian scalar).
        pub const SIGNING_KEY_LENGTH: usize = 32;

        /// A compressed SEC1 point on the curve (never the identity).
        #[derive(Clone, Debug, PartialEq, Eq)]
        pub struct VerifyingKey {
            bytes: [u8; 33],
            inner: $krate::ecdsa::VerifyingKey,
        }

        impl VerifyingKey {
            /// Decodes 33 bytes: a wrong length is `InvalidEncoding`, anything but a compressed curve point
            /// `InvalidKey`.
            pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
                let bytes: [u8; 33] = exact(concat!($label, " verifying key"), bytes)?;
                let invalid = || SigningError::InvalidKey {
                    reason: concat!($label, " verifying key is not a valid point").into(),
                };
                // the SEC1 parser also takes other tags (0x05 parses); only the compressed tags are keys here
                if !matches!(bytes.first(), Some(0x02 | 0x03)) {
                    return Err(invalid());
                }
                let inner = $krate::ecdsa::VerifyingKey::from_sec1_bytes(&bytes).map_err(|_| invalid())?;
                Ok(Self { bytes, inner })
            }

            /// The compressed encoding.
            pub const fn as_bytes(&self) -> &[u8; 33] {
                &self.bytes
            }

            /// The curve point.
            pub fn as_affine(&self) -> &$krate::AffinePoint {
                self.inner.as_affine()
            }

            #[cfg(feature = "sign")]
            #[allow(dead_code, reason = "not every curve derives keys")]
            pub(crate) fn from_affine(point: $krate::AffinePoint) -> Result<Self, SigningError> {
                use $krate::elliptic_curve::group::GroupEncoding as _;
                let inner = $krate::ecdsa::VerifyingKey::from_affine(point)
                    .map_err(|_| SigningError::InvalidKey { reason: "the identity is not a verifying key".into() })?;
                Ok(Self { bytes: point.to_bytes().into(), inner })
            }
        }

        /// A signature `r || s` with `1 <= r < n` and `1 <= s <= n/2`.
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub struct Signature {
            bytes: [u8; 64],
            inner: $krate::ecdsa::Signature,
        }

        impl Signature {
            /// Decodes 64 bytes: a wrong length, `r` or `s` outside `[1, n)`, or `s > n/2`, is `InvalidEncoding`.
            pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
                const WHAT: &str = concat!($label, " signature");
                let bytes: [u8; 64] = exact(WHAT, bytes)?;
                let (r, s) = halves(&bytes);
                let inner = $krate::ecdsa::Signature::from_scalars(r, s).map_err(|_| {
                    SigningError::InvalidEncoding { what: WHAT, reason: "r and s must be in [1, n)".into() }
                })?;
                if bool::from(inner.s().is_high()) {
                    return Err(SigningError::InvalidEncoding {
                        what: WHAT,
                        reason: "s must be low (s <= n/2)".into(),
                    });
                }
                Ok(Self { bytes, inner })
            }

            /// The encoding.
            pub const fn as_bytes(&self) -> &[u8; 64] {
                &self.bytes
            }
        }

        /// ECDSA verification of `SHA-256(message)`.
        pub fn verify(key: &VerifyingKey, message: &[u8], signature: &Signature) -> bool {
            key.inner.verify_prehash(&sha256(&[message]), &signature.inner).is_ok()
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

            use $krate::ecdsa::signature::hazmat::PrehashSigner;

            use super::{Signature, VerifyingKey};
            use crate::error::SigningError;
            use crate::util::{exact, sha256};

            /// A secret scalar in `[1, n)` (zeroized on drop by the wrapped key).
            #[derive(Clone)]
            pub struct SigningKey {
                inner: $krate::ecdsa::SigningKey,
            }

            impl SigningKey {
                /// Decodes 32 big-endian bytes: a wrong length is `InvalidEncoding`, a scalar outside `[1, n)`
                /// `InvalidKey`.
                pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
                    let bytes: [u8; 32] = exact(concat!($label, " signing key"), bytes)?;
                    let bytes = zeroize::Zeroizing::new(bytes);
                    $krate::ecdsa::SigningKey::from_bytes(&(*bytes).into()).map(|inner| Self { inner }).map_err(|_| {
                        SigningError::InvalidKey {
                            reason: concat!($label, " secret key must be a scalar in [1, n)").into(),
                        }
                    })
                }

                /// The big-endian scalar.
                pub fn to_bytes(&self) -> zeroize::Zeroizing<[u8; 32]> {
                    zeroize::Zeroizing::new(self.inner.to_bytes().into())
                }

                #[allow(dead_code, reason = "only secp256k1 keys are derived")]
                pub(crate) fn scalar(&self) -> &$krate::NonZeroScalar {
                    self.inner.as_nonzero_scalar()
                }

                /// The compressed public key.
                pub fn verifying_key(&self) -> VerifyingKey {
                    use $krate::elliptic_curve::group::GroupEncoding as _;
                    let inner = *self.inner.verifying_key();
                    VerifyingKey { bytes: inner.as_affine().to_bytes().into(), inner }
                }

                /// RFC 6979 over `SHA-256(message)` with HMAC-SHA-256, then `s` replaced by `n - s` if high.
                pub fn sign(&self, message: &[u8]) -> Result<Signature, SigningError> {
                    let inner: $krate::ecdsa::Signature = self
                        .inner
                        .sign_prehash(&sha256(&[message]))
                        .map_err(|_| SigningError::InvalidKey { reason: "signing failed".into() })?;
                    let inner = inner.normalize_s();
                    Ok(Signature { bytes: inner.to_bytes().into(), inner })
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
                    self.to_bytes().ct_eq(&*other.to_bytes()).into()
                }
            }

            impl Eq for SigningKey {}

            /// The wrapped `RustCrypto` key zeroizes its scalar on drop.
            impl zeroize::ZeroizeOnDrop for SigningKey {}

            /// Seed derivation (`signing.md` section 1.7): the big-endian scalar, which must lie in `[1, n)`.
            pub fn from_seed(seed: &[u8]) -> Result<(SigningKey, VerifyingKey), SigningError> {
                let sk = SigningKey::from_bytes(seed)?;
                let vk = sk.verifying_key();
                Ok((sk, vk))
            }
        }
    };
}

/// NIST P-256 (secp256r1) ECDSA.
pub mod p256 {
    ecdsa_scheme!(p256, "P-256");
}

/// secp256k1 ECDSA.
pub mod secp256k1 {
    ecdsa_scheme!(k256, "secp256k1");
}
