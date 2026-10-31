//! One verification interface over every signature scheme, by type ([`Scheme`]) or by name ([`SchemeId`]).
//!
//! Decoding is the malformed check (a typed [`SigningError`]); `verify` on decoded values is the validity check
//! (`bool`). The bytes-level form decodes the key first, then the signature, then the message.

use crate::error::SigningError;
use crate::{bls, ecdsa, ed25519, ristretto255, schnorr};

/// A signature scheme's decoders and verifier.
pub trait Scheme {
    /// The scheme's name, as `signing.md` and the vectors spell it.
    const NAME: &'static str;
    /// A decoded, valid verifying key.
    type VerifyingKey;
    /// A decoded signature.
    type Signature;
    /// The message type: bytes, or exactly 32 bytes for BIP-340.
    type Message: ?Sized;

    /// Decodes a verifying key.
    fn verifying_key(bytes: &[u8]) -> Result<Self::VerifyingKey, SigningError>;
    /// Decodes a signature.
    fn signature(bytes: &[u8]) -> Result<Self::Signature, SigningError>;
    /// Views bytes as a message.
    fn message(bytes: &[u8]) -> Result<&Self::Message, SigningError>;
    /// Verifies decoded values.
    fn verify(key: &Self::VerifyingKey, message: &Self::Message, signature: &Self::Signature) -> bool;

    /// Decodes the key, then the signature, then the message, and verifies.
    fn verify_bytes(key: &[u8], message: &[u8], signature: &[u8]) -> Result<bool, SigningError> {
        let key = Self::verifying_key(key)?;
        let signature = Self::signature(signature)?;
        let message = Self::message(message)?;
        Ok(Self::verify(&key, message, &signature))
    }
}

macro_rules! scheme {
    ($(#[$doc:meta])* $ty:ident, $name:literal, $vk:ty, $sig:ty, $verify:path) => {
        $(#[$doc])*
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum $ty {}

        impl Scheme for $ty {
            const NAME: &'static str = $name;
            type VerifyingKey = $vk;
            type Signature = $sig;
            type Message = [u8];

            fn verifying_key(bytes: &[u8]) -> Result<$vk, SigningError> {
                <$vk>::from_bytes(bytes)
            }
            fn signature(bytes: &[u8]) -> Result<$sig, SigningError> {
                <$sig>::from_bytes(bytes)
            }
            fn message(bytes: &[u8]) -> Result<&[u8], SigningError> {
                Ok(bytes)
            }
            fn verify(key: &$vk, message: &[u8], signature: &$sig) -> bool {
                $verify(key, message, signature)
            }
        }
    };
}

scheme!(
    /// Ed25519 with ZIP-215 verification (Ed25519-BIP32 signatures verify under it too).
    Ed25519, "Ed25519", ed25519::VerifyingKey, ed25519::Signature, ed25519::verify
);
scheme!(
    /// Ed25519 with libsodium's verification rule.
    Ed25519Libsodium, "Ed25519-libsodium", ed25519::VerifyingKey, ed25519::Signature, ed25519::verify_libsodium
);
scheme!(
    /// P-256 ECDSA.
    P256Ecdsa, "P-256-ECDSA", ecdsa::p256::VerifyingKey, ecdsa::p256::Signature, ecdsa::p256::verify
);
scheme!(
    /// secp256k1 ECDSA.
    Secp256k1Ecdsa, "secp256k1-ECDSA", ecdsa::secp256k1::VerifyingKey, ecdsa::secp256k1::Signature,
    ecdsa::secp256k1::verify
);
scheme!(
    /// dilf4s tagged Schnorr over secp256k1.
    TaggedSchnorr, "secp256k1-tagged-Schnorr", schnorr::tagged::VerifyingKey, schnorr::tagged::Signature,
    schnorr::tagged::verify
);
scheme!(
    /// ristretto255 Schnorr (FROST ciphersuite).
    Ristretto255Schnorr, "ristretto255-Schnorr", ristretto255::VerifyingKey, ristretto255::Signature,
    ristretto255::verify
);
scheme!(
    /// BLS12-381 with the proof-of-possession ciphersuite.
    BlsPop, "BLS12-381-PoP", bls::VerifyingKey, bls::Signature, bls::verify
);

/// Standard BIP-340 over exactly 32-byte messages.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Bip340 {}

impl Scheme for Bip340 {
    const NAME: &'static str = "BIP-340";
    type VerifyingKey = schnorr::bip340::VerifyingKey;
    type Signature = schnorr::bip340::Signature;
    type Message = [u8; 32];

    fn verifying_key(bytes: &[u8]) -> Result<Self::VerifyingKey, SigningError> {
        Self::VerifyingKey::from_bytes(bytes)
    }
    fn signature(bytes: &[u8]) -> Result<Self::Signature, SigningError> {
        Self::Signature::from_bytes(bytes)
    }
    fn message(bytes: &[u8]) -> Result<&[u8; 32], SigningError> {
        <&[u8; 32]>::try_from(bytes).map_err(|_| SigningError::InvalidEncoding {
            what: "BIP-340 message",
            reason: alloc::format!("expected 32 bytes, got {}", bytes.len()),
        })
    }
    fn verify(key: &Self::VerifyingKey, message: &[u8; 32], signature: &Self::Signature) -> bool {
        schnorr::bip340::verify(key, message, signature)
    }
}

/// A scheme chosen at run time by name.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SchemeId {
    /// [`Ed25519`] (ZIP-215); Ed25519-BIP32 signatures verify under it too.
    Ed25519,
    /// [`Ed25519Libsodium`].
    Ed25519Libsodium,
    /// [`P256Ecdsa`].
    P256Ecdsa,
    /// [`Secp256k1Ecdsa`].
    Secp256k1Ecdsa,
    /// [`Bip340`].
    Bip340,
    /// [`TaggedSchnorr`].
    TaggedSchnorr,
    /// [`Ristretto255Schnorr`].
    Ristretto255Schnorr,
    /// [`BlsPop`].
    BlsPop,
}

impl SchemeId {
    /// Every scheme.
    pub const ALL: [Self; 8] = [
        Self::Ed25519,
        Self::Ed25519Libsodium,
        Self::P256Ecdsa,
        Self::Secp256k1Ecdsa,
        Self::Bip340,
        Self::TaggedSchnorr,
        Self::Ristretto255Schnorr,
        Self::BlsPop,
    ];

    /// The name `signing.md` and the vectors use.
    pub const fn name(self) -> &'static str {
        match self {
            Self::Ed25519 => Ed25519::NAME,
            Self::Ed25519Libsodium => Ed25519Libsodium::NAME,
            Self::P256Ecdsa => P256Ecdsa::NAME,
            Self::Secp256k1Ecdsa => Secp256k1Ecdsa::NAME,
            Self::Bip340 => Bip340::NAME,
            Self::TaggedSchnorr => TaggedSchnorr::NAME,
            Self::Ristretto255Schnorr => Ristretto255Schnorr::NAME,
            Self::BlsPop => BlsPop::NAME,
        }
    }

    /// The scheme with this name.
    pub fn from_name(name: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|s| s.name() == name)
    }

    /// Bytes-level verification: the key is decoded first, then the signature, then the message.
    pub fn verify_bytes(self, key: &[u8], message: &[u8], signature: &[u8]) -> Result<bool, SigningError> {
        match self {
            Self::Ed25519 => Ed25519::verify_bytes(key, message, signature),
            Self::Ed25519Libsodium => Ed25519Libsodium::verify_bytes(key, message, signature),
            Self::P256Ecdsa => P256Ecdsa::verify_bytes(key, message, signature),
            Self::Secp256k1Ecdsa => Secp256k1Ecdsa::verify_bytes(key, message, signature),
            Self::Bip340 => Bip340::verify_bytes(key, message, signature),
            Self::TaggedSchnorr => TaggedSchnorr::verify_bytes(key, message, signature),
            Self::Ristretto255Schnorr => Ristretto255Schnorr::verify_bytes(key, message, signature),
            Self::BlsPop => BlsPop::verify_bytes(key, message, signature),
        }
    }
}
