//! BLS12-381 signatures, Ethereum consensus profile (`signing.md` section 1.6): minimal-pubkey-size (keys in G1,
//! signatures in G2) with the proof-of-possession ciphersuite `BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_`.
//!
//! The signature layer is written here on zkcrypto's `bls12_381` (hash to G2, pairings); that crate's
//! `from_compressed` accepts the identity, so `KeyValidate` rejects it explicitly.

use alloc::vec::Vec;

use bls12_381::hash_to_curve::{ExpandMsgXmd, HashToCurve};
use bls12_381::{G1Affine, G1Projective, G2Affine, G2Prepared, G2Projective, Gt, multi_miller_loop};

use crate::error::SigningError;
use crate::util::exact;

/// Verifying key length (compressed G1).
pub const VERIFYING_KEY_LENGTH: usize = 48;
/// Signature length (compressed G2).
pub const SIGNATURE_LENGTH: usize = 96;
/// Secret key length (big-endian scalar in `[1, r)`).
pub const SIGNING_KEY_LENGTH: usize = 32;
/// The message-signature DST.
pub const SIGNATURE_DST: &[u8] = b"BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_";
/// The proof-of-possession DST.
pub const POP_DST: &[u8] = b"BLS_POP_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_";

/// A public key that passed `KeyValidate`: it decodes, lies in G1, and is not the identity.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct VerifyingKey {
    bytes: [u8; 48],
    point: G1Affine,
}

impl VerifyingKey {
    /// `KeyValidate` over 48 bytes: a wrong length is `InvalidEncoding`, anything else that fails `InvalidKey`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
        let bytes: [u8; 48] = exact("BLS public key", bytes)?;
        let point: Option<G1Affine> = G1Affine::from_compressed(&bytes).into();
        let point =
            point.ok_or_else(|| SigningError::InvalidKey { reason: "BLS public key does not decode".into() })?;
        if bool::from(point.is_identity()) {
            return Err(SigningError::InvalidKey { reason: "BLS public key is the identity".into() });
        }
        Ok(Self { bytes, point })
    }

    /// The compressed encoding.
    pub const fn as_bytes(&self) -> &[u8; 48] {
        &self.bytes
    }
}

/// A signature: a compressed point of G2. The identity decodes, but never verifies.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Signature {
    bytes: [u8; 96],
    point: G2Affine,
}

impl Signature {
    /// Decodes 96 bytes; a wrong length, a non-point or a point outside G2 is `InvalidEncoding`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
        const WHAT: &str = "BLS signature";
        let bytes: [u8; 96] = exact(WHAT, bytes)?;
        let point: Option<G2Affine> = G2Affine::from_compressed(&bytes).into();
        point.map(|point| Self { bytes, point }).ok_or_else(|| SigningError::InvalidEncoding {
            what: WHAT,
            reason: "does not decode to a point of G2".into(),
        })
    }

    /// The compressed encoding.
    pub const fn as_bytes(&self) -> &[u8; 96] {
        &self.bytes
    }

    fn from_point(point: G2Affine) -> Self {
        Self { bytes: point.to_compressed(), point }
    }
}

/// RFC 9380 `hash_to_curve` into G2 (`BLS12381G2_XMD:SHA-256_SSWU_RO_`) with `dst`.
pub fn hash_to_g2(message: &[u8], dst: &[u8]) -> G2Affine {
    let point = <G2Projective as HashToCurve<ExpandMsgXmd<sha2::Sha256>>>::hash_to_curve([message], dst);
    G2Affine::from(point)
}

/// `prod e(pk_i, H(m_i)) == e(g1, sig)`, for a signature other than the identity.
fn pairing_check(pairs: &[(G1Affine, G2Affine)], signature: &G2Affine) -> bool {
    if bool::from(signature.is_identity()) || pairs.is_empty() {
        return false;
    }
    let neg_g1 = -G1Affine::generator();
    let prepared: Vec<(G1Affine, G2Prepared)> = pairs
        .iter()
        .map(|(p, q)| (*p, G2Prepared::from(*q)))
        .chain(core::iter::once((neg_g1, G2Prepared::from(*signature))))
        .collect();
    let terms: Vec<(&G1Affine, &G2Prepared)> = prepared.iter().map(|(p, q)| (p, q)).collect();
    multi_miller_loop(&terms).final_exponentiation() == Gt::identity()
}

fn core_verify(key: &VerifyingKey, message: &[u8], signature: &Signature, dst: &[u8]) -> bool {
    pairing_check(&[(key.point, hash_to_g2(message, dst))], &signature.point)
}

/// `Verify(PK, message, signature)`.
pub fn verify(key: &VerifyingKey, message: &[u8], signature: &Signature) -> bool {
    core_verify(key, message, signature, SIGNATURE_DST)
}

/// Verification over raw encodings: the key is decoded first, then the signature.
pub fn verify_bytes(key: &[u8], message: &[u8], signature: &[u8]) -> Result<bool, SigningError> {
    let key = VerifyingKey::from_bytes(key)?;
    let signature = Signature::from_bytes(signature)?;
    Ok(verify(&key, message, &signature))
}

/// `Aggregate`: the sum of one or more signatures. None is `InvalidEncoding`.
pub fn aggregate(signatures: &[Signature]) -> Result<Signature, SigningError> {
    if signatures.is_empty() {
        return Err(SigningError::InvalidEncoding {
            what: "BLS aggregate",
            reason: "at least one signature is required".into(),
        });
    }
    let sum = signatures.iter().fold(G2Projective::identity(), |acc, s| acc.add_mixed(&s.point));
    Ok(Signature::from_point(G2Affine::from(sum)))
}

/// `FastAggregateVerify`: one message signed by every key (the keys must carry proofs of possession). No keys is
/// `false`.
pub fn fast_aggregate_verify(keys: &[VerifyingKey], message: &[u8], signature: &Signature) -> bool {
    if keys.is_empty() {
        return false;
    }
    let sum = keys.iter().fold(G1Projective::identity(), |acc, k| acc.add_mixed(&k.point));
    pairing_check(&[(G1Affine::from(sum), hash_to_g2(message, SIGNATURE_DST))], &signature.point)
}

/// `AggregateVerify`: `(key, message)` pairs under one aggregate signature. No pairs is `false`.
pub fn aggregate_verify(pairs: &[(VerifyingKey, &[u8])], signature: &Signature) -> bool {
    let points: Vec<(G1Affine, G2Affine)> =
        pairs.iter().map(|(k, m)| (k.point, hash_to_g2(m, SIGNATURE_DST))).collect();
    pairing_check(&points, &signature.point)
}

/// Every `(key, message, signature)` triple verifies; none is `false`.
pub fn batch_verify(triples: &[(VerifyingKey, &[u8], Signature)]) -> bool {
    !triples.is_empty() && triples.iter().all(|(k, m, s)| verify(k, m, s))
}

/// `PopVerify`: a proof of possession is a signature over the key's encoding under [`POP_DST`].
pub fn pop_verify(key: &VerifyingKey, proof: &Signature) -> bool {
    core_verify(key, &key.bytes, proof, POP_DST)
}

#[cfg(feature = "sign")]
pub use signing::{SigningKey, derive_child_sk, derive_master_sk, from_seed};

#[cfg(feature = "sign")]
mod signing {
    use alloc::vec::Vec;
    use core::fmt;

    use bls12_381::{G1Affine, G2Affine, Scalar};
    use hkdf::Hkdf;
    use subtle::ConstantTimeEq as _;
    use zeroize::{Zeroize, ZeroizeOnDrop, Zeroizing};

    use super::{POP_DST, SIGNATURE_DST, Signature, VerifyingKey, hash_to_g2};
    use crate::error::SigningError;
    use crate::util::{exact, sha256};

    /// A secret scalar in `[1, r)`, kept as its 32 big-endian bytes.
    #[derive(Clone, Zeroize, ZeroizeOnDrop)]
    pub struct SigningKey {
        bytes: [u8; 32],
    }

    fn scalar_of(be: &[u8; 32]) -> Option<Scalar> {
        let mut le = Zeroizing::new(*be);
        le.reverse();
        let s: Option<Scalar> = Scalar::from_bytes(&le).into();
        s.filter(|s| *s != Scalar::zero())
    }

    impl SigningKey {
        /// Decodes 32 big-endian bytes: a wrong length is `InvalidEncoding`, a scalar outside `[1, r)`
        /// `InvalidKey`.
        pub fn from_bytes(bytes: &[u8]) -> Result<Self, SigningError> {
            let bytes: [u8; 32] = exact("BLS signing key", bytes)?;
            let key = Self { bytes };
            key.scalar().map(|_| key)
        }

        /// The big-endian scalar.
        pub const fn as_bytes(&self) -> &[u8; 32] {
            &self.bytes
        }

        fn scalar(&self) -> Result<Zeroizing<Scalar>, SigningError> {
            scalar_of(&self.bytes)
                .map(Zeroizing::new)
                .ok_or_else(|| SigningError::InvalidKey { reason: "BLS secret key must be a scalar in [1, r)".into() })
        }

        /// `SkToPk`.
        pub fn verifying_key(&self) -> VerifyingKey {
            let point = self.scalar().map_or(G1Affine::identity(), |s| G1Affine::from(G1Affine::generator() * *s));
            VerifyingKey { bytes: point.to_compressed(), point }
        }

        fn sign_with(&self, message: &[u8], dst: &[u8]) -> Signature {
            let point = self.scalar().map_or(G2Affine::identity(), |s| G2Affine::from(hash_to_g2(message, dst) * *s));
            Signature::from_point(point)
        }

        /// `Sign(SK, message)`.
        pub fn sign(&self, message: &[u8]) -> Signature {
            self.sign_with(message, SIGNATURE_DST)
        }

        /// `PopProve(SK)`: a signature over the public key's encoding under [`POP_DST`].
        pub fn pop_prove(&self) -> Signature {
            self.sign_with(self.verifying_key().as_bytes(), POP_DST)
        }
    }

    impl fmt::Debug for SigningKey {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str("bls::SigningKey(<redacted>)")
        }
    }

    impl PartialEq for SigningKey {
        fn eq(&self, other: &Self) -> bool {
            self.bytes.ct_eq(&other.bytes).into()
        }
    }

    impl Eq for SigningKey {}

    /// EIP-2333 `HKDF_mod_r(IKM)` with an empty `key_info`.
    fn hkdf_mod_r(ikm: &[u8]) -> Result<SigningKey, SigningError> {
        let mut salt = sha256(&[b"BLS-SIG-KEYGEN-SALT-"]);
        let mut input = Zeroizing::new(Vec::with_capacity(ikm.len().saturating_add(1)));
        input.extend_from_slice(ikm);
        input.push(0);
        // each round fails with probability about 2^-255; bound the loop rather than trust that
        for _ in 0..64 {
            let mut okm = Zeroizing::new([0u8; 48]);
            Hkdf::<sha2::Sha256>::new(Some(&salt), &input)
                .expand(&[0, 48], &mut *okm)
                .map_err(|_| SigningError::InvalidKey { reason: "HKDF expansion failed".into() })?;
            let mut wide = Zeroizing::new([0u8; 64]);
            for (w, o) in wide.iter_mut().zip(okm.iter().rev()) {
                *w = *o;
            }
            let sk = Zeroizing::new(Scalar::from_bytes_wide(&wide));
            if *sk != Scalar::zero() {
                let mut be = sk.to_bytes();
                be.reverse();
                let key = SigningKey { bytes: be };
                be.zeroize();
                return Ok(key);
            }
            salt = sha256(&[&salt]);
        }
        Err(SigningError::InvalidKey { reason: "HKDF_mod_r found no non-zero key".into() })
    }

    /// EIP-2333 `derive_master_SK(seed)`; the seed is at least 32 bytes.
    pub fn derive_master_sk(seed: &[u8]) -> Result<SigningKey, SigningError> {
        if seed.len() < 32 {
            return Err(SigningError::InvalidKey { reason: "EIP-2333 seeds are at least 32 bytes".into() });
        }
        hkdf_mod_r(seed)
    }

    /// EIP-2333 `IKM_to_lamport_SK`: 255 chunks of 32 bytes.
    fn lamport_hashes(ikm: &[u8; 32], salt: [u8; 4], out: &mut Vec<u8>) -> Result<(), SigningError> {
        let mut okm = Zeroizing::new(alloc::vec![0u8; 32 * 255]);
        Hkdf::<sha2::Sha256>::new(Some(&salt), ikm)
            .expand(&[], &mut okm)
            .map_err(|_| SigningError::InvalidKey { reason: "HKDF expansion failed".into() })?;
        for chunk in okm.chunks(32) {
            out.extend_from_slice(&sha256(&[chunk]));
        }
        Ok(())
    }

    /// EIP-2333 `derive_child_SK(parent_SK, index)`.
    pub fn derive_child_sk(parent: &SigningKey, index: u32) -> Result<SigningKey, SigningError> {
        let salt = index.to_be_bytes();
        let ikm = Zeroizing::new(parent.bytes);
        let mut not_ikm = Zeroizing::new(parent.bytes);
        for b in not_ikm.iter_mut() {
            *b = !*b;
        }
        let mut lamport_pk = Vec::with_capacity(2 * 255 * 32);
        lamport_hashes(&ikm, salt, &mut lamport_pk)?;
        lamport_hashes(&not_ikm, salt, &mut lamport_pk)?;
        hkdf_mod_r(&sha256(&[&lamport_pk]))
    }

    /// Seed derivation (`signing.md` section 1.7): EIP-2333 `derive_master_SK(seed)`.
    pub fn from_seed(seed: &[u8]) -> Result<(SigningKey, VerifyingKey), SigningError> {
        let sk = derive_master_sk(seed)?;
        let vk = sk.verifying_key();
        Ok((sk, vk))
    }
}
