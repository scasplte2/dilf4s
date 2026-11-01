//! The cryptographic backend on `dilf4s-signing` and `dilf4s-proofs`, with the reference's verdicts
//! (`CryptoOps.scala`, `ZkOps.scala`, `ProofOps.scala`): malformed input is a failure (a `Crypto` error), a well-formed
//! input that does not verify is `false`.

use alloc::string::ToString;
use alloc::vec::Vec;

use dilf4s_core::hash::{Digest, Sha256};
use dilf4s_core::{BigInt, Value};
use dilf4s_proofs::merkle::{self, ConsistencyProof, InclusionProof, Rfc6962, Tagged};
use dilf4s_proofs::poseidon::{self, merkle as pmt};
use dilf4s_proofs::trie::{self, TrieHashing};
use dilf4s_proofs::zk::{self, groth16};
use dilf4s_signing::vrf::SuiteId;
use dilf4s_signing::{bls, ecdsa, ecrecover, ed25519, schnorr, sigma};
use num_bigint::Sign;

use crate::crypto::{CryptoBackend, CryptoFailure, KeyMode, MerkleProfile, TrieEntry};

type R<T> = Result<T, CryptoFailure>;

#[allow(clippy::needless_pass_by_value, reason = "the error types are passed straight from `map_err`")]
fn fail(e: impl ToString) -> CryptoFailure {
    CryptoFailure(e.to_string())
}

/// The native backend. It holds what is decoded once: SP1's Groth16 verifying key, and the Merkle and trie hashings.
#[derive(Clone)]
pub struct Native {
    groth16: groth16::VerifyingKey,
    tagged: Tagged<Sha256>,
    trie: TrieHashing<Sha256>,
}

impl Native {
    /// The backend, decoding SP1's Groth16 verifying key (circuit v6.1.0) once.
    pub fn new() -> Result<Self, CryptoFailure> {
        Ok(Self {
            groth16: groth16::sp1::verifying_key().map_err(fail)?,
            tagged: Tagged::new(),
            trie: TrieHashing::new(),
        })
    }
}

impl core::fmt::Debug for Native {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("Native")
    }
}

fn digest(bytes: &[u8]) -> R<Digest<Sha256>> {
    Digest::<Sha256>::from_slice(bytes).map_err(fail)
}

/// A Merkle verdict: `InvalidProof` and `RootMismatch` are `false` (the verifiers' own mapping), the rest malformed.
fn merkle_verdict(r: Result<bool, merkle::MerkleError>) -> R<bool> {
    r.map_err(fail)
}

fn trie_mode(m: KeyMode) -> R<trie::KeyMode> {
    match m {
        KeyMode::Fixed(n) => trie::KeyMode::fixed(u64::from(n)),
        KeyMode::Variable(n) => trie::KeyMode::variable(u64::from(n)),
    }
    .map_err(fail)
}

impl CryptoBackend for Native {
    fn ed25519_verify(&self, key: &[u8], message: &[u8], signature: &[u8]) -> R<bool> {
        ed25519::verify_bytes(key, message, signature).map_err(fail)
    }

    fn secp256k1_verify(&self, key: &[u8], message: &[u8], signature: &[u8]) -> R<bool> {
        ecdsa::secp256k1::verify_bytes(key, message, signature).map_err(fail)
    }

    fn bls_verify(&self, key: &[u8], message: &[u8], signature: &[u8]) -> R<bool> {
        bls::verify_bytes(key, message, signature).map_err(fail)
    }

    fn bip340_verify(&self, key: &[u8], message: &[u8], signature: &[u8]) -> R<bool> {
        schnorr::bip340::verify_bytes(key, message, signature).map_err(fail)
    }

    fn bls_aggregate_verify(&self, keys: &[&[u8]], message: &[u8], signature: &[u8]) -> R<bool> {
        let vks = keys.iter().map(|k| bls::VerifyingKey::from_bytes(k)).collect::<Result<Vec<_>, _>>().map_err(fail)?;
        let sig = bls::Signature::from_bytes(signature).map_err(fail)?;
        Ok(bls::fast_aggregate_verify(&vks, message, &sig))
    }

    fn ecrecover(&self, digest: &[u8], signature: &[u8]) -> R<Vec<u8>> {
        ecrecover::recover(digest, signature).map_err(fail)?.map(|a| a.to_vec()).ok_or_else(|| {
            CryptoFailure::new("the signature recovers no key (r or s out of range, high s, or r not an x-coordinate)")
        })
    }

    fn schnorr_verify(&self, key: &[u8], message: &[u8], proof: &[u8]) -> R<bool> {
        sigma::verify_dlog_bytes(key, message, proof).map_err(fail)
    }

    fn sigma_proposition(&self, proposition: &Value) -> R<()> {
        sigma::Proposition::from_value(proposition).map(drop).map_err(fail)
    }

    fn sigma_verify(&self, proposition: &Value, proof: &[u8], message: &[u8]) -> R<bool> {
        sigma::verify_value(proposition, proof, message).map_err(fail)
    }

    fn poseidon(&self, inputs: &[&[u8]]) -> R<Vec<u8>> {
        let xs = inputs.iter().map(|b| poseidon::fr_from_be_bytes(b)).collect::<Result<Vec<_>, _>>().map_err(fail)?;
        poseidon::hash(&xs).map(|h| poseidon::fr_to_be_bytes(h).to_vec()).map_err(fail)
    }

    fn pmt_verify(&self, root: &[u8], leaf: &[u8], index: &BigInt, siblings: &[&[u8]]) -> R<bool> {
        let fr = |b: &[u8]| poseidon::fr_from_be_bytes(b).map_err(fail);
        let (root, leaf) = (fr(root)?, fr(leaf)?);
        let sibs = siblings.iter().map(|b| fr(b)).collect::<R<Vec<_>>>()?;
        let depth = sibs.len();
        // decided on the borrowed index, before any copy, and never rendered: it may be millions of bits
        let out_of_range = || CryptoFailure(alloc::format!("index is out of range for depth {depth}"));
        let fits = u64::try_from(depth).is_ok_and(|d| index.bits() <= d);
        if index.sign() == Sign::Minus || !fits {
            return Err(out_of_range());
        }
        let position = index.magnitude().clone();
        let proof = pmt::PoseidonMerkleProof::new(position, sibs).map_err(|_| out_of_range())?;
        Ok(pmt::verify_inclusion(leaf, &proof, root))
    }

    fn groth16_verify(&self, vkey: &[u8], public_values: &[u8], proof: &[u8]) -> R<bool> {
        match groth16::sp1::verify_with(&self.groth16, vkey, public_values, proof) {
            Ok(()) => Ok(true),
            Err(groth16::Groth16Error::Malformed { reason }) => Err(CryptoFailure(reason)),
            Err(
                groth16::Groth16Error::InvalidProof { .. }
                | groth16::Groth16Error::PairingCheckFailed
                | groth16::Groth16Error::Rejected { .. },
            ) => Ok(false),
        }
    }

    fn ecvrf_verify(&self, key: &[u8], alpha: &[u8], proof: &[u8]) -> R<Option<Vec<u8>>> {
        match SuiteId::Tai.verify_bytes(key, alpha, proof) {
            Ok(beta) => Ok(Some(beta.to_vec())),
            Err(e) if e.is_malformed() => Err(fail(e)),
            Err(_) => Ok(None),
        }
    }

    fn bn254_add(&self, a: &[u8], b: &[u8]) -> R<Vec<u8>> {
        zk::add(a, b).map(|p| p.to_vec()).map_err(fail)
    }

    fn bn254_mul(&self, point: &[u8], scalar: &[u8]) -> R<Vec<u8>> {
        zk::mul(point, scalar).map(|p| p.to_vec()).map_err(fail)
    }

    fn bn254_pairing(&self, pairs: &[(&[u8], &[u8])]) -> R<bool> {
        zk::pairing(pairs).map_err(fail)
    }

    fn merkle_inclusion_size(&self, proof: &Value) -> R<u64> {
        InclusionProof::<Sha256>::from_value(proof).map(|p| p.size()).map_err(fail)
    }

    fn merkle_inclusion_verify(&self, profile: MerkleProfile, root: &[u8], leaf: &Value, proof: &Value) -> R<bool> {
        let root = digest(root)?;
        let p = InclusionProof::<Sha256>::from_value(proof).map_err(fail)?;
        match (profile, leaf) {
            (MerkleProfile::Tagged, v) => {
                use dilf4s_proofs::merkle::MerkleHashing;
                merkle_verdict(merkle::verify_inclusion(&self.tagged, &root, &self.tagged.leaf_hash(v), &p))
            }
            (MerkleProfile::Rfc6962, Value::Bytes(b)) => {
                use dilf4s_proofs::merkle::MerkleHashing;
                let profile = Rfc6962::<Sha256>::new();
                merkle_verdict(merkle::verify_inclusion(&profile, &root, &profile.leaf_hash(b), &p))
            }
            (MerkleProfile::Rfc6962, _) => Err(CryptoFailure::new("an rfc6962 leaf is bytes")),
        }
    }

    fn merkle_consistency_sizes(&self, proof: &Value) -> R<(u64, u64)> {
        ConsistencyProof::<Sha256>::from_value(proof).map(|p| (p.old_size(), p.new_size())).map_err(fail)
    }

    fn merkle_consistency_verify(
        &self,
        profile: MerkleProfile,
        old_root: &[u8],
        new_root: &[u8],
        proof: &Value,
    ) -> R<bool> {
        let (old, new) = (digest(old_root)?, digest(new_root)?);
        let p = ConsistencyProof::<Sha256>::from_value(proof).map_err(fail)?;
        match profile {
            MerkleProfile::Tagged => merkle_verdict(merkle::verify_consistency(&self.tagged, &old, &new, &p)),
            MerkleProfile::Rfc6962 => {
                merkle_verdict(merkle::verify_consistency(&Rfc6962::<Sha256>::new(), &old, &new, &p))
            }
        }
    }

    fn trie_verify(&self, mode: KeyMode, root: &[u8], key: &[u8], value: Option<&Value>, proof: &Value) -> R<bool> {
        let claimed = value.unwrap_or(&Value::Null);
        trie::trie_verify(&self.trie, trie_mode(mode)?, root, key, claimed, proof).map_err(fail)
    }

    fn trie_prefix_verify(
        &self,
        mode: KeyMode,
        root: &[u8],
        prefix: &[u8],
        entries: &[TrieEntry<'_>],
        proof: &Value,
    ) -> R<bool> {
        trie::trie_prefix_verify(&self.trie, trie_mode(mode)?, root, prefix, entries, proof).map_err(fail)
    }

    fn trie_range_verify(
        &self,
        mode: KeyMode,
        root: &[u8],
        from: &[u8],
        until: Option<&[u8]>,
        entries: &[TrieEntry<'_>],
        proof: &Value,
    ) -> R<bool> {
        trie::trie_range_verify(&self.trie, trie_mode(mode)?, root, from, until, entries, proof).map_err(fail)
    }
}
