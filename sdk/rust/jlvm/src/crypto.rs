//! The cryptographic operations (`CryptoOps.scala`, `ZkOps.scala`, `ProofOps.scala`): the JLVM's own argument checks,
//! and the seam to a [`CryptoBackend`] that does the cryptography.
//!
//! The JLVM checks what needs no cryptographic library: arity, kinds, fixed widths, canonical BN254 scalars, Merkle
//! tree heads, trie key modes and key lengths, and the Merkle profile. The backend decodes keys, signatures,
//! propositions and proofs, and verifies. The calls interleave exactly as the reference's checks do, so the first error
//! is the same: where the reference decodes something cryptographic before checking a later argument's kind (a sigma
//! proposition, a Merkle proof), the backend is called at that point. Every backend failure is a `Crypto` error.

use alloc::string::String;
use alloc::vec::Vec;

use dilf4s_core::value::Text;
use dilf4s_core::{BigInt, Value};
use num_bigint::Sign;
use num_traits::{Signed, ToPrimitive};

use crate::error::JlvmError;
use crate::op::Op;
use crate::values::{self, kind};

type Result<T> = core::result::Result<T, JlvmError>;

/// Why a backend refused its input: malformed cryptographic input (a well-formed proof that does not verify is
/// `false`, not a failure). Reported as a `Crypto` error.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CryptoFailure(pub String);

impl CryptoFailure {
    /// A failure with this message.
    pub fn new(message: impl Into<String>) -> Self {
        Self(message.into())
    }
}

/// A trie key mode (`trie.md` section 1), already validated by the JLVM.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KeyMode {
    /// Keys of exactly this many bytes, `1..=1024`.
    Fixed(u32),
    /// Keys of at most this many bytes, `0..=65535`.
    Variable(u32),
}

/// The largest fixed key width.
pub const MAX_FIXED_KEY_BYTES: u32 = 1024;
/// The largest variable key limit.
pub const MAX_VARIABLE_KEY_BYTES: u32 = 65535;

impl KeyMode {
    fn admits(self, key: &[u8]) -> bool {
        let n = u64::try_from(key.len()).unwrap_or(u64::MAX);
        match self {
            Self::Fixed(b) => n == u64::from(b),
            Self::Variable(max) => n <= u64::from(max),
        }
    }
}

/// The Merkle hashing profile (`merkle.md`).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MerkleProfile {
    /// dilf4s's tagged hashing; leaves are value commitments.
    Tagged,
    /// RFC 6962 hashing; leaves are raw bytes.
    Rfc6962,
}

/// A claimed trie entry: the key and the value it commits to.
pub type TrieEntry<'v> = (&'v [u8], &'v Value);

/// The cryptography behind the JLVM's cryptographic operations. Each method receives arguments the JLVM has already
/// checked as documented, and returns `Err` for malformed input, `Ok(false)` (or `Ok(None)`) for a well-formed input that
/// does not verify.
pub trait CryptoBackend {
    /// Ed25519 under ZIP-215: decode the 32-byte key, then the 64-byte signature, then verify.
    fn ed25519_verify(&self, key: &[u8], message: &[u8], signature: &[u8])
    -> core::result::Result<bool, CryptoFailure>;
    /// ECDSA over secp256k1 (SHA-256 of the message, compact low-S): decode the key, then the signature, then verify.
    fn secp256k1_verify(
        &self,
        key: &[u8],
        message: &[u8],
        signature: &[u8],
    ) -> core::result::Result<bool, CryptoFailure>;
    /// BLS12-381 minimal-pubkey-size: decode the key, then the signature, then verify.
    fn bls_verify(&self, key: &[u8], message: &[u8], signature: &[u8]) -> core::result::Result<bool, CryptoFailure>;
    /// BIP-340: decode the key, check the message is 32 bytes, decode the signature, then verify.
    fn bip340_verify(&self, key: &[u8], message: &[u8], signature: &[u8]) -> core::result::Result<bool, CryptoFailure>;
    /// Fast aggregate BLS verification: decode every key in order, then the signature, then verify.
    fn bls_aggregate_verify(
        &self,
        keys: &[&[u8]],
        message: &[u8],
        signature: &[u8],
    ) -> core::result::Result<bool, CryptoFailure>;
    /// Ethereum's signer recovery: the 20-byte address; a signature that recovers no key is a failure.
    fn ecrecover(&self, digest: &[u8], signature: &[u8]) -> core::result::Result<Vec<u8>, CryptoFailure>;
    /// A one-leaf `dlog` sigma proof (Ergo's P2PK signature): decode the 33-byte key, then verify.
    fn schnorr_verify(&self, key: &[u8], message: &[u8], proof: &[u8]) -> core::result::Result<bool, CryptoFailure>;
    /// Decodes a sigma proposition (`sigma.md` section 3), before the JLVM checks the proof's and message's kinds.
    fn sigma_proposition(&self, proposition: &Value) -> core::result::Result<(), CryptoFailure>;
    /// Verifies a sigma proof of a proposition [`CryptoBackend::sigma_proposition`] accepted.
    fn sigma_verify(
        &self,
        proposition: &Value,
        proof: &[u8],
        message: &[u8],
    ) -> core::result::Result<bool, CryptoFailure>;
    /// Poseidon over BN254 of 1 to 4 canonical 32-byte field elements: the 32-byte digest.
    fn poseidon(&self, inputs: &[&[u8]]) -> core::result::Result<Vec<u8>, CryptoFailure>;
    /// A Poseidon Merkle inclusion proof (canonical field elements, at most 256 siblings, root-first): an index out of
    /// range for the depth is a failure.
    fn pmt_verify(
        &self,
        root: &[u8],
        leaf: &[u8],
        index: &BigInt,
        siblings: &[&[u8]],
    ) -> core::result::Result<bool, CryptoFailure>;
    /// An SP1 Groth16 proof against a 32-byte program verifying key.
    fn groth16_verify(
        &self,
        vkey: &[u8],
        public_values: &[u8],
        proof: &[u8],
    ) -> core::result::Result<bool, CryptoFailure>;
    /// ECVRF-EDWARDS25519-SHA512-TAI (RFC 9381) with a 32-byte key and an 80-byte proof: `Some(beta)` when valid.
    fn ecvrf_verify(
        &self,
        key: &[u8],
        alpha: &[u8],
        proof: &[u8],
    ) -> core::result::Result<Option<Vec<u8>>, CryptoFailure>;
    /// EIP-196 addition of two 64-byte G1 points.
    fn bn254_add(&self, a: &[u8], b: &[u8]) -> core::result::Result<Vec<u8>, CryptoFailure>;
    /// EIP-196 multiplication of a 64-byte G1 point by a 32-byte scalar.
    fn bn254_mul(&self, point: &[u8], scalar: &[u8]) -> core::result::Result<Vec<u8>, CryptoFailure>;
    /// EIP-197 pairing check over (64-byte G1, 128-byte G2) pairs, at most 64; no pairs is true.
    fn bn254_pairing(&self, pairs: &[(&[u8], &[u8])]) -> core::result::Result<bool, CryptoFailure>;
    /// Decodes a Merkle inclusion proof (`merkle-inclusion`), returning the tree size it claims.
    fn merkle_inclusion_size(&self, proof: &Value) -> core::result::Result<u64, CryptoFailure>;
    /// Verifies a decoded Merkle inclusion proof of a leaf against a 32-byte root; a proof of something else is
    /// `false`. Under [`MerkleProfile::Tagged`] the leaf is any value, committed by `commit(v)`; under
    /// [`MerkleProfile::Rfc6962`] it is bytes, hashed raw (the JLVM has checked its kind).
    fn merkle_inclusion_verify(
        &self,
        profile: MerkleProfile,
        root: &[u8],
        leaf: &Value,
        proof: &Value,
    ) -> core::result::Result<bool, CryptoFailure>;
    /// Decodes a Merkle consistency proof (`merkle-consistency`), returning the old and new sizes it claims.
    fn merkle_consistency_sizes(&self, proof: &Value) -> core::result::Result<(u64, u64), CryptoFailure>;
    /// Verifies a decoded Merkle consistency proof between two 32-byte roots.
    fn merkle_consistency_verify(
        &self,
        profile: MerkleProfile,
        old_root: &[u8],
        new_root: &[u8],
        proof: &Value,
    ) -> core::result::Result<bool, CryptoFailure>;
    /// A trie proof (`trie-proof`) of `key`'s value (`None` claims absence) under a 32-byte root. A proof of another
    /// root is `false`; a proof that does not cover the key is a failure.
    fn trie_verify(
        &self,
        mode: KeyMode,
        root: &[u8],
        key: &[u8],
        value: Option<&Value>,
        proof: &Value,
    ) -> core::result::Result<bool, CryptoFailure>;
    /// Whether `entries` (in key order) are exactly the trie's entries under `prefix`.
    fn trie_prefix_verify(
        &self,
        mode: KeyMode,
        root: &[u8],
        prefix: &[u8],
        entries: &[TrieEntry<'_>],
        proof: &Value,
    ) -> core::result::Result<bool, CryptoFailure>;
    /// Whether `entries` (in key order) are exactly the trie's entries in `[from, until)` (`until` `None` is
    /// unbounded).
    fn trie_range_verify(
        &self,
        mode: KeyMode,
        root: &[u8],
        from: &[u8],
        until: Option<&[u8]>,
        entries: &[TrieEntry<'_>],
        proof: &Value,
    ) -> core::result::Result<bool, CryptoFailure>;
}

// ------------------------------------------------------------------------------------------------ dispatch

/// The BN254 scalar field modulus `r`.
const BN254_R: &str = "21888242871839275222246405745257275088548364400416034343698204186575808495617";
/// The most pairs `bn254_pairing` accepts.
const MAX_PAIRS: usize = 64;
/// The most siblings `pmt_verify` accepts: the deepest Poseidon Merkle tree.
const MAX_SIBLINGS: usize = 256;
/// The most inputs `poseidon` accepts.
const MAX_POSEIDON_INPUTS: usize = 4;
const FIELD_BYTES: usize = 32;
const G1_BYTES: usize = 64;
const G2_BYTES: usize = 128;
const VRF_KEY_BYTES: usize = 32;
const VRF_PROOF_BYTES: usize = 80;
const DIGEST_BYTES: usize = 32;

/// Runs cryptographic operation `op` on its evaluated arguments.
#[allow(clippy::too_many_lines, reason = "one arm per operation")]
pub(crate) fn apply(backend: &dyn CryptoBackend, op: Op, args: &[Value]) -> Result<Value> {
    let w = op.wire();
    let fail = |e: CryptoFailure| JlvmError::crypto(w, e.0);
    match op {
        Op::Ed25519Verify => {
            triple(op, args).and_then(|(k, m, s)| backend.ed25519_verify(k, m, s).map(Value::Bool).map_err(fail))
        }
        Op::Secp256k1Verify => {
            triple(op, args).and_then(|(k, m, s)| backend.secp256k1_verify(k, m, s).map(Value::Bool).map_err(fail))
        }
        Op::BlsVerify => {
            triple(op, args).and_then(|(k, m, s)| backend.bls_verify(k, m, s).map(Value::Bool).map_err(fail))
        }
        Op::Bip340Verify => {
            triple(op, args).and_then(|(k, m, s)| backend.bip340_verify(k, m, s).map(Value::Bool).map_err(fail))
        }
        Op::SchnorrVerify => {
            triple(op, args).and_then(|(k, m, s)| backend.schnorr_verify(k, m, s).map(Value::Bool).map_err(fail))
        }
        Op::Ecrecover => match args {
            [d, s] => {
                let digest = bytes(op, d)?;
                let sig = bytes(op, s)?;
                backend.ecrecover(digest, sig).map(Value::bytes).map_err(fail)
            }
            _ => Err(arity(op, alloc::format!("takes [digest, signature], got {} arguments", args.len()))),
        },
        Op::SigmaVerify => match args {
            [p, proof, message] => {
                backend.sigma_proposition(p).map_err(fail)?;
                let pf = bytes(op, proof)?;
                let m = bytes(op, message)?;
                backend.sigma_verify(p, pf, m).map(Value::Bool).map_err(fail)
            }
            _ => Err(arity(op, alloc::format!("takes [proposition, proof, message], got {} arguments", args.len()))),
        },
        Op::BlsAggregateVerify => match args {
            [Value::Array(ks), m, s] => {
                let keys = ks.items().iter().map(|k| bytes(op, k)).collect::<Result<Vec<_>>>()?;
                let msg = bytes(op, m)?;
                let sig = bytes(op, s)?;
                backend.bls_aggregate_verify(&keys, msg, sig).map(Value::Bool).map_err(fail)
            }
            [other, _, _] => {
                Err(JlvmError::type_error(w, alloc::format!("expected an array of keys, got {}", kind(other))))
            }
            _ => Err(arity(op, alloc::format!("takes [[key, ...], message, signature], got {} arguments", args.len()))),
        },
        Op::Poseidon => poseidon(backend, op, args),
        Op::PmtVerify => pmt(backend, op, args),
        Op::Groth16Verify => match args {
            [vkey, public, proof] => {
                let k = sized(op, "vkey", vkey, FIELD_BYTES)?;
                let p = role_bytes(op, "publicValues", public)?;
                let pf = role_bytes(op, "proof", proof)?;
                backend.groth16_verify(k, p, pf).map(Value::Bool).map_err(fail)
            }
            _ => Err(arity(op, alloc::format!("takes [vkey, publicValues, proof], got {} arguments", args.len()))),
        },
        Op::EcvrfVerify => match args {
            [pk, alpha, proof] => {
                let k = sized(op, "pk", pk, VRF_KEY_BYTES)?;
                let a = role_bytes(op, "alpha", alpha)?;
                let pf = sized(op, "proof", proof, VRF_PROOF_BYTES)?;
                match backend.ecvrf_verify(k, a, pf).map_err(fail)? {
                    Some(beta) => values::map(
                        w,
                        alloc::vec![
                            (String::from("valid"), Value::Bool(true)),
                            (String::from("beta"), Value::bytes(beta))
                        ],
                    ),
                    None => values::map(w, alloc::vec![(String::from("valid"), Value::Bool(false))]),
                }
            }
            _ => Err(arity(op, alloc::format!("takes [pk, alpha, proof], got {} arguments", args.len()))),
        },
        Op::Bn254Add => match args {
            [a, b] => {
                let x = sized(op, "a", a, G1_BYTES)?;
                let y = sized(op, "b", b, G1_BYTES)?;
                let sum = backend.bn254_add(x, y).map_err(fail)?;
                values::bounded(w, Value::bytes(sum))
            }
            _ => Err(arity(op, alloc::format!("takes [a, b], got {} arguments", args.len()))),
        },
        Op::Bn254Mul => match args {
            [p, s] => {
                let x = sized(op, "point", p, G1_BYTES)?;
                let k = sized(op, "scalar", s, FIELD_BYTES)?;
                let q = backend.bn254_mul(x, k).map_err(fail)?;
                values::bounded(w, Value::bytes(q))
            }
            _ => Err(arity(op, alloc::format!("takes [point, scalar], got {} arguments", args.len()))),
        },
        Op::Bn254Pairing => pairing(backend, op, args),
        Op::MerkleVerify => merkle(backend, op, args),
        Op::MerkleConsistencyVerify => consistency(backend, op, args),
        Op::TrieVerify => trie(backend, op, args),
        Op::TriePrefixVerify => trie_prefix(backend, op, args),
        Op::TrieRangeVerify => trie_range(backend, op, args),
        _ => Err(JlvmError::crypto(w, "not a cryptographic operation")),
    }
}

fn arity(op: Op, message: String) -> JlvmError {
    JlvmError::arity(op.wire(), message)
}

fn bytes(op: Op, v: &Value) -> Result<&[u8]> {
    match v {
        Value::Bytes(b) => Ok(b),
        other => Err(JlvmError::type_error(op.wire(), alloc::format!("expected bytes, got {}", kind(other)))),
    }
}

fn role_bytes<'v>(op: Op, role: &str, v: &'v Value) -> Result<&'v [u8]> {
    match v {
        Value::Bytes(b) => Ok(b),
        other => Err(JlvmError::type_error(op.wire(), alloc::format!("{role}: expected bytes, got {}", kind(other)))),
    }
}

/// Bytes of exactly `width`: the wrong kind is `Type`, the wrong width `Crypto`.
fn sized<'v>(op: Op, role: &str, v: &'v Value, width: usize) -> Result<&'v [u8]> {
    let b = role_bytes(op, role, v)?;
    if b.len() == width {
        Ok(b)
    } else {
        Err(JlvmError::crypto(op.wire(), alloc::format!("{role}: expected {width} bytes, got {}", b.len())))
    }
}

/// 32 bytes, big-endian, below the BN254 scalar field modulus `r`.
fn fr<'v>(op: Op, role: &str, v: &'v Value) -> Result<&'v [u8]> {
    let b = sized(op, role, v, FIELD_BYTES)?;
    let r = BigInt::parse_bytes(BN254_R.as_bytes(), 10).unwrap_or_default();
    if BigInt::from_bytes_be(Sign::Plus, b) < r {
        Ok(b)
    } else {
        Err(JlvmError::crypto(
            op.wire(),
            alloc::format!("{role}: not a canonical BN254 scalar field element (must be < r)"),
        ))
    }
}

/// `[key, message, signature]`, all bytes.
fn triple(op: Op, args: &[Value]) -> Result<(&[u8], &[u8], &[u8])> {
    match args {
        [k, m, s] => Ok((bytes(op, k)?, bytes(op, m)?, bytes(op, s)?)),
        _ => Err(arity(op, alloc::format!("takes [key, message, signature], got {} arguments", args.len()))),
    }
}

/// `poseidon [x1, .., xn]` or `[[x1, .., xn]]`, `1 <= n <= 4`.
fn poseidon(backend: &dyn CryptoBackend, op: Op, args: &[Value]) -> Result<Value> {
    let inputs: &[Value] = match args {
        [Value::Array(vs)] if !vs.items().is_empty() => vs.items(),
        _ => args,
    };
    if inputs.is_empty() || inputs.len() > MAX_POSEIDON_INPUTS {
        return Err(arity(op, alloc::format!("takes 1 to {MAX_POSEIDON_INPUTS} field elements, got {}", inputs.len())));
    }
    let xs =
        inputs.iter().enumerate().map(|(i, v)| fr(op, &alloc::format!("input[{i}]"), v)).collect::<Result<Vec<_>>>()?;
    let digest = backend.poseidon(&xs).map_err(|e| JlvmError::crypto(op.wire(), e.0))?;
    values::bounded(op.wire(), Value::bytes(digest))
}

/// `pmt_verify [root, leaf, index, [siblings, root-first]]`.
fn pmt(backend: &dyn CryptoBackend, op: Op, args: &[Value]) -> Result<Value> {
    let [root_v, leaf_v, index_v, siblings_v] = args else {
        return Err(arity(op, alloc::format!("takes [root, leaf, index, siblings], got {} arguments", args.len())));
    };
    let root = fr(op, "root", root_v)?;
    let leaf = fr(op, "leaf", leaf_v)?;
    let Value::Int(index) = index_v else {
        return Err(JlvmError::type_error(
            op.wire(),
            alloc::format!("index: expected an integer, got {}", kind(index_v)),
        ));
    };
    let Value::Array(raw) = siblings_v else {
        return Err(JlvmError::type_error(
            op.wire(),
            alloc::format!("siblings: expected an array, got {}", kind(siblings_v)),
        ));
    };
    if raw.items().len() > MAX_SIBLINGS {
        return Err(JlvmError::crypto(
            op.wire(),
            alloc::format!("at most {MAX_SIBLINGS} siblings, got {}", raw.items().len()),
        ));
    }
    let siblings = raw
        .items()
        .iter()
        .enumerate()
        .map(|(i, v)| fr(op, &alloc::format!("sibling[{i}]"), v))
        .collect::<Result<Vec<_>>>()?;
    backend.pmt_verify(root, leaf, index, &siblings).map(Value::Bool).map_err(|e| JlvmError::crypto(op.wire(), e.0))
}

/// `bn254_pairing [[g1, g2], ..]` or `[[[g1, g2], ..]]`: a single argument whose elements are all arrays is the list.
fn pairing(backend: &dyn CryptoBackend, op: Op, args: &[Value]) -> Result<Value> {
    let pairs: &[Value] = match args {
        [Value::Array(vs)] if vs.items().iter().all(|v| matches!(v, Value::Array(_))) => vs.items(),
        _ => args,
    };
    if pairs.len() > MAX_PAIRS {
        return Err(JlvmError::crypto(op.wire(), alloc::format!("at most {MAX_PAIRS} pairs, got {}", pairs.len())));
    }
    let ps = pairs
        .iter()
        .enumerate()
        .map(|(i, v)| match v {
            Value::Array(a) => match a.items() {
                [g1, g2] => Ok((
                    sized(op, &alloc::format!("pair[{i}].g1"), g1, G1_BYTES)?,
                    sized(op, &alloc::format!("pair[{i}].g2"), g2, G2_BYTES)?,
                )),
                other => Err(JlvmError::crypto(
                    op.wire(),
                    alloc::format!("pair[{i}]: expected [g1, g2], got {} elements", other.len()),
                )),
            },
            other => Err(JlvmError::type_error(
                op.wire(),
                alloc::format!("pair[{i}]: expected an array, got {}", kind(other)),
            )),
        })
        .collect::<Result<Vec<_>>>()?;
    backend.bn254_pairing(&ps).map(Value::Bool).map_err(|e| JlvmError::crypto(op.wire(), e.0))
}

// ---- Merkle trees (merkle.md)

/// A SHA-256 digest: bytes (`Type`) of 32 bytes (`Crypto`).
fn digest<'v>(op: Op, what: &str, v: &'v Value) -> Result<&'v [u8]> {
    let b = match v {
        Value::Bytes(b) => b,
        other => {
            return Err(JlvmError::type_error(op.wire(), alloc::format!("{what} is bytes, got {}", kind(other))));
        }
    };
    if b.len() == DIGEST_BYTES {
        Ok(b)
    } else {
        Err(JlvmError::crypto(op.wire(), alloc::format!("{what}: expected {DIGEST_BYTES} bytes, got {}", b.len())))
    }
}

/// A trusted tree head `{root, size}`.
fn head<'v>(op: Op, what: &str, v: &'v Value) -> Result<(&'v [u8], u64)> {
    let crypto = |m: String| JlvmError::crypto(op.wire(), m);
    let Value::Map(m) = v else {
        return Err(crypto(alloc::format!("{what} is a tree head {{root, size}}")));
    };
    let keys: Vec<&str> = m.keys().map(Text::as_str).collect();
    if keys.len() != 2 || !keys.contains(&"root") || !keys.contains(&"size") {
        return Err(crypto(alloc::format!("{what} is a tree head {{root, size}}")));
    }
    let root = match m.get("root") {
        Some(r) => digest(op, &alloc::format!("{what}.root"), r)?,
        None => return Err(crypto(alloc::format!("{what}.root"))),
    };
    let size = match m.get("size") {
        Some(Value::Int(n)) if !n.is_negative() => n.to_i64().and_then(|n| u64::try_from(n).ok()),
        _ => None,
    }
    .ok_or_else(|| crypto(alloc::format!("{what}.size is a tree size")))?;
    Ok((root, size))
}

/// The optional last argument: none, or `"rfc6962"`.
fn profile(op: Op, rest: &[Value]) -> Result<MerkleProfile> {
    match rest {
        [] => Ok(MerkleProfile::Tagged),
        [Value::Text(t)] if t == "rfc6962" => Ok(MerkleProfile::Rfc6962),
        _ => Err(JlvmError::domain(op.wire(), "the only optional profile is \"rfc6962\"")),
    }
}

fn merkle(backend: &dyn CryptoBackend, op: Op, args: &[Value]) -> Result<Value> {
    let [h, leaf, proof, rest @ ..] = args else {
        return Err(arity(op, String::from("takes [head, leaf, proof] or [head, leaf, proof, profile]")));
    };
    let fail = |e: CryptoFailure| JlvmError::crypto(op.wire(), e.0);
    let (root, size) = head(op, "head", h)?;
    let p = profile(op, rest)?;
    let claimed = backend.merkle_inclusion_size(proof).map_err(fail)?;
    if p == MerkleProfile::Rfc6962 && !matches!(leaf, Value::Bytes(_)) {
        return Err(JlvmError::type_error(op.wire(), alloc::format!("an rfc6962 leaf is bytes, got {}", kind(leaf))));
    }
    // the size comes from the trusted head: a smaller tree's root can satisfy a proof claiming a larger size
    if claimed != size {
        return Ok(Value::Bool(false));
    }
    backend.merkle_inclusion_verify(p, root, leaf, proof).map(Value::Bool).map_err(fail)
}

fn consistency(backend: &dyn CryptoBackend, op: Op, args: &[Value]) -> Result<Value> {
    let [o, n, proof, rest @ ..] = args else {
        return Err(arity(op, String::from("takes [oldHead, newHead, proof] or [oldHead, newHead, proof, profile]")));
    };
    let fail = |e: CryptoFailure| JlvmError::crypto(op.wire(), e.0);
    let (old_root, old_size) = head(op, "oldHead", o)?;
    let (new_root, new_size) = head(op, "newHead", n)?;
    let p = profile(op, rest)?;
    let (claimed_old, claimed_new) = backend.merkle_consistency_sizes(proof).map_err(fail)?;
    if claimed_old != old_size || claimed_new != new_size {
        return Ok(Value::Bool(false));
    }
    backend.merkle_consistency_verify(p, old_root, new_root, proof).map(Value::Bool).map_err(fail)
}

// ---- binary Patricia tries (trie.md)

/// `{"fixed": n}` or `{"variable": max}`: anything else, or a bound out of range, is `Domain`; not a map is `Type`.
fn mode(op: Op, v: &Value) -> Result<KeyMode> {
    let domain = |m: &str| JlvmError::domain(op.wire(), m);
    let Value::Map(m) = v else {
        return Err(JlvmError::type_error(op.wire(), alloc::format!("a mode is an object, got {}", kind(v))));
    };
    let shape = "a mode is {\"fixed\": n} or {\"variable\": max}";
    let mut entries = m.entries();
    match (entries.next(), entries.next()) {
        (Some((k, Value::Int(n))), None) if n.to_i32().is_some() => {
            let n = n.to_i64().unwrap_or(-1);
            match k.as_str() {
                "fixed" => u32::try_from(n)
                    .ok()
                    .filter(|b| (1..=MAX_FIXED_KEY_BYTES).contains(b))
                    .map(KeyMode::Fixed)
                    .ok_or_else(|| domain(&alloc::format!("fixed key length must be 1..{MAX_FIXED_KEY_BYTES}"))),
                "variable" => {
                    u32::try_from(n).ok().filter(|b| *b <= MAX_VARIABLE_KEY_BYTES).map(KeyMode::Variable).ok_or_else(
                        || domain(&alloc::format!("variable key limit must be 0..{MAX_VARIABLE_KEY_BYTES}")),
                    )
                }
                _ => Err(domain(shape)),
            }
        }
        _ => Err(domain(shape)),
    }
}

/// A key: bytes (`Type`) valid for the mode (`Crypto`).
fn key_path(op: Op, m: KeyMode, v: &Value) -> Result<&[u8]> {
    let b = match v {
        Value::Bytes(b) => b,
        other => return Err(JlvmError::type_error(op.wire(), alloc::format!("a key is bytes, got {}", kind(other)))),
    };
    if m.admits(b) {
        Ok(b)
    } else {
        Err(JlvmError::crypto(op.wire(), alloc::format!("a key invalid for the mode {m:?}: {} bytes", b.len())))
    }
}

/// Claimed entries: `[key, value]` pairs with a value, keys valid for the mode.
fn entries(op: Op, m: KeyMode, v: &Value) -> Result<Vec<TrieEntry<'_>>> {
    let Value::Array(es) = v else {
        return Err(JlvmError::type_error(op.wire(), alloc::format!("entries is an array, got {}", kind(v))));
    };
    es.items()
        .iter()
        .map(|e| match e {
            Value::Array(a) => match a.items() {
                [k, x] if *x != Value::Null => key_path(op, m, k).map(|k| (k, x)),
                _ => Err(JlvmError::type_error(op.wire(), "an entry is [key, value], with a value")),
            },
            _ => Err(JlvmError::type_error(op.wire(), "an entry is [key, value], with a value")),
        })
        .collect()
}

fn trie(backend: &dyn CryptoBackend, op: Op, args: &[Value]) -> Result<Value> {
    let [md, root, key, value, proof] = args else {
        return Err(arity(op, String::from("takes [mode, root, key, value, proof]")));
    };
    let m = mode(op, md)?;
    let k = key_path(op, m, key)?;
    let r = digest(op, "root", root)?;
    let claimed = (*value != Value::Null).then_some(value);
    backend.trie_verify(m, r, k, claimed, proof).map(Value::Bool).map_err(|e| JlvmError::crypto(op.wire(), e.0))
}

fn trie_prefix(backend: &dyn CryptoBackend, op: Op, args: &[Value]) -> Result<Value> {
    let [md, root, prefix, es, proof] = args else {
        return Err(arity(op, String::from("takes [mode, root, prefix, entries, proof]")));
    };
    let m = mode(op, md)?;
    let pre = match prefix {
        Value::Bytes(b) => &b[..],
        other => {
            return Err(JlvmError::type_error(op.wire(), alloc::format!("a prefix is bytes, got {}", kind(other))));
        }
    };
    let claimed = entries(op, m, es)?;
    let r = digest(op, "root", root)?;
    backend
        .trie_prefix_verify(m, r, pre, &claimed, proof)
        .map(Value::Bool)
        .map_err(|e| JlvmError::crypto(op.wire(), e.0))
}

fn trie_range(backend: &dyn CryptoBackend, op: Op, args: &[Value]) -> Result<Value> {
    let [md, root, from, until, es, proof] = args else {
        return Err(arity(op, String::from("takes [mode, root, from, until, entries, proof]")));
    };
    let m = mode(op, md)?;
    let lo = match from {
        Value::Bytes(b) => &b[..],
        other => return Err(JlvmError::type_error(op.wire(), alloc::format!("from is bytes, got {}", kind(other)))),
    };
    let hi = match until {
        Value::Null => None,
        Value::Bytes(b) => Some(&b[..]),
        other => return Err(JlvmError::type_error(op.wire(), alloc::format!("until is bytes, got {}", kind(other)))),
    };
    let claimed = entries(op, m, es)?;
    let r = digest(op, "root", root)?;
    backend
        .trie_range_verify(m, r, lo, hi, &claimed, proof)
        .map(Value::Bool)
        .map_err(|e| JlvmError::crypto(op.wire(), e.0))
}
