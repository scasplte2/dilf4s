//! The cases, read from `shared/vectors`, each with what its vector says the outcome is. Off the guest only.

extern crate std;

use alloc::borrow::ToOwned as _;
use alloc::string::String;
use alloc::vec::Vec;
use alloc::{format, vec};
use std::path::PathBuf;

use dilf4s_core::codec::Model;
use dilf4s_core::generated::jlvm_vectors::{JlvmOutcome, JlvmVectors};
use dilf4s_core::generated::poseidon_vectors::PoseidonVectors;
use dilf4s_core::generated::sigma_vectors::{SigmaKind, SigmaProposition, SigmaType, SigmaVectors};
use dilf4s_core::generated::signing_vectors::{SigningVectors, VerificationResult};
use dilf4s_core::generated::vrf_vectors::VrfVectors;
use dilf4s_core::generated::zk_vectors::{ZkBoolResult, ZkFailure, ZkGroth16Result, ZkPointResult, ZkVectors};
use dilf4s_core::hash::{Hasher as _, Sha256};
use dilf4s_core::{Value, cbor, hex, json};
use dilf4s_proofs::poseidon::merkle::PoseidonMerkleProof;
use dilf4s_proofs::poseidon::{fr_from_int, fr_to_be_bytes};
use dilf4s_signing::ecrecover::address_of_key;
use num_bigint::{BigInt, BigUint};

use crate::leaves::Leaf;
use crate::{Case, Outcome, Output};

/// A failure reading the vectors.
pub type Error = String;

/// What the vector says a case gives.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expect {
    /// Exactly this output.
    Output(Output),
    /// An error whose `Debug` rendering starts with this variant name.
    Failure(&'static str),
    /// An error that is exactly this variant (its `Debug` rendering is the name, alone or before its fields).
    FailureNamed(String),
    /// Any error.
    AnyFailure,
    /// Some bytes: a verified output the vector does not give.
    Verified,
    /// `false` or an error.
    Rejected,
    /// A JLVM outcome: the value's canonical CBOR or the error kind, the gas and the number of warnings.
    Jlvm {
        /// The canonical CBOR, or the error kind.
        result: Result<Vec<u8>, String>,
        /// The gas used.
        gas: u64,
        /// The number of warnings.
        warnings: u64,
    },
    /// One parity of a signature from the signing vectors: some address or none. The pair's two cases together must
    /// recover the signer exactly once ([`Expect::signer_pairs_hold`]).
    SignerParity {
        /// The signer's address.
        signer: Vec<u8>,
    },
}

impl Expect {
    /// Whether an outcome is what the vector says.
    pub fn holds(&self, outcome: &Outcome) -> bool {
        match (self, outcome) {
            (Self::Output(want), Ok(got)) => want == got,
            (Self::Failure(variant), Err(e)) => e.starts_with(variant),
            (Self::FailureNamed(name), Err(e)) => {
                e.strip_prefix(name.as_str()).is_some_and(|rest| rest.is_empty() || rest.starts_with(' '))
            }
            (Self::AnyFailure, Err(_))
            | (Self::Verified, Ok(Output::Bytes(_)))
            | (Self::Rejected, Ok(Output::Bool(false)) | Err(_))
            | (Self::SignerParity { .. }, Ok(Output::Address(_))) => true,
            (Self::Jlvm { result, gas, warnings }, Ok(Output::Jlvm { result: got, gas: g, warnings: w })) => {
                let got = got.clone().map_err(|(kind, _)| kind);
                got == *result && g == gas && w == warnings
            }
            _ => false,
        }
    }

    /// Of each two consecutive `SignerParity` cases (`v` = 27 then 28), exactly one recovers the signer.
    pub fn signer_pairs_hold(named: &[Named], outcomes: &[Outcome]) -> Vec<String> {
        let pairs: Vec<(&Named, &Outcome)> =
            named.iter().zip(outcomes).filter(|(n, _)| matches!(n.expect, Self::SignerParity { .. })).collect();
        pairs
            .chunks(2)
            .filter_map(|pair| {
                let recovered = pair
                    .iter()
                    .filter(|(n, o)| match (&n.expect, o) {
                        (Self::SignerParity { signer }, Ok(Output::Address(Some(a)))) => a == signer,
                        _ => false,
                    })
                    .count();
                (pair.len() != 2 || recovered != 1).then(|| {
                    format!("{}: {recovered} parities recover the signer", pair.first().map_or("", |(n, _)| &n.name))
                })
            })
            .collect()
    }
}

/// A case with its name and what its vector says it gives.
#[derive(Debug, Clone)]
pub struct Named {
    /// The vector file and case.
    pub name: String,
    /// The case.
    pub case: Case,
    /// What the vector says.
    pub expect: Expect,
}

fn named(name: String, case: Case, expect: Expect) -> Named {
    Named { name, case, expect }
}

fn vectors_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../../shared/vectors")
}

/// A vector file parsed with the SDK's JSON parser and decoded into its generated type.
fn load<T: Model>(file: &str) -> Result<T, Error> {
    let text = std::fs::read_to_string(vectors_dir().join(file)).map_err(|e| format!("{file}: {e}"))?;
    let value = json::parse(&text).map_err(|e| format!("{file} is not canonical JSON: {e}"))?;
    T::from_value(&value).map_err(|e| format!("{file} does not fit its schema: {e}"))
}

fn bytes(text: &str) -> Result<Vec<u8>, Error> {
    hex::decode(text.strip_prefix("0x").unwrap_or(text)).map_err(|e| format!("{text}: {e}"))
}

fn decimal_bytes(text: &str) -> Result<Vec<u8>, Error> {
    let n = BigInt::parse_bytes(text.as_bytes(), 10).ok_or_else(|| format!("not a decimal: {text}"))?;
    let x = fr_from_int(&n).map_err(|e| format!("{text}: {e}"))?;
    Ok(fr_to_be_bytes(x).to_vec())
}

/// The `verification` rows and committed signatures of `signing_v1.json` whose scheme `keep` picks, with the BLS
/// proofs of possession when it picks BLS.
pub fn signatures(keep: impl Fn(&str) -> bool) -> Result<Vec<Named>, Error> {
    let v: SigningVectors = load("signing_v1.json")?;
    let mut out = Vec::new();
    for (i, c) in v.verification.iter().enumerate().filter(|(_, c)| keep(&c.scheme)) {
        let expect = match c.result {
            VerificationResult::Valid => Expect::Output(Output::Bool(true)),
            VerificationResult::Invalid => Expect::Output(Output::Bool(false)),
            VerificationResult::MalformedKey | VerificationResult::MalformedSignature => Expect::AnyFailure,
        };
        let case = Case::Verify {
            scheme: c.scheme.clone(),
            key: bytes(&c.verifying_key)?,
            message: bytes(&c.message)?,
            signature: bytes(&c.signature)?,
        };
        out.push(named(format!("verification {i} {} {}", c.scheme, c.name), case, expect));
    }
    for scheme in v.signing.iter().filter(|s| keep(&s.scheme)) {
        for key in &scheme.keys {
            for s in &key.signatures {
                let case = Case::Verify {
                    scheme: scheme.scheme.clone(),
                    key: bytes(&key.verifying_key)?,
                    message: bytes(&s.message)?,
                    signature: bytes(&s.signature)?,
                };
                out.push(named(
                    format!("signing {} {}", scheme.scheme, s.message),
                    case,
                    Expect::Output(Output::Bool(true)),
                ));
            }
        }
    }
    for (i, p) in v.bls_pop.iter().enumerate().filter(|_| keep("BLS12-381-PoP")) {
        let case = Case::BlsPop { key: bytes(&p.verifying_key)?, proof: bytes(&p.proof)? };
        out.push(named(format!("blsPop {i}"), case, Expect::Output(Output::Bool(true))));
    }
    Ok(out)
}

/// Recovery of each committed secp256k1 ECDSA signature, both parities, and go-ethereum's precompile vector with the
/// refusals of `signing/tests/ecrecover.rs`.
pub fn ecrecover() -> Result<Vec<Named>, Error> {
    let v: SigningVectors = load("signing_v1.json")?;
    let mut out = Vec::new();
    for scheme in v.signing.iter().filter(|s| s.scheme == "secp256k1-ECDSA") {
        for key in &scheme.keys {
            let signer = address_of_key(&bytes(&key.verifying_key)?).map_err(|e| format!("{e}"))?.to_vec();
            for s in &key.signatures {
                let digest = Sha256::hash_parts(&[&bytes(&s.message)?]).to_vec();
                for parity in [27u8, 28] {
                    let mut signature = bytes(&s.signature)?;
                    signature.push(parity);
                    out.push(named(
                        format!("ecrecover {} v={parity}", s.message),
                        Case::EcRecover { digest: digest.clone(), signature },
                        Expect::SignerParity { signer: signer.clone() },
                    ));
                }
            }
        }
    }
    // go-ethereum core/vm/testdata/precompiles/ecRecover.json, "ValidKey", through its low-s twin
    let digest = "18c547e4f7b0f325ad1e56f57e26c745b09a3e503d86e00e5255ff7f715d3d1c";
    let r = "73b1693892219d736caba55bdb67216e485557ea6b6af75f37096c9aa6a5a75f";
    let high_s = "eeb940b1d03b21e36b0e47e79769f095fe2ab855bd91e3a38756b7d75a9c4549";
    let low_s = "1146bf4e2fc4de1c94f1b81868960f68bc842490f1b6bc98387ba6b57599fbf8";
    let n = "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141";
    let gx = "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798";
    let address = bytes("a94f5374fce5edbc8e2a8697c15331677e6ebf0b")?;
    let zero = "00".repeat(32);
    let (one, five) = (format!("{}01", "00".repeat(31)), format!("{}05", "00".repeat(31)));
    let none = || Expect::Output(Output::Address(None));
    let fixed: [(&str, &str, &str, &str, u8, Expect); 10] = [
        ("valid key", digest, r, low_s, 27, Expect::Output(Output::Address(Some(address.clone())))),
        ("valid key, v = 0", digest, r, low_s, 0, Expect::Output(Output::Address(Some(address)))),
        ("high s", digest, r, high_s, 28, none()),
        ("r zero", digest, &zero, low_s, 27, none()),
        ("s zero", digest, r, &zero, 27, none()),
        ("r = n", digest, n, low_s, 27, none()),
        ("s = n", digest, r, n, 27, none()),
        ("r = 5, no point", digest, &five, low_s, 28, none()),
        // R = G, s = 1, e = 1: s R - e G is the identity
        ("recovers the identity", &one, gx, &one, 27, none()),
        ("v = 29", digest, r, low_s, 29, Expect::AnyFailure),
    ];
    for (name, digest, r, s, v, expect) in fixed {
        let mut signature = bytes(r)?;
        signature.extend(bytes(s)?);
        signature.push(v);
        out.push(named(format!("ecrecover {name}"), Case::EcRecover { digest: bytes(digest)?, signature }, expect));
    }
    Ok(out)
}

/// The canonical value of a vector proposition, as the JLVM holds it.
fn proposition_value(p: &SigmaProposition) -> Result<Value, Error> {
    let ty = match p.r#type {
        SigmaType::Dlog => "dlog",
        SigmaType::Dhtuple => "dhtuple",
        SigmaType::And => "and",
        SigmaType::Or => "or",
        SigmaType::Threshold => "threshold",
    };
    let mut entries = vec![("type".to_owned(), Value::text(ty))];
    for (key, field) in [("pk", &p.pk), ("g", &p.g), ("h", &p.h), ("u", &p.u), ("v", &p.v)] {
        if let Some(hex) = field {
            entries.push((key.to_owned(), Value::bytes(bytes(hex)?)));
        }
    }
    if let Some(k) = p.k {
        entries.push(("k".to_owned(), Value::int(k)));
    }
    if let Some(children) = &p.children {
        let children = children.iter().map(proposition_value).collect::<Result<Vec<_>, _>>()?;
        entries.push(("children".to_owned(), Value::array(children).map_err(|e| format!("{e}"))?));
    }
    Value::map(entries).map_err(|e| format!("{e}"))
}

/// Every case of a sigma vector file (`sigma_v1.json` or `sigma_dilf4s_v1.json`), with Ergo's verdict.
pub fn sigma(file: &str) -> Result<Vec<Named>, Error> {
    let v: SigmaVectors = load(file)?;
    v.cases
        .iter()
        .map(|c| {
            let expect = match (&c.kind, c.ergo) {
                (SigmaKind::Valid, true) => Expect::Output(Output::Bool(true)),
                (SigmaKind::TrailingByte | SigmaKind::Truncated, _) => Expect::Failure("ProofLength"),
                // a flipped response may leave the scalar range: then an error, as `sigma_vectors.rs` allows
                _ => Expect::Rejected,
            };
            let case = Case::Sigma {
                proposition: cbor::encode(&proposition_value(&c.proposition)?),
                proof: bytes(&c.proof)?,
                message: bytes(&c.message)?,
            };
            Ok(named(format!("{file} {} ({:?})", c.name, c.kind), case, expect))
        })
        .collect()
}

fn zk_failure(f: &ZkFailure) -> Expect {
    Expect::Failure(match f.error {
        dilf4s_core::generated::zk_vectors::ZkError::Malformed => "Malformed",
        dilf4s_core::generated::zk_vectors::ZkError::InvalidPoint => "InvalidPoint",
    })
}

fn zk_point(r: &ZkPointResult) -> Result<Expect, Error> {
    Ok(match r {
        ZkPointResult::ZkPoint(p) => Expect::Output(Output::Bytes(bytes(&p.point)?)),
        ZkPointResult::ZkFailure(f) => zk_failure(f),
    })
}

/// Every BN254 addition, multiplication and pairing case and every SP1 Groth16 case of `zk_v1.json`.
pub fn bn254() -> Result<Vec<Named>, Error> {
    let v: ZkVectors = load("zk_v1.json")?;
    let mut out = Vec::new();
    for c in &v.add {
        let case = Case::Bn254Add { a: bytes(&c.a)?, b: bytes(&c.b)? };
        out.push(named(format!("add {}", c.name), case, zk_point(&c.result)?));
    }
    for c in &v.mul {
        let case = Case::Bn254Mul { point: bytes(&c.point)?, scalar: bytes(&c.scalar)? };
        out.push(named(format!("mul {}", c.name), case, zk_point(&c.result)?));
    }
    for c in &v.pairing {
        let pairs = c.pairs.iter().map(|p| Ok((bytes(&p.g1)?, bytes(&p.g2)?))).collect::<Result<Vec<_>, Error>>()?;
        let expect = match &c.result {
            ZkBoolResult::ZkBool(b) => Expect::Output(Output::Bool(b.value)),
            ZkBoolResult::ZkFailure(f) => zk_failure(f),
        };
        out.push(named(format!("pairing {}", c.name), Case::Bn254Pairing { pairs }, expect));
    }
    for c in &v.sp1_groth16 {
        let expect = match c.result {
            ZkGroth16Result::Verified => Expect::Output(Output::Unit),
            ZkGroth16Result::Malformed => Expect::Failure("Malformed"),
            ZkGroth16Result::InvalidProof => Expect::Failure("InvalidProof"),
            ZkGroth16Result::PairingCheckFailed => Expect::Failure("PairingCheckFailed"),
            ZkGroth16Result::Rejected => Expect::Failure("Rejected"),
        };
        let case = Case::Sp1Groth16 {
            program_vkey: bytes(&c.program_vkey)?,
            public_values: bytes(&c.public_values)?,
            proof: bytes(&c.proof_bytes)?,
        };
        out.push(named(format!("sp1Groth16 {}", c.name), case, expect));
    }
    Ok(out)
}

/// Every Poseidon hash of `poseidon_v1.json`, and the root each Merkle proof folds its position's leaf (0 when empty)
/// to.
pub fn poseidon() -> Result<Vec<Named>, Error> {
    let v: PoseidonVectors = load("poseidon_v1.json")?;
    let mut out = Vec::new();
    for c in &v.poseidon {
        let inputs = c.inputs.iter().map(|x| decimal_bytes(x)).collect::<Result<Vec<_>, _>>()?;
        let output = decimal_bytes(&c.output)?;
        out.push(named(
            format!("poseidon {:?}", c.inputs),
            Case::Poseidon { inputs },
            Expect::Output(Output::Bytes(output)),
        ));
    }
    let root = decimal_bytes(&v.merkle.root)?;
    for p in &v.merkle.proofs {
        let position = BigUint::parse_bytes(p.position.as_bytes(), 10).ok_or("a position")?;
        let siblings = p
            .siblings
            .iter()
            .map(|s| {
                let n = BigInt::parse_bytes(s.as_bytes(), 10).ok_or("a sibling")?;
                fr_from_int(&n).map_err(|e| format!("{e}"))
            })
            .collect::<Result<Vec<_>, Error>>()?;
        let proof = PoseidonMerkleProof::new(position, siblings).map_err(|e| format!("{e}"))?;
        let value = proof.to_value().map_err(|e| format!("{e}"))?;
        let leaf = v.merkle.leaves.get(&p.position).map_or("0", String::as_str);
        out.push(named(
            format!("poseidon merkle position {}", p.position),
            Case::PoseidonRoot { leaf: decimal_bytes(leaf)?, proof: cbor::encode(&value) },
            Expect::Output(Output::Bytes(root.clone())),
        ));
    }
    Ok(out)
}

/// The `vrf_v1.json` cases of one suite (by `SuiteId::name`): every committed proof, with its `beta`, then, when
/// `rejections` is set, every rejection row with the refusal it names (or any output, for its valid row).
pub fn vrf(suite: &str, rejections: bool) -> Result<Vec<Named>, Error> {
    let v: VrfVectors = load("vrf_v1.json")?;
    let mut out = Vec::new();
    for s in v.suites.iter().filter(|s| s.suite == suite) {
        for (k, key) in s.keys.iter().enumerate() {
            for (i, p) in key.proofs.iter().enumerate() {
                let case = Case::Vrf {
                    suite: suite.to_owned(),
                    key: bytes(&key.verifying_key)?,
                    alpha: bytes(&p.alpha)?,
                    proof: bytes(&p.pi)?,
                };
                out.push(named(
                    format!("vrf {suite} key {k} proof {i}"),
                    case,
                    Expect::Output(Output::Bytes(bytes(&p.beta)?)),
                ));
            }
        }
    }
    for r in v.rejections.iter().filter(|r| rejections && r.suite == suite) {
        let expect = if r.result == "valid" { Expect::Verified } else { Expect::FailureNamed(r.result.clone()) };
        let case = Case::Vrf {
            suite: suite.to_owned(),
            key: bytes(&r.verifying_key)?,
            alpha: bytes(&r.alpha)?,
            proof: bytes(&r.pi)?,
        };
        out.push(named(format!("vrf {suite} rejection {}", r.name), case, expect));
    }
    Ok(out)
}

/// The JLVM cases `keep` picks (by index and case), run as the conformance vectors run them.
pub fn jlvm(
    keep: impl Fn(usize, &dilf4s_core::generated::jlvm_vectors::JlvmCase) -> bool,
) -> Result<Vec<Named>, Error> {
    let v: JlvmVectors = load("jlvm_v1.json")?;
    v.cases
        .iter()
        .enumerate()
        .filter(|(i, c)| keep(*i, c))
        .map(|(_, c)| {
            let result = match &c.outcome {
                JlvmOutcome::JlvmValue(x) => Ok(bytes(&x.cbor)?),
                JlvmOutcome::JlvmError(e) => Err(format!("{:?}", e.error)),
            };
            let case = Case::Jlvm {
                program: c.program.clone(),
                data: c.data.clone(),
                env: c.env.iter().flatten().map(|(k, x)| (k.clone(), x.clone())).collect(),
                gas_limit: c.gas_limit.unwrap_or(v.gas_limit),
            };
            Ok(named(format!("jlvm {}", c.name), case, Expect::Jlvm { result, gas: c.gas_used, warnings: c.warnings }))
        })
        .collect()
}

fn hex_point(p: Option<&String>) -> Result<Vec<u8>, Error> {
    bytes(p.ok_or("a missing point")?)
}

/// Issue #33's leaves: from each valid single-leaf proof of both sigma vector files, its challenge and response with
/// its points, split into generator leaves (dlog) and the rest (each DH tuple gives two).
pub fn leaves() -> Result<(Vec<Leaf>, Vec<Leaf>), Error> {
    const CHALLENGE: usize = 24;
    let files = [load::<SigmaVectors>("sigma_v1.json")?, load::<SigmaVectors>("sigma_dilf4s_v1.json")?];
    let mut generator = Vec::new();
    let mut other = Vec::new();
    for c in files.iter().flat_map(|v| &v.cases).filter(|c| c.kind == SigmaKind::Valid && c.ergo) {
        let proof = bytes(&c.proof)?;
        let (challenge, response) = (proof.get(..CHALLENGE), proof.get(CHALLENGE..));
        let (Some(challenge), Some(response)) = (challenge, response) else { continue };
        if response.len() != 32 {
            continue;
        }
        let leaf = |base: Option<Vec<u8>>, point: Vec<u8>| Leaf {
            base,
            point,
            challenge: challenge.to_vec(),
            response: response.to_vec(),
        };
        let p = &c.proposition;
        match p.r#type {
            SigmaType::Dlog => generator.push(leaf(None, hex_point(p.pk.as_ref())?)),
            SigmaType::Dhtuple => {
                other.push(leaf(Some(hex_point(p.g.as_ref())?), hex_point(p.u.as_ref())?));
                other.push(leaf(Some(hex_point(p.h.as_ref())?), hex_point(p.v.as_ref())?));
            }
            SigmaType::And | SigmaType::Or | SigmaType::Threshold => {}
        }
    }
    Ok((generator, other))
}
