//! The cases, read from `shared/vectors`, each with what its vector says the outcome is. Off the guest only.

extern crate std;

use alloc::borrow::ToOwned as _;
use alloc::collections::{BTreeMap, BTreeSet};
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use alloc::{format, vec};
use std::path::PathBuf;

use dilf4s_core::codec::Model;
use dilf4s_core::generated::jlvm::{JlvmExpr, JlvmOp, JlvmProgram};
use dilf4s_core::generated::jlvm_errors::JlvmSite;
use dilf4s_core::generated::jlvm_vectors::{JlvmCase, JlvmCause, JlvmErrorKind, JlvmOutcome, JlvmVectors};
use dilf4s_core::generated::kes_vectors::{
    KesCheck, KesConstruction, KesOutcome, KesParameters, KesRefusal, KesVectors,
};
use dilf4s_core::generated::poseidon_vectors::PoseidonVectors;
use dilf4s_core::generated::sigma_refusal_vectors::{SigmaOutcome, SigmaRefusalVectors};
use dilf4s_core::generated::sigma_vectors::{SigmaKind, SigmaProposition, SigmaType, SigmaVectors};
use dilf4s_core::generated::signing_vectors::{SigningVectors, VerificationResult};
use dilf4s_core::generated::vrf_vectors::{VrfResult, VrfVectors};
use dilf4s_core::generated::zk_vectors::{ZkBoolResult, ZkFailure, ZkGroth16Result, ZkPointResult, ZkVectors};
use dilf4s_core::hash::{Hasher as _, Sha256};
use dilf4s_core::{Value, cbor, hex, json};
use dilf4s_proofs::poseidon::merkle::PoseidonMerkleProof;
use dilf4s_proofs::poseidon::{fr_from_int, fr_to_be_bytes};
use dilf4s_signing::ecrecover::address_of_key;
use num_bigint::{BigInt, BigUint};

use crate::leaves::Leaf;
use crate::{Case, KesScheme, Outcome, Output, Recipe, RecipeStep};

/// A failure reading the vectors.
pub type Error = String;

/// What the vector says a case gives.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expect {
    /// Exactly this output.
    Output(Output),
    /// An error of exactly this kind: its `Debug` rendering is the kind's variant name, alone or before its fields.
    Kind(String),
    /// Any error: only the fixed `ecrecover` refusal of an invalid `v`, which no vector file records.
    AnyFailure,
    /// Some bytes: a verified output the vector does not give.
    Verified,
    /// A JLVM outcome: the value's canonical CBOR or the error, the gas and the number of warnings.
    Jlvm {
        /// The canonical CBOR, or the error's kind, normative `op` and normative `cause` (`Family.Kind`).
        result: Result<Vec<u8>, (String, Option<String>, Option<String>)>,
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
            (Self::Kind(name), Err(e)) => {
                e.strip_prefix(name.as_str()).is_some_and(|rest| rest.is_empty() || rest.starts_with(' '))
            }
            (Self::AnyFailure, Err(_))
            | (Self::Verified, Ok(Output::Bytes(_)))
            | (Self::SignerParity { .. }, Ok(Output::Address(_))) => true,
            (Self::Jlvm { result, gas, warnings }, Ok(Output::Jlvm { result: got, gas: g, warnings: w })) => {
                let got = got.clone().map_err(|(kind, op, cause, _)| (kind, op, cause));
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
        let expect = match &c.result {
            VerificationResult::Valid => Expect::Output(Output::Bool(true)),
            VerificationResult::Invalid => Expect::Output(Output::Bool(false)),
            // the guest verifies the pair at once, so the refused part shows only through the kind
            VerificationResult::VerificationRefusal(r) => Expect::Kind(format!("{:?}", r.kind)),
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

/// What a sigma outcome of `sigma_refusals_v1.json` says a case gives.
fn sigma_expect(o: SigmaOutcome) -> Expect {
    match o {
        SigmaOutcome::Accepted => Expect::Output(Output::Bool(true)),
        SigmaOutcome::Rejected => Expect::Output(Output::Bool(false)),
        SigmaOutcome::SigmaErrorKind(kind) => Expect::Kind(format!("{kind:?}")),
    }
}

/// Every case of a sigma vector file (`sigma_v1.json` or `sigma_dilf4s_v1.json`), with the reference's outcome on it,
/// by kind, from `sigma_refusals_v1.json` (the file itself carries Ergo's verdict).
pub fn sigma(file: &str) -> Result<Vec<Named>, Error> {
    let v: SigmaVectors = load(file)?;
    let r: SigmaRefusalVectors = load("sigma_refusals_v1.json")?;
    let outcomes = match file {
        "sigma_v1.json" => &r.ergo_cases,
        "sigma_dilf4s_v1.json" => &r.dilf4s_cases,
        other => return Err(format!("no outcomes for {other}")),
    };
    if outcomes.len() != v.cases.len() {
        return Err(format!("{file}: {} cases, {} outcomes", v.cases.len(), outcomes.len()));
    }
    v.cases
        .iter()
        .zip(outcomes)
        .map(|(c, outcome)| {
            let expect = sigma_expect(*outcome);
            let case = Case::Sigma {
                proposition: cbor::encode(&proposition_value(&c.proposition)?),
                proof: bytes(&c.proof)?,
                message: bytes(&c.message)?,
            };
            Ok(named(format!("{file} {} ({:?})", c.name, c.kind), case, expect))
        })
        .collect()
}

/// The guest's form of a KES construction and its parameters.
fn kes_scheme(construction: KesConstruction, parameters: &KesParameters) -> Result<KesScheme, Error> {
    match (construction, parameters) {
        (KesConstruction::Dilf4sSum, KesParameters::KesSumParameters(p)) => {
            Ok(KesScheme::Sum { hash: p.hash.clone(), height: p.height })
        }
        (KesConstruction::Dilf4sProduct, KesParameters::KesProductParameters(p)) => {
            Ok(KesScheme::Product { hash: p.hash.clone(), super_height: p.super_height, sub_height: p.sub_height })
        }
        (KesConstruction::CardanoSum, KesParameters::KesCardanoParameters(p)) => {
            Ok(KesScheme::Cardano { depth: p.depth })
        }
        other => Err(format!("unexpected KES construction {other:?}")),
    }
}

/// Every period's signature of `kes_v1.json`, and every `verify` row and refused `parameters` row of its refusals, by
/// kind (the guest has no key generation, loading or evolution: those need the `sign` feature).
pub fn kes() -> Result<Vec<Named>, Error> {
    let v: KesVectors = load("kes_v1.json")?;
    let mut out = Vec::new();
    for (i, vector) in v.vectors.iter().enumerate() {
        let scheme = kes_scheme(vector.construction, &vector.parameters)?;
        for p in &vector.periods {
            let case = Case::KesVerify {
                scheme: scheme.clone(),
                key: bytes(&vector.verifying_key)?,
                period: p.period,
                message: bytes(&vector.message)?,
                signature: bytes(&p.signature)?,
            };
            out.push(named(format!("kes {i} period {}", p.period), case, Expect::Output(Output::Bool(true))));
        }
    }
    let in_guest = |c: &&KesRefusal| match c.check {
        KesCheck::Verify => true,
        KesCheck::Parameters => c.outcome != KesOutcome::Accepted,
        KesCheck::Generate | KesCheck::Load | KesCheck::Evolve => false,
    };
    for c in v.refusals.iter().filter(in_guest) {
        let expect = match &c.outcome {
            KesOutcome::Accepted => Expect::Output(Output::Bool(true)),
            KesOutcome::Rejected => Expect::Output(Output::Bool(false)),
            KesOutcome::KesErrorKind(kind) => Expect::Kind(format!("{kind:?}")),
        };
        let field = |f: &Option<String>| bytes(f.as_deref().unwrap_or_default());
        let case = Case::KesVerify {
            scheme: kes_scheme(c.construction, &c.parameters)?,
            key: field(&c.verifying_key)?,
            period: c.period.unwrap_or_default(),
            message: field(&c.message)?,
            signature: field(&c.signature)?,
        };
        out.push(named(format!("kes refusal {:?} {:?} {}", c.construction, c.check, c.name), case, expect));
    }
    Ok(out)
}

/// Every call of `sigma_refusals_v1.json`: the proposition's canonical CBOR, its proof and message, by kind.
pub fn sigma_refusals() -> Result<Vec<Named>, Error> {
    let v: SigmaRefusalVectors = load("sigma_refusals_v1.json")?;
    v.cases
        .iter()
        .map(|c| {
            let case = Case::Sigma {
                proposition: bytes(&c.proposition)?,
                proof: bytes(&c.proof)?,
                message: bytes(&c.message)?,
            };
            Ok(named(format!("sigma refusal {}", c.name), case, sigma_expect(c.outcome)))
        })
        .collect()
}

fn zk_failure(f: &ZkFailure) -> Expect {
    Expect::Kind(format!("{:?}", f.error))
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
        let expect = match &c.result {
            ZkGroth16Result::Verified => Expect::Output(Output::Unit),
            ZkGroth16Result::Groth16ErrorKind(kind) => Expect::Kind(format!("{kind:?}")),
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
        let expect = match &r.result {
            VrfResult::Valid => Expect::Verified,
            VrfResult::VrfErrorKind(kind) => Expect::Kind(format!("{kind:?}")),
        };
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

/// The index of the first case with each error kind of the schema, of the first whose `op` is `var`, and of the first
/// with each recorded `cause`: so every kind, both kinds of site and every cause run wherever the picked cases do. A kind with no row is an error (the coverage gate,
/// conformance.md section 6).
fn first_errors(v: &JlvmVectors) -> Result<Vec<usize>, Error> {
    let error = |c: &JlvmCase| match &c.outcome {
        JlvmOutcome::JlvmError(e) => Some(e.clone()),
        JlvmOutcome::JlvmValue(_) => None,
    };
    let mut firsts = JlvmErrorKind::ALL
        .iter()
        .map(|k| {
            v.cases
                .iter()
                .position(|c| error(c).is_some_and(|e| e.error == *k))
                .ok_or_else(|| format!("jlvm_v1.json: no row has error {k:?}"))
        })
        .collect::<Result<Vec<_>, Error>>()?;
    let var = v
        .cases
        .iter()
        .position(|c| error(c).is_some_and(|e| e.op == Some(JlvmSite::Var)))
        .ok_or_else(|| String::from("jlvm_v1.json: no row has op var"))?;
    firsts.push(var);
    // and the first row of each cause a Crypto error records, so every pinned cause runs in the guest too
    let mut causes: Vec<String> = Vec::new();
    for (i, c) in v.cases.iter().enumerate() {
        if let Some(name) = error(c).and_then(|e| e.cause).map(cause)
            && !causes.contains(&name)
        {
            causes.push(name);
            firsts.push(i);
        }
    }
    Ok(firsts)
}

/// A row's cause as `Family.Kind`, as the guest renders an error's.
fn cause(c: JlvmCause) -> String {
    match c {
        JlvmCause::JlvmSigningCause(c) => format!("Signing.{}", c.signing),
        JlvmCause::JlvmSigmaCause(c) => format!("Sigma.{}", c.sigma),
        JlvmCause::JlvmZkCause(c) => format!("Zk.{}", c.zk),
        JlvmCause::JlvmGroth16Cause(c) => format!("Groth16.{}", c.groth16),
        JlvmCause::JlvmVrfCause(c) => format!("Vrf.{}", c.vrf),
        JlvmCause::JlvmHashCause(c) => format!("Hash.{}", c.hash),
        JlvmCause::JlvmMerkleCause(c) => format!("Merkle.{}", c.merkle),
        JlvmCause::JlvmTrieCause(c) => format!("Trie.{}", c.trie),
        JlvmCause::JlvmPoseidonCause(c) => format!("Poseidon.{}", c.poseidon),
        JlvmCause::JlvmPoseidonMerkleCause(c) => format!("PoseidonMerkle.{}", c.poseidon_merkle),
    }
}

/// The operation at the root of a program, read from its canonical form (the generated `jlvm-expr`): the surface
/// parsed as the JLVM parses it, so an alias (`getKey`, `size`, ...) is the operation it names, and no text is matched.
/// `None` for text that is not a program, or whose root is not an operation.
fn root_op(program: &str) -> Option<JlvmOp> {
    let form = dilf4s_jlvm::parse(program).ok()?.canonical().ok()?;
    match JlvmProgram::from_value(&form).ok()?.expr {
        JlvmExpr::JlvmApply(a) => Some(a.op),
        JlvmExpr::JlvmSpread(s) => Some(s.spread),
        _ => None,
    }
}

/// What a `jlvm_v1.json` row shows of one operation, read exactly: from the typed site of the row's error, or from the
/// operation at the root of its program and the value it returned. A row shows nothing of an operation called deeper
/// down, which may not have run (a branch not taken), nor of one whose name is only text in the row.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Shows {
    /// The root operation returned a value that is not a negative verdict.
    Value,
    /// The root operation returned a negative verdict: `false`, or `ecvrf_verify`'s `{"valid": false}`.
    False,
    /// The operation refused: an error of this kind, with this normative cause (`Family.Kind`), arose at its site.
    Refusal(String, Option<String>),
}

/// Whether a value's canonical CBOR is a negative verdict ([`Shows::False`]).
fn negative_verdict(value: &[u8]) -> bool {
    ["false", r#"{"valid": false}"#].iter().any(|text| json::parse(text).is_ok_and(|v| cbor::encode(&v) == value))
}

/// The operation a row shows something of, and what ([`Shows`]). `None` for an error at no operation's site (out of
/// gas, a limit, malformed text, a `var` read) and for a value whose program's root is no operation.
pub fn shows(c: &JlvmCase) -> Option<(JlvmOp, Shows)> {
    match &c.outcome {
        JlvmOutcome::JlvmError(e) => match e.op {
            Some(JlvmSite::JlvmOp(op)) => Some((op, Shows::Refusal(e.error.to_string(), e.cause.clone().map(cause)))),
            Some(JlvmSite::Var) | None => None,
        },
        JlvmOutcome::JlvmValue(x) => {
            let op = root_op(&c.program)?;
            let value = bytes(&x.cbor).ok()?;
            Some((op, if negative_verdict(&value) { Shows::False } else { Shows::Value }))
        }
    }
}

/// Whether a row is one of `OpCoverageCases`' sweeps (each operation over 0 to 3 arguments of each kind) rather than a
/// row with real inputs.
fn sweep(c: &JlvmCase) -> bool {
    c.name.contains(" × ") || c.name.ends_with(": no arguments")
}

/// A `jlvm_v1.json` row the guest runs.
#[derive(Debug, Clone)]
pub struct JlvmRow {
    /// What it shows, of which operation ([`shows`]).
    pub shows: Option<(JlvmOp, Shows)>,
    /// The row, run as the conformance vectors run it, with what its vector says.
    pub named: Named,
}

/// The `jlvm_v1.json` rows the SP1 guest runs ([`jlvm`]), and what they show of each operation.
#[derive(Debug, Clone)]
pub struct JlvmSample {
    /// The rows, in file order.
    pub rows: Vec<JlvmRow>,
    /// What the rows show of each operation, and how many show it.
    pub run: BTreeMap<JlvmOp, BTreeMap<Shows, usize>>,
    /// What the whole file shows of each operation, and how many rows show it.
    pub file: BTreeMap<JlvmOp, BTreeMap<Shows, usize>>,
}

fn tally<'a>(rows: impl Iterator<Item = &'a (JlvmOp, Shows)>) -> BTreeMap<JlvmOp, BTreeMap<Shows, usize>> {
    let mut out: BTreeMap<JlvmOp, BTreeMap<Shows, usize>> = BTreeMap::new();
    for (op, s) in rows {
        let n = out.entry(*op).or_default().entry(s.clone()).or_default();
        *n = n.saturating_add(1);
    }
    out
}

/// Rows the guest runs of each operation and each thing a row shows of it ([`Shows`]), lightest first ([`Rows`]).
pub const PER_SHOWN: usize = 2;

/// The rows that show one thing of one operation, each in file order: those with real inputs, those with host-built
/// values (tests of the JLVM's bounds, the heaviest rows to run: one builds 508,399 digests), and the sweeps.
#[derive(Default)]
struct Rows {
    real: Vec<usize>,
    built: Vec<usize>,
    swept: Vec<usize>,
}

/// The JLVM rows the guest runs, chosen by operation from the closed list ([`JlvmOp`]), deterministically:
/// - for each operation and each thing a row shows of it ([`Shows`]: a value, a negative verdict, each kind and cause
///   of refusal), the first [`PER_SHOWN`] rows: those with real inputs, then those with host-built values, then the
///   sweeps;
/// - and of those with real inputs, the first `real(op)`;
/// - every 50th row, whatever it calls;
/// - the first row of each error kind, of the `var` site and of each cause ([`first_errors`]).
pub fn jlvm(real: impl Fn(JlvmOp) -> usize) -> Result<JlvmSample, Error> {
    let v: JlvmVectors = load("jlvm_v1.json")?;
    let shown: Vec<Option<(JlvmOp, Shows)>> = v.cases.iter().map(shows).collect();
    let mut groups: BTreeMap<&(JlvmOp, Shows), Rows> = BTreeMap::new();
    for (i, (c, s)) in v.cases.iter().zip(&shown).enumerate() {
        if let Some(key) = s {
            let rows = groups.entry(key).or_default();
            let bucket = match (sweep(c), c.built.as_ref().is_some_and(|b| !b.is_empty())) {
                (true, _) => &mut rows.swept,
                (false, true) => &mut rows.built,
                (false, false) => &mut rows.real,
            };
            bucket.push(i);
        }
    }
    let mut picked: BTreeSet<usize> = groups
        .iter()
        .flat_map(|((op, _), g)| {
            let lightest = g.real.iter().chain(&g.built).chain(&g.swept).take(PER_SHOWN);
            lightest.chain(g.real.iter().take(real(*op)))
        })
        .copied()
        .collect();
    picked.extend((0..v.cases.len()).step_by(50));
    picked.extend(first_errors(&v)?);
    let rows = picked
        .iter()
        .map(|&i| {
            let c = v.cases.get(i).ok_or_else(|| format!("jlvm_v1.json: no row {i}"))?;
            Ok(JlvmRow { shows: shown.get(i).cloned().flatten(), named: jlvm_named(&v, c)? })
        })
        .collect::<Result<Vec<_>, Error>>()?;
    Ok(JlvmSample {
        run: tally(rows.iter().filter_map(|r| r.shows.as_ref())),
        file: tally(shown.iter().flatten()),
        rows,
    })
}

/// A row, run as the conformance vectors run it, with what its vector says.
fn jlvm_named(v: &JlvmVectors, c: &JlvmCase) -> Result<Named, Error> {
    let result = match &c.outcome {
        JlvmOutcome::JlvmValue(x) => Ok(bytes(&x.cbor)?),
        JlvmOutcome::JlvmError(e) => {
            Err((e.error.to_string(), e.op.map(|s| s.to_string()), e.cause.clone().map(cause)))
        }
    };
    let case = Case::Jlvm {
        program: c.program.clone(),
        data: c.data.clone(),
        env: c.env.iter().flatten().map(|(k, x)| (k.clone(), x.clone())).collect(),
        built: c.built.iter().flatten().map(|(k, b)| Ok((k.clone(), recipe(b)?))).collect::<Result<_, Error>>()?,
        gas_limit: c.gas_limit.unwrap_or(v.gas_limit),
    };
    Ok(named(format!("jlvm {}", c.name), case, Expect::Jlvm { result, gas: c.gas_used, warnings: c.warnings }))
}

/// A `built` binding's recipe, its leaf and templates decoded from hex: built where the case runs.
fn recipe(b: &dilf4s_core::generated::jlvm_vectors::BuiltValue) -> Result<Recipe, Error> {
    use dilf4s_core::generated::jlvm_vectors::BuiltStep;
    let steps = b
        .steps
        .iter()
        .flatten()
        .map(|s| match s {
            BuiltStep::BuiltWrap(w) => Ok(RecipeStep::Wrap { template: bytes(&w.template)?, times: w.times }),
            BuiltStep::BuiltCopies(c) => Ok(RecipeStep::Copies(c.copies)),
        })
        .collect::<Result<Vec<_>, Error>>()?;
    Ok(Recipe { leaf: bytes(&b.leaf)?, steps })
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
