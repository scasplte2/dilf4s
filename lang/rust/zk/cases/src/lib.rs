//! What the SP1 guest computes over the shared vectors (`shared/vectors`), and what the host computes natively to
//! compare it with.
//!
//! The guest and the host run this same code. The guest builds it for `riscv64im-succinct-zkvm-elf` with the SP1
//! patches, so the cryptography goes through their `target_os = "zkvm"` paths (precompiles and hints). The host builds
//! it with the crates.io releases the SDK ships. Every outcome is the full result, its error rendered with `Debug`, so
//! any difference between the two shows.

#![no_std]

extern crate alloc;

pub mod call;
pub mod leaves;
#[cfg(feature = "vectors")]
pub mod vectors;

use alloc::collections::BTreeMap;
use alloc::format;
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::cell::OnceCell;
use core::fmt::Debug;

use dilf4s_core::hash::HashAlgorithm;
use dilf4s_core::{built, cbor};
use dilf4s_jlvm::{Config, ErrorSite, Gas, GasLimit, GasSchedule, Jlvm, JlvmError, Native, parse, parse_value};
use dilf4s_proofs::poseidon::merkle::{PoseidonMerkleProof, compute_root};
use dilf4s_proofs::poseidon::{self, Fr, fr_from_be_bytes, fr_to_be_bytes};
use dilf4s_proofs::zk::{self, groth16};
use dilf4s_signing::vrf::SuiteId;
use dilf4s_signing::{SchemeId, bls, ecrecover, kes, sigma};
use serde::{Deserialize, Serialize};

use crate::leaves::Leaf;

/// One computation over vector inputs.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Case {
    /// A signature checked by `SchemeId::verify_bytes`, the scheme by its name.
    Verify {
        /// The scheme's name, as `SchemeId::name` gives it.
        scheme: String,
        /// The verifying key.
        key: Vec<u8>,
        /// The message.
        message: Vec<u8>,
        /// The signature.
        signature: Vec<u8>,
    },
    /// A BLS proof of possession.
    BlsPop {
        /// The verifying key.
        key: Vec<u8>,
        /// The proof.
        proof: Vec<u8>,
    },
    /// Ethereum signer recovery.
    EcRecover {
        /// The 32-byte digest.
        digest: Vec<u8>,
        /// `r || s || v`.
        signature: Vec<u8>,
    },
    /// A sigma proof, checked through the JLVM path: the proposition's canonical value, then `verify_value`.
    Sigma {
        /// The proposition's canonical value, as canonical CBOR.
        proposition: Vec<u8>,
        /// The proof.
        proof: Vec<u8>,
        /// The message.
        message: Vec<u8>,
    },
    /// EIP-196 addition.
    Bn254Add {
        /// The first point.
        a: Vec<u8>,
        /// The second point.
        b: Vec<u8>,
    },
    /// EIP-196 scalar multiplication.
    Bn254Mul {
        /// The point.
        point: Vec<u8>,
        /// The scalar.
        scalar: Vec<u8>,
    },
    /// The EIP-197 pairing check.
    Bn254Pairing {
        /// The (G1, G2) pairs.
        pairs: Vec<(Vec<u8>, Vec<u8>)>,
    },
    /// An SP1 Groth16 proof, checked against SP1's verifying key.
    Sp1Groth16 {
        /// The program's verifying key hash.
        program_vkey: Vec<u8>,
        /// The public values.
        public_values: Vec<u8>,
        /// The proof bytes.
        proof: Vec<u8>,
    },
    /// Poseidon of 1 to 4 field elements (big-endian).
    Poseidon {
        /// The inputs.
        inputs: Vec<Vec<u8>>,
    },
    /// The root a Poseidon Merkle proof folds a leaf to.
    PoseidonRoot {
        /// The leaf (big-endian).
        leaf: Vec<u8>,
        /// The proof's canonical value, as canonical CBOR.
        proof: Vec<u8>,
    },
    /// ECVRF verification, the suite by its name (`SuiteId::name`): the output `beta` or the refusal.
    Vrf {
        /// The suite's name.
        suite: String,
        /// The verifying key.
        key: Vec<u8>,
        /// The input `alpha`.
        alpha: Vec<u8>,
        /// The proof `pi`.
        proof: Vec<u8>,
    },
    /// KES verification: the construction from its parameters (a refusal there is the outcome), then the signature.
    KesVerify {
        /// The construction and its parameters.
        scheme: KesScheme,
        /// The verifying key.
        key: Vec<u8>,
        /// The period.
        period: u64,
        /// The message.
        message: Vec<u8>,
        /// The signature.
        signature: Vec<u8>,
    },
    /// A JLVM evaluation, as the conformance vectors run it.
    Jlvm {
        /// The program text.
        program: String,
        /// The data text.
        data: String,
        /// The host bindings' texts.
        env: Vec<(String, String)>,
        /// The host bindings built with shared parts (`built` in jlvm-vectors.cddl), built where the case runs.
        built: Vec<(String, Recipe)>,
        /// The gas limit.
        gas_limit: u64,
    },
}

/// A KES construction and its parameters, as `kes-vectors.cddl` gives them.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum KesScheme {
    /// The dilf4s sum composition.
    Sum {
        /// The hash, by name.
        hash: String,
        /// The height.
        height: u64,
    },
    /// The dilf4s product composition.
    Product {
        /// The hash, by name.
        hash: String,
        /// The super tree's height.
        super_height: u64,
        /// The sub trees' height.
        sub_height: u64,
    },
    /// Cardano's `SumKES`.
    Cardano {
        /// The depth.
        depth: u64,
    },
}

/// A KES height as the SDK takes it: one past `u32` is past every limit, so it is refused as such.
fn kes_height(h: u64) -> u32 {
    u32::try_from(h).unwrap_or(u32::MAX)
}

fn kes_verify(scheme: &KesScheme, key: &[u8], period: u64, message: &[u8], signature: &[u8]) -> Outcome {
    let hash = |name: &str| HashAlgorithm::from_name(name).map_err(debug);
    match scheme {
        KesScheme::Sum { hash: h, height } => kes::KesSum::new(hash(h)?, kes_height(*height))
            .and_then(|k| k.verify(key, period, message, signature))
            .map_err(debug),
        KesScheme::Product { hash: h, super_height, sub_height } => {
            kes::KesProduct::new(hash(h)?, kes_height(*super_height), kes_height(*sub_height))
                .and_then(|k| k.verify(key, period, message, signature))
                .map_err(debug)
        }
        KesScheme::Cardano { depth } => kes::CardanoSumKes::new(kes_height(*depth))
            .and_then(|k| k.verify(key, period, message, signature))
            .map_err(debug),
    }
    .map(Output::Bool)
}

/// A `built-value` recipe, its leaf and templates as canonical CBOR: built by `dilf4s_core::built::build` on whichever
/// side runs the case, so the guest and the native run build the same value the same way.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Recipe {
    /// The leaf.
    pub leaf: Vec<u8>,
    /// The steps, in order.
    pub steps: Vec<RecipeStep>,
}

/// A step of a [`Recipe`].
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum RecipeStep {
    /// Replace every text `"$"` in the template by the value so far, `times` times over.
    Wrap {
        /// The template.
        template: Vec<u8>,
        /// How many times.
        times: u64,
    },
    /// An array of `n` of the value so far.
    Copies(u64),
}

impl Recipe {
    /// The value it describes.
    pub fn build(&self) -> Result<dilf4s_jlvm::Value, String> {
        let steps: Vec<built::Step<'_>> = self
            .steps
            .iter()
            .map(|s| match s {
                RecipeStep::Wrap { template, times } => built::Step::Wrap { template, times: *times },
                RecipeStep::Copies(n) => built::Step::Copies(*n),
            })
            .collect();
        built::build(&self.leaf, &steps).map_err(debug)
    }
}

/// What a case gives when it does not fail.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Output {
    /// A verdict.
    Bool(bool),
    /// Bytes: a point, a field element, a digest.
    Bytes(Vec<u8>),
    /// A recovered address, or none.
    Address(Option<Vec<u8>>),
    /// Success with nothing to return.
    Unit,
    /// A JLVM outcome: the value's canonical CBOR or the error, the gas and the ordered warning contents.
    Jlvm {
        /// The value's canonical CBOR, or the error's kind, its normative `op` and `cause` (jlvm.md section 3, the
        /// cause as `Family.Kind`) and its `Debug` rendering.
        result: Result<Vec<u8>, JlvmFailure>,
        /// The gas used.
        gas: u64,
        /// The warnings, including their full preview contents, in order.
        warnings: Vec<Warning>,
    },
}

/// An ordered JLVM warning with every normative preview field, committed by the conformance guest.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Warning {
    /// A plain path in a callback also names a key in an enclosing frame or the data.
    AmbiguousPath {
        /// The first 32 Unicode code points of the path.
        head: String,
        /// The full path's UTF-8 length.
        length: u64,
        /// Whether the full path extends beyond the preview.
        truncated: bool,
    },
}

impl From<dilf4s_jlvm::Warning> for Warning {
    fn from(warning: dilf4s_jlvm::Warning) -> Self {
        match warning {
            dilf4s_jlvm::Warning::AmbiguousPath(path) => Self::AmbiguousPath {
                head: path.head().to_string(),
                length: u64::try_from(path.length()).unwrap_or(u64::MAX),
                truncated: path.is_truncated(),
            },
        }
    }
}

/// A JLVM error as a case reports it: its kind, its `op`, its `cause` (`Family.Kind`) and its `Debug` rendering.
pub type JlvmFailure = (String, Option<String>, Option<String>, String);

/// A case's outcome: its output, or its error rendered with `Debug`.
pub type Outcome = Result<Output, String>;

/// What the guest is asked.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Request {
    /// Run these cases.
    Run(Vec<Case>),
    /// Issue #33: each leaf's commitment in one form, or none (the baseline): [`leaves::commit`].
    Leaves {
        /// The form, or none.
        form: Option<leaves::Form>,
        /// The leaves.
        leaves: Vec<Leaf>,
    },
}

/// What the guest commits.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Response {
    /// Whether the code ran with `target_os = "zkvm"`: the patches' zkVM paths.
    pub zkvm: bool,
    /// The answer.
    pub body: Body,
}

/// The answer to a request.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Body {
    /// Each case's outcome, in order.
    Run(Vec<Outcome>),
    /// What [`leaves::commit`] gives.
    Leaves(Result<Vec<Vec<u8>>, String>),
}

/// A `Crypto` error's normative cause as `Family.Kind` (`Sigma.InvalidPoint`), as [`vectors`] renders a row's.
fn cause(e: &JlvmError) -> Option<String> {
    let JlvmError::Crypto { cause: Some(c), .. } = e else { return None };
    let kind = match c {
        dilf4s_jlvm::CryptoCause::Signing { error } => error.kind().to_string(),
        dilf4s_jlvm::CryptoCause::Sigma { error } => error.kind().to_string(),
        dilf4s_jlvm::CryptoCause::Zk { error } => error.kind().to_string(),
        dilf4s_jlvm::CryptoCause::Groth16 { error } => error.kind().to_string(),
        dilf4s_jlvm::CryptoCause::Vrf { error } => error.kind().to_string(),
        dilf4s_jlvm::CryptoCause::Hash { error } => error.kind().to_string(),
        dilf4s_jlvm::CryptoCause::Merkle { error } => error.kind().to_string(),
        dilf4s_jlvm::CryptoCause::Trie { error } => error.kind().to_string(),
        dilf4s_jlvm::CryptoCause::Poseidon { error } => error.kind().to_string(),
        dilf4s_jlvm::CryptoCause::PoseidonMerkle { error } => error.kind().to_string(),
    };
    Some(format!("{}.{kind}", c.kind()))
}

fn debug(e: impl Debug) -> String {
    format!("{e:?}")
}

fn field(bytes: &[u8]) -> Result<Fr, String> {
    fr_from_be_bytes(bytes).map_err(debug)
}

type Inputs = (dilf4s_jlvm::Program, dilf4s_jlvm::Value, BTreeMap<String, dilf4s_jlvm::Value>);

/// The reference's order: the first parse failure (program, then data, then bindings) is the outcome, with no gas.
fn parse_inputs(program: &str, data: &str, env: &[(String, String)]) -> Result<Inputs, JlvmError> {
    let program = parse(program)?;
    let data = parse_value(data)?;
    let env = env.iter().map(|(k, v)| parse_value(v).map(|x| (k.clone(), x))).collect::<Result<BTreeMap<_, _>, _>>()?;
    Ok((program, data, env))
}

fn jlvm_case(
    jlvm: &Result<Jlvm<Native>, String>,
    program: &str,
    data: &str,
    env: &[(String, String)],
    built: &[(String, Recipe)],
    gas_limit: u64,
) -> Outcome {
    let jlvm = jlvm.as_ref().map_err(String::clone)?;
    // a recipe that does not build is a broken vector, not an outcome
    let built = built.iter().map(|(k, r)| r.build().map(|v| (k.clone(), v))).collect::<Result<Vec<_>, _>>()?;
    let (result, gas, warnings) = match parse_inputs(program, data, env) {
        Err(e) => (Err(e), Gas::ZERO, Vec::new()),
        Ok((program, data, mut env)) => {
            env.extend(built);
            let config = Config { limit: GasLimit(Gas::new(gas_limit)), schedule: GasSchedule::LATEST };
            let o = jlvm.evaluate(&program, &data, &env, &config);
            (o.result, o.gas_used, o.warnings.into_iter().map(Warning::from).collect())
        }
    };
    Ok(Output::Jlvm {
        result: result
            .map(|v| cbor::encode(&v))
            .map_err(|e| (e.kind().to_string(), e.site().map(|s| s.to_string()), cause(&e), debug(&e))),
        gas: gas.value(),
        warnings,
    })
}

fn groth16_case(
    key: &Result<groth16::VerifyingKey, String>,
    program_vkey: &[u8],
    public_values: &[u8],
    proof: &[u8],
) -> Outcome {
    let key = key.as_ref().map_err(String::clone)?;
    groth16::sp1::verify_with(key, program_vkey, public_values, proof).map(|()| Output::Unit).map_err(debug)
}

/// Runs each case, decoding SP1's Groth16 verifying key and building the JLVM once, and only if a case needs them (so
/// a family's precompile calls are its own).
pub fn run(cases: &[Case]) -> Vec<Outcome> {
    let groth16_key = OnceCell::new();
    let jlvm = OnceCell::new();
    cases
        .iter()
        .map(|case| match case {
            Case::Verify { scheme, key, message, signature } => SchemeId::from_name(scheme)
                .ok_or_else(|| format!("unknown scheme {scheme}"))
                .and_then(|id| id.verify_bytes(key, message, signature).map_err(debug))
                .map(Output::Bool),
            Case::BlsPop { key, proof } => bls::VerifyingKey::from_bytes(key)
                .and_then(|k| bls::Signature::from_bytes(proof).map(|p| bls::pop_verify(&k, &p)))
                .map(Output::Bool)
                .map_err(debug),
            Case::EcRecover { digest, signature } => {
                ecrecover::recover(digest, signature).map(|a| Output::Address(a.map(|a| a.to_vec()))).map_err(debug)
            }
            Case::Sigma { proposition, proof, message } => cbor::decode(proposition)
                .map_err(debug)
                .and_then(|p| sigma::verify_value(&p, proof, message).map_err(debug))
                .map(Output::Bool),
            Case::Bn254Add { a, b } => zk::add(a, b).map(|p| Output::Bytes(p.to_vec())).map_err(debug),
            Case::Bn254Mul { point, scalar } => {
                zk::mul(point, scalar).map(|p| Output::Bytes(p.to_vec())).map_err(debug)
            }
            Case::Bn254Pairing { pairs } => {
                let pairs: Vec<(&[u8], &[u8])> = pairs.iter().map(|(a, b)| (a.as_slice(), b.as_slice())).collect();
                zk::pairing(&pairs).map(Output::Bool).map_err(debug)
            }
            Case::Sp1Groth16 { program_vkey, public_values, proof } => {
                let key = groth16_key.get_or_init(|| groth16::sp1::verifying_key().map_err(debug));
                groth16_case(key, program_vkey, public_values, proof)
            }
            Case::Poseidon { inputs } => {
                inputs.iter().map(|x| field(x)).collect::<Result<Vec<_>, _>>().and_then(|xs| {
                    poseidon::hash(&xs).map(|h| Output::Bytes(fr_to_be_bytes(h).to_vec())).map_err(debug)
                })
            }
            Case::PoseidonRoot { leaf, proof } => field(leaf).and_then(|leaf| {
                let value = cbor::decode(proof).map_err(debug)?;
                let proof = PoseidonMerkleProof::from_value(&value).map_err(debug)?;
                Ok(Output::Bytes(fr_to_be_bytes(compute_root(leaf, &proof)).to_vec()))
            }),
            Case::Vrf { suite, key, alpha, proof } => SuiteId::from_name(suite)
                .ok_or_else(|| format!("unknown VRF suite {suite}"))
                .and_then(|id| id.verify_bytes(key, alpha, proof).map_err(debug))
                .map(|beta| Output::Bytes(beta.to_vec())),
            Case::KesVerify { scheme, key, period, message, signature } => {
                kes_verify(scheme, key, *period, message, signature)
            }
            Case::Jlvm { program, data, env, built, gas_limit } => {
                let jlvm = jlvm.get_or_init(|| Native::new().map(Jlvm::new).map_err(debug));
                jlvm_case(jlvm, program, data, env, built, *gas_limit)
            }
        })
        .collect()
}
