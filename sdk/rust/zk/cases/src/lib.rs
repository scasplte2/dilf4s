//! What the SP1 guest computes over the shared vectors (`shared/vectors`), and what the host computes natively to
//! compare it with.
//!
//! The guest and the host run this same code. The guest builds it for `riscv64im-succinct-zkvm-elf` with the SP1
//! patches, so the cryptography goes through their `target_os = "zkvm"` paths (precompiles and hints). The host builds
//! it with the crates.io releases the SDK ships. Every outcome is the full result, its error rendered with `Debug`, so
//! any difference between the two shows.

#![no_std]

extern crate alloc;

pub mod leaves;
#[cfg(feature = "vectors")]
pub mod vectors;

use alloc::collections::BTreeMap;
use alloc::format;
use alloc::string::String;
use alloc::vec::Vec;
use core::cell::OnceCell;
use core::fmt::Debug;

use dilf4s_core::cbor;
use dilf4s_jlvm::{Config, Gas, GasLimit, GasSchedule, Jlvm, JlvmError, Native, parse, parse_value};
use dilf4s_proofs::poseidon::merkle::{PoseidonMerkleProof, compute_root};
use dilf4s_proofs::poseidon::{self, Fr, fr_from_be_bytes, fr_to_be_bytes};
use dilf4s_proofs::zk::{self, groth16};
use dilf4s_signing::vrf::SuiteId;
use dilf4s_signing::{SchemeId, bls, ecrecover, sigma};
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
    /// A JLVM evaluation, as the conformance vectors run it.
    Jlvm {
        /// The program text.
        program: String,
        /// The data text.
        data: String,
        /// The host bindings' texts.
        env: Vec<(String, String)>,
        /// The gas limit.
        gas_limit: u64,
    },
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
    /// A JLVM outcome: the value's canonical CBOR or the error, the gas and the number of warnings.
    Jlvm {
        /// The value's canonical CBOR, or the error's kind and `Debug` rendering.
        result: Result<Vec<u8>, (String, String)>,
        /// The gas used.
        gas: u64,
        /// The number of warnings.
        warnings: u64,
    },
}

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
    gas_limit: u64,
) -> Outcome {
    let jlvm = jlvm.as_ref().map_err(String::clone)?;
    let (result, gas, warnings) = match parse_inputs(program, data, env) {
        Err(e) => (Err(e), Gas::ZERO, 0),
        Ok((program, data, env)) => {
            let config = Config { limit: GasLimit(Gas::new(gas_limit)), schedule: GasSchedule::LATEST };
            let o = jlvm.evaluate(&program, &data, &env, &config);
            (o.result, o.gas_used, o.warnings.len())
        }
    };
    Ok(Output::Jlvm {
        result: result.map(|v| cbor::encode(&v)).map_err(|e| (String::from(e.kind()), debug(&e))),
        gas: gas.value(),
        warnings: u64::try_from(warnings).unwrap_or(u64::MAX),
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
            Case::Jlvm { program, data, env, gas_limit } => {
                let jlvm = jlvm.get_or_init(|| Native::new().map(Jlvm::new).map_err(debug));
                jlvm_case(jlvm, program, data, env, *gas_limit)
            }
        })
        .collect()
}
