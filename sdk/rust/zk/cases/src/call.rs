//! One JLVM call and the statement the SP1 guest `dilf4s-zk-jlvm-call` commits for it (`docs/specs/zk-jlvm.md`,
//! `shared/schemas/zk-jlvm.cddl`). The guest and the host compute it with this same code.

use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

use dilf4s_core::codec::Model as _;
use dilf4s_core::commitment::value_commitment_into;
use dilf4s_core::generated::jlvm_vectors::{
    JlvmCause, JlvmError as ErrorRecord, JlvmGroth16Cause, JlvmHashCause, JlvmMerkleCause, JlvmPoseidonCause,
    JlvmPoseidonMerkleCause, JlvmSigmaCause, JlvmSigningCause, JlvmTrieCause, JlvmVrfCause, JlvmZkCause,
};
use dilf4s_core::generated::zk_jlvm::{ZkJlvmOutcome, ZkJlvmResult, ZkJlvmStatement};
use dilf4s_core::hash::{DomainTag, HashAlgorithm, Sha256, TaggedHash};
use dilf4s_core::{CanonicalError, cbor};
use dilf4s_jlvm::{
    Config, CryptoCause, ErrorSite as _, GasLimit, GasSchedule, Jlvm, JlvmError, Native, Program, VERSION, Value,
    parse, parse_value,
};
use serde::{Deserialize, Serialize};

/// The tag of the input data's digest.
pub const INPUT_TAG: &str = "dilf4s/zk/jlvm/input/v1";
/// The tag of one host binding's digest.
pub const BINDING_TAG: &str = "dilf4s/zk/jlvm/binding/v1";
/// The tag of the host bindings' digest.
pub const BINDINGS_TAG: &str = "dilf4s/zk/jlvm/bindings/v1";
/// The tag of a result value's digest.
pub const RESULT_TAG: &str = "dilf4s/zk/jlvm/result/v1";

/// One JLVM call, as the guest reads it: the program, the input data and each host binding as JSON surface text (as
/// `jlvm_v1.json` holds them), and the gas limit. It runs under the latest gas schedule.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Call {
    /// The program's surface text.
    pub program: String,
    /// The input data's surface text.
    pub data: String,
    /// The host bindings: each name and its value's surface text.
    pub env: Vec<(String, String)>,
    /// The gas limit.
    pub gas_limit: u64,
}

/// Why a call has no statement: an input that does not parse, or a binding named twice. The guest then exits with a
/// non-zero code, so no proof of it verifies.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Rejected {
    /// The program does not parse.
    Program(JlvmError),
    /// The input data does not parse.
    Data(JlvmError),
    /// A binding's value does not parse.
    Binding(String, JlvmError),
    /// A binding is named twice.
    Duplicate(String),
    /// A digest or the statement could not be built.
    Encoding(String),
}

/// The parsed call: the program, the data and the bindings by name.
struct Parsed {
    program: Program,
    data: Value,
    env: BTreeMap<String, Value>,
}

fn parse_call(call: &Call) -> Result<Parsed, Rejected> {
    let program = parse(&call.program).map_err(Rejected::Program)?;
    let data = parse_value(&call.data).map_err(Rejected::Data)?;
    let mut env = BTreeMap::new();
    for (name, text) in &call.env {
        let value = parse_value(text).map_err(|e| Rejected::Binding(name.clone(), e))?;
        if env.insert(name.clone(), value).is_some() {
            return Err(Rejected::Duplicate(name.clone()));
        }
    }
    Ok(Parsed { program, data, env })
}

fn tagged(tag: &str) -> Result<TaggedHash<Sha256>, Rejected> {
    DomainTag::new(tag).map(TaggedHash::new).map_err(|e| Rejected::Encoding(alloc::format!("{e}")))
}

fn word(bytes: &[u8]) -> Result<[u8; 32], Rejected> {
    bytes.try_into().map_err(|_| Rejected::Encoding(alloc::format!("a digest of {} bytes", bytes.len())))
}

/// `taggedHash_SHA-256(tag, commit(value))`, the commitment streamed into the hash.
pub fn value_digest(tag: &str, value: &Value) -> Result<[u8; 32], Rejected> {
    let mut h = tagged(tag)?.start();
    value_commitment_into(&mut h, value);
    word(h.finish().as_bytes())
}

/// The host bindings' digest: [`BINDINGS_TAG`] over each binding's [`BINDING_TAG`] digest of `cbor(name) ||
/// commit(value)`, in name order. A map would drop a binding to null, which differs from an unbound name.
pub fn bindings_digest(env: &BTreeMap<String, Value>) -> Result<[u8; 32], Rejected> {
    let entry = tagged(BINDING_TAG)?;
    let mut all = tagged(BINDINGS_TAG)?.start();
    for (name, value) in env {
        let mut h = entry.start();
        h.update(&cbor::encode(&Value::text(name.as_str())));
        value_commitment_into(&mut h, value);
        all.update(h.finish().as_bytes());
    }
    word(all.finish().as_bytes())
}

/// A `Crypto` error's normative cause: its family and that family's kind.
fn cause(c: &CryptoCause) -> JlvmCause {
    match c {
        CryptoCause::Signing { error } => JlvmCause::JlvmSigningCause(JlvmSigningCause { signing: error.kind() }),
        CryptoCause::Sigma { error } => JlvmCause::JlvmSigmaCause(JlvmSigmaCause { sigma: error.kind() }),
        CryptoCause::Zk { error } => JlvmCause::JlvmZkCause(JlvmZkCause { zk: error.kind() }),
        CryptoCause::Groth16 { error } => JlvmCause::JlvmGroth16Cause(JlvmGroth16Cause { groth16: error.kind() }),
        CryptoCause::Vrf { error } => JlvmCause::JlvmVrfCause(JlvmVrfCause { vrf: error.kind() }),
        CryptoCause::Hash { error } => JlvmCause::JlvmHashCause(JlvmHashCause { hash: error.kind() }),
        CryptoCause::Merkle { error } => JlvmCause::JlvmMerkleCause(JlvmMerkleCause { merkle: error.kind() }),
        CryptoCause::Trie { error } => JlvmCause::JlvmTrieCause(JlvmTrieCause { trie: error.kind() }),
        CryptoCause::Poseidon { error } => JlvmCause::JlvmPoseidonCause(JlvmPoseidonCause { poseidon: error.kind() }),
        CryptoCause::PoseidonMerkle { error } => {
            JlvmCause::JlvmPoseidonMerkleCause(JlvmPoseidonMerkleCause { poseidon_merkle: error.kind() })
        }
    }
}

/// An error's normative fields (jlvm.md section 3): its kind, its `op`, and a `Crypto` error's cause. Never its
/// message, which is informative.
pub fn error_record(e: &JlvmError) -> ErrorRecord {
    let cause = match e {
        JlvmError::Crypto { cause: Some(c), .. } => Some(cause(c)),
        _ => None,
    };
    ErrorRecord { error: e.kind(), op: e.site(), cause }
}

/// Evaluates `call` and builds its statement. `Err` only for a call that has none ([`Rejected`]); an evaluation that
/// fails is a statement of that failure.
pub fn statement(jlvm: &Jlvm<Native>, call: &Call) -> Result<ZkJlvmStatement, Rejected> {
    let Parsed { program, data, env } = parse_call(call)?;
    let id = program.id(HashAlgorithm::Sha256).map_err(|e| Rejected::Encoding(alloc::format!("{e}")))?;
    let schedule = GasSchedule::LATEST;
    let config = Config { limit: GasLimit(dilf4s_jlvm::Gas::new(call.gas_limit)), schedule };
    let outcome = jlvm.evaluate(&program, &data, &env, &config);
    let outcome_record = match &outcome.result {
        Ok(v) => ZkJlvmOutcome::ZkJlvmResult(ZkJlvmResult { result: value_digest(RESULT_TAG, v)? }),
        Err(e) => ZkJlvmOutcome::JlvmError(error_record(e)),
    };
    Ok(ZkJlvmStatement {
        language: u64::from(VERSION),
        schedule: u64::from(schedule.version),
        program: word(&id)?,
        input: value_digest(INPUT_TAG, &data)?,
        bindings: bindings_digest(&env)?,
        gas_limit: call.gas_limit,
        gas_used: outcome.gas_used.value(),
        outcome: outcome_record,
    })
}

/// The statement's canonical CBOR: what the guest commits as its public values.
pub fn public_values(s: &ZkJlvmStatement) -> Result<Vec<u8>, CanonicalError> {
    s.to_value().map(|v| cbor::encode(&v))
}

/// Public values decoded as a statement: canonical CBOR of exactly one `zk-jlvm-statement`, nothing before or after.
pub fn decode_statement(bytes: &[u8]) -> Result<ZkJlvmStatement, String> {
    let value = cbor::decode(bytes).map_err(|e| alloc::format!("{e}"))?;
    let statement = ZkJlvmStatement::from_value(&value).map_err(|e| alloc::format!("{e}"))?;
    // one encoding per statement: anything the decoder tolerates that the encoder would not write is refused
    let again = public_values(&statement).map_err(|e| alloc::format!("{e}"))?;
    if again.as_slice() == bytes {
        Ok(statement)
    } else {
        Err(String::from("the public values are not the statement's canonical encoding"))
    }
}

/// The JLVM the statement evaluates with.
pub fn jlvm() -> Result<Jlvm<Native>, String> {
    Native::new().map(Jlvm::new).map_err(|e| alloc::format!("{e:?}"))
}
