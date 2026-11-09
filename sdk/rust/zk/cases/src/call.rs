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
    decode, parse, parse_value,
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

/// One JLVM call, as the guest reads it (`docs/specs/zk-jlvm.md` section 1): the program's canonical form and the
/// input data and each host binding's value, each as its canonical CBOR, and the gas limit. It runs under the latest
/// gas schedule. [`Call::of_surface`] makes one from JSON surface texts, as `jlvm_v1.json` holds them.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Call {
    /// The program's canonical CBOR (jlvm.md section 1).
    #[serde(with = "bytes")]
    pub program: Vec<u8>,
    /// The input data's canonical CBOR.
    #[serde(with = "bytes")]
    pub data: Vec<u8>,
    /// The host bindings, in order and never merged.
    pub env: Vec<Binding>,
    /// The gas limit.
    pub gas_limit: u64,
}

/// One host binding of a [`Call`]: its name and its value's canonical CBOR.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Binding {
    /// The name.
    pub name: String,
    /// The value's canonical CBOR.
    #[serde(with = "bytes")]
    pub value: Vec<u8>,
}

/// A byte string as serde's bytes, not a sequence of `u8`: SP1's stdin (bincode) writes a length and the bytes either
/// way, and reads bytes in one copy rather than one element at a time.
mod bytes {
    use alloc::vec::Vec;
    use core::fmt;

    use serde::de::{Error, Visitor};
    use serde::{Deserializer, Serializer};

    pub(super) fn serialize<S: Serializer>(b: &[u8], s: S) -> Result<S::Ok, S::Error> {
        s.serialize_bytes(b)
    }

    struct Bytes;

    impl Visitor<'_> for Bytes {
        type Value = Vec<u8>;

        fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str("bytes")
        }

        fn visit_bytes<E: Error>(self, b: &[u8]) -> Result<Vec<u8>, E> {
            Ok(b.to_vec())
        }

        fn visit_byte_buf<E: Error>(self, b: Vec<u8>) -> Result<Vec<u8>, E> {
            Ok(b)
        }
    }

    pub(super) fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Vec<u8>, D::Error> {
        d.deserialize_byte_buf(Bytes)
    }
}

/// Why surface texts are not a call: the program, the input data or a binding's value does not parse.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Unparsed {
    /// The program does not parse.
    Program(JlvmError),
    /// The input data does not parse.
    Data(JlvmError),
    /// A binding's value does not parse.
    Binding(String, JlvmError),
}

impl Call {
    /// The call surface texts write (the program, the input data and each binding's value, as `jlvm_v1.json` holds
    /// them), parsed as the conformance vectors are run and written in canonical form. The bindings keep their order,
    /// a name given twice included: the guest refuses that call.
    pub fn of_surface(program: &str, data: &str, env: &[(String, String)], gas_limit: u64) -> Result<Self, Unparsed> {
        let program = parse(program).and_then(|p| p.encode()).map_err(Unparsed::Program)?;
        let data = parse_value(data).map(|v| cbor::encode(&v)).map_err(Unparsed::Data)?;
        let env = env
            .iter()
            .map(|(name, text)| {
                parse_value(text)
                    .map(|v| Binding { name: name.clone(), value: cbor::encode(&v) })
                    .map_err(|e| Unparsed::Binding(name.clone(), e))
            })
            .collect::<Result<_, _>>()?;
        Ok(Self { program, data, env, gas_limit })
    }
}

/// Why a call has no statement: an input that does not decode, or a binding named twice. The guest then exits with a
/// non-zero code, so no proof of it verifies.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Rejected {
    /// The program's bytes are not a program's canonical form (`dilf4s_jlvm::decode`).
    Program(JlvmError),
    /// The input data's bytes are not canonical CBOR.
    Data(CanonicalError),
    /// A binding's bytes are not canonical CBOR.
    Binding(String, CanonicalError),
    /// A binding is named twice.
    Duplicate(String),
    /// A digest or the statement could not be built.
    Encoding(String),
}

/// The decoded call: the program, the data and the bindings by name.
struct Decoded {
    program: Program,
    data: Value,
    env: BTreeMap<String, Value>,
}

fn decode_call(call: &Call) -> Result<Decoded, Rejected> {
    let program = decode(&call.program).map_err(Rejected::Program)?;
    let data = cbor::decode(&call.data).map_err(Rejected::Data)?;
    let mut env = BTreeMap::new();
    for Binding { name, value } in &call.env {
        let value = cbor::decode(value).map_err(|e| Rejected::Binding(name.clone(), e))?;
        if env.insert(name.clone(), value).is_some() {
            return Err(Rejected::Duplicate(name.clone()));
        }
    }
    Ok(Decoded { program, data, env })
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
    let Decoded { program, data, env } = decode_call(call)?;
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
