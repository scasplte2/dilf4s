//! `shared/vectors/zk_jlvm_v1.json` (`shared/schemas/zk-jlvm-vectors.cddl`): the calls, each a row of `jlvm_v1.json`,
//! with its statement and public values computed natively; the guest's key; and a real SP1 Groth16 proof where one
//! has been made (`README.md`, "Proving a JLVM call"). [`expected`] is what the file must hold, so a changed guest, a
//! changed statement or a changed row makes the committed file stale.

use std::path::PathBuf;

use dilf4s_core::codec::Model;
use dilf4s_core::generated::jlvm_vectors::{JlvmCase, JlvmVectors};
use dilf4s_core::generated::zk_jlvm::{ZkJlvmOutcome, ZkJlvmStatement};
use dilf4s_core::generated::zk_jlvm_vectors::{
    ZkJlvmCall, ZkJlvmOutcomeHex, ZkJlvmResultHex, ZkJlvmStatementHex, ZkJlvmVectors,
};
use dilf4s_core::{hex, json};
use dilf4s_jlvm::{Jlvm, Native};
use dilf4s_proofs::zk::groth16;
use dilf4s_zk_cases::call::{Call, decode_statement, public_values, statement};

use crate::GuestKey;

/// The `jlvm_v1.json` rows the file covers, by name: a success that calls `ecrecover` (the one proved), an error a
/// cryptographic operation raises, out of gas with a host binding, a sigma threshold, a host binding read, a `Type` and
/// an `Arithmetic` error, and a `Crypto` error with a cause.
pub const CALLS: [&str; 8] = [
    "contract multisig_wallet: 2 of 3 owners sign",
    "contract multisig_wallet: an unrecoverable signature is an error",
    "contract erc721: mint with too little gas",
    "contract threshold_treasury: 2 of 3 sign",
    "contract vesting: release at 1333",
    "metakit HexOpsSuite: hex_to_int rejects a non-string argument (integer 5)",
    "metakit JsonLogicSpec: division by zero should throw error for integer division",
    "metakit gas vector 203",
];

/// The call whose Groth16 proof the file pins.
pub const PROVED: &str = "contract multisig_wallet: 2 of 3 owners sign";

/// How to make the proof again, on euler (`README.md`, "Proving a JLVM call", "On euler"): GitHub's hosted runners
/// cannot prove (a core proof of the smallest call needs more than their 16 GB).
pub fn reprove() -> String {
    format!(
        "re-prove on euler (sdk/rust/zk/README.md, \"On euler\", steps 4 to 6, with EXPECT set to the key built here, \
         without 0x): in sdk/rust/zk, `cargo run --release \
         --features cuda --manifest-path host/Cargo.toml --bin dilf4s-zk-jlvm -- prove --case \"{PROVED}\" --mode \
         groth16 --out /tmp/dilf4s-out`, then `cargo run --release --features cuda --manifest-path host/Cargo.toml \
         --bin dilf4s-zk-jlvm -- vector --proof \"{PROVED}=/tmp/dilf4s-out/proof-groth16.bin\"`, and commit \
         shared/vectors/zk_jlvm_v1.json"
    )
}

/// The SP1 release that proves, and the Groth16 circuit `groth16::sp1` verifies.
pub const SP1: &str = "v6.10.0";
/// The Groth16 circuit version.
pub const CIRCUIT: &str = "v6.1.0";

fn vectors_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../../shared/vectors")
}

/// The committed file's path.
pub fn path() -> PathBuf {
    vectors_dir().join("zk_jlvm_v1.json")
}

fn load<T: Model>(file: &PathBuf) -> Result<T, String> {
    let text = std::fs::read_to_string(file).map_err(|e| format!("{}: {e}", file.display()))?;
    let value = json::parse(&text).map_err(|e| format!("{} is not canonical JSON: {e}", file.display()))?;
    T::from_value(&value).map_err(|e| format!("{} does not fit its schema: {e}", file.display()))
}

/// The committed file, if there is one.
pub fn committed() -> Result<Option<ZkJlvmVectors>, String> {
    let p = path();
    if p.exists() { load(&p).map(Some) } else { Ok(None) }
}

/// A row as a call: its texts, its bindings in name order, and its gas limit (the file's when it has none).
pub fn call_of(row: &JlvmCase, default_limit: u64) -> Result<Call, String> {
    if row.built.is_some() {
        return Err(format!("{}: a call has no built bindings", row.name));
    }
    Ok(Call {
        program: row.program.clone(),
        data: row.data.clone(),
        env: row.env.iter().flatten().map(|(k, v)| (k.clone(), v.clone())).collect(),
        gas_limit: row.gas_limit.unwrap_or(default_limit),
    })
}

/// Each of [`CALLS`] as a call, with its row.
pub fn calls() -> Result<Vec<(JlvmCase, Call)>, String> {
    let v: JlvmVectors = load(&vectors_dir().join("jlvm_v1.json"))?;
    CALLS
        .iter()
        .map(|name| {
            let row =
                v.cases.iter().find(|c| c.name == *name).ok_or_else(|| format!("jlvm_v1.json has no row {name}"))?;
            Ok((row.clone(), call_of(row, v.gas_limit)?))
        })
        .collect()
}

/// A call by its name in [`CALLS`].
pub fn call(name: &str) -> Result<Call, String> {
    calls()?.into_iter().find(|(r, _)| r.name == name).map(|(_, c)| c).ok_or_else(|| format!("no call {name}"))
}

/// A statement with its digests as hex, as the file holds it.
pub fn statement_hex(s: &ZkJlvmStatement) -> ZkJlvmStatementHex {
    ZkJlvmStatementHex {
        language: s.language,
        schedule: s.schedule,
        program: hex::encode(&s.program),
        input: hex::encode(&s.input),
        bindings: hex::encode(&s.bindings),
        gas_limit: s.gas_limit,
        gas_used: s.gas_used,
        outcome: match &s.outcome {
            ZkJlvmOutcome::ZkJlvmResult(r) => {
                ZkJlvmOutcomeHex::ZkJlvmResultHex(ZkJlvmResultHex { result: hex::encode(&r.result) })
            }
            ZkJlvmOutcome::JlvmError(e) => ZkJlvmOutcomeHex::JlvmError(e.clone()),
        },
    }
}

/// The statement natively, checked against what its row says: the same outcome (error kind and normative fields, or
/// a value whose commitment digest is the statement's) and the same gas, so a bug native code shares with the guest
/// cannot pass.
pub fn native(jlvm: &Jlvm<Native>, row: &JlvmCase, call: &Call) -> Result<ZkJlvmStatement, String> {
    use dilf4s_core::generated::jlvm_vectors::JlvmOutcome;
    let s = statement(jlvm, call).map_err(|e| format!("{}: no statement: {e:?}", row.name))?;
    let agrees = match (&row.outcome, &s.outcome) {
        (JlvmOutcome::JlvmValue(v), ZkJlvmOutcome::ZkJlvmResult(r)) => {
            let value = dilf4s_core::cbor::decode(&hex::decode(&v.cbor).map_err(|e| format!("{e}"))?)
                .map_err(|e| format!("{e}"))?;
            dilf4s_zk_cases::call::value_digest(dilf4s_zk_cases::call::RESULT_TAG, &value)
                .map_err(|e| format!("{e:?}"))?
                == r.result
        }
        (JlvmOutcome::JlvmError(want), ZkJlvmOutcome::JlvmError(got)) => want == got,
        _ => false,
    };
    if !agrees || s.gas_used != row.gas_used {
        return Err(format!("{}: the statement {s:?} disagrees with jlvm_v1.json's {:?}", row.name, row.outcome));
    }
    Ok(s)
}

/// Whether `proof` is a valid SP1 Groth16 proof of `public` under `key`, by dilf4s's own verifier (zk.md section 2).
pub fn verify(key: &GuestKey, public: &[u8], proof: &[u8]) -> Result<(), String> {
    groth16::sp1::verify(&key.vkey_bytes()?, public, proof).map_err(|e| format!("{e:?}"))
}

/// What the file must hold under the guest's `key`: every call's native statement and public values, and, for a call
/// with a proof, `proofs`' (the CLI's new one) or else the committed file's, if it still verifies against `key` and
/// these public values. [`PROVED`] must have one. A proof that no longer verifies, or none for [`PROVED`], is an
/// error: it is never dropped, only made again ([`reprove`]).
pub fn expected(
    key: &GuestKey,
    committed: Option<&ZkJlvmVectors>,
    proofs: &[(String, Vec<u8>)],
) -> Result<ZkJlvmVectors, String> {
    let jlvm = dilf4s_zk_cases::call::jlvm()?;
    let calls = calls()?
        .into_iter()
        .map(|(row, call)| {
            let s = native(&jlvm, &row, &call)?;
            let public = public_values(&s).map_err(|e| format!("{e}"))?;
            let old =
                committed.and_then(|c| c.calls.iter().find(|x| x.name == row.name)).and_then(|x| x.proof_bytes.clone());
            let proof = match proofs.iter().find(|(n, _)| *n == row.name) {
                Some((_, p)) => Some(p.clone()),
                None => old.map(|h| hex::decode(&h).map_err(|e| format!("{e}"))).transpose()?,
            };
            let proof = match proof {
                Some(p) => match verify(key, &public, &p) {
                    Ok(()) => Some(hex::encode(&p)),
                    Err(e) => {
                        return Err(format!(
                            "{}: its proof does not verify under the guest's key {} and its public values ({e}): \
                             the guest or the statement changed; {}",
                            row.name,
                            key.vkey,
                            reprove()
                        ));
                    }
                },
                None if row.name == PROVED => {
                    return Err(format!("{}: the call has no Groth16 proof; {}", row.name, reprove()));
                }
                None => None,
            };
            Ok(ZkJlvmCall {
                name: row.name.clone(),
                program: call.program.clone(),
                data: call.data.clone(),
                env: row.env.clone(),
                gas_limit: call.gas_limit,
                statement: statement_hex(&s),
                public_values: hex::encode(&public),
                proof_bytes: proof,
            })
        })
        .collect::<Result<Vec<_>, String>>()?;
    Ok(ZkJlvmVectors {
        sp1: SP1.into(),
        circuit: CIRCUIT.into(),
        program_vkey: key.vkey.strip_prefix("0x").unwrap_or(&key.vkey).into(),
        elf_sha256: key.elf_sha256.clone(),
        calls,
    })
}

/// The file's text: its canonical JSON, two-space indented, one member or element per line.
pub fn render(v: &ZkJlvmVectors) -> Result<String, String> {
    let value = v.to_value().map_err(|e| format!("{e}"))?;
    let compact = json::render_within(&value, 1 << 26).map_err(|e| format!("{e:?}"))?;
    Ok(indent(&compact))
}

/// Indents compact JSON: a new line after each `{`, `[` and `,` outside text, and before each closing one; empty
/// containers stay on one line.
fn indent(compact: &str) -> String {
    let mut out = String::with_capacity(compact.len().saturating_mul(2));
    let mut depth = 0usize;
    let (mut in_text, mut escaped) = (false, false);
    let mut chars = compact.chars().peekable();
    let newline = |out: &mut String, depth: usize| {
        out.push('\n');
        out.push_str(&"  ".repeat(depth));
    };
    while let Some(c) = chars.next() {
        if in_text {
            out.push(c);
            match (escaped, c) {
                (true, _) => escaped = false,
                (false, '\\') => escaped = true,
                (false, '"') => in_text = false,
                _ => {}
            }
            continue;
        }
        match c {
            '"' => {
                in_text = true;
                out.push(c);
            }
            '{' | '[' if matches!(chars.peek(), Some('}' | ']')) => {
                out.push(c);
                out.extend(chars.next());
            }
            '{' | '[' => {
                out.push(c);
                depth = depth.saturating_add(1);
                newline(&mut out, depth);
            }
            '}' | ']' => {
                depth = depth.saturating_sub(1);
                newline(&mut out, depth);
                out.push(c);
            }
            ',' => {
                out.push(c);
                newline(&mut out, depth);
            }
            ':' => out.push_str(": "),
            other => out.push(other),
        }
    }
    out.push('\n');
    out
}

/// Each committed call's public values decoded, checked against its statement, and re-encoded: one encoding per
/// statement, and the file's two forms of it agree.
pub fn check_public_values(v: &ZkJlvmVectors) -> Vec<String> {
    v.calls
        .iter()
        .filter_map(|c| {
            let decoded = hex::decode(&c.public_values).map_err(|e| format!("{e}")).and_then(|b| decode_statement(&b));
            match decoded {
                Ok(s) if statement_hex(&s) == c.statement => None,
                Ok(s) => Some(format!("{}: the public values decode to {s:?}, not the file's statement", c.name)),
                Err(e) => Some(format!("{}: the public values are not a statement: {e}", c.name)),
            }
        })
        .collect()
}
