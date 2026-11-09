//! The Rust runner of the JLVM differential fuzzing harness (`tools/difffuzz/README.md`). It reads cases as JSON
//! lines, from a file named as its one argument or from standard input, and writes one record line per case to
//! standard output, flushed per record, so a driver can stream to it.
//!
//! A case is a `jlvm_v1.json` input case, every text as the hex of its UTF-8 (so no transport parser reads it before
//! the JLVM's own parser does): `name`; `program` (surface text) or `form` (a canonical form's CBOR, read by
//! [`decode`]); `data`; `env`, `[name, text]` pairs; `built`, `[name, built-value]` pairs; `gasLimit`. It is run as
//! the reference's `JlvmVectors.run` runs a vector case: the program, then the data, then each binding in order; the
//! first that fails is the outcome, with no gas. Then the bindings built with shared parts are added, and the program
//! is evaluated with the case's limit (2^24 when absent), the latest schedule and the native backend.
//!
//! A record holds what the vectors pin, and what is as normative: `program`, the canonical CBOR of the program read
//! (its identity, jlvm.md section 1); `outcome`, the vectors' `jlvm-outcome` (a value's CBOR, or an error's kind, `op`
//! and `cause`); `available`, an `OutOfGas` error's limit; `gasUsed`; and `warnings`, each path preview as
//! `[head, length]`. A line that cannot be read is a fault: the runner says why on standard error and exits non-zero.

extern crate alloc;

use alloc::collections::BTreeMap;
use std::io::{BufRead, Write};
use std::process::ExitCode;

use dilf4s_core::built::{self, Step};
use dilf4s_core::codec::Model;
use dilf4s_core::generated::jlvm_vectors::{
    BuiltStep, BuiltValue, JlvmCause, JlvmError as VectorError, JlvmGroth16Cause, JlvmHashCause, JlvmMerkleCause,
    JlvmOutcome, JlvmPoseidonCause, JlvmPoseidonMerkleCause, JlvmSigmaCause, JlvmSigningCause, JlvmTrieCause,
    JlvmValue, JlvmVrfCause, JlvmZkCause,
};
use dilf4s_core::{cbor, hex, json};
use dilf4s_jlvm::{
    Config, CryptoCause, ErrorSite, Gas, GasLimit, GasSchedule, Jlvm, JlvmError, Native, Outcome, Program, Value,
    Warning, decode, parse, parse_value,
};

/// What the program is read from.
enum Source {
    /// Surface text.
    Surface(String),
    /// A canonical form's CBOR.
    Form(Vec<u8>),
}

/// One case, read from its line.
struct Case {
    name: String,
    source: Source,
    data: String,
    env: Vec<(String, String)>,
    built: Vec<(String, BuiltValue)>,
    gas_limit: Option<u64>,
}

fn field<'a>(m: &'a dilf4s_core::value::Map, key: &str) -> Option<&'a Value> {
    m.get(key)
}

fn text<'a>(v: &'a Value, what: &str) -> Result<&'a str, String> {
    v.as_str().ok_or_else(|| format!("{what} is not text"))
}

/// Hex of UTF-8 back to text: ill-formed UTF-8 is refused, never replaced.
fn utf8(v: &Value, what: &str) -> Result<String, String> {
    let bytes = hex::decode(text(v, what)?).map_err(|e| format!("{what}: {e}"))?;
    String::from_utf8(bytes).map_err(|_| format!("{what}: ill-formed UTF-8"))
}

/// `[name, x]` pairs, in order.
fn pairs<'a>(v: Option<&'a Value>, what: &str) -> Result<Vec<(String, &'a Value)>, String> {
    let Some(v) = v else { return Ok(Vec::new()) };
    let Value::Array(items) = v else { return Err(format!("{what} is not an array")) };
    items
        .items()
        .iter()
        .map(|p| match p {
            Value::Array(kv) => match kv.items() {
                [k, x] => Ok((utf8(k, what)?, x)),
                _ => Err(format!("{what}: a pair has two items")),
            },
            _ => Err(format!("{what}: a pair is an array")),
        })
        .collect()
}

fn read(line: &str) -> Result<Case, String> {
    let value = json::parse(line).map_err(|e| format!("not canonical JSON: {e}"))?;
    let Value::Map(m) = &value else { return Err("a case is a map".into()) };
    let name = text(field(m, "name").ok_or("no name")?, "name")?.to_owned();
    let source = match (field(m, "program"), field(m, "form")) {
        (Some(p), None) => Source::Surface(utf8(p, "program")?),
        (None, Some(f)) => Source::Form(hex::decode(text(f, "form")?).map_err(|e| format!("form: {e}"))?),
        _ => return Err(format!("{name}: exactly one of program and form")),
    };
    let data = utf8(field(m, "data").ok_or("no data")?, "data")?;
    let env = pairs(field(m, "env"), "env")?
        .into_iter()
        .map(|(k, v)| utf8(v, "env").map(|t| (k, t)))
        .collect::<Result<Vec<_>, _>>()?;
    let built = pairs(field(m, "built"), "built")?
        .into_iter()
        .map(|(k, v)| BuiltValue::from_value(v).map(|b| (k, b)).map_err(|e| format!("built: {e}")))
        .collect::<Result<Vec<_>, _>>()?;
    let gas_limit = field(m, "gasLimit")
        .map(|g| g.as_int().and_then(|n| u64::try_from(n).ok()).ok_or("gasLimit is not a u64"))
        .transpose()?;
    Ok(Case { name, source, data, env, built, gas_limit })
}

/// A recipe that does not build is a broken case file, never an outcome.
fn build(b: &BuiltValue) -> Result<Value, String> {
    let leaf = hex::decode(&b.leaf).map_err(|e| format!("leaf: {e}"))?;
    let templates = b
        .steps
        .iter()
        .flatten()
        .map(|s| match s {
            BuiltStep::BuiltWrap(w) => hex::decode(&w.template).map_err(|e| format!("template: {e}")),
            BuiltStep::BuiltCopies(_) => Ok(Vec::new()),
        })
        .collect::<Result<Vec<_>, _>>()?;
    let steps: Vec<Step<'_>> = b
        .steps
        .iter()
        .flatten()
        .zip(&templates)
        .map(|(s, t)| match s {
            BuiltStep::BuiltWrap(w) => Step::Wrap { template: t, times: w.times },
            BuiltStep::BuiltCopies(c) => Step::Copies(c.copies),
        })
        .collect();
    built::build(&leaf, &steps).map_err(|e| format!("recipe: {e}"))
}

/// A Crypto error's cause as the vectors record it: its family and that family's kind.
fn cause(e: &JlvmError) -> Option<JlvmCause> {
    let JlvmError::Crypto { cause: Some(c), .. } = e else { return None };
    Some(match c {
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
    })
}

fn error(e: &JlvmError) -> JlvmOutcome {
    JlvmOutcome::JlvmError(VectorError { error: e.kind(), op: e.site(), cause: cause(e) })
}

/// The record of one case: `Err` only for a broken recipe.
fn run(jlvm: &Jlvm<Native>, case: &Case) -> Result<(Option<String>, Outcome), String> {
    let failed = |e: JlvmError| Outcome { gas_used: Gas::ZERO, result: Err(e), warnings: Vec::new() };
    let program: Result<Program, JlvmError> = match &case.source {
        Source::Surface(t) => parse(t),
        Source::Form(b) => decode(b),
    };
    let program = match program {
        Ok(p) => p,
        Err(e) => return Ok((None, failed(e))),
    };
    let form = program.encode().ok().map(|b| hex::encode(&b));
    let data = match parse_value(&case.data) {
        Ok(d) => d,
        Err(e) => return Ok((form, failed(e))),
    };
    let mut env = BTreeMap::new();
    for (k, v) in &case.env {
        match parse_value(v) {
            Ok(x) => {
                env.insert(k.clone(), x);
            }
            Err(e) => return Ok((form, failed(e))),
        }
    }
    for (k, b) in &case.built {
        env.insert(k.clone(), build(b).map_err(|e| format!("{}: binding {k}: {e}", case.name))?);
    }
    let limit = GasLimit(Gas::new(case.gas_limit.unwrap_or(GasLimit::DEFAULT.0.value())));
    let config = Config { limit, schedule: GasSchedule::LATEST };
    Ok((form, jlvm.evaluate(&program, &data, &env, &config)))
}

fn record(name: &str, form: Option<String>, o: &Outcome) -> Result<String, String> {
    let outcome = match &o.result {
        Ok(v) => JlvmOutcome::JlvmValue(JlvmValue { cbor: hex::encode(&cbor::encode(v)), json: None }),
        Err(e) => error(e),
    };
    let warnings = o
        .warnings
        .iter()
        .map(|w| match w {
            Warning::AmbiguousPath(p) => {
                let length = u64::try_from(p.length()).map_err(|_| "a preview length past u64".to_owned())?;
                Value::array(vec![Value::text(p.head()), Value::int(length)]).map_err(|e| e.to_string())
            }
        })
        .collect::<Result<Vec<_>, String>>()?;
    let mut fields = vec![
        ("name".to_owned(), Value::text(name)),
        ("outcome".to_owned(), outcome.to_value().map_err(|e| e.to_string())?),
        ("gasUsed".to_owned(), Value::int(o.gas_used.value())),
        ("warnings".to_owned(), Value::array(warnings).map_err(|e| e.to_string())?),
    ];
    if let Some(f) = form {
        fields.push(("program".to_owned(), Value::text(f)));
    }
    if let Err(JlvmError::OutOfGas { available, .. }) = &o.result {
        fields.push(("available".to_owned(), Value::int(*available)));
    }
    let value = Value::map(fields).map_err(|e| e.to_string())?;
    json::render_within(&value, usize::MAX).map_err(|e| e.to_string())
}

fn line(jlvm: &Jlvm<Native>, text: &str) -> Result<String, String> {
    let case = read(text)?;
    let (form, outcome) = run(jlvm, &case)?;
    record(&case.name, form, &outcome)
}

fn main() -> ExitCode {
    let backend = match Native::new() {
        Ok(b) => b,
        Err(e) => {
            eprintln!("runner: the native backend: {e:?}");
            return ExitCode::FAILURE;
        }
    };
    let jlvm = Jlvm::new(backend);
    let input: Box<dyn BufRead> = match std::env::args().nth(1) {
        Some(path) => match std::fs::File::open(&path) {
            Ok(f) => Box::new(std::io::BufReader::new(f)),
            Err(e) => {
                eprintln!("runner: {path}: {e}");
                return ExitCode::FAILURE;
            }
        },
        None => Box::new(std::io::BufReader::new(std::io::stdin())),
    };
    let mut out = std::io::stdout().lock();
    for l in input.lines() {
        let reply =
            l.map_err(|e| e.to_string()).and_then(|t| if t.is_empty() { Ok(None) } else { line(&jlvm, &t).map(Some) });
        match reply {
            Ok(None) => {}
            Ok(Some(r)) => {
                if let Err(e) = writeln!(out, "{r}").and_then(|()| out.flush()) {
                    eprintln!("runner: writing: {e}");
                    return ExitCode::FAILURE;
                }
            }
            Err(e) => {
                eprintln!("runner: {e}");
                return ExitCode::FAILURE;
            }
        }
    }
    ExitCode::SUCCESS
}

#[cfg(test)]
mod tests {
    use super::*;

    fn case(name: &str, program: &str, data: &str, extra: &str) -> String {
        format!(
            r#"{{"name":"{name}","program":"{}","data":"{}"{extra}}}"#,
            hex::encode(program.as_bytes()),
            hex::encode(data.as_bytes())
        )
    }

    const SUM: &str = r#"{"+": [1, {"var": "x"}]}"#;
    const SUM_FORM: &str = "a26465787072a2626f70612b646172677382a1636c697401a1637661726178646a6c766d01";

    #[test]
    fn a_case_gives_its_record_and_out_of_gas_its_limit() {
        let jlvm = Jlvm::new(Native::new().unwrap());
        let done = line(&jlvm, &case("a", SUM, r#"{"x": 2}"#, "")).unwrap();
        let want =
            format!(r#"{{"gasUsed":212,"name":"a","outcome":{{"cbor":"03"}},"program":"{SUM_FORM}","warnings":[]}}"#);
        assert_eq!(done, want);
        let starved = line(&jlvm, &case("b", SUM, r#"{"x": 2}"#, r#","gasLimit":5"#)).unwrap();
        let want = format!(
            r#"{{"available":5,"gasUsed":5,"name":"b","outcome":{{"error":"OutOfGas"}},"program":"{SUM_FORM}","warnings":[]}}"#
        );
        assert_eq!(starved, want);
    }

    #[test]
    fn a_form_is_decoded_and_a_refusal_is_its_kind() {
        let jlvm = Jlvm::new(Native::new().unwrap());
        let data = hex::encode(br#"{"x": 2}"#);
        let decoded = line(&jlvm, &format!(r#"{{"name":"c","form":"{SUM_FORM}","data":"{data}"}}"#)).unwrap();
        assert!(decoded.contains(r#""outcome":{"cbor":"03"}"#), "{decoded}");
        let cut = SUM_FORM.get(..SUM_FORM.len() - 2).unwrap();
        let refused = line(&jlvm, &format!(r#"{{"name":"d","form":"{cut}","data":"{data}"}}"#)).unwrap();
        assert_eq!(refused, r#"{"gasUsed":0,"name":"d","outcome":{"error":"Malformed"},"warnings":[]}"#);
    }

    #[test]
    fn a_warning_is_its_preview_and_a_cause_its_family() {
        let jlvm = Jlvm::new(Native::new().unwrap());
        let warned =
            line(&jlvm, &case("e", r#"{"map": [{"literal": [{"x": 1}]}, {"var": "x"}]}"#, r#"{"x": 2}"#, "")).unwrap();
        assert!(warned.ends_with(r#""warnings":[["x",1]]}"#), "{warned}");
        let refused = line(&jlvm, &case("f", r#"{"ed25519_verify": ["0x00", "0x01", "0x02"]}"#, "null", "")).unwrap();
        let cause = r#""outcome":{"cause":{"signing":"InvalidEncoding"},"error":"Crypto","op":"ed25519_verify"}"#;
        assert!(refused.contains(cause), "{refused}");
    }

    #[test]
    fn a_line_that_is_not_a_case_is_a_fault() {
        let jlvm = Jlvm::new(Native::new().unwrap());
        assert!(line(&jlvm, "not json").is_err());
        assert!(line(&jlvm, r#"{"name":"g","data":"6e756c6c"}"#).is_err(), "no program");
        assert!(line(&jlvm, r#"{"name":"g","program":"31","form":"01","data":"6e756c6c"}"#).is_err(), "both");
        assert!(line(&jlvm, r#"{"name":"g","program":"ff","data":"6e756c6c"}"#).is_err(), "not UTF-8");
        let broken = r#","built":[["61",{"leaf":"ff"}]]"#;
        assert!(line(&jlvm, &case("h", SUM, "null", broken)).is_err(), "a recipe that does not build");
    }
}
