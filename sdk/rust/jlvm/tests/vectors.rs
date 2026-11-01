//! The conformance vectors `shared/vectors/jlvm_v1.json`, run as the reference's `JlvmVectors.run` runs them: parse the
//! program, the data and the host bindings (the first failure is the outcome, with no gas), then evaluate with the
//! case's gas limit (the file's when absent), the latest schedule and the native cryptographic backend. Every case must
//! give the same value (canonical CBOR) or error kind, the same gas and the same number of warnings.
//!
//! `JLVM_CASE=<substring>` runs only the matching cases and prints each outcome.

#![allow(clippy::panic, clippy::print_stdout, reason = "tests report by printing and fail by panicking")]

extern crate alloc;

use alloc::collections::BTreeMap;
use std::path::PathBuf;

use dilf4s_core::codec::Model;
use dilf4s_core::generated::jlvm_vectors::{JlvmCase, JlvmOutcome, JlvmVectors};
use dilf4s_core::{cbor, hex, json};
use dilf4s_jlvm::{
    Compiled, Config, Gas, GasLimit, GasSchedule, Jlvm, JlvmError, Native, Outcome, Value, parse, parse_value,
};

fn load() -> JlvmVectors {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../shared/vectors/jlvm_v1.json");
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    let value = json::parse(&text).unwrap_or_else(|e| panic!("jlvm_v1.json is not canonical JSON: {e}"));
    JlvmVectors::from_value(&value).unwrap_or_else(|e| panic!("jlvm_v1.json does not fit its schema: {e}"))
}

/// `JlvmVectors.run`: the first parse failure (program, then data, then bindings) is the outcome, with no gas.
fn run(jlvm: &Jlvm<Native>, case: &JlvmCase, default_limit: u64) -> Outcome {
    let failed = |e: JlvmError| Outcome { gas_used: Gas::ZERO, result: Err(e), warnings: Vec::new() };
    let program = match parse(&case.program) {
        Ok(p) => p,
        Err(e) => return failed(e),
    };
    let data = match parse_value(&case.data) {
        Ok(d) => d,
        Err(e) => return failed(e),
    };
    let mut env = BTreeMap::new();
    for (k, v) in case.env.iter().flatten() {
        match parse_value(v) {
            Ok(x) => {
                env.insert(k.clone(), x);
            }
            Err(e) => return failed(e),
        }
    }
    let config =
        Config { limit: GasLimit(Gas::new(case.gas_limit.unwrap_or(default_limit))), schedule: GasSchedule::LATEST };
    jlvm.evaluate(&program, &data, &env, &config)
}

/// What a case's outcome must be, rendered for comparison.
fn expected(case: &JlvmCase) -> String {
    match &case.outcome {
        JlvmOutcome::JlvmValue(v) => format!("value {}", v.cbor),
        JlvmOutcome::JlvmError(e) => format!("error {:?}", e.error),
    }
}

fn actual(o: &Outcome) -> String {
    match &o.result {
        Ok(v) => format!("value {}", hex::encode(&cbor::encode(v))),
        Err(e) => format!("error {}", e.kind()),
    }
}

/// Where a case's outcome differs from the vector's, if it does: the bucket and a description.
fn mismatch(case: &JlvmCase, o: &Outcome) -> Option<(&'static str, String)> {
    let (want, got) = (expected(case), actual(o));
    let bucket = if want != got {
        "outcome"
    } else if o.gas_used.value() != case.gas_used {
        "gas"
    } else if u64::try_from(o.warnings.len()).unwrap_or(u64::MAX) != case.warnings {
        "warnings"
    } else {
        return None;
    };
    Some((
        bucket,
        format!(
            "{}: want {want} gas {} warnings {}, got {got} gas {} warnings {} ({:?})",
            case.name,
            case.gas_used,
            case.warnings,
            o.gas_used.value(),
            o.warnings.len(),
            o.result.as_ref().err()
        ),
    ))
}

#[test]
fn conformance_vectors() {
    let vectors = load();
    let filter = std::env::var("JLVM_CASE").ok();
    let jlvm = Jlvm::new(Native::new().unwrap());
    let mut passed = 0usize;
    let mut failures: BTreeMap<&str, Vec<String>> = BTreeMap::new();
    for case in &vectors.cases {
        if filter.as_ref().is_some_and(|f| !case.name.contains(f.as_str())) {
            continue;
        }
        let o = run(&jlvm, case, vectors.gas_limit);
        if filter.is_some() {
            println!("{}: {} gas {} ({:?})", case.name, actual(&o), o.gas_used.value(), o.result.as_ref().err());
        }
        match mismatch(case, &o) {
            None => passed += 1,
            Some((bucket, why)) => failures.entry(bucket).or_default().push(why),
        }
    }
    println!("{passed} of {} cases pass", vectors.cases.len());
    for (bucket, cases) in &failures {
        println!("-- {bucket}: {}", cases.len());
        for c in cases.iter().take(40) {
            println!("   {c}");
        }
    }
    assert!(failures.is_empty(), "{} buckets of failures", failures.len());
    if filter.is_none() {
        assert_eq!(passed, vectors.cases.len());
    }
}

#[test]
fn compiled_programs_evaluate_alike() {
    let p = parse(r#"{"+": [1, {"var": "x"}]}"#).unwrap();
    let c = Compiled::new(p.clone()).unwrap();
    let jlvm = Jlvm::new(Native::new().unwrap());
    let data = parse_value(r#"{"x": 2}"#).unwrap();
    let a = jlvm.evaluate(&p, &data, &BTreeMap::new(), &Config::default());
    let b = jlvm.evaluate_compiled(&c, &data, &BTreeMap::new(), &Config::default());
    assert_eq!(a, b);
    assert_eq!(a.result, Ok(Value::int(3)));
}
