//! The conformance vectors `shared/vectors/jlvm_v1.json`, run as the reference's `JlvmVectors.run` runs them: parse the
//! program, the data and the host bindings (the first failure is the outcome, with no gas), add the bindings built
//! with shared parts (`built`, which no JSON text could carry), then evaluate with the
//! case's gas limit (the file's when absent), the latest schedule and the native cryptographic backend. Every case must
//! give the same value (canonical CBOR) or error (its kind and `op`), the same gas and the same number of warnings.
//!
//! `JLVM_CASE=<substring>` runs only the matching cases and prints each outcome.

#![allow(clippy::panic, clippy::print_stdout, reason = "tests report by printing and fail by panicking")]

extern crate alloc;

#[path = "../../core/tests/common/mod.rs"]
mod common;

use alloc::collections::BTreeMap;
use std::path::PathBuf;

use common::{Step, build};
use dilf4s_core::codec::Model;
use dilf4s_core::generated::jlvm_vectors::{BuiltStep, BuiltValue, JlvmCase, JlvmErrorKind, JlvmOutcome, JlvmVectors};
use dilf4s_core::{cbor, hex, json};
use dilf4s_jlvm::{
    Compiled, Config, Gas, GasLimit, GasSchedule, Jlvm, JlvmError, Native, Op, Outcome, Site, Value, parse, parse_value,
};

fn load() -> JlvmVectors {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../shared/vectors/jlvm_v1.json");
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    let value = json::parse(&text).unwrap_or_else(|e| panic!("jlvm_v1.json is not canonical JSON: {e}"));
    JlvmVectors::from_value(&value).unwrap_or_else(|e| panic!("jlvm_v1.json does not fit its schema: {e}"))
}

/// A `built-value` binding, built with sharing.
fn built(b: &BuiltValue) -> Value {
    let steps: Vec<Step<'_>> = b
        .steps
        .iter()
        .flatten()
        .map(|s| match s {
            BuiltStep::BuiltWrap(w) => Step::Wrap { template: &w.template, times: w.times },
            BuiltStep::BuiltCopies(c) => Step::Copies(c.copies),
        })
        .collect();
    build(&b.leaf, &steps)
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
    for (k, b) in case.built.iter().flatten() {
        env.insert(k.clone(), built(b));
    }
    let config =
        Config { limit: GasLimit(Gas::new(case.gas_limit.unwrap_or(default_limit))), schedule: GasSchedule::LATEST };
    jlvm.evaluate(&program, &data, &env, &config)
}

/// The schema's kind of an SDK error. Exhaustive, so a new variant does not compile until it is mapped.
const fn kind(e: &JlvmError) -> JlvmErrorKind {
    match e {
        JlvmError::Malformed(_) => JlvmErrorKind::Malformed,
        JlvmError::Type { .. } => JlvmErrorKind::Type,
        JlvmError::Arity { .. } => JlvmErrorKind::Arity,
        JlvmError::Arithmetic { .. } => JlvmErrorKind::Arithmetic,
        JlvmError::Domain { .. } => JlvmErrorKind::Domain,
        JlvmError::Crypto { .. } => JlvmErrorKind::Crypto,
        JlvmError::Limit(_) => JlvmErrorKind::Limit,
        JlvmError::OutOfGas { .. } => JlvmErrorKind::OutOfGas,
    }
}

/// What a case's outcome must be, rendered for comparison: a value, or an error's kind and normative `op`.
fn expected(case: &JlvmCase) -> String {
    match &case.outcome {
        JlvmOutcome::JlvmValue(v) => format!("value {}", v.cbor),
        JlvmOutcome::JlvmError(e) => format!("error {:?} op {:?}", e.error, e.op.as_deref()),
    }
}

fn actual(o: &Outcome) -> String {
    match &o.result {
        Ok(v) => format!("value {}", hex::encode(&cbor::encode(v))),
        Err(e) => format!("error {:?} op {:?}", kind(e), e.site().map(Site::wire)),
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

/// Whether a kind carries a normative `op` (jlvm.md section 3). Exhaustive, so a new kind must decide.
const fn has_site(k: &JlvmErrorKind) -> bool {
    match k {
        JlvmErrorKind::Type
        | JlvmErrorKind::Arity
        | JlvmErrorKind::Arithmetic
        | JlvmErrorKind::Domain
        | JlvmErrorKind::Crypto => true,
        JlvmErrorKind::Malformed | JlvmErrorKind::Limit | JlvmErrorKind::OutOfGas => false,
    }
}

/// The coverage gate (conformance.md section 6) for `JlvmError`: every kind of the schema has a row. [`kind`] maps
/// every SDK variant onto a schema kind, so every SDK kind has a row too. Each row's `op` is present exactly for the
/// kinds that carry one, and names a site the SDK has: an operation's wire name, or `var`, which has a row of its own.
#[test]
fn every_jlvm_error_kind_has_a_row() {
    let vectors = load();
    let errors: Vec<_> = vectors
        .cases
        .iter()
        .filter_map(|c| match &c.outcome {
            JlvmOutcome::JlvmError(e) => Some((c.name.as_str(), e)),
            JlvmOutcome::JlvmValue(_) => None,
        })
        .collect();
    for k in JlvmErrorKind::ALL {
        assert!(errors.iter().any(|(_, e)| e.error == k), "no jlvm_v1.json row has error {k:?}");
    }
    for (name, e) in &errors {
        assert_eq!(has_site(&e.error), e.op.is_some(), "{name}: {:?} with op {:?}", e.error, e.op);
        if let Some(op) = &e.op {
            assert!(op == Site::Var.wire() || Op::from_canonical(op).is_some(), "{name}: op {op:?} is not a site");
        }
    }
    assert!(errors.iter().any(|(_, e)| e.op.as_deref() == Some(Site::Var.wire())), "no row has op var");
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
