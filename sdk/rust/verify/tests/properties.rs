//! The properties on concrete inputs: every byte string of up to two bytes, the conformance vectors, and edge cases.
//! The Kani harnesses (`src/proofs.rs`) run the same functions on every input up to their bounds.

#![allow(
    clippy::panic,
    clippy::indexing_slicing,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    reason = "tests fail by panicking, and run with std"
)]

#[path = "../../core/tests/common/mod.rs"]
mod common;

use std::path::PathBuf;

use common::{bytes, load};
use dilf4s_core::codec::Model;
use dilf4s_core::generated::canonical_vectors::{CanonicalJsonCase, CanonicalVectors};
use dilf4s_core::generated::jlvm_vectors::JlvmVectors;
use dilf4s_core::json::DEFAULT_MAX_SCALE;
use dilf4s_core::{BigInt, Value, cbor, json};
use dilf4s_jlvm::{Gas, Op, OpClass};
use dilf4s_verify::{Broken, canonical, gas, helpers, program};

/// Every byte string of at most two bytes.
fn short_byte_strings() -> impl Iterator<Item = Vec<u8>> {
    let one = (0..=u8::MAX).map(|a| vec![a]);
    let two = (0..=u8::MAX).flat_map(|a| (0..=u8::MAX).map(move |b| vec![a, b]));
    core::iter::once(Vec::new()).chain(one).chain(two)
}

fn check(what: &str, input: &str, result: Result<(), Broken>) {
    if let Err(broken) = result {
        panic!("{what} of {input}: {broken:?}");
    }
}

#[test]
fn every_short_byte_string() {
    for b in short_byte_strings() {
        let shown = format!("{b:02x?}");
        check("cbor_decode", &shown, canonical::cbor_decode(&b));
        for depth in [0, 1, 2, usize::MAX] {
            check("cbor_depth_limit", &shown, canonical::cbor_depth_limit(&b, depth));
        }
        for max in [0, 1, 2, 4, 8, usize::MAX] {
            check("json_render", &shown, canonical::json_render(&b, max));
        }
        check("program_decode", &shown, program::program_decode(&b));
        check("hex_round_trip", &shown, helpers::hex_round_trip(&b));
        if let Ok(text) = core::str::from_utf8(&b) {
            for scale in [0, 1, 4, DEFAULT_MAX_SCALE] {
                check("json_parse", &shown, canonical::json_parse(text, scale));
            }
            check("program_parse", &shown, program::program_parse(text));
            program::data_parse(text);
            check("hex_decode", &shown, helpers::hex_decode(text));
            check("preview", &shown, helpers::preview(text));
            check("text_measures", &shown, helpers::text_measures(text));
            check("op_wire", &shown, helpers::op_wire(text));
            check("key_order", &shown, helpers::key_order(text, "a"));
        }
    }
}

#[test]
fn canonical_vectors() {
    let v: CanonicalVectors = load("canonical_encoding_v1.json");
    let mut accepted = 0;
    for case in &v.json {
        let text = match case {
            CanonicalJsonCase::CanonicalJsonAccepted(c) => {
                let encoded = bytes(&c.cbor);
                check("cbor_decode", &c.cbor, canonical::cbor_decode(&encoded));
                check("cbor_depth_limit", &c.cbor, canonical::cbor_depth_limit(&encoded, 1));
                for max in [0, 1, c.render.len().saturating_sub(1), c.render.len(), usize::MAX] {
                    check("json_render", &c.cbor, canonical::json_render(&encoded, max));
                }
                check("program_decode", &c.cbor, program::program_decode(&encoded));
                accepted += 1;
                &c.json
            }
            CanonicalJsonCase::CanonicalJsonRejected(c) => &c.json,
        };
        let shown: String = text.chars().take(80).collect();
        check("json_parse", &shown, canonical::json_parse(text, DEFAULT_MAX_SCALE));
        check("json_parse", &shown, canonical::json_parse(text, 2));
        check("program_parse", &shown, program::program_parse(text));
        check("preview", &shown, helpers::preview(text));
    }
    for r in &v.cbor_rejections {
        check("cbor_decode", &r.cbor, canonical::cbor_decode(&bytes(&r.cbor)));
    }
    for r in &v.render_rejections {
        check("json_render", &r.cbor, canonical::json_render(&bytes(&r.cbor), usize::MAX));
    }
    for m in &v.model {
        check("cbor_decode", &m.cbor, canonical::cbor_decode(&bytes(&m.cbor)));
    }
    assert!(accepted > 50, "{accepted} accepted JSON cases");
}

#[test]
fn jlvm_vectors() {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../shared/vectors/jlvm_v1.json");
    let text = std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()));
    let value = json::parse(&text).unwrap_or_else(|e| panic!("jlvm_v1.json: {e}"));
    let v = JlvmVectors::from_value(&value).unwrap_or_else(|e| panic!("jlvm_v1.json: {e}"));
    let mut parsed = 0;
    for case in &v.cases {
        check("program_parse", &case.name, program::program_parse(&case.program));
        program::data_parse(&case.data);
        if let Ok(p) = dilf4s_jlvm::parse(&case.program) {
            let encoded = p.encode().unwrap_or_else(|e| panic!("{}: {e}", case.name));
            check("program_decode", &case.name, program::program_decode(&encoded));
            parsed += 1;
        }
    }
    assert!(parsed > 500, "{parsed} programs parsed");
}

#[test]
fn json_edge_cases() {
    let texts = [
        "0",
        "-0",
        "1e0",
        "1E+2",
        "1e-2",
        "0.50",
        "-0.000",
        "1.5e1",
        "\"\\u0000\"",
        "\"\\ud800\"",
        "\"\\ud800\\udc00\"",
        "{\"a\":null}",
        "{\"a\":1,\"a\":2}",
        "[[[]]]",
        "{\"b\":1,\"a\":[1.25,-3]}",
        "\"\u{1f600}\"",
        " [ 1 , 2 ] ",
        "1e400",
        "1e-400",
        "123456789012345678901234567890",
        "0.1e1",
        "9e9999",
    ];
    for t in texts {
        for scale in [0, 2, 400, DEFAULT_MAX_SCALE] {
            check("json_parse", t, canonical::json_parse(t, scale));
        }
        check("program_parse", t, program::program_parse(t));
        program::data_parse(t);
    }
}

#[test]
fn gas_edges() {
    let edges = [0, 1, 2, 3, gas::MAX / 2, gas::MAX - 1, gas::MAX, gas::MAX + 1, u64::MAX - 1, u64::MAX];
    for a in edges {
        for b in edges {
            for n in edges {
                check("gas_clamp", &format!("{a} {b} {n}"), gas::gas_clamp(a, b, n));
                check("gas_meter", &format!("{a} {b} {n}"), gas::gas_meter(a, b, n));
            }
        }
    }
    let samples: Vec<Vec<u8>> =
        ["", "f6", "01", "3903e7", "6161", "a1616101", "8201a0", "d81e820102", "c249010000000000000000"]
            .iter()
            .map(|h| bytes(h))
            .collect();
    let values: Vec<Value> = samples.iter().map(|b| cbor::decode(b).unwrap_or(Value::Null)).collect();
    for op in Op::ALL {
        for (i, a) in values.iter().enumerate() {
            for b in &values[i..] {
                let args = [a.clone(), b.clone(), a.clone()];
                for n in 0..=3 {
                    for cap in [0, 1, 50, 100, 1000, gas::MAX] {
                        check("gas_cap", op.wire(), gas::gas_cap(op, &args[..n], Gas::new(cap)));
                    }
                }
            }
        }
    }
    for (i, a) in values.iter().enumerate() {
        check("gas_rendered", &samples[i].len().to_string(), gas::gas_rendered(core::slice::from_ref(a)));
        check("gas_rendered", "all", gas::gas_rendered(&values));
    }
    let wide = [Value::int(i128::MIN), Value::number(&BigInt::from(-1), &BigInt::from(5u8).pow(30)).unwrap()];
    check("gas_rendered", "wide", gas::gas_rendered(&wide));
}

/// The properties compare values by their canonical encodings, as the identity of a value; `Value`'s own equality
/// agrees with that on every pair of values here.
#[test]
fn encodings_are_identity() {
    let v: CanonicalVectors = load("canonical_encoding_v1.json");
    let values: Vec<Value> = v
        .json
        .iter()
        .filter_map(|c| match c {
            CanonicalJsonCase::CanonicalJsonAccepted(c) => cbor::decode(&bytes(&c.cbor)).ok(),
            CanonicalJsonCase::CanonicalJsonRejected(_) => None,
        })
        .collect();
    for a in &values {
        for b in &values {
            assert_eq!(a == b, cbor::encode(a) == cbor::encode(b), "{a:?} and {b:?}");
        }
    }
}

#[test]
fn helper_edges() {
    for n in [0, 1, -1, 2, -2, i128::from(i64::MIN), i128::from(u64::MAX), i128::MAX, i128::MIN, i128::MIN + 1] {
        check("bit_length", &n.to_string(), helpers::bit_length(n));
    }
    let long = "\u{e9}".repeat(40);
    for t in ["", "a", &"x".repeat(32), &"x".repeat(33), &long, "a\u{1f600}b"] {
        check("preview", t, helpers::preview(t));
        check("text_measures", t, helpers::text_measures(t));
        check("key_order", t, helpers::key_order(t, "ab"));
        check("key_order", t, helpers::key_order("\u{e9}", t));
    }
    for op in Op::ALL {
        assert_eq!(Op::from_wire(op.wire()), Some(op), "{} reads back", op.wire());
        assert_eq!(<Op as OpClass>::from_name(op.wire()), Some(op), "{} reads back by name", op.wire());
        check("op_wire", op.wire(), helpers::op_wire(op.wire()));
    }
    for alias in ["getKey", "setKey", "deleteKey", "size"] {
        check("op_wire", alias, helpers::op_wire(alias));
    }
    let records: [(&[&str], &[&str]); 3] = [(&["a"], &["a"]), (&["a"], &["a", "b"]), (&[], &["c"])];
    let maps = [vec![], vec!["a"], vec!["a", "b"], vec!["b"], vec!["c"]];
    for keys in maps {
        let m = Value::map(keys.iter().map(|k| ((*k).to_owned(), Value::Bool(true)))).unwrap_or(Value::Null);
        check("alternative", &format!("{keys:?}"), helpers::alternative(&m, &records, &["a", "b"]));
    }
    for v in [Value::text("b"), Value::text("z"), Value::Null] {
        check("alternative", v.kind(), helpers::alternative(&v, &records, &["a", "b"]));
    }
}
