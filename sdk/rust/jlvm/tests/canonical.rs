//! Reading a program from its canonical form (`from_canonical`, `decode`; jlvm.md section 1, "Reading a canonical
//! form"): the refusals and their kinds, as the reference's `Parser.fromCanonical` and `Parser.decode` give them, and
//! programs that read back as themselves. `jlvm_v1.json`'s `decodings` (tests/vectors.rs) pin the order.

#![allow(clippy::panic, clippy::unwrap_used, reason = "tests fail by panicking")]

extern crate alloc;

use alloc::collections::BTreeMap;

use dilf4s_core::built::{Step, build};
use dilf4s_core::codec::Model as _;
use dilf4s_core::generated::jlvm::{
    JlvmApply, JlvmArray, JlvmExpr, JlvmLit, JlvmMap, JlvmNull, JlvmProgram, JlvmSpread, JlvmVar,
};
use dilf4s_core::generated::jlvm_errors::JlvmErrorKind;
use dilf4s_core::{cbor, hex};
use dilf4s_jlvm::{
    Config, Jlvm, MAX_CANONICAL_WEIGHT, Native, Op, Program, Value, decode, from_canonical, parse, parse_value,
};

fn form(expr: JlvmExpr) -> Value {
    JlvmProgram { expr }.to_value().unwrap()
}

fn kind(r: Result<Program, dilf4s_jlvm::JlvmError>) -> Result<Program, JlvmErrorKind> {
    r.map_err(|e| e.kind())
}

fn var(path: &str) -> JlvmExpr {
    JlvmExpr::JlvmVar(JlvmVar { var: path.into(), default: None })
}

fn lit(v: Value) -> JlvmExpr {
    JlvmExpr::JlvmLit(JlvmLit { lit: v })
}

fn text(s: &str) -> Value {
    Value::text(s)
}

fn map(entries: &[(&str, Value)]) -> Value {
    Value::map(entries.iter().map(|(k, v)| (String::from(*k), v.clone()))).unwrap()
}

/// The canonical forms no surface text writes, and forms outside `jlvm.cddl`, are `Malformed`.
#[test]
fn a_form_no_surface_writes_or_outside_the_schema_is_malformed() {
    let spread = |op, arg| JlvmExpr::JlvmSpread(JlvmSpread { spread: op, arg: Box::new(arg) });
    let one = lit(Value::int(1));
    for (what, f) in [
        ("a spread of a non-variadic operation", form(spread(Op::Sub, var("xs")))),
        ("a spread of an argument list", form(spread(Op::Add, JlvmExpr::JlvmArray(JlvmArray { arr: vec![one] })))),
        ("a var path with an empty segment", form(var("a..b"))),
        ("an empty binding name", form(var("$"))),
        ("a root that is not a map", Value::int(1)),
        ("an empty map", map(&[])),
        ("a version other than 1", map(&[("jlvm", Value::int(2)), ("expr", map(&[]))])),
        ("a version that is text", map(&[("jlvm", text("1")), ("expr", map(&[]))])),
        ("no expression", map(&[("jlvm", Value::int(1))])),
        ("a key beside the program's", map(&[("jlvm", Value::int(1)), ("expr", map(&[])), ("x", Value::int(1))])),
        ("an alias in the form", form_of(map(&[("op", text("getKey")), ("args", Value::array(vec![]).unwrap())]))),
        ("an unknown operation", form_of(map(&[("op", text("noop")), ("args", Value::array(vec![]).unwrap())]))),
        ("an operation's arguments not an array", form_of(map(&[("op", text("+")), ("args", map(&[]))]))),
        ("a var path that is not text", form_of(map(&[("var", Value::int(1))]))),
        ("a map expression's fields not a map", form_of(map(&[("map", Value::array(vec![]).unwrap())]))),
        ("a field that is not an expression", form_of(map(&[("map", map(&[("a", Value::int(1))]))]))),
        ("an expression of two alternatives", form_of(map(&[("lit", Value::int(1)), ("var", text("x"))]))),
        ("a lit with a default", form_of(map(&[("lit", Value::int(1)), ("default", Value::int(2))]))),
        ("an expression that is text", form_of(text("x"))),
    ] {
        assert_eq!(kind(from_canonical(&f)), Err(JlvmErrorKind::Malformed), "{what}");
        assert_eq!(kind(decode(&cbor::encode(&f))), Err(JlvmErrorKind::Malformed), "{what}, from its CBOR");
    }
}

fn form_of(expr: Value) -> Value {
    map(&[("jlvm", Value::int(1)), ("expr", expr)])
}

/// Bytes are decoded strictly first: what is not canonical CBOR is `Malformed`, and nesting past the depth bound is
/// `Limit`, however it is reached.
#[test]
fn bytes_that_are_not_canonical_cbor_are_refused_first() {
    let program = parse(r#"{"+": [1, 2]}"#).unwrap();
    let bytes = program.encode().unwrap();
    let mut trailing = bytes.clone();
    trailing.push(0xf6);
    // the version, 1, written in two bytes
    let long = hex::encode(&bytes).replace("646a6c766d01", "646a6c766d1801");
    for (what, b) in [
        ("an empty map", vec![0xa0]),
        ("not CBOR", vec![0xff]),
        ("nothing", vec![]),
        ("trailing bytes", trailing),
        ("an integer not in its shortest form", hex::decode(&long).unwrap()),
    ] {
        assert_eq!(kind(decode(&b)), Err(JlvmErrorKind::Malformed), "{what}");
    }
    let deep = |n: usize| [vec![0x81; n], vec![0xf6]].concat();
    assert_eq!(kind(decode(&deep(256))), Err(JlvmErrorKind::Malformed), "256 arrays: canonical, but no program");
    assert_eq!(kind(decode(&deep(257))), Err(JlvmErrorKind::Limit), "257 arrays: past the depth bound");
    assert_eq!(kind(decode(&bytes)), Ok(program));
}

/// A form built with shared parts that weighs past the guard is `Limit` from its cached weight, without being
/// expanded: `[x, x]` nested 60 deep is 2^60 leaves in 60 arrays, which no reader could walk (the reference's test).
#[test]
fn an_overweight_form_is_refused_from_its_weight_without_being_expanded() {
    let pair = hex::decode("8261246124").unwrap();
    let envelope = cbor::encode(&form_of(map(&[("arr", text("$"))])));
    let f = build(&[0xf6], &[Step::Wrap { template: &pair, times: 60 }, Step::Wrap { template: &envelope, times: 1 }])
        .unwrap();
    assert!(f.weight() > MAX_CANONICAL_WEIGHT, "the form is overweight");
    assert_eq!(kind(from_canonical(&f)), Err(JlvmErrorKind::Limit));
}

/// Forms no surface text writes but that are programs are read: a literal of `0x` text and a non-terminating
/// rational have no rendering, yet a canonical form and an identity.
#[test]
fn a_program_with_no_surface_form_is_read() {
    for v in [text("0xab"), Value::number(&1.into(), &3.into()).unwrap()] {
        let p = from_canonical(&form(lit(v.clone()))).unwrap();
        assert_eq!(p.canonical().unwrap(), form(lit(v)));
    }
}

/// The null literal is the empty record, and a map expression's fields are its own: a field named as an operation,
/// or holding the null literal, reads back as written.
#[test]
fn the_null_literal_and_map_expressions_read_back() {
    let fields = BTreeMap::from([
        (String::from("+"), JlvmExpr::JlvmNull(JlvmNull)),
        (String::from("aa"), lit(Value::int(1))),
        (String::from("b"), JlvmExpr::JlvmApply(JlvmApply { op: Op::Add, args: vec![lit(Value::int(2))] })),
    ]);
    let f = form(JlvmExpr::JlvmMap(JlvmMap { map: fields }));
    let p = from_canonical(&f).unwrap();
    assert_eq!(p.canonical().unwrap(), f);
    assert_eq!(p, parse(r#"{"+": null, "aa": 1, "b": {"+": [2]}}"#).unwrap());
}

/// A program decoded from its CBOR is the program parsed: it evaluates alike, with the same result and gas.
#[test]
fn a_decoded_program_evaluates_as_the_parsed_one() {
    let jlvm = Jlvm::new(Native::new().unwrap());
    let data = parse_value(r#"{"xs": [1, 2, 3], "k": "b"}"#).unwrap();
    for text in [
        r#"{"b": {"reduce": [{"var": "xs"}, {"+": [{"var": "$current"}, {"var": "$accumulator"}]}, 0]}, "aa": 2}"#,
        r#"{"let": [[["k", {"var": "k"}]], {"get": [{"object": {"b": 1, "aa": 2}}, {"var": "$k"}]}]}"#,
        r#"{"+": {"var": "xs"}}"#,
        r#"{"var": [["xs", 1], 9]}"#,
    ] {
        let parsed = parse(text).unwrap();
        let decoded = decode(&parsed.encode().unwrap()).unwrap();
        let env = BTreeMap::new();
        let config = Config::default();
        assert_eq!(
            jlvm.evaluate(&decoded, &data, &env, &config),
            jlvm.evaluate(&parsed, &data, &env, &config),
            "{text}"
        );
    }
}

/// Programs at the depth bound read back from their CBOR: 127 nested operations or arrays, each two levels of the
/// canonical form (its map and its array); one more is past the bound.
#[test]
fn programs_at_the_depth_bound_read_back() {
    let ops = |n: usize| format!("{}{{\"var\": \"x\"}}{}", "{\"!!\": [".repeat(n), "]}".repeat(n));
    let arrays = |n: usize| format!("{}1{}", "[".repeat(n), "]".repeat(n));
    for text in [ops(127), arrays(127)] {
        let p = parse(&text).unwrap_or_else(|e| panic!("{e}"));
        assert_eq!(decode(&p.encode().unwrap()), Ok(p));
    }
    for text in [ops(128), arrays(128)] {
        assert_eq!(kind(parse(&text)), Err(JlvmErrorKind::Limit));
    }
}
