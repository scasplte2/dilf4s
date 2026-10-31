//! Values share structure: clones are O(1) pointer copies, and the cached weight is the logical (unshared) weight.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    reason = "tests fail by panicking, and run with std"
)]

mod common;

use std::time::Instant;

use common::{bytes, load, read_vectors, vectors_dir};
use dilf4s_core::generated::canonical_vectors::{CanonicalJsonCase, CanonicalVectors};
use dilf4s_core::{BigInt, CanonicalError, MAX_DEPTH, MAX_WEIGHT, Value, cbor, json};

/// Java's `BigInteger.bitLength`, computed differently from core: a negative `n` has the bit length of `-n - 1`.
fn java_bit_length(n: &BigInt) -> BigInt {
    let m = if *n < BigInt::from(0) { -n - BigInt::from(1) } else { n.clone() };
    BigInt::from(m.bits())
}

/// The weight from scratch, exactly and without saturation, as the Scala suite's `recount`.
fn recount(v: &Value) -> BigInt {
    let limbs = |n: &BigInt| java_bit_length(n) / 64;
    match v {
        Value::Null | Value::Bool(_) => BigInt::from(1),
        Value::Int(n) => 1 + limbs(n),
        Value::Rational(r) => 2 + limbs(r.numerator()) + limbs(r.denominator()),
        Value::Text(t) => BigInt::from(1 + t.len()),
        Value::Bytes(b) => BigInt::from(1 + b.len()),
        Value::Array(a) => a.items().iter().fold(BigInt::from(1), |w, x| w + recount(x)),
        Value::Map(m) => m.entries().iter().fold(BigInt::from(1), |w, (k, x)| w + 1 + k.len() + recount(x)),
    }
}

fn saturated(w: &BigInt) -> u64 {
    u64::try_from(w).unwrap_or(MAX_WEIGHT).min(MAX_WEIGHT)
}

/// Every value in the shared vectors: each file as one JSON value, and every accepted canonical case decoded.
fn vector_values() -> Vec<(String, Value)> {
    let mut out = Vec::new();
    let mut files: Vec<_> = std::fs::read_dir(vectors_dir()).unwrap().map(|e| e.unwrap().file_name()).collect();
    files.sort();
    for file in files {
        let name = file.to_string_lossy().into_owned();
        if std::path::Path::new(&name).extension().is_some_and(|e| e == "json") {
            out.push((name.clone(), json::parse(&read_vectors(&name)).unwrap_or_else(|e| panic!("{name}: {e}"))));
        }
    }
    let v: CanonicalVectors = load("canonical_encoding_v1.json");
    for case in &v.json {
        if let CanonicalJsonCase::CanonicalJsonAccepted(c) = case {
            out.push((c.json.clone(), cbor::decode(&bytes(&c.cbor)).unwrap()));
        }
    }
    for c in &v.model {
        out.push((c.value.clone(), cbor::decode(&bytes(&c.cbor)).unwrap()));
    }
    out
}

/// Every sub-value of `v`, `v` first.
fn walk<'a>(v: &'a Value, out: &mut Vec<&'a Value>) {
    out.push(v);
    match v {
        Value::Array(a) => a.items().iter().for_each(|x| walk(x, out)),
        Value::Map(m) => m.entries().iter().for_each(|(_, x)| walk(x, out)),
        _ => {}
    }
}

#[test]
fn cached_weight_matches_a_recount_on_every_vector() {
    let values = vector_values();
    let mut checked = 0usize;
    for (name, root) in &values {
        let mut all = Vec::new();
        walk(root, &mut all);
        for v in all {
            let expected = saturated(&recount(v));
            assert_eq!(v.weight(), expected, "{name}: weight of {v:?}");
            checked += 1;
        }
    }
    eprintln!("checked the weight of {checked} values from {} roots", values.len());
    assert!(checked > 1000, "too few values checked: {checked}");
}

#[test]
fn number_weights_follow_java_bit_length() {
    let two = BigInt::from(2);
    let pow = |e: u32| num_traits_pow(&two, e);
    let cases: Vec<(BigInt, u64)> = vec![
        (BigInt::from(0), 1),
        (BigInt::from(-1), 1),
        (pow(63) - 1, 1),
        (pow(63), 2),
        (-pow(63), 1),
        (-pow(63) - 1, 2),
        (pow(64), 2),
        (-pow(64), 2),
        (-pow(127), 2),
        (-pow(128), 3),
    ];
    for (n, w) in cases {
        let v = Value::int(n.clone());
        assert_eq!(v.weight(), w, "weight of {n}");
        assert_eq!(v.weight(), saturated(&recount(&v)), "recount of {n}");
    }
    // a negative power-of-two numerator: Java counts 63 bits, so no whole limb
    let r = Value::number(&-pow(63), &BigInt::from(3)).unwrap();
    assert!(matches!(r, Value::Rational(_)), "a rational");
    assert_eq!(r.weight(), 2, "weight of -2^63/3");
    let r = Value::number(&BigInt::from(1), &pow(64)).unwrap();
    assert_eq!(r.weight(), 3, "weight of 1/2^64");
    assert_eq!(Value::text("é😀").weight(), 7, "text weighs its UTF-8 bytes");
}

fn num_traits_pow(base: &BigInt, e: u32) -> BigInt {
    (0..e).fold(BigInt::from(1), |acc, _| acc * base)
}

#[test]
fn a_doubling_dag_weighs_its_logical_size_and_saturates() {
    // text "a" weighs 2; [x, x] weighs 1 + 2w, so after k doublings 3·2^k - 1, past i64::MAX from k = 62
    let mut v = Value::text("a");
    for k in 1..=MAX_DEPTH {
        v = Value::array(vec![v.clone(), v]).unwrap();
        let expected = if k <= 61 { 3 * (1u64 << k) - 1 } else { MAX_WEIGHT };
        assert_eq!(v.weight(), expected, "weight after {k} doublings");
        assert_eq!(v.depth(), k, "depth after {k} doublings");
    }
    assert_eq!(v.weight(), MAX_WEIGHT, "saturated, not wrapped");
    // never format or compare the DAG itself: either walks all 2^256 leaves
    let too_deep = Value::array(vec![v.clone(), v]).map(|_| ());
    assert_eq!(too_deep, Err(CanonicalError::DepthExceeded { limit: MAX_DEPTH }), "one doubling too many");

    // maps add one per entry and the key's bytes: {a: x, b: x} weighs 1 + 2(2 + w)
    let mut m = Value::Null;
    let mut expected: u64 = 1;
    for k in 1..=MAX_DEPTH {
        m = Value::map([("a".to_owned(), m.clone()), ("b".to_owned(), m)]).unwrap();
        expected = if k == 1 { 1 } else { expected.saturating_mul(2).saturating_add(5).min(MAX_WEIGHT) };
        assert_eq!(m.weight(), expected, "map weight after {k} doublings");
    }
    assert_eq!(m.weight(), MAX_WEIGHT, "map weight saturated");
}

#[test]
fn clones_share_their_payloads() {
    let text = Value::text("shared text");
    let bytes = Value::bytes(vec![1u8, 2, 3]);
    let int = Value::int(BigInt::from(7) << 200);
    let rational = Value::number(&BigInt::from(1), &BigInt::from(3)).unwrap();
    let array = Value::array(vec![text.clone(), int.clone()]).unwrap();
    let map = Value::map([("k".to_owned(), array.clone()), ("r".to_owned(), rational.clone())]).unwrap();
    let all = [text, bytes, int, rational, array, map];
    for v in &all {
        let c = v.clone();
        let shared = match (v, &c) {
            (Value::Text(a), Value::Text(b)) => std::ptr::eq(a.as_str(), b.as_str()),
            (Value::Bytes(a), Value::Bytes(b)) => std::sync::Arc::ptr_eq(a, b),
            (Value::Int(a), Value::Int(b)) => std::ptr::eq(a.as_bigint(), b.as_bigint()),
            (Value::Rational(a), Value::Rational(b)) => std::ptr::eq(a.numerator(), b.numerator()),
            (Value::Array(a), Value::Array(b)) => std::ptr::eq(a.items(), b.items()),
            (Value::Map(a), Value::Map(b)) => std::ptr::eq(a.entries(), b.entries()),
            _ => false,
        };
        assert!(shared, "a clone of the {} shares its payload", v.kind());
        assert_eq!(*v, c, "and is equal");
        assert_eq!(v.weight(), c.weight(), "with the same weight");
    }
    // the elements of an array are the values put in, not copies of them
    let (Value::Text(t), Value::Array(a)) = (&all[0], &all[4]) else { panic!("text and array") };
    let Value::Text(inner) = &a.items()[0] else { panic!("text element") };
    assert!(std::ptr::eq(t.as_str(), inner.as_str()), "an element shares the text it was built from");
}

#[test]
fn equality_is_on_content_not_identity() {
    let a = Value::array(vec![Value::text("x"), Value::int(1)]).unwrap();
    let b = Value::array(vec![Value::text("x"), Value::int(1)]).unwrap();
    let (Value::Array(pa), Value::Array(pb)) = (&a, &b) else { panic!("arrays") };
    assert!(!std::ptr::eq(pa.items(), pb.items()), "built separately");
    assert_eq!(a, b, "equal by content");
    assert_ne!(a, Value::array(vec![Value::text("x"), Value::int(2)]).unwrap(), "different content");
    assert_eq!(cbor::encode(&a), cbor::encode(&b), "one encoding");
}

/// The resident set size in KiB, where `/proc` has it.
fn rss_kib() -> Option<u64> {
    let status = std::fs::read_to_string("/proc/self/status").ok()?;
    let line = status.lines().find(|l| l.starts_with("VmRSS:"))?;
    line.split_whitespace().nth(1)?.parse().ok()
}

#[test]
fn cloning_a_large_value_many_times_does_not_copy_it() {
    const MIB: usize = 1 << 20;
    const CLONES: usize = 10_000;
    let big = Value::text("x".repeat(MIB));
    let nested = Value::array(vec![big.clone(); 16]).unwrap();
    let before = rss_kib();
    let start = Instant::now();
    let clones: Vec<Value> = (0..CLONES).map(|_| big.clone()).collect();
    let nested_clones: Vec<Value> = (0..CLONES).map(|_| nested.clone()).collect();
    let elapsed = start.elapsed();
    let after = rss_kib();
    eprintln!("{CLONES} clones of a 1 MiB text and of a 16 MiB array: {elapsed:?}, RSS {before:?} -> {after:?} KiB");
    let Value::Text(original) = &big else { panic!("text") };
    for c in &clones {
        let Value::Text(t) = c else { panic!("text") };
        assert!(std::ptr::eq(original.as_str(), t.as_str()), "every clone shares the buffer");
    }
    assert_eq!(nested_clones.len(), CLONES);
    assert_eq!(nested.weight(), 16 * (u64::try_from(MIB).unwrap() + 1) + 1, "logical weight of the shared array");
    if let (Some(b), Some(a)) = (before, after) {
        // a deep copy would be 10,000 MiB; the clones themselves are 2 × 10,000 × 40 bytes
        assert!(a.saturating_sub(b) < 64 * 1024, "RSS grew by {} KiB", a.saturating_sub(b));
    }
    assert!(elapsed.as_secs() < 5, "{CLONES} clones took {elapsed:?}");
}
