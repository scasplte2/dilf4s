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

/// `[x, x]` nested `k` deep around `leaf`, every level one allocation.
fn doubled(leaf: Value, k: usize) -> Value {
    (0..k).fold(leaf, |v, _| Value::array(vec![v.clone(), v]).unwrap())
}

#[test]
fn every_encoding_is_at_least_its_weight_and_the_budgeted_forms_agree() {
    let mut checked = 0usize;
    for (name, root) in &vector_values() {
        let mut all = Vec::new();
        walk(root, &mut all);
        for v in all {
            let full = cbor::encode(v);
            let n = full.len();
            assert!(u64::try_from(n).unwrap() >= v.weight(), "{name}: {n} bytes under weight {}", v.weight());
            let within = cbor::encode_within(v, n).unwrap();
            assert_eq!(within, full, "{name}: encode_within at the exact length");
            assert!(within.capacity() <= n, "{name}: buffer of {} over the budget {n}", within.capacity());
            assert_eq!(cbor::encoded_len_within(v, n), Ok(n), "{name}: encoded_len_within");
            if n > 0 {
                let over = cbor::encode_within(v, n - 1).unwrap_err();
                assert!(over.at_least.is_some_and(|a| a > n - 1), "{name}: {over:?} reports more than the limit");
                assert_eq!(cbor::encoded_len_within(v, n - 1).unwrap_err().limit, n - 1, "{name}: limit");
            }
            checked += 1;
        }
    }
    assert!(checked > 1000, "too few values checked: {checked}");
}

#[test]
fn budgeted_encoding_refuses_a_shared_expansion_without_expanding_it() {
    let start = Instant::now();
    // 2^60 leaves in 60 allocations: refused by weight alone
    let dag = doubled(Value::text("a"), 60);
    let e = cbor::encode_within(&dag, 1 << 20).unwrap_err();
    assert_eq!(e.limit, 1 << 20, "limit");
    assert_eq!(
        e.at_least.map(|a| u64::try_from(a).unwrap()),
        Some(dag.weight()),
        "the weight is the lower bound reported"
    );
    assert!(cbor::encoded_len_within(&dag, 1 << 20).is_err(), "length refused too");
    // within the weight bound but over in bytes (each u64::MAX is weight 2, 9 bytes): refused by the byte budget
    let ints = doubled(Value::int(u64::MAX), 17);
    assert!(ints.weight() < 1 << 20, "passes the weight check");
    let e = cbor::encode_within(&ints, 1 << 20).unwrap_err();
    assert!(e.at_least.is_some_and(|a| a > 1 << 20), "{e:?}");
    assert_eq!(cbor::encoded_len_within(&ints, 1 << 21), Ok(cbor::encode(&ints).len()), "counted exactly");
    assert!(start.elapsed().as_millis() < 2000, "took {:?}", start.elapsed());
}

#[test]
fn equality_of_shared_dags_never_expands_them() {
    let start = Instant::now();
    // separately allocated, so no pointer is shared between them: 2^60 leaves each
    let a = doubled(Value::text("a"), 60);
    let b = doubled(Value::text("a"), 60);
    assert!(a == b, "content-equal DAGs are equal");
    // one leaf differs, at the bottom of the second half: the first half is proven equal once, then skipped
    let differing = doubled(Value::array(vec![Value::text("a"), Value::text("b")]).unwrap(), 58);
    let half = doubled(Value::text("a"), 59);
    let c = Value::array(vec![half, differing]).unwrap();
    assert_eq!(a.weight(), c.weight(), "same weight, so no early reject");
    assert_eq!(a.depth(), c.depth(), "same depth");
    assert!(a != c, "one differing leaf");
    assert!(c != a, "either way round");
    // maps: {k: x, l: x} nested 60 deep
    let map = |leaf: Value| {
        (0..60).fold(leaf, |v, _| Value::map([("k".to_owned(), v.clone()), ("l".to_owned(), v)]).unwrap())
    };
    assert!(map(Value::int(1)) == map(Value::int(1)), "equal map DAGs");
    assert!(map(Value::int(1)) != map(Value::int(2)), "unequal map DAGs");
    let (Value::Array(first), Value::Array(second)) = (&a, &b) else { panic!("arrays") };
    assert!(first == second, "Array equality shares the algorithm");
    assert!(start.elapsed().as_millis() < 2000, "took {:?}", start.elapsed());
}

#[test]
fn debug_output_is_budgeted() {
    let small = Value::map([("a".to_owned(), Value::array(vec![Value::int(1), Value::text("x")]).unwrap())]).unwrap();
    assert_eq!(
        format!("{small:?}"),
        "Map(Map { entries: [(\"a\", Array(Array { items: [Int(1), Text(\"x\")], depth: 1, weight: 4 }))], depth: 2, \
         weight: 7 })",
        "a small value prints in full"
    );
    let start = Instant::now();
    let dag = doubled(Value::text("a"), 60);
    let shown = format!("{dag:?}");
    assert!(shown.len() < 1 << 21, "{} bytes of output", shown.len());
    assert!(shown.contains('…'), "the rest is elided");
    let big = Value::array(vec![Value::text("x".repeat(1 << 20))]).unwrap();
    let shown = format!("{big:?}");
    assert!(shown.len() < 1000 && shown.contains('…'), "a text over the budget is elided: {shown}");
    assert!(start.elapsed().as_millis() < 2000, "took {:?}", start.elapsed());
}

#[test]
fn equality_compares_each_pair_of_large_payloads_once() {
    const N: usize = 100_000;
    let mib = "x".repeat(1 << 20);
    // separately allocated, equal: without the memo this is N full comparisons of 1 MiB (about 100 GiB)
    let start = Instant::now();
    let texts = |s: &str| Value::array(vec![Value::text(s); N]).unwrap();
    assert!(texts(&mib) == texts(&mib), "equal texts");
    let bytes = |b: &[u8]| Value::array(vec![Value::bytes(b); N]).unwrap();
    assert!(bytes(mib.as_bytes()) == bytes(mib.as_bytes()), "equal byte strings");
    let big = BigInt::from(1) << (1 << 23);
    let ints = |n: &BigInt| Value::array(vec![Value::int(n.clone()); N]).unwrap();
    assert!(ints(&big) == ints(&big), "equal integers");
    let mut other = mib.clone();
    other.replace_range(..1, "y");
    assert!(texts(&mib) != texts(&other), "a differing text");
    assert!(start.elapsed().as_secs() < 5, "took {:?}", start.elapsed());
}

/// Equality as it was before sharing: a full recursive walk.
fn naive_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Array(x), Value::Array(y)) => {
            x.len() == y.len() && x.items().iter().zip(y.items()).all(|(p, q)| naive_eq(p, q))
        }
        (Value::Map(x), Value::Map(y)) => {
            x.len() == y.len()
                && x.entries().iter().zip(y.entries()).all(|((kp, p), (kq, q))| kp == kq && naive_eq(p, q))
        }
        (Value::Array(_) | Value::Map(_), _) | (_, Value::Array(_) | Value::Map(_)) => false,
        _ => a == b,
    }
}

#[test]
fn equality_of_unshared_trees_costs_about_a_walk() {
    // every vector against an independent copy, so no address is shared and every pair is compared
    let pairs: Vec<(Value, Value)> = vector_values()
        .into_iter()
        .map(|(_, v)| {
            let copy = cbor::decode(&cbor::encode(&v)).unwrap();
            (v, copy)
        })
        .collect();
    let start = Instant::now();
    assert!(pairs.iter().all(|(a, b)| a == b), "every copy is equal");
    let memoised = start.elapsed();
    let start = Instant::now();
    assert!(pairs.iter().all(|(a, b)| naive_eq(a, b)), "and equal by a full walk");
    let naive = start.elapsed();
    eprintln!(
        "{} vector roots against copies: memoised equality {memoised:?}, full recursive walk {naive:?}",
        pairs.len()
    );
}

/// CBOR of an integer the plain way, materialising `-1 - n` and its bytes, to check the streamed encoder against.
fn reference_integer(n: &BigInt) -> Vec<u8> {
    let negative = *n < BigInt::from(0);
    let carried = if negative { -n - BigInt::from(1) } else { n.clone() };
    let major: u8 = if negative { 0x20 } else { 0x00 };
    let head = |out: &mut Vec<u8>, mt: u8, arg: u64| {
        let be = arg.to_be_bytes();
        match arg {
            0..=23 => out.push(mt | be[7]),
            24..=0xff => out.extend([mt | 0x18, be[7]]),
            0x100..=0xffff => out.extend([mt | 0x19, be[6], be[7]]),
            0x1_0000..=0xffff_ffff => out.extend([mt | 0x1a, be[4], be[5], be[6], be[7]]),
            _ => out.extend([&[mt | 0x1b][..], &be[..]].concat()),
        }
    };
    let mut out = Vec::new();
    if let Ok(small) = u64::try_from(&carried) {
        head(&mut out, major, small);
    } else {
        let (_, bytes) = carried.to_bytes_be();
        head(&mut out, 0xc0, if negative { 3 } else { 2 });
        head(&mut out, 0x40, u64::try_from(bytes.len()).unwrap());
        out.extend(bytes);
    }
    out
}

#[test]
fn integers_are_streamed_exactly() {
    let two = BigInt::from(2);
    let mut cases = vec![BigInt::from(0), BigInt::from(1), BigInt::from(-1), BigInt::from(23), BigInt::from(-24)];
    for e in [7u32, 8, 31, 32, 63, 64, 65, 127, 128, 129, 191, 192, 256, 1000, 4096] {
        let p = num_traits_pow(&two, e);
        for n in [p.clone() - 1, p.clone(), p.clone() + 1] {
            cases.push(-n.clone());
            cases.push(n);
        }
        // a power of two times a non-trivial low pattern, and a negative with zero low limbs
        cases.push(-(p.clone() * BigInt::from(3) + num_traits_pow(&two, e / 2)));
        cases.push(-(p * BigInt::from(5)));
    }
    for n in &cases {
        let v = Value::int(n.clone());
        let expected = reference_integer(n);
        assert_eq!(cbor::encode(&v), expected, "encoding of {n}");
        assert_eq!(cbor::decode(&expected).unwrap(), v, "decodes back: {n}");
        assert_eq!(cbor::encoded_len_within(&v, expected.len()), Ok(expected.len()), "counted: {n}");
        let r = Value::number(n, &BigInt::from(7)).unwrap();
        assert_eq!(cbor::decode(&cbor::encode(&r)).unwrap(), r, "a rational round-trips: {n}/7");
    }
}

#[test]
fn an_integer_over_the_budget_is_refused_from_its_length_alone() {
    // weight 1 + 2^20/64 = 16,385 passes a 20,000-byte limit; its 131,073 bytes do not, known before any is produced
    let n = BigInt::from(1) << (1 << 20);
    let v = Value::int(n);
    assert_eq!(v.weight(), 16_385);
    let full = cbor::encode(&v).len();
    let e = cbor::encoded_len_within(&v, 20_000).unwrap_err();
    assert_eq!(e.at_least, Some(full), "the whole length, from the bit length");
    assert_eq!(cbor::encode_within(&v, 20_000).unwrap_err(), e, "the same refusal when writing");
}
