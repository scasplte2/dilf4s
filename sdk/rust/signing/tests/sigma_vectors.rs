//! `shared/vectors/sigma_v1.json` (Ergo's proofs) and `sigma_dilf4s_v1.json` (the Scala prover's), both with Ergo's
//! verdict on every case (`docs/specs/sigma.md`), then the malformed inputs of sections 1, 3 and 6.
//!
//! Each case goes through the JLVM path: the proposition in its canonical form, decoded by `Proposition::from_value`.
//! The verdicts are exact: a tampered challenge or message is `Ok(false)`, never an error, and a proof of the wrong
//! length is the `ProofLength` error (where Ergo ignores trailing bytes).

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    clippy::as_conversions,
    clippy::doc_markdown,
    reason = "tests fail by panicking, and run with std"
)]

#[path = "../../core/tests/common/mod.rs"]
mod common;

use common::{bytes, load};
use dilf4s_core::Value;
use dilf4s_core::generated::sigma_vectors::{SigmaCase, SigmaKind, SigmaProposition, SigmaType, SigmaVectors};
use dilf4s_signing::SigmaError;
use dilf4s_signing::sigma::{self, MAX_DEPTH, Proposition};

/// Bytes from the vectors' `0x`-prefixed hex (`hex0x` in the schema).
fn hex0x(text: &str) -> Vec<u8> {
    bytes(text.strip_prefix("0x").unwrap_or_else(|| panic!("not 0x-hex: {text}")))
}

/// The canonical form of a vector proposition: its hex fields as bytes, as the JLVM holds them.
fn to_value(p: &SigmaProposition) -> Value {
    let ty = match p.r#type {
        SigmaType::Dlog => "dlog",
        SigmaType::Dhtuple => "dhtuple",
        SigmaType::And => "and",
        SigmaType::Or => "or",
        SigmaType::Threshold => "threshold",
    };
    let mut entries = vec![("type".to_owned(), Value::text(ty))];
    for (key, field) in [("pk", &p.pk), ("g", &p.g), ("h", &p.h), ("u", &p.u), ("v", &p.v)] {
        if let Some(hex) = field {
            entries.push((key.to_owned(), Value::bytes(hex0x(hex))));
        }
    }
    if let Some(k) = p.k {
        entries.push(("k".to_owned(), Value::int(k)));
    }
    if let Some(children) = &p.children {
        entries.push(("children".to_owned(), Value::array(children.iter().map(to_value).collect()).unwrap()));
    }
    Value::map(entries).unwrap()
}

/// The same proposition through the constructors.
fn build(p: &SigmaProposition) -> Result<Proposition, SigmaError> {
    let point = |h: &Option<String>| hex0x(h.as_ref().unwrap());
    let children = || p.children.as_ref().unwrap().iter().map(build).collect::<Result<Vec<_>, _>>();
    match p.r#type {
        SigmaType::Dlog => Proposition::dlog(&point(&p.pk)),
        SigmaType::Dhtuple => Proposition::dh_tuple(&point(&p.g), &point(&p.h), &point(&p.u), &point(&p.v)),
        SigmaType::And => Proposition::and(children()?),
        SigmaType::Or => Proposition::or(children()?),
        SigmaType::Threshold => Proposition::threshold(p.k.unwrap() as usize, children()?),
    }
}

#[derive(Default, Debug)]
struct Tally {
    cases: usize,
    valid: usize,
    rejected: usize,
    response_out_of_range: usize,
    wrong_length: usize,
    ergo_accepts_trailing: usize,
}

fn check(c: &SigmaCase, tally: &mut Tally) -> Result<(), String> {
    let what = format!("{} ({:?})", c.name, c.kind);
    let (proof, message) = (hex0x(&c.proof), hex0x(&c.message));
    let value = to_value(&c.proposition);
    let prop = Proposition::from_value(&value).map_err(|e| format!("{what}: proposition: {e}"))?;
    if build(&c.proposition).as_ref() != Ok(&prop) {
        return Err(format!("{what}: the constructors build another proposition"));
    }
    let ours = sigma::verify_value(&value, &proof, &message);
    if ours != sigma::verify(&prop, &proof, &message) {
        return Err(format!("{what}: verify_value and verify disagree"));
    }
    if c.proposition.r#type == SigmaType::Dlog {
        let pk = hex0x(c.proposition.pk.as_ref().unwrap());
        if sigma::verify_dlog_bytes(&pk, &message, &proof) != ours {
            return Err(format!("{what}: verify_dlog_bytes disagrees"));
        }
    }
    tally.cases += 1;
    match (&c.kind, &ours) {
        (SigmaKind::Valid, Ok(true)) if c.ergo => {
            tally.valid += 1;
            if proof.len() == prop.proof_length() { Ok(()) } else { Err(format!("{what}: proof length")) }
        }
        (SigmaKind::OtherMessage | SigmaKind::FlippedChallenge | SigmaKind::FlippedResponse, Ok(false)) if !c.ergo => {
            tally.rejected += 1;
            Ok(())
        }
        (SigmaKind::FlippedResponse, Err(SigmaError::ResponseOutOfRange)) if !c.ergo => {
            tally.response_out_of_range += 1;
            Ok(())
        }
        (SigmaKind::TrailingByte | SigmaKind::Truncated, Err(SigmaError::ProofLength { expected, actual })) => {
            let off_by = if c.kind == SigmaKind::TrailingByte { *expected + 1 } else { *expected - 1 };
            tally.wrong_length += 1;
            tally.ergo_accepts_trailing += usize::from(c.kind == SigmaKind::TrailingByte && c.ergo);
            if *actual == off_by && *expected == prop.proof_length() {
                Ok(())
            } else {
                Err(format!("{what}: expected {expected}, actual {actual}"))
            }
        }
        _ => Err(format!("{what}: ours {ours:?}, Ergo's {}", c.ergo)),
    }
}

fn run(file: &str) -> Tally {
    let vectors: SigmaVectors = load(file);
    let mut tally = Tally::default();
    let failures: Vec<String> = vectors.cases.iter().filter_map(|c| check(c, &mut tally).err()).collect();
    assert!(
        failures.is_empty(),
        "{file}: {} of {} cases failed:\n{}",
        failures.len(),
        vectors.cases.len(),
        failures.join("\n")
    );
    println!("{file}: {tally:?}");
    assert_eq!(tally.cases, 120, "{file}: case count");
    assert_eq!(tally.valid, 20, "{file}: valid proofs");
    assert_eq!(tally.wrong_length, 40, "{file}: trailing and truncated proofs");
    assert_eq!(tally.rejected + tally.response_out_of_range, 60, "{file}: tampered proofs");
    assert!(tally.ergo_accepts_trailing > 0, "{file}: Ergo ignores trailing bytes, so some trailing-byte case is true");
    tally
}

#[test]
fn ergo_proofs() {
    run("sigma_v1.json");
}

#[test]
fn dilf4s_proofs() {
    run("sigma_dilf4s_v1.json");
}

#[test]
fn the_vectors_cover_every_connective_and_threshold_shape() {
    let vectors: SigmaVectors = load("sigma_v1.json");
    let names: Vec<&str> = vectors.cases.iter().map(|c| c.name.as_str()).collect();
    assert!(names.iter().any(|n| n.starts_with("ZeroJoin")), "ZeroJoin");
    assert!(names.iter().any(|n| n.contains("3-of-5")), "3-of-5");
}

// ---- malformed input

fn valid_dlog() -> (Vec<u8>, Vec<u8>, Vec<u8>) {
    let vectors: SigmaVectors = load("sigma_v1.json");
    let c =
        vectors.cases.iter().find(|c| c.kind == SigmaKind::Valid && c.proposition.r#type == SigmaType::Dlog).unwrap();
    (hex0x(c.proposition.pk.as_ref().unwrap()), hex0x(&c.message), hex0x(&c.proof))
}

fn generator() -> Vec<u8> {
    hex0x("0x0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")
}

/// `02 ‖ x` with `x = 5`, which has no `y` on secp256k1.
fn off_curve() -> Vec<u8> {
    let mut p = vec![0x02];
    p.extend([0; 31]);
    p.push(5);
    p
}

fn kind<T: std::fmt::Debug>(r: Result<T, SigmaError>) -> &'static str {
    r.err().map_or("Ok", |e| e.kind())
}

#[test]
fn group_elements_decode_only_in_canonical_form() {
    let g = generator();
    let x5 = off_curve();
    let mut beyond = vec![0x02];
    beyond.extend([0xff; 32]);
    let mut prefix04 = g.clone();
    prefix04[0] = 0x04;
    let uncompressed = hex0x(
        "0x0479be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798\
         483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8",
    );
    assert_eq!(kind(Proposition::dlog(&g)), "Ok");
    assert_eq!(kind(Proposition::dlog(&uncompressed)), "InvalidPoint", "uncompressed");
    assert_eq!(kind(Proposition::dlog(&prefix04)), "InvalidPoint", "prefix 04 at 33 bytes");
    assert_eq!(kind(Proposition::dlog(&beyond)), "InvalidPoint", "x beyond the field");
    assert_eq!(kind(Proposition::dlog(&x5)), "InvalidPoint", "x = 5 has no y");
    assert_eq!(kind(Proposition::dlog(&g[1..])), "InvalidPoint", "short");
    let mut odd = g.clone();
    odd[0] = 0x03;
    assert_eq!(kind(Proposition::dlog(&odd)), "Ok", "-G");
    assert_ne!(Proposition::dlog(&odd).unwrap(), Proposition::dlog(&g).unwrap());
}

#[test]
fn the_identity_is_no_statement_point() {
    let (g, o) = (generator(), [0u8; 33]);
    assert_eq!(kind(Proposition::dlog(&o)), "InvalidProposition", "dlog(O)");
    assert_eq!(kind(Proposition::dh_tuple(&g, &g, &o, &o)), "InvalidProposition", "u = v = O");
    assert_eq!(kind(Proposition::dh_tuple(&o, &g, &g, &g)), "InvalidProposition", "an identity base");
    assert_eq!(kind(sigma::verify_dlog_bytes(&o, b"m", &[0; 56])), "InvalidProposition", "the identity key");
    assert_eq!(kind(sigma::verify_dlog_bytes(&g[1..], b"m", &[0; 56])), "InvalidPoint", "a short key");
}

#[test]
fn connectives_are_bounded_and_never_trivial() {
    let leaf = || Proposition::dlog(&generator()).unwrap();
    let leaves = |n: usize| (0..n).map(|_| leaf()).collect::<Vec<_>>();
    assert_eq!(kind(Proposition::and(vec![])), "InvalidProposition", "empty and");
    assert_eq!(kind(Proposition::or(vec![])), "InvalidProposition", "empty or");
    assert_eq!(kind(Proposition::threshold(0, leaves(1))), "InvalidProposition", "k = 0");
    assert_eq!(kind(Proposition::threshold(2, leaves(1))), "InvalidProposition", "k > n");
    assert_eq!(kind(Proposition::and(leaves(255))), "Ok", "255 children");
    assert_eq!(kind(Proposition::and(leaves(256))), "InvalidProposition", "256 children");
    assert_eq!(kind(Proposition::threshold(1, leaves(256))), "InvalidProposition", "a threshold of 256");

    let nest = |depth: usize| (1..depth).try_fold(leaf(), |c, _| Proposition::and(vec![c]));
    assert_eq!(nest(MAX_DEPTH).unwrap().depth(), 64);
    assert_eq!(kind(nest(MAX_DEPTH + 1)), "InvalidProposition", "65 deep");

    let wide = |groups: usize, each: usize| {
        Proposition::and((0..groups).map(|_| Proposition::and(leaves(each)).unwrap()).collect())
    };
    assert_eq!(wide(16, 255).unwrap().leaves(), 4080);
    assert_eq!(kind(wide(17, 241)), "InvalidProposition", "4097 leaves");
}

fn map(entries: &[(&str, Value)]) -> Value {
    Value::map(entries.iter().map(|(k, v)| ((*k).to_owned(), v.clone()))).unwrap()
}

#[test]
fn the_canonical_form_takes_exactly_its_types_fields() {
    let g = Value::bytes(generator());
    let dlog = map(&[("type", Value::text("dlog")), ("pk", g.clone())]);
    let children = |cs: Vec<Value>| Value::array(cs).unwrap();
    let decode = |v: &Value| kind(Proposition::from_value(v));
    assert_eq!(decode(&dlog), "Ok");
    assert_eq!(decode(&Value::text("dlog")), "InvalidProposition", "not an object");
    assert_eq!(decode(&map(&[("type", Value::text("xor")), ("pk", g.clone())])), "InvalidProposition", "type");
    assert_eq!(decode(&map(&[("pk", g.clone())])), "InvalidProposition", "no type");
    assert_eq!(
        decode(&map(&[("type", Value::text("dlog")), ("pk", g.clone()), ("g", g.clone())])),
        "InvalidProposition",
        "an extra field"
    );
    assert_eq!(
        decode(&map(&[("type", Value::text("dlog")), ("pk", Value::text("02"))])),
        "InvalidProposition",
        "pk text"
    );
    assert_eq!(decode(&map(&[("type", Value::text("dlog")), ("pk", Value::bytes(off_curve()))])), "InvalidPoint");
    assert_eq!(decode(&map(&[("type", Value::text("and")), ("children", children(vec![dlog.clone()]))])), "Ok", "and");
    assert_eq!(decode(&map(&[("type", Value::text("and")), ("children", dlog.clone())])), "InvalidProposition");
    let threshold = |k: Value| {
        map(&[("type", Value::text("threshold")), ("k", k), ("children", children(vec![dlog.clone(), dlog.clone()]))])
    };
    assert_eq!(decode(&threshold(Value::int(2))), "Ok", "2-of-2");
    assert_eq!(decode(&threshold(Value::int(3))), "InvalidProposition", "3-of-2");
    assert_eq!(decode(&threshold(Value::int(-1))), "InvalidProposition", "k = -1");
    assert_eq!(decode(&threshold(Value::int(1u64 << 40))), "InvalidProposition", "k beyond an Int");
    assert_eq!(decode(&threshold(Value::text("2"))), "InvalidProposition", "k text");
    // children are decoded before the connective is checked, as the reference does
    let bad_child = map(&[("type", Value::text("dlog")), ("pk", Value::bytes(off_curve()))]);
    let wide_with_bad_point =
        map(&[("type", Value::text("and")), ("children", children(std::iter::repeat_n(bad_child, 256).collect()))]);
    assert_eq!(decode(&wide_with_bad_point), "InvalidPoint");
}

#[test]
fn a_deep_canonical_form_is_malformed_not_a_stack_overflow() {
    let deep = (0..120).fold(map(&[("type", Value::text("dlog")), ("pk", Value::bytes(generator()))]), |c, _| {
        map(&[("type", Value::text("or")), ("children", Value::array(vec![c]).unwrap())])
    });
    assert_eq!(kind(Proposition::from_value(&deep)), "InvalidProposition");
}

#[test]
fn proofs_of_the_wrong_length_or_with_z_beyond_the_order_are_malformed() {
    let (pk, message, proof) = valid_dlog();
    assert_eq!(sigma::verify_dlog_bytes(&pk, &message, &proof), Ok(true));
    assert_eq!(
        sigma::verify_dlog_bytes(&pk, &message, &proof[..55]),
        Err(SigmaError::ProofLength { expected: 56, actual: 55 })
    );
    assert_eq!(sigma::verify_dlog_bytes(&pk, &message, &[]), Err(SigmaError::ProofLength { expected: 56, actual: 0 }));
    let mut high = proof.clone();
    high[24..].fill(0xff);
    assert_eq!(sigma::verify_dlog_bytes(&pk, &message, &high), Err(SigmaError::ResponseOutOfRange));
    // q itself is out of range; q - 1 is a response
    let q = hex0x("0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141");
    let mut at_q = proof.clone();
    at_q[24..].copy_from_slice(&q);
    assert_eq!(sigma::verify_dlog_bytes(&pk, &message, &at_q), Err(SigmaError::ResponseOutOfRange));
    at_q[55] = 0x40;
    assert_eq!(sigma::verify_dlog_bytes(&pk, &message, &at_q), Ok(false));
}

#[test]
fn a_proof_is_bound_to_its_proposition() {
    let (pk, message, proof) = valid_dlog();
    let leaf = || Proposition::dlog(&pk).unwrap();
    let and = Proposition::and(vec![leaf()]).unwrap();
    assert_eq!(and.proof_length(), 56);
    assert_eq!(sigma::verify(&and, &proof, &message), Ok(false), "and(dlog) has its own transcript");
    let or = Proposition::or(vec![leaf(), leaf()]).unwrap();
    assert_eq!(or.proof_length(), 24 + 24 + 64);
    let two_of_three = Proposition::threshold(2, vec![leaf(), leaf(), leaf()]).unwrap();
    assert_eq!(two_of_three.proof_length(), 24 + 24 + 96);
    assert_eq!(kind(sigma::verify(&or, &proof, &message)), "ProofLength");
}
