//! Bounded diagnostics (docs/specs/conformance.md section 8): a refusal holds caller text only as a [`Preview`], so
//! however long the input, what an error holds and renders stays within a fixed bound.

#![allow(clippy::unwrap_used, clippy::arithmetic_side_effects, reason = "tests fail by panicking")]

mod common;

use dilf4s_core::codec::{
    Codec, Fields, LiteralText, Matching, Model, ModelError, TextMapOf, TextPattern, UINT, select_alternative,
};
use dilf4s_core::generated::diagnostic_vectors::DiagnosticVectors;
use dilf4s_core::generated::merkle_vectors::MerkleVectors;
use dilf4s_core::hash::{DomainTag, HashAlgorithm, HashError, HashErrorKind};
use dilf4s_core::preview::MAX_CODE_POINTS;
use dilf4s_core::{CanonicalError, DecodeFailure, Preview, Value, json};

/// A preview renders as at most 32 code points, each escaped to at most 6 characters, quotes, a mark and a length.
const PREVIEW_BOUND: usize = 32 * 6 + 64;
const MESSAGE_BOUND: usize = 2 * PREVIEW_BOUND + 256;
const HUGE: usize = 1 << 20;

fn bounded(what: &str, rendered: &str) {
    assert!(rendered.len() <= MESSAGE_BOUND, "{what} renders {} bytes, more than {MESSAGE_BOUND}", rendered.len());
}

fn kept(p: &Preview) {
    assert!(p.head().chars().count() <= MAX_CODE_POINTS, "a preview keeps {} bytes", p.head().len());
}

fn rendered(e: &(impl core::fmt::Display + core::fmt::Debug)) -> String {
    format!("{e}{e:?}")
}

fn text(s: &str) -> Value {
    Value::text(s)
}

/// `shared/vectors/diagnostics_v1.json` (conformance.md section 8), the same in every SDK: each object repeats one
/// member name, refused as `DuplicateKey` with the preview given.
#[test]
fn a_preview_keeps_the_first_32_code_points_and_the_utf8_length() {
    let v: DiagnosticVectors = common::load("diagnostics_v1.json");
    assert!(v.previews.len() >= 11, "every row of the spec's table");
    for c in &v.previews {
        let Err(CanonicalError::DuplicateKey { key: p }) = json::parse(&c.json) else {
            panic!("{}: expected a duplicate key", c.json)
        };
        assert_eq!(p.head(), c.head, "{}", c.json);
        assert_eq!(u64::try_from(p.length()).unwrap(), c.length, "{}", c.json);
        assert_eq!(p.is_truncated(), c.truncated, "{}", c.json);
    }
}

#[test]
fn a_preview_renders_the_head_escaped_a_mark_and_the_length() {
    assert_eq!(Preview::of("abc").to_string(), "\"abc\" (UTF-8 length 3)");
    assert_eq!(Preview::of(&"a".repeat(40)).to_string(), format!("\"{}\"… (UTF-8 length 40)", "a".repeat(32)));
    assert_eq!(Preview::of("\"\\\u{0}").to_string(), "\"\\\"\\\\\\u0000\" (UTF-8 length 3)");
    let failure = DecodeFailure::new("r").at("b".repeat(33)).at("a");
    assert_eq!(failure.to_string(), format!("a.{}…: r", "b".repeat(32)));
}

#[test]
fn a_repeated_1_mib_json_member_name_is_a_preview() {
    let key = "k".repeat(HUGE);
    let e = json::parse(&format!("{{\"{key}\":1,\"{key}\":2}}")).unwrap_err();
    let CanonicalError::DuplicateKey { key: p } = &e else { panic!("a duplicate key, got {e}") };
    kept(p);
    assert_eq!(p.length(), HUGE);
    bounded("DuplicateKey", &rendered(&e));
    let m = Value::map([(key.clone(), Value::int(1)), (key, Value::int(2))]).unwrap_err();
    bounded("DuplicateKey", &rendered(&m));
}

#[test]
fn literal_pattern_and_alternative_refusals_preview_a_1_mib_text() {
    let huge = text(&"x".repeat(HUGE));
    let failures = [
        LiteralText("a").decode(&huge).unwrap_err(),
        Matching(TextPattern::Decimal).decode(&huge).unwrap_err(),
        select_alternative(&huge, &[], &["a"]).unwrap_err(),
    ];
    for f in &failures {
        bounded("refusal", &rendered(f));
    }
}

#[test]
fn unknown_keys_are_counted_and_the_first_in_canonical_order_previewed() {
    let small = Value::map(["a", "zz", "é", "aaa", "b"].map(|k| (String::from(k), Value::int(1)))).unwrap();
    let Err(f) = Fields::of(&small, &["a"]) else { panic!("unknown keys accepted") };
    assert_eq!(f.to_string(), "<root>: 4 unknown keys, the first \"b\" (UTF-8 length 1)");
    let many = Value::map((0..4096).map(|i| (format!("{}{i}", "u".repeat(256)), Value::int(i)))).unwrap();
    let Err(f) = Fields::of(&many, &["a"]) else { panic!("unknown keys accepted") };
    assert!(f.reason.starts_with("4096 unknown keys"), "{f}");
    bounded("unknown keys", &rendered(&f));
    let record: (&[&str], &[&str]) = (&["a"], &["a"]);
    let none = select_alternative(&many, &[record], &[]).unwrap_err();
    assert_eq!(none.to_string(), "<root>: no alternative fits a map of 4096 keys");
}

#[test]
fn a_model_refusing_1_mib_of_unknown_keys_renders_bounded() {
    let json = format!("{{\"{}\":1}}", "k".repeat(HUGE));
    let e = MerkleVectors::from_json(&json).unwrap_err();
    assert!(matches!(e, ModelError::Decode(_)), "{e}");
    bounded("ModelError", &rendered(&e));
}

#[test]
fn a_1_mib_caller_key_on_a_path_is_a_preview() {
    let m = Value::map([("k".repeat(HUGE), text("not an int"))]).unwrap();
    let f = TextMapOf(UINT).decode(&m).unwrap_err();
    f.path.iter().for_each(kept);
    bounded("path", &rendered(&f));
}

#[test]
fn a_1_mib_domain_tag_or_algorithm_name_is_a_preview() {
    let tag = DomainTag::new(&"X".repeat(HUGE)).unwrap_err();
    let HashError::InvalidDomainTag { tag: p } = &tag else { panic!("an invalid tag") };
    kept(p);
    bounded("InvalidDomainTag", &rendered(&tag));
    let name = HashAlgorithm::from_name(&"X".repeat(HUGE)).unwrap_err();
    let HashError::UnknownAlgorithm { name: p } = &name else { panic!("an unknown algorithm") };
    kept(p);
    bounded("UnknownAlgorithm", &rendered(&name));
}

#[test]
fn a_1_mib_digest_hex_is_refused_by_its_width_holding_sizes_only() {
    let huge = u64::try_from(HUGE).unwrap();
    let cases = [
        ("a".repeat(HUGE), HashError::InvalidLength { algorithm: "SHA-256".into(), expected: 32, actual: huge / 2 }),
        (
            format!("0x{}", "a".repeat(HUGE)),
            HashError::InvalidLength { algorithm: "SHA-256".into(), expected: 32, actual: huge / 2 },
        ),
        (format!("{}{}", "a".repeat(64), "é".repeat(HUGE)), HashError::InvalidHex { offset: 64 }),
        ("é".repeat(HUGE), HashError::InvalidHex { offset: 0 }),
    ];
    for (text, expected) in cases {
        let e = HashAlgorithm::Sha256.digest_from_hex(&text).unwrap_err();
        assert_eq!(e, expected);
        bounded("digest hex", &rendered(&e));
    }
    // past the one digit it reads beyond the width, the text is not read: its length refuses it, whatever follows
    let past = HashAlgorithm::Sha256.digest_from_hex(&format!("{}{}", "a".repeat(65), "é".repeat(HUGE))).unwrap_err();
    assert_eq!(past.kind(), HashErrorKind::InvalidLength);
    bounded("digest hex", &rendered(&past));
}
