//! An error family: struct and unit variants, `kind()`, `Display` and `Error`; a choice naming its kinds round-trips.

#![allow(clippy::expect_used, clippy::unreachable, reason = "tests fail by panicking")]

use dilf4s_core::Preview;
use dilf4s_core::codec::Model;
use dilf4s_fixtures::generated::fixture_errors::{Failure, FailureKind, Outcome, Why};

#[test]
fn each_variant_gives_its_kind_and_renders_its_fields() {
    let broken = Failure::Broken { what: "key".into(), size: 3, limit: Some(32) };
    let refused = Failure::Refused { why: Why::Late, r#type: "x".into() };
    assert_eq!(broken.kind(), FailureKind::Broken);
    assert_eq!(Failure::Gone.kind(), FailureKind::Gone);
    assert_eq!(refused.kind(), FailureKind::Refused);
    assert_eq!(broken.to_string(), "Broken (what: key, size: 3, limit: Some(32))");
    assert_eq!(Failure::Gone.to_string(), "Gone");
    assert_eq!(refused.to_string(), "Refused (why: Late, type: x)");
    let _: &dyn core::error::Error = &broken;
}

#[test]
fn a_preview_field_holds_the_core_preview_and_renders_bounded() {
    let long = "x".repeat(1 << 20);
    let unknown = Failure::Unknown { name: Preview::of(&long) };
    assert_eq!(unknown.kind(), FailureKind::Unknown);
    let Failure::Unknown { name } = &unknown else { unreachable!("built as Unknown") };
    assert_eq!((name.head().len(), name.length()), (32, 1 << 20));
    let shown = unknown.to_string();
    assert_eq!(shown, format!("Unknown (name: \"{}\"… (UTF-8 length {}))", "x".repeat(32), 1 << 20));
}

#[test]
fn a_choice_naming_the_kinds_holds_each_and_round_trips() {
    assert_eq!(Outcome::ALL.len(), 1 + FailureKind::ALL.len());
    for o in Outcome::ALL.into_iter().chain([Outcome::FailureKind(Failure::Gone.kind())]) {
        let bytes = o.to_cbor().expect("encodes");
        assert_eq!(Outcome::from_cbor(&bytes), Ok(o));
    }
    let kind = FailureKind::Refused.to_cbor().expect("encodes");
    assert_eq!(
        Outcome::from_cbor(&kind),
        Ok(Outcome::FailureKind(FailureKind::Refused)),
        "a kind's text is the outcome's"
    );
}
