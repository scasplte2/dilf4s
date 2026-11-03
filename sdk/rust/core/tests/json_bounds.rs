//! JSON numbers have at most 10,000 significant digits, counted on the text before any conversion, and render only
//! within the same bounds, so that a rendering always reads back.

#![allow(
    clippy::unwrap_used,
    clippy::arithmetic_side_effects,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    reason = "tests fail by panicking, and run with std"
)]

use dilf4s_core::error::{DigitsOutOfRange, NotJson, NumberBound};
use dilf4s_core::value::Rational;
use dilf4s_core::{BigInt, CanonicalError, NumberError, Value, json};

fn digits(n: usize) -> String {
    "9".repeat(n)
}

#[test]
fn ten_thousand_digits_read_and_write() {
    let v = json::parse(&digits(10_000)).unwrap();
    assert_eq!(json::render_within(&v, usize::MAX).unwrap(), digits(10_000));
    let small = json::parse(&format!("0.{}1e1", "0".repeat(10_000))).unwrap();
    assert_eq!(json::render_within(&small, usize::MAX).unwrap(), format!("0.{}1", "0".repeat(9_999)));
    assert_eq!(json::parse(&format!("1e{}2", "0".repeat(20_000))).unwrap(), Value::int(100));
}

#[test]
fn more_digits_are_refused_by_their_length() {
    let refused = json::parse(&digits(10_001)).unwrap_err();
    assert_eq!(
        refused,
        CanonicalError::InvalidNumber {
            source: "a number of 10001 characters at offset 0".into(),
            bound: Some(NumberBound::Digits(DigitsOutOfRange { digits: 10_001, max_digits: 10_000 })),
        }
    );
    assert_eq!(json::parse(&format!("0.{}e1", "1".repeat(10_001))).unwrap_err().kind(), "InvalidNumber");
    assert_eq!(json::parse(&format!("1e{}", "1".repeat(10_001))).unwrap_err().kind(), "InvalidNumber");
    assert_eq!(json::parse(&format!("1e{}", "9".repeat(20))).unwrap_err().kind(), "InvalidNumber");
}

#[test]
fn malformed_numbers_are_described_by_length_and_offset_never_their_text() {
    let nines = digits(10_001);
    let cases = [
        (format!("[0{nines}]"), 10_002),
        (format!("[{nines}.]"), 10_002),
        (format!("[1.{nines}e]"), 10_004),
        (format!("[-{nines}e+]"), 10_004),
    ];
    for (text, length) in cases {
        let e = json::parse(&text).unwrap_err();
        let CanonicalError::InvalidNumber { source, .. } = &e else { panic!("an invalid number, got {e}") };
        assert_eq!(source, &format!("a number of {length} characters at offset 1"));
        assert!(!e.to_string().contains("99999"), "Display keeps no digits");
        assert!(!format!("{e:?}").contains("99999"), "Debug keeps no digits");
    }
}

#[test]
fn rendering_refuses_what_would_not_read_back() {
    let over_long = Err(json::RenderError::Canonical(CanonicalError::NotJsonRepresentable { kind: NotJson::OverLong }));
    assert_eq!(json::render_within(&Value::int(num_traits::pow(BigInt::from(10u8), 10_000)), usize::MAX), over_long);
    // 1/5^14400 (33436 bits) is no value at all (MAX_RATIONAL_BITS); 1/5^14306 (33218 bits) is, at a scale past the bound
    let five = BigInt::from(5u8);
    assert_eq!(
        Value::number(&BigInt::from(1u8), &num_traits::pow(five.clone(), 14_400)),
        Err(NumberError::RationalTooWide)
    );
    let wide = Value::number(&BigInt::from(1u8), &num_traits::pow(five, 14_306)).unwrap();
    let Value::Rational(r) = &wide else { panic!("a rational") };
    assert_eq!(json::decimal(r), Err(NotJson::OverLong));
    assert_eq!(json::render_within(&wide, usize::MAX), over_long);
    let third: Rational = match Value::number(&BigInt::from(1u8), &BigInt::from(3u8)).unwrap() {
        Value::Rational(r) => r,
        _ => panic!("a rational"),
    };
    assert_eq!(json::decimal(&third), Err(NotJson::NonTerminatingRational));
}

/// `cargo test --release -p dilf4s-core --test json_bounds -- --ignored --nocapture`
#[test]
#[ignore = "a measurement, not a check"]
fn worst_case_parse_time() {
    let cases = [
        ("10,000-digit integer", digits(10_000)),
        ("10,000-digit fraction at scale 10,000", format!("0.{}", digits(10_000))),
        ("1e9999", "1e9999".to_string()),
        ("1e-9999", "1e-9999".to_string()),
    ];
    for (name, text) in cases {
        let start = std::time::Instant::now();
        let runs = 20u32;
        (0..runs).for_each(|_| drop(json::parse(&text).unwrap()));
        eprintln!("{name}: {:?} per parse", start.elapsed() / runs);
    }
}
