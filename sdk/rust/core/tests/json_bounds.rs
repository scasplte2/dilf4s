//! JSON numbers have at most 10,000 significant digits, counted on the text before any conversion, and render only
//! within the same bounds, so that a rendering always reads back.

#![allow(
    clippy::unwrap_used,
    clippy::arithmetic_side_effects,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    reason = "tests fail by panicking, and run with std"
)]

use dilf4s_core::error::{CanonicalErrorKind, NotJson, NumberBound};
use dilf4s_core::value::Rational;
use dilf4s_core::{BigInt, CanonicalError, NumberError, Value, json};

fn digits(n: usize) -> String {
    "9".repeat(n)
}

/// A scale bound may be lowered, never raised past the spec's: a larger one is a `TypeMismatch`, as in Scala and TS.
#[test]
fn a_scale_bound_past_the_spec_is_refused() {
    let refused = CanonicalError::TypeMismatch { reason: "the scale bound is not an integer from 0 to 10000".into() };
    for max_scale in [json::DEFAULT_MAX_SCALE + 1, u32::MAX] {
        assert_eq!(json::parse_with_max_scale("1.5", max_scale), Err(refused.clone()), "{max_scale}");
    }
    assert!(json::parse_with_max_scale("1.5", json::DEFAULT_MAX_SCALE).is_ok());
    assert!(json::parse_with_max_scale("1.5", 0).is_err(), "scale 1 past a bound of 0");
}

#[test]
fn ten_thousand_digits_read_and_write() {
    let ones = "1".repeat(10_000);
    let v = json::parse(&ones).unwrap();
    assert_eq!(json::render_within(&v, usize::MAX).unwrap(), ones);
    let small = json::parse(&format!("0.{}2e1", "0".repeat(10_000))).unwrap();
    assert_eq!(json::render_within(&small, usize::MAX).unwrap(), format!("0.{}2", "0".repeat(9_999)));
    // within the digit and scale bounds, but past the data model's: 10,000 nines (33220 bits), 1/10^10000
    assert_eq!(json::parse(&digits(10_000)), Err(CanonicalError::IntegerTooWide { limit: 33_219 }));
    assert_eq!(
        json::parse(&format!("0.{}1", "0".repeat(9_999))),
        Err(CanonicalError::RationalTooWide { limit: 33_219 })
    );
    assert_eq!(json::parse(&format!("1e{}2", "0".repeat(20_000))).unwrap(), Value::int(100));
}

#[test]
fn more_digits_are_refused_by_their_length() {
    let refused = json::parse(&digits(10_001)).unwrap_err();
    assert_eq!(
        refused,
        CanonicalError::InvalidNumber {
            length: Some(10_001),
            offset: Some(0),
            bound: Some(NumberBound::Digits { digits: 10_001, limit: 10_000 }),
        }
    );
    assert_eq!(
        json::parse(&format!("0.{}e1", "1".repeat(10_001))).unwrap_err().kind(),
        CanonicalErrorKind::InvalidNumber
    );
    assert_eq!(
        json::parse(&format!("1e{}", "1".repeat(10_001))).unwrap_err().kind(),
        CanonicalErrorKind::InvalidNumber
    );
    assert_eq!(json::parse(&format!("1e{}", "9".repeat(20))).unwrap_err().kind(), CanonicalErrorKind::InvalidNumber);
    // both past the bound: the significand's count, as every SDK reports it; a scale saturates at the 32-bit range
    let both = json::parse(&format!("{}e{}", "1".repeat(10_002), "1".repeat(10_001))).unwrap_err();
    assert!(matches!(
        both,
        CanonicalError::InvalidNumber { bound: Some(NumberBound::Digits { digits: 10_002, .. }), .. }
    ));
    let scale = json::parse(&format!("1e{}", "9".repeat(20))).unwrap_err();
    let saturated = NumberBound::Scale { scale: i32::MIN, limit: 10_000 };
    assert!(matches!(&scale, CanonicalError::InvalidNumber { bound: Some(b), .. } if *b == saturated), "{scale:?}");
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
        let CanonicalError::InvalidNumber { length: l, offset, .. } = &e else { panic!("an invalid number, got {e}") };
        assert_eq!((*l, *offset), (Some(length), Some(1)));
        assert!(!e.to_string().contains("99999"), "Display keeps no digits");
        assert!(!format!("{e:?}").contains("99999"), "Debug keeps no digits");
    }
}

#[test]
fn rendering_refuses_what_would_not_read_back() {
    let over_long =
        Err(json::RenderError::Canonical(CanonicalError::NotJsonRepresentable { what: NotJson::OverLongNumber }));
    // every integer renders: the widest, of either sign, has 10,000 digits
    let edge = BigInt::from(1u8) << dilf4s_core::MAX_INTEGER_BITS;
    for n in [&edge - 1u8, -edge] {
        let text = json::render_within(&Value::integer(n.clone()).unwrap(), usize::MAX).unwrap();
        assert_eq!(text, n.to_string());
        assert_eq!(text.trim_start_matches('-').len(), 10_000);
    }
    // 1/5^14400 (33436 bits) is no value at all (MAX_RATIONAL_BITS); 1/5^14306 (33218 bits) is, at a scale past the bound
    let five = BigInt::from(5u8);
    assert_eq!(
        Value::number(&BigInt::from(1u8), &num_traits::pow(five.clone(), 14_400)),
        Err(NumberError::RationalTooWide)
    );
    let wide = Value::number(&BigInt::from(1u8), &num_traits::pow(five, 14_306)).unwrap();
    let Value::Rational(r) = &wide else { panic!("a rational") };
    assert_eq!(json::decimal(r), Err(NotJson::OverLongNumber));
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

/// A text's exponents together spend at most 2^24 (canonical-encoding.md section 3): 1677 exponents of 9999 and one of
/// 8893 spend it exactly, and the number that passes it is refused before it is converted, even one too wide.
#[test]
fn the_exponent_budget() {
    assert_eq!(json::MAX_JSON_EXPONENTS, 1 << 24, "2^24");
    let full: Vec<&str> = core::iter::repeat_n("8e9999", 1677).chain(["8e8893"]).collect();
    let array = |items: &[&str]| format!("[{}]", items.join(","));
    assert!(json::parse(&array(&full)).is_ok(), "a text that spends it exactly is read");
    let mut over = full.clone();
    over.push("1e1");
    let text = array(&over);
    assert_eq!(
        json::parse(&text),
        Err(CanonicalError::InvalidNumber {
            length: Some(3),
            offset: Some(u64::try_from(text.len() - "1e1]".len()).expect("an offset")),
            bound: Some(NumberBound::Exponents { exponents: (1 << 24) + 1, limit: 1 << 24 }),
        }),
        "passed by one"
    );
    let mut wide = full.clone();
    wide.push("9e9999");
    assert!(
        matches!(
            json::parse(&array(&wide)),
            Err(CanonicalError::InvalidNumber { bound: Some(NumberBound::Exponents { .. }), .. })
        ),
        "the budget comes before the width"
    );
    // an exponent costs at most one past the largest scale: 0.(10,000 zeros)1e10005 is 10^4 and costs 10,001
    let capped = format!("0.{}1e10005", "0".repeat(10_000));
    let before: Vec<&str> = core::iter::repeat_n("8e9999", 1676).collect();
    let mut fits = before.clone();
    fits.extend(["8e8890", capped.as_str()]);
    assert!(json::parse(&array(&fits)).is_ok(), "a capped exponent fits as 10,001");
    let mut passes = before;
    passes.extend(["8e8892", capped.as_str()]);
    assert!(
        matches!(
            json::parse(&array(&passes)),
            Err(CanonicalError::InvalidNumber { bound: Some(NumberBound::Exponents { exponents, .. }), .. }) if exponents == (1 << 24) + 1
        ),
        "and passes the budget by one after one more"
    );
    // digits written out spend nothing: 18 MiB of 8-digit numbers
    let long = format!("[{}]", vec!["12345678"; 1 << 21].join(","));
    assert!(long.len() > 1 << 24 && json::parse(&long).is_ok(), "a long text without exponents is read");
}
