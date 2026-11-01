//! A refused integer is named by its size: its digits would be work no caller pays for.

#![allow(clippy::unwrap_used, clippy::arithmetic_side_effects, reason = "tests fail by panicking")]

use dilf4s_core::codec::{BigBound, BigIntIn, Codec, INT, LiteralInt};
use dilf4s_core::{BigInt, CanonicalError, Value};

fn huge() -> BigInt {
    BigInt::from(1u8) << 1_000_000u32
}

#[test]
fn native_range_refusal_names_the_size() {
    let failure = INT.decode(&Value::int(huge())).unwrap_err();
    assert_eq!(failure.reason, "integer of 1000001 bits outside -18446744073709551616..18446744073709551615");
}

#[test]
fn big_range_refusals_name_the_size() {
    let codec = BigIntIn {
        lo: Some(BigBound { negative: false, magnitude: &[] }),
        hi: Some(BigBound { negative: false, magnitude: &[1] }),
    };
    assert_eq!(codec.decode(&Value::int(huge())).unwrap_err().reason, "integer of 1000001 bits outside 0..1");
    assert_eq!(
        codec.encode(&huge()).unwrap_err(),
        CanonicalError::TypeMismatch { reason: "integer of 1000001 bits outside 0..1".into() }
    );
}

#[test]
fn literal_refusal_names_the_size() {
    assert_eq!(
        LiteralInt(7).decode(&Value::int(huge())).unwrap_err().reason,
        "expected 7, got an integer of 1000001 bits"
    );
}
