//! A `Crypto` error names the typed refusal it wraps (`cause`, jlvm-errors.cddl), as the reference's does: the family
//! the operation called and that family's error. The operation's own checks of its arguments' shape name none.

#![allow(clippy::panic, reason = "tests fail by panicking")]

extern crate alloc;

use alloc::collections::BTreeMap;

use dilf4s_jlvm::{Config, CryptoCause, Jlvm, JlvmError, Native, Value, parse};
use dilf4s_signing::{CryptoErrorKind, SigmaErrorKind};

/// Runs `op` on its arguments, each the bytes of its hex.
fn refusal(op: &str, args: &[String]) -> JlvmError {
    let args: Vec<String> = args.iter().map(|h| format!("{{\"unhex\": \"0x{h}\"}}")).collect();
    let program = parse(&format!("{{\"{op}\": [{}]}}", args.join(", "))).unwrap_or_else(|e| panic!("{op}: {e}"));
    let jlvm = Jlvm::new(Native::new().unwrap_or_else(|e| panic!("{e:?}")));
    match jlvm.evaluate(&program, &Value::Null, &BTreeMap::new(), &Config::default()).result {
        Err(e) => e,
        Ok(v) => panic!("{op}: accepted as {v:?}"),
    }
}

fn cause(e: &JlvmError) -> Option<&CryptoCause> {
    match e {
        JlvmError::Crypto { cause, .. } => cause.as_ref(),
        other => panic!("not a Crypto error: {other:?}"),
    }
}

#[test]
fn a_crypto_error_names_the_refusal_it_wraps() {
    let short_key = refusal("ed25519_verify", &["01".repeat(31), String::new(), "00".repeat(64)]);
    assert!(
        matches!(cause(&short_key), Some(CryptoCause::Signing { error }) if error.kind() == CryptoErrorKind::InvalidEncoding),
        "{short_key:?}"
    );
    let bad_point = refusal("schnorr_verify", &["09".repeat(33), String::new(), "00".repeat(56)]);
    assert!(
        matches!(cause(&bad_point), Some(CryptoCause::Sigma { error }) if error.kind() == SigmaErrorKind::InvalidPoint),
        "{bad_point:?}"
    );
    let bad_g1 = refusal("bn254_add", &["01".repeat(64), "00".repeat(64)]);
    assert!(matches!(cause(&bad_g1), Some(CryptoCause::Zk { .. })), "{bad_g1:?}");
    let short_g1 = refusal("bn254_add", &["00".repeat(63), "00".repeat(64)]);
    assert_eq!(cause(&short_g1), None, "a width the operation checks itself");
}
