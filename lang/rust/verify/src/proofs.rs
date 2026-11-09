//! The Kani harnesses (`cargo kani -p dilf4s-verify`, with Kani 0.68.0). Each runs one property of this crate on every
//! input its harness covers, and Kani checks every call it makes for panics, arithmetic overflow, out-of-bounds
//! access, division by zero and unreachable code, and that every loop is fully unwound within the harness's `unwind`
//! (a bound too low fails rather than passing vacuously).
//!
//! | Harness | Inputs | Time | Peak memory |
//! |---|---|---|---|
//! | `gas_clamp` | every `u64` amount, amount and count | 19 s | 0.2 GB |
//! | `gas_meter` | every `u64` amount used, charge and limit | 0.1 s | 0.2 GB |
//! | `gas_rendered` | every two [`scalar`]s: null, booleans, `i128`s, texts and byte strings | 39 s | 2.4 GB |
//! | `bit_length` | every `i128` | 10 s | 0.6 GB |
//! | `op_wire` | every text of at most 6 bytes | 50 s | 3.6 GB |
//! | `text_measures` | every text of at most 4 bytes | 27 s | 2.4 GB |
//!
//! Measured with Kani 0.68.0 (CBMC 6.11.0) on euler under a 12 GB cap, as CI's `kani` job runs them (every one, in
//! turn): 2.5 minutes in all with the build, at most 3.6 GB.
//!
//! **What does not fit.** CBMC, Kani's back end, executes the program symbolically, and it does not fold what the code
//! reads back from the heap: a `Value` held in memory, inside a `Result` or a container, is of any kind as far as it
//! can tell, and a `Vec` or `String` of a length it cannot fold grows by every path. Dropping such a `Value` explores
//! every kind, and a container's release (`lang/scala/core/src/value/map.rs`) recurses through its elements' kinds. So harnesses
//! here forget the values they build (`core::mem::forget`, leaving only the code under test), and the decoders and
//! parsers, which build and drop containers on every path the checker cannot rule out whatever the input, do not fit:
//!
//! | Harness (removed) | Inputs | Result |
//! |---|---|---|
//! | `cbor::decode` | the one fixed byte `[0x01]` | over 8 GB in 5 minutes |
//! | `cbor::decode`, [`crate::canonical::cbor_decode`] | every byte | over 10 GB in 21 minutes |
//! | `json::parse`, [`crate::canonical::json_parse`] | every one-byte text | over 10 GB in 21 minutes |
//! | `Value::array([1])`, dropped | that value | over 10 GB in 7 minutes |
//! | `dilf4s_jlvm::decode`, `parse` | the empty input | over 10 minutes (eight harnesses at once) |
//! | `hex::decode(hex::encode(b))` | the empty `b` | over 12 GB |
//! | `Preview::of`, the canonical key order, `select_alternative` | texts of at most 4 and 2 bytes, maps of 3 keys | over 12 GB |
//! | `GasSchedule::sized_within`, [`crate::gas::gas_cap`] | every operation, two [`scalar`]s, every cap | over 12 GB |
//!
//! Their properties are in this crate all the same, and run on concrete inputs (`tests/`): every byte string of up to
//! two bytes and every conformance vector.

use dilf4s_core::Value;

use crate::{gas, helpers};

/// Every byte string of at most `N` bytes: a prefix of any length of `N` arbitrary bytes.
fn prefix<const N: usize>(buf: &[u8; N]) -> &[u8] {
    let len: usize = kani::any_where(|l| *l <= N);
    buf.get(..len).unwrap_or(&[])
}

/// `bytes` as text, if they are UTF-8: a `&str` is UTF-8, so other byte strings are not inputs.
fn text(bytes: &[u8]) -> Option<&str> {
    core::str::from_utf8(bytes).ok()
}

// ------------------------------------------------------------------------------------------------ gas

/// Gas arithmetic on every amount and count: [`gas::gas_clamp`]. Unbounded.
#[kani::proof]
fn gas_clamp() {
    assert!(gas::gas_clamp(kani::any(), kani::any(), kani::any()).is_ok());
}

/// The meter's test on every amount used, charge and limit: [`gas::gas_meter`]. Unbounded.
#[kani::proof]
fn gas_meter() {
    assert!(gas::gas_meter(kani::any(), kani::any(), kani::any()).is_ok());
}

/// One of: null, a boolean, every 128-bit integer, every ASCII text and byte string of at most 2 bytes.
fn scalar() -> Value {
    let ascii: [u8; 2] = kani::any();
    let ascii = ascii.map(|b| b & 0x7f);
    let bytes = prefix(&ascii);
    match kani::any::<u8>() {
        0 => Value::Null,
        1 => Value::Bool(kani::any()),
        2 => Value::int(kani::any::<i128>()),
        // ASCII, so always text
        3 => Value::text(text(bytes).unwrap_or_default()),
        _ => Value::bytes(bytes),
    }
}

/// `GasSchedule::rendered` of every two [`scalar`]s, so of every two 128-bit integers: [`gas::gas_rendered`].
#[kani::proof]
#[kani::unwind(4)]
fn gas_rendered() {
    let args = [scalar(), scalar()];
    assert!(gas::gas_rendered(&args).is_ok());
    core::mem::forget(args);
}

// ------------------------------------------------------------------------------------------------ helpers

/// The bit length and weight of every primitive integer: [`helpers::bit_length`]. Unbounded over `i128`.
#[kani::proof]
#[kani::unwind(4)]
fn bit_length() {
    assert!(helpers::bit_length(kani::any()).is_ok());
}

/// `Op::from_wire` of every text of at most 6 bytes, which every one-to-six-byte operation name is:
/// [`helpers::op_wire`].
#[kani::proof]
#[kani::unwind(8)]
fn op_wire() {
    let buf: [u8; 6] = kani::any();
    if let Some(t) = text(prefix(&buf)) {
        assert!(helpers::op_wire(t).is_ok());
    }
}

/// The UTF-16 length and weight of every text of at most 4 bytes, so of every code point: [`helpers::text_measures`].
#[kani::proof]
#[kani::unwind(6)]
fn text_measures() {
    let buf: [u8; 4] = kani::any();
    if let Some(t) = text(prefix(&buf)) {
        assert!(helpers::text_measures(t).is_ok());
    }
}
