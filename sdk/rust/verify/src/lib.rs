//! Properties of the dilf4s decoders, parsers and gas arithmetic, and Kani proofs of those Kani can take.
//!
//! Each property is an ordinary function over one input that returns which of its claims failed, if any: it compiles,
//! is linted and is tested on concrete inputs (`tests/`) with the rest of the workspace. Under `cargo kani -p
//! dilf4s-verify` each harness in `proofs` runs one on every input it covers and also checks that nothing the property
//! calls panics, overflows, indexes out of bounds or reaches unreachable code on the way, which no claim needs to
//! state. `src/proofs.rs` says which properties Kani can take, at what bound and cost, and why not the others.
//!
//! Only the public APIs of `dilf4s-core` and `dilf4s-jlvm` are used: the SP1 guest links both, and their sources fix
//! its verifying key (`sdk/rust/zk/README.md`).

#![no_std]

extern crate alloc;

pub mod canonical;
pub mod gas;
pub mod helpers;
pub mod program;

#[cfg(kani)]
mod proofs;

/// A claim that does not hold, named for what it states.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Broken {
    /// A decoded value's canonical encoding is not the bytes it was decoded from.
    CborRoundTrip,
    /// A decoded value weighs more than its encoding is long: `encode_within` refuses by that bound before encoding.
    CborWeight,
    /// `encode_within` or `encoded_len_within` at the encoding's length is not the encoding, or a byte less is not
    /// refused.
    CborWithin,
    /// `decode_with_limit` does not succeed exactly when `decode` does with a value within the depth limit, or it
    /// decodes another value.
    CborDepthLimit,
    /// A parsed JSON value does not render, or its rendering does not parse back to it.
    JsonRoundTrip,
    /// A rendering is shorter than its value's weight (the bound `render_within` refuses by), or a byte less is not
    /// refused.
    JsonWithin,
    /// A rendering within a limit differs from the unlimited one, or a refusal does not match it.
    JsonBudget,
    /// A decoded program's canonical encoding is not the bytes it was decoded from.
    ProgramRoundTrip,
    /// A parsed program's canonical encoding does not decode back to it.
    SurfaceRoundTrip,
    /// Gas arithmetic is not exact arithmetic clamped at `Gas::MAX`.
    GasClamp,
    /// The meter's test of a charge does not decide as exact arithmetic does.
    GasMeter,
    /// `sized_within` disagrees with `sized` on a charge the cap can pay, or on whether the cap is exceeded.
    GasCap,
    /// The charge for rendering numbers is not jlvm.md's formula.
    GasRendered,
    /// Hex does not round-trip.
    Hex,
    /// An integer's bit length is not Java's `bitLength`, or its weight does not follow from it.
    BitLength,
    /// A text's cached UTF-16 length or weight is not its own.
    TextMeasures,
    /// A preview is not the text's first code points, or misreports its truncation or length.
    Preview,
    /// The canonical key order is not the order of the keys' encodings.
    KeyOrder,
    /// An operation's wire name does not read back as it.
    OpWire,
    /// `select_alternative` does not pick the first alternative that fits.
    Alternative,
}

/// `Ok` when `claim` holds, else `broken`.
pub(crate) fn holds(claim: bool, broken: Broken) -> Result<(), Broken> {
    if claim { Ok(()) } else { Err(broken) }
}
