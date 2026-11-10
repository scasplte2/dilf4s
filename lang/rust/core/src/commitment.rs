//! The bytes a value commits to as a Merkle leaf or a trie value (docs/specs/merkle.md section 2).

use alloc::vec::Vec;

use crate::cbor::{self, Sink, Staged};
use crate::hash::{Hasher, Hashing};
use crate::value::Value;

/// The commitment tag of a byte string.
pub const BYTES_TAG: u8 = 0x00;
/// The commitment tag of any other value.
pub const CBOR_TAG: u8 = 0x01;

/// `0x00 || b` for a byte string `b`, `0x01 || cbor(v)` for any other value. The tag keeps the mapping injective:
/// the bytes `0x6161` and the text `"a"` (whose CBOR is `0x6161`) commit apart.
///
/// O([`Value::weight`]) time and memory, as [`cbor::encode`]: bound the weight of a value built with shared parts first.
/// To hash a commitment, stream it with [`value_commitment_into`] instead.
pub fn value_commitment(value: &Value) -> Vec<u8> {
    let mut out = Vec::new();
    match write_commitment(&mut out, value) {
        Ok(()) => out,
        Err(never) => match never {},
    }
}

/// Streams [`value_commitment`]'s bytes into `hashing`, without building them: O([`Value::weight`]) time, and no
/// memory proportional to the commitment (see [`cbor::encode_into`]).
pub fn value_commitment_into<H: Hasher>(hashing: &mut Hashing<H>, value: &Value) {
    let mut staged = Staged::new(hashing);
    match write_commitment(&mut staged, value) {
        Ok(()) => staged.flush(),
        Err(never) => match never {},
    }
}

/// The one writer of a commitment, into a buffer or a hash.
fn write_commitment<S: Sink>(out: &mut S, value: &Value) -> Result<(), S::Error> {
    match value {
        Value::Bytes(b) => {
            out.put(&[BYTES_TAG])?;
            out.put(b)
        }
        other => {
            out.put(&[CBOR_TAG])?;
            cbor::write_value(out, other)
        }
    }
}
