//! The bytes a value commits to as a Merkle leaf or a trie value (docs/specs/merkle.md section 2).

use alloc::vec::Vec;

use crate::cbor;
use crate::value::Value;

/// The commitment tag of a byte string.
pub const BYTES_TAG: u8 = 0x00;
/// The commitment tag of any other value.
pub const CBOR_TAG: u8 = 0x01;

/// `0x00 || b` for a byte string `b`, `0x01 || cbor(v)` for any other value. The tag keeps the mapping injective:
/// the bytes `0x6161` and the text `"a"` (whose CBOR is `0x6161`) commit apart.
pub fn value_commitment(value: &Value) -> Vec<u8> {
    match value {
        Value::Bytes(b) => {
            let mut out = Vec::with_capacity(b.len().saturating_add(1));
            out.push(BYTES_TAG);
            out.extend_from_slice(b);
            out
        }
        other => {
            let encoded = cbor::encode(other);
            let mut out = Vec::with_capacity(encoded.len().saturating_add(1));
            out.push(CBOR_TAG);
            out.extend_from_slice(&encoded);
            out
        }
    }
}
