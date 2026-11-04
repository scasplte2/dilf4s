//! Tagged hashing with the tag's hash computed once (docs/specs/canonical-encoding.md §4).

use dilf4s_core::commitment::value_commitment_into;
use dilf4s_core::hash::{Digest, Hasher, Hashing};
use dilf4s_core::value::Value;

/// `taggedHash_H(tag, msg) = H(H(tag) || H(tag) || msg)` for one fixed tag. The tags here are constants of the specs,
/// all within the `DomainTag` grammar (a test checks them against `dilf4s_core::hash::TaggedHash`).
pub(crate) struct Tag<H: Hasher> {
    hash: H::Output,
}

impl<H: Hasher> Clone for Tag<H> {
    fn clone(&self) -> Self {
        Self { hash: self.hash.clone() }
    }
}

impl<H: Hasher> Tag<H> {
    pub(crate) fn new(tag: &str) -> Self {
        Self { hash: H::hash_parts(&[tag.as_bytes()]) }
    }

    /// A hash with the prefix `H(tag) || H(tag)` absorbed.
    pub(crate) fn start(&self) -> Hashing<H> {
        let t = self.hash.as_ref();
        let mut h = Hashing::new();
        h.update(t);
        h.update(t);
        h
    }

    /// The tagged hash of `commit(value)`, streamed into the hash: O([`Value::weight`]) time, and no memory
    /// proportional to the commitment.
    pub(crate) fn commitment(&self, value: &Value) -> Digest<H> {
        let mut h = self.start();
        value_commitment_into(&mut h, value);
        h.finish()
    }

    /// The tagged hash of the concatenation of `parts`.
    pub(crate) fn hash(&self, parts: &[&[u8]]) -> Digest<H> {
        let mut h = self.start();
        for p in parts {
            h.update(p);
        }
        h.finish()
    }
}

/// Lowercase hex of a digest, for error messages.
pub(crate) fn hex<H: Hasher>(d: &Digest<H>) -> alloc::string::String {
    dilf4s_core::hex::encode(d.as_bytes())
}
