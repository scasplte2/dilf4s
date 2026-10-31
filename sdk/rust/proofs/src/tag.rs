//! Tagged hashing with the tag's hash computed once (docs/specs/canonical-encoding.md §4).

use dilf4s_core::hash::{Digest, Hasher};

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

    /// The tagged hash of the concatenation of `parts`.
    pub(crate) fn hash(&self, parts: &[&[u8]]) -> Digest<H> {
        let t = self.hash.as_ref();
        match parts {
            [] => Digest::from_output(H::hash_parts(&[t, t])),
            [a] => Digest::from_output(H::hash_parts(&[t, t, a])),
            [a, b] => Digest::from_output(H::hash_parts(&[t, t, a, b])),
            more => {
                let mut all: alloc::vec::Vec<&[u8]> = alloc::vec::Vec::with_capacity(more.len().saturating_add(2));
                all.push(t);
                all.push(t);
                all.extend_from_slice(more);
                Digest::from_output(H::hash_parts(&all))
            }
        }
    }
}

/// Lowercase hex of a digest, for error messages.
pub(crate) fn hex<H: Hasher>(d: &Digest<H>) -> alloc::string::String {
    dilf4s_core::hex::encode(d.as_bytes())
}
