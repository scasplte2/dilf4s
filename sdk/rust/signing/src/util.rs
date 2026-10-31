//! Small byte helpers shared by the schemes: fixed-size splits, lengths, hashing.

use alloc::format;
use alloc::vec::Vec;

use dilf4s_core::hash::{Hasher, Sha256, Sha512};

use crate::error::SigningError;

/// Exactly `N` bytes, or `InvalidEncoding` naming `what`.
pub(crate) fn exact<const N: usize>(what: &'static str, bytes: &[u8]) -> Result<[u8; N], SigningError> {
    <[u8; N]>::try_from(bytes)
        .map_err(|_| SigningError::InvalidEncoding { what, reason: format!("expected {N} bytes, got {}", bytes.len()) })
}

/// The two 32-byte halves of 64 bytes.
pub(crate) fn halves(bytes: &[u8; 64]) -> ([u8; 32], [u8; 32]) {
    let (x, y) = bytes.split_at(32);
    let (mut a, mut b) = ([0u8; 32], [0u8; 32]);
    a.copy_from_slice(x);
    b.copy_from_slice(y);
    (a, b)
}

/// `a || b` as one array.
#[cfg(feature = "sign")]
pub(crate) fn join<const A: usize, const B: usize, const N: usize>(a: &[u8; A], b: &[u8; B]) -> [u8; N] {
    let mut out = [0u8; N];
    for (o, v) in out.iter_mut().zip(a.iter().chain(b.iter())) {
        *o = *v;
    }
    out
}

/// The concatenation of `parts`.
pub(crate) fn concat(parts: &[&[u8]]) -> Vec<u8> {
    let mut out = Vec::with_capacity(parts.iter().map(|p| p.len()).fold(0usize, usize::saturating_add));
    for p in parts {
        out.extend_from_slice(p);
    }
    out
}

pub(crate) fn sha512(parts: &[&[u8]]) -> [u8; 64] {
    Sha512::hash_parts(parts)
}

pub(crate) fn sha256(parts: &[&[u8]]) -> [u8; 32] {
    Sha256::hash_parts(parts)
}

/// `SHA-256(SHA-256(tag) || SHA-256(tag) || parts...)`, the BIP-340 tagged hash (`canonical-encoding.md` section 4).
pub(crate) fn tagged_sha256(tag: &[u8], parts: &[&[u8]]) -> [u8; 32] {
    let t = sha256(&[tag]);
    let mut all: Vec<&[u8]> = Vec::with_capacity(parts.len().saturating_add(2));
    all.push(&t);
    all.push(&t);
    all.extend_from_slice(parts);
    sha256(&all)
}

/// Whether the little-endian integer `a` is below the little-endian `bound` (variable time: public data only).
pub(crate) fn le_less_than(a: &[u8; 32], bound: &[u8; 32]) -> bool {
    a.iter().rev().cmp(bound.iter().rev()) == core::cmp::Ordering::Less
}

/// Whether the big-endian integer `a` is below the big-endian `bound` (variable time: public data only).
pub(crate) fn be_less_than(a: &[u8; 32], bound: &[u8; 32]) -> bool {
    a.as_slice() < bound.as_slice()
}

/// The order `L` of the ed25519 / ristretto255 prime-order group, little-endian.
pub(crate) const ED25519_L: [u8; 32] = [
    0xed, 0xd3, 0xf5, 0x5c, 0x1a, 0x63, 0x12, 0x58, 0xd6, 0x9c, 0xf7, 0xa2, 0xde, 0xf9, 0xde, 0x14, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0x10,
];

/// `p = 2^255 - 19`, little-endian.
pub(crate) const ED25519_P: [u8; 32] = [
    0xed, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f,
];

/// Whether the `y` of an edwards25519 encoding (bit 255 cleared) is below `p`.
pub(crate) fn canonical_y(encoding: &[u8; 32]) -> bool {
    let mut y = *encoding;
    if let Some(last) = y.last_mut() {
        *last &= 0x7f;
    }
    le_less_than(&y, &ED25519_P)
}
