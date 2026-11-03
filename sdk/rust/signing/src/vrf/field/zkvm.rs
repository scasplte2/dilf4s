//! GF(2^255 - 19) multiplication on SP1's `sys_bigint` (issue #69), for [`super::Fe::pow_p58`]'s addition chain in the
//! SP1 guest. The only `unsafe` in `dilf4s-signing`, and only on `target_os = "zkvm"`, `target_vendor = "succinct"`.
//!
//! [`Canonical`] holds a value below `p` by construction, so every multiplication's inputs are fully reduced, and
//! every product is checked to be below `p` before it becomes one: a product that is not is
//! [`NonCanonical`], never a panic.

#![allow(unsafe_code, reason = "the one call into SP1's sys_bigint; issue #69")]

use super::ChainField;

/// `p = 2^255 - 19`, as little-endian 64-bit limbs.
const P: [u64; 4] = [0xffff_ffff_ffff_ffed, 0xffff_ffff_ffff_ffff, 0xffff_ffff_ffff_ffff, 0x7fff_ffff_ffff_ffff];

// SAFETY: `sys_bigint` is defined, with exactly this signature, by `sp1-zkvm` 6.10.0 (`syscalls/bigint.rs`), which
// every SP1 guest links (the dilf4s guest pins `sp1-zkvm = "=6.10.0"`). It reads 32 bytes through each of `x`, `y` and
// `modulus`, writes 32 bytes through `result`, needs all four 8-byte aligned, and sets `result` to
// `x y mod modulus` (`op` is unused). Declared here rather than taken from `sp1-lib`, which would add `sp1-lib` and
// its dependencies to the workspace lockfile for one declaration.
unsafe extern "C" {
    fn sys_bigint(result: *mut [u64; 4], op: u64, x: *const [u64; 4], y: *const [u64; 4], modulus: *const [u64; 4]);
}

/// A field element as its value, below `p`, in little-endian 64-bit limbs.
#[derive(Clone, Copy)]
pub(super) struct Canonical([u64; 4]);

/// A `sys_bigint` product that is not below `p`: it cannot come from a correct precompile.
#[derive(Debug)]
pub(in crate::vrf) struct NonCanonical;

/// `limbs < p`, comparing from the most significant limb.
fn below_p(limbs: &[u64; 4]) -> bool {
    limbs.iter().rev().zip(P.iter().rev()).find(|(a, b)| a != b).is_some_and(|(a, b)| a < b)
}

impl Canonical {
    /// The value of 32 little-endian bytes, if it is below `p`.
    pub(super) fn from_bytes(bytes: &[u8; 32]) -> Result<Self, NonCanonical> {
        let (chunks, _) = bytes.as_chunks::<8>();
        let mut limbs = [0u64; 4];
        for (limb, chunk) in limbs.iter_mut().zip(chunks) {
            *limb = u64::from_le_bytes(*chunk);
        }
        if below_p(&limbs) { Ok(Self(limbs)) } else { Err(NonCanonical) }
    }

    /// The value as 32 little-endian bytes.
    pub(super) fn to_bytes(self) -> [u8; 32] {
        let mut out = [0u8; 32];
        let (chunks, _) = out.as_chunks_mut::<8>();
        for (chunk, limb) in chunks.iter_mut().zip(self.0) {
            *chunk = limb.to_le_bytes();
        }
        out
    }
}

impl ChainField for Canonical {
    type Error = NonCanonical;

    fn chain_mul(self, that: Self) -> Result<Self, NonCanonical> {
        let mut product = [0u64; 4];
        let modulus = P;
        // SAFETY: every pointer is to a live `[u64; 4]`: 32 bytes, 8-byte aligned, valid for the whole call; `product`
        // is a distinct local, the only one written. `self.0` and `that.0` are below `p` (the
        // `Canonical` invariant), and the modulus is `p`, not zero, so the precompile reduces mod `p` and not mod
        // `2^256`. The call returns nothing to unwrap, and the product is checked below before it is used.
        unsafe { sys_bigint(&raw mut product, 0, &raw const self.0, &raw const that.0, &raw const modulus) };
        if below_p(&product) { Ok(Self(product)) } else { Err(NonCanonical) }
    }

    fn chain_square(self) -> Result<Self, NonCanonical> {
        self.chain_mul(self)
    }
}
