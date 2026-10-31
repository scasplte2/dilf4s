//! Key derivation (`signing.md` sections 2 and 3): BIP-39, SLIP-10 (Ed25519), BIP32 (secp256k1), BIP32-Ed25519
//! (Icarus), and derivation paths.
//!
//! Path parsing is always available; deriving keys needs the `sign` feature.

pub mod path;

#[cfg(feature = "sign")]
pub mod bip32;
#[cfg(feature = "sign")]
pub mod bip39;
#[cfg(feature = "sign")]
pub mod icarus;
#[cfg(feature = "sign")]
pub mod slip10;

#[cfg(feature = "sign")]
pub use extended::ExtendedKey;

#[cfg(feature = "sign")]
mod extended {
    use core::fmt;

    use zeroize::{Zeroize, ZeroizeOnDrop};

    /// An extended private key: a signing key and its 32-byte chain code.
    #[derive(Clone)]
    pub struct ExtendedKey<K> {
        /// The signing key.
        pub key: K,
        /// The chain code.
        pub chain_code: [u8; 32],
    }

    impl<K> fmt::Debug for ExtendedKey<K> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str("ExtendedKey(<redacted>)")
        }
    }

    impl<K> Drop for ExtendedKey<K> {
        fn drop(&mut self) {
            self.chain_code.zeroize();
        }
    }

    impl<K: ZeroizeOnDrop> ZeroizeOnDrop for ExtendedKey<K> {}
}

/// `HMAC-SHA512(key, parts...)`.
#[cfg(feature = "sign")]
pub(crate) fn hmac_sha512(key: &[u8], parts: &[&[u8]]) -> zeroize::Zeroizing<[u8; 64]> {
    use hmac::{KeyInit as _, Mac as _};
    let mut out = zeroize::Zeroizing::new([0u8; 64]);
    // HMAC takes keys of any length, so construction cannot fail; an error would leave the output zero
    if let Ok(mut mac) = hmac::Hmac::<sha2::Sha512>::new_from_slice(key) {
        for p in parts {
            mac.update(p);
        }
        out.copy_from_slice(&mac.finalize().into_bytes());
    }
    out
}

/// The two 32-byte halves of an HMAC-SHA512 output.
#[cfg(feature = "sign")]
pub(crate) fn split_i(i: &[u8; 64]) -> (zeroize::Zeroizing<[u8; 32]>, [u8; 32]) {
    let (l, r) = crate::util::halves(i);
    (zeroize::Zeroizing::new(l), r)
}
