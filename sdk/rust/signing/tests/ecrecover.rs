//! Ethereum signer recovery: go-ethereum's precompile vector (through its low-`s` twin), recovery of this crate's
//! own signatures, and every refusal.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    reason = "tests fail by panicking, and run with std"
)]

use dilf4s_core::hex;
use dilf4s_signing::ecrecover::{address_of_key, recover};

fn b(s: &str) -> Vec<u8> {
    hex::decode(s).unwrap()
}

fn sig(r: &str, s: &str, v: u8) -> Vec<u8> {
    let mut out = b(r);
    out.extend(b(s));
    out.push(v);
    out
}

// go-ethereum core/vm/testdata/precompiles/ecRecover.json, "ValidKey"
const DIGEST: &str = "18c547e4f7b0f325ad1e56f57e26c745b09a3e503d86e00e5255ff7f715d3d1c";
const R: &str = "73b1693892219d736caba55bdb67216e485557ea6b6af75f37096c9aa6a5a75f";
const HIGH_S: &str = "eeb940b1d03b21e36b0e47e79769f095fe2ab855bd91e3a38756b7d75a9c4549";
const ADDRESS: &str = "a94f5374fce5edbc8e2a8697c15331677e6ebf0b";
/// `n - HIGH_S`: the same signature with low `s`, which recovers the same key with the other parity.
const LOW_S: &str = "1146bf4e2fc4de1c94f1b81868960f68bc842490f1b6bc98387ba6b57599fbf8";
const N: &str = "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141";

#[test]
fn the_precompile_vector_recovers_through_its_low_s_twin() {
    let got = recover(&b(DIGEST), &sig(R, LOW_S, 27)).unwrap().map(|a| hex::encode(&a));
    assert_eq!(got.as_deref(), Some(ADDRESS));
    // 0 and 1 are 27 and 28
    assert_eq!(recover(&b(DIGEST), &sig(R, LOW_S, 0)).unwrap().map(|a| hex::encode(&a)).as_deref(), Some(ADDRESS));
}

#[test]
fn a_high_s_signature_recovers_nothing() {
    // the precompile accepts it; EIP-2 and OpenZeppelin do not, and neither does the JLVM
    assert_eq!(recover(&b(DIGEST), &sig(R, HIGH_S, 28)).unwrap(), None);
}

#[cfg(feature = "sign")]
#[test]
fn a_signature_of_this_crate_recovers_its_key_with_exactly_one_parity() {
    use dilf4s_core::hash::{Hasher, Sha256};
    use dilf4s_signing::ecdsa::secp256k1::from_seed;
    let (sk, vk) = from_seed(&[7u8; 32]).unwrap();
    let message = b"transfer 10 to bob";
    let signature = sk.sign(message).unwrap();
    let digest = Sha256::hash_parts(&[message]);
    let want = address_of_key(vk.as_bytes()).unwrap();
    let found: Vec<_> = [27u8, 28]
        .into_iter()
        .filter_map(|v| {
            let mut s = signature.as_bytes().to_vec();
            s.push(v);
            recover(&digest, &s).unwrap()
        })
        .collect();
    assert!(found.contains(&want), "one parity recovers the signer");
    assert_eq!(found.iter().filter(|a| **a == want).count(), 1);
}

#[test]
fn out_of_range_values_recover_nothing() {
    let zero = "00".repeat(32);
    // r or s zero, r or s not below n, and r = 5, which is no point's x-coordinate
    for (r, s) in [(zero.as_str(), LOW_S), (R, zero.as_str()), (N, LOW_S), (R, N)] {
        assert_eq!(recover(&b(DIGEST), &sig(r, s, 27)).unwrap(), None, "r {r} s {s}");
    }
    let five = format!("{}05", "00".repeat(31));
    for v in [27, 28] {
        assert_eq!(recover(&b(DIGEST), &sig(&five, LOW_S, v)).unwrap(), None);
    }
}

#[test]
fn wrong_sizes_and_another_v_are_malformed() {
    assert!(recover(&b(DIGEST)[1..], &sig(R, LOW_S, 27)).is_err(), "a 31-byte digest");
    assert!(recover(&b(DIGEST), &sig(R, LOW_S, 27)[1..]).is_err(), "a 64-byte signature");
    for v in [2u8, 26, 29, 35, 255] {
        assert!(recover(&b(DIGEST), &sig(R, LOW_S, v)).is_err(), "v = {v}");
    }
}

#[test]
fn an_address_from_a_compressed_or_uncompressed_key_is_the_same() {
    let compressed = b("0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798");
    // the generator's address, a well-known constant (the key of private key 1)
    assert_eq!(hex::encode(&address_of_key(&compressed).unwrap()), "7e5f4552091a69125d5dfcb7b8c2659029395bdf");
    // x = 5 is no point's x-coordinate
    assert!(address_of_key(&b(&format!("02{}05", "00".repeat(31)))).is_err());
}
