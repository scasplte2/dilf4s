//! The signing side of `shared/vectors/signing_v1.json`: each scheme's keys and signatures (re-signed byte for byte,
//! and verified), proofs of possession, and every key derivation. The verdicts are in `verification_vectors.rs`.

#![cfg(feature = "sign")]
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    clippy::as_conversions,
    clippy::many_single_char_names,
    clippy::items_after_statements,
    clippy::doc_markdown,
    reason = "tests fail by panicking, and run with std"
)]

#[path = "../../core/tests/common/mod.rs"]
mod common;

use common::{bytes, load};
use dilf4s_core::generated::signing_vectors::SigningVectors;
use dilf4s_core::hex;
use dilf4s_signing::keys::{bip32, bip39, icarus, path, slip10};
use dilf4s_signing::{SchemeId, bls, ecdsa, ed25519, ristretto255, schnorr};

/// Checks every case, collecting failures so one run reports all of them; returns the number of cases.
fn run<T>(cases: &[T], check: impl Fn(&T) -> Result<(), String>) -> usize {
    let failures: Vec<String> = cases.iter().filter_map(|c| check(c).err()).collect();
    assert!(failures.is_empty(), "{} of {} cases failed:\n{}", failures.len(), cases.len(), failures.join("\n"));
    cases.len()
}

fn same(what: &str, expected: &str, actual: &str) -> Result<(), String> {
    if expected == actual { Ok(()) } else { Err(format!("{what}: expected {expected}, got {actual}")) }
}

fn ensure(what: &str, ok: bool) -> Result<(), String> {
    if ok { Ok(()) } else { Err(format!("{what}: failed")) }
}

/// One key of one scheme: the derived key pair, then every signature re-signed and verified.
fn check_key(
    scheme: &str,
    key: &dilf4s_core::generated::signing_vectors::SigningKey,
    aux: &[u8; 32],
) -> Result<(), String> {
    let seed = bytes(&key.seed);
    let id = SchemeId::from_name(scheme).ok_or_else(|| format!("unknown scheme {scheme}"))?;
    // (signing key, verifying key, signer)
    type Signer = Box<dyn Fn(&[u8]) -> Vec<u8>>;
    let (sk, vk, sign): (Vec<u8>, Vec<u8>, Signer) = match scheme {
        "Ed25519" => {
            let (sk, vk) = ed25519::from_seed(&seed).map_err(|e| e.to_string())?;
            (sk.as_bytes().to_vec(), vk.as_bytes().to_vec(), Box::new(move |m| sk.sign(m).as_bytes().to_vec()))
        }
        "P-256-ECDSA" => {
            let (sk, vk) = ecdsa::p256::from_seed(&seed).map_err(|e| e.to_string())?;
            (sk.to_bytes().to_vec(), vk.as_bytes().to_vec(), Box::new(move |m| sk.sign(m).unwrap().as_bytes().to_vec()))
        }
        "secp256k1-ECDSA" => {
            let (sk, vk) = ecdsa::secp256k1::from_seed(&seed).map_err(|e| e.to_string())?;
            (sk.to_bytes().to_vec(), vk.as_bytes().to_vec(), Box::new(move |m| sk.sign(m).unwrap().as_bytes().to_vec()))
        }
        "BIP-340" => {
            let (sk, vk) = schnorr::bip340::from_seed(&seed).map_err(|e| e.to_string())?;
            let aux = *aux;
            (
                sk.as_bytes().to_vec(),
                vk.as_bytes().to_vec(),
                Box::new(move |m| sk.sign(m.try_into().unwrap(), &aux).unwrap().as_bytes().to_vec()),
            )
        }
        "secp256k1-tagged-Schnorr" => {
            let (sk, vk) = schnorr::tagged::from_seed(&seed).map_err(|e| e.to_string())?;
            let aux = *aux;
            (
                sk.as_bytes().to_vec(),
                vk.as_bytes().to_vec(),
                Box::new(move |m| sk.sign(m, &aux).unwrap().as_bytes().to_vec()),
            )
        }
        "ristretto255-Schnorr" => {
            let (sk, vk) = ristretto255::from_seed(&seed).map_err(|e| e.to_string())?;
            let aux = *aux;
            (sk.to_bytes().to_vec(), vk.as_bytes().to_vec(), Box::new(move |m| sk.sign(m, &aux).as_bytes().to_vec()))
        }
        "BLS12-381-PoP" => {
            let (sk, vk) = bls::from_seed(&seed).map_err(|e| e.to_string())?;
            (sk.as_bytes().to_vec(), vk.as_bytes().to_vec(), Box::new(move |m| sk.sign(m).as_bytes().to_vec()))
        }
        other => return Err(format!("unexpected scheme {other}")),
    };
    same(&format!("{scheme} signing key"), &key.signing_key, &hex::encode(&sk))?;
    same(&format!("{scheme} verifying key"), &key.verifying_key, &hex::encode(&vk))?;
    for s in &key.signatures {
        let m = bytes(&s.message);
        same(&format!("{scheme} signature on {}", s.message), &s.signature, &hex::encode(&sign(&m)))?;
        ensure(
            &format!("{scheme} verifies {}", s.message),
            id.verify_bytes(&vk, &m, &bytes(&s.signature)) == Ok(true),
        )?;
    }
    Ok(())
}

#[test]
fn every_scheme_signs_and_verifies_byte_for_byte() {
    let v: SigningVectors = load("signing_v1.json");
    let aux: [u8; 32] = bytes(&v.randomness).try_into().unwrap();
    let mut count = 0;
    for scheme in &v.signing {
        count += run(&scheme.keys, |k| check_key(&scheme.scheme, k, &aux));
    }
    assert_eq!(count, 21, "7 schemes x 3 keys");
}

#[test]
fn bls_proofs_of_possession() {
    let v: SigningVectors = load("signing_v1.json");
    let n = run(&v.bls_pop, |p| {
        let sk = bls::SigningKey::from_bytes(&bytes(&p.signing_key)).map_err(|e| e.to_string())?;
        let vk = sk.verifying_key();
        same("PoP verifying key", &p.verifying_key, &hex::encode(vk.as_bytes()))?;
        let proof = sk.pop_prove();
        same("PoP", &p.proof, &hex::encode(proof.as_bytes()))?;
        ensure("PopVerify", bls::pop_verify(&vk, &proof))?;
        ensure("a PoP is not a message signature", !bls::verify(&vk, vk.as_bytes(), &proof))
    });
    assert_eq!(n, 3);
}

#[test]
fn bip39_mnemonics_and_seeds() {
    let v: SigningVectors = load("signing_v1.json");
    let english = bip39::WordList::english();
    let n = run(&v.bip39, |c| {
        let entropy = bytes(&c.entropy);
        let mnemonic = bip39::to_mnemonic(&entropy, &english).map_err(|e| e.to_string())?;
        same("mnemonic", &c.mnemonic, &mnemonic.phrase())?;
        let (parsed, back) = bip39::to_entropy(&c.mnemonic, &english).map_err(|e| e.to_string())?;
        same("entropy", &c.entropy, &hex::encode(back.expose()))?;
        same("seed", &c.seed, &hex::encode(bip39::seed(&parsed, &c.passphrase).expose()))
    });
    assert_eq!(n, 6);
}

#[test]
fn slip10_bip32_and_icarus_derivations() {
    let v: SigningVectors = load("signing_v1.json");

    let s = &v.slip10_ed25519;
    let master = slip10::master(&bytes(&s.seed)).unwrap();
    assert_eq!(hex::encode(master.key.as_bytes()), s.master.key);
    assert_eq!(hex::encode(&master.chain_code), s.master.chain_code);
    let child = slip10::derive(&bytes(&s.seed), &path::parse(&s.path).unwrap()).unwrap();
    assert_eq!(hex::encode(child.key.as_bytes()), s.child.key);
    assert_eq!(hex::encode(&child.chain_code), s.child.chain_code);

    let b = &v.bip32_secp256k1;
    let master = bip32::master(&bytes(&b.seed)).unwrap();
    assert_eq!(hex::encode(&*master.key.to_bytes()), b.master.key);
    assert_eq!(hex::encode(&master.chain_code), b.master.chain_code);
    let child = bip32::derive(&bytes(&b.seed), &path::parse(&b.path).unwrap()).unwrap();
    assert_eq!(hex::encode(&*child.key.to_bytes()), b.child.key);
    assert_eq!(hex::encode(&child.chain_code), b.child.chain_code);
    assert_eq!(hex::encode(child.key.verifying_key().as_bytes()), b.verifying_key);

    let p = &b.public_derivation;
    let parent = bip32::derive(&bytes(&b.seed), &path::parse(&p.parent_path).unwrap()).unwrap();
    assert_eq!(hex::encode(parent.key.verifying_key().as_bytes()), p.parent_verifying_key);
    assert_eq!(hex::encode(&parent.chain_code), p.parent_chain_code);
    let mut vk = ecdsa::secp256k1::VerifyingKey::from_bytes(&bytes(&p.parent_verifying_key)).unwrap();
    let mut cc: [u8; 32] = bytes(&p.parent_chain_code).try_into().unwrap();
    for index in path::parse(&p.path).unwrap() {
        (vk, cc) = bip32::public_child(&vk, &cc, index).unwrap();
    }
    assert_eq!(hex::encode(vk.as_bytes()), p.verifying_key);
    assert_eq!(hex::encode(&cc), p.chain_code);
    assert_eq!(p.verifying_key, b.verifying_key, "CKDpub reaches the private path's key");

    let i = &v.ed25519_bip32;
    let root = icarus::from_entropy(&bytes(&i.entropy), &i.passphrase).unwrap();
    assert_eq!(hex::encode(&*root.key.to_bytes()), i.root.key);
    assert_eq!(hex::encode(&root.chain_code), i.root.chain_code);
    let account = icarus::derive(&root, &path::parse(&i.account_path).unwrap()).unwrap();
    assert_eq!(hex::encode(&*account.key.to_bytes()), i.account.key);
    assert_eq!(hex::encode(&account.chain_code), i.account.chain_code);
    assert_eq!(hex::encode(account.key.verifying_key().as_bytes()), i.account_verifying_key);
    let address = icarus::derive(&account, &path::parse(&i.address_path).unwrap()).unwrap();
    assert_eq!(hex::encode(&*address.key.to_bytes()), i.address.key);
    assert_eq!(hex::encode(&address.chain_code), i.address.chain_code);
    let address_vk = address.key.verifying_key();
    assert_eq!(hex::encode(address_vk.as_bytes()), i.address_verifying_key);

    // public derivation from the account reaches the same address key
    let mut pk = account.key.verifying_key();
    let mut cc = account.chain_code;
    for index in path::parse(&i.address_path).unwrap() {
        (pk, cc) = icarus::public_child(&pk, &cc, index).unwrap();
    }
    assert_eq!(pk, address_vk);
    assert_eq!(cc, address.chain_code);

    let n = run(&i.signatures, |s| {
        let m = bytes(&s.message);
        let sig = address.key.sign(&m);
        same("Icarus signature", &s.signature, &hex::encode(sig.as_bytes()))?;
        ensure("Icarus signature verifies", ed25519::verify(&address_vk, &m, &sig))
    });
    assert_eq!(n, 3);
}
