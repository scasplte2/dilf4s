//! `shared/vectors/kes_v1.json` (every period of each construction: key encodings, signatures, loading, evolution
//! and verification) and the Cardano Sum6 KES reference data in `docs/test-vectors/kes/cardano`.

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

use std::path::PathBuf;

use common::{bytes, load};
use dilf4s_core::generated::kes_vectors::{KesConstruction, KesParameters, KesVector, KesVectors};
use dilf4s_core::hash::HashAlgorithm;
use dilf4s_core::hex;
use dilf4s_signing::KesError;
use dilf4s_signing::kes::{CardanoSumKes, KesProduct, KesSum};

enum Kes {
    Sum(KesSum),
    Product(KesProduct),
    Cardano(CardanoSumKes),
}

fn scheme(v: &KesVector) -> Kes {
    let height = |h: u64| u32::try_from(h).unwrap();
    match (&v.construction, &v.parameters) {
        (KesConstruction::Dilf4sSum, KesParameters::KesSumParameters(p)) => {
            Kes::Sum(KesSum::new(HashAlgorithm::from_name(&p.hash).unwrap(), height(p.height)).unwrap())
        }
        (KesConstruction::Dilf4sProduct, KesParameters::KesProductParameters(p)) => Kes::Product(
            KesProduct::new(HashAlgorithm::from_name(&p.hash).unwrap(), height(p.super_height), height(p.sub_height))
                .unwrap(),
        ),
        (KesConstruction::CardanoSum, KesParameters::KesCardanoParameters(p)) => {
            Kes::Cardano(CardanoSumKes::new(height(p.depth)).unwrap())
        }
        other => panic!("unexpected construction {other:?}"),
    }
}

fn verify(kes: &Kes, vk: &[u8], period: u64, message: &[u8], signature: &[u8]) -> Result<bool, KesError> {
    match kes {
        Kes::Sum(k) => k.verify(vk, period, message, signature),
        Kes::Product(k) => k.verify(vk, period, message, signature),
        Kes::Cardano(k) => k.verify(vk, period, message, signature),
    }
}

fn total(kes: &Kes) -> u64 {
    match kes {
        Kes::Sum(k) => k.total_periods(),
        Kes::Product(k) => k.total_periods(),
        Kes::Cardano(k) => k.total_periods(),
    }
}

#[test]
fn every_period_verifies_and_nowhere_else() {
    let v: KesVectors = load("kes_v1.json");
    let mut count = 0;
    for vector in &v.vectors {
        let kes = scheme(vector);
        let (vk, message) = (bytes(&vector.verifying_key), bytes(&vector.message));
        assert_eq!(vector.periods.len() as u64, total(&kes));
        for p in &vector.periods {
            let sig = bytes(&p.signature);
            assert_eq!(
                verify(&kes, &vk, p.period, &message, &sig),
                Ok(true),
                "{:?} period {}",
                vector.construction,
                p.period
            );
            for other in (0..total(&kes)).filter(|o| *o != p.period) {
                assert_eq!(verify(&kes, &vk, other, &message, &sig), Ok(false), "other period {other}");
            }
            let mut wrong = message.clone();
            wrong.push(0);
            assert_eq!(verify(&kes, &vk, p.period, &wrong, &sig), Ok(false));
            assert!(matches!(verify(&kes, &vk, total(&kes), &message, &sig), Err(KesError::PeriodOutOfRange { .. })));
            assert!(matches!(verify(&kes, &vk[1..], p.period, &message, &sig), Err(KesError::Malformed { .. })));
            assert!(matches!(verify(&kes, &vk, p.period, &message, &sig[1..]), Err(KesError::Malformed { .. })));
            count += 1;
        }
    }
    assert_eq!(count, 4 + 2 + 8 + 4);
}

#[cfg(feature = "sign")]
#[test]
fn keys_signatures_loading_and_evolution_byte_for_byte() {
    let v: KesVectors = load("kes_v1.json");
    for vector in &v.vectors {
        let (seed, message) = (bytes(&vector.seed), bytes(&vector.message));
        let last = vector.periods.last().unwrap().period;
        match scheme(vector) {
            Kes::Sum(k) => {
                let mut key = k.generate(&seed).unwrap();
                assert_eq!(hex::encode(&k.verifying_key(&key)), vector.verifying_key);
                for p in &vector.periods {
                    assert_eq!(key.period(), p.period);
                    assert_eq!(hex::encode(&k.key_bytes(&key)), p.key, "sum key at {}", p.period);
                    assert_eq!(hex::encode(&k.sign(&key, &message).unwrap()), p.signature, "sum sig at {}", p.period);
                    let loaded = k.load(&bytes(&p.key), p.period).unwrap();
                    assert_eq!(hex::encode(&k.sign(&loaded, &message).unwrap()), p.signature);
                    assert_eq!(k.verifying_key(&loaded), k.verifying_key(&key));
                    if p.period != last {
                        assert!(k.load(&bytes(&p.key), p.period + 1).is_err(), "loads only at its period");
                        key = k.evolve(&key).unwrap();
                    }
                }
                assert_eq!(k.evolve(&key).unwrap_err(), KesError::Exhausted { period: last });
            }
            Kes::Product(k) => {
                let mut key = k.generate(&seed).unwrap();
                assert_eq!(hex::encode(&k.verifying_key(&key)), vector.verifying_key);
                for p in &vector.periods {
                    assert_eq!(key.period(), p.period);
                    assert_eq!(hex::encode(&k.key_bytes(&key)), p.key, "product key at {}", p.period);
                    assert_eq!(
                        hex::encode(&k.sign(&key, &message).unwrap()),
                        p.signature,
                        "product sig at {}",
                        p.period
                    );
                    let loaded = k.load(&bytes(&p.key), p.period).unwrap();
                    assert_eq!(hex::encode(&k.sign(&loaded, &message).unwrap()), p.signature);
                    if p.period != last {
                        assert!(k.load(&bytes(&p.key), p.period + 1).is_err(), "loads only at its period");
                        key = k.evolve(&key).unwrap();
                    }
                }
                assert_eq!(k.evolve(&key).unwrap_err(), KesError::Exhausted { period: last });
            }
            Kes::Cardano(k) => {
                let mut key = k.generate(&seed).unwrap();
                assert_eq!(hex::encode(&k.verifying_key(&key)), vector.verifying_key);
                for p in &vector.periods {
                    assert_eq!(key.period(), p.period);
                    assert_eq!(hex::encode(&key.to_bytes()), p.key, "cardano key at {}", p.period);
                    assert_eq!(hex::encode(&k.sign(&key, &message)), p.signature, "cardano sig at {}", p.period);
                    let loaded = k.load(&bytes(&p.key), p.period).unwrap();
                    assert_eq!(hex::encode(&k.sign(&loaded, &message)), p.signature);
                    if p.period != last {
                        assert!(k.load(&bytes(&p.key), p.period + 1).is_err(), "loads only at its period");
                        key = k.evolve(&key).unwrap();
                    }
                }
                assert_eq!(k.evolve(&key).unwrap_err(), KesError::Exhausted { period: last });
            }
        }
    }
}

// ------------------------------------------------------------------ Cardano reference data

fn cardano(name: &str) -> Vec<Vec<String>> {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../docs/test-vectors/kes/cardano").join(name);
    std::fs::read_to_string(path).unwrap().lines().map(|l| l.split('\t').map(String::from).collect()).collect()
}

#[test]
fn cardano_fixture_sums() {
    let rows = cardano("SHA256SUMS");
    assert_eq!(rows.len(), 3);
    for row in rows {
        let (sum, name) = row[0].split_once("  ").unwrap();
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../docs/test-vectors/kes/cardano").join(name);
        assert_eq!(hex::encode(&HashAlgorithm::Sha256.digest(&std::fs::read(path).unwrap())), sum, "{name}");
    }
}

#[cfg(feature = "sign")]
#[test]
fn cardano_cli_key_gen_kes() {
    let depth5 = CardanoSumKes::new(5).unwrap();
    let rows = cardano("keygen.tsv");
    assert_eq!(rows.len(), 3);
    for r in rows {
        let key = depth5.generate(&bytes(&r[1])).unwrap();
        assert_eq!(hex::encode(&depth5.verifying_key(&key)), r[2], "{}", r[0]);
        let vk = HashAlgorithm::Blake2b256.digest(&[bytes(&r[3]), bytes(&r[2])].concat());
        assert_eq!(hex::encode(&vk), r[4], "{}", r[0]);
    }
}

fn flip(b: &mut [u8], at: &str, bit: &str) {
    let (at, bit): (usize, u32) = (at.parse().unwrap(), bit.parse().unwrap());
    b[at] ^= 1 << bit;
}

/// Rebuilds a mutated input from its case name, as the corpus README describes.
fn mutate(original: &[String], recipe: &str) -> (Vec<u8>, u64, Vec<u8>, Vec<u8>) {
    let (mut root, mut period, mut msg, mut sig) =
        (bytes(&original[1]), original[2].parse::<u64>().unwrap(), bytes(&original[3]), bytes(&original[4]));
    let parts: Vec<&str> = recipe.split('/').collect();
    match parts.as_slice() {
        ["period", p] => period = p.parse().unwrap(),
        ["root", "xor", o, b] => flip(&mut root, o, b),
        ["signature", "xor", o, b] => flip(&mut sig, o, b),
        ["message", "xor", o, b] => flip(&mut msg, o, b),
        ["signature", "swap-pair", d] => {
            let off = 64 + 64 * (d.parse::<usize>().unwrap() - 1);
            let (a, b) = (sig[off..off + 32].to_vec(), sig[off + 32..off + 64].to_vec());
            sig[off..off + 32].copy_from_slice(&b);
            sig[off + 32..off + 64].copy_from_slice(&a);
        }
        ["message", "empty"] => msg.clear(),
        ["message", "truncate-last"] => {
            msg.pop();
        }
        ["message", "append-zero"] => msg.push(0),
        ["original"] => {}
        other => panic!("unknown recipe {other:?}"),
    }
    (root, period, msg, sig)
}

#[test]
fn cardano_sum6_observations() {
    let originals: std::collections::HashMap<String, Vec<String>> =
        cardano("originals.tsv").into_iter().map(|r| (r[0].clone(), r)).collect();
    let observations = cardano("observations.tsv");
    assert_eq!(observations.len(), 17_336);
    assert_eq!(observations.iter().filter(|o| o[2] == "VERIFIED").count(), 4);
    let mismatches: Vec<String> = observations
        .iter()
        .filter_map(|o| {
            let (root, period, msg, sig) = mutate(&originals[&o[0]], &o[1]);
            let verdict =
                if CardanoSumKes::SUM6.verify(&root, period, &msg, &sig) == Ok(true) { "VERIFIED" } else { "REJECTED" };
            (verdict != o[2]).then(|| format!("{} {}: expected {}, got {verdict}", o[0], o[1], o[2]))
        })
        .collect();
    assert!(
        mismatches.is_empty(),
        "{} mismatches:\n{}",
        mismatches.len(),
        mismatches.iter().take(10).cloned().collect::<Vec<_>>().join("\n")
    );
}

#[cfg(feature = "sign")]
#[test]
fn cardano_keys_load_at_their_period_and_layouts_match() {
    let sum6 = CardanoSumKes::SUM6;
    let k0 = sum6.generate(&[1; 32]).unwrap();
    let k40 = sum6.evolve_to(&k0, 40).unwrap();
    let reloaded = sum6.load(&k40.to_bytes(), 40).unwrap();
    assert_eq!(k0.to_bytes().len(), 608);
    assert_eq!(sum6.signature_length(), 448);
    assert_eq!(sum6.sign(&k40, &[1]), sum6.sign(&reloaded, &[1]));
    assert_eq!(&k40.to_bytes()[512..544], &[0u8; 32]);
    assert!(!format!("{k0:?}").contains(&hex::encode(&k0.to_bytes()[..8])), "redacted");

    let k31 = sum6.evolve_to(&k0, 31).unwrap();
    let mut bad_r1 = k31.to_bytes().to_vec();
    bad_r1[512] ^= 1;
    assert!(sum6.load(&k0.to_bytes(), 32).is_err(), "period-0 key as period 32");
    assert!(sum6.load(&k0.to_bytes(), 1).is_err(), "period-0 key as period 1");
    assert!(sum6.load(&sum6.evolve(&k31).unwrap().to_bytes(), 31).is_err(), "period-32 key as period 31");
    assert!(matches!(sum6.load(&bad_r1, 31).and_then(|k| sum6.evolve(&k)), Err(KesError::InconsistentKey { .. })));
    assert_eq!(sum6.load(&k0.to_bytes(), 64).unwrap_err(), KesError::PeriodOutOfRange { period: 64, total: 64 });
    assert!(sum6.generate(&[0; 31]).is_err());
    assert!(CardanoSumKes::new(8).is_err());
}

#[test]
#[cfg(feature = "sign")]
fn a_zero_seed_leaf_loads_because_active_leaves_are_checked_against_their_key() {
    let kes = KesSum::new(HashAlgorithm::Blake2b256, 0).unwrap();
    let k = kes.generate(&[0; 32]).unwrap();
    let loaded = kes.load(&kes.key_bytes(&k), 0).unwrap();
    assert_eq!(*kes.key_bytes(&loaded), *kes.key_bytes(&k));
    let sig = kes.sign(&loaded, b"beef").unwrap();
    assert_eq!(kes.verify(&kes.verifying_key(&k), 0, b"beef", &sig), Ok(true));
}

#[test]
fn parameters_are_checked() {
    assert!(KesSum::new(HashAlgorithm::Blake2b256, 31).is_err());
    assert!(KesProduct::new(HashAlgorithm::Sha256, 20, 11).is_err());
    assert!(KesSum::new(HashAlgorithm::Sha3_512, 30).is_ok());
}
