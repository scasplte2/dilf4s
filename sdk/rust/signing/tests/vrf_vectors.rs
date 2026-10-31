//! `shared/vectors/vrf_v1.json` (every suite's keys, proofs and rejections) and the reference corpora in
//! `docs/test-vectors/vrf`: RFC 9381 B.3/B.4, draft-03 A.4, RFC 9380 (expand_message_xmd and ELL2), and the
//! libsodium (Cardano) draft-03 oracle corpus.

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

use common::bytes;
use dilf4s_core::hex;
use dilf4s_signing::VrfError;
#[cfg(feature = "sign")]
use dilf4s_signing::vrf;
use dilf4s_signing::vrf::{SuiteId, internals};

fn docs(path: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../docs/test-vectors").join(path)
}

fn json(path: &str) -> dilf4s_core::Value {
    dilf4s_core::json::parse(&std::fs::read_to_string(docs(path)).unwrap()).unwrap()
}

/// A text field of a JSON object.
fn text<'a>(v: &'a dilf4s_core::Value, key: &str) -> &'a str {
    match v {
        dilf4s_core::Value::Map(m) => match m.get(key) {
            Some(dilf4s_core::Value::Text(t)) => t,
            other => panic!("{key}: {other:?}"),
        },
        other => panic!("not an object: {other:?}"),
    }
}

fn field<'a>(v: &'a dilf4s_core::Value, key: &str) -> &'a dilf4s_core::Value {
    match v {
        dilf4s_core::Value::Map(m) => m.get(key).unwrap_or_else(|| panic!("{key}")),
        other => panic!("not an object: {other:?}"),
    }
}

fn list(v: &dilf4s_core::Value) -> &[dilf4s_core::Value] {
    match v {
        dilf4s_core::Value::Array(a) => a.items(),
        other => panic!("not an array: {other:?}"),
    }
}

fn arr<const N: usize>(hex_text: &str) -> [u8; N] {
    bytes(hex_text).try_into().unwrap()
}

#[cfg(feature = "sign")]
fn prove(suite: SuiteId, seed: &[u8], alpha: &[u8]) -> (Vec<u8>, Vec<u8>, [u8; 80], [u8; 64]) {
    fn run<S: vrf::Suite>(seed: &[u8], alpha: &[u8]) -> (Vec<u8>, Vec<u8>, [u8; 80], [u8; 64]) {
        let (sk, vk) = vrf::from_seed::<S>(seed).unwrap();
        let (pi, beta) = sk.prove(alpha).unwrap();
        (sk.to_bytes().to_vec(), vk.as_bytes().to_vec(), *pi.as_bytes(), *beta.as_bytes())
    }
    match suite {
        SuiteId::Tai => run::<vrf::Tai>(seed, alpha),
        SuiteId::Ell2 => run::<vrf::Ell2>(seed, alpha),
        SuiteId::Draft03 => run::<vrf::Draft03>(seed, alpha),
    }
}

#[test]
fn vrf_v1_keys_proofs_and_outputs() {
    let v: dilf4s_core::generated::vrf_vectors::VrfVectors = common::load("vrf_v1.json");
    let mut count = 0;
    for suite in &v.suites {
        let id = SuiteId::from_name(&suite.suite).unwrap_or_else(|| panic!("unknown suite {}", suite.suite));
        for key in &suite.keys {
            for p in &key.proofs {
                let beta = id.verify_bytes(&bytes(&key.verifying_key), &bytes(&p.alpha), &bytes(&p.pi));
                assert_eq!(beta.map(|b| hex::encode(&b)), Ok(p.beta.clone()), "{} verify", suite.suite);
                #[cfg(feature = "sign")]
                {
                    let (sk, vk, pi, beta) = prove(id, &bytes(&key.seed), &bytes(&p.alpha));
                    assert_eq!(hex::encode(&sk), key.signing_key, "{} signing key", suite.suite);
                    assert_eq!(hex::encode(&vk), key.verifying_key, "{} verifying key", suite.suite);
                    assert_eq!(hex::encode(&pi), p.pi, "{} proof", suite.suite);
                    assert_eq!(hex::encode(&beta), p.beta, "{} output", suite.suite);
                }
                count += 1;
            }
        }
    }
    assert_eq!(count, 27, "3 suites x 3 keys x 3 messages");
}

#[test]
fn vrf_v1_rejections() {
    let v: dilf4s_core::generated::vrf_vectors::VrfVectors = common::load("vrf_v1.json");
    let failures: Vec<String> = v
        .rejections
        .iter()
        .filter_map(|r| {
            let id = SuiteId::from_name(&r.suite)?;
            let actual = match id.verify_bytes(&bytes(&r.verifying_key), &bytes(&r.alpha), &bytes(&r.pi)) {
                Ok(_) => "valid",
                Err(e) => e.kind(),
            };
            (actual != r.result).then(|| format!("{} {}: expected {}, got {actual}", r.suite, r.name, r.result))
        })
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n"));
    assert!(v.rejections.len() > 10, "{} rejection cases", v.rejections.len());
    assert!(v.rejections.iter().all(|r| SuiteId::from_name(&r.suite).is_some()));
}

/// RFC 9381 B.3 (TAI) / B.4 (ELL2) and draft-03 A.4: keys, proofs, outputs, and the intermediate `H`.
fn examples(suite: SuiteId, cases: &[dilf4s_core::Value]) {
    assert_eq!(cases.len(), 3);
    for c in cases {
        let (pk, alpha, pi) = (bytes(text(c, "PK")), bytes(text(c, "alpha")), bytes(text(c, "pi")));
        let beta = suite.verify_bytes(&pk, &alpha, &pi).unwrap();
        assert_eq!(hex::encode(&beta), text(c, "beta"));
        let mut longer = alpha.clone();
        longer.push(0);
        assert_eq!(suite.verify_bytes(&pk, &longer, &pi), Err(VrfError::ChallengeMismatch));
        let mut tampered = pi.clone();
        tampered[40] ^= 1;
        assert!(suite.verify_bytes(&pk, &alpha, &tampered).is_err());
        let pk32: [u8; 32] = pk.clone().try_into().unwrap();
        match suite {
            SuiteId::Ell2 => assert_eq!(hex::encode(&internals::hash_to_curve_ell2(&pk32, &alpha)), text(c, "H")),
            SuiteId::Draft03 => assert_eq!(hex::encode(&internals::hash_to_curve_draft03(&pk32, &alpha)), text(c, "H")),
            SuiteId::Tai => {}
        }
        #[cfg(feature = "sign")]
        {
            let (sk, vk, proof, out) = prove(suite, &bytes(text(c, "SK")), &alpha);
            assert_eq!(hex::encode(&vk), text(c, "PK"));
            assert_eq!(hex::encode(&proof), text(c, "pi"));
            assert_eq!(hex::encode(&out), text(c, "beta"));
            if suite == SuiteId::Draft03 {
                assert_eq!(hex::encode(&sk), format!("{}{}", text(c, "SK"), text(c, "PK")));
            }
        }
    }
}

#[test]
fn rfc9381_and_draft03_examples() {
    let rfc = json("vrf/rfc9381.json");
    examples(SuiteId::Tai, list(field(&rfc, "tai")));
    examples(SuiteId::Ell2, list(field(&rfc, "ell2")));
    examples(SuiteId::Draft03, list(field(&json("vrf/draft03.json"), "examples")));
}

#[test]
fn rfc9380_ell2_encode_to_curve() {
    let doc = json("vrf/rfc9380.json");
    let section = field(&doc, "edwards25519_XMD_SHA512_ELL2_NU");
    let dst = text(field(section, "header"), "dst").as_bytes().to_vec();
    let vectors = list(field(section, "vectors"));
    assert_eq!(vectors.len(), 5);
    assert!(internals::encode_to_curve_ell2(b"", b"").is_none(), "an empty DST is refused, not a panic");
    assert!(internals::encode_to_curve_ell2(b"", &[0; 256]).is_none(), "an oversize DST is refused");
    for v in vectors {
        // the affine (x, y) as an RFC 8032 encoding: little-endian y with the sign of x in bit 255
        let x = bytes(text(v, "P.x"));
        let mut y = bytes(text(v, "P.y"));
        y.reverse();
        y[31] |= (x[31] & 1) << 7;
        assert_eq!(
            internals::encode_to_curve_ell2(text(v, "msg").as_bytes(), &dst).map(|p| hex::encode(&p)),
            Some(hex::encode(&y))
        );
    }
}

#[test]
fn rfc9380_expand_message_xmd() {
    use bls12_381::hash_to_curve::{ExpandMessage, ExpandMsgXmd};
    use sha2::digest::consts::U32;
    let doc = json("vrf/rfc9380.json");
    for (section, sha512) in [("expand_message_xmd_sha256", false), ("expand_message_xmd_sha512", true)] {
        let s = field(&doc, section);
        let dst = text(field(s, "header"), "DST").as_bytes().to_vec();
        let vectors = list(field(s, "vectors"));
        assert_eq!(vectors.len(), 10);
        for v in vectors {
            let msg = text(v, "msg").as_bytes().to_vec();
            let len = usize::from_str_radix(text(v, "len_in_bytes").trim_start_matches("0x"), 16).unwrap();
            let out = if sha512 {
                ExpandMsgXmd::<sha2::Sha512>::init_expand::<_, U32>([&msg], &dst, len).into_vec()
            } else {
                ExpandMsgXmd::<sha2::Sha256>::init_expand::<_, U32>([&msg], &dst, len).into_vec()
            };
            assert_eq!(hex::encode(&out), text(v, "uniform_bytes"), "{section}");
        }
    }
}

// ------------------------------------------------------------------ the libsodium (Cardano) oracle corpus

fn corpus(name: &str) -> Vec<Vec<String>> {
    std::fs::read_to_string(docs("vrf/draft03-corpus").join(name))
        .unwrap()
        .lines()
        .map(|l| l.split('\t').map(String::from).collect())
        .collect()
}

#[test]
fn draft03_corpus_files_match_their_sums() {
    let sums = std::fs::read_to_string(docs("vrf/draft03-corpus/SHA256SUMS")).unwrap();
    let mut n = 0;
    for line in sums.lines() {
        let (sum, name) = line.split_once("  ").unwrap();
        let data = std::fs::read(docs("vrf/draft03-corpus").join(name)).unwrap();
        let digest = dilf4s_core::hash::HashAlgorithm::Sha256.digest(&data);
        assert_eq!(hex::encode(&digest), sum, "{name}");
        n += 1;
    }
    assert_eq!(n, 6);
}

/// `NULL` cells are the oracle's null-pointer envelopes; a typed API cannot receive null, so they are malformed.
fn outcome(row: &[String]) -> String {
    if row.iter().any(|c| c == "NULL") {
        return "MALFORMED".into();
    }
    match SuiteId::Draft03.verify_bytes(&bytes(&row[1]), &bytes(&row[3]), &bytes(&row[2])) {
        Ok(beta) => format!("VALID:{}", hex::encode(&beta)),
        Err(e) if e.is_malformed() => "MALFORMED".into(),
        Err(_) => "INVALID".into(),
    }
}

fn replay(rows_file: &str, expected_file: &str, count: usize, valid: usize, malformed: usize) {
    let (rows, expected) = (corpus(rows_file), corpus(expected_file));
    assert_eq!(rows.len(), count);
    assert_eq!(expected.len(), count);
    assert_eq!(expected.iter().filter(|e| e[1].starts_with("VALID:")).count(), valid);
    assert_eq!(expected.iter().filter(|e| e[1] == "MALFORMED").count(), malformed);
    let mismatches: Vec<String> = rows
        .iter()
        .zip(&expected)
        .filter_map(|(r, e)| {
            assert_eq!(r[0], e[0]);
            let actual = outcome(r);
            (actual != e[1]).then(|| format!("{}: expected {}, got {actual}", r[0], e[1]))
        })
        .collect();
    assert!(mismatches.is_empty(), "{} mismatches:\n{}", mismatches.len(), mismatches.join("\n"));
}

#[test]
fn draft03_oracle_corpus() {
    replay("vectors.tsv", "expected.tsv", 2048, 3, 27);
}

#[test]
fn draft03_additional_corpus() {
    replay("additional.tsv", "additional-results.tsv", 2338, 3, 0);
}

fn fixed<const N: usize>(text: &str) -> Option<[u8; N]> {
    dilf4s_core::hex::decode(text).ok()?.try_into().ok()
}

/// The four helper kinds, in the oracle helper's output formats.
fn helper(row: &[String]) -> String {
    let invalid = || String::from("INVALID");
    match row[1].as_str() {
        "d" => fixed::<32>(&row[2]).and_then(|b| internals::decode_draft03(&b)).map_or_else(invalid, |(enc, small)| {
            format!("{}{}", if small { "SMALL:" } else { "POINT:" }, hex::encode(&enc))
        }),
        "u" => fixed::<32>(&row[2]).map_or_else(invalid, |b| hex::encode(&internals::from_uniform_draft03(&b))),
        "e" => {
            let c = bytes(&row[4]);
            let mut c16 = [0u8; 16];
            c16[..c.len().min(16)].copy_from_slice(&c[..c.len().min(16)]);
            match (fixed::<32>(&row[2]), fixed::<32>(&row[3]), fixed::<32>(&row[5])) {
                (Some(p), Some(q), Some(s)) if c.len() <= 16 => {
                    internals::equation_draft03(&p, &q, &c16, &s).map_or_else(invalid, |e| hex::encode(&e))
                }
                _ => invalid(),
            }
        }
        _ => match (fixed::<32>(&row[2]), fixed::<80>(&row[3])) {
            (Some(pk), Some(pi)) => {
                internals::pipeline_draft03(&pk, &pi, &bytes(&row[4])).map_or_else(invalid, |b| hex::encode(&b))
            }
            _ => invalid(),
        },
    }
}

#[test]
fn draft03_helper_rows() {
    let (rows, expected) = (corpus("helpers.tsv"), corpus("helper-results.tsv"));
    assert_eq!(rows.len(), 3426);
    for (op, count) in [("d", 612), ("u", 519), ("e", 240), ("t", 2055)] {
        let selected: Vec<_> = rows.iter().zip(&expected).filter(|(r, _)| r[1] == op).collect();
        assert_eq!(selected.len(), count, "{op}");
        let mismatches: Vec<String> = selected
            .iter()
            .filter_map(|(r, e)| {
                let actual = helper(r);
                (actual != e[1]).then(|| format!("{}: expected {}, got {actual}", r[0], e[1]))
            })
            .collect();
        assert!(mismatches.is_empty(), "{op}: {} mismatches:\n{}", mismatches.len(), mismatches.join("\n"));
    }
}

#[test]
fn malformed_envelopes_and_typed_rejections() {
    let key = bytes("d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a");
    let pi = [0u8; 80];
    assert!(SuiteId::Tai.verify_bytes(&key[1..], &[], &pi).unwrap_err().is_malformed(), "31-byte key");
    assert!(SuiteId::Tai.verify_bytes(&key, &[], &pi[1..]).unwrap_err().is_malformed(), "79-byte proof");
    let identity = arr::<32>("0100000000000000000000000000000000000000000000000000000000000000");
    let neg_zero = arr::<32>("0100000000000000000000000000000000000000000000000000000000000080");
    let non_canonical = arr::<32>("edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f");
    for suite in SuiteId::ALL {
        assert_eq!(suite.verify_bytes(&identity, &[], &pi), Err(VrfError::SmallOrderPublicKey), "{suite:?}");
        assert_eq!(suite.verify_bytes(&non_canonical, &[], &pi), Err(VrfError::InvalidPublicKey), "{suite:?}");
    }
    // RFC 9381 decoding rejects negative zero; draft-03 decodes it to the identity, which is small order
    assert_eq!(SuiteId::Tai.verify_bytes(&neg_zero, &[], &pi), Err(VrfError::InvalidPublicKey));
    assert_eq!(SuiteId::Ell2.verify_bytes(&neg_zero, &[], &pi), Err(VrfError::InvalidPublicKey));
    assert_eq!(SuiteId::Draft03.verify_bytes(&neg_zero, &[], &pi), Err(VrfError::SmallOrderPublicKey));
    // s = L
    let mut high_s = [0u8; 80];
    high_s[..32].copy_from_slice(&key);
    high_s[48..].copy_from_slice(&bytes("edd3f55c1a631258d69cf7a2def9de1400000000000000000000000000000010"));
    assert_eq!(SuiteId::Tai.verify_bytes(&key, &[], &high_s), Err(VrfError::NonCanonicalScalar));
}

#[cfg(feature = "sign")]
#[test]
fn suites_sharing_a_suite_byte_reject_each_other() {
    let (sk, vk) = vrf::from_seed::<vrf::Ell2>(&[5; 32]).unwrap();
    let (pi, _) = sk.prove(&[1]).unwrap();
    assert!(SuiteId::Draft03.verify_bytes(vk.as_bytes(), &[1], pi.as_bytes()).is_err());
    assert!(vrf::verify::<vrf::Ell2>(&vk, &[1], &pi).is_ok());
    let mismatched: Vec<u8> = [[1u8; 32], [2u8; 32]].concat();
    assert_eq!(vrf::SigningKey::<vrf::Draft03>::from_bytes(&mismatched).unwrap_err(), VrfError::KeyMismatch);
    assert!(!format!("{sk:?}").contains("0505"), "secret keys are redacted");
}
