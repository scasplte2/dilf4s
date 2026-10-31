//! Every case of `shared/vectors/canonical_encoding_v1.json`, and the value commitments of `merkle_v1.json`.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    reason = "tests fail by panicking, and run with std"
)]

mod common;

use common::{bytes, load};
use dilf4s_core::codec::Codec;
use dilf4s_core::commitment::value_commitment;
use dilf4s_core::generated::canonical_vectors::{CanonicalJsonCase, CanonicalVectors};
use dilf4s_core::generated::merkle_vectors::MerkleVectors;
use dilf4s_core::generated::versioned_trie::STATE_KEY;
use dilf4s_core::generated::versioned_trie_vectors::VersionedTrieVectors;
use dilf4s_core::hash::{DomainTag, HashAlgorithm};
use dilf4s_core::{cbor, hex, json};

/// Checks every case, collecting failures so one run reports all of them; returns the number of cases.
fn run<T>(cases: &[T], check: impl Fn(&T) -> Result<(), String>) -> usize {
    let failures: Vec<String> = cases.iter().filter_map(|c| check(c).err()).collect();
    assert!(failures.is_empty(), "{} of {} cases failed:\n{}", failures.len(), cases.len(), failures.join("\n"));
    cases.len()
}

fn same(what: &str, case: &str, expected: &str, actual: &str) -> Result<(), String> {
    if expected == actual { Ok(()) } else { Err(format!("{case}: {what} expected {expected}, got {actual}")) }
}

fn vectors() -> (CanonicalVectors, DomainTag, HashAlgorithm) {
    let v: CanonicalVectors = load("canonical_encoding_v1.json");
    let tag = DomainTag::new(&v.content_hash_tag).expect("content hash tag");
    let algorithm = HashAlgorithm::from_name(&v.content_hash_algorithm).expect("content hash algorithm");
    (v, tag, algorithm)
}

#[test]
fn json_cases() {
    let (v, tag, algorithm) = vectors();
    let n = run(&v.json, |case| match case {
        CanonicalJsonCase::CanonicalJsonAccepted(c) => {
            let value = json::parse(&c.json).map_err(|e| format!("{}: rejected with {e}", c.json))?;
            same("cbor", &c.json, &c.cbor, &hex::encode(&cbor::encode(&value)))?;
            same("content hash", &c.json, &c.content_hash, &hex::encode(&algorithm.content_hash(&tag, &value)))?;
            same("render", &c.json, &c.render, &json::render(&value).map_err(|e| e.to_string())?)?;
            let decoded = cbor::decode(&bytes(&c.cbor)).map_err(|e| format!("{}: cbor rejected with {e}", c.json))?;
            same("decode then encode", &c.json, &c.cbor, &hex::encode(&cbor::encode(&decoded)))
        }
        CanonicalJsonCase::CanonicalJsonRejected(c) => {
            let kind = json::parse(&c.json).map_or_else(|e| e.kind(), |_| "accepted");
            same("error kind", &c.json, &c.error, kind)
        }
    });
    eprintln!("json: {n} cases");
}

#[test]
fn model_cases() {
    let (v, tag, algorithm) = vectors();
    let n = run(&v.model, |c| {
        let value = cbor::decode(&bytes(&c.cbor)).map_err(|e| format!("{}: {e}", c.value))?;
        same("cbor", &c.value, &c.cbor, &hex::encode(&cbor::encode(&value)))?;
        same("content hash", &c.value, &c.content_hash, &hex::encode(&algorithm.content_hash(&tag, &value)))
    });
    eprintln!("model: {n} cases");
}

#[test]
fn cbor_rejections() {
    let (v, _, _) = vectors();
    let n = run(&v.cbor_rejections, |c| {
        let kind = cbor::decode(&bytes(&c.cbor)).map_or_else(|e| e.kind(), |_| "accepted");
        same("error kind", &c.cbor, &c.error, kind)
    });
    eprintln!("cborRejections: {n} cases");
}

#[test]
fn tagged_hashes() {
    let (v, _, _) = vectors();
    let n = run(&v.tagged_hash, |c| {
        let algorithm = HashAlgorithm::from_name(&c.algorithm).map_err(|e| e.to_string())?;
        let tag = DomainTag::new(&c.tag).map_err(|e| e.to_string())?;
        same(&c.algorithm, &c.message, &c.digest, &hex::encode(&algorithm.tagged_hash(&tag, &bytes(&c.message))))
    });
    eprintln!("taggedHash: {n} cases");
}

#[test]
fn digests() {
    let (v, _, _) = vectors();
    let n = run(&v.digests, |c| {
        let algorithm = HashAlgorithm::from_name(&c.algorithm).map_err(|e| e.to_string())?;
        same(&c.algorithm, &c.message, &c.digest, &hex::encode(&algorithm.digest(&bytes(&c.message))))
    });
    eprintln!("digests: {n} cases");
}

#[test]
fn domain_tags() {
    let (v, _, _) = vectors();
    let n = run(&v.domain_tags, |c| {
        let valid = DomainTag::new(&c.tag).is_ok();
        if valid == c.valid { Ok(()) } else { Err(format!("{:?}: expected valid = {}, got {valid}", c.tag, c.valid)) }
    });
    eprintln!("domainTags: {n} cases");
}

#[test]
fn state_keys() {
    let vt: VersionedTrieVectors = load("versioned_trie_v1.json");
    let n = run(&vt.state_keys, |c| {
        let decoded = STATE_KEY.decode(&dilf4s_core::Value::text(c.key.clone())).is_ok();
        let encoded = STATE_KEY.encode(&c.key).is_ok();
        if decoded == c.schema && encoded == c.schema {
            Ok(())
        } else {
            Err(format!("{:?}: expected schema = {}, decoded {decoded}, encoded {encoded}", c.key, c.schema))
        }
    });
    eprintln!("stateKeys: {n} cases");
}

#[test]
fn value_commitments() {
    let m: MerkleVectors = load("merkle_v1.json");
    let leaf = DomainTag::new("dilf4s/merkle/leaf/v1").expect("leaf tag");
    let n = run(&m.value_commitments, |c| {
        let value = cbor::decode(&bytes(&c.cbor)).map_err(|e| format!("{}: {e}", c.cbor))?;
        let commitment = value_commitment(&value);
        same("commitment", &c.cbor, &c.commitment, &hex::encode(&commitment))?;
        same("leaf hash", &c.cbor, &c.leaf_hash, &hex::encode(&HashAlgorithm::Sha256.tagged_hash(&leaf, &commitment)))
    });
    eprintln!("valueCommitments: {n} cases");
}
