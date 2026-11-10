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
use dilf4s_core::commitment::{value_commitment, value_commitment_into};
use dilf4s_core::generated::canonical_vectors::{
    CanonicalErrorKind, CanonicalJsonCase, CanonicalLimitEncoding, CanonicalLimitKind, CanonicalLimitOutcome,
    CanonicalLimitRefusal, CanonicalVectors, HashOutcome,
};
use dilf4s_core::generated::merkle_vectors::MerkleVectors;
use dilf4s_core::generated::versioned_trie::STATE_KEY;
use dilf4s_core::generated::versioned_trie_vectors::{VersionedTrieVectors, VtSchemaOutcome};
use dilf4s_core::hash::{
    Digest, DomainTag, ExpansionRefusal, HashAlgorithm, HashError, HashErrorKind, Hasher, Sha256, TaggedHash,
};
use dilf4s_core::{CanonicalError, cbor, hex, json};

/// A refusal's normative limit (conformance.md section 3): `RationalTooWide` and `IntegerTooWide` carry one.
const fn normative_limit(e: &CanonicalError) -> Option<u64> {
    match e {
        CanonicalError::RationalTooWide { limit } | CanonicalError::IntegerTooWide { limit } => Some(*limit),
        _ => None,
    }
}

/// A refusal as a row records it: its kind and its normative limit.
fn refused(
    case: &str,
    kind: CanonicalErrorKind,
    limit: Option<u64>,
    actual: Option<&CanonicalError>,
) -> Result<(), String> {
    let actual = actual.map(|e| (e.kind(), normative_limit(e)));
    if actual.as_ref().is_some_and(|(k, l)| *k == kind && *l == limit) {
        Ok(())
    } else {
        Err(format!("{case}: expected {kind:?} (limit {limit:?}), got {actual:?}"))
    }
}

/// Checks every case, collecting failures so one run reports all of them; returns the number of cases.
fn run<T>(cases: &[T], check: impl Fn(&T) -> Result<(), String>) -> usize {
    let failures: Vec<String> = cases.iter().filter_map(|c| check(c).err()).collect();
    assert!(failures.is_empty(), "{} of {} cases failed:\n{}", failures.len(), cases.len(), failures.join("\n"));
    cases.len()
}

/// The content hash the old way, as the oracle of the streamed one: the whole encoding built, then hashed.
fn buffered(algorithm: HashAlgorithm, tag: &DomainTag, value: &dilf4s_core::Value) -> Vec<u8> {
    algorithm.tagged_hash(tag, &cbor::encode(value))
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
            same("buffered content hash", &c.json, &c.content_hash, &hex::encode(&buffered(algorithm, &tag, &value)))?;
            same("render", &c.json, &c.render, &json::render_within(&value, usize::MAX).map_err(|e| e.to_string())?)?;
            let decoded = cbor::decode(&bytes(&c.cbor)).map_err(|e| format!("{}: cbor rejected with {e}", c.json))?;
            same("decode then encode", &c.json, &c.cbor, &hex::encode(&cbor::encode(&decoded)))
        }
        CanonicalJsonCase::CanonicalJsonRejected(c) => {
            refused(&c.json, c.error, c.limit, json::parse(&c.json).err().as_ref())
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
        same("content hash", &c.value, &c.content_hash, &hex::encode(&algorithm.content_hash(&tag, &value)))?;
        same("buffered content hash", &c.value, &c.content_hash, &hex::encode(&buffered(algorithm, &tag, &value)))
    });
    eprintln!("model: {n} cases");
}

#[test]
fn render_rejections() {
    let (v, _, _) = vectors();
    let n = run(&v.render_rejections, |c| {
        let value = cbor::decode(&bytes(&c.cbor)).map_err(|e| format!("{}: {e}", c.cbor))?;
        match json::render_within(&value, usize::MAX) {
            Err(json::RenderError::Canonical(e)) => refused(&c.cbor, c.error, None, Some(&e)),
            other => Err(format!("{}: expected {:?}, got {other:?}", c.cbor, c.error)),
        }
    });
    eprintln!("renderRejections: {n} cases");
}

/// The budgeted calls (canonical-encoding.md sections 2 and 3): `encode_within` or `render_within` of each value at
/// each row's limit gives the row's outcome, by kind and limit, and the row's output exactly when it is accepted.
/// CBOR length measurement must give the exact same refusal or the accepted output's length.
#[test]
fn limits() {
    let (v, _, _) = vectors();
    assert_eq!(
        v.limits.iter().filter(|c| c.encoding == CanonicalLimitEncoding::Cbor).count(),
        9,
        "CBOR measurement rows"
    );
    let n = run(&v.limits, |c| {
        let value = cbor::decode(&bytes(&c.value)).map_err(|e| format!("{}: {e}", c.name))?;
        let max = usize::try_from(c.max_bytes).map_err(|e| format!("{}: {e}", c.name))?;
        let too_large = |limit: usize| CanonicalLimitRefusal {
            kind: CanonicalLimitKind::TooLarge,
            limit: Some(u64::try_from(limit).unwrap_or(u64::MAX)),
        };
        let (outcome, output) = match c.encoding {
            CanonicalLimitEncoding::Cbor => {
                let encoded = cbor::encode_within(&value, max);
                let measured = cbor::encoded_len_within(&value, max);
                let expected = encoded.as_ref().map(Vec::len).map_err(|e| *e);
                if measured != expected {
                    return Err(format!("{}: measured length expected {expected:?}, got {measured:?}", c.name));
                }
                match encoded {
                    Ok(b) => (CanonicalLimitOutcome::Accepted, Some(hex::encode(&b))),
                    Err(e) => (CanonicalLimitOutcome::CanonicalLimitRefusal(too_large(e.limit)), None),
                }
            }
            CanonicalLimitEncoding::Json => match json::render_within(&value, max) {
                Ok(text) => (CanonicalLimitOutcome::Accepted, Some(text)),
                Err(json::RenderError::TooLarge(e)) => {
                    (CanonicalLimitOutcome::CanonicalLimitRefusal(too_large(e.limit)), None)
                }
                Err(json::RenderError::Canonical(e)) => {
                    let kind = CanonicalLimitKind::CanonicalErrorKind(e.kind());
                    (CanonicalLimitOutcome::CanonicalLimitRefusal(CanonicalLimitRefusal { kind, limit: None }), None)
                }
            },
        };
        if outcome == c.outcome && output == c.output {
            Ok(())
        } else {
            Err(format!("{}: expected {:?} {:?}, got {outcome:?} {output:?}", c.name, c.outcome, c.output))
        }
    });
    assert_eq!(n, 19, "limits cases");
    eprintln!("limits: {n} cases");
}

#[test]
fn cbor_rejections() {
    let (v, _, _) = vectors();
    let n =
        run(&v.cbor_rejections, |c| refused(&c.cbor, c.error, c.limit, cbor::decode(&bytes(&c.cbor)).err().as_ref()));
    eprintln!("cborRejections: {n} cases");
}

/// The coverage gate (conformance.md section 6) for `CanonicalError`: every kind of the schema's `canonical-error-kind`
/// has a row. The SDK's error is generated from the same schema (`codec-errors.cddl`), so its kinds are these.
#[test]
fn every_canonical_error_kind_has_a_row() {
    let (v, _, _) = vectors();
    let vt: VersionedTrieVectors = load("versioned_trie_v1.json");
    let json = v.json.iter().filter_map(|c| match c {
        CanonicalJsonCase::CanonicalJsonRejected(r) => Some(r.error),
        CanonicalJsonCase::CanonicalJsonAccepted(_) => None,
    });
    let typed = vt.state_keys.iter().filter_map(|c| match c.schema {
        VtSchemaOutcome::TypeMismatch => Some(CanonicalErrorKind::TypeMismatch),
        VtSchemaOutcome::Accepted => None,
    });
    let pinned: Vec<CanonicalErrorKind> = json
        .chain(v.render_rejections.iter().map(|c| c.error))
        .chain(v.cbor_rejections.iter().map(|c| c.error))
        .chain(typed)
        .collect();
    for kind in CanonicalErrorKind::ALL {
        assert!(pinned.contains(&kind), "no row pins {kind:?}");
    }
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

/// A hash call's outcome as a row records it: "accepted", or the refusal's kind (total: every kind of the generated
/// family is a kind of the schema's), with its normative `offset` (`InvalidHex`) or `refusal` (`InvalidExpansion`).
fn hash_outcome<T>(r: &Result<T, HashError>) -> (HashOutcome, Option<u64>, Option<ExpansionRefusal>) {
    match r {
        Ok(_) => (HashOutcome::Accepted, None, None),
        Err(e) => (
            HashOutcome::HashErrorKind(e.kind()),
            match e {
                HashError::InvalidHex { offset } => Some(*offset),
                _ => None,
            },
            match e {
                HashError::InvalidExpansion { refusal, .. } => Some(*refusal),
                _ => None,
            },
        ),
    }
}

fn same_outcome<T>(
    case: &str,
    expected: (&HashOutcome, Option<u64>, Option<&ExpansionRefusal>),
    actual: &Result<T, HashError>,
) -> Result<(), String> {
    let (kind, offset, refusal) = hash_outcome(actual);
    if (&kind, offset, refusal.as_ref()) == expected {
        Ok(())
    } else {
        Err(format!("{case}: expected {expected:?}, got {:?}", (kind, offset, refusal)))
    }
}

#[test]
fn domain_tags() {
    let (v, _, _) = vectors();
    let n = run(&v.domain_tags, |c| {
        same_outcome(&format!("{:?}", c.tag), (&c.outcome, None, None), &DomainTag::new(&c.tag))
    });
    eprintln!("domainTags: {n} cases");
}

#[test]
fn algorithms() {
    let (v, _, _) = vectors();
    let n = run(&v.algorithms, |c| {
        let r = HashAlgorithm::from_name(&c.name);
        same_outcome(&format!("{:?}", c.name), (&c.outcome, None, None), &r)?;
        r.map_or(Ok(()), |a| same("name", &c.name, &c.name, a.name()))
    });
    eprintln!("algorithms: {n} cases");
}

#[test]
fn digest_hex() {
    let (v, _, _) = vectors();
    let n = run(&v.digest_hex, |c| {
        let algorithm = HashAlgorithm::from_name(&c.algorithm).map_err(|e| e.to_string())?;
        let case = format!("{} {:?}", c.algorithm, c.hex);
        let r = algorithm.digest_from_hex(&c.hex);
        same_outcome(&case, (&c.outcome, c.offset, None), &r)?;
        r.map_or(Ok(()), |d| {
            let lower = c.hex.strip_prefix("0x").unwrap_or(&c.hex).to_ascii_lowercase();
            same("bytes", &case, &lower, &hex::encode(&d))
        })
    });
    eprintln!("digestHex: {n} cases");
}

/// The typed form agrees with the run-time one on every row of its algorithm.
#[test]
fn typed_digest_hex() {
    let (v, _, _) = vectors();
    let rows: Vec<_> = v.digest_hex.iter().filter(|c| c.algorithm == Sha256::NAME).collect();
    let n = run(&rows, |c| {
        let typed = Digest::<Sha256>::from_hex(&c.hex).map(|d| d.as_bytes().to_vec());
        let dynamic = HashAlgorithm::Sha256.digest_from_hex(&c.hex);
        if typed == dynamic { Ok(()) } else { Err(format!("{:?}: {typed:?} and {dynamic:?}", c.hex)) }
    });
    assert_eq!(n, 33, "SHA-256 rows");
}

#[test]
fn expansions() {
    let (v, _, _) = vectors();
    let n = run(&v.expansions, |c| {
        let algorithm = HashAlgorithm::from_name(&c.algorithm).map_err(|e| e.to_string())?;
        let case = format!("{} dst {} length {}", c.algorithm, c.dst, c.length);
        // a length past usize is past every limit, so it is refused as such
        let length = usize::try_from(c.length).unwrap_or(usize::MAX);
        let r = algorithm.expand_message_xmd(&[&bytes(&c.message)], &bytes(&c.dst), length);
        same_outcome(&case, (&c.outcome, None, c.refusal.as_ref()), &r)?;
        match (&r, &c.output) {
            (Ok(out), Some(expected)) => same("output", &case, expected, &hex::encode(out)),
            (Err(_), None) => Ok(()),
            _ => Err(format!("{case}: an output exactly when accepted")),
        }
    });
    eprintln!("expansions: {n} cases");
}

/// The coverage gate (conformance.md section 6) for the hash family: every kind of the generated `HashErrorKind` has a
/// row, in whichever section reaches it, and the expansion refusals each have one.
#[test]
fn every_hash_error_kind_has_a_row() {
    let (v, _, _) = vectors();
    let pinned: Vec<&HashOutcome> = v
        .domain_tags
        .iter()
        .map(|c| &c.outcome)
        .chain(v.algorithms.iter().map(|c| &c.outcome))
        .chain(v.digest_hex.iter().map(|c| &c.outcome))
        .chain(v.expansions.iter().map(|c| &c.outcome))
        .collect();
    for kind in HashErrorKind::ALL {
        assert!(pinned.contains(&&HashOutcome::HashErrorKind(kind)), "no row pins {kind:?}");
    }
    for refusal in ExpansionRefusal::ALL {
        assert!(v.expansions.iter().any(|c| c.refusal.as_ref() == Some(&refusal)), "no row pins {refusal:?}");
    }
}

#[test]
fn state_keys() {
    let vt: VersionedTrieVectors = load("versioned_trie_v1.json");
    let n = run(&vt.state_keys, |c| {
        let decoded = STATE_KEY.decode(&dilf4s_core::Value::text(c.key.clone())).is_ok();
        let encoded = match STATE_KEY.encode(&c.key) {
            Ok(_) => Some(VtSchemaOutcome::Accepted),
            Err(e) => match e.kind() {
                CanonicalErrorKind::TypeMismatch => Some(VtSchemaOutcome::TypeMismatch),
                _ => None,
            },
        };
        if decoded == (c.schema == VtSchemaOutcome::Accepted) && encoded.as_ref() == Some(&c.schema) {
            Ok(())
        } else {
            Err(format!("{:?}: expected schema {:?}, decoded {decoded}, encoded {encoded:?}", c.key, c.schema))
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
        same("leaf hash", &c.cbor, &c.leaf_hash, &hex::encode(&HashAlgorithm::Sha256.tagged_hash(&leaf, &commitment)))?;
        let mut streamed = TaggedHash::<Sha256>::new(leaf.clone()).start();
        value_commitment_into(&mut streamed, &value);
        same("streamed leaf hash", &c.cbor, &c.leaf_hash, &hex::encode(streamed.finish().as_bytes()))
    });
    eprintln!("valueCommitments: {n} cases");
}
