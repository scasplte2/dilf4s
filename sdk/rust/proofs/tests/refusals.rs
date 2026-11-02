//! `shared/vectors/refusals_v1.json` (docs/specs/conformance.md): every case run as section 6 of the spec defines its
//! check, and its outcome compared with the vector's: "accepted", or the refusal's kind and normative fields.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    reason = "tests fail by panicking, and run with std"
)]

#[path = "../../core/tests/common/mod.rs"]
mod common;

use common::{Step, build, bytes, load};
use dilf4s_core::generated::refusal_vectors::{
    BuiltStep, BuiltValue, RefusalCase, RefusalCheck, RefusalMode, RefusalOutcome, RefusalVectors,
};
use dilf4s_core::hash::{Digest, Sha256};
use dilf4s_core::value::Value;
use dilf4s_proofs::merkle::{self, ConsistencyProof, InclusionProof, MerkleError, Tagged};
use dilf4s_proofs::poseidon::merkle::{PoseidonMerkleError, PoseidonMerkleProof};
use dilf4s_proofs::trie::{KeyMode, Lookup, TrieError, TrieHashing, TrieProof};
use dilf4s_proofs::versioned::{Replica, StateKey, U64Key, VersionedTrie, VersionedTrieError, check_value};

type H = Sha256;
type Vt = VersionedTrie<U64Key, H>;

/// An outcome as the vectors record it: `None` for "accepted", else the kind and its normative fields.
#[derive(Debug, PartialEq, Eq)]
struct Refused {
    kind: String,
    limit: Option<u64>,
    depth: Option<u64>,
    component: Option<String>,
}

fn kind(kind: &str) -> Refused {
    Refused { kind: kind.into(), limit: None, depth: None, component: None }
}

fn versioned(e: &VersionedTrieError) -> Refused {
    match e {
        VersionedTrieError::TooLarge { limit, .. } => Refused { limit: Some(*limit), ..kind("TooLarge") },
        VersionedTrieError::TooDeep { depth, limit, .. } => Refused {
            limit: Some(u64::try_from(*limit).unwrap()),
            depth: Some(u64::try_from(*depth).unwrap()),
            ..kind("TooDeep")
        },
        VersionedTrieError::RootMismatch { component, .. } => {
            Refused { component: Some((*component).into()), ..kind("RootMismatch") }
        }
        VersionedTrieError::Trie(t) => kind(&format!("Trie.{}", t.kind())),
        other => kind(other.kind()),
    }
}

fn merkle_kind(e: &MerkleError) -> Refused {
    kind(e.kind())
}

fn trie_kind(e: &TrieError) -> Refused {
    kind(e.kind())
}

fn poseidon_kind(e: &PoseidonMerkleError) -> Refused {
    match e {
        PoseidonMerkleError::Position { depth, .. } => {
            Refused { depth: Some(u64::try_from(*depth).unwrap()), ..kind("Position") }
        }
        PoseidonMerkleError::Encoding { .. } => kind(e.kind()),
    }
}

fn built(b: &BuiltValue) -> Value {
    let steps: Vec<Step<'_>> = b
        .steps
        .iter()
        .flatten()
        .map(|s| match s {
            BuiltStep::BuiltWrap(w) => Step::Wrap { template: &w.template, times: w.times },
            BuiltStep::BuiltCopies(c) => Step::Copies(c.copies),
        })
        .collect();
    build(&b.leaf, &steps)
}

fn digest(hex: Option<&String>) -> Digest<H> {
    Digest::from_slice(&bytes(hex.expect("a digest"))).unwrap()
}

/// The case's outcome, by its check (conformance.md section 6): the first refusal, or `None`.
fn run(vt: &Vt, c: &RefusalCase) -> Option<Refused> {
    let input = || built(c.input.as_ref().expect("an input"));
    let state_key = || StateKey::parse(c.state_key.as_deref().expect("a state key")).unwrap();
    let root = || digest(c.root.as_ref());
    match c.check {
        RefusalCheck::VtValue => check_value(&state_key(), &input()).err().map(|e| versioned(&e)),
        RefusalCheck::VtChunk => {
            let n = usize::try_from(c.zeros.expect("zeros")).unwrap();
            vt.decode_chunk(&vec![0u8; n]).err().map(|e| versioned(&e))
        }
        RefusalCheck::VtDelta => vt.decode_delta(&input()).err().map(|e| versioned(&e)),
        RefusalCheck::VtProof => vt.decode_proof(&input()).err().map(|e| versioned(&e)),
        RefusalCheck::VtState => {
            let answer = vt.decode_proof(&input()).and_then(|p| {
                let root = root();
                vt.verify_state(&root, &p).and_then(|s| s.lookup(&state_key())).map(drop)
            });
            answer.err().map(|e| versioned(&e))
        }
        RefusalCheck::VtHistorical => {
            let answer = vt.decode_proof(&input()).and_then(|p| {
                let root = root();
                let at = c.at.expect("at");
                vt.verify_historical_key(&root, &p, &at).and_then(|s| s.lookup(&state_key())).map(drop)
            });
            answer.err().map(|e| versioned(&e))
        }
        RefusalCheck::VtReplica => {
            let mut replica = Replica::empty(vt.clone());
            c.inputs
                .iter()
                .flatten()
                .try_for_each(|d| vt.decode_delta(&built(d)).and_then(|d| replica.apply_delta(&d)))
                .err()
                .map(|e| versioned(&e))
        }
        RefusalCheck::MerkleInclusion => {
            let tagged = Tagged::<H>::new();
            InclusionProof::<H>::from_value(&input())
                .and_then(|p| merkle::check_inclusion(&tagged, &root(), &digest(c.leaf_hash.as_ref()), &p))
                .err()
                .map(|e| merkle_kind(&e))
        }
        RefusalCheck::MerkleConsistency => {
            let tagged = Tagged::<H>::new();
            ConsistencyProof::<H>::from_value(&input())
                .and_then(|p| merkle::check_consistency(&tagged, &digest(c.old_root.as_ref()), &root(), &p))
                .err()
                .map(|e| merkle_kind(&e))
        }
        RefusalCheck::TrieLookup => {
            let mode = match c.mode.as_ref().expect("a mode") {
                RefusalMode::RefusalFixedMode(m) => KeyMode::fixed(m.fixed),
                RefusalMode::RefusalVariableMode(m) => KeyMode::variable(m.variable),
            }
            .unwrap();
            let key = bytes(c.key.as_ref().expect("a key"));
            let hashing = TrieHashing::<H>::new();
            let answer = TrieProof::<H>::from_value(&input()).and_then(|p| {
                let verified = p.check(&hashing, mode, &root())?;
                mode.path(&key)?;
                match verified.lookup(&key) {
                    Lookup::NotCovered => Err(TrieError::NotCovered { what: "the key".into() }),
                    Lookup::Present(_) | Lookup::Absent => Ok(()),
                }
            });
            answer.err().map(|e| trie_kind(&e))
        }
        RefusalCheck::PoseidonProof => PoseidonMerkleProof::from_value(&input()).err().map(|e| poseidon_kind(&e)),
    }
}

#[test]
fn every_refusal_case_gives_its_kind_and_normative_fields() {
    let v: RefusalVectors = load("refusals_v1.json");
    let vt = Vt::new(U64Key::new(8).unwrap()).unwrap();
    assert!(v.cases.len() >= 30, "{} cases", v.cases.len());
    for c in &v.cases {
        let expected = match &c.outcome {
            RefusalOutcome::Accepted => None,
            RefusalOutcome::RefusalError(e) => {
                Some(Refused { kind: e.kind.clone(), limit: e.limit, depth: e.depth, component: e.component.clone() })
            }
        };
        let start = std::time::Instant::now();
        assert_eq!(run(&vt, c), expected, "{}", c.name);
        assert!(start.elapsed().as_secs() < 20, "{}: took {:?}", c.name, start.elapsed());
    }
}
