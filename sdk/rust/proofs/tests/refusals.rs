//! `shared/vectors/refusals_v1.json` (docs/specs/conformance.md): every case run as section 6 of the spec defines its
//! check, and its outcome compared with the vector's: "accepted", or the refusal's kind and normative fields.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    clippy::too_many_lines,
    reason = "tests fail by panicking, and run with std"
)]

#[path = "../../core/tests/common/mod.rs"]
mod common;

use common::{Step, build, bytes, load};
use dilf4s_core::codec::Model;
use dilf4s_core::generated::refusal_vectors::{
    BuiltStep, BuiltValue, MerkleRefusal, PoseidonRefusal, RefusalCase, RefusalCheck, RefusalMode, RefusalOutcome,
    RefusalVectors, TrieRefusal, VtCause, VtKeyCause, VtRefusal, VtTrieCause,
};
use dilf4s_core::generated::versioned_trie as w;
use dilf4s_core::generated::versioned_trie_vectors::{VersionedTrieVectors, VtVersionError};
use dilf4s_core::generated::{merkle_vectors::MerkleVectors, trie_vectors::TrieVectors};
use dilf4s_core::hash::{Digest, Sha256};
use dilf4s_core::value::Value;
use dilf4s_proofs::MAX_PROOF_WEIGHT;
use dilf4s_proofs::merkle::{self, ConsistencyProof, InclusionProof, MerkleError, MerkleErrorKind, Tagged, TreeHead};
use dilf4s_proofs::poseidon::merkle::{PoseidonMerkleError, PoseidonMerkleErrorKind, PoseidonMerkleProof};
use dilf4s_proofs::trie::{self, KeyMode, Lookup, TrieError, TrieErrorKind, TrieHashing, TrieProof};
use dilf4s_proofs::versioned::{
    MAX_VALUE_BYTES, MAX_VALUE_DEPTH, Replica, StateKey, StateKeyError, U64Key, VersionedTrie, VersionedTrieError,
    VersionedTrieErrorKind, check_value,
};

type H = Sha256;
type Vt = VersionedTrie<U64Key, H>;

/// A refusal as the vectors record it: its family and that family's kind, and its normative fields.
fn vt(e: &VersionedTrieError) -> RefusalOutcome {
    let (limit, depth) = match e {
        VersionedTrieError::TooLarge { limit, .. } => (Some(*limit), None),
        VersionedTrieError::TooDeep { depth, limit, .. } => (Some(*limit), Some(*depth)),
        _ => (None, None),
    };
    let cause = match e {
        VersionedTrieError::Trie { error } => Some(VtCause::VtTrieCause(VtTrieCause { trie: error.kind() })),
        VersionedTrieError::Key { error } => {
            let segment = match error {
                StateKeyError::InvalidSegment { segment, .. } | StateKeyError::SegmentTooLong { segment, .. } => {
                    Some(*segment)
                }
                _ => None,
            };
            Some(VtCause::VtKeyCause(VtKeyCause { state_key: error.kind(), segment }))
        }
        _ => None,
    };
    let component = match e {
        VersionedTrieError::RootMismatch { component, .. } => Some(*component),
        _ => None,
    };
    RefusalOutcome::VtRefusal(VtRefusal { versioned_trie: e.kind(), cause, limit, depth, component })
}

fn versioned(e: &VersionedTrieError) -> RefusalOutcome {
    vt(e)
}

fn merkle_kind(e: &MerkleError) -> RefusalOutcome {
    let limit = match e {
        MerkleError::TooLarge { limit } => Some(*limit),
        _ => None,
    };
    RefusalOutcome::MerkleRefusal(MerkleRefusal { merkle: e.kind(), limit })
}

fn trie_kind(e: &TrieError) -> RefusalOutcome {
    let limit = match e {
        TrieError::TooLarge { limit } => Some(*limit),
        _ => None,
    };
    RefusalOutcome::TrieRefusal(TrieRefusal { trie: e.kind(), limit })
}

fn mode_of(m: Option<&RefusalMode>) -> KeyMode {
    match m.expect("a mode") {
        RefusalMode::RefusalFixedMode(m) => KeyMode::fixed(m.fixed),
        RefusalMode::RefusalVariableMode(m) => KeyMode::variable(m.variable),
    }
    .unwrap()
}

fn poseidon_kind(e: &PoseidonMerkleError) -> RefusalOutcome {
    let depth = match e {
        PoseidonMerkleError::Position { depth, .. } => Some(*depth),
        _ => None,
    };
    RefusalOutcome::PoseidonRefusal(PoseidonRefusal { poseidon_merkle: e.kind(), depth })
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

/// `vt-commit`: the weight of the delta it would serve at version 1 (no parent; the head's version key and roots at
/// their widths), checked first, then each value as a commit checks it, as the store refuses before writing.
fn commit(c: &RefusalCase) -> Option<RefusalOutcome> {
    let upserts: Vec<w::VersionedTrieUpsert> = c
        .batch
        .iter()
        .flatten()
        .map(|u| w::VersionedTrieUpsert { k: u.state_key.clone(), v: built(&u.value) })
        .collect();
    let check =
        |u: &w::VersionedTrieUpsert| check_value(&StateKey::parse(&u.k).unwrap(), &u.v).err().map(|e| versioned(&e));
    // what each value's depth and weight alone refuse, then the delta's weight, then each value's measured length
    if let Some(deep) =
        upserts.iter().filter(|u| u.v.depth() > MAX_VALUE_DEPTH || u.v.weight() > MAX_VALUE_BYTES).find_map(check)
    {
        return Some(deep);
    }
    let head = w::VersionedTrieHead { v: 1u64.to_be_bytes().to_vec(), s: vec![0; 32], h: vec![0; 32] };
    let delta =
        w::VersionedTrieDelta { parent: None, head, upserts: upserts.clone(), removes: vec![] }.to_value().unwrap();
    if delta.weight() > MAX_PROOF_WEIGHT {
        return Some(RefusalOutcome::VtRefusal(VtRefusal {
            versioned_trie: VersionedTrieErrorKind::TooLarge,
            cause: None,
            limit: Some(MAX_PROOF_WEIGHT),
            depth: None,
            component: None,
        }));
    }
    upserts.iter().find_map(check)
}

/// The case's outcome, by its check (conformance.md section 6): the first refusal, or `None`.
fn run(vt: &Vt, c: &RefusalCase) -> Option<RefusalOutcome> {
    let input = || built(c.input.as_ref().expect("an input"));
    let state_key = || StateKey::parse(c.state_key.as_deref().expect("a state key")).unwrap();
    let root = || digest(c.root.as_ref());
    match c.check {
        RefusalCheck::VtValue => check_value(&state_key(), &input()).err().map(|e| versioned(&e)),
        RefusalCheck::VtChunk => {
            let chunk =
                c.chunk.as_deref().map_or_else(|| vec![0u8; usize::try_from(c.zeros.expect("zeros")).unwrap()], bytes);
            vt.decode_chunk(&chunk).err().map(|e| versioned(&e))
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
            let mode = mode_of(c.mode.as_ref());
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
        RefusalCheck::VtCommit => commit(c),
        RefusalCheck::MerkleVerify => {
            // the trusted head is parsed by the caller, before the claim: a root of the wrong length is Encoding
            let Ok(root) = Digest::<H>::from_slice(&bytes(c.root.as_ref().expect("a root"))) else {
                return Some(RefusalOutcome::MerkleRefusal(MerkleRefusal {
                    merkle: MerkleErrorKind::Encoding,
                    limit: None,
                }));
            };
            let head = TreeHead::<H> { root, size: c.size.expect("a size") };
            let leaf = built(c.claim.as_ref().expect("a leaf"));
            merkle::merkle_verify(&Tagged::<H>::new(), &head, &leaf, &input()).err().map(|e| merkle_kind(&e))
        }
        RefusalCheck::TrieVerify => {
            let claim = c.claim.as_ref().map_or(Value::Null, built);
            let key = bytes(c.key.as_ref().expect("a key"));
            let root = bytes(c.root.as_ref().expect("a root"));
            trie::trie_verify(&TrieHashing::<H>::new(), mode_of(c.mode.as_ref()), &root, &key, &claim, &input())
                .err()
                .map(|e| trie_kind(&e))
        }
        RefusalCheck::TriePrefixVerify => {
            let entries: Vec<(Vec<u8>, Value)> =
                c.entries.iter().flatten().map(|e| (bytes(&e.key), built(&e.value))).collect();
            let claimed: Vec<(&[u8], &Value)> = entries.iter().map(|(k, v)| (k.as_slice(), v)).collect();
            let prefix = bytes(c.prefix.as_ref().expect("a prefix"));
            let root = bytes(c.root.as_ref().expect("a root"));
            trie::trie_prefix_verify(
                &TrieHashing::<H>::new(),
                mode_of(c.mode.as_ref()),
                &root,
                &prefix,
                &claimed,
                &input(),
            )
            .err()
            .map(|e| trie_kind(&e))
        }
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
            refused => Some(refused.clone()),
        };
        let start = std::time::Instant::now();
        assert_eq!(run(&vt, c), expected, "{}", c.name);
        assert!(start.elapsed().as_secs() < 20, "{}: took {:?}", c.name, start.elapsed());
    }
}

/// The kinds no input reaches through this SDK's verifiers (conformance.md section 6), each with its reason.
const EXEMPT_VT: [VersionedTrieErrorKind; 5] = [
    VersionedTrieErrorKind::Empty,
    VersionedTrieErrorKind::VersionConflict,
    VersionedTrieErrorKind::CommitRewrite,
    VersionedTrieErrorKind::NotRetained,
    VersionedTrieErrorKind::Store,
];
const EXEMPT_TRIE: [TrieErrorKind; 2] = [TrieErrorKind::MissingNode, TrieErrorKind::InvalidNode];
const EXEMPT_POSEIDON: [PoseidonMerkleErrorKind; 2] =
    [PoseidonMerkleErrorKind::Depth, PoseidonMerkleErrorKind::OccupiedPosition];

/// The gate of conformance.md section 6, total over each family's generated kinds (`ALL`): every kind has a row in
/// `refusals_v1.json`, the malformed sections of `merkle_v1.json` and `trie_v1.json`, or `versioned_trie_v1.json`'s
/// version errors, or is exempt; no exempt kind has one; and every row is of its check's family.
#[test]
fn every_kind_of_the_proof_families_has_a_row_or_is_exempt() {
    let v: RefusalVectors = load("refusals_v1.json");
    let m: MerkleVectors = load("merkle_v1.json");
    let t: TrieVectors = load("trie_v1.json");
    let vtv: VersionedTrieVectors = load("versioned_trie_v1.json");
    let (mut vts, mut merkles, mut tries, mut poseidons) = (Vec::new(), Vec::new(), Vec::new(), Vec::new());
    for c in &v.cases {
        let check = c.check.to_string();
        match &c.outcome {
            RefusalOutcome::VtRefusal(r) => {
                assert!(check.starts_with("vt-"), "{}: a {check} row is a versioned-trie refusal", c.name);
                vts.push(r.versioned_trie);
                if let Some(VtCause::VtTrieCause(cause)) = &r.cause {
                    tries.push(cause.trie);
                }
            }
            RefusalOutcome::MerkleRefusal(r) => {
                assert!(check.starts_with("merkle-"), "{}: a {check} row is a Merkle refusal", c.name);
                merkles.push(r.merkle);
            }
            RefusalOutcome::TrieRefusal(r) => {
                assert!(check.starts_with("trie-"), "{}: a {check} row is a trie refusal", c.name);
                tries.push(r.trie);
            }
            RefusalOutcome::PoseidonRefusal(r) => {
                assert!(check.starts_with("poseidon-"), "{}: a {check} row is a Poseidon refusal", c.name);
                poseidons.push(r.poseidon_merkle);
            }
            RefusalOutcome::Accepted => {}
        }
    }
    merkles.extend(m.malformed.iter().map(|r| r.error));
    tries.extend(t.malformed.iter().map(|r| r.error));
    vts.extend(vtv.version_errors.iter().map(|r| match &r.outcome {
        VtVersionError::VtVersionNotIncreasing(_) => VersionedTrieErrorKind::VersionNotIncreasing,
        VtVersionError::VtUnknownVersion(_) => VersionedTrieErrorKind::UnknownVersion,
    }));
    for k in VersionedTrieErrorKind::ALL {
        assert_ne!(vts.contains(&k), EXEMPT_VT.contains(&k), "versioned trie {k}: a row exactly when not exempt");
    }
    for k in MerkleErrorKind::ALL {
        assert!(merkles.contains(&k), "Merkle {k} has no row");
    }
    for k in TrieErrorKind::ALL {
        assert_ne!(tries.contains(&k), EXEMPT_TRIE.contains(&k), "trie {k}: a row exactly when not exempt");
    }
    for k in PoseidonMerkleErrorKind::ALL {
        assert_ne!(poseidons.contains(&k), EXEMPT_POSEIDON.contains(&k), "Poseidon {k}: a row exactly when not exempt");
    }
}
