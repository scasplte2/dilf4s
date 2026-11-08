//! `shared/vectors/versioned_trie_v1.json`: the head KAT, every proof kind, deltas replayed through a replica, the
//! snapshot restored chunk by chunk, state keys; and refusals.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    clippy::as_conversions,
    clippy::cast_possible_truncation,
    clippy::many_single_char_names,
    clippy::too_many_lines,
    reason = "tests fail by panicking, and run with std"
)]

#[path = "../../core/tests/common/mod.rs"]
mod common;

use common::{bytes, load};
use dilf4s_core::cbor;
use dilf4s_core::codec::{Model, TextPattern};
use dilf4s_core::generated::trie::TrieRecord;
use dilf4s_core::generated::versioned_trie_vectors::{
    StateKeyErrorKind, VersionedTrieVectors, VtKind, VtProof, VtSchemaOutcome, VtStateKeyOutcome, VtStateKeyRefusal,
    VtUnknownVersion, VtVersionCheck, VtVersionError, VtVersionErrorCase, VtVersionKeyOutcome, VtVersionNotIncreasing,
};
use dilf4s_core::hash::{Digest, Sha256};
use dilf4s_core::value::Value;
use dilf4s_proofs::trie::{TrieErrorKind, TrieProof};
use dilf4s_proofs::versioned::VersionedTrieErrorKind;
use dilf4s_proofs::versioned::{
    BytesKey, Head, Replica, SnapshotChunk, StateKey, StateKeyError, StateNamespace, U64Key, VersionKey, VersionedTrie,
    VersionedTrieError, VersionedTrieProof,
};

type H = Sha256;
type Vt = VersionedTrie<U64Key, H>;

/// The schema's outcome of a state-key refusal: its kind (the SDK's error is generated from the schema's family, so its
/// kind is the schema's) and normative segment.
fn key_outcome<T>(r: &Result<T, StateKeyError>) -> VtStateKeyOutcome {
    let Err(e) = r else { return VtStateKeyOutcome::Accepted };
    let segment = match e {
        StateKeyError::InvalidSegment { segment, .. } | StateKeyError::SegmentTooLong { segment, .. } => Some(*segment),
        _ => None,
    };
    VtStateKeyOutcome::VtStateKeyRefusal(VtStateKeyRefusal { kind: e.kind(), segment })
}

fn d(hex: &str) -> Digest<H> {
    Digest::from_slice(&bytes(hex)).unwrap()
}

fn value(hex: &str) -> Value {
    cbor::decode(&bytes(hex)).unwrap()
}

fn key(s: &str) -> StateKey {
    StateKey::parse(s).unwrap()
}

fn setup() -> (VersionedTrieVectors, Vt) {
    let v: VersionedTrieVectors = load("versioned_trie_v1.json");
    let vt = Vt::new(U64Key::new(u8::try_from(v.width).unwrap()).unwrap()).unwrap();
    (v, vt)
}

fn proof(vt: &Vt, p: &VtProof) -> VersionedTrieProof<u64, H> {
    vt.decode_proof(&value(&p.cbor)).unwrap_or_else(|e| panic!("{}: {e}", p.name))
}

#[test]
fn head_kat() {
    let (v, vt) = setup();
    assert_eq!(vt.hashing().empty(), d(&v.empty_state_root));
    let k = &v.head;
    let head = Head { version: k.version, state_root: d(&k.state_root), history_root: d(&k.history_root) };
    assert_eq!(dilf4s_core::hex::encode(&vt.vkey(&k.version).unwrap()), k.vkey);
    assert_eq!(vt.root(&head).unwrap(), d(&k.root));
    assert_eq!(dilf4s_core::hex::encode(&vt.head_to_wire(&head).unwrap().to_cbor().unwrap()), k.cbor);
    assert_eq!(d(&k.root), d(&v.root));
}

#[test]
fn proofs() {
    let (v, vt) = setup();
    let root = d(&v.root);
    let mut counts = std::collections::BTreeMap::<&str, usize>::new();
    // 2^60 leaves in 60 allocations
    let shared = (0..60).fold(Value::text("a"), |v, _| Value::array(vec![v.clone(), v]).unwrap());
    for p in &v.proofs {
        let pr = proof(&vt, p);
        let claims: Vec<(StateKey, Option<Value>)> =
            p.claims.iter().flatten().map(|c| (key(&c.key), c.value.as_deref().map(value))).collect();
        let answer = p.answer.as_ref().map(|a| a.version.zip(a.state_root.as_deref().map(d)));
        let kind = match p.kind {
            VtKind::State => {
                let state = vt.verify_state(&root, &pr).unwrap();
                for (k, x) in &claims {
                    assert!(state.holds(k, x.as_ref()).unwrap(), "{}: {k}", p.name);
                    // a different claim is false
                    assert!(!state.holds(k, Some(&Value::text("other"))).unwrap(), "{}: {k} other", p.name);
                    // a compact claim with 2^60 leaves is not held, and is never committed (expanded) to say so
                    assert!(!state.holds(k, Some(&shared)).unwrap(), "{}: {k} shared", p.name);
                    // absent equals null: a null claim is a claim of absence
                    assert_eq!(state.holds(k, Some(&Value::Null)).unwrap(), x.is_none(), "{}: {k} null", p.name);
                }
                for l in p.listings.iter().flatten() {
                    let ns = StateNamespace::parse(&l.namespace).unwrap();
                    let expected: Vec<(StateKey, Digest<H>)> =
                        l.entries.iter().map(|e| (key(&e.key), vt.value_hash(&value(&e.value)))).collect();
                    assert_eq!(state.namespace(&ns).unwrap(), expected, "{}: {}", p.name, l.namespace);
                }
                // a state proof is not a head proof, nor about a version
                assert!(vt.verify_version(&root, &pr, &1).is_err());
                "state"
            }
            VtKind::Historical => {
                let state = vt.verify_historical_key(&root, &pr, &p.at.unwrap()).unwrap();
                for (k, x) in &claims {
                    assert!(state.holds(k, x.as_ref()).unwrap(), "{}: {k}", p.name);
                }
                assert_eq!(vt.verify_state(&root, &pr).unwrap_err().kind(), VersionedTrieErrorKind::Malformed);
                "historical"
            }
            VtKind::Version => {
                let at = p.at.unwrap();
                assert_eq!(
                    vt.verify_version(&root, &pr, &at).unwrap(),
                    answer.clone().unwrap().map(|(_, s)| s),
                    "{}",
                    p.name
                );
                assert_eq!(
                    vt.verify_version(&root, &pr, &(at + 1)).unwrap_err().kind(),
                    VersionedTrieErrorKind::Malformed
                );
                if answer.unwrap().is_none() {
                    assert_eq!(
                        vt.verify_historical_key(&root, &pr, &at).unwrap_err().kind(),
                        VersionedTrieErrorKind::UnknownVersion
                    );
                }
                "version"
            }
            VtKind::AsOf => {
                assert_eq!(vt.verify_as_of(&root, &pr, &p.at.unwrap()).unwrap(), answer.unwrap(), "{}", p.name);
                "asOf"
            }
            VtKind::Head => {
                let head = vt.verify_head(&root, &pr).unwrap();
                assert_eq!(vt.root(&head).unwrap(), root);
                "head"
            }
            VtKind::Versions => {
                let versions: Vec<(u64, Digest<H>)> = p
                    .versions
                    .iter()
                    .flatten()
                    .map(|x| (x.version.unwrap(), d(x.state_root.as_deref().unwrap())))
                    .collect();
                let (from, until) = (p.at.unwrap(), p.until);
                assert!(vt.verify_versions(&root, &pr, &from, until.as_ref(), &versions).unwrap(), "{}", p.name);
                assert!(!vt.verify_versions(&root, &pr, &from, until.as_ref(), &versions[1..]).unwrap(), "{}", p.name);
                let mut wrong = versions.clone();
                wrong[0].1 = root.clone();
                assert!(!vt.verify_versions(&root, &pr, &from, until.as_ref(), &wrong).unwrap(), "{}", p.name);
                "versions"
            }
        };
        *counts.entry(kind).or_default() += 1;
        // against another root, every check is a root mismatch
        let other = vt.hashing().empty();
        assert_eq!(vt.verify_head(&other, &pr).unwrap_err().kind(), VersionedTrieErrorKind::RootMismatch, "{}", p.name);
    }
    eprintln!("versioned trie: {} proofs {counts:?}", v.proofs.len());
    assert!(counts.contains_key("versions"), "the vectors carry versions proofs");
}

#[test]
fn deltas_replay_through_a_replica() {
    let (v, vt) = setup();
    let mut replica = Replica::empty(vt.clone());
    for c in &v.commits {
        let delta = vt.decode_delta(&value(&c.delta)).unwrap();
        assert_eq!(delta.head.version, c.version);
        let upserts: Vec<(String, String)> =
            delta.upserts.iter().map(|(k, x)| (k.to_string(), dilf4s_core::hex::encode(&cbor::encode(x)))).collect();
        let expected: Vec<(String, String)> = c.upserts.iter().map(|e| (e.key.clone(), e.value.clone())).collect();
        assert_eq!(upserts, expected, "version {}", c.version);
        assert_eq!(delta.removes.iter().map(ToString::to_string).collect::<Vec<_>>(), c.removes);
        // a delta out of order is refused, and leaves the replica as it was
        let before = replica.root().unwrap();
        replica.apply_delta(&delta).unwrap_or_else(|e| panic!("version {}: {e}", c.version));
        let head = replica.head().unwrap();
        assert_eq!((&head.state_root, &head.history_root), (&d(&c.state_root), &d(&c.history_root)));
        assert_eq!(replica.root().unwrap(), Some(d(&c.root)));
        let again = replica.apply_delta(&delta).unwrap_err();
        assert_eq!(again.kind(), VersionedTrieErrorKind::RootMismatch, "a replayed delta");
        assert_ne!(before, replica.root().unwrap());
    }
    assert_eq!(replica.root().unwrap(), Some(d(&v.root)));
    eprintln!("versioned trie: {} deltas replayed", v.commits.len());
}

fn kat_head(v: &VersionedTrieVectors) -> Head<u64, H> {
    Head { version: v.head.version, state_root: d(&v.head.state_root), history_root: d(&v.head.history_root) }
}

#[test]
fn snapshot_restores_the_root() {
    let (v, vt) = setup();
    let trusted = kat_head(&v);
    let chunks: Vec<SnapshotChunk<u64, H>> =
        v.snapshot.chunks.iter().map(|c| vt.decode_chunk(&bytes(c)).unwrap()).collect();
    assert!(chunks.len() > 3);
    let snapshot = vt.read_snapshot(trusted.clone(), chunks.clone()).unwrap();
    let replica = Replica::from_snapshot(vt.clone(), trusted.clone(), &snapshot).unwrap();
    assert_eq!(replica.root().unwrap(), Some(d(&v.root)));

    // the same state as following every delta
    let mut followed = Replica::empty(vt.clone());
    for c in &v.commits {
        followed.apply_delta(&vt.decode_delta(&value(&c.delta)).unwrap()).unwrap();
    }
    assert_eq!(followed.entries().collect::<Vec<_>>(), replica.entries().collect::<Vec<_>>());
    assert_eq!(
        replica.entries().map(|(k, v)| (k.clone(), v.clone())).collect::<Vec<_>>(),
        snapshot.entries,
        "the replica holds the snapshot's entries, in key order"
    );
    assert!(snapshot.entries.iter().all(|(k, v)| replica.get(k) == Some(v)));

    // refusals: another trusted head, reordered, truncated, a second head, state after history
    let other = Head { version: trusted.version + 1, ..trusted.clone() };
    assert_eq!(vt.read_snapshot(other, chunks.clone()).unwrap_err().kind(), VersionedTrieErrorKind::Malformed);
    let mut swapped = chunks.clone();
    swapped.swap(1, 2);
    assert_eq!(vt.read_snapshot(trusted.clone(), swapped).unwrap_err().kind(), VersionedTrieErrorKind::Malformed);
    let truncated = chunks[..chunks.len() - 1].to_vec();
    assert_eq!(vt.read_snapshot(trusted.clone(), truncated).unwrap_err().kind(), VersionedTrieErrorKind::Malformed);
    let mut extra = chunks.clone();
    extra.push(chunks[0].clone());
    assert_eq!(vt.read_snapshot(trusted.clone(), extra).unwrap_err().kind(), VersionedTrieErrorKind::Malformed);
    let first_state = chunks.iter().position(|c| matches!(c, SnapshotChunk::State(_))).unwrap();
    let mut late = chunks[..chunks.len() - 1].to_vec();
    late.push(chunks[first_state].clone());
    assert_eq!(vt.read_snapshot(trusted.clone(), late).unwrap_err().kind(), VersionedTrieErrorKind::Malformed);
    // a null value, an empty chunk
    let mut reader = vt.snapshot_reader(trusted.clone());
    reader.feed(chunks[0].clone()).unwrap();
    assert_eq!(
        reader.feed(SnapshotChunk::State(vec![(key("a"), Value::Null)])).unwrap_err().kind(),
        VersionedTrieErrorKind::Malformed
    );
    assert_eq!(reader.feed(SnapshotChunk::State(vec![])).unwrap_err().kind(), VersionedTrieErrorKind::Malformed);
    // a self-consistent snapshot of another state does not rebuild the trusted roots
    let mut forged = snapshot.clone();
    forged.entries[0].1 = Value::int(12345);
    assert_eq!(
        Replica::from_snapshot(vt.clone(), trusted.clone(), &forged).err().unwrap().kind(),
        VersionedTrieErrorKind::RootMismatch
    );
    // a chunk over 4 MiB is refused before decoding
    assert_eq!(vt.decode_chunk(&vec![0u8; (1 << 22) + 1]).unwrap_err().kind(), VersionedTrieErrorKind::TooLarge);
    assert_eq!(vt.decode_chunk(&[0xff]).unwrap_err().kind(), VersionedTrieErrorKind::Malformed);
    eprintln!("versioned trie: snapshot of {} chunks restored", chunks.len());
}

#[test]
fn state_keys() {
    let (v, _) = setup();
    for c in &v.state_keys {
        assert_eq!(key_outcome(&StateKey::parse(&c.key)), c.outcome, "{:?}", c.key);
        assert_eq!(TextPattern::StateKey.matches(&c.key), c.schema == VtSchemaOutcome::Accepted, "{:?} schema", c.key);
    }
    for c in &v.state_key_bytes {
        assert_eq!(key_outcome(&StateKey::from_bytes(&bytes(&c.bytes))), c.outcome, "bytes {}", c.bytes);
    }
    for c in &v.namespaces {
        assert_eq!(key_outcome(&StateNamespace::parse(&c.namespace)), c.outcome, "namespace {:?}", c.namespace);
    }
    assert!(StateNamespace::parse("fiber").is_ok());
    assert!(StateNamespace::parse(&"x".repeat(255)).is_err());
    assert!(StateNamespace::parse(&["a"; 16].join("/")).is_err());
    assert!(StateNamespace::parse(&["a"; 15].join("/")).is_ok());
    let ns = StateNamespace::parse("fiber").unwrap();
    assert!(ns.contains(&key("fiber/1")) && !ns.contains(&key("fiberx/1")) && !ns.contains(&key("fiber")));
    assert!(StateKey::from_bytes(&[0xff]).is_err());
    eprintln!("versioned trie: {} state keys", v.state_keys.len());
}

/// The coverage gate (conformance.md section 6) for `StateKeyError`: every kind of the schema's `state-key-error-kind`
/// has a row. [`key_outcome`] maps every SDK variant onto a schema kind (exhaustively, so a new variant does not
/// compile until it is mapped), so every SDK kind has a row too.
#[test]
fn every_state_key_error_kind_has_a_row() {
    let (v, _) = setup();
    let outcomes = v
        .state_keys
        .iter()
        .map(|c| &c.outcome)
        .chain(v.state_key_bytes.iter().map(|c| &c.outcome))
        .chain(v.namespaces.iter().map(|c| &c.outcome));
    let pinned: Vec<StateKeyErrorKind> = outcomes
        .filter_map(|o| match o {
            VtStateKeyOutcome::VtStateKeyRefusal(r) => Some(r.kind),
            VtStateKeyOutcome::Accepted => None,
        })
        .collect();
    for kind in StateKeyErrorKind::ALL {
        assert!(pinned.contains(&kind), "no row pins {kind:?}");
    }
}

/// conformance.md section 8: a 1 MiB key or trie key is refused by its length, as a preview or a size.
#[test]
fn a_huge_state_key_is_refused_with_a_bounded_error() {
    let huge = 1usize << 20;
    let text = StateKey::parse(&"a/".repeat(huge / 2)).unwrap_err();
    let bytes = StateKey::from_bytes(&vec![0xff; huge]).unwrap_err();
    let namespace = StateNamespace::parse(&"x".repeat(huge)).unwrap_err();
    for e in [&text, &bytes, &namespace] {
        let rendered = format!("{e}{e:?}");
        assert!(rendered.len() <= 1024, "the refusal renders {} bytes", rendered.len());
        assert!(
            dilf4s_proofs::versioned::input(e).is_none_or(|p| p.head().chars().count() <= 32),
            "a preview of at most 32 code points"
        );
    }
    assert_eq!(text, StateKeyError::TooLong { limit: 256 });
    assert_eq!(bytes, StateKeyError::TooLong { limit: 256 });
    assert_eq!(namespace, StateKeyError::TooLong { limit: 256 });
    assert_eq!(
        StateKey::parse("a/B/c").unwrap_err(),
        StateKeyError::InvalidSegment { key: dilf4s_core::Preview::of("a/B/c"), segment: 1 }
    );
}

#[test]
fn version_keys() {
    let k = U64Key::default();
    assert_eq!(k.encode(&258).unwrap(), vec![0, 0, 0, 0, 0, 0, 1, 2]);
    assert_eq!(k.decode(&[0, 0, 0, 0, 0, 0, 1, 2]).unwrap(), 258);
    assert_eq!(k.encode(&u64::MAX).unwrap_err().kind(), VersionedTrieErrorKind::InvalidVersion);
    assert_eq!(k.decode(&[0xff; 8]).unwrap_err().kind(), VersionedTrieErrorKind::InvalidVersion);
    assert_eq!(k.decode(&[0; 7]).unwrap_err().kind(), VersionedTrieErrorKind::InvalidVersion);
    let two = U64Key::new(2).unwrap();
    assert_eq!(two.encode(&0xffff).unwrap(), vec![0xff, 0xff]);
    assert!(two.encode(&0x1_0000).is_err());
    assert!(U64Key::new(0).is_err() && U64Key::new(9).is_err());
    let b = BytesKey::new(16).unwrap();
    assert_eq!(b.encode(&vec![1; 16]).unwrap(), vec![1; 16]);
    assert!(b.encode(&vec![1; 15]).is_err());
    assert!(BytesKey::new(0).is_err() && BytesKey::new(1025).is_err());
    assert!(VersionedTrie::<BytesKey, H>::new(b).is_ok());
}

fn tamper_top(p: &VersionedTrieProof<u64, H>) -> VersionedTrieProof<u64, H> {
    let mut records = p.top.to_records();
    for r in &mut records {
        if let TrieRecord::TrieLeaf(l) = r {
            l.v[0] ^= 1;
            break;
        }
    }
    VersionedTrieProof { top: TrieProof::from_records(&records).unwrap(), ..p.clone() }
}

#[test]
fn refusals() {
    let (v, vt) = setup();
    let root = d(&v.root);
    let find = |kind: VtKind| v.proofs.iter().find(|p| p.kind == kind).unwrap();
    let state = proof(&vt, find(VtKind::State));
    // a tampered top trie no longer hashes to the root: a trie root mismatch, an error here
    let e = vt.verify_state(&root, &tamper_top(&state)).err().unwrap();
    assert!(
        matches!(e, VersionedTrieError::Trie { ref error } if error.kind() == TrieErrorKind::RootMismatch),
        "{e:?}"
    );
    // a head claiming another state root does not bind
    let mut forged = state.clone();
    forged.head.state_root = forged.head.history_root.clone();
    assert_eq!(vt.verify_state(&root, &forged).err().unwrap().kind(), VersionedTrieErrorKind::RootMismatch);
    // no state part
    let none = VersionedTrieProof { state: None, ..state.clone() };
    assert_eq!(vt.verify_state(&root, &none).err().unwrap().kind(), VersionedTrieErrorKind::Malformed);
    // a key the state proof does not reach
    let s = vt.verify_state(&root, &state).unwrap();
    let far = key("zzz/never/proven");
    match s.lookup(&far) {
        Ok(None) | Err(VersionedTrieError::NotCovered { .. }) => {}
        other => panic!("{other:?}"),
    }
    // the lookup decides first, so a vast claim on such a key fails the same way, without being committed
    let shared = (0..60).fold(Value::text("a"), |v, _| Value::array(vec![v.clone(), v]).unwrap());
    match (s.lookup(&far), s.holds(&far, Some(&shared))) {
        (Ok(None), Ok(false))
        | (Err(VersionedTrieError::NotCovered { .. }), Err(VersionedTrieError::NotCovered { .. })) => {}
        other => panic!("{other:?}"),
    }
    // the head proof's history without its tail, against the version check: the as-of proof answers the version
    let head = proof(&vt, find(VtKind::Head));
    assert!(vt.verify_head(&root, &head).is_ok());
    let no_history = VersionedTrieProof { history: None, ..head.clone() };
    assert_eq!(vt.verify_head(&root, &no_history).unwrap_err().kind(), VersionedTrieErrorKind::Malformed);
    // an as-of answer of another version, or after t
    let as_of = v
        .proofs
        .iter()
        .find(|p| p.kind == VtKind::AsOf && p.answer.as_ref().is_some_and(|a| a.version.is_some()))
        .unwrap();
    let mut pr = proof(&vt, as_of);
    let t = as_of.at.unwrap();
    pr.at.as_mut().unwrap().version = t + 1;
    assert_eq!(vt.verify_as_of(&root, &pr, &t).unwrap_err().kind(), VersionedTrieErrorKind::Malformed);
    pr.at.as_mut().unwrap().version = 1;
    assert_eq!(
        vt.verify_as_of(&root, &pr, &t).unwrap_err().kind(),
        VersionedTrieErrorKind::NotCovered,
        "an earlier answer needs history the proof does not reach"
    );
    // a malformed proof value
    assert_eq!(vt.decode_proof(&Value::int(1)).unwrap_err().kind(), VersionedTrieErrorKind::Malformed);
}

/// The default version key stops at `2^63 - 1`: `key` is `int_to_bytes [version, 8]`, and the codec's outcome, both
/// ways, is the row's: the key, or a refusal of its kind.
#[test]
fn version_key_vectors() {
    let (v, _) = setup();
    let k = U64Key::default();
    let kind = |o: &VtVersionKeyOutcome| match o {
        VtVersionKeyOutcome::Accepted => None,
        VtVersionKeyOutcome::VtVersionKeyRefusal(_) => Some(VersionedTrieErrorKind::InvalidVersion),
    };
    assert!(v.version_keys.iter().any(|c| kind(&c.outcome).is_some()));
    assert!(v.version_keys.iter().any(|c| kind(&c.outcome).is_none()));
    for c in &v.version_keys {
        let key = bytes(&c.key);
        let expected = kind(&c.outcome);
        assert_eq!(key, c.version.to_be_bytes().to_vec(), "version {}: int_to_bytes [v, 8]", c.version);
        let encoded = k.encode(&c.version);
        assert_eq!(encoded.as_ref().err().map(VersionedTrieError::kind), expected, "encode {}", c.version);
        assert!(encoded.is_err() || encoded.ok() == Some(key.clone()), "encode {}: the key", c.version);
        let decoded = k.decode(&key);
        assert_eq!(decoded.as_ref().err().map(VersionedTrieError::kind), expected, "decode {}", c.version);
        assert!(decoded.is_err() || decoded.ok() == Some(c.version), "decode {}: the version", c.version);
    }
}

/// A compact value whose encoding would be vast is refused by measuring it up to the limit, never by encoding it.
#[test]
fn check_value_refuses_a_shared_expansion_without_expanding_it() {
    use dilf4s_proofs::versioned::{MAX_VALUE_BYTES, check_value};
    let k = key("big");
    let max = usize::try_from(MAX_VALUE_BYTES).unwrap();
    let doubled = |leaf: Value, n: usize| (0..n).fold(leaf, |v, _| Value::array(vec![v.clone(), v]).unwrap());
    let start = std::time::Instant::now();
    // 2^60 leaves in 60 allocations
    let refused = Err(VersionedTrieError::TooLarge { what: "the value of big".into(), limit: MAX_VALUE_BYTES });
    assert_eq!(check_value(&k, &doubled(Value::text("a"), 60)), refused, "the limit, nothing measured");
    // light by weight, over in bytes: each u64::MAX weighs 2 and encodes in 9
    let ints = doubled(Value::int(u64::MAX), 17);
    assert_eq!(check_value(&k, &ints), refused);
    assert!(start.elapsed().as_millis() < 2000, "took {:?}", start.elapsed());
    // the limit itself is exact: a byte string with a 5-byte head
    assert!(check_value(&k, &Value::bytes(vec![0u8; max - 5])).is_ok(), "exactly at the limit");
    assert_eq!(check_value(&k, &Value::bytes(vec![0u8; max - 4])), refused, "one over");
}

/// A `versionErrors` case under version key `key`: the refusal its check gives.
fn version_error<K: VersionKey + Clone>(key: K, c: &VtVersionErrorCase) -> VersionedTrieError {
    let vt = VersionedTrie::<K, H>::new(key).unwrap();
    match c.check {
        VtVersionCheck::Replica => {
            let mut replica = Replica::empty(vt.clone());
            c.deltas
                .iter()
                .flatten()
                .try_for_each(|d| vt.decode_delta(&value(d)).and_then(|d| replica.apply_delta(&d)))
                .unwrap_err()
        }
        VtVersionCheck::Historical => {
            let proof = vt.decode_proof(&value(c.proof.as_ref().unwrap())).unwrap();
            let at = vt.version_key().decode(&bytes(c.at.as_ref().unwrap())).unwrap();
            vt.verify_historical_key(&d(c.root.as_ref().unwrap()), &proof, &at).err().unwrap()
        }
    }
}

/// The versions a refusal names are their keys' full lowercase hex, at the default width and at wide ones up to 1024
/// bytes (conformance.md section 3): each case gives exactly its recorded refusal and keys.
#[test]
fn version_errors_name_full_keys() {
    let (v, _) = setup();
    let mut widths: Vec<u64> = v.version_errors.iter().map(|c| c.width).collect();
    widths.dedup();
    assert_eq!(widths, vec![8, 32, 1024]);
    for c in &v.version_errors {
        let e = if c.width == 8 {
            version_error(U64Key::default(), c)
        } else {
            version_error(BytesKey::new(u16::try_from(c.width).unwrap()).unwrap(), c)
        };
        let got = match e {
            VersionedTrieError::VersionNotIncreasing { head, next } => {
                VtVersionError::VtVersionNotIncreasing(VtVersionNotIncreasing { head, next })
            }
            VersionedTrieError::UnknownVersion { version } => {
                VtVersionError::VtUnknownVersion(VtUnknownVersion { version })
            }
            other => panic!("{}: {other:?}", c.name),
        };
        assert_eq!(got, c.outcome, "{}", c.name);
        let keys = match &got {
            VtVersionError::VtVersionNotIncreasing(r) => vec![r.head.clone(), r.next.clone()],
            VtVersionError::VtUnknownVersion(r) => vec![r.version.clone()],
        };
        assert!(keys.iter().all(|k| k.len() as u64 == 2 * c.width), "{}: keys in full", c.name);
    }
}

/// A caller's version key whose encoding ignores its width.
struct Lying(u16, usize);

impl VersionKey for Lying {
    type Version = u64;
    fn width(&self) -> u16 {
        self.0
    }
    fn encode(&self, _: &u64) -> Result<Vec<u8>, VersionedTrieError> {
        Ok(vec![0xab; self.1])
    }
    fn decode(&self, _: &[u8]) -> Result<u64, VersionedTrieError> {
        Ok(0)
    }
}

/// `vkey` checks every key at its width, so a caller's version key never lengthens the key a refusal names.
#[test]
fn a_version_key_of_another_width_is_refused_before_a_refusal_names_it() {
    let lying = VersionedTrie::<Lying, H>::new(Lying(8, 1 << 20)).unwrap();
    assert_eq!(lying.vkey(&1).unwrap_err().kind(), VersionedTrieErrorKind::InvalidVersion);
    assert_eq!(VersionedTrie::<Lying, H>::new(Lying(8, 8)).unwrap().vkey(&1).unwrap(), vec![0xab; 8]);
    assert!(VersionedTrie::<Lying, H>::new(Lying(1025, 1025)).is_err(), "a width past 1024");
}
