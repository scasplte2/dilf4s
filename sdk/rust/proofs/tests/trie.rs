//! `shared/vectors/trie_v1.json`, the JLVM-shaped verifiers, and malformed, invalid and uncovered proofs.

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
    reason = "tests fail by panicking, and run with std"
)]

#[path = "../../core/tests/common/mod.rs"]
mod common;

use std::collections::BTreeMap;

use common::{bytes, load};
use dilf4s_core::cbor;
use dilf4s_core::codec::{Codec, Model};
use dilf4s_core::generated::trie::{TRIE_PROOF, TrieBranch, TrieChildren, TrieLeaf, TrieRecord, TrieSegment, TrieStub};
use dilf4s_core::generated::trie_vectors::{
    TrieLookupResult, TrieMalformedError, TrieMode, TrieRangeResult, TrieVectors,
};
use dilf4s_core::hash::{Digest, DomainTag, Sha256, TaggedHash};
use dilf4s_core::value::Value;
use dilf4s_proofs::Bits;
use dilf4s_proofs::trie::{
    self, KeyMode, Lookup, TrieError, TrieHashing, TrieProof, root_from_entries, trie_prefix_verify, trie_range_verify,
    trie_verify,
};

type H = Sha256;

fn d(hex: &str) -> Digest<H> {
    Digest::from_slice(&bytes(hex)).unwrap()
}

/// The value hash of opaque bytes, as the vectors' tries use it (`Trie.putValue`). The library has no such API; core's
/// tagged hash computes it here.
fn raw_value_hash(data: &[u8]) -> Digest<H> {
    TaggedHash::<H>::new(DomainTag::new(trie::VALUE_TAG).unwrap()).hash(data)
}

fn mode_of(m: &TrieMode) -> KeyMode {
    match m {
        TrieMode::TrieFixedMode(f) => KeyMode::fixed(f.fixed).unwrap(),
        TrieMode::TrieVariableMode(v) => KeyMode::variable(v.variable).unwrap(),
    }
}

fn listing(entries: &[dilf4s_core::generated::trie_vectors::TrieListed]) -> Vec<(Vec<u8>, Digest<H>)> {
    entries.iter().map(|e| (bytes(&e.key), d(&e.value_hash))).collect()
}

#[test]
fn tags_match_core() {
    let t = TrieHashing::<H>::new();
    for tag in [trie::VALUE_TAG, trie::LEAF_TAG, trie::BRANCH_TAG, trie::EMPTY_TAG] {
        DomainTag::new(tag).unwrap();
    }
    assert_eq!(t.empty(), TaggedHash::<H>::new(DomainTag::new(trie::EMPTY_TAG).unwrap()).hash(&[]));
    let value_tag = TaggedHash::<H>::new(DomainTag::new(trie::VALUE_TAG).unwrap());
    assert_eq!(t.value_of(&Value::bytes(b"xy".to_vec())), value_tag.hash(b"\x00xy"));
}

#[test]
fn node_hashes_and_empty_root() {
    let v: TrieVectors = load("trie_v1.json");
    let t = TrieHashing::<H>::new();
    assert_eq!(t.empty(), d(&v.empty_root));
    let leaf = &v.node_hashes.leaf;
    let seg = Bits::from_padded(leaf.segment.bits, bytes(&leaf.segment.bytes)).unwrap();
    assert_eq!(t.leaf(&seg, &d(&leaf.value_hash)), d(&leaf.hash));
    let branch = &v.node_hashes.branch;
    let seg = Bits::from_padded(branch.segment.bits, bytes(&branch.segment.bytes)).unwrap();
    assert_eq!(t.branch(&seg, Some(&d(&branch.value_hash)), Some(&d(&branch.left)), None), d(&branch.hash));
}

#[test]
fn fixtures() {
    let v: TrieVectors = load("trie_v1.json");
    let t = TrieHashing::<H>::new();
    let (mut lookups, mut prefixes, mut ranges) = (0, 0, 0);
    for f in &v.fixtures {
        let mode = mode_of(&f.mode);
        let root = d(&f.root);
        for e in &f.entries {
            assert_eq!(raw_value_hash(&bytes(&e.value)), d(&e.value_hash), "{}: value hash", f.name);
        }
        let keys: Vec<Vec<u8>> = f.entries.iter().map(|e| bytes(&e.key)).collect();
        let built =
            root_from_entries(&t, mode, keys.iter().zip(&f.entries).map(|(k, e)| (k.as_slice(), d(&e.value_hash))))
                .unwrap();
        assert_eq!(built, root, "{}: root from entries", f.name);

        let value = cbor::decode(&bytes(&f.proof.cbor)).unwrap();
        let proof = TrieProof::<H>::from_value(&value).unwrap();
        assert_eq!(proof.to_value().unwrap(), value, "{}: re-encoding", f.name);
        let verified = proof.verify(&t, mode, &root).unwrap().unwrap_or_else(|| panic!("{}: verifies", f.name));
        for l in &f.proof.lookups {
            let expected = match &l.result {
                TrieLookupResult::TriePresent(p) => Lookup::Present(d(&p.present)),
                TrieLookupResult::Absent => Lookup::Absent,
                TrieLookupResult::NotCovered => Lookup::NotCovered,
            };
            assert_eq!(verified.lookup(&bytes(&l.key)), expected, "{}: lookup {}", f.name, l.key);
            lookups += 1;
        }
        for p in &f.proof.prefix_listings {
            let expected = listing(&p.entries);
            assert_eq!(verified.prefix(&bytes(&p.prefix)), Some(expected.clone()), "{}: prefix {}", f.name, p.prefix);
            // the streamed comparison agrees, and refuses any other claim
            assert_eq!(
                verified.lists_prefix(&bytes(&p.prefix), &expected),
                Some(true),
                "{}: lists {}",
                f.name,
                p.prefix
            );
            let other: Vec<_> = expected.iter().skip(1).cloned().collect();
            assert_eq!(
                verified.lists_prefix(&bytes(&p.prefix), &other),
                Some(expected.is_empty()),
                "{}: lists less {}",
                f.name,
                p.prefix
            );
            prefixes += 1;
        }
        for r in &f.proof.range_listings {
            let until = r.until.as_deref().map(bytes);
            let expected = match &r.result {
                TrieRangeResult::TrieRangeEntries(e) => Some(listing(&e.entries)),
                TrieRangeResult::NotCovered => None,
            };
            assert_eq!(
                verified.range(&bytes(&r.from), until.as_deref()),
                expected,
                "{}: range {} {:?}",
                f.name,
                r.from,
                r.until
            );
            let claimed = expected.clone().unwrap_or_default();
            assert_eq!(
                verified.lists_range(&bytes(&r.from), until.as_deref(), &claimed),
                expected.as_ref().map(|_| true),
                "{}: lists range {} {:?}",
                f.name,
                r.from,
                r.until
            );
            ranges += 1;
        }
        // every range the proof was made for is covered
        for r in &f.proof.ranges {
            let until = r.until.as_deref().map(bytes);
            assert!(
                verified.range(&bytes(&r.from), until.as_deref()).is_some(),
                "{}: range {} covered",
                f.name,
                r.from
            );
        }
        // another root is a root mismatch: None, never an error
        assert!(proof.verify(&t, mode, &t.empty()).unwrap().is_none());
        assert_eq!(proof.check(&t, mode, &t.empty()).unwrap_err().kind(), "RootMismatch");
    }
    eprintln!("trie: {} fixtures, {lookups} lookups, {prefixes} prefix and {ranges} range listings", v.fixtures.len());
}

// ---- a test-side prover: the whole trie as a proof, then stubs where asked

fn split(entries: &[(Bits, Digest<H>)], offset: u64) -> TrieRecordTree {
    if entries.len() == 1 {
        let (k, v) = &entries[0];
        return TrieRecordTree::Leaf(k.slice(offset, k.len()), v.clone());
    }
    let first = &entries[0].0;
    let last = &entries[entries.len() - 1].0;
    let pos = offset + first.common_prefix_from(offset, last, offset);
    let segment = first.slice(offset, pos);
    let (value, rest) = if first.len() == pos { (Some(entries[0].1.clone()), &entries[1..]) } else { (None, entries) };
    let cut = rest.partition_point(|(k, _)| k.get(pos) != Some(true));
    let side = |s: &[(Bits, Digest<H>)]| (!s.is_empty()).then(|| Box::new(split(s, pos + 1)));
    TrieRecordTree::Branch(segment, value, side(&rest[..cut]), side(&rest[cut..]))
}

enum TrieRecordTree {
    Leaf(Bits, Digest<H>),
    Branch(Bits, Option<Digest<H>>, Option<Box<TrieRecordTree>>, Option<Box<TrieRecordTree>>),
}

fn wire(b: &Bits) -> TrieSegment {
    TrieSegment { n: b.len(), b: b.padded_bytes().to_vec() }
}

fn node_hash(t: &TrieHashing<H>, n: &TrieRecordTree) -> Digest<H> {
    match n {
        TrieRecordTree::Leaf(s, v) => t.leaf(s, v),
        TrieRecordTree::Branch(s, v, l, r) => t.branch(
            s,
            v.as_ref(),
            l.as_ref().map(|x| node_hash(t, x)).as_ref(),
            r.as_ref().map(|x| node_hash(t, x)).as_ref(),
        ),
    }
}

/// Pre-order records, replacing by a stub every subtree whose path starts with one of `stubs`.
fn records(t: &TrieHashing<H>, n: &TrieRecordTree, at: &Bits, stubs: &[Bits], out: &mut Vec<TrieRecord>) {
    if stubs.iter().any(|s| at.starts_with(s)) {
        out.push(TrieRecord::TrieStub(TrieStub { h: node_hash(t, n).as_bytes().to_vec() }));
        return;
    }
    match n {
        TrieRecordTree::Leaf(s, v) => out.push(TrieRecord::TrieLeaf(TrieLeaf { s: wire(s), v: v.as_bytes().to_vec() })),
        TrieRecordTree::Branch(s, v, l, r) => {
            let c = match (l, r) {
                (Some(_), Some(_)) => TrieChildren::Both,
                (Some(_), None) => TrieChildren::Left,
                _ => TrieChildren::Right,
            };
            out.push(TrieRecord::TrieBranch(TrieBranch {
                s: wire(s),
                v: v.as_ref().map(|x| x.as_bytes().to_vec()),
                c,
            }));
            let here = at.concat(s);
            if let Some(l) = l {
                records(t, l, &here.with_bit(false), stubs, out);
            }
            if let Some(r) = r {
                records(t, r, &here.with_bit(true), stubs, out);
            }
        }
    }
}

struct Committed {
    mode: KeyMode,
    root: Vec<u8>,
    tree: TrieRecordTree,
    values: BTreeMap<Vec<u8>, Value>,
}

fn committed(mode: KeyMode, kv: &[(&[u8], Value)]) -> Committed {
    let t = TrieHashing::<H>::new();
    let values: BTreeMap<Vec<u8>, Value> = kv.iter().map(|(k, v)| (k.to_vec(), v.clone())).collect();
    let entries: Vec<(Bits, Digest<H>)> = values.iter().map(|(k, v)| (Bits::from_bytes(k), t.value_of(v))).collect();
    let tree = split(&entries, 0);
    let root = root_from_entries(&t, mode, values.iter().map(|(k, v)| (k.as_slice(), t.value_of(v)))).unwrap();
    assert_eq!(root, node_hash(&t, &tree), "builder and test prover agree");
    Committed { mode, root: root.as_bytes().to_vec(), tree, values }
}

impl Committed {
    fn proof(&self, stubs: &[Bits]) -> Value {
        let mut out = Vec::new();
        records(&TrieHashing::new(), &self.tree, &Bits::new(), stubs, &mut out);
        TRIE_PROOF.encode(&out).unwrap()
    }
}

fn sample() -> Committed {
    let mode = KeyMode::variable(4).unwrap();
    committed(
        mode,
        &[
            (b"", Value::int(0)),
            (b"a", Value::bytes(vec![0x61, 0x61])),
            (b"ab", Value::text("a")),
            (b"abc", Value::int(3)),
            (b"b", Value::Bool(false)),
            (b"ba", Value::array(vec![Value::int(1)]).unwrap()),
            (b"\xff\xff", Value::bytes(vec![])),
        ],
    )
}

fn bits(b: &[u8]) -> Bits {
    Bits::from_bytes(b)
}

#[test]
fn jlvm_trie_verify() {
    let c = sample();
    let t = TrieHashing::<H>::new();
    let full = c.proof(&[]);
    for (k, v) in &c.values {
        assert!(trie_verify(&t, c.mode, &c.root, k, v, &full).unwrap(), "{k:?}");
        // a null claims absence, so a present key with a null claim is false
        assert!(!trie_verify(&t, c.mode, &c.root, k, &Value::Null, &full).unwrap());
    }
    // bytes 0x6161 never verify the text "a"
    assert!(!trie_verify(&t, c.mode, &c.root, b"a", &Value::text("a"), &full).unwrap());
    // absent keys
    assert!(trie_verify(&t, c.mode, &c.root, b"abd", &Value::Null, &full).unwrap());
    assert!(!trie_verify(&t, c.mode, &c.root, b"abd", &Value::int(3), &full).unwrap());
    // another root: false
    let other = vec![0u8; 32];
    assert!(!trie_verify(&t, c.mode, &other, b"a", &Value::bytes(vec![0x61, 0x61]), &full).unwrap());
    // a key too long for the mode, a root of the wrong length, an undecodable proof: errors
    assert_eq!(trie_verify(&t, c.mode, &c.root, b"abcde", &Value::Null, &full).unwrap_err().kind(), "InvalidKey");
    assert_eq!(trie_verify(&t, c.mode, &c.root[1..], b"a", &Value::Null, &full).unwrap_err().kind(), "Encoding");
    assert_eq!(trie_verify(&t, c.mode, &c.root, b"a", &Value::Null, &Value::int(1)).unwrap_err().kind(), "Encoding");
    // a stub on the key's path: not covered, an error; elsewhere: still answers
    let stubbed = c.proof(&[bits(b"ab")]);
    assert_eq!(trie_verify(&t, c.mode, &c.root, b"abc", &Value::int(3), &stubbed).unwrap_err().kind(), "NotCovered");
    assert!(trie_verify(&t, c.mode, &c.root, b"ba", &Value::array(vec![Value::int(1)]).unwrap(), &stubbed).unwrap());
    // the wrong mode: fixed keys of another length, or a structure the mode forbids
    let fixed = KeyMode::fixed(2).unwrap();
    assert_eq!(trie_verify(&t, fixed, &c.root, b"a", &Value::Null, &full).unwrap_err().kind(), "InvalidKey");
    assert_eq!(trie_verify(&t, fixed, &c.root, b"ab", &Value::Null, &full).unwrap_err().kind(), "InvalidProof");
    let narrow = KeyMode::variable(2).unwrap();
    assert_eq!(trie_verify(&t, narrow, &c.root, b"ab", &Value::Null, &full).unwrap_err().kind(), "InvalidProof");
}

#[test]
fn jlvm_listings() {
    let c = sample();
    let t = TrieHashing::<H>::new();
    let full = c.proof(&[]);
    let under = |p: &[u8]| -> Vec<(Vec<u8>, Value)> {
        c.values.iter().filter(|(k, _)| k.starts_with(p)).map(|(k, v)| (k.clone(), v.clone())).collect()
    };
    let refs = |es: &[(Vec<u8>, Value)]| -> Vec<(Vec<u8>, Value)> { es.to_vec() };
    for p in [&b""[..], b"a", b"ab", b"b", b"c", b"\xff"] {
        let es = refs(&under(p));
        let claim: Vec<(&[u8], &Value)> = es.iter().map(|(k, v)| (k.as_slice(), v)).collect();
        assert!(trie_prefix_verify(&t, c.mode, &c.root, p, &claim, &full).unwrap(), "prefix {p:?}");
        if !claim.is_empty() {
            assert!(!trie_prefix_verify(&t, c.mode, &c.root, p, &claim[1..], &full).unwrap(), "incomplete {p:?}");
            let mut reversed = claim.clone();
            reversed.reverse();
            if reversed.len() > 1 {
                assert!(!trie_prefix_verify(&t, c.mode, &c.root, p, &reversed, &full).unwrap(), "order {p:?}");
            }
        }
    }
    // ranges, bounds need not be keys; a prefix sorts before its extensions
    let range = |from: &[u8], until: Option<&[u8]>| -> Vec<(Vec<u8>, Value)> {
        c.values
            .iter()
            .filter(|(k, _)| k.as_slice() >= from && until.is_none_or(|u| k.as_slice() < u))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect()
    };
    for (from, until) in
        [(&b""[..], None), (b"a", Some(&b"b"[..])), (b"ab", Some(b"ab\x00")), (b"b\x00", None), (b"z", Some(b"a"))]
    {
        let es = range(from, until);
        let claim: Vec<(&[u8], &Value)> = es.iter().map(|(k, v)| (k.as_slice(), v)).collect();
        assert!(
            trie_range_verify(&t, c.mode, &c.root, from, until, &claim, &full).unwrap(),
            "range {from:?} {until:?}"
        );
    }
    // a stub inside the queried prefix or range is not covered; outside it, it does not matter
    let stubbed = c.proof(&[bits(b"ab")]);
    let es = under(b"a");
    let claim: Vec<(&[u8], &Value)> = es.iter().map(|(k, v)| (k.as_slice(), v)).collect();
    assert_eq!(trie_prefix_verify(&t, c.mode, &c.root, b"a", &claim, &stubbed).unwrap_err().kind(), "NotCovered");
    assert_eq!(
        trie_range_verify(&t, c.mode, &c.root, b"a", Some(b"b"), &claim, &stubbed).unwrap_err().kind(),
        "NotCovered"
    );
    let es = under(b"b");
    let claim: Vec<(&[u8], &Value)> = es.iter().map(|(k, v)| (k.as_slice(), v)).collect();
    assert!(trie_prefix_verify(&t, c.mode, &c.root, b"b", &claim, &stubbed).unwrap());
    assert!(trie_range_verify(&t, c.mode, &c.root, b"b", Some(b"c"), &claim, &stubbed).unwrap());
    // a null value in a claim, or a key invalid for the mode, is malformed
    let null = Value::Null;
    assert_eq!(trie_prefix_verify(&t, c.mode, &c.root, b"b", &[(b"b", &null)], &full).unwrap_err().kind(), "Encoding");
    let x = Value::int(1);
    assert_eq!(
        trie_prefix_verify(&t, c.mode, &c.root, b"b", &[(b"bbbbbb", &x)], &full).unwrap_err().kind(),
        "InvalidKey"
    );
}

fn records_of(v: &Value) -> Vec<TrieRecord> {
    TRIE_PROOF.decode(v).unwrap()
}

fn decode(rs: &[TrieRecord]) -> Result<TrieProof<H>, TrieError> {
    TrieProof::<H>::from_records(rs)
}

#[test]
fn malformed_records() {
    let c = sample();
    let full = records_of(&c.proof(&[]));
    assert!(decode(&full).is_ok());
    assert!(decode(&[]).unwrap().is_empty());
    // trailing records, missing children
    let mut trailing = full.clone();
    trailing.push(full[full.len() - 1].clone());
    assert_eq!(decode(&trailing).unwrap_err().kind(), "Encoding");
    assert_eq!(decode(&full[..full.len() - 1]).unwrap_err().kind(), "Encoding");
    // a branch whose mask breaks the invariant: one child, no value
    let bad_mask = full
        .iter()
        .map(|r| match r {
            TrieRecord::TrieBranch(b) if b.c == TrieChildren::Both => {
                TrieRecord::TrieBranch(TrieBranch { c: TrieChildren::Left, v: None, ..b.clone() })
            }
            other => other.clone(),
        })
        .collect::<Vec<_>>();
    assert_eq!(decode(&bad_mask).unwrap_err().kind(), "Encoding");
    // segments: non-zero padding, and len(b) != ceil(n/8)
    let leaf = |n: u64, b: Vec<u8>| vec![TrieRecord::TrieLeaf(TrieLeaf { s: TrieSegment { n, b }, v: vec![0; 32] })];
    assert!(decode(&leaf(3, vec![0b1110_0000])).is_ok());
    assert_eq!(decode(&leaf(3, vec![0b1110_0001])).unwrap_err().kind(), "Encoding");
    assert_eq!(decode(&leaf(3, vec![0b1110_0000, 0])).unwrap_err().kind(), "Encoding");
    assert_eq!(decode(&leaf(9, vec![0xff])).unwrap_err().kind(), "Encoding");
    assert_eq!(decode(&leaf(0, vec![0])).unwrap_err().kind(), "Encoding");
    // digests of the wrong length
    assert_eq!(decode(&[TrieRecord::TrieStub(TrieStub { h: vec![0; 31] })]).unwrap_err().kind(), "Encoding");
    // a record matching none of the shapes, and a non-array
    let odd =
        Value::array(vec![Value::map([("h".into(), Value::bytes(vec![0; 32])), ("s".into(), Value::int(1))]).unwrap()])
            .unwrap();
    assert_eq!(TrieProof::<H>::from_value(&odd).unwrap_err().kind(), "Encoding");
    assert_eq!(TrieProof::<H>::from_value(&Value::int(0)).unwrap_err().kind(), "Encoding");
}

#[test]
fn structural_checks() {
    let t = TrieHashing::<H>::new();
    let fixed2 = KeyMode::fixed(2).unwrap();
    let v = vec![0u8; 32];
    let leaf = |n: u64| {
        TrieRecord::TrieLeaf(TrieLeaf { s: TrieSegment { n, b: vec![0; n.div_ceil(8) as usize] }, v: v.clone() })
    };
    let root = t.empty();
    // a fixed-mode leaf must end exactly at the key length
    for (n, ok) in [(16, true), (15, false), (8, false)] {
        let p = decode(&[leaf(n)]).unwrap();
        assert_eq!(p.verify(&t, fixed2, &root).map(|_| ()).is_ok(), ok, "fixed leaf of {n} bits");
    }
    // a variable-mode leaf ends on a byte boundary within the limit
    let var1 = KeyMode::variable(1).unwrap();
    for (n, ok) in [(0, true), (8, true), (4, false), (16, false)] {
        let p = decode(&[leaf(n)]).unwrap();
        assert_eq!(p.verify(&t, var1, &root).is_ok(), ok, "variable leaf of {n} bits");
    }
    // branch values only in variable mode, on byte boundaries
    let branch = |n: u64, value: bool| {
        TrieRecord::TrieBranch(TrieBranch {
            s: TrieSegment { n, b: vec![0; n.div_ceil(8) as usize] },
            v: value.then(|| v.clone()),
            c: if value { TrieChildren::Left } else { TrieChildren::Both },
        })
    };
    let p = decode(&[branch(8, true), leaf(7)]).unwrap();
    assert_eq!(p.verify(&t, fixed2, &root).unwrap_err().kind(), "InvalidProof");
    assert!(p.verify(&t, KeyMode::variable(2).unwrap(), &root).unwrap().is_none());
    let p = decode(&[branch(7, true), leaf(0)]).unwrap();
    assert_eq!(p.verify(&t, KeyMode::variable(2).unwrap(), &root).unwrap_err().kind(), "InvalidProof");
    // a branch whose children would pass the key length
    let p = decode(&[branch(16, false), leaf(0), leaf(0)]).unwrap();
    assert_eq!(p.verify(&t, fixed2, &root).unwrap_err().kind(), "InvalidProof");
    // a valid two-leaf fixed proof, recomputed
    let p = decode(&[branch(3, false), leaf(12), leaf(12)]).unwrap();
    let r = p.compute_root(&t, fixed2).unwrap();
    assert!(p.verify(&t, fixed2, &r).unwrap().is_some());
}

#[test]
fn tampered_vector_proofs_are_root_mismatches() {
    let v: TrieVectors = load("trie_v1.json");
    let t = TrieHashing::<H>::new();
    for f in &v.fixtures {
        let mode = mode_of(&f.mode);
        let root = d(&f.root);
        let rs = records_of(&cbor::decode(&bytes(&f.proof.cbor)).unwrap());
        for i in 0..rs.len() {
            let mut tampered = rs.clone();
            match &mut tampered[i] {
                TrieRecord::TrieStub(s) => s.h[0] ^= 1,
                TrieRecord::TrieLeaf(l) => l.v[0] ^= 1,
                TrieRecord::TrieBranch(b) => match &mut b.v {
                    Some(x) => x[0] ^= 1,
                    None => continue,
                },
            }
            let p = decode(&tampered).unwrap();
            assert!(p.verify(&t, mode, &root).unwrap().is_none(), "{}: record {i}", f.name);
        }
    }
}

#[test]
fn modes() {
    assert!(KeyMode::fixed(0).is_err());
    assert!(KeyMode::fixed(1025).is_err());
    assert!(KeyMode::fixed(1024).is_ok());
    assert!(KeyMode::variable(65535).is_ok());
    assert_eq!(KeyMode::variable(65536).unwrap_err().kind(), "InvalidKey");
    let m = |k: &str, n: i64| Value::map([(k.into(), Value::int(n))]).unwrap();
    assert_eq!(KeyMode::from_value(&m("fixed", 8)).unwrap(), KeyMode::fixed(8).unwrap());
    assert_eq!(KeyMode::from_value(&m("variable", 256)).unwrap(), KeyMode::variable(256).unwrap());
    assert_eq!(KeyMode::from_value(&m("fixed", 0)).unwrap_err().kind(), "InvalidKey");
    assert_eq!(KeyMode::from_value(&m("fixed", -1)).unwrap_err().kind(), "InvalidKey");
    assert_eq!(KeyMode::from_value(&m("other", 1)).unwrap_err().kind(), "Encoding");
    let _ = TrieRecord::to_value(&TrieRecord::TrieStub(TrieStub { h: vec![] }));
}

#[test]
fn deep_proofs_do_not_recurse() {
    // a variable-mode chain 30000 levels deep: each branch holds a value and one child
    let depth = 30_000u64;
    let t = TrieHashing::<H>::new();
    let mode = KeyMode::variable(65535).unwrap();
    let v = vec![1u8; 32];
    let mut rs = Vec::new();
    for i in 0..depth {
        let n = if i == 0 { 0 } else { 7 };
        rs.push(TrieRecord::TrieBranch(TrieBranch {
            s: TrieSegment { n, b: vec![0; n.div_ceil(8) as usize] },
            v: Some(v.clone()),
            c: TrieChildren::Left,
        }));
    }
    rs.push(TrieRecord::TrieLeaf(TrieLeaf { s: TrieSegment { n: 7, b: vec![0] }, v: v.clone() }));
    let p = decode(&rs).unwrap();
    let root = p.compute_root(&t, mode).unwrap();
    let verified = p.verify(&t, mode, &root).unwrap().unwrap();
    let key = vec![0u8; depth as usize];
    assert_eq!(verified.lookup(&key), Lookup::Present(Digest::from_slice(&v).unwrap()));
    assert_eq!(verified.prefix(&[]).unwrap().len(), depth as usize + 1);
    assert_eq!(verified.range(&[], None).unwrap().len(), depth as usize + 1);
    drop(p);
}

/// Every `malformed` row: decoding refuses the mode-free rules as `Encoding` (the branch invariant among them), and
/// verification against the root the records hash to refuses the mode's rules as `InvalidProof`.
#[test]
fn malformed_vectors() {
    let v: TrieVectors = load("trie_v1.json");
    let t = TrieHashing::<H>::new();
    assert!(v.malformed.len() >= 10, "{} malformed rows", v.malformed.len());
    for c in &v.malformed {
        let kind = TrieProof::<H>::from_value(&cbor::decode(&bytes(&c.cbor)).unwrap())
            .and_then(|p| p.verify(&t, mode_of(&c.mode), &d(&c.root)).map(|_| ()))
            .map_or_else(|e| e.kind(), |()| "accepted");
        let expected = match c.error {
            TrieMalformedError::Encoding => "Encoding",
            TrieMalformedError::InvalidProof => "InvalidProof",
        };
        assert_eq!(kind, expected, "{}", c.name);
    }
}

#[test]
fn listings_of_many_keys_behind_a_long_prefix_are_not_expanded() {
    // a branch with a 60,000-byte segment over a complete subtree of 4,096 leaves: every key is 60,002 bytes, so
    // listing them would build 246 MB from a proof of about 60 KB plus 8,191 records. Comparing a claim streams the
    // walk over one shared path buffer, and the range bounds are compared incrementally
    let t = TrieHashing::<H>::new();
    let mode = KeyMode::variable(65535).unwrap();
    let v = vec![1u8; 32];
    let prefix_bits = 60_000u64 * 8;
    let depth = 12u32;
    // pre-order: a node at depth d < depth is a branch with both children, at depth `depth` a leaf
    let records: Vec<TrieRecord> = (0..(1u32 << (depth + 1)) - 1)
        .scan(Vec::<u32>::new(), |stack, _| {
            // the depth of the next node in pre-order, from the depths of the right children still pending
            let d = stack.pop().unwrap_or(0);
            if d < depth {
                stack.push(d + 1);
                stack.push(d + 1);
            }
            Some(d)
        })
        .map(|d| {
            let n = if d == 0 {
                prefix_bits
            } else if d == depth {
                4
            } else {
                0
            };
            let s = TrieSegment { n, b: vec![0xa5; n.div_ceil(8) as usize] };
            if d == depth {
                TrieRecord::TrieLeaf(TrieLeaf { s: TrieSegment { n: 4, b: vec![0xa0] }, v: v.clone() })
            } else {
                TrieRecord::TrieBranch(TrieBranch { s, v: None, c: TrieChildren::Both })
            }
        })
        .collect();
    let p = decode(&records).unwrap();
    let root = p.compute_root(&t, mode).unwrap();
    let verified = p.verify(&t, mode, &root).unwrap().unwrap();
    let start = std::time::Instant::now();
    assert_eq!(verified.lists_prefix(&[], &Vec::new()), Some(false));
    assert_eq!(verified.lists_range(&[], None, &Vec::new()), Some(false));
    assert_eq!(verified.lists_range(&[0xa5; 100], Some(&[0xa6]), &Vec::new()), Some(false));
    assert_eq!(verified.lists_range(&[0xa6], None, &Vec::new()), Some(true));
    assert!(start.elapsed().as_secs() < 20, "{:?}", start.elapsed());
}
