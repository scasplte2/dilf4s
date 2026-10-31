//! `shared/vectors/merkle_v1.json` under both profiles, and malformed and invalid proofs.

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

use common::{bytes, load};
use dilf4s_core::cbor;
use dilf4s_core::commitment::value_commitment;
use dilf4s_core::generated::merkle_vectors::MerkleVectors;
use dilf4s_core::hash::{Digest, DomainTag, Sha256, TaggedHash};
use dilf4s_core::value::Value;
use dilf4s_proofs::merkle::{
    self, ConsistencyProof, InclusionProof, MerkleError, MerkleHashing, Rfc6962, Tagged, TreeHead,
    merkle_consistency_verify, merkle_verify, verify_consistency, verify_inclusion,
};

type H = Sha256;

fn d(hex: &str) -> Digest<H> {
    Digest::from_slice(&bytes(hex)).unwrap()
}

fn value(hex: &str) -> Value {
    cbor::decode(&bytes(hex)).unwrap()
}

/// The tagged profile's leaf hash of raw bytes, as the vectors' opaque leaves use it (`MerkleHashing.leaf`). The
/// library has no such API; core's tagged hash computes it here.
fn raw_tagged_leaf(data: &[u8]) -> Digest<H> {
    TaggedHash::<H>::new(DomainTag::new(merkle::LEAF_TAG).unwrap()).hash(data)
}

/// RFC 9162 §2.1.3.1 audit path, from levels of node hashes.
fn audit_path<M: MerkleHashing<H>>(profile: &M, leaves: &[Digest<H>], index: usize) -> Vec<Digest<H>> {
    let mut levels = vec![leaves.to_vec()];
    while levels.last().unwrap().len() > 1 {
        let next = levels
            .last()
            .unwrap()
            .chunks(2)
            .map(|p| if p.len() == 2 { profile.node_hash(&p[0], &p[1]) } else { p[0].clone() })
            .collect();
        levels.push(next);
    }
    let mut path = Vec::new();
    let mut i = index;
    for level in &levels[..levels.len() - 1] {
        if i % 2 == 1 {
            path.push(level[i - 1].clone());
        } else if i + 1 < level.len() {
            path.push(level[i + 1].clone());
        }
        i /= 2;
    }
    path
}

#[test]
fn tags_are_domain_tags_and_match_core() {
    for tag in [merkle::LEAF_TAG, merkle::NODE_TAG, merkle::EMPTY_TAG] {
        DomainTag::new(tag).unwrap();
    }
    let t = Tagged::<H>::new();
    let empty = TaggedHash::<H>::new(DomainTag::new(merkle::EMPTY_TAG).unwrap()).hash(&[]);
    assert_eq!(t.empty_hash(), empty);
    let (a, b) = (d(&"11".repeat(32)), d(&"22".repeat(32)));
    let node =
        TaggedHash::<H>::new(DomainTag::new(merkle::NODE_TAG).unwrap()).hash(&[a.as_bytes(), b.as_bytes()].concat());
    assert_eq!(t.node_hash(&a, &b), node);
}

#[test]
fn vectors_under_both_profiles() {
    let v: MerkleVectors = load("merkle_v1.json");
    let tagged = Tagged::<H>::new();
    let rfc = Rfc6962::<H>::new();
    let leaves: Vec<Vec<u8>> = v.leaves.iter().map(|l| bytes(l)).collect();
    let leaf_hashes: Vec<Digest<H>> = v.leaf_hashes.iter().map(|h| d(h)).collect();
    let rfc_hashes: Vec<Digest<H>> = leaves.iter().map(|l| rfc.leaf_hash(l)).collect();
    for (l, h) in leaves.iter().zip(&leaf_hashes) {
        assert_eq!(&raw_tagged_leaf(l), h, "leaf hash of {}", dilf4s_core::hex::encode(l));
    }
    let (mut inclusions, mut consistencies) = (0, 0);
    for t in &v.trees {
        let n = t.size as usize;
        assert_eq!(merkle::root(&tagged, &leaf_hashes[..n]), d(&t.root), "tagged root of size {n}");
        assert_eq!(merkle::root(&rfc, &rfc_hashes[..n]), d(&t.rfc6962_root), "rfc6962 root of size {n}");
        let tagged_head = TreeHead { root: d(&t.root), size: t.size };
        let rfc_head = TreeHead { root: d(&t.rfc6962_root), size: t.size };
        assert_eq!(t.inclusion.len(), n);
        assert_eq!(t.rfc6962_inclusion.len(), n);
        for i in 0..n {
            let p = InclusionProof::<H>::from_value(&value(&t.inclusion[i])).unwrap();
            assert_eq!((p.index(), p.size()), (i as u64, t.size));
            assert!(verify_inclusion(&tagged, &tagged_head.root, &leaf_hashes[i], &p).unwrap(), "tagged {i} of {n}");
            assert_eq!(p.to_value().unwrap(), value(&t.inclusion[i]), "re-encoding");
            let rp = value(&t.rfc6962_inclusion[i]);
            assert!(merkle_verify(&rfc, &rfc_head, leaves[i].as_slice(), &rp).unwrap(), "rfc6962 {i} of {n}");
            // another leaf, and the other profile's head, are false
            let other = (i + 1) % n;
            if leaves[other] != leaves[i] {
                assert!(!merkle_verify(&rfc, &rfc_head, leaves[other].as_slice(), &rp).unwrap());
            }
            assert!(!verify_inclusion(&tagged, &rfc_head.root, &leaf_hashes[i], &p).unwrap());
            inclusions += 2;
        }
        assert_eq!(t.consistency.len(), n + 1);
        assert_eq!(t.rfc6962_consistency.len(), n + 1);
        for m in 0..=n {
            let old = &v.trees[m];
            let p = ConsistencyProof::<H>::from_value(&value(&t.consistency[m])).unwrap();
            assert!(verify_consistency(&tagged, &d(&old.root), &d(&t.root), &p).unwrap(), "tagged {m} -> {n}");
            let old_head = TreeHead { root: d(&old.rfc6962_root), size: old.size };
            let rp = value(&t.rfc6962_consistency[m]);
            assert!(merkle_consistency_verify(&rfc, &old_head, &rfc_head, &rp).unwrap(), "rfc6962 {m} -> {n}");
            if m > 0 && m < n {
                // the wrong old root is false
                assert!(!verify_consistency(&tagged, &d(&t.root), &d(&t.root), &p).unwrap());
            }
            consistencies += 2;
        }
    }
    eprintln!("merkle: {} trees, {inclusions} inclusion and {consistencies} consistency checks", v.trees.len());
}

#[test]
fn value_commitments() {
    let v: MerkleVectors = load("merkle_v1.json");
    let tagged = Tagged::<H>::new();
    for c in &v.value_commitments {
        let x = value(&c.cbor);
        assert_eq!(dilf4s_core::hex::encode(&value_commitment(&x)), c.commitment);
        assert_eq!(tagged.leaf_hash(&x), d(&c.leaf_hash));
    }
    eprintln!("merkle: {} value commitments", v.value_commitments.len());
}

#[test]
fn value_leaves_through_the_jlvm_helper() {
    let tagged = Tagged::<H>::new();
    let values: Vec<Value> =
        vec![Value::bytes(vec![0x61, 0x61]), Value::text("a"), Value::int(7), Value::Null, Value::bytes(vec![])];
    let hashes: Vec<Digest<H>> = values.iter().map(|x| tagged.leaf_hash(x)).collect();
    let head = TreeHead { root: merkle::root(&tagged, &hashes), size: values.len() as u64 };
    for (i, x) in values.iter().enumerate() {
        let p = InclusionProof::new(i as u64, head.size, audit_path(&tagged, &hashes, i)).unwrap().to_value().unwrap();
        assert!(merkle_verify(&tagged, &head, x, &p).unwrap());
        // the bytes 0x6161 never verify the text "a", whose CBOR is 0x6161
        if i == 0 {
            assert!(!merkle_verify(&tagged, &head, &Value::text("a"), &p).unwrap());
        }
        // a head of another size is false, even with the right root
        let wrong = TreeHead { root: head.root.clone(), size: head.size + 1 };
        assert!(!merkle_verify(&tagged, &wrong, x, &p).unwrap());
    }
}

fn first_tree_with_size(v: &MerkleVectors, n: u64) -> &dilf4s_core::generated::merkle_vectors::MerkleVectorTree {
    v.trees.iter().find(|t| t.size == n).unwrap()
}

#[test]
fn tampered_and_malformed_proofs() {
    let v: MerkleVectors = load("merkle_v1.json");
    let tagged = Tagged::<H>::new();
    let t = first_tree_with_size(&v, 7);
    let leaf = d(&v.leaf_hashes[3]);
    let good = InclusionProof::<H>::from_value(&value(&t.inclusion[3])).unwrap();
    let root = d(&t.root);

    // a flipped path byte, a dropped element and an extra element are well formed but false
    let mut path = good.path().to_vec();
    let mut b = path[0].as_bytes().to_vec();
    b[0] ^= 1;
    path[0] = Digest::from_slice(&b).unwrap();
    let flipped = InclusionProof::new(3, 7, path).unwrap();
    assert!(!verify_inclusion(&tagged, &root, &leaf, &flipped).unwrap());
    assert!(matches!(merkle::check_inclusion(&tagged, &root, &leaf, &flipped), Err(MerkleError::RootMismatch { .. })));
    let short = InclusionProof::new(3, 7, good.path()[1..].to_vec()).unwrap();
    assert!(matches!(merkle::check_inclusion(&tagged, &root, &leaf, &short), Err(MerkleError::InvalidProof { .. })));
    assert!(!verify_inclusion(&tagged, &root, &leaf, &short).unwrap());
    let mut longer = good.path().to_vec();
    longer.push(leaf.clone());
    let long = InclusionProof::new(3, 7, longer).unwrap();
    assert!(!verify_inclusion(&tagged, &root, &leaf, &long).unwrap());

    // malformed: index outside the tree, a digest of the wrong length, the wrong shape
    let mut m = match value(&t.inclusion[3]) {
        Value::Map(m) => m.into_entries(),
        _ => panic!(),
    };
    for (k, x) in &mut m {
        if k == "index" {
            *x = Value::int(7);
        }
    }
    let out_of_range = Value::map(m.clone()).unwrap();
    let e = InclusionProof::<H>::from_value(&out_of_range).unwrap_err();
    assert_eq!(e.kind(), "IndexOutOfRange");
    let head = TreeHead { root: root.clone(), size: 7 };
    assert_eq!(
        merkle_verify(&Rfc6962::<H>::new(), &head, &[][..], &out_of_range).unwrap_err().kind(),
        "IndexOutOfRange"
    );
    for (k, x) in &mut m {
        if k == "index" {
            *x = Value::int(3);
        }
        if k == "path" {
            *x = Value::array(vec![Value::bytes(vec![0; 31])]).unwrap();
        }
    }
    assert_eq!(InclusionProof::<H>::from_value(&Value::map(m).unwrap()).unwrap_err().kind(), "Encoding");
    assert_eq!(InclusionProof::<H>::from_value(&Value::int(1)).unwrap_err().kind(), "Encoding");
    assert_eq!(InclusionProof::<H>::new(0, 0, vec![]).unwrap_err().kind(), "IndexOutOfRange");

    // consistency: old > new is IndexOutOfRange at decode (as Scala); the empty-tree rules; a wrong-size head is false
    let c = value(&t.consistency[3]);
    let mut cm = match &c {
        Value::Map(m) => m.clone().into_entries(),
        _ => panic!(),
    };
    for (k, x) in &mut cm {
        if k == "old" {
            *x = Value::int(8);
        }
    }
    assert_eq!(ConsistencyProof::<H>::from_value(&Value::map(cm).unwrap()).unwrap_err().kind(), "IndexOutOfRange");
    let zero = ConsistencyProof::<H>::new(0, 7, vec![]).unwrap();
    assert!(verify_consistency(&tagged, &tagged.empty_hash(), &root, &zero).unwrap());
    assert!(!verify_consistency(&tagged, &root, &root, &zero).unwrap());
    let old = first_tree_with_size(&v, 3);
    let old_head = TreeHead { root: d(&old.root), size: 3 };
    let wrong_new = TreeHead { root: root.clone(), size: 8 };
    assert!(merkle_consistency_verify(&tagged, &old_head, &TreeHead { root: root.clone(), size: 7 }, &c).unwrap());
    assert!(!merkle_consistency_verify(&tagged, &old_head, &wrong_new, &c).unwrap());
}

#[test]
fn heads_decode_from_values() {
    let root = vec![7u8; 32];
    let ok = Value::map([("root".into(), Value::bytes(root.clone())), ("size".into(), Value::int(5))]).unwrap();
    let head = TreeHead::<H>::from_value(&ok).unwrap();
    assert_eq!((head.root.as_bytes(), head.size), (root.as_slice(), 5));
    let negative = Value::map([("root".into(), Value::bytes(root.clone())), ("size".into(), Value::int(-1))]).unwrap();
    assert_eq!(TreeHead::<H>::from_value(&negative).unwrap_err().kind(), "Encoding");
    let extra =
        Value::map([("root".into(), Value::bytes(root)), ("size".into(), Value::int(1)), ("x".into(), Value::int(1))])
            .unwrap();
    assert_eq!(TreeHead::<H>::from_value(&extra).unwrap_err().kind(), "Encoding");
}
