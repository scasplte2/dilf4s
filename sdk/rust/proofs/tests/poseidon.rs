//! `shared/vectors/poseidon_v1.json`, the constant tables (pinned digests and a Grain LFSR derivation), the two
//! evaluation forms, and the Poseidon Merkle tree. The crate ships only the optimised form; the base form (the round
//! constants and the rounds as defined) is derived here, as Scala's `PoseidonDerivation` does, to check it.

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
use std::str::FromStr;
use std::sync::LazyLock;

use common::load;
use dilf4s_core::generated::poseidon_vectors::PoseidonVectors;
use dilf4s_core::hash::{Digest, Hasher, Sha256};
use dilf4s_core::value::Value;
use dilf4s_proofs::poseidon::merkle::{
    PoseidonMerkleProof, compute_root, verify_absence, verify_inclusion, zero_hashes,
};
use dilf4s_proofs::poseidon::{
    self, FULL_ROUNDS, Fr, Params, PoseidonError, compress, constants, fr_from_be_bytes, fr_from_int, fr_to_be_bytes,
    fr_to_int, hash,
};
use num_bigint::{BigInt, BigUint};

fn fr(s: &str) -> Fr {
    fr_from_int(&BigInt::from_str(s).unwrap()).unwrap()
}

fn dec(x: Fr) -> String {
    fr_to_int(x).to_string()
}

fn r() -> BigUint {
    BigUint::from_str("21888242871839275222246405745257275088548364400416034343698204186575808495617").unwrap()
}

#[test]
fn hash_vectors() {
    let v: PoseidonVectors = load("poseidon_v1.json");
    for c in &v.poseidon {
        let inputs: Vec<Fr> = c.inputs.iter().map(|s| fr(s)).collect();
        assert_eq!(dec(hash(&inputs).unwrap()), c.output, "{:?}", c.inputs);
        assert_eq!(dec(hash_reference(&inputs)), c.output, "{:?} reference", c.inputs);
    }
    // circomlibjs known answers from Scala's PoseidonSuite
    let rm1 = fr_from_int(&BigInt::from(r() - 1u32)).unwrap();
    assert_eq!(
        dec(poseidon::hash1(rm1)),
        "3366645945435192953002076803303112651887535928162668198103357554665518664470"
    );
    assert_eq!(
        dec(hash(&[fr("1"), fr("2")]).unwrap()),
        "7853200120776062878684798364095072458815029376092732009249414926327459813530"
    );
    assert_eq!(hash(&[]).unwrap_err(), PoseidonError::InputCount { count: 0, max: 4 });
    assert_eq!(hash(&[fr("1"); 5]).unwrap_err(), PoseidonError::InputCount { count: 5, max: 4 });
    assert_eq!(compress(fr("1"), fr("2")), poseidon::hash2(fr("1"), fr("2")));
    eprintln!("poseidon: {} hash vectors", v.poseidon.len());
}

#[test]
fn optimised_and_reference_forms_agree() {
    // a deterministic spread of inputs: small, large and edge values
    let mut seed = Sha256::hash_parts(&[b"poseidon"]);
    let mut next = || {
        seed = Sha256::hash_parts(&[&seed]);
        let mut b = seed;
        b[0] &= 0x1f;
        fr_from_be_bytes(&b).unwrap()
    };
    let edges = [fr("0"), fr("1"), fr_from_int(&BigInt::from(r() - 1u32)).unwrap()];
    for n in 1..=4 {
        for round in 0..20 {
            let inputs: Vec<Fr> = (0..n).map(|i| if round < 3 { edges[(round + i) % 3] } else { next() }).collect();
            assert_eq!(hash(&inputs).unwrap(), hash_reference(&inputs), "{n} inputs, case {round}");
        }
    }
}

/// The digest over the base round constants and the shipped tables, in `PoseidonDerivation.digest`'s order.
fn digest_of<const T: usize, const RP: usize, const N: usize>(base: &Base<T>, p: &Params<T, RP, N>) -> String {
    let join = |xs: Vec<Fr>| xs.into_iter().map(dec).collect::<Vec<_>>().join(",");
    let flat = |m: &[[Fr; T]]| m.iter().flatten().copied().collect::<Vec<_>>();
    let mut constants: Vec<Fr> = p.first.to_vec();
    constants.extend(flat(&p.full_first));
    constants.extend(p.opened);
    constants.extend(p.partial);
    constants.extend(flat(&p.full_last));
    let sparse: Vec<Fr> = p.sparse_w.iter().zip(&p.sparse_v).flat_map(|(w, v)| w.iter().chain(v).copied()).collect();
    let tables = [flat(&base.round_constants), flat(&p.mds), constants, sparse, flat(&p.mds), flat(&p.pre_sparse)];
    let text = tables.into_iter().map(join).collect::<Vec<_>>().join(";");
    dilf4s_core::hex::encode(Digest::<Sha256>::of(text.as_bytes()).as_bytes())
}

/// The digests of circomlibjs's tables at commit 48b3ab37, as Scala's `PoseidonSuite` pins them.
#[test]
fn tables_match_the_pinned_circomlibjs_digests() {
    let b = &*BASE;
    assert_eq!(
        digest_of(&b.w2, &constants::WIDTH2),
        "d5e58f509cc3494297ac43fd51badea46b75bcc505b18cd31dffa274fe154149"
    );
    assert_eq!(
        digest_of(&b.w3, &constants::WIDTH3),
        "38e80297a0e0ecd5a20acc1cf519eb5eb898e924e2ae68808b49c7027e7e0b94"
    );
    assert_eq!(
        digest_of(&b.w4, &constants::WIDTH4),
        "6bc25738e42a0b5a18ca2b5d262ce7da4f3aa029bebf1090a08f4a466ce313b6"
    );
    assert_eq!(
        digest_of(&b.w5, &constants::WIDTH5),
        "fb21c20f55dc308123237c06f8838aa22772201c518746bb2ca699f0a4b7530e"
    );
}

// ---- the Grain LFSR derivation (zk.md §3, Constants)

struct Grain(Vec<u8>);

impl Grain {
    fn new(t: usize, rp: usize) -> Self {
        let field = |value: usize, width: usize| (0..width).rev().map(move |i| ((value >> i) & 1) as u8);
        let mut reg: Vec<u8> = field(1, 2).chain(field(0, 4)).chain(field(254, 12)).chain(field(t, 12)).collect();
        reg.extend(field(8, 10).chain(field(rp, 10)));
        reg.extend([1u8; 30]);
        assert_eq!(reg.len(), 80);
        let mut g = Self(reg);
        for _ in 0..160 {
            g.step();
        }
        g
    }

    fn step(&mut self) -> u8 {
        let b = &self.0;
        let new = b[62] ^ b[51] ^ b[38] ^ b[23] ^ b[13] ^ b[0];
        self.0.remove(0);
        self.0.push(new);
        new
    }

    fn bit(&mut self) -> u8 {
        loop {
            let (a, b) = (self.step(), self.step());
            if a == 1 {
                return b;
            }
        }
    }

    fn bits(&mut self) -> BigUint {
        (0..254).fold(BigUint::from(0u32), |v, _| (v << 1u32) + BigUint::from(self.bit()))
    }
}

/// One width's base form: round constants per round and the MDS matrix, as the Grain LFSR yields them.
struct Base<const T: usize> {
    round_constants: Vec<[Fr; T]>,
    mds: [[Fr; T]; T],
}

impl<const T: usize> Base<T> {
    /// Derived for the width and partial rounds of `_shipped`.
    fn derive<const RP: usize, const N: usize>(_shipped: &Params<T, RP, N>) -> Self {
        let modulus = r();
        let element = |v: &BigUint| fr_from_int(&BigInt::from(v.clone())).unwrap();
        let mut g = Grain::new(T, RP);
        let mut rc = Vec::new();
        while rc.len() < (FULL_ROUNDS + RP) * T {
            let v = g.bits();
            if v < modulus {
                rc.push(element(&v));
            }
        }
        let points: Vec<BigUint> = (0..2 * T).map(|_| g.bits() % &modulus).collect();
        let mds = std::array::from_fn(|i| {
            std::array::from_fn(|j| {
                let sum = (&points[i] + &points[T + j]) % &modulus;
                element(&sum.modpow(&(&modulus - 2u32), &modulus))
            })
        });
        let round_constants = rc.as_chunks::<T>().0.to_vec();
        Self { round_constants, mds }
    }

    /// The permutation as defined: each round adds its constants, applies the S-box to every element (full rounds)
    /// or to element 0 (partial rounds), and multiplies by the MDS matrix.
    fn permute(&self, state: [Fr; T]) -> [Fr; T] {
        let half = FULL_ROUNDS / 2;
        let partial = half..self.round_constants.len() - half;
        let pow5 = |x: Fr| x * x * x * x * x;
        self.round_constants.iter().enumerate().fold(state, |s, (r, c)| {
            let added: [Fr; T] = std::array::from_fn(|i| s[i] + c[i]);
            let boxed: [Fr; T] =
                std::array::from_fn(|i| if i == 0 || !partial.contains(&r) { pow5(added[i]) } else { added[i] });
            std::array::from_fn(|i| (0..T).map(|j| self.mds[i][j] * boxed[j]).sum())
        })
    }

    /// `poseidon(inputs)`, over the state `[0, x1, ..., xn]`.
    fn hash(&self, inputs: &[Fr]) -> Fr {
        let state = std::array::from_fn(|i| if i == 0 { fr("0") } else { inputs[i - 1] });
        self.permute(state)[0]
    }
}

struct Bases {
    w2: Base<2>,
    w3: Base<3>,
    w4: Base<4>,
    w5: Base<5>,
}

static BASE: LazyLock<Bases> = LazyLock::new(|| Bases {
    w2: Base::derive(&constants::WIDTH2),
    w3: Base::derive(&constants::WIDTH3),
    w4: Base::derive(&constants::WIDTH4),
    w5: Base::derive(&constants::WIDTH5),
});

/// `poseidon(inputs)` through the rounds as defined, over the derived base form, for 1 to 4 inputs.
fn hash_reference(inputs: &[Fr]) -> Fr {
    let b = &*BASE;
    match inputs.len() {
        1 => b.w2.hash(inputs),
        2 => b.w3.hash(inputs),
        3 => b.w4.hash(inputs),
        4 => b.w5.hash(inputs),
        n => panic!("{n} inputs"),
    }
}

#[test]
fn grain_lfsr_derives_the_mds_tables() {
    let b = &*BASE;
    assert_eq!(b.w2.mds, constants::WIDTH2.mds, "width 2");
    assert_eq!(b.w3.mds, constants::WIDTH3.mds, "width 3");
    assert_eq!(b.w4.mds, constants::WIDTH4.mds, "width 4");
    assert_eq!(b.w5.mds, constants::WIDTH5.mds, "width 5");
    let rounds = [b.w2.round_constants.len(), b.w3.round_constants.len(), b.w4.round_constants.len()];
    assert_eq!(rounds, [64, 65, 64]);
    assert_eq!(b.w5.round_constants.len(), 68);
}

#[test]
fn field_elements_are_canonical() {
    let r_bytes = {
        let mut b = [0u8; 32];
        let rb = r().to_bytes_be();
        b[32 - rb.len()..].copy_from_slice(&rb);
        b
    };
    assert_eq!(fr_from_be_bytes(&r_bytes).unwrap_err(), PoseidonError::NonCanonical);
    assert_eq!(fr_from_be_bytes(&[0u8; 31]).unwrap_err(), PoseidonError::NonCanonical);
    assert_eq!(fr_from_int(&BigInt::from(-1)).unwrap_err(), PoseidonError::NonCanonical);
    assert_eq!(fr_from_int(&BigInt::from(r())).unwrap_err(), PoseidonError::NonCanonical);
    let x = fr("123456789");
    assert_eq!(fr_from_be_bytes(&fr_to_be_bytes(x)).unwrap(), x);
}

/// The root of the sparse tree of `depth` holding `leaves`, rebuilt level by level with the zero hashes.
fn sparse_root(depth: usize, leaves: &BTreeMap<BigUint, Fr>) -> Fr {
    let zeros = zero_hashes(depth);
    let mut level: BTreeMap<BigUint, Fr> = leaves.clone();
    for z in zeros.iter().take(depth) {
        let mut up: BTreeMap<BigUint, Fr> = BTreeMap::new();
        for p in level.keys() {
            let parent = p >> 1u32;
            if up.contains_key(&parent) {
                continue;
            }
            let l = level.get(&(&parent << 1u32)).copied().unwrap_or(*z);
            let r = level.get(&((&parent << 1u32) + 1u32)).copied().unwrap_or(*z);
            up.insert(parent, compress(l, r));
        }
        level = up;
    }
    level.get(&BigUint::from(0u32)).copied().unwrap_or(zeros[depth])
}

/// zk.md section 4: entries apply in order, a later entry for a position replacing an earlier one and a later 0
/// emptying it.
#[test]
fn build_vectors() {
    let v: PoseidonVectors = load("poseidon_v1.json");
    assert_eq!(v.builds.len(), 5);
    for b in &v.builds {
        let depth = usize::try_from(b.depth).unwrap();
        let mut leaves: BTreeMap<BigUint, Fr> = BTreeMap::new();
        for e in &b.entries {
            let position = BigUint::from_str(&e.position).unwrap();
            let leaf = fr(&e.leaf);
            if leaf == fr("0") {
                leaves.remove(&position);
            } else {
                leaves.insert(position, leaf);
            }
        }
        assert_eq!(sparse_root(depth, &leaves), fr(&b.root), "{:?}", b.entries);
    }
}

#[test]
fn merkle_vectors() {
    let v: PoseidonVectors = load("poseidon_v1.json");
    let m = &v.merkle;
    let depth = usize::try_from(m.depth).unwrap();
    let root = fr(&m.root);
    let leaves: BTreeMap<BigUint, Fr> = m.leaves.iter().map(|(p, x)| (BigUint::from_str(p).unwrap(), fr(x))).collect();

    let zeros = zero_hashes(depth);
    assert_eq!(sparse_root(depth, &leaves), root);

    for p in &m.proofs {
        let position = BigUint::from_str(&p.position).unwrap();
        let siblings: Vec<Fr> = p.siblings.iter().map(|s| fr(s)).collect();
        let proof = PoseidonMerkleProof::new(position.clone(), siblings).unwrap();
        let value = proof.to_value().unwrap();
        assert_eq!(PoseidonMerkleProof::from_value(&value).unwrap(), proof);
        if let Some(leaf) = leaves.get(&position) {
            assert!(verify_inclusion(*leaf, &proof, root), "position {position}");
            assert!(!verify_absence(&proof, root));
            assert!(!verify_inclusion(*leaf + fr("1"), &proof, root));
        } else {
            assert!(verify_absence(&proof, root), "position {position} empty");
            assert!(!verify_inclusion(fr("1"), &proof, root));
        }
        assert_ne!(compute_root(fr("0"), &proof), compute_root(fr("5"), &proof));
    }
    // an empty tree's root is the top zero hash
    let empty = PoseidonMerkleProof::new(BigUint::from(3u32), zeros[..depth].iter().rev().copied().collect()).unwrap();
    assert!(verify_absence(&empty, zeros[depth]));
    eprintln!("poseidon: merkle depth {depth}, {} proofs", m.proofs.len());
}

#[test]
fn malformed_merkle_proofs() {
    let s = fr("7");
    assert_eq!(PoseidonMerkleProof::new(BigUint::from(4u32), vec![s, s]).unwrap_err().kind(), "Position");
    assert!(PoseidonMerkleProof::new(BigUint::from(3u32), vec![s, s]).is_ok());
    assert!(PoseidonMerkleProof::new(BigUint::from(0u32), vec![]).is_ok());
    assert_eq!(PoseidonMerkleProof::new(BigUint::from(1u32), vec![]).unwrap_err().kind(), "Position");
    let wire = |position: BigInt, siblings: Vec<BigInt>| {
        Value::map([
            ("position".into(), Value::int(position)),
            ("siblings".into(), Value::array(siblings.into_iter().map(Value::int).collect()).unwrap()),
        ])
        .unwrap()
    };
    assert_eq!(
        PoseidonMerkleProof::from_value(&wire(BigInt::from(2), vec![BigInt::from(1)])).unwrap_err().kind(),
        "Position"
    );
    assert_eq!(PoseidonMerkleProof::from_value(&wire(BigInt::from(-1), vec![])).unwrap_err().kind(), "Encoding");
    assert_eq!(
        PoseidonMerkleProof::from_value(&wire(BigInt::from(0), vec![BigInt::from(r())])).unwrap_err().kind(),
        "Encoding"
    );
    assert_eq!(PoseidonMerkleProof::from_value(&Value::int(0)).unwrap_err().kind(), "Encoding");
    let deep = vec![BigInt::from(0); 257];
    assert_eq!(PoseidonMerkleProof::from_value(&wire(BigInt::from(0), deep)).unwrap_err().kind(), "Encoding");
    assert_eq!(PoseidonMerkleProof::new(BigUint::from(0u32), vec![s; 257]).unwrap_err().kind(), "Position");
}

#[test]
fn a_refused_position_is_shown_by_its_size_never_its_digits() {
    let e = PoseidonMerkleProof::new(BigUint::from(1u8) << 1_000_000u32, Vec::new()).unwrap_err();
    assert_eq!(e.to_string(), "a position of 1000001 bits is out of range for depth 0");
    assert_eq!(format!("{e:?}"), "Position { position_bits: 1000001, depth: 0 }");
}
