//! Key-evolving signatures (`kes.md`): the dilf4s sum and product compositions over any hash with at least 32 bytes
//! of output ([`KesSum`], [`KesProduct`]), and Cardano's `SumKES Blake2b_256` ([`CardanoSumKes`]).
//!
//! Verification returns `bool`; malformed sizes and out-of-range periods are [`KesError`]s. Key generation,
//! evolution, signing and loading need the `sign` feature.

use alloc::format;
use alloc::vec::Vec;

use dilf4s_core::hash::HashAlgorithm;

use crate::ed25519;
use crate::error::KesError;

#[cfg(feature = "sign")]
const SEED_TAG: &str = "dilf4s/kes/seed/v1";
const LEAF_TAG: &str = "dilf4s/kes/leaf/v1";
const NODE_TAG: &str = "dilf4s/kes/node/v1";
const PRODUCT_TAG: &str = "dilf4s/kes/product/v1";

/// The largest sum-tree height (and super + sub height of a product).
pub const MAX_HEIGHT: u32 = 30;

/// `taggedHash_H(tag, parts...) = H(H(tag) || H(tag) || parts...)` (`canonical-encoding.md` section 4).
fn tagged(hash: HashAlgorithm, tag: &str, parts: &[&[u8]]) -> Vec<u8> {
    let t = hash.digest(tag.as_bytes());
    let mut all: Vec<&[u8]> = Vec::with_capacity(parts.len().saturating_add(2));
    all.push(&t);
    all.push(&t);
    all.extend_from_slice(parts);
    hash.hash_parts(&all)
}

/// `2^(d-1)`, the periods of each half of a height-`d` tree (`d >= 1`).
fn half(d: u32) -> u64 {
    1u64.checked_shl(d.saturating_sub(1)).unwrap_or(0)
}

/// `2^h`.
fn periods(h: u32) -> u64 {
    1u64.checked_shl(h).unwrap_or(0)
}

fn bit(period: u64, level: u32) -> bool {
    period.checked_shr(level).is_some_and(|p| p & 1 == 1)
}

fn malformed(what: &'static str, expected: usize, got: usize) -> KesError {
    KesError::Malformed { what, reason: format!("expected {expected} bytes, got {got}") }
}

fn check_period(period: u64, total: u64) -> Result<(), KesError> {
    if period < total { Ok(()) } else { Err(KesError::PeriodOutOfRange { period, total }) }
}

/// The Merkle-tree algorithms over one hash, shared by the sum and product compositions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Ops {
    hash: HashAlgorithm,
}

impl Ops {
    fn n(self) -> usize {
        self.hash.output_size()
    }

    fn leaf(self, vk: &[u8]) -> Vec<u8> {
        tagged(self.hash, LEAF_TAG, &[vk])
    }

    fn node(self, l: &[u8], r: &[u8]) -> Vec<u8> {
        tagged(self.hash, NODE_TAG, &[l, r])
    }

    fn product_message(self, sub_root: &[u8]) -> Vec<u8> {
        tagged(self.hash, PRODUCT_TAG, &[sub_root])
    }

    fn signature_length(self, d: u32) -> usize {
        self.n().saturating_mul(usize::try_from(d).unwrap_or(usize::MAX)).saturating_add(96)
    }

    /// `vk_leaf || sig || w_0 .. w_{h-1}` against the root at `period`.
    fn verify(self, root: &[u8], d: u32, period: u64, message: &[u8], signature: &[u8]) -> bool {
        if signature.len() != self.signature_length(d) || period >= periods(d) {
            return false;
        }
        let (head, witnesses) = signature.split_at(96);
        let (vk, leaf_sig) = head.split_at(32);
        let computed = witnesses.chunks(self.n()).zip(0u32..).fold(self.leaf(vk), |acc, (w, level)| {
            if bit(period, level) { self.node(w, &acc) } else { self.node(&acc, w) }
        });
        computed == root && ed25519::verify_bytes(vk, message, leaf_sig) == Ok(true)
    }
}

fn hash_ok(hash: HashAlgorithm) -> Result<(), KesError> {
    if hash.output_size() >= 32 {
        Ok(())
    } else {
        Err(KesError::Malformed {
            what: "KES hash",
            reason: format!("{} has a {}-byte output; at least 32 required", hash.name(), hash.output_size()),
        })
    }
}

/// The dilf4s sum composition over a hash: `2^height` periods, signatures `vk || Ed25519 sig || Merkle path`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct KesSum {
    ops: Ops,
    height: u32,
}

impl KesSum {
    /// The composition of `height` (at most 30) over `hash` (at least 32 bytes of output).
    pub fn new(hash: HashAlgorithm, height: u32) -> Result<Self, KesError> {
        hash_ok(hash)?;
        if height > MAX_HEIGHT {
            return Err(KesError::Malformed { what: "KES height", reason: format!("expected 0..30, got {height}") });
        }
        Ok(Self { ops: Ops { hash }, height })
    }

    /// The height.
    pub const fn height(&self) -> u32 {
        self.height
    }

    /// `2^height`.
    pub fn total_periods(&self) -> u64 {
        periods(self.height)
    }

    /// The verifying key length (the hash's output size).
    pub fn verifying_key_length(&self) -> usize {
        self.ops.n()
    }

    /// `96 + n h`.
    pub fn signature_length(&self) -> usize {
        self.ops.signature_length(self.height)
    }

    /// `64 + (32 + 2n) h`.
    pub fn key_length(&self) -> usize {
        encoded_length(self.ops, self.height)
    }

    /// Verification of `signature` on `message` at `period` under the root `vk`.
    pub fn verify(&self, vk: &[u8], period: u64, message: &[u8], signature: &[u8]) -> Result<bool, KesError> {
        if vk.len() != self.ops.n() {
            return Err(malformed("KES verifying key", self.ops.n(), vk.len()));
        }
        if signature.len() != self.signature_length() {
            return Err(malformed("KES signature", self.signature_length(), signature.len()));
        }
        check_period(period, self.total_periods())?;
        Ok(self.ops.verify(vk, self.height, period, message, signature))
    }
}

fn encoded_length(ops: Ops, d: u32) -> usize {
    ops.n()
        .saturating_mul(2)
        .saturating_add(32)
        .saturating_mul(usize::try_from(d).unwrap_or(usize::MAX))
        .saturating_add(64)
}

/// The dilf4s product composition: a super sum tree of height `hs` certifies `2^hs` successive sub trees of height
/// `hb`, for `2^(hs + hb)` periods. Signatures are `superSig || subRoot || subSig`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct KesProduct {
    ops: Ops,
    super_height: u32,
    sub_height: u32,
}

impl KesProduct {
    /// The composition of a super tree of `super_height` and sub trees of `sub_height` (summing to at most 30) over
    /// `hash` (at least 32 bytes of output).
    pub fn new(hash: HashAlgorithm, super_height: u32, sub_height: u32) -> Result<Self, KesError> {
        hash_ok(hash)?;
        if super_height.saturating_add(sub_height) > MAX_HEIGHT {
            return Err(KesError::Malformed {
                what: "KES heights",
                reason: format!("expected heights summing to at most 30, got ({super_height}, {sub_height})"),
            });
        }
        Ok(Self { ops: Ops { hash }, super_height, sub_height })
    }

    /// `2^(hs + hb)`.
    pub fn total_periods(&self) -> u64 {
        periods(self.super_height.saturating_add(self.sub_height))
    }

    fn sub_periods(&self) -> u64 {
        periods(self.sub_height)
    }

    /// The verifying key length (the hash's output size).
    pub fn verifying_key_length(&self) -> usize {
        self.ops.n()
    }

    /// `superSig || subRoot || subSig`.
    pub fn signature_length(&self) -> usize {
        self.ops
            .signature_length(self.super_height)
            .saturating_add(self.ops.n())
            .saturating_add(self.ops.signature_length(self.sub_height))
    }

    /// `super tree || sub tree || chain seed (32) || superSig`.
    pub fn key_length(&self) -> usize {
        encoded_length(self.ops, self.super_height)
            .saturating_add(encoded_length(self.ops, self.sub_height))
            .saturating_add(32)
            .saturating_add(self.ops.signature_length(self.super_height))
    }

    /// Verification: `superSig` over `productHash(subRoot)` at `t_sup` under `vk`, then `subSig` over the message at
    /// `t_sub` under `subRoot`.
    pub fn verify(&self, vk: &[u8], period: u64, message: &[u8], signature: &[u8]) -> Result<bool, KesError> {
        if vk.len() != self.ops.n() {
            return Err(malformed("KES verifying key", self.ops.n(), vk.len()));
        }
        if signature.len() != self.signature_length() {
            return Err(malformed("KES signature", self.signature_length(), signature.len()));
        }
        check_period(period, self.total_periods())?;
        let (super_sig, rest) = signature.split_at(self.ops.signature_length(self.super_height));
        let (sub_root, sub_sig) = rest.split_at(self.ops.n());
        let t_sup = period.checked_shr(self.sub_height).unwrap_or(0);
        let t_sub = period.checked_rem(self.sub_periods()).unwrap_or(0);
        Ok(self.ops.verify(vk, self.super_height, t_sup, &self.ops.product_message(sub_root), super_sig)
            && self.ops.verify(sub_root, self.sub_height, t_sub, message, sub_sig))
    }
}

/// Cardano's `SumKES Blake2b_256 (... (SingleKES Ed25519DSIGN))` of depth 0 to 7 (`Sum6KES` is the Praos hot key).
/// Leaves verify under libsodium's Ed25519 rule, as cardano-node does.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CardanoSumKes {
    depth: u32,
}

/// `Blake2b-256`.
fn blake(parts: &[&[u8]]) -> Vec<u8> {
    HashAlgorithm::Blake2b256.hash_parts(parts)
}

impl CardanoSumKes {
    /// `Sum6KES`, the Praos hot key: 64 periods, 448-byte signatures and 608-byte keys.
    pub const SUM6: Self = Self { depth: 6 };

    /// `SumKES` of `depth` 0 to 7.
    pub fn new(depth: u32) -> Result<Self, KesError> {
        if depth <= 7 {
            Ok(Self { depth })
        } else {
            Err(KesError::Malformed { what: "KES depth", reason: format!("expected 0..7, got {depth}") })
        }
    }

    /// The depth.
    pub const fn depth(&self) -> u32 {
        self.depth
    }

    /// `2^depth`.
    pub fn total_periods(&self) -> u64 {
        periods(self.depth)
    }

    /// `64 + 64 d`.
    pub fn signature_length(&self) -> usize {
        cardano_sig_len(self.depth)
    }

    /// `32 + 96 d`.
    pub fn signing_key_length(&self) -> usize {
        cardano_sk_len(self.depth)
    }

    /// Verification of `signature` by the root key `vk` at `period` (`verifyKES`).
    pub fn verify(&self, vk: &[u8], period: u64, message: &[u8], signature: &[u8]) -> Result<bool, KesError> {
        if vk.len() != 32 {
            return Err(malformed("KES verifying key", 32, vk.len()));
        }
        if signature.len() != self.signature_length() {
            return Err(malformed("KES signature", self.signature_length(), signature.len()));
        }
        check_period(period, self.total_periods())?;
        Ok(cardano_verify(vk, self.depth, period, message, signature))
    }
}

fn cardano_sig_len(d: u32) -> usize {
    usize::try_from(d).unwrap_or(usize::MAX).saturating_mul(64).saturating_add(64)
}

fn cardano_sk_len(d: u32) -> usize {
    usize::try_from(d).unwrap_or(usize::MAX).saturating_mul(96).saturating_add(32)
}

/// `sigma || vk0 || vk1`, recursively; the leaf under libsodium's rule.
fn cardano_verify(vk: &[u8], d: u32, period: u64, message: &[u8], signature: &[u8]) -> bool {
    let Some(d1) = d.checked_sub(1) else {
        return ed25519::verify_libsodium_bytes(vk, message, signature) == Ok(true);
    };
    let Some((inner, vks)) = signature.split_at_checked(signature.len().saturating_sub(64)) else {
        return false;
    };
    let Some((vk0, vk1)) = vks.split_at_checked(32) else {
        return false;
    };
    let h = half(d);
    blake(&[vk0, vk1]) == vk
        && if period < h {
            cardano_verify(vk0, d1, period, message, inner)
        } else {
            cardano_verify(vk1, d1, period.wrapping_sub(h), message, inner)
        }
}

#[cfg(feature = "sign")]
pub use signing::{CardanoKey, ProductKey, SumKey};

#[cfg(feature = "sign")]
mod signing {
    use alloc::boxed::Box;
    use alloc::format;
    use alloc::vec::Vec;
    use core::cmp::Ordering;
    use core::fmt;

    use zeroize::{Zeroize, ZeroizeOnDrop, Zeroizing};

    use super::{
        CardanoSumKes, KesProduct, KesSum, Ops, SEED_TAG, blake, cardano_sk_len, check_period, encoded_length, half,
        malformed, tagged,
    };
    use crate::ed25519;
    use crate::error::KesError;

    const ZERO32: [u8; 32] = [0; 32];

    fn inconsistent(reason: impl Into<alloc::string::String>) -> KesError {
        KesError::InconsistentKey { reason: reason.into() }
    }

    fn seed32(seed: &[u8]) -> Result<Zeroizing<[u8; 32]>, KesError> {
        seed.try_into().map(Zeroizing::new).map_err(|_| malformed("KES seed", 32, seed.len()))
    }

    fn leaf_vk(seed: &[u8; 32]) -> [u8; 32] {
        // any 32 bytes are an Ed25519 seed
        ed25519::SigningKey::from_bytes(seed).map_or([0; 32], |sk| *sk.verifying_key().as_bytes())
    }

    fn leaf_sign(seed: &[u8; 32], message: &[u8]) -> [u8; 64] {
        ed25519::SigningKey::from_bytes(seed).map_or([0; 64], |sk| *sk.sign(message).as_bytes())
    }

    /// A sum-tree signing key: the active subtree, the right subtree's seed while it is pending, and both roots. A
    /// leaf's seed is `None` once erased.
    #[derive(Clone, Zeroize, ZeroizeOnDrop)]
    enum Tree {
        Leaf { seed: Option<[u8; 32]>, vk: [u8; 32] },
        Node { active: Box<Tree>, right_seed: Option<[u8; 32]>, left: Vec<u8>, right: Vec<u8> },
    }

    impl Ops {
        /// `(seedHash(00 || r)[0:32], seedHash(01 || r)[0:32])`.
        fn expand(self, seed: &[u8]) -> (Zeroizing<[u8; 32]>, Zeroizing<[u8; 32]>) {
            let first = |b: u8| {
                let h = Zeroizing::new(tagged(self.hash, SEED_TAG, &[&[b], seed]));
                let mut out = Zeroizing::new([0u8; 32]);
                for (o, v) in out.iter_mut().zip(h.iter()) {
                    *o = *v;
                }
                out
            };
            (first(0), first(1))
        }

        fn root(self, t: &Tree) -> Vec<u8> {
            match t {
                Tree::Leaf { vk, .. } => self.leaf(vk),
                Tree::Node { left, right, .. } => self.node(left, right),
            }
        }

        fn generate(self, seed: &[u8; 32], d: u32) -> Tree {
            match d.checked_sub(1) {
                None => Tree::Leaf { seed: Some(*seed), vk: leaf_vk(seed) },
                Some(d1) => {
                    let (r0, r1) = self.expand(seed);
                    let sub = self.generate(&r0, d1);
                    let right = self.root(&self.generate(&r1, d1));
                    Tree::Node { left: self.root(&sub), active: Box::new(sub), right_seed: Some(*r1), right }
                }
            }
        }

        /// The tree for `period + 1`; activating a right half regenerates it from its seed, checked against the root.
        fn evolve(self, t: &Tree, d: u32, period: u64) -> Result<Tree, KesError> {
            match t {
                Tree::Leaf { .. } => Err(KesError::Exhausted { period }),
                Tree::Node { active, right_seed, left, right } => {
                    let h = half(d);
                    let d1 = d.saturating_sub(1);
                    let next = period.saturating_add(1);
                    let (active, right_seed) = match next.cmp(&h) {
                        Ordering::Less => (self.evolve(active, d1, period)?, *right_seed),
                        Ordering::Equal => (
                            right_seed
                                .as_ref()
                                .map(|s| self.generate(s, d1))
                                .filter(|sub| self.root(sub) == *right)
                                .ok_or_else(|| {
                                    inconsistent(format!("the right seed at height {d} does not match its root"))
                                })?,
                            None,
                        ),
                        Ordering::Greater => (self.evolve(active, d1, period.wrapping_sub(h))?, None),
                    };
                    Ok(Tree::Node { active: Box::new(active), right_seed, left: left.clone(), right: right.clone() })
                }
            }
        }

        /// A tree of height `d` at `period`: the period says which right seeds are used (zero) and whether the leaf
        /// may be erased; every stored root is checked against its subtree.
        fn decode(self, b: &[u8], d: u32, period: u64, leaf_erased: bool) -> Result<Tree, KesError> {
            let Some(d1) = d.checked_sub(1) else {
                let (seed, vk) = b.split_at_checked(32).ok_or_else(|| inconsistent("short leaf"))?;
                let seed: [u8; 32] = seed.try_into().map_err(|_| inconsistent("short leaf"))?;
                let vk: [u8; 32] = vk.try_into().map_err(|_| inconsistent("short leaf"))?;
                return match (leaf_erased, seed == ZERO32) {
                    (true, true) => Ok(Tree::Leaf { seed: None, vk }),
                    (false, false) if leaf_vk(&seed) == vk => Ok(Tree::Leaf { seed: Some(seed), vk }),
                    _ => Err(inconsistent("leaf seed and verifying key disagree")),
                };
            };
            let h = half(d);
            let n = self.n();
            let (inner, rest) =
                b.split_at_checked(encoded_length(self, d1)).ok_or_else(|| inconsistent("short node"))?;
            let (seed, rest) = rest.split_at_checked(32).ok_or_else(|| inconsistent("short node"))?;
            let (l, r) = rest.split_at_checked(n).ok_or_else(|| inconsistent("short node"))?;
            let active = self.decode(inner, d1, period.checked_rem(h).unwrap_or(0), leaf_erased)?;
            let pending = period < h;
            if pending == (seed == ZERO32) {
                return Err(inconsistent(format!("right seed state does not match period {period} at height {d}")));
            }
            if self.root(&active) != if pending { l } else { r } {
                return Err(inconsistent(format!("active subtree root mismatch at height {d}")));
            }
            let right_seed = if pending { seed.try_into().ok() } else { None };
            Ok(Tree::Node { active: Box::new(active), right_seed, left: l.to_vec(), right: r.to_vec() })
        }
    }

    /// `vk || sig || witnesses (bottom-up)`, or `None` if the active leaf is erased.
    fn sign_tree(t: &Tree, d: u32, period: u64, message: &[u8]) -> Option<Vec<u8>> {
        match t {
            Tree::Leaf { seed, vk } => seed.as_ref().map(|s| crate::util::concat(&[vk, &leaf_sign(s, message)])),
            Tree::Node { active, left, right, .. } => {
                let h = half(d);
                let mut sig = sign_tree(active, d.saturating_sub(1), period.checked_rem(h).unwrap_or(0), message)?;
                sig.extend_from_slice(if period < h { right } else { left });
                Some(sig)
            }
        }
    }

    /// Leaf: `seed (zeros once erased) || vk`; node: `active || rightSeed (zeros once used) || left || right`.
    fn encode_tree(t: &Tree, out: &mut Vec<u8>) {
        match t {
            Tree::Leaf { seed, vk } => {
                out.extend_from_slice(seed.as_ref().unwrap_or(&ZERO32));
                out.extend_from_slice(vk);
            }
            Tree::Node { active, right_seed, left, right } => {
                encode_tree(active, out);
                out.extend_from_slice(right_seed.as_ref().unwrap_or(&ZERO32));
                out.extend_from_slice(left);
                out.extend_from_slice(right);
            }
        }
    }

    fn erase_active_leaf(t: &Tree) -> Tree {
        match t {
            Tree::Leaf { vk, .. } => Tree::Leaf { seed: None, vk: *vk },
            Tree::Node { active, right_seed, left, right } => Tree::Node {
                active: Box::new(erase_active_leaf(active)),
                right_seed: *right_seed,
                left: left.clone(),
                right: right.clone(),
            },
        }
    }

    /// A [`KesSum`] signing key at its period.
    #[derive(Clone)]
    pub struct SumKey {
        tree: Tree,
        period: u64,
    }

    impl SumKey {
        /// The key's period.
        pub const fn period(&self) -> u64 {
            self.period
        }
    }

    impl fmt::Debug for SumKey {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "SumKey(period={}, <redacted>)", self.period)
        }
    }

    fn rewrite_exhausted(e: KesError, period: u64) -> KesError {
        match e {
            KesError::Exhausted { .. } => KesError::Exhausted { period },
            other => other,
        }
    }

    impl KesSum {
        /// The period-0 key for a 32-byte seed.
        pub fn generate(&self, seed: &[u8]) -> Result<SumKey, KesError> {
            Ok(SumKey { tree: self.ops.generate(&*seed32(seed)?, self.height), period: 0 })
        }

        /// The key's canonical encoding (`64 + (32 + 2n) h` bytes).
        pub fn key_bytes(&self, key: &SumKey) -> Zeroizing<Vec<u8>> {
            let mut out = Zeroizing::new(Vec::with_capacity(self.key_length()));
            encode_tree(&key.tree, &mut out);
            out
        }

        /// Loads an encoded key at `period`, checking every leaf, seed state and stored root.
        pub fn load(&self, raw: &[u8], period: u64) -> Result<SumKey, KesError> {
            if raw.len() != self.key_length() {
                return Err(malformed("KES key", self.key_length(), raw.len()));
            }
            check_period(period, self.total_periods())?;
            Ok(SumKey { tree: self.ops.decode(raw, self.height, period, false)?, period })
        }

        /// The root: fixed for the key's lifetime.
        pub fn verifying_key(&self, key: &SumKey) -> Vec<u8> {
            self.ops.root(&key.tree)
        }

        /// The signature on `message` at the key's period.
        pub fn sign(&self, key: &SumKey, message: &[u8]) -> Result<Vec<u8>, KesError> {
            sign_tree(&key.tree, self.height, key.period, message)
                .ok_or_else(|| inconsistent("the active leaf is erased"))
        }

        /// The key for the next period; the last period is `Exhausted`.
        pub fn evolve(&self, key: &SumKey) -> Result<SumKey, KesError> {
            let tree =
                self.ops.evolve(&key.tree, self.height, key.period).map_err(|e| rewrite_exhausted(e, key.period))?;
            Ok(SumKey { tree, period: key.period.saturating_add(1) })
        }

        /// The key at `target` (at or after the key's period), evolving one period at a time.
        pub fn evolve_to(&self, key: &SumKey, target: u64) -> Result<SumKey, KesError> {
            if target < key.period || target >= self.total_periods() {
                return Err(KesError::PeriodOutOfRange { period: target, total: self.total_periods() });
            }
            let mut k = key.clone();
            while k.period < target {
                k = self.evolve(&k)?;
            }
            Ok(k)
        }
    }

    /// A [`KesProduct`] signing key: the super tree (active leaf erased), the sub tree, the next chain seed and the
    /// super signature on the sub root.
    #[derive(Clone)]
    pub struct ProductKey {
        super_tree: Tree,
        sub_tree: Tree,
        next_seed: Zeroizing<[u8; 32]>,
        super_sig: Vec<u8>,
        period: u64,
    }

    impl ProductKey {
        /// The key's period.
        pub const fn period(&self) -> u64 {
            self.period
        }
    }

    impl fmt::Debug for ProductKey {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "ProductKey(period={}, <redacted>)", self.period)
        }
    }

    impl KesProduct {
        /// Certifies a fresh sub tree from a chain seed with the super tree at `super_period`, then erases the
        /// super leaf.
        fn certify(
            &self,
            super_tree: &Tree,
            super_period: u64,
            chain_seed: &[u8; 32],
            period: u64,
        ) -> Result<ProductKey, KesError> {
            let (sub_seed, next) = self.ops.expand(chain_seed);
            let sub = self.ops.generate(&sub_seed, self.sub_height);
            let sig =
                sign_tree(super_tree, self.super_height, super_period, &self.ops.product_message(&self.ops.root(&sub)))
                    .ok_or_else(|| inconsistent("the super leaf is erased"))?;
            Ok(ProductKey {
                super_tree: erase_active_leaf(super_tree),
                sub_tree: sub,
                next_seed: next,
                super_sig: sig,
                period,
            })
        }

        /// The period-0 key for a 32-byte seed.
        pub fn generate(&self, seed: &[u8]) -> Result<ProductKey, KesError> {
            let (super_seed, chain) = self.ops.expand(&*seed32(seed)?);
            self.certify(&self.ops.generate(&super_seed, self.super_height), 0, &chain, 0)
        }

        /// The key's encoding: `super tree || sub tree || chain seed || superSig`.
        pub fn key_bytes(&self, key: &ProductKey) -> Zeroizing<Vec<u8>> {
            let mut out = Zeroizing::new(Vec::with_capacity(self.key_length()));
            encode_tree(&key.super_tree, &mut out);
            encode_tree(&key.sub_tree, &mut out);
            out.extend_from_slice(&*key.next_seed);
            out.extend_from_slice(&key.super_sig);
            out
        }

        /// The super tree's root: fixed for the key's lifetime.
        pub fn verifying_key(&self, key: &ProductKey) -> Vec<u8> {
            self.ops.root(&key.super_tree)
        }

        /// `superSig || subRoot || subSig`.
        pub fn sign(&self, key: &ProductKey, message: &[u8]) -> Result<Vec<u8>, KesError> {
            let t_sub = key.period.checked_rem(self.sub_periods()).unwrap_or(0);
            let sub = sign_tree(&key.sub_tree, self.sub_height, t_sub, message)
                .ok_or_else(|| inconsistent("the sub leaf is erased"))?;
            Ok(super::super::util::concat(&[&key.super_sig, &self.ops.root(&key.sub_tree), &sub]))
        }

        /// The key for the next period: the sub tree evolves, or at its end the super tree evolves and certifies the
        /// next sub tree.
        pub fn evolve(&self, key: &ProductKey) -> Result<ProductKey, KesError> {
            let t_sup = key.period.checked_shr(self.sub_height).unwrap_or(0);
            let t_sub = key.period.checked_rem(self.sub_periods()).unwrap_or(0);
            let next = key.period.saturating_add(1);
            if t_sub.saturating_add(1) < self.sub_periods() {
                let sub = self.ops.evolve(&key.sub_tree, self.sub_height, t_sub)?;
                Ok(ProductKey { sub_tree: sub, period: next, ..key.clone() })
            } else {
                let sup = self
                    .ops
                    .evolve(&key.super_tree, self.super_height, t_sup)
                    .map_err(|e| rewrite_exhausted(e, key.period))?;
                self.certify(&sup, t_sup.saturating_add(1), &key.next_seed, next)
            }
        }

        /// Loads an encoded key at `period`; also checks that `superSig` certifies the sub tree.
        pub fn load(&self, raw: &[u8], period: u64) -> Result<ProductKey, KesError> {
            if raw.len() != self.key_length() {
                return Err(malformed("KES key", self.key_length(), raw.len()));
            }
            check_period(period, self.total_periods())?;
            let t_sup = period.checked_shr(self.sub_height).unwrap_or(0);
            let t_sub = period.checked_rem(self.sub_periods()).unwrap_or(0);
            let (sup_raw, rest) = raw.split_at(encoded_length(self.ops, self.super_height));
            let (sub_raw, rest) = rest.split_at(encoded_length(self.ops, self.sub_height));
            let (next, sig) = rest.split_at(32);
            let sup = self.ops.decode(sup_raw, self.super_height, t_sup, true)?;
            let sub = self.ops.decode(sub_raw, self.sub_height, t_sub, false)?;
            if !self.ops.verify(
                &self.ops.root(&sup),
                self.super_height,
                t_sup,
                &self.ops.product_message(&self.ops.root(&sub)),
                sig,
            ) {
                return Err(inconsistent("the super signature does not certify the sub tree"));
            }
            let next_seed = Zeroizing::new(next.try_into().map_err(|_| inconsistent("short chain seed"))?);
            Ok(ProductKey { super_tree: sup, sub_tree: sub, next_seed, super_sig: sig.to_vec(), period })
        }

        /// The key at `target` (at or after the key's period), evolving one period at a time.
        pub fn evolve_to(&self, key: &ProductKey, target: u64) -> Result<ProductKey, KesError> {
            if target < key.period || target >= self.total_periods() {
                return Err(KesError::PeriodOutOfRange { period: target, total: self.total_periods() });
            }
            let mut k = key.clone();
            while k.period < target {
                k = self.evolve(&k)?;
            }
            Ok(k)
        }
    }

    /// A [`CardanoSumKes`] signing key (cardano-base's raw `kes.skey` payload) at its period.
    #[derive(Clone)]
    pub struct CardanoKey {
        tree: Zeroizing<Vec<u8>>,
        period: u64,
    }

    impl CardanoKey {
        /// The key's period.
        pub const fn period(&self) -> u64 {
            self.period
        }

        /// The raw key: `sk || r1 || vk0 || vk1`, recursively, with `r1` zeroed once used.
        pub fn to_bytes(&self) -> Zeroizing<Vec<u8>> {
            self.tree.clone()
        }
    }

    impl fmt::Debug for CardanoKey {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "CardanoKey(period={}, <redacted>)", self.period)
        }
    }

    /// A key's inner key, `r1`, `vk0` and `vk1`.
    type Parts<'a> = (&'a [u8], &'a [u8], &'a [u8], &'a [u8]);

    /// The parts of a depth-`d` key: the inner key, `r1`, `vk0`, `vk1`.
    fn parts(sk: &[u8], d1: u32) -> Option<Parts<'_>> {
        let (inner, rest) = sk.split_at_checked(cardano_sk_len(d1))?;
        let (r1, rest) = rest.split_at_checked(32)?;
        let (vk0, vk1) = rest.split_at_checked(32)?;
        Some((inner, r1, vk0, vk1))
    }

    fn cardano_vk(sk: &[u8], d: u32) -> Vec<u8> {
        match d.checked_sub(1) {
            None => sk.try_into().map_or_else(|_| Vec::new(), |seed: [u8; 32]| leaf_vk(&seed).to_vec()),
            Some(d1) => parts(sk, d1).map_or_else(Vec::new, |(_, _, vk0, vk1)| blake(&[vk0, vk1])),
        }
    }

    fn cardano_gen(seed: &[u8], d: u32) -> Zeroizing<Vec<u8>> {
        match d.checked_sub(1) {
            None => Zeroizing::new(seed.to_vec()),
            Some(d1) => {
                let r0 = Zeroizing::new(blake(&[&[1], seed]));
                let r1 = Zeroizing::new(blake(&[&[2], seed]));
                let sk0 = cardano_gen(&r0, d1);
                let vk0 = cardano_vk(&sk0, d1);
                let vk1 = cardano_vk(&cardano_gen(&r1, d1), d1);
                let mut out = sk0;
                out.extend_from_slice(&r1);
                out.extend_from_slice(&vk0);
                out.extend_from_slice(&vk1);
                out
            }
        }
    }

    fn cardano_sign(sk: &[u8], d: u32, t: u64, message: &[u8]) -> Vec<u8> {
        match d.checked_sub(1) {
            None => sk.try_into().map_or_else(|_| Vec::new(), |seed: [u8; 32]| leaf_sign(&seed, message).to_vec()),
            Some(d1) => {
                let Some((inner, _, vk0, vk1)) = parts(sk, d1) else {
                    return Vec::new();
                };
                let h = half(d);
                let mut sig = cardano_sign(inner, d1, if t < h { t } else { t.wrapping_sub(h) }, message);
                sig.extend_from_slice(vk0);
                sig.extend_from_slice(vk1);
                sig
            }
        }
    }

    /// `updateKES`: `None` at the last period; activating a right half regenerates it from `r1`, checked against
    /// `vk1`.
    fn cardano_update(sk: &[u8], d: u32, t: u64) -> Result<Option<Zeroizing<Vec<u8>>>, KesError> {
        let Some(d1) = d.checked_sub(1) else {
            return Ok(None);
        };
        let (inner, r1, vk0, vk1) = parts(sk, d1).ok_or_else(|| inconsistent("short key"))?;
        let h = half(d);
        let next = t.saturating_add(1);
        let rebuild = |inner: Zeroizing<Vec<u8>>, r1: &[u8]| {
            let mut out = inner;
            out.extend_from_slice(r1);
            out.extend_from_slice(vk0);
            out.extend_from_slice(vk1);
            out
        };
        match next.cmp(&h) {
            Ordering::Less => Ok(cardano_update(inner, d1, t)?.map(|i| rebuild(i, r1))),
            Ordering::Equal => {
                let right = cardano_gen(r1, d1);
                if cardano_vk(&right, d1) != vk1 {
                    return Err(inconsistent(format!("r1 at depth {d} does not match vk1")));
                }
                Ok(Some(rebuild(right, &ZERO32)))
            }
            Ordering::Greater => Ok(cardano_update(inner, d1, t.wrapping_sub(h))?.map(|i| rebuild(i, r1))),
        }
    }

    /// Load-time consistency at `t`: `r1` is zero exactly once used, and each active subtree's vk is the stored one.
    fn cardano_check(sk: &[u8], d: u32, t: u64) -> Result<(), KesError> {
        let Some(d1) = d.checked_sub(1) else {
            return Ok(());
        };
        let (inner, r1, vk0, vk1) = parts(sk, d1).ok_or_else(|| inconsistent("short key"))?;
        let h = half(d);
        if (t >= h) != (r1 == ZERO32) {
            return Err(inconsistent(format!("r1 state does not match period {t} at depth {d}")));
        }
        if cardano_vk(inner, d1) != if t < h { vk0 } else { vk1 } {
            return Err(inconsistent(format!("active subtree does not match its vk at depth {d}")));
        }
        cardano_check(inner, d1, t.checked_rem(h).unwrap_or(0))
    }

    impl CardanoSumKes {
        /// The period-0 key for a 32-byte seed (`genKeyKES`).
        pub fn generate(&self, seed: &[u8]) -> Result<CardanoKey, KesError> {
            Ok(CardanoKey { tree: cardano_gen(&*seed32(seed)?, self.depth), period: 0 })
        }

        /// Loads a raw key at the period the caller knows it to be at.
        pub fn load(&self, raw: &[u8], period: u64) -> Result<CardanoKey, KesError> {
            if raw.len() != self.signing_key_length() {
                return Err(malformed("KES signing key", self.signing_key_length(), raw.len()));
            }
            check_period(period, self.total_periods())?;
            cardano_check(raw, self.depth, period)?;
            Ok(CardanoKey { tree: Zeroizing::new(raw.to_vec()), period })
        }

        /// The root verifying key.
        pub fn verifying_key(&self, key: &CardanoKey) -> Vec<u8> {
            cardano_vk(&key.tree, self.depth)
        }

        /// The signature at the key's period.
        pub fn sign(&self, key: &CardanoKey, message: &[u8]) -> Vec<u8> {
            cardano_sign(&key.tree, self.depth, key.period, message)
        }

        /// The key for the next period (`updateKES`); the last period is `Exhausted`.
        pub fn evolve(&self, key: &CardanoKey) -> Result<CardanoKey, KesError> {
            cardano_update(&key.tree, self.depth, key.period)?
                .map(|tree| CardanoKey { tree, period: key.period.saturating_add(1) })
                .ok_or(KesError::Exhausted { period: key.period })
        }

        /// The key at `target` (at or after the key's period), evolving one period at a time.
        pub fn evolve_to(&self, key: &CardanoKey, target: u64) -> Result<CardanoKey, KesError> {
            if target < key.period || target >= self.total_periods() {
                return Err(KesError::PeriodOutOfRange { period: target, total: self.total_periods() });
            }
            let mut k = key.clone();
            while k.period < target {
                k = self.evolve(&k)?;
            }
            Ok(k)
        }
    }

    #[allow(dead_code, reason = "keeps the zeroize import used in every configuration")]
    fn _zeroize(mut b: [u8; 32]) {
        b.zeroize();
    }
}
