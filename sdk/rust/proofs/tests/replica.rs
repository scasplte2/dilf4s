//! The replica's tries (docs/specs/versioned-trie.md §7): updated incrementally, equal to a full rebuild after every
//! delta (deletes, re-inserts, an emptied state, refused deltas), and hashing only the changed keys' paths, whatever
//! the state's size or however its keys are chosen. Work is counted (trie node hashes), never timed.

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
    clippy::too_many_lines,
    reason = "tests fail by panicking, and run with std"
)]

use std::cell::Cell;
use std::collections::BTreeMap;

use dilf4s_core::hash::{Digest, Hasher, Sha256};
use dilf4s_core::value::Value;
use dilf4s_proofs::trie::{BRANCH_TAG, LEAF_TAG, root_from_entries};
use dilf4s_proofs::versioned::VersionedTrieErrorKind;
use dilf4s_proofs::versioned::{Delta, Head, Replica, Snapshot, StateKey, U64Key, VersionKey, VersionedTrie};

thread_local! {
    static NODE_HASHES: Cell<u64> = const { Cell::new(0) };
    static NODE_TAGS: [[u8; 32]; 2] =
        [Sha256::hash_parts(&[LEAF_TAG.as_bytes()]), Sha256::hash_parts(&[BRANCH_TAG.as_bytes()])];
}

/// SHA-256, counting the trie node hashes (leaf and branch: tagged hashes whose first part is their tag's hash) made on
/// this thread. Each test runs on its own thread, so the count is the test's own.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Counted {}

impl Hasher for Counted {
    const NAME: &'static str = Sha256::NAME;
    const OUTPUT_SIZE: usize = Sha256::OUTPUT_SIZE;
    type Output = <Sha256 as Hasher>::Output;
    /// SHA-256's state, and whether nothing has been absorbed yet: a hash's first part tells a node hash.
    type State = (<Sha256 as Hasher>::State, bool);

    fn start() -> Self::State {
        (Sha256::start(), true)
    }

    fn absorb(state: &mut Self::State, data: &[u8]) {
        if state.1 {
            state.1 = false;
            if NODE_TAGS.with(|tags| tags.iter().any(|t| t.as_slice() == data)) {
                NODE_HASHES.with(|n| n.set(n.get() + 1));
            }
        }
        Sha256::absorb(&mut state.0, data);
    }

    fn finish(state: Self::State) -> Self::Output {
        Sha256::finish(state.0)
    }

    fn output_from_slice(bytes: &[u8]) -> Option<Self::Output> {
        Sha256::output_from_slice(bytes)
    }
}

type H = Counted;
type Vt = VersionedTrie<U64Key, H>;

/// The node hashes `f` makes.
fn counting<A>(f: impl FnOnce() -> A) -> (A, u64) {
    let before = NODE_HASHES.with(Cell::get);
    let a = f();
    (a, NODE_HASHES.with(Cell::get) - before)
}

fn key(s: &str) -> StateKey {
    StateKey::parse(s).unwrap()
}

fn vt() -> Vt {
    VersionedTrie::new(U64Key::DEFAULT).unwrap()
}

/// The reference: a replica's state and history as plain sorted maps, with roots rebuilt from scratch.
#[derive(Clone)]
struct Model {
    state: BTreeMap<StateKey, Value>,
    history: BTreeMap<Vec<u8>, Digest<H>>,
    head: Option<Head<u64, H>>,
}

impl Model {
    const fn new() -> Self {
        Self { state: BTreeMap::new(), history: BTreeMap::new(), head: None }
    }

    /// The delta writing `upserts` and removing `removes` at `version`, its head rebuilt from the whole state.
    fn delta(
        &self,
        vt: &Vt,
        version: u64,
        upserts: &[(StateKey, Value)],
        removes: &[StateKey],
    ) -> (Delta<u64, H>, Self) {
        let mut next = self.clone();
        for k in removes {
            next.state.remove(k);
        }
        for (k, v) in upserts {
            next.state.insert(k.clone(), v.clone());
        }
        let hashed: Vec<(Vec<u8>, Digest<H>)> =
            next.state.iter().map(|(k, v)| (k.as_bytes().to_vec(), vt.value_hash(v))).collect();
        let state_root =
            root_from_entries(vt.hashing(), vt.state_mode(), hashed.iter().map(|(k, h)| (k.as_slice(), h.clone())))
                .unwrap();
        next.history.insert(vt.vkey(&version).unwrap(), vt.bytes_hash(state_root.as_bytes()));
        let history_root = root_from_entries(
            vt.hashing(),
            vt.history_mode(),
            next.history.iter().map(|(k, h)| (k.as_slice(), h.clone())),
        )
        .unwrap();
        let head = Head { version, state_root, history_root };
        next.head = Some(head.clone());
        let delta = Delta { parent: self.head.clone(), head, upserts: upserts.to_vec(), removes: removes.to_vec() };
        (delta, next)
    }

    /// The model's snapshot, its history read back from the version keys.
    fn snapshot(&self, vt: &Vt, roots: &BTreeMap<Vec<u8>, Digest<H>>) -> Snapshot<u64, H> {
        Snapshot {
            head: self.head.clone().unwrap(),
            entries: self.state.iter().map(|(k, v)| (k.clone(), v.clone())).collect(),
            history: roots.iter().map(|(k, s)| (vt.version_key().decode(k).unwrap(), s.clone())).collect(),
        }
    }
}

fn entries(r: &Replica<U64Key, H>) -> Vec<(StateKey, Value)> {
    r.entries().map(|(k, v)| (k.clone(), v.clone())).collect()
}

/// xorshift64*: a fixed, seeded stream.
struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 >> 12;
        self.0 ^= self.0 << 25;
        self.0 ^= self.0 >> 27;
        self.0.wrapping_mul(0x2545_f491_4f6c_dd1d)
    }

    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }
}

/// Keys where the trie restructures: prefixes of each other (branch values), shared namespaces, single-bit splits.
fn universe() -> Vec<StateKey> {
    let names = [
        "a", "a/b", "a/b/c", "a/b/c/d", "a/c", "ab", "ab/c", "b", "b0", "b1", "b2", "c", "fiber/1", "fiber/2",
        "fiber/10", "fiberx/1", "z", "z/z", "0", "00", "01", "1", "q.r", "q-r", "q_r",
    ];
    names.iter().map(|s| key(s)).collect()
}

#[test]
fn incremental_roots_equal_a_full_rebuild_for_every_delta() {
    let vt = vt();
    let keys = universe();
    for seed in 1..=8u64 {
        let mut rng = Rng(seed.wrapping_mul(0x9e37_79b9_7f4a_7c15));
        let mut model = Model::new();
        let mut roots: BTreeMap<Vec<u8>, Digest<H>> = BTreeMap::new();
        let mut replica = Replica::empty(vt.clone());
        for step in 1..=80u64 {
            let version = step * 3;
            // every 20th delta empties the state; the next ones re-insert into it
            let (upserts, removes) = if step % 20 == 0 {
                (Vec::new(), model.state.keys().cloned().collect::<Vec<_>>())
            } else {
                let mut chosen: BTreeMap<StateKey, Option<Value>> = BTreeMap::new();
                for _ in 0..=rng.below(5) {
                    let k = keys[rng.below(keys.len())].clone();
                    let write = rng.below(3) != 0;
                    chosen.insert(k, write.then(|| Value::int(rng.below(1000))));
                }
                let upserts = chosen.iter().filter_map(|(k, v)| v.clone().map(|v| (k.clone(), v))).collect::<Vec<_>>();
                // a remove may name an absent key, which changes nothing
                let removes = chosen.iter().filter(|(_, v)| v.is_none()).map(|(k, _)| k.clone()).collect::<Vec<_>>();
                (upserts, removes)
            };
            let (delta, next) = model.delta(&vt, version, &upserts, &removes);

            // a forged delta is refused and leaves the replica exactly as it was
            let before = (replica.root().unwrap(), entries(&replica), replica.len());
            let other = Digest::<H>::from_slice(&[0xab; 32]).unwrap();
            let forged_state =
                Delta { head: Head { state_root: other.clone(), ..delta.head.clone() }, ..delta.clone() };
            let forged_history = Delta { head: Head { history_root: other, ..delta.head.clone() }, ..delta.clone() };
            assert_eq!(
                replica.apply_delta(&forged_state).unwrap_err().kind(),
                VersionedTrieErrorKind::RootMismatch,
                "seed {seed} step {step}"
            );
            assert_eq!(
                replica.apply_delta(&forged_history).unwrap_err().kind(),
                VersionedTrieErrorKind::RootMismatch,
                "seed {seed} step {step}"
            );
            assert_eq!((replica.root().unwrap(), entries(&replica), replica.len()), before, "seed {seed} step {step}");

            replica.apply_delta(&delta).unwrap_or_else(|e| panic!("seed {seed} step {step}: {e}"));
            model = next;
            roots.insert(vt.vkey(&version).unwrap(), delta.head.state_root.clone());
            assert_eq!(replica.head().unwrap(), model.head.as_ref().unwrap(), "seed {seed} step {step}");
            let expected: Vec<(StateKey, Value)> = model.state.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
            assert_eq!(entries(&replica), expected, "seed {seed} step {step}");
            assert_eq!(replica.len(), expected.len());
            assert_eq!(replica.is_empty(), expected.is_empty());
            for k in &keys {
                assert_eq!(replica.get(k), model.state.get(k), "seed {seed} step {step}: {k}");
            }
            if step % 20 == 0 {
                assert!(replica.is_empty(), "the state was emptied");
                assert_eq!(replica.head().unwrap().state_root, vt.hashing().empty());
            }
            // a replica restored from the snapshot here is the same replica
            if step % 7 == 0 {
                let head = model.head.clone().unwrap();
                let restored = Replica::from_snapshot(vt.clone(), head, &model.snapshot(&vt, &roots)).unwrap();
                assert_eq!(restored.root().unwrap(), replica.root().unwrap());
                assert_eq!(entries(&restored), expected);
            }
        }
    }
}

/// Bit positions at which `keys` do not all agree (a position past a key's end counts): every branch on a path sits
/// at one, so a path holds at most this many branches and one leaf.
fn varying_bits(keys: &[Vec<u8>]) -> u64 {
    let longest = keys.iter().map(Vec::len).max().unwrap_or(0) * 8;
    let bit = |k: &Vec<u8>, i: usize| k.get(i / 8).map(|b| (b >> (7 - i % 8)) & 1);
    (0..longest).filter(|&i| keys.iter().any(|k| bit(k, i) != bit(&keys[0], i))).count() as u64
}

/// The most node hashes one delta may make: two top-trie roots (the parent check and the claimed parent, five node
/// hashes each), and for each changed key and the new version its path re-hashed with at most one lowered or merged
/// node and one new leaf.
fn bound(changes: u64, state_depth: u64, history_depth: u64) -> u64 {
    2 * 5 + changes * (state_depth + 1 + 2) + (history_depth + 1 + 2)
}

/// A replica of `keys` (each holding its index) at version 1, built from a snapshot, and its model.
fn seeded(vt: &Vt, keys: &[StateKey]) -> (Replica<U64Key, H>, Model) {
    let upserts: Vec<(StateKey, Value)> = keys.iter().enumerate().map(|(i, k)| (k.clone(), Value::int(i))).collect();
    let (delta, model) = Model::new().delta(vt, 1, &upserts, &[]);
    let roots = BTreeMap::from([(vt.vkey(&1).unwrap(), delta.head.state_root.clone())]);
    let replica = Replica::from_snapshot(vt.clone(), delta.head, &model.snapshot(vt, &roots)).unwrap();
    (replica, model)
}

/// Applies a delta changing three keys of `keys` (one updated, one removed, one new), returning its node hashes.
fn three_changes(vt: &Vt, keys: &[StateKey], fresh: &StateKey) -> (u64, u64) {
    let (mut replica, model) = seeded(vt, keys);
    let n = keys.len();
    let upserts = [(keys[n / 3].clone(), Value::int(-1)), (fresh.clone(), Value::int(-2))];
    let removes = [keys[2 * n / 3].clone()];
    let (delta, _) = model.delta(vt, 2, &upserts, &removes);
    let ((), hashes) = counting(|| replica.apply_delta(&delta).unwrap());
    let mut all: Vec<Vec<u8>> = keys.iter().map(|k| k.as_bytes().to_vec()).collect();
    all.push(fresh.as_bytes().to_vec());
    let versions = [vt.vkey(&1).unwrap(), vt.vkey(&2).unwrap()];
    (hashes, bound(3, varying_bits(&all), varying_bits(&versions)))
}

#[test]
fn a_delta_hashes_only_the_changed_keys_paths() {
    let vt = vt();
    let n = 1usize << 12;
    // ordinary keys; keys sharing one Java String hash code (every mix of "an" and "c0"); keys V8 hashes alike
    // (multiples of 65536): no hashed table sees a key, so every family costs only its paths
    let ordinary: Vec<StateKey> = (0..n).map(|i| key(&format!("k/{i:04}"))).collect();
    let java: Vec<StateKey> =
        (0..n).map(|i| key(&(0..12).map(|b| if i >> b & 1 == 1 { "c0" } else { "an" }).collect::<String>())).collect();
    let v8: Vec<StateKey> = (0..n).map(|i| key(&(i * 65536).to_string())).collect();
    for (name, keys) in [("ordinary", &ordinary), ("java-colliding", &java), ("v8-colliding", &v8)] {
        // the reference root and the restore each hash every node once, and there is a node per key
        let (_, built) = counting(|| seeded(&vt, keys));
        assert!(built >= 2 * n as u64, "{name}: a build of {n} keys made {built} node hashes");
        let (hashes, limit) = three_changes(&vt, keys, &key("fresh/key"));
        eprintln!("replica {name}: {n} keys, build {built} node hashes, a 3-key delta {hashes} (bound {limit})");
        assert!(hashes <= limit, "{name}: a 3-key delta made {hashes} node hashes, over {limit}");
        assert!(hashes * 40 < built, "{name}: a 3-key delta made {hashes} node hashes, against {built} for a rebuild");
    }
    // the same delta on a state 64 times smaller makes about as many node hashes: the cost is the paths', not the size's
    let (small, _) = three_changes(&vt, &ordinary[..n / 64], &key("fresh/key"));
    let (large, _) = three_changes(&vt, &ordinary, &key("fresh/key"));
    assert!(large <= small + 3 * 8, "{small} node hashes at {} keys, {large} at {n}", n / 64);
}

#[test]
fn a_deep_key_costs_its_depth_and_nothing_more() {
    let vt = vt();
    // every prefix of a 255-character key that is itself a key: each a branch value above the next, 248 levels deep
    let long: String = (0..4).map(|_| "a".repeat(63)).collect::<Vec<_>>().join("/");
    let chain: Vec<StateKey> = (1..=long.len()).filter_map(|i| StateKey::parse(&long[..i]).ok()).collect();
    let bulk: Vec<StateKey> = (0..1usize << 12).map(|i| key(&format!("k/{i:04}"))).collect();
    let keys: Vec<StateKey> = chain.iter().chain(&bulk).cloned().collect();
    let depth = chain.len() as u64;
    let (mut replica, model) = seeded(&vt, &keys);
    let deepest = chain.last().unwrap().clone();
    let (delta, model) = model.delta(&vt, 2, &[(deepest, Value::int(7))], &[]);
    let ((), deep) = counting(|| replica.apply_delta(&delta).unwrap());
    assert!(deep > depth, "the deepest key's path has {depth} nodes, re-hashed: {deep}");
    assert!(deep <= bound(1, depth + 1, 2), "{deep} node hashes for one deep key");
    // a key beside the chain costs its own short path, not the chain's
    let (delta, _) = model.delta(&vt, 3, &[(bulk[7].clone(), Value::int(7))], &[]);
    let ((), shallow) = counting(|| replica.apply_delta(&delta).unwrap());
    let bulk_bytes: Vec<Vec<u8>> = bulk.iter().map(|k| k.as_bytes().to_vec()).collect();
    assert!(shallow <= bound(1, varying_bits(&bulk_bytes) + 1, 2), "{shallow} node hashes for one shallow key");
    eprintln!("replica: a key {depth} levels deep re-hashes {deep} nodes, one beside it {shallow}");
}
