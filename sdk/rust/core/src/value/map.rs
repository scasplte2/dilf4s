//! [`Map`]: a persistent AVL tree in canonical key order.
//!
//! Every node is immutable and shared. An update copies the path from the root to the changed key, rebalancing on the
//! way back with at most two rotations per level, so it creates O(log n) nodes and shares every other subtree with the
//! map it came from. Keys are ordered by [`canonical_key_order`] and never hashed.
//!
//! Each node caches its subtree's height, its exact (never saturated) entry weight sum, and its deepest value, so a
//! map's [`Map::weight`] and [`Map::depth`] are O(1) and stay exact through any sequence of updates.
//!
//! **Bounds.** An AVL tree of `n` entries is at most `1.44 log2(n + 2)` high: under 92 for any `n` below `2^64`.
//! Insertion, removal and building recurse at most that deep; iteration keeps an explicit stack of at most that many
//! nodes. Dropping never recurses: see [`release`].

use alloc::boxed::Box;
use alloc::collections::BTreeSet;
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;
use core::cmp::Ordering;
use core::iter::FusedIterator;

use super::{Array, MAX_DEPTH, MAX_WEIGHT, Text, Value, canonical_key_order};
use crate::error::CanonicalError;

type Link = Option<Arc<Node>>;

/// One entry, its subtrees, and what they cache.
struct Node {
    key: Text,
    value: Value,
    left: Link,
    right: Link,
    /// 1 for a leaf.
    height: u8,
    /// The sum over this subtree of each entry's `1 + key length + value weight`, exactly: each term is at most
    /// `2^63 + 2^64`, so no tree that fits in memory reaches `u128::MAX`.
    weight: u128,
    /// The deepest value in this subtree.
    depth: usize,
}

fn height(link: &Link) -> u8 {
    link.as_ref().map_or(0, |n| n.height)
}

fn weight_sum(link: &Link) -> u128 {
    link.as_ref().map_or(0, |n| n.weight)
}

fn deepest(link: &Link) -> usize {
    link.as_ref().map_or(0, |n| n.depth)
}

/// An entry's share of its map's weight: one node, the key's UTF-8 bytes, and the value's weight.
fn entry_weight(key: &str, value: &Value) -> u128 {
    let key_len = u64::try_from(key.len()).map_or(u128::MAX, u128::from);
    key_len.saturating_add(1).saturating_add(u128::from(value.weight()))
}

/// A node over `left` and `right`, which must already be balanced against each other.
fn node(key: Text, value: Value, left: Link, right: Link) -> Arc<Node> {
    let height = height(&left).max(height(&right)).saturating_add(1);
    let weight = weight_sum(&left).saturating_add(weight_sum(&right)).saturating_add(entry_weight(&key, &value));
    let depth = value.depth().max(deepest(&left)).max(deepest(&right));
    Arc::new(Node { key, value, left, right, height, weight, depth })
}

/// `key: value` over `left` and `right`, whose heights differ by at most 2: rebalanced with one or two rotations.
/// A child as high as its sibling inside it (possible after a removal) takes the single rotation.
fn balanced(key: Text, value: Value, left: Link, right: Link) -> Arc<Node> {
    let (hl, hr) = (height(&left), height(&right));
    if hl > hr.saturating_add(1) {
        if let Some(l) = &left {
            if height(&l.left) >= height(&l.right) {
                let lowered = node(key, value, l.right.clone(), right);
                return node(l.key.clone(), l.value.clone(), l.left.clone(), Some(lowered));
            }
            if let Some(lr) = &l.right {
                let low = node(l.key.clone(), l.value.clone(), l.left.clone(), lr.left.clone());
                let high = node(key, value, lr.right.clone(), right);
                return node(lr.key.clone(), lr.value.clone(), Some(low), Some(high));
            }
        }
    } else if hr > hl.saturating_add(1) {
        if let Some(r) = &right {
            if height(&r.right) >= height(&r.left) {
                let lowered = node(key, value, left, r.left.clone());
                return node(r.key.clone(), r.value.clone(), Some(lowered), r.right.clone());
            }
            if let Some(rl) = &r.left {
                let low = node(key, value, left, rl.left.clone());
                let high = node(r.key.clone(), r.value.clone(), rl.right.clone(), r.right.clone());
                return node(rl.key.clone(), rl.value.clone(), Some(low), Some(high));
            }
        }
    }
    node(key, value, left, right)
}

/// The tree with `key` set to `value`, and whether the key is new. A replaced entry keeps its key.
fn insert(link: &Link, key: Text, value: Value) -> (Arc<Node>, bool) {
    let Some(n) = link else {
        return (node(key, value, None, None), true);
    };
    match canonical_key_order(&key, &n.key) {
        Ordering::Less => {
            let (left, added) = insert(&n.left, key, value);
            (balanced(n.key.clone(), n.value.clone(), Some(left), n.right.clone()), added)
        }
        Ordering::Greater => {
            let (right, added) = insert(&n.right, key, value);
            (balanced(n.key.clone(), n.value.clone(), n.left.clone(), Some(right)), added)
        }
        Ordering::Equal => (node(n.key.clone(), value, n.left.clone(), n.right.clone()), false),
    }
}

/// The tree without `key`, or `None` when `key` is absent (so nothing is copied).
fn remove(link: &Link, key: &str) -> Option<Link> {
    let n = link.as_ref()?;
    match canonical_key_order(key, &n.key) {
        Ordering::Less => {
            remove(&n.left, key).map(|left| Some(balanced(n.key.clone(), n.value.clone(), left, n.right.clone())))
        }
        Ordering::Greater => {
            remove(&n.right, key).map(|right| Some(balanced(n.key.clone(), n.value.clone(), n.left.clone(), right)))
        }
        Ordering::Equal => Some(match (&n.left, &n.right) {
            (None, right) => right.clone(),
            (left, None) => left.clone(),
            (left, Some(right)) => {
                let (k, v, rest) = remove_min(right);
                Some(balanced(k, v, left.clone(), rest))
            }
        }),
    }
}

/// The least entry of a tree, and the tree without it, rebalanced.
fn remove_min(n: &Arc<Node>) -> (Text, Value, Link) {
    match &n.left {
        None => (n.key.clone(), n.value.clone(), n.right.clone()),
        Some(left) => {
            let (k, v, rest) = remove_min(left);
            (k, v, Some(balanced(n.key.clone(), n.value.clone(), rest, n.right.clone())))
        }
    }
}

/// A tree of the next `n` entries, in order: as balanced as possible (heights differ by at most one).
fn build(entries: &mut impl Iterator<Item = (Text, Value)>, n: usize) -> Link {
    if n == 0 {
        return None;
    }
    let half = n / 2;
    let left = build(entries, half);
    let (key, value) = entries.next()?;
    let right = build(entries, n.saturating_sub(half).saturating_sub(1));
    Some(node(key, value, left, right))
}

// ------------------------------------------------------------------------------------------------ dropping

/// A part of a value being released whose children may still need releasing.
enum Part {
    /// A tree node, released only by whoever holds its last reference.
    Node(Arc<Node>),
    /// The elements of an array whose last reference was released here, still to be looked at.
    Items(alloc::vec::IntoIter<Value>),
}

fn is_container(v: &Value) -> bool {
    matches!(v, Value::Array(_) | Value::Map(_))
}

/// Queues an array's storage if this was its last reference and it holds containers; otherwise the storage is only
/// let go: shared, it loses a reference; unshared but flat, its drop never recurses.
fn queue_items(work: &mut Vec<Part>, items: Option<Arc<Box<[Value]>>>) {
    if let Some(owned) = items.and_then(Arc::into_inner) {
        if owned.iter().any(is_container) {
            work.push(Part::Items(owned.into_vec().into_iter()));
        }
    }
}

/// Queues what `v` holds, or lets it go at once: a scalar's drop never recurses.
fn queue(work: &mut Vec<Part>, v: Value) {
    match v {
        Value::Array(mut array) => queue_items(work, array.items.take()),
        Value::Map(mut m) => {
            if let Some(root) = m.root.take() {
                work.push(Part::Node(root));
            }
        }
        _ => {}
    }
}

impl Node {
    /// Moves this node's subtrees and container value into `work`, leaving a node whose drop is flat.
    fn surrender(&mut self, work: &mut Vec<Part>) {
        if let Some(left) = self.left.take() {
            work.push(Part::Node(left));
        }
        if let Some(right) = self.right.take() {
            work.push(Part::Node(right));
        }
        if is_container(&self.value) {
            queue(work, core::mem::replace(&mut self.value, Value::Null));
        }
    }
}

/// Releases everything in `work` with an explicit worklist, never by recursion.
///
/// Ownership is decided atomically: [`Arc::into_inner`] gives a part to exactly one of its holders, the last, even
/// when several threads drop clones at once; every other holder only loses its reference. The last holder moves the
/// part's children into `work` before the part itself is dropped, so every drop that runs is flat. An array's
/// elements are looked at one container at a time, so the worklist holds at most a few parts per level of nesting and
/// of tree height, never a whole array's elements.
///
/// Every `Drop` of a [`Value`]'s containers ([`Array`], [`Map`], and a tree node) starts here, and none of them ever
/// lets a shared payload go by a plain field drop, so dropping any value, however deep and however large its maps,
/// and from however many threads, uses a bounded native stack.
fn release(work: &mut Vec<Part>) {
    while let Some(part) = work.pop() {
        match part {
            Part::Node(n) => {
                if let Some(mut node) = Arc::into_inner(n) {
                    node.surrender(work);
                }
            }
            Part::Items(mut items) => {
                // scalars passed over are dropped here, flat
                if let Some(child) = items.find(is_container) {
                    work.push(Part::Items(items));
                    queue(work, child);
                }
            }
        }
    }
}

impl Drop for Node {
    fn drop(&mut self) {
        let mut work = Vec::new();
        self.surrender(&mut work);
        release(&mut work);
    }
}

impl Drop for Map {
    fn drop(&mut self) {
        if let Some(mut node) = self.root.take().and_then(Arc::into_inner) {
            let mut work = Vec::new();
            node.surrender(&mut work);
            release(&mut work);
        }
    }
}

impl Drop for Array {
    fn drop(&mut self) {
        let mut work = Vec::new();
        queue_items(&mut work, self.items.take());
        release(&mut work);
    }
}

/// A map with unique text keys, no null values, keys in canonical order and nesting within [`MAX_DEPTH`]: a persistent
/// AVL tree whose nodes are shared between versions. Clones in O(1); [`Map::get`], [`Map::updated`] and [`Map::removed`]
/// are O(log n), each update creating O(log n) nodes; the weight and depth are cached. The tree is at most
/// `1.44 log2(n + 2)` high (under 92 for any `n` below `2^64`), which bounds every walk of it and every drop. Keys
/// are never hashed, and equality, encoding and hashing see only the entries in order, never the tree's shape.
#[derive(Clone)]
pub struct Map {
    root: Link,
    len: usize,
}

impl Map {
    /// The map of `entries`: null values are dropped, a repeated key (even with a null value) is
    /// [`CanonicalError::DuplicateKey`], and excess depth is [`CanonicalError::DepthExceeded`].
    pub fn new(entries: impl IntoIterator<Item = (String, Value)>) -> Result<Self, CanonicalError> {
        let mut seen: BTreeSet<String> = BTreeSet::new();
        let mut kept: Vec<(String, Value)> = Vec::new();
        for (k, v) in entries {
            if seen.contains(&k) {
                return Err(CanonicalError::DuplicateKey { key: k });
            }
            seen.insert(k.clone());
            if v != Value::Null {
                kept.push((k, v));
            }
        }
        kept.sort_by(|(a, _), (b, _)| canonical_key_order(a, b));
        Self::from_sorted(kept.into_iter().map(|(k, v)| (Text::from(k), v)).collect())
    }

    /// For entries already unique, non-null and in canonical order (the strict decoder checks all three). O(n).
    pub(crate) fn from_sorted(entries: Vec<(Text, Value)>) -> Result<Self, CanonicalError> {
        let depth = entries.iter().map(|(_, v)| v.depth()).max().unwrap_or(0).saturating_add(1);
        if depth > MAX_DEPTH {
            return Err(CanonicalError::DepthExceeded { limit: MAX_DEPTH });
        }
        let len = entries.len();
        Ok(Self { root: build(&mut entries.into_iter(), len), len })
    }

    /// The map with `key` set to `value`, or without `key` when `value` is null, in O(log n): every untouched subtree
    /// is shared with `self`. [`CanonicalError::DepthExceeded`] if the value nests too deep for a map.
    pub fn updated(&self, key: Text, value: Value) -> Result<Self, CanonicalError> {
        if matches!(value, Value::Null) {
            return Ok(self.removed(&key));
        }
        if value.depth() >= MAX_DEPTH {
            return Err(CanonicalError::DepthExceeded { limit: MAX_DEPTH });
        }
        let (root, added) = insert(&self.root, key, value);
        let len = if added { self.len.saturating_add(1) } else { self.len };
        Ok(Self { root: Some(root), len })
    }

    /// The map without `key`, in O(log n), sharing every untouched subtree with `self` (or `self` itself, cloned in
    /// O(1), when `key` is absent).
    #[must_use]
    pub fn removed(&self, key: &str) -> Self {
        remove(&self.root, key).map_or_else(|| self.clone(), |root| Self { root, len: self.len.saturating_sub(1) })
    }

    /// The value at `key`, if present, in O(log n).
    pub fn get(&self, key: &str) -> Option<&Value> {
        let mut link = self.root.as_deref();
        while let Some(n) = link {
            link = match canonical_key_order(key, &n.key) {
                Ordering::Less => n.left.as_deref(),
                Ordering::Greater => n.right.as_deref(),
                Ordering::Equal => return Some(&n.value),
            };
        }
        None
    }

    /// The entries in canonical key order. Each step is amortised O(1).
    pub fn entries(&self) -> Entries<'_> {
        let mut entries = Entries { stack: Vec::with_capacity(usize::from(height(&self.root))), remaining: self.len };
        entries.descend(self.root.as_deref());
        entries
    }

    /// The keys in canonical order.
    pub fn keys(&self) -> impl ExactSizeIterator<Item = &Text> {
        self.entries().map(|(k, _)| k)
    }

    /// The values, in canonical key order.
    pub fn values(&self) -> impl ExactSizeIterator<Item = &Value> {
        self.entries().map(|(_, v)| v)
    }

    /// The entries by value, in canonical key order: the keys are copied, the values are O(1) clones.
    pub fn into_entries(self) -> Vec<(String, Value)> {
        self.entries().map(|(k, v)| (String::from(k.as_str()), v.clone())).collect()
    }

    /// The number of entries.
    pub const fn len(&self) -> usize {
        self.len
    }

    /// Whether the map has no entries.
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// See [`Value::depth`]: one more than the deepest value, 1 when empty. O(1).
    pub fn depth(&self) -> usize {
        deepest(&self.root).saturating_add(1)
    }

    /// See [`Value::weight`]: one, plus each entry's `1 + key length + value weight`, saturating at [`MAX_WEIGHT`].
    /// O(1), and exact through updates: the tree keeps the unsaturated sum.
    pub fn weight(&self) -> u64 {
        let total = weight_sum(&self.root).saturating_add(1).min(u128::from(MAX_WEIGHT));
        u64::try_from(total).unwrap_or(MAX_WEIGHT)
    }

    /// The address of the root node (0 when empty): a key for the equality memo, never a decision by itself.
    pub(super) fn address(&self) -> usize {
        self.root.as_ref().map_or(0, |n| Arc::as_ptr(n).addr())
    }
}

/// The keys of a map, checked once: distinct, and their canonical order found. A map with exactly these keys is then
/// built from their values in O(n), without comparing a key (a program's object literal, built on every evaluation).
#[derive(Clone, Debug)]
pub struct MapShape {
    /// The keys, in the order their values are given.
    keys: Vec<Text>,
    /// Positions into `keys`, in canonical key order.
    order: Vec<usize>,
}

impl MapShape {
    /// The shape of `keys` (in the order their values will be given): [`CanonicalError::DuplicateKey`] if a key repeats.
    pub fn new(keys: Vec<Text>) -> Result<Self, CanonicalError> {
        let mut order: Vec<usize> = (0..keys.len()).collect();
        let key = |i: &usize| keys.get(*i).map_or("", Text::as_str);
        order.sort_by(|a, b| canonical_key_order(key(a), key(b)));
        if let Some(w) = order.windows(2).find(|w| matches!(w, [a, b] if key(a) == key(b))) {
            let repeated = w.first().map_or("", key);
            return Err(CanonicalError::DuplicateKey { key: String::from(repeated) });
        }
        Ok(Self { keys, order })
    }

    /// The keys, in the order their values are given.
    pub fn keys(&self) -> &[Text] {
        &self.keys
    }

    /// The number of keys.
    pub fn len(&self) -> usize {
        self.keys.len()
    }

    /// Whether there are no keys.
    pub fn is_empty(&self) -> bool {
        self.keys.is_empty()
    }

    /// The map of these keys to `values` (in the keys' given order): null values are dropped, which keeps the rest in
    /// order; [`CanonicalError::DepthExceeded`] if a value nests too deep; [`CanonicalError::TypeMismatch`] if the
    /// number of values is not the number of keys. O(n), and no key is compared or copied.
    pub fn map(&self, values: &[Value]) -> Result<Map, CanonicalError> {
        if values.len() != self.keys.len() {
            return Err(CanonicalError::TypeMismatch {
                reason: alloc::format!("{} values for {} keys", values.len(), self.keys.len()),
            });
        }
        let entries: Vec<(Text, Value)> = self
            .order
            .iter()
            .filter_map(|&i| match (self.keys.get(i), values.get(i)) {
                (Some(_), Some(Value::Null)) | (None, _) | (_, None) => None,
                (Some(k), Some(v)) => Some((k.clone(), v.clone())),
            })
            .collect();
        Map::from_sorted(entries)
    }
}

/// The entries of a [`Map`] in canonical key order, as `(key, value)`.
pub struct Entries<'a> {
    /// The nodes whose entry and right subtree are still to come: at most the tree's height.
    stack: Vec<&'a Node>,
    remaining: usize,
}

impl<'a> Entries<'a> {
    fn descend(&mut self, mut link: Option<&'a Node>) {
        while let Some(n) = link {
            self.stack.push(n);
            link = n.left.as_deref();
        }
    }
}

impl<'a> Iterator for Entries<'a> {
    type Item = (&'a Text, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        let n = self.stack.pop()?;
        self.descend(n.right.as_deref());
        self.remaining = self.remaining.saturating_sub(1);
        Some((&n.key, &n.value))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl ExactSizeIterator for Entries<'_> {}

impl FusedIterator for Entries<'_> {}

#[cfg(test)]
mod tests {
    #![allow(
        clippy::arithmetic_side_effects,
        clippy::as_conversions,
        clippy::cast_precision_loss,
        clippy::cast_possible_truncation,
        reason = "tests"
    )]

    use alloc::collections::BTreeMap;
    use alloc::format;
    use alloc::string::{String, ToString};
    use alloc::vec;
    use alloc::vec::Vec;

    use super::{Link, Map, MapShape, Node, Text, Value, canonical_key_order, entry_weight};
    use crate::value::{MAX_DEPTH, MAX_WEIGHT};

    /// Checks every node's balance, order and caches against a recount; returns (height, weight sum, depth, count).
    fn check(link: &Link, low: Option<&str>, high: Option<&str>) -> (u8, u128, usize, usize) {
        let Some(n) = link else { return (0, 0, 0, 0) };
        if let Some(low) = low {
            assert_eq!(canonical_key_order(low, &n.key), core::cmp::Ordering::Less, "order");
        }
        if let Some(high) = high {
            assert_eq!(canonical_key_order(&n.key, high), core::cmp::Ordering::Less, "order");
        }
        assert!(!matches!(n.value, Value::Null), "no null values");
        let (hl, wl, dl, cl) = check(&n.left, low, Some(&n.key));
        let (hr, wr, dr, cr) = check(&n.right, Some(&n.key), high);
        assert!(hl.abs_diff(hr) <= 1, "balanced: {hl} vs {hr}");
        let (h, w, d) = (hl.max(hr) + 1, wl + wr + entry_weight(&n.key, &n.value), dl.max(dr).max(n.value.depth()));
        assert_eq!((n.height, n.weight, n.depth), (h, w, d), "cached height, weight and depth");
        (h, w, d, cl + cr + 1)
    }

    fn checked(m: &Map) -> u8 {
        let (h, _, _, count) = check(&m.root, None, None);
        assert_eq!(count, m.len(), "len");
        h
    }

    fn addresses(link: &Link, out: &mut Vec<usize>) {
        if let Some(n) = link {
            out.push(alloc::sync::Arc::as_ptr(n).addr());
            addresses(&n.left, out);
            addresses(&n.right, out);
        }
    }

    /// xorshift64*: deterministic, no dependency.
    struct Rng(u64);

    impl Rng {
        fn next(&mut self) -> u64 {
            self.0 ^= self.0 >> 12;
            self.0 ^= self.0 << 25;
            self.0 ^= self.0 >> 27;
            self.0.wrapping_mul(0x2545_f491_4f6c_dd1d)
        }
        fn below(&mut self, n: u64) -> u64 {
            self.next() % n
        }
    }

    fn sorted_keys(n: usize) -> Vec<String> {
        let mut keys: Vec<String> = (0..n).map(|i| format!("k{i}")).collect();
        keys.sort_by(|a, b| canonical_key_order(a, b));
        keys
    }

    /// The AVL bound: height < 1.4405 log2(n + 2) - 0.3277.
    fn avl_bound(n: usize) -> f64 {
        1.4405 * ((n + 2) as f64).log2() - 0.3277
    }

    #[test]
    fn random_updates_match_a_model_and_every_cache_matches_a_recount() {
        let mut rng = Rng(0x9e37_79b9_7f4a_7c15);
        let mut map = Map::new([]).unwrap();
        // the model in canonical order: by length, then bytes
        let mut model: BTreeMap<(usize, String), Value> = BTreeMap::new();
        let deep = (0..5).fold(Value::Null, |v, _| Value::array(vec![v]).unwrap());
        for step in 0..4000 {
            let key = format!("{}", rng.below(300));
            let value = match rng.below(6) {
                0 => Value::Null,
                1 => deep.clone(),
                2 => Value::text("x".repeat(rng.below(40) as usize)),
                3 => Value::int(rng.next()),
                _ => Value::int(rng.below(10)),
            };
            let before = map.clone();
            let before_entries: Vec<(String, Value)> = before.clone().into_entries();
            if rng.below(4) == 0 {
                map = map.removed(&key);
                model.remove(&(key.len(), key.clone()));
            } else {
                map = map.updated(Text::from(key.as_str()), value.clone()).unwrap();
                if matches!(value, Value::Null) {
                    model.remove(&(key.len(), key.clone()));
                } else {
                    model.insert((key.len(), key.clone()), value);
                }
            }
            assert_eq!(before.into_entries(), before_entries, "step {step}: the original is unchanged");
            checked(&map);
            let expected: Vec<(String, Value)> = model.iter().map(|((_, k), v)| (k.clone(), v.clone())).collect();
            assert_eq!(map.clone().into_entries(), expected, "step {step}: entries");
            let rebuilt = Map::new(expected.clone()).unwrap();
            assert_eq!(map.weight(), rebuilt.weight(), "step {step}: weight equals a fresh build");
            assert_eq!(map.depth(), rebuilt.depth(), "step {step}: depth equals a fresh build");
            assert!(map == rebuilt, "step {step}: equal to a fresh build of another shape");
            let probe = format!("{}", rng.below(300));
            assert_eq!(map.get(&probe), model.get(&(probe.len(), probe.clone())), "step {step}: get {probe}");
        }
    }

    #[test]
    fn adversarial_orders_stay_logarithmic() {
        let n = 100_000;
        let keys = sorted_keys(n);
        let reversed: Vec<String> = keys.iter().rev().cloned().collect();
        let alternating: Vec<String> =
            (0..n).map(|i| if i % 2 == 0 { keys[i / 2].clone() } else { keys[n - 1 - i / 2].clone() }).collect();
        for (name, order) in [("sorted", &keys), ("reversed", &reversed), ("alternating", &alternating)] {
            let map = order
                .iter()
                .fold(Map::new([]).unwrap(), |m, k| m.updated(Text::from(k.as_str()), Value::int(1)).unwrap());
            let h = checked(&map);
            assert!(f64::from(h) <= avl_bound(n), "{name}: height {h} over {}", avl_bound(n));
            // and removing every other key keeps it balanced
            let thinned = order.iter().step_by(2).fold(map, |m, k| m.removed(k));
            let h = checked(&thinned);
            assert!(f64::from(h) <= avl_bound(n / 2), "{name} thinned: height {h}");
        }
    }

    #[test]
    fn an_update_copies_one_path_and_shares_the_rest() {
        let n = 10_000;
        let map = Map::new(sorted_keys(n).into_iter().map(|k| (k, Value::int(1)))).unwrap();
        let h = usize::from(checked(&map));
        let mut old = Vec::new();
        addresses(&map.root, &mut old);
        old.sort_unstable();
        for change in [
            map.updated(Text::from("k5000"), Value::int(2)).unwrap(),
            map.updated(Text::from("new"), Value::int(2)).unwrap(),
            map.removed("k5000"),
        ] {
            let mut new = Vec::new();
            addresses(&change.root, &mut new);
            let fresh = new.iter().filter(|a| old.binary_search(a).is_err()).count();
            assert!(fresh <= 2 * h + 2, "{fresh} new nodes for height {h}");
            assert_eq!(map.get("k5000"), Some(&Value::int(1)), "the original is unchanged");
            assert_eq!(map.len(), n, "the original keeps its length");
        }
    }

    #[test]
    fn replacing_or_removing_the_deepest_value_lowers_the_depth() {
        let deep = (0..6).fold(Value::Null, |v, _| Value::array(vec![v]).unwrap());
        let map = Map::new([("a".to_string(), Value::int(1)), ("b".to_string(), deep.clone())]).unwrap();
        assert_eq!(map.depth(), 7);
        assert_eq!(map.updated(Text::from("b"), Value::int(2)).unwrap().depth(), 1, "replaced");
        assert_eq!(map.removed("b").depth(), 1, "removed");
        assert_eq!(map.updated(Text::from("b"), Value::Null).unwrap().len(), 1, "null removes");
        let too_deep = (0..MAX_DEPTH).fold(Value::Null, |v, _| Value::array(vec![v]).unwrap());
        assert!(map.updated(Text::from("c"), too_deep).is_err(), "depth checked");
        assert_eq!(map.len(), 2, "unchanged by a refused update");
    }

    #[test]
    fn the_weight_stays_exact_through_saturation() {
        // never format or compare these: each holds 2^61 or more leaves
        let doubled = |k: usize| (0..k).fold(Value::text("a"), |v, _| Value::array(vec![v.clone(), v]).unwrap());
        let big = doubled(61);
        let big_weight = 3 * (1u64 << 61) - 1;
        assert_eq!(big.weight(), big_weight, "not saturated");
        let entry = |w: u64| 1 + 1 + w; // a one-byte key
        let m = Map::new([])
            .unwrap()
            .updated(Text::from("x"), big.clone())
            .and_then(|m| m.updated(Text::from("y"), big.clone()))
            .and_then(|m| m.updated(Text::from("z"), Value::int(1)))
            .unwrap();
        assert_eq!(m.weight(), MAX_WEIGHT, "two big entries pass the cap");
        let without_y = m.removed("y");
        let exact = 1 + entry(big_weight) + entry(1);
        assert_eq!(without_y.weight(), exact, "exact again after a removal");
        let fresh = Map::new([("x".to_string(), big.clone()), ("z".to_string(), Value::int(1))]).unwrap();
        assert_eq!(without_y.weight(), fresh.weight(), "as a fresh build");
        assert_eq!(m.updated(Text::from("y"), Value::int(1)).unwrap().weight(), exact + entry(1), "after a replace");
        assert_eq!(m.removed("x").removed("y").weight(), 1 + entry(1), "both removed");
        // an entry whose own weight is saturated
        let saturated = doubled(62);
        assert_eq!(saturated.weight(), MAX_WEIGHT);
        let s = Map::new([("x".to_string(), saturated), ("z".to_string(), Value::int(1))]).unwrap();
        assert_eq!(s.weight(), MAX_WEIGHT, "saturated");
        assert_eq!(s.removed("x").weight(), 1 + entry(1), "exact once the saturated entry is gone");
    }

    #[test]
    fn a_node_type_stays_small() {
        assert!(core::mem::size_of::<Node>() <= 112, "{}", core::mem::size_of::<Node>());
        assert_eq!(core::mem::size_of::<Map>(), 16);
    }

    #[test]
    fn a_shape_builds_the_map_of_its_keys() {
        let keys: Vec<Text> = ["bb", "a", "ccc", "d"].into_iter().map(Text::from).collect();
        let shape = MapShape::new(keys).unwrap();
        let values = [Value::int(1), Value::int(2), Value::Null, Value::text("x")];
        let built = shape.map(&values).unwrap();
        let expected = Map::new(
            [("bb", Value::int(1)), ("a", Value::int(2)), ("d", Value::text("x"))].map(|(k, v)| (String::from(k), v)),
        )
        .unwrap();
        assert_eq!(Value::Map(built.clone()), Value::Map(expected.clone()));
        assert_eq!(built.weight(), expected.weight());
        assert_eq!(built.depth(), expected.depth());
        // the keys are the shape's own, shared
        let a = shape.keys().get(1).map(|k| k.as_ptr());
        assert_eq!(built.keys().find(|k| k.as_str() == "a").map(|k| k.as_ptr()), a);
        assert!(MapShape::new(vec![Text::from("k"), Text::from("k")]).is_err());
        assert!(shape.map(&values[..3]).is_err());
    }

    #[test]
    fn text_counts_its_utf16_units_once() {
        assert_eq!(Text::from("a\u{e9}\u{1F600}").utf16_len(), 4);
        assert_eq!(Text::from("").utf16_len(), 0);
    }
}
