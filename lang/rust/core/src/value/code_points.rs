//! A map's entries in code-point order of their keys (the order of their UTF-8 bytes alone), from the canonical order
//! the map keeps them in, in time linear in the map's weight: the order JSON writes an object's members in, and the
//! JLVM's `keys`, `values` and `entries` list them in (`docs/specs/jlvm.md` section 5.4). The TypeScript core derives
//! it the same way (`codePointOrder` in `lang/ts/packages/core/src/tree.ts`).

use alloc::vec::Vec;

use super::{Text, Value};

/// A map entry, borrowed from its map.
pub(super) type Entry<'a> = (&'a Text, &'a Value);

/// The length of the common prefix of `a` and `b`, which agree on their first `from` bytes, adding to `reads` the
/// pairs of bytes read: those that match, and the one that differs unless either ends first.
fn common(a: &[u8], b: &[u8], from: usize, reads: &mut u64) -> usize {
    let (ra, rb) = (a.get(from..).unwrap_or_default(), b.get(from..).unwrap_or_default());
    let matched = ra.iter().zip(rb).take_while(|(x, y)| x == y).count();
    let l = from.saturating_add(matched);
    let differs = usize::from(l < a.len().min(b.len()));
    *reads = reads.saturating_add(u64::try_from(matched.saturating_add(differs)).unwrap_or(u64::MAX));
    l
}

/// Entries in byte order of their keys, and each one's common prefix with the one before it (0 for the first).
struct Run<'a> {
    items: Vec<Entry<'a>>,
    lcp: Vec<usize>,
}

/// Two runs merged into one, in byte order, by their common prefixes (Ng and Kakehi's LCP merge, "Merging String
/// Sequences by Longest Common Prefixes", IPSJ Digital Courier 4, 2008). With `o` the last entry output, `ha` and `hb`
/// are each head's common prefix with `o`. The head sharing more with `o` comes first, read without a byte: both follow
/// `o`, and the other differs from `o` earlier, upward. Only when they share as much is a byte read, from that common
/// length on, and the one not taken then shares exactly the bytes read with the new `o`. So a byte that matches raises
/// an entry's common prefix with the entry before it, which never falls from one merge to the next, and is read once per
/// entry over every merge; a byte that differs is read once per entry output.
fn merged<'a>(left: &Run<'a>, right: &Run<'a>, reads: &mut u64) -> Run<'a> {
    let total = left.items.len().saturating_add(right.items.len());
    let mut items: Vec<Entry<'a>> = Vec::with_capacity(total);
    let mut lcp: Vec<usize> = Vec::with_capacity(total);
    // the heads' positions, and their common prefixes with the last entry output
    let (mut at_left, mut at_right, mut ha, mut hb) = (0usize, 0usize, 0usize, 0usize);
    while let (Some(&x), Some(&y)) = (left.items.get(at_left), right.items.get(at_right)) {
        let h = ha.max(hb);
        let first = if ha == hb {
            let (xs, ys) = (x.0.as_bytes(), y.0.as_bytes());
            let l = common(xs, ys, h, reads);
            // keys of different lengths, never equal: a prefix sorts first
            let first = l == xs.len() || (l < ys.len() && xs.get(l) < ys.get(l));
            if first {
                hb = l;
            } else {
                ha = l;
            }
            first
        } else {
            ha > hb
        };
        lcp.push(h);
        if first {
            items.push(x);
            at_left = at_left.saturating_add(1);
            ha = left.lcp.get(at_left).copied().unwrap_or(0);
        } else {
            items.push(y);
            at_right = at_right.saturating_add(1);
            hb = right.lcp.get(at_right).copied().unwrap_or(0);
        }
    }
    // the rest of one run follows: its head shares its h with the last entry output, each after it its own lcp
    let (rest, from, h) = if at_left < left.items.len() { (left, at_left, ha) } else { (right, at_right, hb) };
    let tail = rest.items.iter().zip(&rest.lcp).skip(from);
    for (k, (&e, &own)) in tail.enumerate() {
        items.push(e);
        lcp.push(if k == 0 { h } else { own });
    }
    Run { items, lcp }
}

/// `canonical`, a map's entries in canonical key order, rearranged into code-point order of the keys: the order of
/// their UTF-8 bytes alone. Keys of one UTF-8 length are in that order already, so the runs of each length are merged,
/// pairwise in rounds, by [`merged`].
///
/// Linear in the map's weight `W`, whatever the keys. With `n` keys of `S` UTF-8 bytes in all, in `k` runs, the rounds
/// output `n ceil(log2 k)` entries, each after at most one pair of bytes that differ; and every pair that matches raises
/// one entry's common prefix with its predecessor, which no round lowers, so each key's bytes match at most once over all
/// rounds (`S`). Finding the runs' own prefixes reads at most `S + n` pairs more. `n` distinct byte strings have
/// `n log2 n < 8 S + 2.45 n` (fewer than `2 * 256^L` strings are `L` bytes or shorter), and every entry adds at least
/// `2 +` its key's length to `W` (`W > S + 2 n`), so the pairs read are at most `2 S + n (log2 n + 2) < 10 W`, and so
/// are the entries moved. Keys of one length (the first and last are as long: the lengths only grow) are already in
/// code-point order, and `canonical` itself is the answer, in O(1).
pub(super) fn ordered(canonical: Vec<Entry<'_>>) -> Vec<Entry<'_>> {
    counted(canonical).0
}

/// [`ordered`], and the pairs of bytes it read.
pub(super) fn counted(canonical: Vec<Entry<'_>>) -> (Vec<Entry<'_>>, u64) {
    let mut reads = 0u64;
    let one_length = match (canonical.first(), canonical.last()) {
        (Some(first), Some(last)) => first.0.len() == last.0.len(),
        _ => true,
    };
    if one_length {
        return (canonical, reads);
    }
    let mut runs: Vec<Run<'_>> = Vec::new();
    for e in canonical {
        let open = runs.last_mut().filter(|r| r.items.last().is_some_and(|before| before.0.len() == e.0.len()));
        match open {
            Some(run) => {
                let before = run.items.last().map_or(&[][..], |b| b.0.as_bytes());
                run.lcp.push(common(before, e.0.as_bytes(), 0, &mut reads));
                run.items.push(e);
            }
            None => runs.push(Run { items: alloc::vec![e], lcp: alloc::vec![0] }),
        }
    }
    while runs.len() > 1 {
        let mut next: Vec<Run<'_>> = Vec::with_capacity(runs.len().div_ceil(2));
        let mut pairs = runs.into_iter();
        while let Some(left) = pairs.next() {
            next.push(match pairs.next() {
                Some(right) => merged(&left, &right, &mut reads),
                None => left,
            });
        }
        runs = next;
    }
    (runs.pop().map(|r| r.items).unwrap_or_default(), reads)
}

#[cfg(test)]
mod tests {
    #![allow(
        clippy::arithmetic_side_effects,
        clippy::as_conversions,
        clippy::cast_possible_truncation,
        clippy::cast_precision_loss,
        clippy::cast_sign_loss,
        reason = "tests"
    )]

    use alloc::format;
    use alloc::string::String;
    use alloc::vec::Vec;
    use core::cmp::Ordering;

    use super::counted;
    use crate::value::{Map, Text, Value};

    /// The map of `keys`, each to 1.
    fn map_of(keys: impl IntoIterator<Item = String>) -> Map {
        Map::new(keys.into_iter().map(|k| (k, Value::int(1)))).unwrap()
    }

    /// The order before: a comparison sort, counting the pairs of bytes each comparison reads (as [`super::common`]
    /// counts them).
    fn old_sort(m: &Map) -> (Vec<String>, u64) {
        let mut reads = 0u64;
        let mut keys: Vec<&str> = m.keys().map(Text::as_str).collect();
        keys.sort_by(|a, b| {
            let (a, b) = (a.as_bytes(), b.as_bytes());
            let l = a.iter().zip(b).take_while(|(x, y)| x == y).count();
            reads += (l + usize::from(l < a.len().min(b.len()))) as u64;
            a.cmp(b)
        });
        (keys.into_iter().map(String::from).collect(), reads)
    }

    fn new_order(m: &Map) -> (Vec<String>, u64) {
        let (entries, reads) = counted(m.entries().collect());
        (entries.into_iter().map(|(k, _)| String::from(k.as_str())).collect(), reads)
    }

    /// The bound the proof gives before rounding to the weight: `2 S + n (ceil(log2 k) + 1)`, for `n` keys of `S` UTF-8
    /// bytes in `k` runs of one length.
    fn bound(m: &Map) -> u64 {
        let n = m.len() as u64;
        let s: u64 = m.keys().map(|k| k.len() as u64).sum();
        let mut lengths: Vec<usize> = m.keys().map(|k| k.len()).collect();
        lengths.dedup();
        let k = lengths.len() as u64;
        let rounds = if k <= 1 { 0 } else { 64 - u64::from((k - 1).leading_zeros()) };
        2 * s + n * (rounds + 1)
    }

    /// `prefix`, then one of `letters` letters, then `a` repeated `0..lengths` times: every length interleaves with every
    /// other in code-point order, so the canonical order's `lengths` runs all merge, under a long shared prefix.
    fn adversarial(prefix: usize, letters: u8, lengths: usize) -> Map {
        let p = "p".repeat(prefix);
        map_of((0..letters).flat_map(|c| {
            let p = p.clone();
            (0..lengths).map(move |j| format!("{p}{}{}", char::from(b'b' + c), "a".repeat(j)))
        }))
    }

    #[test]
    fn the_listing_reads_linearly_in_the_weight_where_a_comparison_sort_reads_log_n_times_more() {
        let m = adversarial(256, 64, 64);
        let (old, old_reads) = old_sort(&m);
        let (new, new_reads) = new_order(&m);
        let (b, w) = (bound(&m), m.weight());
        assert_eq!(new, old, "the same order");
        assert!(new_reads <= b, "{new_reads} pairs read, over the proof's bound {b}");
        assert!(new_reads < 10 * w, "{new_reads} pairs read, over 10 W = {}", 10 * w);
        assert!(old_reads > 2 * b, "the comparison sort reads {old_reads} pairs, not over twice the bound {b}");
    }

    #[test]
    fn keys_of_one_length_are_listed_as_they_are_without_a_read() {
        let m = map_of((0..1000).map(|i| format!("{}{i:04}", "p".repeat(300))));
        let (new, reads) = new_order(&m);
        assert_eq!(new, old_sort(&m).0);
        assert_eq!(reads, 0);
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

    #[test]
    fn the_order_is_the_code_point_order_of_any_keys() {
        // 1-, 2-, 3- and 4-byte code points, the empty key, prefixes, and U+E000..U+FFFF against supplementary ones
        let alphabet: Vec<char> =
            "ab\u{7f}\u{80}\u{7ff}\u{800}\u{d7ff}\u{e000}\u{ffff}\u{10000}\u{10ffff}".chars().collect();
        let mut rng = Rng(0x9e37_79b9_7f4a_7c15);
        for _ in 0..500 {
            let n = rng.below(40) as usize;
            let keys: Vec<String> = (0..n)
                .map(|_| (0..rng.below(6)).map(|_| alphabet[rng.below(alphabet.len() as u64) as usize]).collect())
                .collect();
            let mut unique = keys.clone();
            unique.sort();
            unique.dedup();
            let m = map_of(unique.iter().cloned());
            let (new, reads) = new_order(&m);
            assert_eq!(new, unique, "{keys:?}");
            assert!(reads <= bound(&m), "{keys:?}: {reads} pairs read, over {}", bound(&m));
            // the listing pairs every key with its own value
            let (entries, _) = counted(m.entries().collect());
            assert!(entries.iter().all(|(k, v)| m.get(k) == Some(*v)));
            assert!(entries.windows(2).all(|w| w[0].0.as_bytes().cmp(w[1].0.as_bytes()) == Ordering::Less));
        }
    }
}
