//! The root of a trie from its entries (Scala's `Trie.fromEntries`), built with an explicit work stack.

use alloc::collections::BTreeMap;
use alloc::vec::Vec;

use dilf4s_core::hash::{Digest, Hasher};

use super::{KeyMode, TrieError, TrieHashing};
use crate::bits::Bits;

enum Work<H: Hasher> {
    /// Build the subtree of `entries[lo..hi]`, whose keys share their first `offset` bits.
    Build { lo: usize, hi: usize, offset: u64 },
    /// Hash a branch from the results its children left on the stack (right on top).
    Combine { segment: Bits, value: Option<Digest<H>>, left: bool, right: bool },
}

/// The root of the trie holding `entries` (later duplicates win), each key checked against `mode`.
pub fn root_from_entries<'a, H: Hasher + 'a>(
    hashing: &TrieHashing<H>,
    mode: KeyMode,
    entries: impl IntoIterator<Item = (&'a [u8], Digest<H>)>,
) -> Result<Digest<H>, TrieError> {
    let mut sorted: BTreeMap<Vec<u8>, Digest<H>> = BTreeMap::new();
    for (k, v) in entries {
        mode.check_key(k)?;
        sorted.insert(k.to_vec(), v);
    }
    root_from_sorted(hashing, mode, &sorted)
}

/// The root of the trie holding `entries`. For byte keys, `Vec<u8>` order is the trie's key order (a prefix before
/// its extensions).
pub fn root_from_sorted<H: Hasher>(
    hashing: &TrieHashing<H>,
    mode: KeyMode,
    entries: &BTreeMap<Vec<u8>, Digest<H>>,
) -> Result<Digest<H>, TrieError> {
    let mut es: Vec<(Bits, &Digest<H>)> = Vec::with_capacity(entries.len());
    for (k, v) in entries {
        es.push((mode.path(k)?, v));
    }
    if es.is_empty() {
        return Ok(hashing.empty());
    }
    let mut work = alloc::vec![Work::Build { lo: 0, hi: es.len(), offset: 0 }];
    let mut results: Vec<Digest<H>> = Vec::new();
    let missing = || TrieError::InvalidNode { reason: alloc::string::String::from("builder state") };
    while let Some(w) = work.pop() {
        match w {
            Work::Build { lo, hi, offset } => {
                let group = es.get(lo..hi).ok_or_else(missing)?;
                let (Some((first, first_value)), Some((last, _))) = (group.first(), group.last()) else {
                    return Err(missing());
                };
                if group.len() == 1 {
                    results.push(hashing.leaf(&first.slice(offset, first.len()), first_value));
                    continue;
                }
                let common = first.common_prefix_from(offset, last, offset);
                let pos = offset.saturating_add(common);
                let segment = first.slice(offset, pos);
                // the key ending here sorts first; the rest split by the next bit, zeros first
                let (value, start) =
                    if first.len() == pos { (Some((*first_value).clone()), lo.saturating_add(1)) } else { (None, lo) };
                let rest = es.get(start..hi).ok_or_else(missing)?;
                let split = start.saturating_add(rest.partition_point(|(k, _)| k.get(pos) != Some(true)));
                let (left, right) = (split > start, hi > split);
                work.push(Work::Combine { segment, value, left, right });
                let next = pos.saturating_add(1);
                if right {
                    work.push(Work::Build { lo: split, hi, offset: next });
                }
                if left {
                    work.push(Work::Build { lo: start, hi: split, offset: next });
                }
            }
            Work::Combine { segment, value, left, right } => {
                let r = if right { Some(results.pop().ok_or_else(missing)?) } else { None };
                let l = if left { Some(results.pop().ok_or_else(missing)?) } else { None };
                results.push(hashing.branch(&segment, value.as_ref(), l.as_ref(), r.as_ref()));
            }
        }
    }
    results.pop().ok_or_else(missing)
}
