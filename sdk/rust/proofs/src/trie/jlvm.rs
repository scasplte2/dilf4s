//! The JLVM's trie operations (docs/specs/jlvm.md §6): `trie_verify`, `trie_prefix_verify` and `trie_range_verify`.
//!
//! A proof of another trie (a root mismatch) or of other contents is `false`. Malformed input (a root of the wrong
//! length, an undecodable proof, a key invalid for the mode, a structurally invalid proof) is an error, and so is a
//! proof that does not reach the query (`TrieError::NotCovered`): it establishes neither answer.

use dilf4s_core::hash::{Digest, Hasher};
use dilf4s_core::value::Value;

use super::{KeyMode, Listing, Lookup, TrieError, TrieHashing, TrieProof, encoding};

fn root<H: Hasher>(bytes: &[u8]) -> Result<Digest<H>, TrieError> {
    Digest::<H>::from_slice(bytes).map_err(encoding)
}

/// Claimed entries as a listing: each key valid for the mode, each value non-null and committed by `commit(v)`.
fn claimed<H: Hasher>(
    hashing: &TrieHashing<H>,
    mode: KeyMode,
    entries: &[(&[u8], &Value)],
) -> Result<Listing<H>, TrieError> {
    entries
        .iter()
        .map(|(k, v)| {
            mode.check_key(k)?;
            if **v == Value::Null {
                return Err(encoding("an entry is [key, value], with a value"));
            }
            Ok((k.to_vec(), hashing.value_of(v)))
        })
        .collect()
}

fn listing<H: Hasher>(claimed: &Listing<H>, proven: Option<Listing<H>>) -> Result<bool, TrieError> {
    proven
        .map(|l| l == *claimed)
        .ok_or_else(|| TrieError::NotCovered { what: alloc::string::String::from("the whole query") })
}

/// `trie_verify [mode, root, key, value, proof]`: `key` holds `value` (committed by `commit(v)`); a `Value::Null`
/// value claims absence.
pub fn trie_verify<H: Hasher>(
    hashing: &TrieHashing<H>,
    mode: KeyMode,
    root_bytes: &[u8],
    key: &[u8],
    value: &Value,
    proof: &Value,
) -> Result<bool, TrieError> {
    let path = mode.path(key)?;
    let r = root::<H>(root_bytes)?;
    let p = TrieProof::<H>::from_value(proof)?;
    let Some(v) = p.verify(hashing, mode, &r)? else { return Ok(false) };
    match v.lookup_bits(&path) {
        Lookup::Present(h) => Ok(*value != Value::Null && hashing.value_of(value) == h),
        Lookup::Absent => Ok(*value == Value::Null),
        Lookup::NotCovered => Err(TrieError::NotCovered { what: alloc::string::String::from("the key") }),
    }
}

/// `trie_prefix_verify [mode, root, prefix, entries, proof]`: `entries` (in key order) are exactly the trie's entries
/// under `prefix`.
pub fn trie_prefix_verify<H: Hasher>(
    hashing: &TrieHashing<H>,
    mode: KeyMode,
    root_bytes: &[u8],
    prefix: &[u8],
    entries: &[(&[u8], &Value)],
    proof: &Value,
) -> Result<bool, TrieError> {
    let r = root::<H>(root_bytes)?;
    let claimed = claimed(hashing, mode, entries)?;
    let p = TrieProof::<H>::from_value(proof)?;
    let Some(v) = p.verify(hashing, mode, &r)? else { return Ok(false) };
    listing(&claimed, v.prefix(prefix))
}

/// `trie_range_verify [mode, root, from, until, entries, proof]`: `entries` (in key order) are exactly the trie's
/// entries in `[from, until)`; no `until` is unbounded.
pub fn trie_range_verify<H: Hasher>(
    hashing: &TrieHashing<H>,
    mode: KeyMode,
    root_bytes: &[u8],
    from: &[u8],
    until: Option<&[u8]>,
    entries: &[(&[u8], &Value)],
    proof: &Value,
) -> Result<bool, TrieError> {
    let r = root::<H>(root_bytes)?;
    let claimed = claimed(hashing, mode, entries)?;
    let p = TrieProof::<H>::from_value(proof)?;
    let Some(v) = p.verify(hashing, mode, &r)? else { return Ok(false) };
    listing(&claimed, v.range(from, until))
}
