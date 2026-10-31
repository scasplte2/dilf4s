//! Verification against a trusted root (docs/specs/versioned-trie.md §6). Every check is the one a JLVM program makes
//! with the trie operations, and the verifier supplies every key: no proof can redirect a query. "Not covered" is an
//! error, never a negative answer; so is a trie root mismatch here.

use alloc::string::String;
use alloc::vec::Vec;

use dilf4s_core::hash::{Digest, Hasher};
use dilf4s_core::value::Value;

use super::wire::{At, VersionedTrieProof};
use super::{
    HISTORY_SLOT, Head, STATE_SLOT, StateKey, StateNamespace, VERSION_SLOT, Version, VersionKey, VersionedTrie,
    VersionedTrieError, malformed, mismatch,
};
use crate::trie::{KeyMode, Lookup, TrieHashing, TrieProof, VerifiedProof};

/// A state proof checked against its state root: what it shows about keys, namespaces and ranges.
pub struct VerifiedState<'a, H: Hasher> {
    proof: VerifiedProof<'a, H>,
    hashing: &'a TrieHashing<H>,
}

impl<H: Hasher> core::fmt::Debug for VerifiedState<'_, H> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("VerifiedState").finish_non_exhaustive()
    }
}

fn not_covered(what: String) -> VersionedTrieError {
    VersionedTrieError::NotCovered { what }
}

fn keyed<H: Hasher>(listing: Vec<(Vec<u8>, Digest<H>)>) -> Result<Vec<(StateKey, Digest<H>)>, VersionedTrieError> {
    listing.into_iter().map(|(k, h)| Ok((StateKey::from_bytes(&k)?, h))).collect()
}

impl<H: Hasher> VerifiedState<'_, H> {
    /// The key's value hash, or `None` if it is proven absent.
    pub fn lookup(&self, key: &StateKey) -> Result<Option<Digest<H>>, VersionedTrieError> {
        match self.proof.lookup(key.as_bytes()) {
            Lookup::Present(h) => Ok(Some(h)),
            Lookup::Absent => Ok(None),
            Lookup::NotCovered => Err(not_covered(alloc::format!("key {key}"))),
        }
    }

    /// Whether the key holds `value` (`None`, or a null value: is absent), as `trie_verify` decides it. Absent equals
    /// null in the data model, and a null value is never stored.
    pub fn holds(&self, key: &StateKey, value: Option<&Value>) -> Result<bool, VersionedTrieError> {
        let claimed = value.filter(|v| !matches!(v, Value::Null)).map(|v| self.hashing.value_of(v));
        Ok(self.lookup(key)? == claimed)
    }

    /// Every entry under the namespace, complete, in key order.
    pub fn namespace(&self, ns: &StateNamespace) -> Result<Vec<(StateKey, Digest<H>)>, VersionedTrieError> {
        keyed(self.proof.prefix(&ns.prefix()).ok_or_else(|| not_covered(alloc::format!("namespace {}", ns.as_str())))?)
    }

    /// Every entry in `[from, until)`, complete, in key order.
    pub fn range(
        &self,
        from: &StateKey,
        until: Option<&StateKey>,
    ) -> Result<Vec<(StateKey, Digest<H>)>, VersionedTrieError> {
        let listing = self.proof.range(from.as_bytes(), until.map(StateKey::as_bytes));
        keyed(
            listing
                .ok_or_else(|| not_covered(alloc::format!("range [{from}, {})", until.map_or("", StateKey::as_str))))?,
        )
    }
}

impl<K: VersionKey, H: Hasher> VersionedTrie<K, H> {
    fn checked<'a>(
        &self,
        mode: KeyMode,
        root: &Digest<H>,
        what: &str,
        proof: Option<&'a TrieProof<H>>,
    ) -> Result<VerifiedProof<'a, H>, VersionedTrieError> {
        let p = proof.ok_or_else(|| malformed(alloc::format!("the proof has no {what} proof")))?;
        Ok(p.check(&self.hashing, mode, root)?)
    }

    /// Rule 1: the head's top trie hashes to `root`, and the proof's top trie verifies with every slot present and
    /// holding the head's values.
    fn bind<'p>(
        &self,
        root: &Digest<H>,
        proof: &'p VersionedTrieProof<K::Version, H>,
    ) -> Result<&'p Head<K::Version, H>, VersionedTrieError> {
        let claimed = self.root(&proof.head)?;
        if &claimed != root {
            return Err(mismatch("root", root, &claimed));
        }
        let top = self.checked(self.top, root, "top", Some(&proof.top))?;
        let vkey = self.vkey(&proof.head.version)?;
        let slots = [
            (VERSION_SLOT, vkey.as_slice()),
            (STATE_SLOT, proof.head.state_root.as_bytes()),
            (HISTORY_SLOT, proof.head.history_root.as_bytes()),
        ];
        for (slot, value) in slots {
            match top.lookup(&[slot]) {
                Lookup::Present(h) if h == self.bytes_hash(value) => {}
                Lookup::NotCovered => return Err(not_covered(alloc::format!("top slot {slot:02x}"))),
                _ => return Err(malformed(alloc::format!("top slot {slot:02x} does not hold the head's value"))),
            }
        }
        Ok(&proof.head)
    }

    fn history<'p>(
        &self,
        head: &Head<K::Version, H>,
        proof: &'p VersionedTrieProof<K::Version, H>,
    ) -> Result<VerifiedProof<'p, H>, VersionedTrieError> {
        self.checked(self.history, &head.history_root, "history", proof.history.as_ref())
    }

    fn asked<'p>(
        &self,
        proof: &'p VersionedTrieProof<K::Version, H>,
        u: &K::Version,
    ) -> Result<&'p At<K::Version, H>, VersionedTrieError> {
        let at = proof.at.as_ref().ok_or_else(|| malformed("the proof names no version"))?;
        let (a, b) = (self.vkey(&at.version)?, self.vkey(u)?);
        if a == b {
            Ok(at)
        } else {
            Err(malformed(alloc::format!(
                "the proof is about version {}, not {}",
                dilf4s_core::hex::encode(&a),
                dilf4s_core::hex::encode(&b)
            )))
        }
    }

    /// The state root of `u` per the history trie: `Some` if committed, `None` if proven never committed.
    fn version_root(
        &self,
        head: &Head<K::Version, H>,
        proof: &VersionedTrieProof<K::Version, H>,
        u: &K::Version,
    ) -> Result<Option<Digest<H>>, VersionedTrieError> {
        let at = self.asked(proof, u)?;
        let hist = self.history(head, proof)?;
        let vkey = self.vkey(u)?;
        let hex = dilf4s_core::hex::encode(&vkey);
        match (hist.lookup(&vkey), &at.state_root) {
            (Lookup::Present(h), Some(s)) => {
                if h == self.bytes_hash(s.as_bytes()) {
                    Ok(Some(s.clone()))
                } else {
                    Err(malformed(alloc::format!("history holds another state root for {hex}")))
                }
            }
            (Lookup::Absent, None) => Ok(None),
            (Lookup::NotCovered, _) => Err(not_covered(alloc::format!("version {hex}"))),
            (Lookup::Present(_), None) => {
                Err(malformed(alloc::format!("version {hex} is committed; the proof claims it is not")))
            }
            (Lookup::Absent, Some(_)) => {
                Err(malformed(alloc::format!("version {hex} is not committed; the proof claims a state")))
            }
        }
    }

    /// Rule 7: the head bound to `root`, with the head invariant: the history trie holds the head version with the
    /// head's state root, and nothing after it.
    pub fn verify_head(
        &self,
        root: &Digest<H>,
        proof: &VersionedTrieProof<K::Version, H>,
    ) -> Result<Head<K::Version, H>, VersionedTrieError> {
        let head = self.bind(root, proof)?;
        let hist = self.history(head, proof)?;
        let vkey = self.vkey(&head.version)?;
        let tail =
            hist.range(&vkey, None).ok_or_else(|| not_covered(String::from("history from the head version up")))?;
        if tail == [(vkey, self.bytes_hash(head.state_root.as_bytes()))] {
            Ok(head.clone())
        } else {
            Err(malformed("the history trie does not end at the head version with the head's state root"))
        }
    }

    /// Rule 2: a proof about the current state, its state part checked against the head's state root.
    pub fn verify_state<'p>(
        &'p self,
        root: &Digest<H>,
        proof: &'p VersionedTrieProof<K::Version, H>,
    ) -> Result<VerifiedState<'p, H>, VersionedTrieError> {
        let head = self.bind(root, proof)?;
        if proof.at.is_some() {
            return Err(malformed("a historical proof is not about the current state"));
        }
        let state = self.checked(self.state, &head.state_root, "state", proof.state.as_ref())?;
        Ok(VerifiedState { proof: state, hashing: &self.hashing })
    }

    /// Rule 3: whether version `u` was committed by the head: its state root, or `None` if it provably never was.
    pub fn verify_version(
        &self,
        root: &Digest<H>,
        proof: &VersionedTrieProof<K::Version, H>,
        u: &K::Version,
    ) -> Result<Option<Digest<H>>, VersionedTrieError> {
        let head = self.bind(root, proof)?;
        self.version_root(head, proof, u)
    }

    /// Rule 4: the state of a past version `u`: the history gives its state root, which the state part is checked
    /// against. A version never committed is `UnknownVersion`.
    pub fn verify_historical_key<'p>(
        &'p self,
        root: &Digest<H>,
        proof: &'p VersionedTrieProof<K::Version, H>,
        u: &K::Version,
    ) -> Result<VerifiedState<'p, H>, VersionedTrieError> {
        let head = self.bind(root, proof)?;
        let uk = self.vkey(u)?;
        let s = self.version_root(head, proof, u)?.ok_or(VersionedTrieError::UnknownVersion { version: uk })?;
        let p = proof.state.as_ref().ok_or_else(|| malformed("the proof has no state proof"))?;
        let state = p.check(&self.hashing, self.state, &s)?;
        Ok(VerifiedState { proof: state, hashing: &self.hashing })
    }

    /// Rule 6: whether `claimed` is exactly the committed versions in `[from, until)` with their state roots, in
    /// order (no `until` is unbounded).
    pub fn verify_versions(
        &self,
        root: &Digest<H>,
        proof: &VersionedTrieProof<K::Version, H>,
        from: &K::Version,
        until: Option<&K::Version>,
        claimed: &[(K::Version, Digest<H>)],
    ) -> Result<bool, VersionedTrieError> {
        let head = self.bind(root, proof)?;
        let hist = self.history(head, proof)?;
        let fk = self.vkey(from)?;
        let uk = until.map(|u| self.vkey(u)).transpose()?;
        let listing = hist.range(&fk, uk.as_deref()).ok_or_else(|| {
            not_covered(alloc::format!(
                "versions [{}, {})",
                dilf4s_core::hex::encode(&fk),
                uk.as_deref().map(dilf4s_core::hex::encode).unwrap_or_default()
            ))
        })?;
        let expected = claimed
            .iter()
            .map(|(v, s)| Ok((self.vkey(v)?, self.bytes_hash(s.as_bytes()))))
            .collect::<Result<Vec<_>, VersionedTrieError>>()?;
        Ok(listing == expected)
    }

    /// Rule 5: the latest committed version `p <= t` and its state root, or `None` if every committed version is
    /// after `t`. The history range `[vkey(p), vkey(t) || 0x00)` must list exactly `p`.
    pub fn verify_as_of(
        &self,
        root: &Digest<H>,
        proof: &VersionedTrieProof<K::Version, H>,
        t: &K::Version,
    ) -> Result<Option<Version<K::Version, H>>, VersionedTrieError> {
        let head = self.bind(root, proof)?;
        let hist = self.history(head, proof)?;
        let tkey = self.vkey(t)?;
        let mut bound = tkey.clone();
        bound.push(0x00);
        let found = match &proof.at {
            None => None,
            Some(at) => {
                let pkey = self.vkey(&at.version)?;
                let s = at.state_root.clone().ok_or_else(|| malformed("an as-of answer names its state root"))?;
                if pkey > tkey {
                    return Err(malformed(alloc::format!(
                        "the answer {} is after {}",
                        dilf4s_core::hex::encode(&pkey),
                        dilf4s_core::hex::encode(&tkey)
                    )));
                }
                Some((pkey, s, at.version.clone()))
            }
        };
        let from = found.as_ref().map(|(k, _, _)| k.clone()).unwrap_or_default();
        let listing = hist
            .range(&from, Some(&bound))
            .ok_or_else(|| not_covered(alloc::format!("history up to {}", dilf4s_core::hex::encode(&tkey))))?;
        let expected: Vec<(Vec<u8>, Digest<H>)> =
            found.iter().map(|(k, s, _)| (k.clone(), self.bytes_hash(s.as_bytes()))).collect();
        if listing != expected {
            return Err(malformed(alloc::format!(
                "the history up to {} is not the claimed answer",
                dilf4s_core::hex::encode(&tkey)
            )));
        }
        Ok(found.map(|(_, s, p)| (p, s)))
    }
}
