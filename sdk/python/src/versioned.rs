//! `dilf4s.proofs`' versioned tries (dilf4s-proofs `versioned`, docs/specs/versioned-trie.md): state keys and
//! namespaces, heads, deltas, snapshot chunks, the proof verifiers and replicas.
//!
//! The Rust types are generic over the version key and the hash; here both are chosen at run time. Each configuration
//! is one Rust `VersionedTrie<K, H>` behind a trait object ([`Vt`]), whose methods take and give plain bytes, values
//! and versions ([`Ver`]: an `int` for an unsigned-long key, `bytes` for a bytes key), so the generic code is written
//! once and the dispatch happens when the trie is made. A version of the other key's type is a `TypeMismatch`.

use alloc::boxed::Box;
use alloc::string::String;
use alloc::sync::Arc;
use alloc::vec::Vec;

use dilf4s_core::Value;
use dilf4s_core::codec::Model as _;
use dilf4s_core::generated::versioned_trie as w;
use dilf4s_core::hash::{Digest, HashAlgorithm, Hasher};
use dilf4s_proofs::versioned::{
    BytesKey, Delta, Head, Replica, SnapshotChunk, StateKey, StateKeyError, StateNamespace, U64Key, VersionKey,
    VersionedTrie, VersionedTrieError, VersionedTrieProof, check_value,
};
use pyo3::prelude::*;

use crate::boundary::{
    Fail, Module, Reads, bytes, bytes_len, call, instance, items, mismatch, name, py_bool, py_bytes, py_none, py_str,
    py_tuple, py_u64, tuple_of, u64_arg, value,
};
use crate::core_api::as_algorithm;
use crate::errors::Refusal;
use crate::proofs::with_hasher;
use crate::value::py_value;

/// What reading claimed versions keeps: whether one of the wrong width was met, the versions met so far (by identity),
/// and the one zero version every claim after the wrong one shares.
#[derive(Default)]
struct Claims {
    past_bad: bool,
    seen: alloc::collections::BTreeMap<usize, Ver>,
    zero: Option<Ver>,
}

/// A version, as Python holds it.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Ver {
    Int(u64),
    Bytes(Arc<[u8]>),
}

/// A head with its parts as bytes.
#[derive(Clone)]
struct HeadN {
    version: Ver,
    state_root: Vec<u8>,
    history_root: Vec<u8>,
}

/// A failure at the type-erased boundary: the family's refusal, or an argument of the wrong type.
enum VtFail {
    Vt(VersionedTrieError),
    Mismatch(&'static str),
}

impl From<VersionedTrieError> for VtFail {
    fn from(e: VersionedTrieError) -> Self {
        Self::Vt(e)
    }
}

impl From<StateKeyError> for VtFail {
    fn from(e: StateKeyError) -> Self {
        Self::Vt(VersionedTrieError::from(e))
    }
}

type R<T> = Result<T, VtFail>;

fn fail<'py>(py: Python<'py>) -> impl Fn(VtFail) -> Fail<'py> {
    move |e| match e {
        VtFail::Vt(v) => v.refuse(py),
        VtFail::Mismatch(reason) => mismatch(py, reason),
    }
}

/// A version key's versions in and out of [`Ver`].
trait Versions: VersionKey + Clone + Send + Sync + 'static
where
    Self::Version: Send + Sync,
{
    fn to_ver(v: &Self::Version) -> Ver;
    fn from_ver(v: &Ver) -> R<Self::Version>;
    /// Whether `v` is of this key's kind, refused as [`Versions::from_ver`] refuses it, without making it.
    fn kind_of(v: &Ver) -> R<()>;
}

impl Versions for U64Key {
    fn to_ver(v: &u64) -> Ver {
        Ver::Int(*v)
    }

    fn from_ver(v: &Ver) -> R<u64> {
        match v {
            Ver::Int(n) => Ok(*n),
            Ver::Bytes(_) => Err(VtFail::Mismatch("expected an int version: the version key is unsignedLong")),
        }
    }

    fn kind_of(v: &Ver) -> R<()> {
        Self::from_ver(v).map(drop)
    }
}

impl Versions for BytesKey {
    fn to_ver(v: &Vec<u8>) -> Ver {
        Ver::Bytes(Arc::from(v.as_slice()))
    }

    fn from_ver(v: &Ver) -> R<Vec<u8>> {
        match v {
            Ver::Bytes(b) => Ok(b.to_vec()),
            Ver::Int(_) => Err(VtFail::Mismatch("expected a bytes version: the version key is bytes")),
        }
    }
    fn kind_of(v: &Ver) -> R<()> {
        match v {
            Ver::Bytes(_) => Ok(()),
            Ver::Int(_) => Err(VtFail::Mismatch("expected a bytes version: the version key is bytes")),
        }
    }
}

fn digest<H: Hasher>(b: &[u8]) -> R<Digest<H>> {
    Digest::<H>::from_slice(b).map_err(|_| {
        VtFail::Vt(VersionedTrieError::Malformed {
            reason: alloc::format!("a digest is {} bytes, got {}", H::OUTPUT_SIZE, b.len()),
        })
    })
}

fn state_key(text: &str) -> R<StateKey> {
    Ok(StateKey::parse(text)?)
}

/// What a checked state proof shows; each answer re-checks the proof (it was checked when made, so it holds).
trait State: Send + Sync {
    fn lookup(&self, key: &str) -> R<Option<Vec<u8>>>;
    fn holds(&self, key: &str, value: Option<&Value>) -> R<bool>;
    fn namespace(&self, ns: &str) -> R<Vec<(String, Vec<u8>)>>;
    fn range(&self, from: &str, until: Option<&str>) -> R<Vec<(String, Vec<u8>)>>;
}

/// A replica, behind the boundary.
trait Copy_: Send + Sync {
    fn head(&self) -> Option<HeadN>;
    fn root(&self) -> R<Option<Vec<u8>>>;
    fn get(&self, key: &str) -> R<Option<Value>>;
    fn entries(&self) -> Vec<(String, Value)>;
    fn len(&self) -> usize;
    fn apply(&self, delta: &Value) -> R<Box<dyn Copy_>>;
}

/// A delta, with its parts as Python reads them.
struct DeltaN {
    parent: Option<HeadN>,
    head: HeadN,
    upserts: Vec<(String, Value)>,
    removes: Vec<String>,
}

/// A snapshot chunk, with its parts as Python reads them.
enum ChunkN {
    Head(HeadN),
    State(Vec<(String, Value)>),
    History(Vec<(Ver, Vec<u8>)>),
}

/// A versioned trie's configuration, behind the boundary.
trait Vt: Send + Sync {
    fn algorithm(&self) -> HashAlgorithm;
    fn empty_state_root(&self) -> Vec<u8>;
    fn vkey(&self, v: &Ver) -> R<Vec<u8>>;
    fn value_hash(&self, v: &Value) -> Vec<u8>;
    fn bytes_hash(&self, b: &[u8]) -> Vec<u8>;
    fn decode_head(&self, v: &Value) -> R<HeadN>;
    fn encode_head(&self, h: &HeadN) -> R<Value>;
    fn root(&self, h: &HeadN) -> R<Vec<u8>>;
    fn decode_delta(&self, v: &Value) -> R<DeltaN>;
    fn decode_chunk(&self, b: &[u8]) -> R<ChunkN>;
    fn decode_proof(&self, v: &Value) -> R<HeadN>;
    fn verify_head(&self, root: &[u8], proof: &Value) -> R<HeadN>;
    fn verify_state(&self, root: &[u8], proof: &Value, at: Option<&Ver>) -> R<Box<dyn State>>;
    fn verify_version(&self, root: &[u8], proof: &Value, u: &Ver) -> R<Option<Vec<u8>>>;
    fn verify_versions(
        &self,
        root: &[u8],
        proof: &Value,
        from: &Ver,
        until: Option<&Ver>,
        claimed: &[(Ver, &[u8])],
    ) -> R<bool>;
    fn verify_as_of(&self, root: &[u8], proof: &Value, t: &Ver) -> R<Option<(Ver, Vec<u8>)>>;
    fn empty_replica(&self) -> Box<dyn Copy_>;
    fn restore(&self, trusted: &HeadN, chunks: &[&[u8]]) -> R<Box<dyn Copy_>>;
}

struct Typed<K: Versions, H: Hasher>(VersionedTrie<K, H>)
where
    K::Version: Send + Sync;

fn head_n<K: Versions, H: Hasher>(h: &Head<K::Version, H>) -> HeadN
where
    K::Version: Send + Sync,
{
    HeadN {
        version: K::to_ver(&h.version),
        state_root: h.state_root.as_bytes().to_vec(),
        history_root: h.history_root.as_bytes().to_vec(),
    }
}

fn head_t<K: Versions, H: Hasher>(h: &HeadN) -> R<Head<K::Version, H>>
where
    K::Version: Send + Sync,
{
    Ok(Head {
        version: K::from_ver(&h.version)?,
        state_root: digest(&h.state_root)?,
        history_root: digest(&h.history_root)?,
    })
}

fn listed<H: Hasher>(es: Vec<(StateKey, Digest<H>)>) -> Vec<(String, Vec<u8>)> {
    es.into_iter().map(|(k, h)| (String::from(k.as_str()), h.as_bytes().to_vec())).collect()
}

/// A state proof and the configuration that checked it.
struct CheckedState<K: Versions, H: Hasher>
where
    K::Version: Send + Sync,
{
    vt: VersionedTrie<K, H>,
    root: Digest<H>,
    proof: VersionedTrieProof<K::Version, H>,
    at: Option<K::Version>,
}

impl<K: Versions, H: Hasher + Send + Sync + 'static> CheckedState<K, H>
where
    K::Version: Send + Sync,
    H::Output: Send + Sync,
{
    fn with<T>(&self, f: impl FnOnce(&dilf4s_proofs::versioned::VerifiedState<'_, H>) -> R<T>) -> R<T> {
        let s = match &self.at {
            None => self.vt.verify_state(&self.root, &self.proof)?,
            Some(u) => self.vt.verify_historical_key(&self.root, &self.proof, u)?,
        };
        f(&s)
    }
}

impl<K: Versions, H: Hasher + Send + Sync + 'static> State for CheckedState<K, H>
where
    K::Version: Send + Sync,
    H::Output: Send + Sync,
{
    fn lookup(&self, key: &str) -> R<Option<Vec<u8>>> {
        let k = state_key(key)?;
        self.with(|s| Ok(s.lookup(&k)?.map(|h| h.as_bytes().to_vec())))
    }

    fn holds(&self, key: &str, value: Option<&Value>) -> R<bool> {
        let k = state_key(key)?;
        self.with(|s| Ok(s.holds(&k, value)?))
    }

    fn namespace(&self, ns: &str) -> R<Vec<(String, Vec<u8>)>> {
        let n = StateNamespace::parse(ns)?;
        self.with(|s| Ok(listed(s.namespace(&n)?)))
    }

    fn range(&self, from: &str, until: Option<&str>) -> R<Vec<(String, Vec<u8>)>> {
        let f = state_key(from)?;
        let u = until.map(state_key).transpose()?;
        self.with(|s| Ok(listed(s.range(&f, u.as_ref())?)))
    }
}

struct Kept<K: Versions, H: Hasher>(Replica<K, H>)
where
    K::Version: Send + Sync;

impl<K: Versions, H: Hasher + Send + Sync + 'static> Copy_ for Kept<K, H>
where
    K::Version: Send + Sync,
    H::Output: Send + Sync,
{
    fn head(&self) -> Option<HeadN> {
        self.0.head().map(head_n::<K, H>)
    }

    fn root(&self) -> R<Option<Vec<u8>>> {
        Ok(self.0.root()?.map(|d| d.as_bytes().to_vec()))
    }

    fn get(&self, key: &str) -> R<Option<Value>> {
        Ok(self.0.get(&state_key(key)?).cloned())
    }

    fn entries(&self) -> Vec<(String, Value)> {
        self.0.entries().map(|(k, v)| (String::from(k.as_str()), v.clone())).collect()
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn apply(&self, delta: &Value) -> R<Box<dyn Copy_>> {
        let d = self.0.versioned_trie().decode_delta(delta)?;
        // the replica Python holds never changes: the delta applies to a copy, which shares the rest
        let mut next = self.0.clone();
        next.apply_delta(&d)?;
        Ok(Box::new(Self(next)))
    }
}

impl<K: Versions, H: Hasher + Send + Sync + 'static> Vt for Typed<K, H>
where
    K::Version: Send + Sync,
    H::Output: Send + Sync,
{
    fn algorithm(&self) -> HashAlgorithm {
        HashAlgorithm::from_name(H::NAME).unwrap_or(HashAlgorithm::Sha256)
    }

    fn empty_state_root(&self) -> Vec<u8> {
        self.0.hashing().empty().as_bytes().to_vec()
    }

    fn vkey(&self, v: &Ver) -> R<Vec<u8>> {
        Ok(self.0.vkey(&K::from_ver(v)?)?)
    }

    fn value_hash(&self, v: &Value) -> Vec<u8> {
        self.0.value_hash(v).as_bytes().to_vec()
    }

    fn bytes_hash(&self, b: &[u8]) -> Vec<u8> {
        self.0.bytes_hash(b).as_bytes().to_vec()
    }

    fn decode_head(&self, v: &Value) -> R<HeadN> {
        let wire = w::VersionedTrieHead::from_value(v)
            .map_err(|e| VersionedTrieError::Malformed { reason: alloc::format!("{e}") })?;
        Ok(head_n::<K, H>(&self.0.head_from_wire(&wire)?))
    }

    fn encode_head(&self, h: &HeadN) -> R<Value> {
        let wire = self.0.head_to_wire(&head_t::<K, H>(h)?)?;
        Ok(wire.to_value().map_err(|e| VersionedTrieError::Malformed { reason: alloc::format!("{e}") })?)
    }

    fn root(&self, h: &HeadN) -> R<Vec<u8>> {
        Ok(self.0.root(&head_t::<K, H>(h)?)?.as_bytes().to_vec())
    }

    fn decode_delta(&self, v: &Value) -> R<DeltaN> {
        let d: Delta<K::Version, H> = self.0.decode_delta(v)?;
        Ok(DeltaN {
            parent: d.parent.as_ref().map(head_n::<K, H>),
            head: head_n::<K, H>(&d.head),
            upserts: d.upserts.into_iter().map(|(k, v)| (String::from(k.as_str()), v)).collect(),
            removes: d.removes.iter().map(|k| String::from(k.as_str())).collect(),
        })
    }

    fn decode_chunk(&self, b: &[u8]) -> R<ChunkN> {
        Ok(match self.0.decode_chunk(b)? {
            SnapshotChunk::Head(h) => ChunkN::Head(head_n::<K, H>(&h)),
            SnapshotChunk::State(es) => {
                ChunkN::State(es.into_iter().map(|(k, v)| (String::from(k.as_str()), v)).collect())
            }
            SnapshotChunk::History(vs) => {
                ChunkN::History(vs.iter().map(|(u, s)| (K::to_ver(u), s.as_bytes().to_vec())).collect())
            }
        })
    }

    fn decode_proof(&self, v: &Value) -> R<HeadN> {
        Ok(head_n::<K, H>(&self.0.decode_proof(v)?.head))
    }

    fn verify_head(&self, root: &[u8], proof: &Value) -> R<HeadN> {
        let p = self.0.decode_proof(proof)?;
        Ok(head_n::<K, H>(&self.0.verify_head(&digest(root)?, &p)?))
    }

    fn verify_state(&self, root: &[u8], proof: &Value, at: Option<&Ver>) -> R<Box<dyn State>> {
        let p = self.0.decode_proof(proof)?;
        let r = digest::<H>(root)?;
        let u = at.map(K::from_ver).transpose()?;
        // checked now, so a refusal is the call's; the answers re-check it
        match &u {
            None => self.0.verify_state(&r, &p).map(drop)?,
            Some(u) => self.0.verify_historical_key(&r, &p, u).map(drop)?,
        }
        Ok(Box::new(CheckedState { vt: self.0.clone(), root: r, proof: p, at: u }))
    }

    fn verify_version(&self, root: &[u8], proof: &Value, u: &Ver) -> R<Option<Vec<u8>>> {
        let p = self.0.decode_proof(proof)?;
        let found = self.0.verify_version(&digest(root)?, &p, &K::from_ver(u)?)?;
        Ok(found.map(|d| d.as_bytes().to_vec()))
    }

    fn verify_versions(
        &self,
        root: &[u8],
        proof: &Value,
        from: &Ver,
        until: Option<&Ver>,
        claimed: &[(Ver, &[u8])],
    ) -> R<bool> {
        let p = self.0.decode_proof(proof)?;
        let (f, u) = (K::from_ver(from)?, until.map(K::from_ver).transpose()?);
        // the claims checked in order (each version's kind, each state root's width) without owning a version, then the
        // root; Rust then binds the proof and reads its history and range before it reads a claim, so those checks are
        // made first with no claims (refusing as Rust would), and the claims' versions are owned only past them
        let roots = claimed.iter().map(|(v, s)| K::kind_of(v).and_then(|()| digest::<H>(s))).collect::<R<Vec<_>>>()?;
        let r = digest(root)?;
        self.0.verify_versions(&r, &p, &f, u.as_ref(), &[])?;
        // Rust next reads each claim's version key, in order, refusing the first that has none: each distinct version
        // (a shared one met again is the same version) is checked so first, owned one at a time, so a claim refused
        // there costs no copy of the others; only claims that all pass are owned together
        let (mut ints, mut shared) = (alloc::collections::BTreeSet::new(), alloc::collections::BTreeSet::new());
        for (v, _) in claimed {
            let fresh = match v {
                Ver::Int(n) => ints.insert(*n),
                Ver::Bytes(b) => shared.insert(Arc::as_ptr(b).cast::<u8>().addr()),
            };
            if fresh {
                self.0.vkey(&K::from_ver(v)?)?;
            }
        }
        let cs = claimed.iter().zip(roots).map(|((v, _), s)| Ok((K::from_ver(v)?, s))).collect::<R<Vec<_>>>()?;
        Ok(self.0.verify_versions(&r, &p, &f, u.as_ref(), &cs)?)
    }

    fn verify_as_of(&self, root: &[u8], proof: &Value, t: &Ver) -> R<Option<(Ver, Vec<u8>)>> {
        let p = self.0.decode_proof(proof)?;
        let found = self.0.verify_as_of(&digest(root)?, &p, &K::from_ver(t)?)?;
        Ok(found.map(|(v, s)| (K::to_ver(&v), s.as_bytes().to_vec())))
    }

    fn empty_replica(&self) -> Box<dyn Copy_> {
        Box::new(Kept(Replica::empty(self.0.clone())))
    }

    fn restore(&self, trusted: &HeadN, chunks: &[&[u8]]) -> R<Box<dyn Copy_>> {
        let head = head_t::<K, H>(trusted)?;
        // each chunk decoded and fed in turn: a refusal comes at the first chunk that fails, decoding or in order,
        // having decoded none after it (many references to one large chunk cost it once, not once each)
        let mut reader = self.0.snapshot_reader(head.clone());
        for c in chunks {
            reader.feed(self.0.decode_chunk(c)?)?;
        }
        let snapshot = reader.finish()?;
        Ok(Box::new(Kept(Replica::from_snapshot(self.0.clone(), head, &snapshot)?)))
    }
}

// ------------------------------------------------------------------------------------------------ Python

#[derive(Clone, Copy)]
enum KeyKind {
    UnsignedLong(U64Key),
    Bytes(BytesKey),
}

/// How a version becomes a history-trie key: `unsigned_long(w)` (an `int`, `w` bytes big-endian, `1..8`) or
/// `bytes(w)` (versions that are `w`-byte keys already, `1..1024`).
#[pyclass(frozen, module = "dilf4s._native", name = "VersionKey")]
pub(crate) struct PyVersionKey(KeyKind);

#[pymethods]
impl PyVersionKey {
    /// Non-negative integers in `width` bytes (8 unless given), the largest `2**(8 * width) - 1`, capped at `2**63 - 1`.
    #[staticmethod]
    #[pyo3(signature = (width = None))]
    fn unsigned_long(py: Python<'_>, width: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
        call(py, || {
            let w = width.filter(|w| !w.is_none()).map_or(Ok(8), |w| u64_arg(w, "the width"))?;
            let w = u8::try_from(w).unwrap_or(u8::MAX);
            let key = U64Key::new(w).map_err(|e| e.refuse(py))?;
            Ok(Bound::new(py, Self(KeyKind::UnsignedLong(key)))?.into_any())
        })
    }

    /// Versions that are `width`-byte keys.
    #[staticmethod]
    fn bytes(width: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = width.py();
        call(py, || {
            let w = u16::try_from(u64_arg(width, "the width")?).unwrap_or(u16::MAX);
            let key = BytesKey::new(w).map_err(|e| e.refuse(py))?;
            Ok(Bound::new(py, Self(KeyKind::Bytes(key)))?.into_any())
        })
    }

    /// `unsignedLong` or `bytes`.
    #[getter]
    const fn kind(&self) -> &'static str {
        match self.0 {
            KeyKind::UnsignedLong(_) => "unsignedLong",
            KeyKind::Bytes(_) => "bytes",
        }
    }

    /// The width in bytes.
    #[getter]
    fn width(&self) -> u16 {
        match self.0 {
            KeyKind::UnsignedLong(k) => k.width(),
            KeyKind::Bytes(k) => k.width(),
        }
    }
}

/// A versioned trie's configuration: the version key and the hash.
#[pyclass(frozen, module = "dilf4s._native", name = "VersionedTrie")]
pub(crate) struct PyVersionedTrie {
    vt: Arc<dyn Vt>,
    kind: KeyKind,
}

/// A state proof checked against a trusted root (`verify_state`, `verify_historical_key`).
#[pyclass(frozen, module = "dilf4s._native", name = "VerifiedState")]
pub(crate) struct PyVerifiedState(Box<dyn State>);

/// A replica: a versioned trie's whole state and history, kept in memory and following deltas. Frozen: applying a
/// delta gives a new replica, which shares what the delta does not change.
#[pyclass(frozen, module = "dilf4s._native", name = "Replica")]
pub(crate) struct PyReplica(Box<dyn Copy_>);

fn vt_of<'a, 'py>(obj: &'a Bound<'py, PyAny>) -> Result<&'a PyVersionedTrie, Fail<'py>> {
    obj.cast::<PyVersionedTrie>()
        .map(Bound::get)
        .map_err(|_| mismatch(obj.py(), "expected a dilf4s.proofs.VersionedTrie"))
}

impl PyVersionedTrie {
    /// A version argument of this trie's key type.
    fn ver<'py>(&self, obj: &Bound<'py, PyAny>) -> Result<Ver, Fail<'py>> {
        match self.kind {
            KeyKind::UnsignedLong(_) => Ok(Ver::Int(u64_arg(obj, "an int version")?)),
            KeyKind::Bytes(_) => Ok(Ver::Bytes(Arc::from(bytes(obj)?.as_ref()))),
        }
    }

    /// A claimed version, as [`Self::ver`] reads it, except after a bytes version of the wrong width. Rust checks the
    /// claims in order and refuses at the first such, by its length: that one is copied, once, and the ones after it,
    /// which no check reaches, stand in as zero versions of the width. Many references to one large version then cost
    /// it once, never its size times the count.
    fn claimed_ver<'py>(&self, obj: &Bound<'py, PyAny>, claims: &mut Claims) -> Result<Ver, Fail<'py>> {
        let KeyKind::Bytes(k) = &self.kind else {
            return self.ver(obj);
        };
        let width = usize::from(k.width());
        let n = bytes_len(obj)?;
        if claims.past_bad {
            return Ok(claims.zero.get_or_insert_with(|| Ver::Bytes(Arc::from(alloc::vec![0; width]))).clone());
        }
        claims.past_bad = n != width;
        // a version met again is shared: its bytes are copied once, however many claims name it, and none is owned
        // apart until the proof has decoded (the trie converts the claims after it)
        let at = obj.as_ptr().addr();
        if let Some(v) = claims.seen.get(&at) {
            return Ok(v.clone());
        }
        let v = self.ver(obj)?;
        claims.seen.insert(at, v.clone());
        Ok(v)
    }

    /// A `VersionedHead` argument, exactly that class (its fields are read by their slots).
    fn head_arg<'py>(&self, obj: &Bound<'py, PyAny>) -> Result<HeadN, Fail<'py>> {
        let py = obj.py();
        let class = crate::boundary::class(py, Module::ProofTypes, "VersionedHead")?;
        if !obj.get_type().is(&class) {
            return Err(mismatch(py, "expected a dilf4s.proofs.VersionedHead"));
        }
        Ok(HeadN {
            version: self.ver(&obj.getattr("version")?)?,
            state_root: bytes(&obj.getattr("state_root")?)?.into_owned(),
            history_root: bytes(&obj.getattr("history_root")?)?.into_owned(),
        })
    }
}

fn py_ver<'py>(py: Python<'py>, v: &Ver) -> PyResult<Bound<'py, PyAny>> {
    match v {
        Ver::Int(n) => py_u64(py, *n),
        Ver::Bytes(b) => Ok(py_bytes(py, b)),
    }
}

fn py_head<'py>(py: Python<'py>, h: &HeadN) -> PyResult<Bound<'py, PyAny>> {
    instance(
        py,
        Module::ProofTypes,
        "VersionedHead",
        &[
            ("version", py_ver(py, &h.version)?),
            ("state_root", py_bytes(py, &h.state_root)),
            ("history_root", py_bytes(py, &h.history_root)),
        ],
    )
}

fn py_entries<'py>(py: Python<'py>, es: &[(String, Value)]) -> PyResult<Bound<'py, PyAny>> {
    let pairs = es
        .iter()
        .map(|(k, v)| py_tuple(py, vec![py_str(py, k), py_value(py, v.clone())?]))
        .collect::<PyResult<Vec<_>>>()?;
    py_tuple(py, pairs)
}

fn py_listing<'py>(py: Python<'py>, es: &[(String, Vec<u8>)]) -> PyResult<Bound<'py, PyAny>> {
    let pairs =
        es.iter().map(|(k, h)| py_tuple(py, vec![py_str(py, k), py_bytes(py, h)])).collect::<PyResult<Vec<_>>>()?;
    py_tuple(py, pairs)
}

fn make(kind: KeyKind, alg: HashAlgorithm) -> Result<Arc<dyn Vt>, VersionedTrieError> {
    Ok(match kind {
        KeyKind::UnsignedLong(k) => {
            with_hasher!(alg, H => { let vt: Arc<dyn Vt> = Arc::new(Typed(VersionedTrie::<U64Key, H>::new(k)?)); vt })
        }
        KeyKind::Bytes(k) => {
            with_hasher!(alg, H => { let vt: Arc<dyn Vt> = Arc::new(Typed(VersionedTrie::<BytesKey, H>::new(k)?)); vt })
        }
    })
}

#[pymethods]
impl PyVersionedTrie {
    /// The configuration for a version key, over SHA-256 unless a hash is named.
    #[staticmethod]
    #[pyo3(signature = (version_key, algorithm = None))]
    fn of(version_key: &Bound<'_, PyAny>, algorithm: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
        let py = version_key.py();
        call(py, || {
            let kind = version_key
                .cast::<PyVersionKey>()
                .map(|k| k.get().0)
                .map_err(|_| mismatch(py, "expected a dilf4s.proofs.VersionKey"))?;
            let alg = algorithm.filter(|a| !a.is_none()).map_or(Ok(HashAlgorithm::Sha256), as_algorithm)?;
            let vt = make(kind, alg).map_err(|e| e.refuse(py))?;
            Ok(Bound::new(py, Self { vt, kind })?.into_any())
        })
    }

    /// The hash algorithm.
    #[getter]
    fn algorithm(&self) -> &'static str {
        self.vt.algorithm().name()
    }

    /// The version key.
    #[getter]
    fn version_key<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        Bound::new(py, PyVersionKey(self.kind)).map(Bound::into_any)
    }

    /// The root of an empty state trie.
    #[getter]
    fn empty_state_root<'py>(&self, py: Python<'py>) -> Bound<'py, PyAny> {
        py_bytes(py, &self.vt.empty_state_root())
    }

    /// `vkey(v)`: a version's history-trie key.
    fn vkey(&self, version: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = version.py();
        call(py, || Ok(py_bytes(py, &self.vt.vkey(&self.ver(version)?).map_err(fail(py))?)))
    }

    /// The hash a trie commits for a value: `valueHash(commit(v))`.
    fn value_hash(&self, value: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = value.py();
        call(py, || {
            let v = crate::boundary::value(value)?;
            Ok(py_bytes(py, &py.detach(|| self.vt.value_hash(&v))))
        })
    }

    /// The hash a trie commits for a byte string (top slots and history entries).
    fn bytes_hash(&self, b: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = b.py();
        call(py, || Ok(py_bytes(py, &self.vt.bytes_hash(&bytes(b)?))))
    }

    /// A value every wire format can carry under `key`: at most `MAX_VALUE_DEPTH` deep and `MAX_VALUE_BYTES` long.
    #[allow(clippy::unused_self, reason = "a check of every configuration alike, kept beside the others it precedes")]
    fn check_value(&self, key: &Bound<'_, PyAny>, value: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = key.py();
        call(py, || {
            let k = state_key(&name(key)?).map_err(fail(py))?;
            let v = crate::boundary::value(value)?;
            py.detach(|| check_value(&k, &v)).map_err(|e| e.refuse(py))?;
            Ok(py_none(py))
        })
    }

    /// A head from its wire record (a value).
    fn decode_head(&self, form: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = form.py();
        call(py, || Ok(py_head(py, &self.vt.decode_head(&value(form)?).map_err(fail(py))?)?))
    }

    /// A head's wire record.
    fn encode_head(&self, head: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = head.py();
        call(py, || Ok(py_value(py, self.vt.encode_head(&self.head_arg(head)?).map_err(fail(py))?)?))
    }

    /// The commitment root of a head: its top trie's root.
    fn root(&self, head: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = head.py();
        call(py, || Ok(py_bytes(py, &self.vt.root(&self.head_arg(head)?).map_err(fail(py))?)))
    }

    /// A delta from its canonical form: the parent head (none for the first commit), the new head, the upserts and the
    /// removes.
    fn decode_delta(&self, form: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = form.py();
        call(py, || {
            let d = self.vt.decode_delta(&value(form)?).map_err(fail(py))?;
            let parent = d.parent.as_ref().map_or_else(|| Ok(py_none(py)), |h| py_head(py, h))?;
            let removes = py_tuple(py, d.removes.iter().map(|k| py_str(py, k)).collect())?;
            Ok(instance(
                py,
                Module::ProofTypes,
                "Delta",
                &[
                    ("parent", parent),
                    ("head", py_head(py, &d.head)?),
                    ("upserts", py_entries(py, &d.upserts)?),
                    ("removes", removes),
                ],
            )?)
        })
    }

    /// A snapshot chunk from its CBOR: `HeadChunk`, `StateChunk` or `HistoryChunk`.
    fn decode_chunk(&self, data: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = data.py();
        call(py, || {
            let c = self.vt.decode_chunk(&bytes(data)?).map_err(fail(py))?;
            Ok(match c {
                ChunkN::Head(h) => instance(py, Module::ProofTypes, "HeadChunk", &[("head", py_head(py, &h)?)])?,
                ChunkN::State(es) => {
                    instance(py, Module::ProofTypes, "StateChunk", &[("entries", py_entries(py, &es)?)])?
                }
                ChunkN::History(vs) => {
                    let pairs = vs
                        .iter()
                        .map(|(v, s)| py_tuple(py, vec![py_ver(py, v)?, py_bytes(py, s)]))
                        .collect::<PyResult<Vec<_>>>()?;
                    instance(py, Module::ProofTypes, "HistoryChunk", &[("versions", py_tuple(py, pairs)?)])?
                }
            })
        })
    }

    /// Decodes a proof (its canonical form), giving the head it claims; a proof heavier than `MAX_PROOF_WEIGHT` is
    /// `Malformed` before it is decoded.
    fn decode_proof(&self, form: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = form.py();
        call(py, || {
            let v = value(form)?;
            Ok(py_head(py, &py.detach(|| self.vt.decode_proof(&v)).map_err(fail(py))?)?)
        })
    }

    /// Rule 7: the head the proof binds to `root`, with the head invariant.
    fn verify_head(&self, root: &Bound<'_, PyAny>, proof: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = root.py();
        call(py, || {
            let (r, p) = (bytes(root)?, value(proof)?);
            Ok(py_head(py, &py.detach(|| self.vt.verify_head(&r, &p)).map_err(fail(py))?)?)
        })
    }

    /// Rule 2: a proof about the current state, checked against the head's state root.
    fn verify_state(&self, root: &Bound<'_, PyAny>, proof: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = root.py();
        call(py, || {
            let (r, p) = (bytes(root)?, value(proof)?);
            let s = py.detach(|| self.vt.verify_state(&r, &p, None)).map_err(fail(py))?;
            Ok(Bound::new(py, PyVerifiedState(s))?.into_any())
        })
    }

    /// Rule 4: a proof about the state at version `at`.
    fn verify_historical_key(
        &self,
        root: &Bound<'_, PyAny>,
        proof: &Bound<'_, PyAny>,
        at: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let py = root.py();
        call(py, || {
            let (r, p, u) = (bytes(root)?, value(proof)?, self.ver(at)?);
            let s = py.detach(|| self.vt.verify_state(&r, &p, Some(&u))).map_err(fail(py))?;
            Ok(Bound::new(py, PyVerifiedState(s))?.into_any())
        })
    }

    /// Rule 3: version `at`'s state root if the history committed it, `None` if it provably never did.
    fn verify_version(
        &self,
        root: &Bound<'_, PyAny>,
        proof: &Bound<'_, PyAny>,
        at: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let py = root.py();
        call(py, || {
            let (r, p, u) = (bytes(root)?, value(proof)?, self.ver(at)?);
            let found = py.detach(|| self.vt.verify_version(&r, &p, &u)).map_err(fail(py))?;
            Ok(found.map_or_else(|| py_none(py), |s| py_bytes(py, &s)))
        })
    }

    /// Rule 5: `claimed` (`(version, state_root)` pairs) is exactly the committed versions in `[start, until)`.
    #[pyo3(signature = (root, proof, start, until, claimed))]
    fn verify_versions(
        &self,
        root: &Bound<'_, PyAny>,
        proof: &Bound<'_, PyAny>,
        start: &Bound<'_, PyAny>,
        until: &Bound<'_, PyAny>,
        claimed: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let py = root.py();
        call(py, || {
            let (r, p, f) = (bytes(root)?, value(proof)?, self.ver(start)?);
            let u = if until.is_none() { None } else { Some(self.ver(until)?) };
            let mut claims = Claims::default();
            let checked = items(claimed, "the claimed versions")?
                .iter()
                .map(|c| {
                    let [v, s] =
                        tuple_of::<2>(c, "a (version, state root) pair", "expected (version, state root) pairs")?;
                    let ver = self.claimed_ver(&v, &mut claims)?;
                    bytes_len(&s)?;
                    Ok((ver, s))
                })
                .collect::<Result<Vec<_>, Fail<'_>>>()?;
            // the state roots read once each (a shared buffer once in all)
            let (vers, roots): (Vec<Ver>, Vec<Bound<'_, PyAny>>) = checked.into_iter().unzip();
            let mut reads = Reads::default();
            let held = roots.iter().map(|s| reads.read(s)).collect::<Result<Vec<_>, _>>()?;
            let cs: Vec<(Ver, &[u8])> = vers.into_iter().zip(&held).map(|(v, s)| (v, reads.get(s))).collect();
            let verdict = py.detach(|| self.vt.verify_versions(&r, &p, &f, u.as_ref(), &cs)).map_err(fail(py))?;
            Ok(py_bool(py, verdict))
        })
    }

    /// Rule 6: the latest committed version at or before `at`, with its state root, or `None`.
    fn verify_as_of(
        &self,
        root: &Bound<'_, PyAny>,
        proof: &Bound<'_, PyAny>,
        at: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let py = root.py();
        call(py, || {
            let (r, p, t) = (bytes(root)?, value(proof)?, self.ver(at)?);
            match py.detach(|| self.vt.verify_as_of(&r, &p, &t)).map_err(fail(py))? {
                None => Ok(py_none(py)),
                Some((v, s)) => Ok(py_tuple(py, vec![py_ver(py, &v)?, py_bytes(py, &s)])?),
            }
        })
    }
}

fn key_text<'py>(obj: &Bound<'py, PyAny>) -> Result<String, Fail<'py>> {
    Ok(name(obj)?.into_owned())
}

#[pymethods]
impl PyVerifiedState {
    /// The key's value hash, or `None` if it is proven absent; `NotCovered` if the proof does not reach it.
    fn lookup(&self, key: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = key.py();
        call(py, || {
            let k = key_text(key)?;
            let found = py.detach(|| self.0.lookup(&k)).map_err(fail(py))?;
            Ok(found.map_or_else(|| py_none(py), |h| py_bytes(py, &h)))
        })
    }

    /// Whether the key holds the value (`None`, or a null value: is absent).
    #[pyo3(signature = (key, claimed = None))]
    fn holds(&self, key: &Bound<'_, PyAny>, claimed: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
        let py = key.py();
        call(py, || {
            let k = key_text(key)?;
            let v = claimed.filter(|c| !c.is_none()).map(value).transpose()?;
            Ok(py_bool(py, py.detach(|| self.0.holds(&k, v.as_ref())).map_err(fail(py))?))
        })
    }

    /// Every `(key, valueHash)` under a namespace, in key order.
    fn namespace(&self, ns: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = ns.py();
        call(py, || {
            let n = key_text(ns)?;
            Ok(py_listing(py, &py.detach(|| self.0.namespace(&n)).map_err(fail(py))?)?)
        })
    }

    /// Every `(key, valueHash)` in `[start, until)` (`until` `None` is unbounded), in key order.
    #[pyo3(signature = (start, until = None))]
    fn range(&self, start: &Bound<'_, PyAny>, until: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
        let py = start.py();
        call(py, || {
            let f = key_text(start)?;
            let u = until.filter(|u| !u.is_none()).map(key_text).transpose()?;
            Ok(py_listing(py, &py.detach(|| self.0.range(&f, u.as_deref())).map_err(fail(py))?)?)
        })
    }
}

#[pymethods]
impl PyReplica {
    /// An empty replica, before the first commit.
    #[staticmethod]
    fn empty(vt: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = vt.py();
        call(py, || Ok(Bound::new(py, Self(vt_of(vt)?.vt.empty_replica()))?.into_any()))
    }

    /// A replica restored from snapshot chunks (their CBOR, in order), every check of the snapshot reader against
    /// `trusted`, and both roots rebuilt.
    #[staticmethod]
    fn restore(vt: &Bound<'_, PyAny>, trusted: &Bound<'_, PyAny>, chunks: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = vt.py();
        call(py, || {
            let t = vt_of(vt)?;
            let head = t.head_arg(trusted)?;
            let objs = items(chunks, "the chunks")?;
            // read once each: a shared buffer once in all
            let mut reads = Reads::default();
            let read = objs.iter().map(|c| reads.read(c)).collect::<Result<Vec<_>, _>>()?;
            let cs: Vec<&[u8]> = read.iter().map(|h| reads.get(h)).collect();
            let r = py.detach(|| t.vt.restore(&head, &cs)).map_err(fail(py))?;
            Ok(Bound::new(py, Self(r))?.into_any())
        })
    }

    /// The head, if anything was committed.
    #[getter]
    fn head<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        self.0.head().map_or_else(|| Ok(py_none(py)), |h| py_head(py, &h))
    }

    /// The commitment root of the head, if anything was committed.
    fn root(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        call(py, || Ok(self.0.root().map_err(fail(py))?.map_or_else(|| py_none(py), |r| py_bytes(py, &r))))
    }

    /// The value at `key`, if present.
    fn get(&self, key: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = key.py();
        call(py, || {
            let found = self.0.get(&key_text(key)?).map_err(fail(py))?;
            Ok(found.map_or_else(|| Ok(py_none(py)), |v| py_value(py, v))?)
        })
    }

    /// Every entry, in key order.
    fn entries<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        py_entries(py, &self.0.entries())
    }

    fn __len__(&self) -> usize {
        self.0.len()
    }

    /// The replica after a delta (its canonical form): every check of versioned-trie.md section 7, both roots
    /// recomputed. This replica is unchanged.
    fn apply_delta(&self, delta: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = delta.py();
        call(py, || {
            let d = value(delta)?;
            let next = py.detach(|| self.0.apply(&d)).map_err(fail(py))?;
            Ok(Bound::new(py, Self(next))?.into_any())
        })
    }
}

/// A state key (versioned-trie.md section 2), checked: its text.
#[pyfunction]
fn state_key_parse(text: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = text.py();
    call(py, || {
        let k = StateKey::parse(&name(text)?).map_err(|e| e.refuse(py))?;
        Ok(py_str(py, k.as_str()))
    })
}

/// A state key from its trie key (UTF-8 bytes): too long, then not UTF-8, then the grammar.
#[pyfunction]
fn state_key_from_bytes(data: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = data.py();
    call(py, || {
        let k = StateKey::from_bytes(&bytes(data)?).map_err(|e| e.refuse(py))?;
        Ok(py_str(py, k.as_str()))
    })
}

/// A state namespace, checked: a key that leaves room for one more segment.
#[pyfunction]
fn state_namespace_parse(text: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = text.py();
    call(py, || {
        let n = StateNamespace::parse(&name(text)?).map_err(|e| e.refuse(py))?;
        Ok(py_str(py, n.as_str()))
    })
}

pub(crate) fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PyVersionKey>()?;
    m.add_class::<PyVersionedTrie>()?;
    m.add_class::<PyVerifiedState>()?;
    m.add_class::<PyReplica>()?;
    m.add_function(wrap_pyfunction!(state_key_parse, m)?)?;
    m.add_function(wrap_pyfunction!(state_key_from_bytes, m)?)?;
    m.add_function(wrap_pyfunction!(state_namespace_parse, m)?)?;
    m.add("VERSIONED_MAX_VALUE_BYTES", dilf4s_proofs::versioned::MAX_VALUE_BYTES)?;
    m.add("VERSIONED_MAX_VALUE_DEPTH", dilf4s_proofs::versioned::MAX_VALUE_DEPTH)?;
    m.add("VERSIONED_MAX_CHUNK_ENTRIES", dilf4s_proofs::versioned::MAX_CHUNK_ENTRIES)?;
    m.add("VERSIONED_MAX_CHUNK_BYTES", dilf4s_proofs::versioned::MAX_CHUNK_BYTES)?;
    Ok(())
}
