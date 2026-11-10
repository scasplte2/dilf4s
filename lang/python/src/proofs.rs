//! `dilf4s.proofs` (dilf4s-proofs): Merkle trees (dilf4s's tagged profile and RFC 6962), binary tries, Poseidon and its
//! Merkle tree, BN254, Groth16 and SP1's Groth16 verifier. Versioned tries are in [`crate::versioned`].
//!
//! The Rust verifiers are generic over the hash; here the hash is chosen at run time, by name, and each call is
//! dispatched to the Rust code for that hash ([`with_hasher!`]). A digest argument of the wrong length is refused by the
//! family's malformed-input kind (`Encoding`), as the JLVM's operations refuse one.

use alloc::boxed::Box;
use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

use dilf4s_core::hash::{Digest, HashAlgorithm, Hasher};
use dilf4s_core::{BigInt, Value};
use dilf4s_proofs::Bits;
use dilf4s_proofs::merkle::{
    self, ConsistencyProof, InclusionProof, MerkleError, MerkleHashing as _, Rfc6962, Tagged, TreeHead,
};
use dilf4s_proofs::poseidon::merkle::{PoseidonMerkleError, PoseidonMerkleProof};
use dilf4s_proofs::poseidon::{self, Fr, PoseidonError};
use dilf4s_proofs::trie::{self, KeyMode, Lookup, TrieError, TrieHashing, TrieProof};
use dilf4s_proofs::zk::groth16::{self, Groth16Error};
use dilf4s_proofs::zk::{self, ZkError};
use pyo3::prelude::*;
use pyo3::pyclass::CompareOp;

use crate::boundary::{
    Answer, Fail, Held, Module, Reads, bigint, bytes, bytes_len, call, count, instance, items, items_within, mismatch,
    py_bool, py_bytes, py_int, py_none, py_tuple, py_u64, tuple_of, u64_arg, usize_arg, value,
};
use crate::core_api::as_algorithm;
use crate::errors::Refusal;
use crate::value::py_value;

/// Runs `$body` with `$H` the Rust hasher of `$alg`.
macro_rules! with_hasher {
    ($alg:expr, $H:ident => $body:expr) => {
        match $alg {
            HashAlgorithm::Sha256 => {
                type $H = dilf4s_core::hash::Sha256;
                $body
            }
            HashAlgorithm::Sha512 => {
                type $H = dilf4s_core::hash::Sha512;
                $body
            }
            HashAlgorithm::Sha3_256 => {
                type $H = dilf4s_core::hash::Sha3_256;
                $body
            }
            HashAlgorithm::Sha3_512 => {
                type $H = dilf4s_core::hash::Sha3_512;
                $body
            }
            HashAlgorithm::Keccak256 => {
                type $H = dilf4s_core::hash::Keccak256;
                $body
            }
            HashAlgorithm::Blake2b256 => {
                type $H = dilf4s_core::hash::Blake2b256;
                $body
            }
            HashAlgorithm::Blake2b512 => {
                type $H = dilf4s_core::hash::Blake2b512;
                $body
            }
        }
    };
}

pub(crate) use with_hasher;

/// A run-time algorithm, `SHA-256` when none is given.
fn algorithm_or_sha256<'py>(obj: Option<&Bound<'py, PyAny>>) -> Result<HashAlgorithm, Fail<'py>> {
    obj.filter(|o| !o.is_none()).map_or(Ok(HashAlgorithm::Sha256), as_algorithm)
}

/// A digest of `H` from bytes, or the family's refusal of one of the wrong length.
pub(crate) fn digest_of<H: Hasher, E>(b: &[u8], refuse: impl FnOnce(usize) -> E) -> Result<Digest<H>, E> {
    Digest::<H>::from_slice(b).map_err(|_| refuse(b.len()))
}

/// A list or tuple of digests of `width` bytes, each measured before it is copied. Rust converts them in order and
/// refuses at the first of another length, by that length. Retain that prefix only, but type-check every later
/// reference so a boundary `TypeMismatch` keeps precedence over the Merkle `Encoding` refusal.
fn digest_list<'py>(obj: &Bound<'py, PyAny>, what: &str, width: usize) -> Result<Vec<Vec<u8>>, Fail<'py>> {
    let mut refused = false;
    items(obj, what)?
        .iter()
        .map(|b| {
            let n = bytes_len(b)?;
            if refused {
                return Ok(None);
            }
            refused = n != width;
            bytes(b).map(|b| Some(b.into_owned()))
        })
        .filter_map(Result::transpose)
        .collect()
}

// ------------------------------------------------------------------------------------------------ Merkle

#[derive(Clone, Copy, PartialEq, Eq)]
enum Profile {
    Tagged,
    Rfc6962,
}

/// A Merkle profile over a hash: dilf4s's tagged profile, whose leaves are values committed by `commit(v)`, or RFC
/// 6962's, whose leaves are bytes.
#[pyclass(frozen, module = "dilf4s._native", name = "MerkleHashing")]
pub(crate) struct PyMerkleHashing {
    profile: Profile,
    alg: HashAlgorithm,
}

fn merkle_encoding(reason: String) -> MerkleError {
    MerkleError::Encoding { reason }
}

fn merkle_digest<H: Hasher>(b: &[u8]) -> Result<Digest<H>, MerkleError> {
    digest_of::<H, _>(b, |n| merkle_encoding(alloc::format!("a digest is {} bytes, got {n}", H::OUTPUT_SIZE)))
}

fn merkle_digests<H: Hasher>(path: &[Vec<u8>]) -> Result<Vec<Digest<H>>, MerkleError> {
    path.iter().map(|b| merkle_digest::<H>(b)).collect()
}

fn hashing_of<'a, 'py>(obj: &'a Bound<'py, PyAny>) -> Result<&'a PyMerkleHashing, Fail<'py>> {
    obj.cast::<PyMerkleHashing>()
        .map(Bound::get)
        .map_err(|_| mismatch(obj.py(), "expected a dilf4s.proofs.MerkleHashing"))
}

/// The Merkle family's refusal.
fn merkle_err<'py>(py: Python<'py>) -> impl Fn(MerkleError) -> Fail<'py> {
    move |e| e.refuse(py)
}

#[pymethods]
impl PyMerkleHashing {
    /// The dilf4s profile: tagged leaf, node and empty hashes; a leaf is a value committed by `commit(v)`.
    #[staticmethod]
    #[pyo3(signature = (algorithm = None))]
    fn tagged(py: Python<'_>, algorithm: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
        call(py, || {
            Ok(Bound::new(py, Self { profile: Profile::Tagged, alg: algorithm_or_sha256(algorithm)? })?.into_any())
        })
    }

    /// The RFC 6962 profile: `H(0x00 || d)`, `H(0x01 || l || r)` and `H("")`, over leaf bytes.
    #[staticmethod]
    #[pyo3(signature = (algorithm = None))]
    fn rfc6962(py: Python<'_>, algorithm: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
        call(py, || {
            Ok(Bound::new(py, Self { profile: Profile::Rfc6962, alg: algorithm_or_sha256(algorithm)? })?.into_any())
        })
    }

    /// `tagged` or `rfc6962`.
    #[getter]
    fn profile(&self) -> &'static str {
        match self.profile {
            Profile::Tagged => "tagged",
            Profile::Rfc6962 => "rfc6962",
        }
    }

    /// The hash algorithm.
    #[getter]
    fn algorithm(&self) -> &'static str {
        self.alg.name()
    }

    /// The digest size in bytes.
    #[getter]
    fn size(&self) -> usize {
        self.alg.output_size()
    }

    /// The root of the empty tree.
    #[getter]
    fn empty<'py>(&self, py: Python<'py>) -> Bound<'py, PyAny> {
        let e = with_hasher!(self.alg, H => match self.profile {
            Profile::Tagged => Tagged::<H>::new().empty_hash().as_bytes().to_vec(),
            Profile::Rfc6962 => Rfc6962::<H>::new().empty_hash().as_bytes().to_vec(),
        });
        py_bytes(py, &e)
    }

    /// A leaf's hash: a `Value` for the tagged profile, bytes for RFC 6962.
    fn leaf_hash(&self, leaf: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = leaf.py();
        call(py, || {
            let h = match self.profile {
                Profile::Tagged => {
                    let v = value(leaf)?;
                    with_hasher!(self.alg, H => py.detach(|| Tagged::<H>::new().leaf_hash(&v).as_bytes().to_vec()))
                }
                Profile::Rfc6962 => {
                    let b = bytes(leaf)?;
                    with_hasher!(self.alg, H => Rfc6962::<H>::new().leaf_hash(&b).as_bytes().to_vec())
                }
            };
            Ok(py_bytes(py, &h))
        })
    }

    /// An interior node's hash.
    fn node_hash(&self, left: &Bound<'_, PyAny>, right: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = left.py();
        call(py, || {
            let (l, r) = (bytes(left)?, bytes(right)?);
            let h = with_hasher!(self.alg, H => {
                let (l, r) = (merkle_digest::<H>(&l).map_err(merkle_err(py))?, merkle_digest::<H>(&r).map_err(merkle_err(py))?);
                match self.profile {
                    Profile::Tagged => Tagged::<H>::new().node_hash(&l, &r).as_bytes().to_vec(),
                    Profile::Rfc6962 => Rfc6962::<H>::new().node_hash(&l, &r).as_bytes().to_vec(),
                }
            });
            Ok(py_bytes(py, &h))
        })
    }

    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> Py<PyAny> {
        let py = other.py();
        let same = |o: &Self| self.profile == o.profile && self.alg == o.alg;
        match (other.cast::<Self>(), op) {
            (Ok(o), CompareOp::Eq) => py_bool(py, same(o.get())).unbind(),
            (Ok(o), CompareOp::Ne) => py_bool(py, !same(o.get())).unbind(),
            _ => py.NotImplemented(),
        }
    }

    fn __hash__(&self) -> u64 {
        u64::from(self.profile == Profile::Tagged)
            .wrapping_mul(31)
            .wrapping_add(u64::try_from(self.alg.output_size()).unwrap_or(0))
    }

    fn __repr__(&self) -> String {
        alloc::format!("MerkleHashing({}, {})", self.profile(), self.alg.name())
    }
}

/// The root over leaf hashes (a list or tuple of digests): adjacent nodes paired level by level, an unpaired last node
/// promoted.
#[pyfunction]
fn merkle_root(hashing: &Bound<'_, PyAny>, leaf_hashes: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = hashing.py();
    call(py, || {
        let m = hashing_of(hashing)?;
        let leaves = digest_list(leaf_hashes, "the leaf hashes", with_hasher!(m.alg, H => H::OUTPUT_SIZE))?;
        let root = with_hasher!(m.alg, H => {
            let ds = merkle_digests::<H>(&leaves).map_err(merkle_err(py))?;
            py.detach(|| match m.profile {
                Profile::Tagged => merkle::root(&Tagged::<H>::new(), &ds).as_bytes().to_vec(),
                Profile::Rfc6962 => merkle::root(&Rfc6962::<H>::new(), &ds).as_bytes().to_vec(),
            })
        });
        Ok(py_bytes(py, &root))
    })
}

/// An inclusion proof (RFC 9162 section 2.1.3): a leaf's index, the tree's size and the audit path.
#[pyclass(frozen, module = "dilf4s._native", name = "InclusionProof")]
pub(crate) struct PyInclusionProof {
    alg: HashAlgorithm,
    index: u64,
    size: u64,
    path: Vec<Vec<u8>>,
}

/// A consistency proof (RFC 9162 section 2.1.4): an old size, a new size and the path.
#[pyclass(frozen, module = "dilf4s._native", name = "ConsistencyProof")]
pub(crate) struct PyConsistencyProof {
    alg: HashAlgorithm,
    old_size: u64,
    new_size: u64,
    path: Vec<Vec<u8>>,
}

fn path_tuple<'py>(py: Python<'py>, path: &[Vec<u8>]) -> PyResult<Bound<'py, PyAny>> {
    py_tuple(py, path.iter().map(|p| py_bytes(py, p)).collect())
}

impl PyInclusionProof {
    fn typed<H: Hasher>(&self) -> Result<InclusionProof<H>, MerkleError> {
        InclusionProof::new(self.index, self.size, merkle_digests::<H>(&self.path)?)
    }
}

impl PyConsistencyProof {
    fn typed<H: Hasher>(&self) -> Result<ConsistencyProof<H>, MerkleError> {
        ConsistencyProof::new(self.old_size, self.new_size, merkle_digests::<H>(&self.path)?)
    }
}

#[pymethods]
impl PyInclusionProof {
    /// A proof from its canonical form, its digests of the hashing's algorithm.
    #[staticmethod]
    fn decode(hashing: &Bound<'_, PyAny>, form: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = hashing.py();
        call(py, || {
            let alg = hashing_of(hashing)?.alg;
            let v = value(form)?;
            let (index, size, path) = with_hasher!(alg, H => {
                let p = InclusionProof::<H>::from_value(&v).map_err(merkle_err(py))?;
                (p.index(), p.size(), p.path().iter().map(|d| d.as_bytes().to_vec()).collect())
            });
            Ok(Bound::new(py, Self { alg, index, size, path })?.into_any())
        })
    }

    /// A proof of its parts: `index < size`, both below 2**63, the path of the hashing's digests.
    #[staticmethod]
    fn of(
        hashing: &Bound<'_, PyAny>,
        index: &Bound<'_, PyAny>,
        size: &Bound<'_, PyAny>,
        path: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let py = hashing.py();
        call(py, || {
            let alg = hashing_of(hashing)?.alg;
            let (i, s) = (u64_arg(index, "the index")?, u64_arg(size, "the size")?);
            let p = digest_list(path, "the path", with_hasher!(alg, H => H::OUTPUT_SIZE))?;
            let proof = Self { alg, index: i, size: s, path: p };
            with_hasher!(alg, H => proof.typed::<H>().map(drop).map_err(merkle_err(py))?);
            Ok(Bound::new(py, proof)?.into_any())
        })
    }

    /// The leaf's index.
    #[getter]
    const fn index(&self) -> u64 {
        self.index
    }

    /// The tree's size.
    #[getter]
    const fn size(&self) -> u64 {
        self.size
    }

    /// The audit path.
    #[getter]
    fn path<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        path_tuple(py, &self.path)
    }

    /// The canonical form.
    fn encode(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        call(py, || {
            let v = with_hasher!(self.alg, H => self.typed::<H>().and_then(|p| p.to_value()).map_err(merkle_err(py))?);
            Ok(py_value(py, v)?)
        })
    }
}

#[pymethods]
impl PyConsistencyProof {
    /// A proof from its canonical form, its digests of the hashing's algorithm.
    #[staticmethod]
    fn decode(hashing: &Bound<'_, PyAny>, form: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = hashing.py();
        call(py, || {
            let alg = hashing_of(hashing)?.alg;
            let v = value(form)?;
            let (old_size, new_size, path) = with_hasher!(alg, H => {
                let p = ConsistencyProof::<H>::from_value(&v).map_err(merkle_err(py))?;
                (p.old_size(), p.new_size(), p.path().iter().map(|d| d.as_bytes().to_vec()).collect())
            });
            Ok(Bound::new(py, Self { alg, old_size, new_size, path })?.into_any())
        })
    }

    /// A proof of its parts: `old_size <= new_size`, both below 2**63.
    #[staticmethod]
    fn of(
        hashing: &Bound<'_, PyAny>,
        old_size: &Bound<'_, PyAny>,
        new_size: &Bound<'_, PyAny>,
        path: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let py = hashing.py();
        call(py, || {
            let alg = hashing_of(hashing)?.alg;
            let (o, n) = (u64_arg(old_size, "the old size")?, u64_arg(new_size, "the new size")?);
            let p = digest_list(path, "the path", with_hasher!(alg, H => H::OUTPUT_SIZE))?;
            let proof = Self { alg, old_size: o, new_size: n, path: p };
            with_hasher!(alg, H => proof.typed::<H>().map(drop).map_err(merkle_err(py))?);
            Ok(Bound::new(py, proof)?.into_any())
        })
    }

    /// The old tree's size.
    #[getter]
    const fn old_size(&self) -> u64 {
        self.old_size
    }

    /// The new tree's size.
    #[getter]
    const fn new_size(&self) -> u64 {
        self.new_size
    }

    /// The path.
    #[getter]
    fn path<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        path_tuple(py, &self.path)
    }

    /// The canonical form.
    fn encode(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        call(py, || {
            let v = with_hasher!(self.alg, H => self.typed::<H>().and_then(|p| p.to_value()).map_err(merkle_err(py))?);
            Ok(py_value(py, v)?)
        })
    }
}

fn inclusion_of<'a, 'py>(obj: &'a Bound<'py, PyAny>, alg: HashAlgorithm) -> Result<&'a PyInclusionProof, Fail<'py>> {
    let py = obj.py();
    let p = obj
        .cast::<PyInclusionProof>()
        .map(Bound::get)
        .map_err(|_| mismatch(py, "expected a dilf4s.proofs.InclusionProof"))?;
    if p.alg == alg { Ok(p) } else { Err(mismatch(py, "expected a proof of the hashing's algorithm")) }
}

fn consistency_of<'a, 'py>(
    obj: &'a Bound<'py, PyAny>,
    alg: HashAlgorithm,
) -> Result<&'a PyConsistencyProof, Fail<'py>> {
    let py = obj.py();
    let p = obj
        .cast::<PyConsistencyProof>()
        .map(Bound::get)
        .map_err(|_| mismatch(py, "expected a dilf4s.proofs.ConsistencyProof"))?;
    if p.alg == alg { Ok(p) } else { Err(mismatch(py, "expected a proof of the hashing's algorithm")) }
}

/// RFC 9162 inclusion with the reason it fails: `None` when the leaf hash is at the proof's index under `root`.
#[pyfunction]
fn merkle_check_inclusion(
    hashing: &Bound<'_, PyAny>,
    root: &Bound<'_, PyAny>,
    leaf_hash: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    inclusion(hashing, root, leaf_hash, proof, false)
}

/// RFC 9162 inclusion as a verdict: `False` for an invalid proof or another root, a `MerkleError` for malformed input.
#[pyfunction]
fn merkle_verify_inclusion(
    hashing: &Bound<'_, PyAny>,
    root: &Bound<'_, PyAny>,
    leaf_hash: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    inclusion(hashing, root, leaf_hash, proof, true)
}

fn inclusion(
    hashing: &Bound<'_, PyAny>,
    root: &Bound<'_, PyAny>,
    leaf_hash: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
    verdict: bool,
) -> PyResult<Py<PyAny>> {
    let py = hashing.py();
    call(py, || {
        let m = hashing_of(hashing)?;
        let (r, l) = (bytes(root)?, bytes(leaf_hash)?);
        let p = inclusion_of(proof, m.alg)?;
        let e = merkle_err(py);
        with_hasher!(m.alg, H => {
            let (r, l, p) = (merkle_digest::<H>(&r).map_err(&e)?, merkle_digest::<H>(&l).map_err(&e)?, p.typed::<H>().map_err(&e)?);
            let out = match (m.profile, verdict) {
                (Profile::Tagged, false) => merkle::check_inclusion(&Tagged::<H>::new(), &r, &l, &p).map(|()| None),
                (Profile::Rfc6962, false) => merkle::check_inclusion(&Rfc6962::<H>::new(), &r, &l, &p).map(|()| None),
                (Profile::Tagged, true) => merkle::verify_inclusion(&Tagged::<H>::new(), &r, &l, &p).map(Some),
                (Profile::Rfc6962, true) => merkle::verify_inclusion(&Rfc6962::<H>::new(), &r, &l, &p).map(Some),
            };
            Ok(out.map_err(e)?.map_or_else(|| py_none(py), |v| py_bool(py, v)))
        })
    })
}

/// RFC 9162 consistency with the reason it fails: `None` when the new tree extends the old.
#[pyfunction]
fn merkle_check_consistency(
    hashing: &Bound<'_, PyAny>,
    old_root: &Bound<'_, PyAny>,
    new_root: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    consistency(hashing, old_root, new_root, proof, false)
}

/// RFC 9162 consistency as a verdict.
#[pyfunction]
fn merkle_verify_consistency(
    hashing: &Bound<'_, PyAny>,
    old_root: &Bound<'_, PyAny>,
    new_root: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    consistency(hashing, old_root, new_root, proof, true)
}

fn consistency(
    hashing: &Bound<'_, PyAny>,
    old_root: &Bound<'_, PyAny>,
    new_root: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
    verdict: bool,
) -> PyResult<Py<PyAny>> {
    let py = hashing.py();
    call(py, || {
        let m = hashing_of(hashing)?;
        let (o, n) = (bytes(old_root)?, bytes(new_root)?);
        let p = consistency_of(proof, m.alg)?;
        let e = merkle_err(py);
        with_hasher!(m.alg, H => {
            let (o, n, p) = (merkle_digest::<H>(&o).map_err(&e)?, merkle_digest::<H>(&n).map_err(&e)?, p.typed::<H>().map_err(&e)?);
            let out = match (m.profile, verdict) {
                (Profile::Tagged, false) => merkle::check_consistency(&Tagged::<H>::new(), &o, &n, &p).map(|()| None),
                (Profile::Rfc6962, false) => merkle::check_consistency(&Rfc6962::<H>::new(), &o, &n, &p).map(|()| None),
                (Profile::Tagged, true) => merkle::verify_consistency(&Tagged::<H>::new(), &o, &n, &p).map(Some),
                (Profile::Rfc6962, true) => merkle::verify_consistency(&Rfc6962::<H>::new(), &o, &n, &p).map(Some),
            };
            Ok(out.map_err(e)?.map_or_else(|| py_none(py), |v| py_bool(py, v)))
        })
    })
}

/// A trusted head's root and size: the root a digest of the profile's (`Encoding` otherwise, checked first, as the
/// caller of the JLVM's `merkle_verify` parses its head), the size below 2**63.
fn head<H: Hasher>(root: &[u8], size: u64) -> Result<TreeHead<H>, MerkleError> {
    Ok(TreeHead { root: merkle_digest::<H>(root)?, size })
}

/// The JLVM's `merkle_verify`: the leaf (a `Value` for the tagged profile, bytes for RFC 6962) is at the proof's index
/// (the proof in its canonical form) of the tree `root` and `size` name. A proof of another size is `False`; a leaf
/// heavier than `MAX_PROOF_WEIGHT` is `TooLarge`, before the proof is decoded.
#[pyfunction]
fn merkle_verify(
    hashing: &Bound<'_, PyAny>,
    root: &Bound<'_, PyAny>,
    size: &Bound<'_, PyAny>,
    leaf: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = hashing.py();
    call(py, || {
        let m = hashing_of(hashing)?;
        let (r, s) = (bytes(root)?, u64_arg(size, "the size")?);
        let e = merkle_err(py);
        let verdict = match m.profile {
            Profile::Tagged => {
                let (l, p) = (value(leaf)?, value(proof)?);
                with_hasher!(m.alg, H => {
                    let h = head::<H>(&r, s).map_err(&e)?;
                    py.detach(|| merkle::merkle_verify(&Tagged::<H>::new(), &h, &l, &p))
                })
            }
            Profile::Rfc6962 => {
                let (l, p) = (bytes(leaf)?, value(proof)?);
                with_hasher!(m.alg, H => {
                    let h = head::<H>(&r, s).map_err(&e)?;
                    py.detach(|| merkle::merkle_verify(&Rfc6962::<H>::new(), &h, &l, &p))
                })
            }
        };
        Ok(py_bool(py, verdict.map_err(e)?))
    })
}

/// The JLVM's `merkle_consistency_verify`: the new tree extends the old one (the proof in its canonical form). Sizes
/// other than the heads' are `False`.
#[pyfunction]
fn merkle_consistency_verify(
    hashing: &Bound<'_, PyAny>,
    old_root: &Bound<'_, PyAny>,
    old_size: &Bound<'_, PyAny>,
    new_root: &Bound<'_, PyAny>,
    new_size: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = hashing.py();
    call(py, || {
        let m = hashing_of(hashing)?;
        let (or, os) = (bytes(old_root)?, u64_arg(old_size, "the old size")?);
        let (nr, ns) = (bytes(new_root)?, u64_arg(new_size, "the new size")?);
        let p = value(proof)?;
        let e = merkle_err(py);
        let verdict = with_hasher!(m.alg, H => {
            let (o, n) = (head::<H>(&or, os).map_err(&e)?, head::<H>(&nr, ns).map_err(&e)?);
            match m.profile {
                Profile::Tagged => merkle::merkle_consistency_verify(&Tagged::<H>::new(), &o, &n, &p),
                Profile::Rfc6962 => merkle::merkle_consistency_verify(&Rfc6962::<H>::new(), &o, &n, &p),
            }
        });
        Ok(py_bool(py, verdict.map_err(e)?))
    })
}

/// A tree head from its JLVM form `{"root": bytes, "size": n}`, as `(root, size)`.
#[pyfunction]
fn merkle_head(hashing: &Bound<'_, PyAny>, form: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = hashing.py();
    call(py, || {
        let m = hashing_of(hashing)?;
        let v = value(form)?;
        let (root, size) = with_hasher!(m.alg, H => {
            let h = TreeHead::<H>::from_value(&v).map_err(merkle_err(py))?;
            (h.root.as_bytes().to_vec(), h.size)
        });
        Ok(py_tuple(py, vec![py_bytes(py, &root), py_u64(py, size)?])?)
    })
}

// ------------------------------------------------------------------------------------------------ tries

/// The trie's hash functions (trie.md section 3) over a hash.
#[pyclass(frozen, module = "dilf4s._native", name = "TrieHashing")]
pub(crate) struct PyTrieHashing {
    alg: HashAlgorithm,
}

/// How a trie's keys are validated: `fixed(n)`, exactly `n` bytes (`1..1024`), or `variable(max)`, `0..=max` bytes.
#[pyclass(frozen, module = "dilf4s._native", name = "KeyMode")]
pub(crate) struct PyKeyMode(pub(crate) KeyMode);

fn trie_err<'py>(py: Python<'py>) -> impl Fn(TrieError) -> Fail<'py> {
    move |e| e.refuse(py)
}

fn trie_encoding(reason: String) -> TrieError {
    TrieError::Encoding { reason }
}

fn trie_digest<H: Hasher>(b: &[u8]) -> Result<Digest<H>, TrieError> {
    digest_of::<H, _>(b, |n| trie_encoding(alloc::format!("a digest is {} bytes, got {n}", H::OUTPUT_SIZE)))
}

fn trie_hashing_of<'py>(obj: &Bound<'py, PyAny>) -> Result<HashAlgorithm, Fail<'py>> {
    obj.cast::<PyTrieHashing>()
        .map(|t| t.get().alg)
        .map_err(|_| mismatch(obj.py(), "expected a dilf4s.proofs.TrieHashing"))
}

pub(crate) fn mode_of<'py>(obj: &Bound<'py, PyAny>) -> Result<KeyMode, Fail<'py>> {
    obj.cast::<PyKeyMode>().map(|m| m.get().0).map_err(|_| mismatch(obj.py(), "expected a dilf4s.proofs.KeyMode"))
}

/// A segment: `bits` bits, MSB first, in `ceil(bits / 8)` bytes whose padding is zero.
fn segment<'py>(bits: &Bound<'py, PyAny>, data: &Bound<'py, PyAny>) -> Result<Bits, Fail<'py>> {
    let py = bits.py();
    let n = u64_arg(bits, "the bit count")?;
    Bits::from_padded(n, bytes(data)?.into_owned())
        .ok_or_else(|| mismatch(py, "expected a segment: ceil(bits / 8) bytes, the padding zero"))
}

#[pymethods]
impl PyTrieHashing {
    /// The hash functions over a hash, SHA-256 unless named.
    #[staticmethod]
    #[pyo3(signature = (algorithm = None))]
    fn of(py: Python<'_>, algorithm: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
        call(py, || Ok(Bound::new(py, Self { alg: algorithm_or_sha256(algorithm)? })?.into_any()))
    }

    /// The hash algorithm.
    #[getter]
    fn algorithm(&self) -> &'static str {
        self.alg.name()
    }

    /// The digest size in bytes.
    #[getter]
    fn size(&self) -> usize {
        self.alg.output_size()
    }

    /// The root of the empty trie.
    #[getter]
    fn empty<'py>(&self, py: Python<'py>) -> Bound<'py, PyAny> {
        py_bytes(py, &with_hasher!(self.alg, H => TrieHashing::<H>::new().empty().as_bytes().to_vec()))
    }

    /// `valueHash(commit(v))`: the hash a trie commits for a value, as a JLVM program claims it.
    fn value_hash(&self, value: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = value.py();
        call(py, || {
            let v = crate::boundary::value(value)?;
            Ok(py_bytes(
                py,
                &with_hasher!(self.alg, H => py.detach(|| TrieHashing::<H>::new().value_of(&v).as_bytes().to_vec())),
            ))
        })
    }

    /// A leaf's hash: its segment (`bits` bits in `data`) and value hash.
    fn leaf_hash(
        &self,
        bits: &Bound<'_, PyAny>,
        data: &Bound<'_, PyAny>,
        value_hash: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let py = bits.py();
        call(py, || {
            let s = segment(bits, data)?;
            let v = bytes(value_hash)?;
            let h = with_hasher!(self.alg, H => TrieHashing::<H>::new().leaf(&s, &trie_digest::<H>(&v).map_err(trie_err(py))?).as_bytes().to_vec());
            Ok(py_bytes(py, &h))
        })
    }

    /// A branch's hash: its segment, and its value hash and children's hashes where present.
    #[pyo3(signature = (bits, data, value_hash = None, left = None, right = None))]
    fn branch_hash(
        &self,
        bits: &Bound<'_, PyAny>,
        data: &Bound<'_, PyAny>,
        value_hash: Option<&Bound<'_, PyAny>>,
        left: Option<&Bound<'_, PyAny>>,
        right: Option<&Bound<'_, PyAny>>,
    ) -> PyResult<Py<PyAny>> {
        let py = bits.py();
        call(py, || {
            let s = segment(bits, data)?;
            // one buffer passed as more than one hash is read once, and `bytes` are borrowed, not copied
            let mut reads = Reads::default();
            let (v, l, r) = (reads.optional(value_hash)?, reads.optional(left)?, reads.optional(right)?);
            let h = with_hasher!(self.alg, H => {
                let d = |b: &Option<Held<'_>>| b.as_ref().map(|b| trie_digest::<H>(reads.get(b))).transpose().map_err(trie_err(py));
                let (v, l, r) = (d(&v)?, d(&l)?, d(&r)?);
                TrieHashing::<H>::new().branch(&s, v.as_ref(), l.as_ref(), r.as_ref()).as_bytes().to_vec()
            });
            Ok(py_bytes(py, &h))
        })
    }

    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> Py<PyAny> {
        let py = other.py();
        match (other.cast::<Self>(), op) {
            (Ok(o), CompareOp::Eq) => py_bool(py, self.alg == o.get().alg).unbind(),
            (Ok(o), CompareOp::Ne) => py_bool(py, self.alg != o.get().alg).unbind(),
            _ => py.NotImplemented(),
        }
    }

    fn __repr__(&self) -> String {
        alloc::format!("TrieHashing({})", self.alg.name())
    }
}

#[pymethods]
impl PyKeyMode {
    /// `fixed(n)`: exactly `n` bytes, `1..1024`.
    #[staticmethod]
    fn fixed(n: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = n.py();
        call(py, || {
            Ok(Bound::new(py, Self(KeyMode::fixed(u64_arg(n, "the key length")?).map_err(trie_err(py))?))?.into_any())
        })
    }

    /// `variable(max)`: at most `max` bytes, `0..65535`.
    #[staticmethod]
    fn variable(max: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = max.py();
        call(py, || {
            Ok(Bound::new(py, Self(KeyMode::variable(u64_arg(max, "the key limit")?).map_err(trie_err(py))?))?
                .into_any())
        })
    }

    /// A mode from its JLVM form, `{"fixed": n}` or `{"variable": max}`.
    #[staticmethod]
    fn from_value(form: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = form.py();
        call(py, || Ok(Bound::new(py, Self(KeyMode::from_value(&value(form)?).map_err(trie_err(py))?))?.into_any()))
    }

    /// `n` for `fixed(n)`, else `None`.
    #[getter]
    fn fixed_bytes(&self) -> Option<u16> {
        self.0.fixed_bytes()
    }

    /// `max` for `variable(max)`, else `None`.
    #[getter]
    fn variable_max(&self) -> Option<u16> {
        self.0.variable_max()
    }

    /// Checks a key's length against the mode.
    fn check_key(&self, key: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = key.py();
        call(py, || {
            self.0.check_key(&bytes(key)?).map_err(trie_err(py))?;
            Ok(py_none(py))
        })
    }

    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> Py<PyAny> {
        let py = other.py();
        match (other.cast::<Self>(), op) {
            (Ok(o), CompareOp::Eq) => py_bool(py, self.0 == o.get().0).unbind(),
            (Ok(o), CompareOp::Ne) => py_bool(py, self.0 != o.get().0).unbind(),
            _ => py.NotImplemented(),
        }
    }

    fn __hash__(&self) -> u64 {
        self.0.fixed_bytes().map_or_else(|| u64::from(self.0.variable_max().unwrap_or(0)) << 17, u64::from)
    }

    fn __repr__(&self) -> String {
        alloc::format!("KeyMode({})", self.0)
    }
}

/// `(key, valueHash)` entries (a list or tuple of pairs of bytes), each part checked to be bytes, in order, and none
/// copied: they are read through [`Reads`].
fn hash_entries<'py>(obj: &Bound<'py, PyAny>) -> Result<Vec<[Bound<'py, PyAny>; 2]>, Fail<'py>> {
    items(obj, "the entries")?
        .iter()
        .map(|e| {
            let [k, v] = tuple_of::<2>(e, "an entry", "expected (key, value hash) entries")?;
            bytes_len(&k)?;
            bytes_len(&v)?;
            Ok([k, v])
        })
        .collect()
}

/// `(key, value)` claims (a list or tuple of pairs of bytes and a `Value`): each key checked to be bytes and not yet
/// copied (it is read through [`Reads`]), each value read.
fn claims<'py>(obj: &Bound<'py, PyAny>) -> Result<Vec<(Bound<'py, PyAny>, Value)>, Fail<'py>> {
    items(obj, "the entries")?
        .iter()
        .map(|e| {
            let [k, v] = tuple_of::<2>(e, "an entry", "expected (key, value) entries")?;
            bytes_len(&k)?;
            Ok((k, value(&v)?))
        })
        .collect()
}

/// The keys of `claims`, read through `reads`.
fn claim_keys<'a, 'py>(reads: &mut Reads, es: &'a [(Bound<'py, PyAny>, Value)]) -> Result<Vec<Held<'a>>, Fail<'py>> {
    es.iter().map(|(k, _)| reads.read(k)).collect()
}

/// The root of the trie holding `(key, valueHash)` entries (later duplicates win), each key checked against the mode.
#[pyfunction]
fn trie_root(hashing: &Bound<'_, PyAny>, mode: &Bound<'_, PyAny>, entries: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = hashing.py();
    call(py, || {
        let alg = trie_hashing_of(hashing)?;
        let m = mode_of(mode)?;
        let es = hash_entries(entries)?;
        let mut reads = Reads::default();
        let held =
            es.iter().map(|[k, v]| Ok((reads.read(k)?, reads.read(v)?))).collect::<Result<Vec<_>, Fail<'_>>>()?;
        let e = trie_err(py);
        let root = with_hasher!(alg, H => {
            let typed = held.iter().map(|(k, v)| Ok((reads.get(k), trie_digest::<H>(reads.get(v))?))).collect::<Result<Vec<_>, TrieError>>().map_err(&e)?;
            py.detach(|| trie::root_from_entries(&TrieHashing::<H>::new(), m, typed)).map_err(e)?.as_bytes().to_vec()
        });
        Ok(py_bytes(py, &root))
    })
}

/// The JLVM's `trie_verify`: `key` holds `value` (committed by `commit(v)`; `None` claims absence) in the trie of
/// `root`, by the proof (its canonical form). Another trie is `False`; a proof that does not reach the key is
/// `NotCovered`.
#[pyfunction]
#[pyo3(signature = (hashing, mode, root, key, claimed, proof))]
fn trie_verify(
    hashing: &Bound<'_, PyAny>,
    mode: &Bound<'_, PyAny>,
    root: &Bound<'_, PyAny>,
    key: &Bound<'_, PyAny>,
    claimed: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = hashing.py();
    call(py, || {
        let alg = trie_hashing_of(hashing)?;
        let m = mode_of(mode)?;
        let (r, k) = (bytes(root)?, bytes(key)?);
        let v = if claimed.is_none() { Value::Null } else { value(claimed)? };
        let p = value(proof)?;
        let verdict =
            with_hasher!(alg, H => py.detach(|| trie::trie_verify(&TrieHashing::<H>::new(), m, &r, &k, &v, &p)));
        Ok(py_bool(py, verdict.map_err(trie_err(py))?))
    })
}

/// The JLVM's `trie_prefix_verify`: `entries` (key order) are exactly the trie's entries under `prefix`.
#[pyfunction]
fn trie_prefix_verify(
    hashing: &Bound<'_, PyAny>,
    mode: &Bound<'_, PyAny>,
    root: &Bound<'_, PyAny>,
    prefix: &Bound<'_, PyAny>,
    entries: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = hashing.py();
    call(py, || {
        let alg = trie_hashing_of(hashing)?;
        let m = mode_of(mode)?;
        let (r, x) = (bytes(root)?, bytes(prefix)?);
        let es = claims(entries)?;
        let p = value(proof)?;
        let mut reads = Reads::default();
        let keys = claim_keys(&mut reads, &es)?;
        let claimed: Vec<(&[u8], &Value)> = keys.iter().zip(&es).map(|(k, (_, v))| (reads.get(k), v)).collect();
        let verdict = with_hasher!(alg, H => py.detach(|| trie::trie_prefix_verify(&TrieHashing::<H>::new(), m, &r, &x, &claimed, &p)));
        Ok(py_bool(py, verdict.map_err(trie_err(py))?))
    })
}

/// The JLVM's `trie_range_verify`: `entries` (key order) are exactly the trie's entries in `[start, until)`; `until`
/// `None` is unbounded.
#[pyfunction]
#[pyo3(signature = (hashing, mode, root, start, until, entries, proof))]
fn trie_range_verify(
    hashing: &Bound<'_, PyAny>,
    mode: &Bound<'_, PyAny>,
    root: &Bound<'_, PyAny>,
    start: &Bound<'_, PyAny>,
    until: &Bound<'_, PyAny>,
    entries: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = hashing.py();
    call(py, || {
        let alg = trie_hashing_of(hashing)?;
        let m = mode_of(mode)?;
        let mut reads = Reads::default();
        let (r, f) = (reads.read(root)?, reads.read(start)?);
        let u = if until.is_none() { None } else { Some(reads.read(until)?) };
        let es = claims(entries)?;
        let p = value(proof)?;
        let keys = claim_keys(&mut reads, &es)?;
        let claimed: Vec<(&[u8], &Value)> = keys.iter().zip(&es).map(|(k, (_, v))| (reads.get(k), v)).collect();
        let verdict = with_hasher!(alg, H => py.detach(|| {
            trie::trie_range_verify(&TrieHashing::<H>::new(), m, reads.get(&r), reads.get(&f), u.as_ref().map(|u| reads.get(u)), &claimed, &p)
        }));
        Ok(py_bool(py, verdict.map_err(trie_err(py))?))
    })
}

/// `(key, digest)` pairs, as bytes.
type Listed = Vec<(Vec<u8>, Vec<u8>)>;

/// What a checked trie says about a key, its digest as bytes (the boundary erases the hash).
enum Found {
    Present(Vec<u8>),
    Absent,
    NotCovered,
}

/// A partial trie checked against a trusted root: what it says about keys, prefixes and ranges.
trait CheckedTrie: Send + Sync {
    fn lookup(&self, key: &[u8]) -> Result<Found, TrieError>;
    fn prefix(&self, prefix: &[u8]) -> Result<Option<Listed>, TrieError>;
    fn range(&self, from: &[u8], until: Option<&[u8]>) -> Result<Option<Listed>, TrieError>;
    fn encode(&self) -> Result<Value, TrieError>;
}

struct Checked<H: Hasher> {
    hashing: TrieHashing<H>,
    mode: KeyMode,
    root: Digest<H>,
    proof: TrieProof<H>,
}

fn listing<H: Hasher>(l: Option<Vec<(Vec<u8>, Digest<H>)>>) -> Option<Listed> {
    l.map(|es| es.into_iter().map(|(k, v)| (k, v.as_bytes().to_vec())).collect())
}

impl<H: Hasher + Send + Sync> CheckedTrie for Checked<H>
where
    H::Output: Send + Sync,
{
    fn lookup(&self, key: &[u8]) -> Result<Found, TrieError> {
        // checked against the trusted root when built; checked again here, as each answer stands alone
        let v = self.proof.check(&self.hashing, self.mode, &self.root)?;
        self.mode.path(key)?;
        Ok(match v.lookup(key) {
            Lookup::Present(h) => Found::Present(h.as_bytes().to_vec()),
            Lookup::Absent => Found::Absent,
            Lookup::NotCovered => Found::NotCovered,
        })
    }

    fn prefix(&self, prefix: &[u8]) -> Result<Option<Listed>, TrieError> {
        let v = self.proof.check(&self.hashing, self.mode, &self.root)?;
        Ok(listing(v.prefix(prefix)))
    }

    fn range(&self, from: &[u8], until: Option<&[u8]>) -> Result<Option<Listed>, TrieError> {
        let v = self.proof.check(&self.hashing, self.mode, &self.root)?;
        Ok(listing(v.range(from, until)))
    }

    fn encode(&self) -> Result<Value, TrieError> {
        self.proof.to_value()
    }
}

/// A partial trie checked against a trusted root (`trie_check`).
#[pyclass(frozen, module = "dilf4s._native", name = "VerifiedTrieProof")]
pub(crate) struct PyVerifiedTrie(Box<dyn CheckedTrie>);

fn py_listing(py: Python<'_>, l: Option<Listed>) -> PyResult<Bound<'_, PyAny>> {
    match l {
        None => instance(py, Module::ProofTypes, "NotCovered", &[]),
        Some(es) => {
            let pairs = es
                .iter()
                .map(|(k, v)| py_tuple(py, vec![py_bytes(py, k), py_bytes(py, v)]))
                .collect::<PyResult<Vec<_>>>()?;
            py_tuple(py, pairs)
        }
    }
}

#[pymethods]
impl PyVerifiedTrie {
    /// What the proof says about `key` (checked against the mode): `Present(value_hash)`, `Absent()` or `NotCovered()`.
    fn lookup(&self, key: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = key.py();
        call(py, || {
            let k = bytes(key)?;
            let found = match self.0.lookup(&k).map_err(trie_err(py))? {
                Found::Present(h) => instance(py, Module::ProofTypes, "Present", &[("value_hash", py_bytes(py, &h))])?,
                Found::Absent => instance(py, Module::ProofTypes, "Absent", &[])?,
                Found::NotCovered => instance(py, Module::ProofTypes, "NotCovered", &[])?,
            };
            Ok(found)
        })
    }

    /// Every `(key, valueHash)` under `prefix` in key order, or `NotCovered()` if the proof does not cover it all.
    fn prefix(&self, prefix: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = prefix.py();
        call(py, || Ok(py_listing(py, self.0.prefix(&bytes(prefix)?).map_err(trie_err(py))?)?))
    }

    /// Every `(key, valueHash)` in `[start, until)` (`until` `None` is unbounded), or `NotCovered()`.
    #[pyo3(signature = (start, until = None))]
    fn range(&self, start: &Bound<'_, PyAny>, until: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
        let py = start.py();
        call(py, || {
            let f = bytes(start)?;
            let u = until.filter(|u| !u.is_none()).map(bytes).transpose()?;
            Ok(py_listing(py, self.0.range(&f, u.as_deref()).map_err(trie_err(py))?)?)
        })
    }

    /// The proof's canonical form.
    fn encode(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        call(py, || Ok(py_value(py, self.0.encode().map_err(trie_err(py))?)?))
    }
}

/// Checks a partial trie (its canonical form) against `mode` and a trusted root: every failure is an error,
/// `RootMismatch` included. With `verdict`, a root mismatch is `None` instead.
fn trie_checked<'py>(
    hashing: &Bound<'py, PyAny>,
    mode: &Bound<'py, PyAny>,
    root: &Bound<'py, PyAny>,
    proof: &Bound<'py, PyAny>,
    verdict: bool,
) -> Answer<'py> {
    let py = hashing.py();
    let alg = trie_hashing_of(hashing)?;
    let m = mode_of(mode)?;
    let r = bytes(root)?;
    let p = value(proof)?;
    let e = trie_err(py);
    let checked: Option<Box<dyn CheckedTrie>> = with_hasher!(alg, H => {
        let hashing = TrieHashing::<H>::new();
        let proof = TrieProof::<H>::from_value(&p).map_err(&e)?;
        // a root of the wrong width is no root of this trie: refused as the proof check refuses another root
        let root = Digest::<H>::from_slice(&r).map_err(|_| TrieError::RootMismatch {
            expected: alloc::format!("a root of {} bytes", r.len()),
            computed: String::from("a root of the hashing's size"),
        });
        match root.and_then(|root| proof.check(&hashing, m, &root).map(drop).map(|()| root)) {
            Ok(root) => {
                let c: Box<dyn CheckedTrie> = Box::new(Checked { hashing, mode: m, root, proof });
                Some(c)
            }
            Err(TrieError::RootMismatch { .. }) if verdict => None,
            Err(x) => return Err(e(x)),
        }
    });
    match checked {
        Some(c) => Ok(Bound::new(py, PyVerifiedTrie(c))?.into_any()),
        None => Ok(py_none(py)),
    }
}

/// trie.md section 4: the proof (canonical form) checked against `mode` and `root`; every failure an error.
#[pyfunction]
fn trie_check(
    hashing: &Bound<'_, PyAny>,
    mode: &Bound<'_, PyAny>,
    root: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = hashing.py();
    call(py, || trie_checked(hashing, mode, root, proof, false))
}

/// As `trie_check`, with a root mismatch as `None`: a well-formed proof of another trie.
#[pyfunction]
fn trie_verify_proof(
    hashing: &Bound<'_, PyAny>,
    mode: &Bound<'_, PyAny>,
    root: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = hashing.py();
    call(py, || trie_checked(hashing, mode, root, proof, true))
}

// ------------------------------------------------------------------------------------------------ Poseidon

fn poseidon_err<'py>(py: Python<'py>) -> impl Fn(PoseidonError) -> Fail<'py> {
    move |e| e.refuse(py)
}

fn pmt_err<'py>(py: Python<'py>) -> impl Fn(PoseidonMerkleError) -> Fail<'py> {
    move |e| e.refuse(py)
}

/// Java's `bitLength` of a Python `int`, through `int`'s own methods: what a refusal of a wide number names.
fn java_bits<'py>(obj: &Bound<'py, PyAny>) -> Result<u64, Fail<'py>> {
    let py = obj.py();
    let int_type = py.get_type::<pyo3::types::PyInt>();
    // `int`'s own comparison, never a subclass's
    let negative: bool = int_type.getattr("__lt__")?.call1((obj, 0))?.extract()?;
    let magnitude = if negative { int_type.getattr("__invert__")?.call1((obj,))? } else { obj.clone() };
    Ok(int_type.getattr("bit_length")?.call1((magnitude,))?.extract()?)
}

/// A BN254 scalar field element: an `int` in `[0, r)`, else `NonCanonical` with its bit length.
fn fr<'py>(obj: &Bound<'py, PyAny>) -> Result<Fr, Fail<'py>> {
    let py = obj.py();
    match bigint(obj, 512)? {
        Some(n) => poseidon::fr_from_int(&n).map_err(poseidon_err(py)),
        None => Err(PoseidonError::NonCanonical { bits: java_bits(obj)? }.refuse(py)),
    }
}

fn py_fr(py: Python<'_>, x: Fr) -> PyResult<Bound<'_, PyAny>> {
    py_int(py, &poseidon::fr_to_int(x))
}

/// At most `max` field elements, or the count of more, refused by it before any element is read (as TypeScript's
/// `frsArg`): Rust refuses more by their count alone.
fn frs_within<'py>(obj: &Bound<'py, PyAny>, what: &str, max: usize) -> Result<Result<Vec<Fr>, u64>, Fail<'py>> {
    match items_within(obj, what, max)? {
        Ok(xs) => xs.iter().map(fr).collect::<Result<Vec<_>, _>>().map(Ok),
        Err(n) => Ok(Err(u64::try_from(n).unwrap_or(u64::MAX))),
    }
}

/// Poseidon (circomlib's parameters) of one to four field elements; more are refused by their count, unread.
#[pyfunction]
fn poseidon_hash(inputs: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = inputs.py();
    call(py, || {
        let max = poseidon::MAX_INPUTS;
        let xs = frs_within(inputs, "the inputs", max)?.map_err(|count| {
            PoseidonError::InputCount { count, max: u64::try_from(max).unwrap_or(u64::MAX) }.refuse(py)
        })?;
        Ok(py_fr(py, poseidon::hash(&xs).map_err(poseidon_err(py))?)?)
    })
}

/// The Poseidon Merkle tree's node compression: Poseidon of two elements.
#[pyfunction]
fn poseidon_compress(left: &Bound<'_, PyAny>, right: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = left.py();
    call(py, || Ok(py_fr(py, poseidon::compress(fr(left)?, fr(right)?))?))
}

/// A field element from 32 big-endian bytes, which must be below `r`.
#[pyfunction]
fn fr_from_bytes(data: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = data.py();
    call(py, || Ok(py_fr(py, poseidon::fr_from_be_bytes(&bytes(data)?).map_err(poseidon_err(py))?)?))
}

/// A field element's 32 big-endian bytes.
#[pyfunction]
fn fr_to_bytes(x: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = x.py();
    call(py, || Ok(py_bytes(py, &poseidon::fr_to_be_bytes(fr(x)?))))
}

/// A Poseidon Merkle proof (zk.md section 4): a leaf's position and its siblings from the leaf up.
#[pyclass(frozen, module = "dilf4s._native", name = "PoseidonMerkleProof")]
pub(crate) struct PyPoseidonProof(PoseidonMerkleProof);

fn pmt_of<'a, 'py>(obj: &'a Bound<'py, PyAny>) -> Result<&'a PoseidonMerkleProof, Fail<'py>> {
    obj.cast::<PyPoseidonProof>()
        .map(|p| &p.get().0)
        .map_err(|_| mismatch(obj.py(), "expected a dilf4s.proofs.PoseidonMerkleProof"))
}

#[pymethods]
impl PyPoseidonProof {
    /// A proof of a position (below `2**depth`) and its siblings, at most `MAX_DEPTH` of them.
    #[staticmethod]
    fn of(position: &Bound<'_, PyAny>, siblings: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = position.py();
        call(py, || {
            let Some(p) = bigint(position, u64::MAX)? else {
                return Err(mismatch(py, "expected an int"));
            };
            let Some(p) = p.to_biguint() else {
                return Err(mismatch(py, "expected a non-negative position"));
            };
            // past MAX_DEPTH siblings Rust refuses the depth whatever they are, so they are refused by their count
            let s = frs_within(siblings, "the siblings", dilf4s_proofs::poseidon::merkle::MAX_DEPTH)?
                .map_err(|depth| PoseidonMerkleError::Position { position_bits: p.bits(), depth }.refuse(py))?;
            Ok(Bound::new(py, Self(PoseidonMerkleProof::new(p, s).map_err(pmt_err(py))?))?.into_any())
        })
    }

    /// A proof from its canonical form `{"position": n, "siblings": [...]}`.
    #[staticmethod]
    fn decode(form: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = form.py();
        call(py, || {
            Ok(Bound::new(py, Self(PoseidonMerkleProof::from_value(&value(form)?).map_err(pmt_err(py))?))?.into_any())
        })
    }

    /// The position.
    #[getter]
    fn position<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        py_int(py, &BigInt::from(self.0.position().clone()))
    }

    /// The siblings, from the leaf up.
    #[getter]
    fn siblings<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        py_tuple(py, self.0.siblings().iter().map(|s| py_fr(py, *s)).collect::<PyResult<Vec<_>>>()?)
    }

    /// The depth: the number of siblings.
    #[getter]
    fn depth(&self) -> usize {
        self.0.depth()
    }

    /// The canonical form.
    fn encode(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        call(py, || Ok(py_value(py, self.0.to_value().map_err(pmt_err(py))?)?))
    }

    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> Py<PyAny> {
        let py = other.py();
        match (other.cast::<Self>(), op) {
            (Ok(o), CompareOp::Eq) => py_bool(py, self.0 == o.get().0).unbind(),
            (Ok(o), CompareOp::Ne) => py_bool(py, self.0 != o.get().0).unbind(),
            _ => py.NotImplemented(),
        }
    }
}

/// The root a leaf and its proof hash to.
#[pyfunction]
fn poseidon_merkle_root(leaf: &Bound<'_, PyAny>, proof: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = leaf.py();
    call(py, || Ok(py_fr(py, dilf4s_proofs::poseidon::merkle::compute_root(fr(leaf)?, pmt_of(proof)?))?))
}

/// Whether the leaf is at the proof's position under `root`.
#[pyfunction]
fn poseidon_verify_inclusion(
    leaf: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
    root: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = leaf.py();
    call(py, || {
        Ok(py_bool(py, dilf4s_proofs::poseidon::merkle::verify_inclusion(fr(leaf)?, pmt_of(proof)?, fr(root)?)))
    })
}

/// Whether the proof's position is empty (the zero leaf) under `root`.
#[pyfunction]
fn poseidon_verify_absence(proof: &Bound<'_, PyAny>, root: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = proof.py();
    call(py, || Ok(py_bool(py, dilf4s_proofs::poseidon::merkle::verify_absence(pmt_of(proof)?, fr(root)?))))
}

/// The empty subtrees' roots, from the leaf (0) up to `depth`, at most `MAX_DEPTH`.
#[pyfunction]
fn poseidon_zero_hashes(depth: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = depth.py();
    call(py, || {
        let d = usize_arg(depth, "the depth")?;
        let max = dilf4s_proofs::poseidon::merkle::MAX_DEPTH;
        if d > max {
            return Err(PoseidonMerkleError::Depth { depth: i64::try_from(d).unwrap_or(i64::MAX) }.refuse(py));
        }
        let zs = dilf4s_proofs::poseidon::merkle::zero_hashes(d);
        Ok(py_tuple(py, zs.into_iter().map(|z| py_fr(py, z)).collect::<PyResult<Vec<_>>>()?)?)
    })
}

// ------------------------------------------------------------------------------------------------ BN254, Groth16, SP1

fn zk_err<'py>(py: Python<'py>) -> impl Fn(ZkError) -> Fail<'py> {
    move |e| e.refuse(py)
}

fn groth16_err<'py>(py: Python<'py>) -> impl Fn(Groth16Error) -> Fail<'py> {
    move |e| e.refuse(py)
}

/// EIP-196 `ECADD` of two 64-byte G1 points.
#[pyfunction]
fn bn254_add(a: &Bound<'_, PyAny>, b: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = a.py();
    call(py, || Ok(py_bytes(py, &zk::add(&bytes(a)?, &bytes(b)?).map_err(zk_err(py))?)))
}

/// EIP-196 `ECMUL` of a 64-byte G1 point by a 32-byte scalar.
#[pyfunction]
fn bn254_mul(point: &Bound<'_, PyAny>, scalar: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = point.py();
    call(py, || {
        let (p, k) = (bytes(point)?, bytes(scalar)?);
        Ok(py_bytes(py, &py.detach(|| zk::mul(&p, &k)).map_err(zk_err(py))?))
    })
}

/// EIP-197's pairing check over `(G1, G2)` pairs (64 and 128 bytes): whether the product of the pairings is one. The
/// GIL is released while it runs.
#[pyfunction]
fn bn254_pairing(pairs: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = pairs.py();
    call(py, || {
        // each pair checked in order, none copied; then read once each (shared buffers once in all)
        let parts = items(pairs, "the pairs")?
            .iter()
            .map(|p| {
                let [a, b] = tuple_of::<2>(p, "a pair", "expected (G1, G2) pairs")?;
                bytes_len(&a)?;
                bytes_len(&b)?;
                Ok([a, b])
            })
            .collect::<Result<Vec<_>, Fail<'_>>>()?;
        let mut reads = Reads::default();
        let held =
            parts.iter().map(|[a, b]| Ok((reads.read(a)?, reads.read(b)?))).collect::<Result<Vec<_>, Fail<'_>>>()?;
        let borrowed: Vec<(&[u8], &[u8])> = held.iter().map(|(a, b)| (reads.get(a), reads.get(b))).collect();
        Ok(py_bool(py, py.detach(|| zk::pairing(&borrowed)).map_err(zk_err(py))?))
    })
}

/// A Groth16 verifying key over BN254: every point finite.
#[pyclass(frozen, module = "dilf4s._native", name = "Groth16VerifyingKey")]
pub(crate) struct PyGroth16Key(groth16::VerifyingKey);

fn fixed<'py, const N: usize>(obj: &Bound<'py, PyAny>, what: &str) -> Result<[u8; N], Fail<'py>> {
    let py = obj.py();
    // measured first: a buffer of another width is refused by its length, never copied
    let n = bytes_len(obj)?;
    if n != N {
        return Err(Groth16Error::Malformed { reason: alloc::format!("{what} must be {N} bytes, got {n}") }.refuse(py));
    }
    let b = bytes(obj)?;
    <[u8; N]>::try_from(b.as_ref()).map_err(|_| {
        Groth16Error::Malformed { reason: alloc::format!("{what} must be {N} bytes, got {}", b.len()) }.refuse(py)
    })
}

#[pymethods]
impl PyGroth16Key {
    /// A key from its points: `alpha` (G1, 64 bytes), `beta`, `gamma` and `delta` (G2, 128 bytes), and `ic`, `IC_0`
    /// then one G1 point per public input.
    #[staticmethod]
    fn from_bytes(
        alpha: &Bound<'_, PyAny>,
        beta: &Bound<'_, PyAny>,
        gamma: &Bound<'_, PyAny>,
        delta: &Bound<'_, PyAny>,
        ic: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let py = alpha.py();
        call(py, || {
            let a = fixed::<64>(alpha, "alpha")?;
            let (b, g, d) = (fixed::<128>(beta, "beta")?, fixed::<128>(gamma, "gamma")?, fixed::<128>(delta, "delta")?);
            // each distinct IC point read once, in the order first met; Rust refuses the header, then the first point
            // that fails, and a repeat of a point that passes passes, so the key of the distinct points refuses as the
            // full key would, and only a key that passes is built from every reference
            let objs = items(ic, "IC")?;
            let mut first_at: BTreeMap<usize, usize> = BTreeMap::new();
            let mut distinct: Vec<[u8; 64]> = Vec::new();
            let mut positions: Vec<usize> = Vec::with_capacity(objs.len());
            for p in &objs {
                let at = if let Some(&at) = first_at.get(&p.as_ptr().addr()) {
                    at
                } else {
                    distinct.push(fixed::<64>(p, "an IC point")?);
                    let at = distinct.len().saturating_sub(1);
                    first_at.insert(p.as_ptr().addr(), at);
                    at
                };
                positions.push(at);
            }
            let once = groth16::VerifyingKey::from_bytes(&a, &b, &g, &d, &distinct).map_err(groth16_err(py))?;
            let key = if distinct.len() == positions.len() {
                once
            } else {
                let points: Vec<[u8; 64]> = positions.iter().filter_map(|&at| distinct.get(at).copied()).collect();
                groth16::VerifyingKey::from_bytes(&a, &b, &g, &d, &points).map_err(groth16_err(py))?
            };
            Ok(Bound::new(py, Self(key))?.into_any())
        })
    }

    /// SP1's Groth16 verifying key (`SP1VerifierGroth16.sol`, circuit v6.1.0).
    #[staticmethod]
    fn sp1(py: Python<'_>) -> PyResult<Py<PyAny>> {
        call(py, || Ok(Bound::new(py, Self(groth16::sp1::verifying_key().map_err(groth16_err(py))?))?.into_any()))
    }

    /// The number of public inputs a proof under this key takes.
    #[getter]
    fn public_inputs(&self) -> usize {
        self.0.public_inputs()
    }
}

/// Groth16 verification over BN254: the 256-byte proof `A || B || C` against the key and the public inputs (32-byte
/// big-endian words). `None` when it verifies; `PairingCheckFailed` when it does not. The GIL is released while it
/// runs.
#[pyfunction]
fn groth16_verify(key: &Bound<'_, PyAny>, proof: &Bound<'_, PyAny>, inputs: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = key.py();
    call(py, || {
        let k = &key
            .cast::<PyGroth16Key>()
            .map_err(|_| mismatch(py, "expected a dilf4s.proofs.Groth16VerifyingKey"))?
            .get()
            .0;
        let p = bytes(proof)?;
        let (n, m) = (k.public_inputs(), count(inputs, "the public inputs")?);
        if m > n {
            // Rust checks the proof, then the inputs' count, before it reads an input: more than the key takes are
            // refused by their count, unread, after the proof's own checks (made by Rust over n + 1 zero words,
            // as many as the key's points bound)
            let over = |got: usize| alloc::format!("expected {n} public inputs, got {got}");
            let probe = groth16::verify(k, &p, &vec![[0u8; 32]; n.saturating_add(1)]);
            let refusal = match probe {
                Err(Groth16Error::Malformed { reason }) if reason == over(n.saturating_add(1)) => {
                    Groth16Error::Malformed { reason: over(m) }
                }
                Err(e) => e,
                Ok(()) => Groth16Error::Malformed { reason: over(m) },
            };
            return Err(refusal.refuse(py));
        }
        let words = items(inputs, "the public inputs")?
            .iter()
            .map(|w| fixed::<32>(w, "a public input"))
            .collect::<Result<Vec<_>, _>>()?;
        py.detach(|| groth16::verify(k, &p, &words)).map_err(groth16_err(py))?;
        Ok(py_none(py))
    })
}

/// SP1's Groth16 verifier: the program with key `program_vkey` (32 bytes) committed `public_values`, by the
/// 256-byte `proof` (selector, exit code, VK root, nonce, then the Groth16 proof). `None` when it verifies.
#[pyfunction]
fn sp1_verify(
    program_vkey: &Bound<'_, PyAny>,
    public_values: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = program_vkey.py();
    call(py, || {
        let mut reads = Reads::default();
        let (k, v, p) = (reads.read(program_vkey)?, reads.read(public_values)?, reads.read(proof)?);
        let (k, v, p) = (reads.get(&k), reads.get(&v), reads.get(&p));
        py.detach(|| groth16::sp1::verify(k, v, p)).map_err(groth16_err(py))?;
        Ok(py_none(py))
    })
}

/// `sha256(public_values)` with its top three bits cleared: the public input SP1 commits.
#[pyfunction]
fn sp1_hash_public_values(public_values: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = public_values.py();
    call(py, || Ok(py_bytes(py, &groth16::sp1::hash_public_values(&bytes(public_values)?))))
}

pub(crate) fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PyMerkleHashing>()?;
    m.add_class::<PyInclusionProof>()?;
    m.add_class::<PyConsistencyProof>()?;
    m.add_class::<PyTrieHashing>()?;
    m.add_class::<PyKeyMode>()?;
    m.add_class::<PyVerifiedTrie>()?;
    m.add_class::<PyPoseidonProof>()?;
    m.add_class::<PyGroth16Key>()?;
    m.add_function(wrap_pyfunction!(merkle_root, m)?)?;
    m.add_function(wrap_pyfunction!(merkle_check_inclusion, m)?)?;
    m.add_function(wrap_pyfunction!(merkle_verify_inclusion, m)?)?;
    m.add_function(wrap_pyfunction!(merkle_check_consistency, m)?)?;
    m.add_function(wrap_pyfunction!(merkle_verify_consistency, m)?)?;
    m.add_function(wrap_pyfunction!(merkle_verify, m)?)?;
    m.add_function(wrap_pyfunction!(merkle_consistency_verify, m)?)?;
    m.add_function(wrap_pyfunction!(merkle_head, m)?)?;
    m.add_function(wrap_pyfunction!(trie_root, m)?)?;
    m.add_function(wrap_pyfunction!(trie_verify, m)?)?;
    m.add_function(wrap_pyfunction!(trie_prefix_verify, m)?)?;
    m.add_function(wrap_pyfunction!(trie_range_verify, m)?)?;
    m.add_function(wrap_pyfunction!(trie_check, m)?)?;
    m.add_function(wrap_pyfunction!(trie_verify_proof, m)?)?;
    m.add_function(wrap_pyfunction!(poseidon_hash, m)?)?;
    m.add_function(wrap_pyfunction!(poseidon_compress, m)?)?;
    m.add_function(wrap_pyfunction!(fr_from_bytes, m)?)?;
    m.add_function(wrap_pyfunction!(fr_to_bytes, m)?)?;
    m.add_function(wrap_pyfunction!(poseidon_merkle_root, m)?)?;
    m.add_function(wrap_pyfunction!(poseidon_verify_inclusion, m)?)?;
    m.add_function(wrap_pyfunction!(poseidon_verify_absence, m)?)?;
    m.add_function(wrap_pyfunction!(poseidon_zero_hashes, m)?)?;
    m.add_function(wrap_pyfunction!(bn254_add, m)?)?;
    m.add_function(wrap_pyfunction!(bn254_mul, m)?)?;
    m.add_function(wrap_pyfunction!(bn254_pairing, m)?)?;
    m.add_function(wrap_pyfunction!(groth16_verify, m)?)?;
    m.add_function(wrap_pyfunction!(sp1_verify, m)?)?;
    m.add_function(wrap_pyfunction!(sp1_hash_public_values, m)?)?;
    m.add("PROOFS_MAX_PROOF_WEIGHT", dilf4s_proofs::MAX_PROOF_WEIGHT)?;
    m.add("POSEIDON_MAX_INPUTS", poseidon::MAX_INPUTS)?;
    m.add("POSEIDON_MAX_DEPTH", dilf4s_proofs::poseidon::merkle::MAX_DEPTH)?;
    // r is one more than the largest element, -1
    let largest = poseidon::fr_to_int(Fr::from(-1i64));
    m.add("POSEIDON_FR_MODULUS", py_int(m.py(), &largest)?.add(1)?)?;
    m.add("TRIE_MAX_FIXED_BYTES", trie::MAX_FIXED_BYTES)?;
    m.add("TRIE_MAX_VARIABLE_BYTES", trie::MAX_VARIABLE_BYTES)?;
    m.add("GROTH16_PROOF_BYTES", groth16::PROOF_BYTES)?;
    Ok(())
}

#[cfg(test)]
#[allow(clippy::panic, reason = "unit tests fail with diagnostics")]
mod tests {
    use super::digest_list;
    use pyo3::prelude::*;
    use pyo3::types::{PyBytes, PyList};

    #[test]
    fn digest_list_retains_exactly_the_prefix_through_the_first_bad_width() {
        Python::initialize();
        Python::attach(|py| {
            let valid = PyBytes::new(py, &[7; 32]);
            let invalid = PyBytes::new(py, &[9]);
            for tail in [0, 1, 65536] {
                let refs =
                    [valid.clone(), invalid.clone()].into_iter().chain(core::iter::repeat_n(valid.clone(), tail));
                let list = PyList::new(py, refs).unwrap_or_else(|e| panic!("list: {e}"));
                let kept = digest_list(list.as_any(), "digests", 32)
                    .unwrap_or_else(|_| panic!("byte arguments must pass the type boundary"));
                assert_eq!(kept, alloc::vec![alloc::vec![7; 32], alloc::vec![9]], "tail length {tail}");
            }
            let list = PyList::new(py, [valid.clone(), valid.clone(), valid]).unwrap_or_else(|e| panic!("list: {e}"));
            let kept =
                digest_list(list.as_any(), "digests", 32).unwrap_or_else(|_| panic!("valid digests must be retained"));
            assert_eq!(kept, alloc::vec![alloc::vec![7; 32]; 3]);
        });
    }
}
