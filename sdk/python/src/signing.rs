//! `dilf4s.signing` (dilf4s-signing): verification for every scheme, signing and key derivation, VRFs, key-evolving
//! signatures and sigma proofs.
//!
//! A scheme, suite or construction is named as `signing.md` and the vectors name it; a name outside the closed set is
//! an argument outside its static type (a `Literal`), so a `TypeMismatch`. Secret keys are frozen classes whose
//! `repr` never shows their bytes; their Rust values are zeroized when the last reference drops. `to_bytes` hands
//! Python a copy, which Python cannot zeroize.

use alloc::collections::{BTreeMap, BTreeSet};
use alloc::string::String;
use alloc::vec::Vec;

use dilf4s_core::hash::HashAlgorithm;
use dilf4s_signing::kes::{CardanoKey, CardanoSumKes, KesProduct, KesSum, ProductKey, SumKey};
use dilf4s_signing::keys::path::{ChildIndex, HARDENED_OFFSET};
use dilf4s_signing::keys::unicode15::assigned_text;
use dilf4s_signing::keys::{ExtendedKey, bip32, bip39, icarus, path, slip10};
use dilf4s_signing::sigma::{self, Proposition};
use dilf4s_signing::vrf::{self, SuiteId};
use dilf4s_signing::{
    CryptoError, KesError, KeyError, SchemeId, SigmaError, VrfError, bls, ecdsa, ecrecover, ed25519, ed25519_bip32,
    ristretto255, schnorr,
};
use pyo3::prelude::*;
use pyo3::pyclass::CompareOp;
use pyo3::types::{PyList, PyString, PyTuple};
use unicode_normalization::UnicodeNormalization as _;

use crate::boundary::{
    Answer, Fail, Reads, bytes, bytes_len, call, count, items, mismatch, name, py_bool, py_bytes, py_none, py_str,
    py_tuple, py_u64, tuple_of, u32_arg, u64_arg, usize_arg, value,
};
use crate::core_api::as_algorithm;
use crate::errors::Refusal;
use crate::value::PyValue;

fn crypto<'py>(py: Python<'py>) -> impl Fn(CryptoError) -> Fail<'py> {
    move |e| e.refuse(py)
}

fn key_error<'py>(py: Python<'py>) -> impl Fn(KeyError) -> Fail<'py> {
    move |e| e.refuse(py)
}

/// Exactly `N` bytes, else a `TypeMismatch` (the argument's static type is a fixed width).
fn exact<'py, const N: usize>(obj: &Bound<'py, PyAny>, what: &str) -> Result<[u8; N], Fail<'py>> {
    let py = obj.py();
    <[u8; N]>::try_from(bytes(obj)?.as_ref()).map_err(|_| mismatch(py, &alloc::format!("expected {what}: {N} bytes")))
}

// ------------------------------------------------------------------------------------------------ verification

const SIGNERS: [&str; 8] = [
    "Ed25519",
    "Ed25519-BIP32",
    "P-256-ECDSA",
    "secp256k1-ECDSA",
    "BIP-340",
    "secp256k1-tagged-Schnorr",
    "ristretto255-Schnorr",
    "BLS12-381-PoP",
];

fn scheme<'py>(obj: &Bound<'py, PyAny>) -> Result<SchemeId, Fail<'py>> {
    let py = obj.py();
    SchemeId::from_name(&name(obj)?).ok_or_else(|| mismatch(py, "expected a signature scheme's name (signing.SCHEMES)"))
}

/// Bytes-level verification: the key is decoded first, then the signature, then the message. Malformed input is a
/// `CryptoError`; a well-formed signature that does not verify is `False`.
#[pyfunction]
fn signing_verify(
    scheme_name: &Bound<'_, PyAny>,
    key: &Bound<'_, PyAny>,
    message: &Bound<'_, PyAny>,
    signature: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = key.py();
    call(py, || {
        let id = scheme(scheme_name)?;
        // one buffer passed as more than one argument is copied once
        let mut reads = Reads::default();
        let (k, m, s) = (reads.read(key)?, reads.read(message)?, reads.read(signature)?);
        let (k, m, s) = (reads.get(&k), reads.get(&m), reads.get(&s));
        let verdict =
            if id == SchemeId::BlsPop { py.detach(|| id.verify_bytes(k, m, s)) } else { id.verify_bytes(k, m, s) };
        Ok(py_bool(py, verdict.map_err(crypto(py))?))
    })
}

/// Decodes a scheme's verifying key, giving its canonical bytes: a wrong length is `InvalidEncoding`, bytes that are
/// no key `InvalidKey`.
#[pyfunction]
fn signing_decode_verifying_key(scheme_name: &Bound<'_, PyAny>, key: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = key.py();
    call(py, || {
        let id = scheme(scheme_name)?;
        let k = bytes(key)?;
        let e = crypto(py);
        let out: Vec<u8> = match id {
            SchemeId::Ed25519 | SchemeId::Ed25519Libsodium => {
                ed25519::VerifyingKey::from_bytes(&k).map_err(e)?.as_bytes().to_vec()
            }
            SchemeId::P256Ecdsa => ecdsa::p256::VerifyingKey::from_bytes(&k).map_err(e)?.as_bytes().to_vec(),
            SchemeId::Secp256k1Ecdsa => ecdsa::secp256k1::VerifyingKey::from_bytes(&k).map_err(e)?.as_bytes().to_vec(),
            SchemeId::Bip340 => schnorr::bip340::VerifyingKey::from_bytes(&k).map_err(e)?.as_bytes().to_vec(),
            SchemeId::TaggedSchnorr => schnorr::tagged::VerifyingKey::from_bytes(&k).map_err(e)?.as_bytes().to_vec(),
            SchemeId::Ristretto255Schnorr => ristretto255::VerifyingKey::from_bytes(&k).map_err(e)?.as_bytes().to_vec(),
            SchemeId::BlsPop => bls::VerifyingKey::from_bytes(&k).map_err(e)?.as_bytes().to_vec(),
        };
        Ok(py_bytes(py, &out))
    })
}

/// Each distinct object of a collection decoded once (by identity, while the collection holds it): the values, and each
/// reference's position among them. Many references to one key or signature then cost one decoding, and the argument
/// Rust takes is built from the positions only once every check before it has passed.
struct Decoded<T> {
    values: Vec<T>,
    at: BTreeMap<usize, usize>,
}

impl<T: Copy> Decoded<T> {
    const fn new() -> Self {
        Self { values: Vec::new(), at: BTreeMap::new() }
    }

    /// The position of `obj`'s value, decoding it the first time it is met.
    fn index<'py>(
        &mut self,
        obj: &Bound<'py, PyAny>,
        decode: impl FnOnce(&Bound<'py, PyAny>) -> Result<T, Fail<'py>>,
    ) -> Result<usize, Fail<'py>> {
        let key = obj.as_ptr().addr();
        if let Some(&i) = self.at.get(&key) {
            return Ok(i);
        }
        self.values.push(decode(obj)?);
        let i = self.values.len().saturating_sub(1);
        self.at.insert(key, i);
        Ok(i)
    }

    /// The values at `positions`, in their order.
    fn all(&self, positions: &[usize]) -> Vec<T> {
        positions.iter().filter_map(|&i| self.values.get(i).copied()).collect()
    }
}

fn bls_key<'py>(obj: &Bound<'py, PyAny>) -> Result<bls::VerifyingKey, Fail<'py>> {
    bls::VerifyingKey::from_bytes(&bytes(obj)?).map_err(crypto(obj.py()))
}

/// The positions of a list of keys, each distinct key decoded once.
fn bls_keys<'py>(keys: &mut Decoded<bls::VerifyingKey>, obj: &Bound<'py, PyAny>) -> Result<Vec<usize>, Fail<'py>> {
    items(obj, "the verifying keys")?.iter().map(|k| keys.index(k, bls_key)).collect()
}

fn bls_signature<'py>(obj: &Bound<'py, PyAny>) -> Result<bls::Signature, Fail<'py>> {
    bls::Signature::from_bytes(&bytes(obj)?).map_err(crypto(obj.py()))
}

/// The BLS aggregate of signatures (a list or tuple of bytes).
#[pyfunction]
fn bls_aggregate(signatures: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = signatures.py();
    call(py, || {
        let mut decoded = Decoded::new();
        let at = items(signatures, "the signatures")?
            .iter()
            .map(|s| decoded.index(s, bls_signature))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(py_bytes(py, bls::aggregate(&decoded.all(&at)).map_err(crypto(py))?.as_bytes()))
    })
}

/// `FastAggregateVerify`: every key signed the one message.
#[pyfunction]
fn bls_fast_aggregate_verify(
    keys: &Bound<'_, PyAny>,
    message: &Bound<'_, PyAny>,
    signature: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = keys.py();
    call(py, || {
        let mut decoded = Decoded::new();
        let at = bls_keys(&mut decoded, keys)?;
        let m = bytes(message)?;
        let s = bls_signature(signature)?;
        // the keys' vector is built only now that the signature decodes
        let ks = decoded.all(&at);
        Ok(py_bool(py, py.detach(|| bls::fast_aggregate_verify(&ks, &m, &s))))
    })
}

/// `AggregateVerify`: each `(key, message)` pair signed, the signatures aggregated.
#[pyfunction]
fn bls_aggregate_verify(pairs: &Bound<'_, PyAny>, signature: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = pairs.py();
    call(py, || {
        // the keys decoded and the messages checked to be bytes, in order; the signature; and only then the messages
        // read, once each (a shared buffer once in all), as nothing after this can refuse
        // the keys decoded (each distinct one once) and the messages checked to be bytes, in order; the signature; and
        // only then the arguments built, the messages read once each (a shared buffer once in all)
        let read = items(pairs, "the (key, message) pairs")?;
        let mut keys = Decoded::new();
        let mut checked: Vec<(usize, Bound<'_, PyAny>)> = Vec::with_capacity(read.len());
        for p in &read {
            let [k, m] = tuple_of::<2>(p, "a (key, message) pair", "expected (key, message) pairs")?;
            let at = keys.index(&k, bls_key)?;
            bytes_len(&m)?;
            checked.push((at, m));
        }
        let s = bls_signature(signature)?;
        let mut reads = Reads::default();
        let held = checked.iter().map(|(at, m)| Ok((*at, reads.read(m)?))).collect::<Result<Vec<_>, Fail<'_>>>()?;
        let borrowed: Vec<(bls::VerifyingKey, &[u8])> =
            held.iter().filter_map(|(at, m)| keys.values.get(*at).map(|k| (*k, reads.get(m)))).collect();
        Ok(py_bool(py, py.detach(|| bls::aggregate_verify(&borrowed, &s))))
    })
}

/// Batch verification of `(key, message, signature)` triples: true only when every one verifies.
#[pyfunction]
fn bls_batch_verify(triples: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = triples.py();
    call(py, || {
        let read = items(triples, "the (key, message, signature) triples")?;
        let (mut keys, mut sigs) = (Decoded::new(), Decoded::new());
        let mut checked: Vec<(usize, Bound<'_, PyAny>, usize)> = Vec::with_capacity(read.len());
        for t in &read {
            let [k, m, s] =
                tuple_of::<3>(t, "a (key, message, signature) triple", "expected (key, message, signature) triples")?;
            let key = keys.index(&k, bls_key)?;
            bytes_len(&m)?;
            checked.push((key, m, sigs.index(&s, bls_signature)?));
        }
        // every check made, the messages are read once each (a shared buffer once in all), each distinct key and
        // signature having been decoded once
        let mut reads = Reads::default();
        let held =
            checked.iter().map(|(k, m, s)| Ok((*k, reads.read(m)?, *s))).collect::<Result<Vec<_>, Fail<'_>>>()?;
        let borrowed: Vec<(bls::VerifyingKey, &[u8], bls::Signature)> = held
            .iter()
            .filter_map(|(k, m, s)| Some((*keys.values.get(*k)?, reads.get(m), *sigs.values.get(*s)?)))
            .collect();
        Ok(py_bool(py, py.detach(|| bls::batch_verify(&borrowed))))
    })
}

/// `PopVerify`: the key's proof of possession.
#[pyfunction]
fn bls_pop_verify(key: &Bound<'_, PyAny>, proof: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = key.py();
    call(py, || {
        let k = bls::VerifyingKey::from_bytes(&bytes(key)?).map_err(crypto(py))?;
        let p = bls_signature(proof)?;
        Ok(py_bool(py, py.detach(|| bls::pop_verify(&k, &p))))
    })
}

/// Ethereum's `ecrecover`: the 20-byte address of the key that signed a 32-byte digest (`r || s || v`, 65 bytes), or
/// `None` when no key did.
#[pyfunction]
fn ecrecover_address(digest: &Bound<'_, PyAny>, signature: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = digest.py();
    call(py, || {
        let (d, s) = (bytes(digest)?, bytes(signature)?);
        Ok(ecrecover::recover(&d, &s).map_err(crypto(py))?.map_or_else(|| py_none(py), |a| py_bytes(py, &a)))
    })
}

/// The Ethereum address of a secp256k1 key (33 bytes compressed or 65 uncompressed).
#[pyfunction]
fn eth_address_of_key(key: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = key.py();
    call(py, || Ok(py_bytes(py, &ecrecover::address_of_key(&bytes(key)?).map_err(crypto(py))?)))
}

// ------------------------------------------------------------------------------------------------ signing keys

enum Secret {
    Ed25519(ed25519::SigningKey),
    Ed25519Bip32(ed25519_bip32::SigningKey),
    P256(ecdsa::p256::SigningKey),
    Secp256k1(ecdsa::secp256k1::SigningKey),
    Bip340(schnorr::bip340::SigningKey),
    Tagged(schnorr::tagged::SigningKey),
    Ristretto(ristretto255::SigningKey),
    Bls(bls::SigningKey),
}

/// A signing key of one scheme. Its bytes never appear in its `repr`.
#[pyclass(frozen, module = "dilf4s._native", name = "SigningKey")]
pub(crate) struct PySigningKey(Secret);

impl Secret {
    const fn scheme(&self) -> &'static str {
        match self {
            Self::Ed25519(_) => "Ed25519",
            Self::Ed25519Bip32(_) => "Ed25519-BIP32",
            Self::P256(_) => "P-256-ECDSA",
            Self::Secp256k1(_) => "secp256k1-ECDSA",
            Self::Bip340(_) => "BIP-340",
            Self::Tagged(_) => "secp256k1-tagged-Schnorr",
            Self::Ristretto(_) => "ristretto255-Schnorr",
            Self::Bls(_) => "BLS12-381-PoP",
        }
    }

    fn verifying_key(&self) -> Result<Vec<u8>, CryptoError> {
        Ok(match self {
            Self::Ed25519(k) => k.verifying_key().as_bytes().to_vec(),
            Self::Ed25519Bip32(k) => k.verifying_key().as_bytes().to_vec(),
            Self::P256(k) => k.verifying_key().as_bytes().to_vec(),
            Self::Secp256k1(k) => k.verifying_key().as_bytes().to_vec(),
            Self::Bip340(k) => k.verifying_key()?.as_bytes().to_vec(),
            Self::Tagged(k) => k.verifying_key()?.as_bytes().to_vec(),
            Self::Ristretto(k) => k.verifying_key().as_bytes().to_vec(),
            Self::Bls(k) => k.verifying_key().as_bytes().to_vec(),
        })
    }

    fn to_bytes(&self) -> Vec<u8> {
        match self {
            Self::Ed25519(k) => k.as_bytes().to_vec(),
            Self::Ed25519Bip32(k) => k.to_bytes().to_vec(),
            Self::P256(k) => k.to_bytes().to_vec(),
            Self::Secp256k1(k) => k.to_bytes().to_vec(),
            Self::Bip340(k) => k.as_bytes().to_vec(),
            Self::Tagged(k) => k.as_bytes().to_vec(),
            Self::Ristretto(k) => k.to_bytes().to_vec(),
            Self::Bls(k) => k.as_bytes().to_vec(),
        }
    }

    const fn randomised(&self) -> bool {
        matches!(self, Self::Bip340(_) | Self::Tagged(_) | Self::Ristretto(_))
    }
}

fn secret_of<'py>(scheme_name: &Bound<'py, PyAny>, key: &[u8], from_seed: bool) -> Result<Secret, Fail<'py>> {
    let py = scheme_name.py();
    let n = name(scheme_name)?;
    let e = crypto(py);
    Ok(match (n.as_ref(), from_seed) {
        ("Ed25519", true) => Secret::Ed25519(ed25519::from_seed(key).map_err(e)?.0),
        ("Ed25519", false) => Secret::Ed25519(ed25519::SigningKey::from_bytes(key).map_err(e)?),
        ("Ed25519-BIP32", _) => Secret::Ed25519Bip32(ed25519_bip32::SigningKey::from_bytes(key).map_err(e)?),
        ("P-256-ECDSA", true) => Secret::P256(ecdsa::p256::from_seed(key).map_err(e)?.0),
        ("P-256-ECDSA", false) => Secret::P256(ecdsa::p256::SigningKey::from_bytes(key).map_err(e)?),
        ("secp256k1-ECDSA", true) => Secret::Secp256k1(ecdsa::secp256k1::from_seed(key).map_err(e)?.0),
        ("secp256k1-ECDSA", false) => Secret::Secp256k1(ecdsa::secp256k1::SigningKey::from_bytes(key).map_err(e)?),
        ("BIP-340", true) => Secret::Bip340(schnorr::bip340::from_seed(key).map_err(e)?.0),
        ("BIP-340", false) => Secret::Bip340(schnorr::bip340::SigningKey::from_bytes(key).map_err(e)?),
        ("secp256k1-tagged-Schnorr", true) => Secret::Tagged(schnorr::tagged::from_seed(key).map_err(e)?.0),
        ("secp256k1-tagged-Schnorr", false) => Secret::Tagged(schnorr::tagged::SigningKey::from_bytes(key).map_err(e)?),
        ("ristretto255-Schnorr", true) => Secret::Ristretto(ristretto255::from_seed(key).map_err(e)?.0),
        ("ristretto255-Schnorr", false) => Secret::Ristretto(ristretto255::SigningKey::from_bytes(key).map_err(e)?),
        ("BLS12-381-PoP", true) => Secret::Bls(bls::from_seed(key).map_err(e)?.0),
        ("BLS12-381-PoP", false) => Secret::Bls(bls::SigningKey::from_bytes(key).map_err(e)?),
        _ => {
            return Err(mismatch(py, "expected a signing scheme's name (signing.SIGNERS)"));
        }
    })
}

fn py_signing_key(py: Python<'_>, s: Secret) -> PyResult<Bound<'_, PyAny>> {
    Bound::new(py, PySigningKey(s)).map(Bound::into_any)
}

/// A BIP-340 message: exactly 32 bytes, refused as the scheme's message decoding refuses it.
fn bip340_message(m: &[u8]) -> Result<[u8; 32], CryptoError> {
    <[u8; 32]>::try_from(m).map_err(|_| CryptoError::InvalidEncoding {
        what: "BIP-340 message".into(),
        reason: alloc::format!("expected 32 bytes, got {}", m.len()),
    })
}

#[pymethods]
impl PySigningKey {
    /// The key a scheme derives from a seed (`signing.md` section 1.7): Ed25519's and the Schnorr schemes' 32-byte
    /// seed, ECDSA's scalar, BLS's `KeyGen`.
    #[staticmethod]
    fn from_seed(scheme_name: &Bound<'_, PyAny>, seed: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = seed.py();
        call(py, || {
            let s = bytes(seed)?;
            Ok(py_signing_key(py, secret_of(scheme_name, &s, true)?)?)
        })
    }

    /// The key of its encoding: a wrong length is `InvalidEncoding`, an out-of-range scalar (or an unclamped Ed25519-BIP32
    /// `kL`) `InvalidKey`.
    #[staticmethod]
    fn from_bytes(scheme_name: &Bound<'_, PyAny>, key: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = key.py();
        call(py, || {
            let k = bytes(key)?;
            Ok(py_signing_key(py, secret_of(scheme_name, &k, false)?)?)
        })
    }

    /// BLS `derive_master_SK` (EIP-2333) of a seed of at least 32 bytes.
    #[staticmethod]
    fn bls_derive_master(seed: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = seed.py();
        call(py, || Ok(py_signing_key(py, Secret::Bls(bls::derive_master_sk(&bytes(seed)?).map_err(crypto(py))?))?))
    }

    /// The scheme.
    #[getter]
    fn scheme(&self) -> &'static str {
        self.0.scheme()
    }

    /// The verifying key's canonical bytes.
    fn verifying_key(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        call(py, || Ok(py_bytes(py, &self.0.verifying_key().map_err(crypto(py))?)))
    }

    /// The secret key's bytes: a copy Python holds, which it cannot zeroize.
    fn to_bytes<'py>(&self, py: Python<'py>) -> Bound<'py, PyAny> {
        py_bytes(py, &self.0.to_bytes())
    }

    /// A signature on `message`. BIP-340, tagged Schnorr and ristretto255 take 32 bytes of `randomness` (the
    /// auxiliary randomness or nonce seed); the others are deterministic and take none. A BIP-340 message is 32 bytes.
    #[pyo3(signature = (message, randomness = None))]
    fn sign(&self, message: &Bound<'_, PyAny>, randomness: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
        let py = message.py();
        call(py, || {
            let m = bytes(message)?;
            let aux: Option<[u8; 32]> = match (self.0.randomised(), randomness.filter(|r| !r.is_none())) {
                (true, Some(r)) => Some(exact::<32>(r, "the randomness")?),
                (true, None) => {
                    return Err(mismatch(py, "expected 32 bytes of randomness for this scheme"));
                }
                (false, Some(_)) => {
                    return Err(mismatch(py, "expected no randomness: this scheme is deterministic"));
                }
                (false, None) => None,
            };
            let a = aux.unwrap_or_default();
            let e = crypto(py);
            let sig: Vec<u8> = match &self.0 {
                Secret::Ed25519(k) => k.sign(&m).as_bytes().to_vec(),
                Secret::Ed25519Bip32(k) => k.sign(&m).as_bytes().to_vec(),
                Secret::P256(k) => k.sign(&m).map_err(e)?.as_bytes().to_vec(),
                Secret::Secp256k1(k) => k.sign(&m).map_err(e)?.as_bytes().to_vec(),
                Secret::Bip340(k) => k.sign(&bip340_message(&m).map_err(&e)?, &a).map_err(e)?.as_bytes().to_vec(),
                Secret::Tagged(k) => k.sign(&m, &a).map_err(e)?.as_bytes().to_vec(),
                Secret::Ristretto(k) => k.sign(&m, &a).as_bytes().to_vec(),
                Secret::Bls(k) => py.detach(|| k.sign(&m)).as_bytes().to_vec(),
            };
            Ok(py_bytes(py, &sig))
        })
    }

    /// A BLS key's proof of possession; another scheme's key has none (`TypeMismatch`).
    fn pop_prove(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        call(py, || match &self.0 {
            Secret::Bls(k) => Ok(py_bytes(py, py.detach(|| k.pop_prove()).as_bytes())),
            _ => Err(mismatch(py, "expected a BLS12-381-PoP key")),
        })
    }

    /// A BLS key's EIP-2333 child; another scheme's key has none (`TypeMismatch`).
    fn bls_derive_child(&self, index: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = index.py();
        call(py, || {
            let i = u32_arg(index, "the index")?;
            match &self.0 {
                Secret::Bls(k) => Ok(py_signing_key(py, Secret::Bls(bls::derive_child_sk(k, i).map_err(crypto(py))?))?),
                _ => Err(mismatch(py, "expected a BLS12-381-PoP key")),
            }
        })
    }

    /// Constant-time equality of two keys of the same scheme.
    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> Py<PyAny> {
        let py = other.py();
        let same = |o: &Self| match (&self.0, &o.0) {
            (Secret::Ed25519(a), Secret::Ed25519(b)) => a == b,
            (Secret::Ed25519Bip32(a), Secret::Ed25519Bip32(b)) => a == b,
            (Secret::P256(a), Secret::P256(b)) => a == b,
            (Secret::Secp256k1(a), Secret::Secp256k1(b)) => a == b,
            (Secret::Bip340(a), Secret::Bip340(b)) => a == b,
            (Secret::Tagged(a), Secret::Tagged(b)) => a == b,
            (Secret::Ristretto(a), Secret::Ristretto(b)) => a == b,
            (Secret::Bls(a), Secret::Bls(b)) => a == b,
            _ => false,
        };
        match (other.cast::<Self>(), op) {
            (Ok(o), CompareOp::Eq) => py_bool(py, same(o.get())).unbind(),
            (Ok(o), CompareOp::Ne) => py_bool(py, !same(o.get())).unbind(),
            _ => py.NotImplemented(),
        }
    }

    fn __repr__(&self) -> String {
        alloc::format!("SigningKey({}, <redacted>)", self.0.scheme())
    }
}

// ------------------------------------------------------------------------------------------------ key derivation

/// A child index as BIP-32 serializes it: hardened from 2**31.
fn child_index<'py>(obj: &Bound<'py, PyAny>) -> Result<ChildIndex, Fail<'py>> {
    let py = obj.py();
    let i = u32_arg(obj, "a child index")?;
    let made = match i.checked_sub(HARDENED_OFFSET) {
        Some(h) => ChildIndex::hardened(h),
        None => ChildIndex::soft(i),
    };
    made.map_err(key_error(py))
}

fn parsed_path<'py>(obj: &Bound<'py, PyAny>) -> Result<Vec<ChildIndex>, Fail<'py>> {
    let py = obj.py();
    path::parse(&name(obj)?).map_err(|e| KeyError::from(e).refuse(py))
}

/// A derivation path (`m/44'/0'/0`, strictly: no spaces, ASCII digits, no leading zeros, at most 10 digits a segment
/// and 255 segments) as its serialized indices, hardened from 2**31.
#[pyfunction]
fn parse_path(text: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = text.py();
    call(py, || {
        let indices = parsed_path(text)?
            .into_iter()
            .map(|i| py_u64(py, u64::from(i.serialized())))
            .collect::<PyResult<Vec<_>>>()?;
        Ok(py_tuple(py, indices)?)
    })
}

/// The words a BIP-39 list holds (`WordList::new` refuses any other count before it reads a word).
const WORDS: usize = 2048;

/// A BIP-39 word list: 2048 distinct words and their separator.
#[pyclass(frozen, module = "dilf4s._native", name = "WordList")]
pub(crate) struct PyWordList(bip39::WordList);

#[pymethods]
impl PyWordList {
    /// The English list.
    #[staticmethod]
    fn english() -> Self {
        Self(bip39::WordList::english())
    }

    /// A list of 2048 distinct words (a list or tuple of str) and one separator character. Rust checks the separator
    /// and then the count before it reads any word, so a list of another count is refused by its count alone, its
    /// words neither read nor copied.
    #[staticmethod]
    #[pyo3(signature = (words, separator = None))]
    fn of(words: &Bound<'_, PyAny>, separator: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
        let py = words.py();
        call(py, || {
            let sep = match separator.filter(|s| !s.is_none()) {
                None => ' ',
                Some(s) => {
                    let t = crate::boundary::text(s)?.unwrap_or_default();
                    let mut chars = t.chars();
                    match (chars.next(), chars.next()) {
                        (Some(c), None) => c,
                        _ => return Err(mismatch(py, "expected one separator character")),
                    }
                }
            };
            let n = count(words, "the words")?;
            // the separator as Rust refuses it (given no words), at every count, before any word is read or copied;
            // then the count, in Rust's words and order
            match bip39::WordList::new(Vec::new(), sep) {
                Err(KeyError::InvalidWordList { separator: None, .. }) | Ok(_) => {}
                Err(e) => return Err(e.refuse(py)),
            }
            if n != WORDS {
                return Err(KeyError::InvalidWordList {
                    reason: alloc::format!("expected {WORDS} distinct words without separators, got {n}"),
                    separator: None,
                }
                .refuse(py));
            }
            let held = items(words, "the words")?;
            // Rust normalises every word (NFKD) and refuses fewer than 2048 distinct ones, naming how many it found.
            // That verdict is made here first, on borrowed text, normalising each distinct object once: a list of many
            // references to one large word is refused at the cost of that word, never of 2048 copies of it
            let mut normal: Vec<String> = Vec::new();
            let mut met: BTreeSet<usize> = BTreeSet::new();
            for w in &held {
                if met.insert(w.as_ptr().addr()) {
                    // a word holding a code point Unicode 15.0 does not assign, a lone surrogate included, is refused
                    // before it is normalised, as Rust refuses it (signing.md section 2.1)
                    let unassigned = || {
                        KeyError::InvalidWordList {
                            reason: "a word holds a code point Unicode 15.0 does not assign".into(),
                            separator: None,
                        }
                        .refuse(py)
                    };
                    let t = crate::boundary::text(w)?.ok_or_else(unassigned)?;
                    if !assigned_text(t) {
                        return Err(unassigned());
                    }
                    normal.push(t.nfkd().collect());
                }
            }
            // the distinct words among the distinct objects: every reference's word is one of them
            let distinct = normal.iter().collect::<BTreeSet<_>>().len();
            if distinct != WORDS {
                return Err(KeyError::InvalidWordList {
                    reason: alloc::format!(
                        "expected {WORDS} distinct words without separators, got {WORDS} ({distinct} distinct)"
                    ),
                    separator: None,
                }
                .refuse(py));
            }
            drop(normal);
            let ws = held
                .iter()
                .map(|w| {
                    crate::boundary::text(w)?
                        .map(String::from)
                        .ok_or_else(|| mismatch(py, "expected words without surrogates"))
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Bound::new(py, Self(bip39::WordList::new(ws, sep).map_err(key_error(py))?))?.into_any())
        })
    }

    /// The separator.
    #[getter]
    fn separator(&self) -> String {
        String::from(self.0.separator())
    }
}

fn word_list<'py>(obj: Option<&Bound<'py, PyAny>>) -> Result<Option<bip39::WordList>, Fail<'py>> {
    match obj.filter(|o| !o.is_none()) {
        None => Ok(None),
        Some(o) => o
            .cast::<PyWordList>()
            .map(|l| Some(l.get().0.clone()))
            .map_err(|_| mismatch(o.py(), "expected a dilf4s.signing.WordList")),
    }
}

/// The BIP-39 mnemonic of 16, 20, 24, 28 or 32 bytes of entropy.
#[pyfunction]
#[pyo3(signature = (entropy, words = None))]
fn mnemonic_from_entropy(entropy: &Bound<'_, PyAny>, words: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
    let py = entropy.py();
    call(py, || {
        let e = bytes(entropy)?;
        let list = word_list(words)?.unwrap_or_else(bip39::WordList::english);
        Ok(py_str(py, &bip39::to_mnemonic(&e, &list).map_err(key_error(py))?.phrase()))
    })
}

/// A mnemonic's text, exactly as given: one holding a surrogate code point is no phrase of any word list (whose words
/// are Rust text), refused as `InvalidMnemonic`, never repaired into another phrase.
fn phrase<'py>(obj: &Bound<'py, PyAny>) -> Result<String, Fail<'py>> {
    let py = obj.py();
    let refused = || KeyError::InvalidMnemonic { reason: "a mnemonic with an unpaired surrogate".into() }.refuse(py);
    crate::boundary::text(obj)?.map(String::from).ok_or_else(refused)
}

/// The entropy a BIP-39 mnemonic encodes, its checksum checked.
#[pyfunction]
#[pyo3(signature = (mnemonic, words = None))]
fn mnemonic_to_entropy(mnemonic: &Bound<'_, PyAny>, words: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
    let py = mnemonic.py();
    call(py, || {
        let p = phrase(mnemonic)?;
        let list = word_list(words)?.unwrap_or_else(bip39::WordList::english);
        let (_, entropy) = bip39::to_entropy(&p, &list).map_err(key_error(py))?;
        Ok(py_bytes(py, entropy.expose()))
    })
}

/// The BIP-39 seed of a mnemonic (checked first) and a passphrase (NFKD-normalised; one holding a code point Unicode
/// 15.0 does not assign, a lone surrogate included, is `InvalidPassphrase`).
#[pyfunction]
#[pyo3(signature = (mnemonic, passphrase = None, words = None))]
fn mnemonic_to_seed(
    mnemonic: &Bound<'_, PyAny>,
    passphrase: Option<&Bound<'_, PyAny>>,
    words: Option<&Bound<'_, PyAny>>,
) -> PyResult<Py<PyAny>> {
    let py = mnemonic.py();
    call(py, || {
        let p = phrase(mnemonic)?;
        // the passphrase's type is checked with the other arguments', its text once the phrase is accepted, as in
        // every SDK: a lone surrogate is a code point Unicode 15.0 does not assign, so InvalidPassphrase
        let pass = passphrase.filter(|o| !o.is_none()).map(crate::boundary::text).transpose()?;
        let list = word_list(words)?.unwrap_or_else(bip39::WordList::english);
        let (m, _) = bip39::to_entropy(&p, &list).map_err(key_error(py))?;
        let pass = match pass {
            None => String::new(),
            Some(Some(t)) => String::from(t),
            Some(None) => {
                return Err(KeyError::InvalidPassphrase {
                    reason: "the passphrase holds a code point Unicode 15.0 does not assign".into(),
                }
                .refuse(py));
            }
        };
        let seed = py.detach(|| bip39::seed(&m, &pass)).map_err(key_error(py))?;
        Ok(py_bytes(py, seed.expose()))
    })
}

#[derive(Clone)]
enum Extended {
    Slip10(ExtendedKey<ed25519::SigningKey>),
    Bip32(ExtendedKey<ecdsa::secp256k1::SigningKey>),
    Icarus(ExtendedKey<ed25519_bip32::SigningKey>),
}

/// An extended private key: a signing key and its 32-byte chain code, of SLIP-10 (Ed25519), BIP-32 (secp256k1) or
/// BIP32-Ed25519 (Icarus).
#[pyclass(frozen, module = "dilf4s._native", name = "ExtendedKey")]
pub(crate) struct PyExtendedKey(Extended);

fn py_extended(py: Python<'_>, e: Extended) -> PyResult<Bound<'_, PyAny>> {
    Bound::new(py, PyExtendedKey(e)).map(Bound::into_any)
}

impl Extended {
    fn child(&self, i: ChildIndex) -> Result<Self, KeyError> {
        Ok(match self {
            Self::Slip10(k) => Self::Slip10(slip10::child(k, i)?),
            Self::Bip32(k) => Self::Bip32(bip32::child(k, i)?),
            Self::Icarus(k) => Self::Icarus(icarus::child(k, i)?),
        })
    }
}

#[pymethods]
impl PyExtendedKey {
    /// The SLIP-10 Ed25519 master key of a 16..64-byte seed.
    #[staticmethod]
    fn slip10_master(seed: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = seed.py();
        call(py, || Ok(py_extended(py, Extended::Slip10(slip10::master(&bytes(seed)?).map_err(key_error(py))?))?))
    }

    /// The BIP-32 secp256k1 master key of a 16..64-byte seed.
    #[staticmethod]
    fn bip32_master(seed: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = seed.py();
        call(py, || Ok(py_extended(py, Extended::Bip32(bip32::master(&bytes(seed)?).map_err(key_error(py))?))?))
    }

    /// The Icarus (BIP32-Ed25519) root key: PBKDF2-HMAC-SHA512 of the passphrase's UTF-8 and the entropy, clamped.
    #[staticmethod]
    #[pyo3(signature = (entropy, passphrase = None))]
    fn icarus_from_entropy(entropy: &Bound<'_, PyAny>, passphrase: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
        let py = entropy.py();
        call(py, || {
            let e = bytes(entropy)?;
            let pass = match passphrase.filter(|o| !o.is_none()) {
                None => String::new(),
                Some(o) => String::from(
                    crate::boundary::text(o)?
                        .ok_or_else(|| mismatch(py, "expected a passphrase without surrogates"))?,
                ),
            };
            let root = py.detach(|| icarus::from_entropy(&e, &pass)).map_err(key_error(py))?;
            Ok(py_extended(py, Extended::Icarus(root))?)
        })
    }

    /// The Icarus root key of 96 bytes `kL || kR || cc`, clamped.
    #[staticmethod]
    fn icarus_root(secret: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = secret.py();
        call(py, || Ok(py_extended(py, Extended::Icarus(icarus::root(&bytes(secret)?).map_err(key_error(py))?))?))
    }

    /// The child at a serialized index (hardened from 2**31). SLIP-10 derives hardened children only.
    fn child(&self, index: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = index.py();
        call(py, || Ok(py_extended(py, self.0.child(child_index(index)?).map_err(key_error(py))?)?))
    }

    /// The key at a path (`m/...`) from this one.
    fn derive(&self, path: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = path.py();
        call(py, || {
            let p = parsed_path(path)?;
            let derived = p.iter().try_fold(self.0.clone(), |k, i| k.child(*i)).map_err(key_error(py))?;
            Ok(py_extended(py, derived)?)
        })
    }

    /// The signing key.
    #[getter]
    fn key<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        py_signing_key(
            py,
            match &self.0 {
                Extended::Slip10(k) => Secret::Ed25519(k.key.clone()),
                Extended::Bip32(k) => Secret::Secp256k1(k.key.clone()),
                Extended::Icarus(k) => Secret::Ed25519Bip32(k.key.clone()),
            },
        )
    }

    /// The chain code.
    #[getter]
    fn chain_code<'py>(&self, py: Python<'py>) -> Bound<'py, PyAny> {
        let cc = match &self.0 {
            Extended::Slip10(k) => k.chain_code,
            Extended::Bip32(k) => k.chain_code,
            Extended::Icarus(k) => k.chain_code,
        };
        py_bytes(py, &cc)
    }

    fn __repr__(&self) -> &'static str {
        match self.0 {
            Extended::Slip10(_) => "ExtendedKey(SLIP-10 Ed25519, <redacted>)",
            Extended::Bip32(_) => "ExtendedKey(BIP-32 secp256k1, <redacted>)",
            Extended::Icarus(_) => "ExtendedKey(BIP32-Ed25519, <redacted>)",
        }
    }
}

/// BIP-32 `CKDpub`: the child verifying key and chain code at a soft index, from a parent's (33-byte compressed key).
#[pyfunction]
fn bip32_public_child(
    key: &Bound<'_, PyAny>,
    chain_code: &Bound<'_, PyAny>,
    index: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = key.py();
    call(py, || {
        let vk = ecdsa::secp256k1::VerifyingKey::from_bytes(&bytes(key)?).map_err(crypto(py))?;
        let cc = exact::<32>(chain_code, "the chain code")?;
        let (child, code) = bip32::public_child(&vk, &cc, child_index(index)?).map_err(key_error(py))?;
        Ok(py_tuple(py, vec![py_bytes(py, child.as_bytes()), py_bytes(py, &code)])?)
    })
}

/// Icarus public (soft-only) derivation: the child verifying key and chain code from a parent's.
#[pyfunction]
fn icarus_public_child(
    key: &Bound<'_, PyAny>,
    chain_code: &Bound<'_, PyAny>,
    index: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = key.py();
    call(py, || {
        let vk = ed25519::VerifyingKey::from_bytes(&bytes(key)?).map_err(crypto(py))?;
        let cc = exact::<32>(chain_code, "the chain code")?;
        let (child, code) = icarus::public_child(&vk, &cc, child_index(index)?).map_err(key_error(py))?;
        Ok(py_tuple(py, vec![py_bytes(py, child.as_bytes()), py_bytes(py, &code)])?)
    })
}

// ------------------------------------------------------------------------------------------------ VRF

fn suite<'py>(obj: &Bound<'py, PyAny>) -> Result<SuiteId, Fail<'py>> {
    let py = obj.py();
    SuiteId::from_name(&name(obj)?).ok_or_else(|| mismatch(py, "expected a VRF suite's name (signing.VRF_SUITES)"))
}

/// VRF verification (RFC 9381 TAI and ELL2, and draft-03): the 64-byte output `beta` of a valid proof, else the typed
/// rejection.
#[pyfunction]
fn vrf_verify(
    suite_name: &Bound<'_, PyAny>,
    key: &Bound<'_, PyAny>,
    alpha: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = key.py();
    call(py, || {
        let id = suite(suite_name)?;
        let mut reads = Reads::default();
        let (k, a, p) = (reads.read(key)?, reads.read(alpha)?, reads.read(proof)?);
        let (k, a, p) = (reads.get(&k), reads.get(&a), reads.get(&p));
        Ok(py_bytes(py, &id.verify_bytes(k, a, p).map_err(|e: VrfError| e.refuse(py))?))
    })
}

enum VrfSecret {
    Tai(vrf::SigningKey<vrf::Tai>),
    Ell2(vrf::SigningKey<vrf::Ell2>),
    Draft03(vrf::SigningKey<vrf::Draft03>),
}

/// A VRF secret key of one suite. Its bytes never appear in its `repr`.
#[pyclass(frozen, module = "dilf4s._native", name = "VrfSigningKey")]
pub(crate) struct PyVrfSigningKey(VrfSecret);

fn vrf_secret<'py>(suite_name: &Bound<'py, PyAny>, key: &[u8], from_seed: bool) -> Result<VrfSecret, Fail<'py>> {
    let py = suite_name.py();
    let e = |e: VrfError| e.refuse(py);
    Ok(match (suite(suite_name)?, from_seed) {
        (SuiteId::Tai, true) => VrfSecret::Tai(vrf::from_seed::<vrf::Tai>(key).map_err(e)?.0),
        (SuiteId::Tai, false) => VrfSecret::Tai(vrf::SigningKey::from_bytes(key).map_err(e)?),
        (SuiteId::Ell2, true) => VrfSecret::Ell2(vrf::from_seed::<vrf::Ell2>(key).map_err(e)?.0),
        (SuiteId::Ell2, false) => VrfSecret::Ell2(vrf::SigningKey::from_bytes(key).map_err(e)?),
        (SuiteId::Draft03, true) => VrfSecret::Draft03(vrf::from_seed::<vrf::Draft03>(key).map_err(e)?.0),
        (SuiteId::Draft03, false) => VrfSecret::Draft03(vrf::SigningKey::from_bytes(key).map_err(e)?),
    })
}

#[pymethods]
impl PyVrfSigningKey {
    /// The key of a 32-byte seed.
    #[staticmethod]
    fn from_seed(suite_name: &Bound<'_, PyAny>, seed: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = seed.py();
        call(py, || {
            let s = bytes(seed)?;
            Ok(Bound::new(py, Self(vrf_secret(suite_name, &s, true)?))?.into_any())
        })
    }

    /// The suite's secret key encoding: 32 bytes for RFC 9381, `seed || pk` (64) for draft-03.
    #[staticmethod]
    fn from_bytes(suite_name: &Bound<'_, PyAny>, key: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = key.py();
        call(py, || {
            let k = bytes(key)?;
            Ok(Bound::new(py, Self(vrf_secret(suite_name, &k, false)?))?.into_any())
        })
    }

    /// The suite.
    #[getter]
    fn suite(&self) -> &'static str {
        match &self.0 {
            VrfSecret::Tai(_) => SuiteId::Tai.name(),
            VrfSecret::Ell2(_) => SuiteId::Ell2.name(),
            VrfSecret::Draft03(_) => SuiteId::Draft03.name(),
        }
    }

    /// The verifying key.
    fn verifying_key<'py>(&self, py: Python<'py>) -> Bound<'py, PyAny> {
        match &self.0 {
            VrfSecret::Tai(k) => py_bytes(py, k.verifying_key().as_bytes()),
            VrfSecret::Ell2(k) => py_bytes(py, k.verifying_key().as_bytes()),
            VrfSecret::Draft03(k) => py_bytes(py, k.verifying_key().as_bytes()),
        }
    }

    /// The secret key's bytes: a copy Python holds, which it cannot zeroize.
    fn to_bytes<'py>(&self, py: Python<'py>) -> Bound<'py, PyAny> {
        match &self.0 {
            VrfSecret::Tai(k) => py_bytes(py, &k.to_bytes()),
            VrfSecret::Ell2(k) => py_bytes(py, &k.to_bytes()),
            VrfSecret::Draft03(k) => py_bytes(py, &k.to_bytes()),
        }
    }

    /// The proof `pi` (80 bytes) and output `beta` (64 bytes) for `alpha`.
    fn prove(&self, alpha: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = alpha.py();
        call(py, || {
            let a = bytes(alpha)?;
            let e = |e: VrfError| e.refuse(py);
            let (pi, beta): (Vec<u8>, Vec<u8>) = match &self.0 {
                VrfSecret::Tai(k) => {
                    k.prove(&a).map(|(p, b)| (p.as_bytes().to_vec(), b.as_bytes().to_vec())).map_err(e)?
                }
                VrfSecret::Ell2(k) => {
                    k.prove(&a).map(|(p, b)| (p.as_bytes().to_vec(), b.as_bytes().to_vec())).map_err(e)?
                }
                VrfSecret::Draft03(k) => {
                    k.prove(&a).map(|(p, b)| (p.as_bytes().to_vec(), b.as_bytes().to_vec())).map_err(e)?
                }
            };
            Ok(py_tuple(py, vec![py_bytes(py, &pi), py_bytes(py, &beta)])?)
        })
    }

    fn __repr__(&self) -> String {
        alloc::format!("VrfSigningKey({}, <redacted>)", self.suite())
    }
}

// ------------------------------------------------------------------------------------------------ KES

#[derive(Clone)]
enum Construction {
    Sum(KesSum),
    Product(KesProduct),
    Cardano(CardanoSumKes),
}

/// A key-evolving signature scheme: the dilf4s sum or product composition over a hash, or Cardano's sum KES.
#[pyclass(frozen, module = "dilf4s._native", name = "Kes")]
pub(crate) struct PyKes(Construction);

enum KesSecret {
    Sum(SumKey),
    Product(ProductKey),
    Cardano(CardanoKey),
}

/// A KES secret key at one period, of the scheme that made it. Its bytes never appear in its `repr`.
#[pyclass(frozen, module = "dilf4s._native", name = "KesKey")]
pub(crate) struct PyKesKey {
    scheme: Construction,
    key: KesSecret,
}

fn kes_error<'py>(py: Python<'py>) -> impl Fn(KesError) -> Fail<'py> {
    move |e| e.refuse(py)
}

fn hash_algorithm_arg<'py>(obj: &Bound<'py, PyAny>) -> Result<HashAlgorithm, Fail<'py>> {
    as_algorithm(obj)
}

fn py_kes_key(py: Python<'_>, scheme: Construction, key: KesSecret) -> PyResult<Bound<'_, PyAny>> {
    Bound::new(py, PyKesKey { scheme, key }).map(Bound::into_any)
}

#[pymethods]
impl PyKes {
    /// The dilf4s sum composition of `height` levels over a hash.
    #[staticmethod]
    fn sum(algorithm: &Bound<'_, PyAny>, height: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = algorithm.py();
        call(py, || {
            let a = hash_algorithm_arg(algorithm)?;
            let h = u32_arg(height, "the height")?;
            Ok(Bound::new(py, Self(Construction::Sum(KesSum::new(a, h).map_err(kes_error(py))?)))?.into_any())
        })
    }

    /// The dilf4s product composition of a super tree and a sub tree over a hash.
    #[staticmethod]
    fn product(
        algorithm: &Bound<'_, PyAny>,
        super_height: &Bound<'_, PyAny>,
        sub_height: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let py = algorithm.py();
        call(py, || {
            let a = hash_algorithm_arg(algorithm)?;
            let (sup, sub) = (u32_arg(super_height, "the super height")?, u32_arg(sub_height, "the sub height")?);
            Ok(Bound::new(py, Self(Construction::Product(KesProduct::new(a, sup, sub).map_err(kes_error(py))?)))?
                .into_any())
        })
    }

    /// Cardano's sum KES (Ed25519, BLAKE2b-256) of `depth` levels.
    #[staticmethod]
    fn cardano(depth: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = depth.py();
        call(py, || {
            let d = u32_arg(depth, "the depth")?;
            Ok(Bound::new(py, Self(Construction::Cardano(CardanoSumKes::new(d).map_err(kes_error(py))?)))?.into_any())
        })
    }

    /// `dilf4s-sum`, `dilf4s-product` or `cardano-sum`.
    #[getter]
    fn construction(&self) -> &'static str {
        match self.0 {
            Construction::Sum(_) => "dilf4s-sum",
            Construction::Product(_) => "dilf4s-product",
            Construction::Cardano(_) => "cardano-sum",
        }
    }

    /// How many periods a key signs for.
    #[getter]
    fn total_periods(&self) -> u64 {
        match &self.0 {
            Construction::Sum(k) => k.total_periods(),
            Construction::Product(k) => k.total_periods(),
            Construction::Cardano(k) => k.total_periods(),
        }
    }

    /// The signature length in bytes.
    #[getter]
    fn signature_length(&self) -> usize {
        match &self.0 {
            Construction::Sum(k) => k.signature_length(),
            Construction::Product(k) => k.signature_length(),
            Construction::Cardano(k) => k.signature_length(),
        }
    }

    /// Verifies a signature at `period`: malformed input or a period out of range is a `KesError`, a well-formed
    /// signature that does not verify `False`.
    fn verify(
        &self,
        key: &Bound<'_, PyAny>,
        period: &Bound<'_, PyAny>,
        message: &Bound<'_, PyAny>,
        signature: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let py = key.py();
        call(py, || {
            let mut reads = Reads::default();
            let k = reads.read(key)?;
            let t = u64_arg(period, "the period")?;
            let (m, s) = (reads.read(message)?, reads.read(signature)?);
            let (k, m, s) = (reads.get(&k), reads.get(&m), reads.get(&s));
            let verdict = match &self.0 {
                Construction::Sum(x) => x.verify(k, t, m, s),
                Construction::Product(x) => x.verify(k, t, m, s),
                Construction::Cardano(x) => x.verify(k, t, m, s),
            };
            Ok(py_bool(py, verdict.map_err(kes_error(py))?))
        })
    }

    /// The period-0 key of a 32-byte seed.
    fn generate(&self, seed: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = seed.py();
        call(py, || {
            let s = bytes(seed)?;
            let e = kes_error(py);
            let key = match &self.0 {
                Construction::Sum(x) => KesSecret::Sum(py.detach(|| x.generate(&s)).map_err(e)?),
                Construction::Product(x) => KesSecret::Product(py.detach(|| x.generate(&s)).map_err(e)?),
                Construction::Cardano(x) => KesSecret::Cardano(py.detach(|| x.generate(&s)).map_err(e)?),
            };
            Ok(py_kes_key(py, self.0.clone(), key)?)
        })
    }

    /// A key's encoding loaded at `period`, every part checked.
    fn load(&self, raw: &Bound<'_, PyAny>, period: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = raw.py();
        call(py, || {
            let (r, t) = (bytes(raw)?, u64_arg(period, "the period")?);
            let e = kes_error(py);
            let key = match &self.0 {
                Construction::Sum(x) => KesSecret::Sum(x.load(&r, t).map_err(e)?),
                Construction::Product(x) => KesSecret::Product(x.load(&r, t).map_err(e)?),
                Construction::Cardano(x) => KesSecret::Cardano(x.load(&r, t).map_err(e)?),
            };
            Ok(py_kes_key(py, self.0.clone(), key)?)
        })
    }
}

#[pymethods]
impl PyKesKey {
    /// The key's period.
    #[getter]
    fn period(&self) -> u64 {
        match &self.key {
            KesSecret::Sum(k) => k.period(),
            KesSecret::Product(k) => k.period(),
            KesSecret::Cardano(k) => k.period(),
        }
    }

    /// The verifying key.
    fn verifying_key<'py>(&self, py: Python<'py>) -> Bound<'py, PyAny> {
        let vk = match (&self.scheme, &self.key) {
            (Construction::Sum(s), KesSecret::Sum(k)) => s.verifying_key(k),
            (Construction::Product(s), KesSecret::Product(k)) => s.verifying_key(k),
            (Construction::Cardano(s), KesSecret::Cardano(k)) => s.verifying_key(k),
            _ => Vec::new(),
        };
        py_bytes(py, &vk)
    }

    /// The key's canonical encoding: a copy Python holds, which it cannot zeroize.
    fn to_bytes<'py>(&self, py: Python<'py>) -> Bound<'py, PyAny> {
        let raw = match (&self.scheme, &self.key) {
            (Construction::Sum(s), KesSecret::Sum(k)) => s.key_bytes(k).to_vec(),
            (Construction::Product(s), KesSecret::Product(k)) => s.key_bytes(k).to_vec(),
            (_, KesSecret::Cardano(k)) => k.to_bytes().to_vec(),
            _ => Vec::new(),
        };
        py_bytes(py, &raw)
    }

    /// A signature on `message` at the key's period.
    fn sign(&self, message: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = message.py();
        call(py, || {
            let m = bytes(message)?;
            let e = kes_error(py);
            let sig = match (&self.scheme, &self.key) {
                (Construction::Sum(s), KesSecret::Sum(k)) => s.sign(k, &m).map_err(e)?,
                (Construction::Product(s), KesSecret::Product(k)) => s.sign(k, &m).map_err(e)?,
                (Construction::Cardano(s), KesSecret::Cardano(k)) => s.sign(k, &m),
                _ => return Err(mismatch(py, "a KES key of another scheme")),
            };
            Ok(py_bytes(py, &sig))
        })
    }

    /// The key at the next period, or `Exhausted` at the last.
    fn evolve(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        call(py, || self.evolved(py, None))
    }

    /// The key at `target`, a period at or after this one.
    fn evolve_to(&self, target: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = target.py();
        call(py, || {
            let t = u64_arg(target, "the target period")?;
            self.evolved(py, Some(t))
        })
    }

    fn __repr__(&self) -> String {
        alloc::format!("KesKey(period={}, <redacted>)", self.period())
    }
}

impl PyKesKey {
    fn evolved<'py>(&self, py: Python<'py>, target: Option<u64>) -> Answer<'py> {
        let e = kes_error(py);
        let key = match (&self.scheme, &self.key) {
            (Construction::Sum(s), KesSecret::Sum(k)) => {
                KesSecret::Sum(py.detach(|| target.map_or_else(|| s.evolve(k), |t| s.evolve_to(k, t))).map_err(e)?)
            }
            (Construction::Product(s), KesSecret::Product(k)) => {
                KesSecret::Product(py.detach(|| target.map_or_else(|| s.evolve(k), |t| s.evolve_to(k, t))).map_err(e)?)
            }
            (Construction::Cardano(s), KesSecret::Cardano(k)) => {
                KesSecret::Cardano(py.detach(|| target.map_or_else(|| s.evolve(k), |t| s.evolve_to(k, t))).map_err(e)?)
            }
            _ => return Err(mismatch(py, "a KES key of another scheme")),
        };
        Ok(py_kes_key(py, self.scheme.clone(), key)?)
    }
}

// ------------------------------------------------------------------------------------------------ sigma

/// A sigma proposition: `dlog`, `dhtuple`, `and`, `or` and `threshold` over secp256k1, bounded (sigma.md).
#[pyclass(frozen, module = "dilf4s._native", name = "SigmaProposition")]
pub(crate) struct PyProposition(Proposition);

fn sigma_error<'py>(py: Python<'py>) -> impl Fn(SigmaError) -> Fail<'py> {
    move |e| e.refuse(py)
}

/// The children, every one checked to be a proposition before any is copied for the owned constructors.
/// How many children a connective's argument holds, each checked a proposition where it stands and none of them
/// copied: the `TypeMismatch` of a child that is not one still comes before Rust's checks.
fn children_count<'py>(obj: &Bound<'py, PyAny>) -> Result<usize, Fail<'py>> {
    let py = obj.py();
    let n = count(obj, "the children")?;
    let is = |c: Bound<'py, PyAny>| c.cast::<PyProposition>().is_ok();
    let all = match (obj.cast::<PyTuple>(), obj.cast::<PyList>()) {
        (Ok(t), _) => t.iter().all(is),
        (_, Ok(l)) => l.iter().all(is),
        _ => false,
    };
    if all { Ok(n) } else { Err(mismatch(py, "expected dilf4s.signing.SigmaProposition children")) }
}

fn propositions<'py>(obj: &Bound<'py, PyAny>) -> Result<Vec<Bound<'py, PyProposition>>, Fail<'py>> {
    let py = obj.py();
    items(obj, "the children")?
        .into_iter()
        .map(|c| {
            c.cast_into::<PyProposition>()
                .map_err(|_| mismatch(py, "expected dilf4s.signing.SigmaProposition children"))
        })
        .collect()
}

/// A connective over `children`: `threshold` is its `k` when it is one. The owned Rust constructors take their
/// children by value, and a list may hold one large proposition many times, so Rust's checks of a connective
/// (sigma.rs `close`, in its order and words: the threshold, an empty one, the width, the depth, the leaves) are made
/// first on the children's own counts, and the children are copied only for a connective that passes them, whose
/// size those checks bound. The SP1 guest compiles the sigma module, whose key a constructor over borrowed children
/// would move.
fn connective<'py>(
    children: &Bound<'py, PyAny>,
    threshold: Option<usize>,
    build: impl FnOnce(Vec<Proposition>) -> Result<Proposition, SigmaError>,
) -> Answer<'py> {
    let py = children.py();
    // the children's types and count first, read in place: the checks by count alone (the threshold, an empty one,
    // the width) then refuse a connective before any child is materialized, so millions of references to one
    // proposition cost no memory in proportion to them
    let n = children_count(children)?;
    let k = threshold.map(|k| i64::try_from(k).unwrap_or(i64::MAX));
    let by_count: Option<String> = if let Some(k) = k
        && !usize::try_from(k).is_ok_and(|k| (1..=n).contains(&k))
    {
        Some(alloc::format!("a threshold needs 1 <= k <= {n} children, got k = {k}"))
    } else if n == 0 {
        Some("a connective needs at least one child: an empty one holds for anyone".into())
    } else if n > sigma::MAX_CHILDREN {
        Some(alloc::format!("a connective has at most {} children", sigma::MAX_CHILDREN))
    } else {
        None
    };
    if let Some(reason) = by_count {
        return Err(SigmaError::InvalidProposition { reason }.refuse(py));
    }
    let held = propositions(children)?;
    let depth = held.iter().map(|p| p.get().0.depth()).max().unwrap_or(0).saturating_add(1);
    let leaves = held.iter().fold(0usize, |l, p| l.saturating_add(p.get().0.leaves()));
    let refused = if depth > sigma::MAX_DEPTH {
        Some(alloc::format!("a proposition nests at most {} deep", sigma::MAX_DEPTH))
    } else if leaves > sigma::MAX_LEAVES {
        Some(alloc::format!("a proposition has at most {} leaves", sigma::MAX_LEAVES))
    } else {
        None
    };
    if let Some(reason) = refused {
        return Err(SigmaError::InvalidProposition { reason }.refuse(py));
    }
    let owned = held.iter().map(|p| p.get().0.clone()).collect();
    Ok(py_proposition(py, build(owned).map_err(sigma_error(py))?)?)
}

fn py_proposition(py: Python<'_>, p: Proposition) -> PyResult<Bound<'_, PyAny>> {
    Bound::new(py, PyProposition(p)).map(Bound::into_any)
}

#[pymethods]
impl PyProposition {
    /// Knowledge of the discrete log of `pk` (33-byte compressed point).
    #[staticmethod]
    fn dlog(pk: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = pk.py();
        call(py, || Ok(py_proposition(py, Proposition::dlog(&bytes(pk)?).map_err(sigma_error(py))?)?))
    }

    /// A Diffie-Hellman tuple: `u = g^x` and `v = h^x` for one `x`.
    #[staticmethod]
    fn dh_tuple(
        g: &Bound<'_, PyAny>,
        h: &Bound<'_, PyAny>,
        u: &Bound<'_, PyAny>,
        v: &Bound<'_, PyAny>,
    ) -> PyResult<Py<PyAny>> {
        let py = g.py();
        call(py, || {
            let mut reads = Reads::default();
            let (g, h, u, v) = (reads.read(g)?, reads.read(h)?, reads.read(u)?, reads.read(v)?);
            let (g, h, u, v) = (reads.get(&g), reads.get(&h), reads.get(&u), reads.get(&v));
            Ok(py_proposition(py, Proposition::dh_tuple(g, h, u, v).map_err(sigma_error(py))?)?)
        })
    }

    /// Every child.
    #[staticmethod]
    fn and_(children: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = children.py();
        call(py, || connective(children, None, Proposition::and))
    }

    /// Any child.
    #[staticmethod]
    fn or_(children: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = children.py();
        call(py, || connective(children, None, Proposition::or))
    }

    /// At least `k` children.
    #[staticmethod]
    fn threshold(k: &Bound<'_, PyAny>, children: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = k.py();
        call(py, || {
            let n = usize_arg(k, "the threshold")?;
            connective(children, Some(n), |cs| Proposition::threshold(n, cs))
        })
    }

    /// The proposition a canonical form denotes (the JLVM's `sigma_verify` argument).
    #[staticmethod]
    fn from_value(form: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = form.py();
        call(py, || {
            let v = value(form)?;
            Ok(py_proposition(py, Proposition::from_value(&v).map_err(sigma_error(py))?)?)
        })
    }

    /// The length of a proof of it, in bytes.
    #[getter]
    fn proof_length(&self) -> usize {
        self.0.proof_length()
    }

    /// The nesting depth.
    #[getter]
    fn depth(&self) -> usize {
        self.0.depth()
    }

    /// The number of leaves.
    #[getter]
    fn leaves(&self) -> usize {
        self.0.leaves()
    }

    fn __richcmp__(&self, other: &Bound<'_, PyAny>, op: CompareOp) -> Py<PyAny> {
        let py = other.py();
        match (other.cast::<Self>(), op) {
            (Ok(o), CompareOp::Eq) => py_bool(py, self.0 == o.get().0).unbind(),
            (Ok(o), CompareOp::Ne) => py_bool(py, self.0 != o.get().0).unbind(),
            _ => py.NotImplemented(),
        }
    }

    fn __repr__(&self) -> String {
        alloc::format!("SigmaProposition(depth={}, leaves={})", self.0.depth(), self.0.leaves())
    }
}

/// Sigma verification of a proof of a proposition (a `SigmaProposition`, or its canonical form as a `Value`) bound to
/// `message`: malformed input is a `SigmaError`, a well-formed proof that does not verify `False`.
#[pyfunction]
fn sigma_verify(
    proposition: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
    message: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = proof.py();
    call(py, || {
        let (p, m) = (bytes(proof)?, bytes(message)?);
        let verdict = if let Ok(prop) = proposition.cast::<PyProposition>() {
            let prop = &prop.get().0;
            py.detach(|| sigma::verify(prop, &p, &m))
        } else if let Ok(v) = proposition.cast::<PyValue>() {
            let v = &v.get().0;
            py.detach(|| sigma::verify_value(v, &p, &m))
        } else {
            return Err(mismatch(py, "expected a dilf4s.signing.SigmaProposition or a dilf4s.Value"));
        };
        Ok(py_bool(py, verdict.map_err(sigma_error(py))?))
    })
}

/// Sigma verification of a proof of knowledge of `key`'s discrete log.
#[pyfunction]
fn sigma_verify_dlog(
    key: &Bound<'_, PyAny>,
    message: &Bound<'_, PyAny>,
    proof: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = key.py();
    call(py, || {
        let mut reads = Reads::default();
        let (k, m, p) = (reads.read(key)?, reads.read(message)?, reads.read(proof)?);
        let (k, m, p) = (reads.get(&k), reads.get(&m), reads.get(&p));
        Ok(py_bool(py, sigma::verify_dlog_bytes(k, m, p).map_err(sigma_error(py))?))
    })
}

pub(crate) fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
    let py = m.py();
    m.add_class::<PySigningKey>()?;
    m.add_class::<PyWordList>()?;
    m.add_class::<PyExtendedKey>()?;
    m.add_class::<PyVrfSigningKey>()?;
    m.add_class::<PyKes>()?;
    m.add_class::<PyKesKey>()?;
    m.add_class::<PyProposition>()?;
    m.add_function(wrap_pyfunction!(signing_verify, m)?)?;
    m.add_function(wrap_pyfunction!(signing_decode_verifying_key, m)?)?;
    m.add_function(wrap_pyfunction!(bls_aggregate, m)?)?;
    m.add_function(wrap_pyfunction!(bls_fast_aggregate_verify, m)?)?;
    m.add_function(wrap_pyfunction!(bls_aggregate_verify, m)?)?;
    m.add_function(wrap_pyfunction!(bls_batch_verify, m)?)?;
    m.add_function(wrap_pyfunction!(bls_pop_verify, m)?)?;
    m.add_function(wrap_pyfunction!(ecrecover_address, m)?)?;
    m.add_function(wrap_pyfunction!(eth_address_of_key, m)?)?;
    m.add_function(wrap_pyfunction!(parse_path, m)?)?;
    m.add_function(wrap_pyfunction!(mnemonic_from_entropy, m)?)?;
    m.add_function(wrap_pyfunction!(mnemonic_to_entropy, m)?)?;
    m.add_function(wrap_pyfunction!(mnemonic_to_seed, m)?)?;
    m.add_function(wrap_pyfunction!(bip32_public_child, m)?)?;
    m.add_function(wrap_pyfunction!(icarus_public_child, m)?)?;
    m.add_function(wrap_pyfunction!(vrf_verify, m)?)?;
    m.add_function(wrap_pyfunction!(sigma_verify, m)?)?;
    m.add_function(wrap_pyfunction!(sigma_verify_dlog, m)?)?;
    let names = |ns: &[&str]| pyo3::types::PyTuple::new(py, ns.iter().map(|n| PyString::new(py, n)));
    m.add("SIGNING_SCHEMES", names(&SchemeId::ALL.map(SchemeId::name))?)?;
    m.add("SIGNING_SIGNERS", names(&SIGNERS)?)?;
    m.add("SIGNING_VRF_SUITES", names(&SuiteId::ALL.map(SuiteId::name))?)?;
    Ok(())
}
