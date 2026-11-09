//! The boundary between Python and the Rust SDK.
//!
//! Every public function takes its arguments as Python objects and reads each once, here, with Python's concrete
//! C API (never an attribute, method or protocol a caller's subclass could override, so no caller code runs): an
//! argument of the wrong type is a `TypeMismatch` refusal, never a Python exception. Mutable buffers (`bytearray`,
//! `memoryview`) are copied once, and `bytes` are borrowed, being immutable. What a call gives back is an `Ok` or
//! an `Err` of `dilf4s._result`, its refusal one of the package's frozen error dataclasses.
//!
//! A Python exception leaves a call only for what is no refusal: the package's own modules failing to import, or the
//! interpreter failing (memory exhausted, an interrupt).

use alloc::borrow::Cow;
use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

use dilf4s_core::BigInt;
use pyo3::IntoPyObjectExt;
use pyo3::exceptions::PyUnicodeEncodeError;
use pyo3::prelude::*;
use pyo3::sync::PyOnceLock;
use pyo3::types::{PyBool, PyByteArray, PyBytes, PyDict, PyInt, PyList, PyMemoryView, PyString, PyTuple, PyType};

use crate::value::PyValue;

/// Why a call gives no value: a refusal (already the Python error object), or an internal failure.
pub(crate) enum Fail<'py> {
    /// The refusal, as one of the package's error dataclasses.
    Refused(Bound<'py, PyAny>),
    /// A Python exception that is no refusal.
    Internal(PyErr),
}

impl From<PyErr> for Fail<'_> {
    fn from(e: PyErr) -> Self {
        Self::Internal(e)
    }
}

/// A call's outcome before it is wrapped: the value, or why there is none.
pub(crate) type Answer<'py> = Result<Bound<'py, PyAny>, Fail<'py>>;

/// The package's Python modules the native module builds objects of.
#[derive(Clone, Copy)]
pub(crate) enum Module {
    Result,
    Preview,
    Refusals,
    JlvmTypes,
    Canonical,
    NumberBound,
    UnsupportedItem,
    StateKey,
    Hash,
    Crypto,
    Key,
    Vrf,
    Kes,
    Sigma,
    Zk,
    Groth16,
    Merkle,
    Trie,
    Poseidon,
    PoseidonMerkle,
    VersionedTrie,
    Store,
    Jlvm,
    JlvmCryptoCause,
    ProofTypes,
}

const MODULES: usize = 25;

impl Module {
    const fn name(self) -> &'static str {
        match self {
            Self::Result => "dilf4s._result",
            Self::Preview => "dilf4s._preview",
            Self::Refusals => "dilf4s._refusals",
            Self::JlvmTypes => "dilf4s._jlvm_types",
            Self::Canonical => "dilf4s.errors.canonical",
            Self::NumberBound => "dilf4s.errors.number_bound",
            Self::UnsupportedItem => "dilf4s.errors.unsupported_item",
            Self::StateKey => "dilf4s.errors.state_key",
            Self::Hash => "dilf4s.errors.hash",
            Self::Crypto => "dilf4s.errors.crypto",
            Self::Key => "dilf4s.errors.key",
            Self::Vrf => "dilf4s.errors.vrf",
            Self::Kes => "dilf4s.errors.kes",
            Self::Sigma => "dilf4s.errors.sigma",
            Self::Zk => "dilf4s.errors.zk",
            Self::Groth16 => "dilf4s.errors.groth16",
            Self::Merkle => "dilf4s.errors.merkle",
            Self::Trie => "dilf4s.errors.trie",
            Self::Poseidon => "dilf4s.errors.poseidon",
            Self::PoseidonMerkle => "dilf4s.errors.poseidon_merkle",
            Self::VersionedTrie => "dilf4s.errors.versioned_trie",
            Self::Store => "dilf4s.errors.store",
            Self::Jlvm => "dilf4s.errors.jlvm",
            Self::JlvmCryptoCause => "dilf4s.errors.jlvm_crypto_cause",
            Self::ProofTypes => "dilf4s._proof_types",
        }
    }

    const fn index(self) -> usize {
        match self {
            Self::Result => 0,
            Self::Preview => 1,
            Self::Refusals => 2,
            Self::JlvmTypes => 3,
            Self::Canonical => 4,
            Self::NumberBound => 5,
            Self::UnsupportedItem => 6,
            Self::StateKey => 7,
            Self::Hash => 8,
            Self::Crypto => 9,
            Self::Key => 10,
            Self::Vrf => 11,
            Self::Kes => 12,
            Self::Sigma => 13,
            Self::Zk => 14,
            Self::Groth16 => 15,
            Self::Merkle => 16,
            Self::Trie => 17,
            Self::Poseidon => 18,
            Self::PoseidonMerkle => 19,
            Self::VersionedTrie => 20,
            Self::Store => 21,
            Self::Jlvm => 22,
            Self::JlvmCryptoCause => 23,
            Self::ProofTypes => 24,
        }
    }
}

#[allow(clippy::declare_interior_mutable_const, reason = "the array's initializer: each cell is written once")]
const EMPTY: PyOnceLock<Py<PyModule>> = PyOnceLock::new();
static LOADED: [PyOnceLock<Py<PyModule>>; MODULES] = [EMPTY; MODULES];
static OK: PyOnceLock<Py<PyType>> = PyOnceLock::new();
static ERR: PyOnceLock<Py<PyType>> = PyOnceLock::new();
static INT: PyOnceLock<Py<PyAny>> = PyOnceLock::new();

/// One of the package's modules, imported once.
fn module(py: Python<'_>, m: Module) -> PyResult<&Bound<'_, PyModule>> {
    let cell = LOADED.get(m.index()).ok_or_else(|| pyo3::exceptions::PyRuntimeError::new_err("no such module"))?;
    cell.get_or_try_init(py, || py.import(m.name()).map(Bound::unbind)).map(|m| m.bind(py))
}

/// The class `name` of module `m`.
pub(crate) fn class<'py>(py: Python<'py>, m: Module, name: &str) -> PyResult<Bound<'py, PyAny>> {
    module(py, m)?.getattr(name)
}

/// An instance of the class `name` of module `m`, built from keyword arguments.
pub(crate) fn instance<'py>(
    py: Python<'py>,
    m: Module,
    name: &str,
    fields: &[(&str, Bound<'py, PyAny>)],
) -> PyResult<Bound<'py, PyAny>> {
    let kwargs = PyDict::new(py);
    for (k, v) in fields {
        kwargs.set_item(*k, v)?;
    }
    class(py, m, name)?.call((), Some(&kwargs))
}

/// The refusal: an instance of the class `name` of module `m`.
pub(crate) fn refused<'py>(py: Python<'py>, m: Module, name: &str, fields: &[(&str, Bound<'py, PyAny>)]) -> Fail<'py> {
    match instance(py, m, name, fields) {
        Ok(e) => Fail::Refused(e),
        Err(e) => Fail::Internal(e),
    }
}

/// `canonical.TypeMismatch`: an argument not of its static type. `reason` says what was expected, never what was given.
pub(crate) fn mismatch<'py>(py: Python<'py>, reason: &str) -> Fail<'py> {
    refused(py, Module::Canonical, "TypeMismatch", &[("reason", PyString::new(py, reason).into_any())])
}

/// `Ok(value)` or `Err(refusal)`; an internal failure is raised.
pub(crate) fn respond(py: Python<'_>, answer: Answer<'_>) -> PyResult<Py<PyAny>> {
    match answer {
        Ok(v) => Ok(OK.import(py, Module::Result.name(), "Ok")?.call1((v,))?.unbind()),
        Err(Fail::Refused(e)) => Ok(ERR.import(py, Module::Result.name(), "Err")?.call1((e,))?.unbind()),
        Err(Fail::Internal(e)) => Err(e),
    }
}

/// Runs a call's body and wraps what it gives.
pub(crate) fn call<'py>(py: Python<'py>, body: impl FnOnce() -> Answer<'py>) -> PyResult<Py<PyAny>> {
    respond(py, body())
}

// ------------------------------------------------------------------------------------------------ arguments

/// Bytes: `bytes` borrowed (they are immutable), `bytearray` and `memoryview` copied once (they are not). A
/// `memoryview` gives its `tobytes()`: its memory's bytes in C order, whatever its format or strides; a released one is
/// a `TypeMismatch`. Nothing else is bytes, so a `str` or an `int` is a `TypeMismatch`.
pub(crate) fn bytes<'a, 'py>(obj: &'a Bound<'py, PyAny>) -> Result<Cow<'a, [u8]>, Fail<'py>> {
    let py = obj.py();
    if let Ok(b) = obj.cast::<PyBytes>() {
        return Ok(Cow::Borrowed(b.as_bytes()));
    }
    if let Ok(b) = obj.cast::<PyByteArray>() {
        return Ok(Cow::Owned(b.to_vec()));
    }
    if obj.is_instance_of::<PyMemoryView>() {
        // the type's own method, so nothing a caller defines runs; a released view raises ValueError
        let copied = py.get_type::<PyMemoryView>().getattr("tobytes")?.call1((obj,));
        return match copied {
            Ok(b) => Ok(Cow::Owned(b.cast::<PyBytes>().map_err(PyErr::from)?.as_bytes().to_vec())),
            Err(e) if e.is_instance_of::<pyo3::exceptions::PyValueError>(py) => {
                Err(mismatch(py, "expected bytes, a bytearray or a memoryview that is not released"))
            }
            Err(e) => Err(Fail::Internal(e)),
        };
    }
    Err(mismatch(py, "expected bytes, a bytearray or a memoryview"))
}

/// The length of a bytes argument, measured without copying it: a `bytearray`'s or a `memoryview`'s own size (a
/// `memoryview` cannot be subclassed, so reading its `nbytes` runs nothing a caller defines). Refused as [`bytes`]
/// refuses: anything else, or a released `memoryview`, is a `TypeMismatch`.
pub(crate) fn bytes_len<'py>(obj: &Bound<'py, PyAny>) -> Result<usize, Fail<'py>> {
    let py = obj.py();
    if let Ok(b) = obj.cast::<PyBytes>() {
        return Ok(b.as_bytes().len());
    }
    if let Ok(b) = obj.cast::<PyByteArray>() {
        return Ok(b.len());
    }
    if obj.is_instance_of::<PyMemoryView>() {
        return match obj.getattr("nbytes").and_then(|n| n.extract::<usize>()) {
            Ok(n) => Ok(n),
            Err(e) if e.is_instance_of::<pyo3::exceptions::PyValueError>(py) => {
                Err(mismatch(py, "expected bytes, a bytearray or a memoryview that is not released"))
            }
            Err(e) => Err(Fail::Internal(e)),
        };
    }
    Err(mismatch(py, "expected bytes, a bytearray or a memoryview"))
}

/// Where a byte argument read through [`Reads`] lives: the caller's `bytes` (immutable, so borrowed), or the call's one
/// copy of a mutable buffer.
pub(crate) enum Held<'a> {
    /// A `bytes` object's own bytes.
    Borrowed(&'a [u8]),
    /// The index of a `bytearray`'s or `memoryview`'s copy.
    Copied(usize),
}

/// A call's byte arguments from a collection: `bytes` borrowed, and each `bytearray` or `memoryview` copied once,
/// however many times the collection names it (by its identity, which holds while the collection holds it). A
/// collection of many references to one buffer then costs that buffer once, never its size times the count.
#[derive(Default)]
pub(crate) struct Reads {
    copies: Vec<Vec<u8>>,
    by_object: BTreeMap<usize, usize>,
}

impl Reads {
    /// Reads `obj` as [`bytes`] does, refusing what it refuses.
    pub(crate) fn read<'a, 'py>(&mut self, obj: &'a Bound<'py, PyAny>) -> Result<Held<'a>, Fail<'py>> {
        if let Ok(b) = obj.cast::<PyBytes>() {
            return Ok(Held::Borrowed(b.as_bytes()));
        }
        let key = obj.as_ptr().addr();
        if let Some(&at) = self.by_object.get(&key) {
            return Ok(Held::Copied(at));
        }
        self.copies.push(bytes(obj)?.into_owned());
        let at = self.copies.len().saturating_sub(1);
        self.by_object.insert(key, at);
        Ok(Held::Copied(at))
    }

    /// An optional argument (absent or `None` is `None`), read as [`Reads::read`] reads it.
    pub(crate) fn optional<'a, 'py>(
        &mut self,
        obj: Option<&'a Bound<'py, PyAny>>,
    ) -> Result<Option<Held<'a>>, Fail<'py>> {
        obj.filter(|o| !o.is_none()).map(|o| self.read(o)).transpose()
    }

    /// The bytes `held` names.
    pub(crate) fn get<'s>(&'s self, held: &Held<'s>) -> &'s [u8] {
        match held {
            Held::Borrowed(b) => b,
            Held::Copied(at) => self.copies.get(*at).map_or(&[], Vec::as_slice),
        }
    }
}

/// Text that is a value's (`Value.text`, a map key, JSON text): `None` when it holds a surrogate code point, which no
/// Rust text can, and which the caller refuses as its own family does (`UnpairedSurrogate`, for core).
pub(crate) fn text<'a, 'py>(obj: &'a Bound<'py, PyAny>) -> Result<Option<&'a str>, Fail<'py>> {
    let py = obj.py();
    let s = obj.cast::<PyString>().map_err(|_| mismatch(py, "expected a str"))?;
    match s.to_str() {
        Ok(t) => Ok(Some(t)),
        Err(e) if e.is_instance_of::<PyUnicodeEncodeError>(py) => Ok(None),
        Err(e) => Err(Fail::Internal(e)),
    }
}

/// A name, tag, key or path: its text with each surrogate code point replaced by U+FFFD. Every grammar these are read
/// against refuses U+FFFD, so the refusal (and its preview: the replacement is 3 UTF-8 bytes, as Rust's preview counts
/// an unpaired surrogate) is the one the text itself would get.
pub(crate) fn name<'a, 'py>(obj: &'a Bound<'py, PyAny>) -> Result<Cow<'a, str>, Fail<'py>> {
    let py = obj.py();
    let s = obj.cast::<PyString>().map_err(|_| mismatch(py, "expected a str"))?;
    match s.to_str() {
        Ok(t) => Ok(Cow::Borrowed(t)),
        Err(e) if e.is_instance_of::<PyUnicodeEncodeError>(py) => {
            Ok(Cow::Owned(from_wtf8(&surrogates_passed(s)?, |out, _| {
                out.push(char::REPLACEMENT_CHARACTER);
            })))
        }
        Err(e) => Err(Fail::Internal(e)),
    }
}

/// A `str`'s generalised UTF-8: each surrogate code point as its 3-byte form (ED A0..BF xx), by the type's own method.
fn surrogates_passed(s: &Bound<'_, PyString>) -> PyResult<Vec<u8>> {
    let py = s.py();
    let encoded = py.get_type::<PyString>().getattr("encode")?.call1((s, "utf-8", "surrogatepass"))?;
    Ok(encoded.cast::<PyBytes>().map_err(PyErr::from)?.as_bytes().to_vec())
}

/// JSON text, each surrogate code point (which no Rust text holds) transcribed so the parser refuses it where it
/// stands, as the TypeScript parser refuses a lone surrogate: inside a string, where it is a character of the string,
/// as its `\uXXXX` escape (`UnpairedSurrogate`); anywhere else (outside a string, or just after a backslash, where it
/// would end an escape) as U+FFFD, which no JSON syntax accepts there either (a syntax error, or `invalidEscape`), at
/// the same UTF-8 offset (both are 3 bytes). The lexical state is tracked across the text, so an escape the input
/// begins is never completed by the transcription.
pub(crate) fn json_text<'a, 'py>(obj: &'a Bound<'py, PyAny>) -> Result<Cow<'a, str>, Fail<'py>> {
    let py = obj.py();
    let s = obj.cast::<PyString>().map_err(|_| mismatch(py, "expected a str"))?;
    match s.to_str() {
        Ok(t) => Ok(Cow::Borrowed(t)),
        Err(e) if e.is_instance_of::<PyUnicodeEncodeError>(py) => {
            let mut lexer = JsonLexer::default();
            Ok(Cow::Owned(from_wtf8(&surrogates_passed(s)?, |out, code| {
                lexer.advance(out.as_bytes().get(lexer.seen..).unwrap_or_default());
                if lexer.in_string && !lexer.escaped {
                    out.push_str("\\u");
                    out.extend([12u32, 8, 4, 0].map(|at| char::from_digit((code >> at) & 0xf, 16).unwrap_or('0')));
                } else {
                    out.push(char::REPLACEMENT_CHARACTER);
                    lexer.escaped = false;
                }
                lexer.seen = out.len();
            })))
        }
        Err(e) => Err(Fail::Internal(e)),
    }
}

/// Where JSON text stands, as far as strings and escapes go: inside a string or not, and just after a backslash in one.
#[derive(Default)]
struct JsonLexer {
    in_string: bool,
    escaped: bool,
    /// How much of the text it has read.
    seen: usize,
}

impl JsonLexer {
    /// Reads `text`, the next part of the text. The structural bytes are ASCII, and no byte of a multi-byte character
    /// is one, so the bytes are read one by one.
    fn advance(&mut self, text: &[u8]) {
        for b in text {
            match (self.in_string, self.escaped, *b) {
                (true, true, _) => self.escaped = false,
                (true, false, b'\\') => self.escaped = true,
                (_, false, b'"') => self.in_string = !self.in_string,
                _ => {}
            }
        }
    }
}

/// Generalised UTF-8 as text, each surrogate's 3-byte form written by `surrogate` (given the text so far and the code
/// unit), any other ill-formed byte as U+FFFD (a `str` has none).
fn from_wtf8(wtf8: &[u8], mut surrogate: impl FnMut(&mut String, u32)) -> String {
    let mut out = String::with_capacity(wtf8.len().saturating_add(16));
    let mut rest = wtf8;
    loop {
        match core::str::from_utf8(rest) {
            Ok(tail) => {
                out.push_str(tail);
                return out;
            }
            Err(e) => {
                let (good, bad) = rest.split_at_checked(e.valid_up_to()).unwrap_or((rest, &[]));
                out.push_str(core::str::from_utf8(good).unwrap_or_default());
                // a surrogate's 3-byte form (ED A0..BF 80..BF), which UTF-8 refuses at its second byte
                let (unit, after) = match bad {
                    [0xed, b1 @ 0xa0..=0xbf, b2 @ 0x80..=0xbf, after @ ..] => {
                        surrogate(&mut out, 0xd000u32 | (u32::from(*b1 & 0x3f) << 6) | u32::from(*b2 & 0x3f));
                        (&[][..], after)
                    }
                    _ => bad.split_at_checked(e.error_len().unwrap_or(bad.len()).max(1)).unwrap_or((bad, &[])),
                };
                if !unit.is_empty() {
                    out.push(char::REPLACEMENT_CHARACTER);
                }
                rest = after;
            }
        }
    }
}

/// An `int` that is not a `bool` (whose subclass of `int` would read `True` as 1).
fn int<'a, 'py>(obj: &'a Bound<'py, PyAny>) -> Option<&'a Bound<'py, PyInt>> {
    if obj.is_instance_of::<PyBool>() { None } else { obj.cast::<PyInt>().ok() }
}

/// A non-negative integer that fits 64 bits.
pub(crate) fn u64_arg<'py>(obj: &Bound<'py, PyAny>, what: &str) -> Result<u64, Fail<'py>> {
    let py = obj.py();
    int(obj)
        .and_then(|n| n.extract::<u64>().ok())
        .ok_or_else(|| mismatch(py, &alloc::format!("expected {what}: an int from 0 to 2**64 - 1")))
}

/// A non-negative integer that fits 32 bits.
pub(crate) fn u32_arg<'py>(obj: &Bound<'py, PyAny>, what: &str) -> Result<u32, Fail<'py>> {
    let py = obj.py();
    int(obj)
        .and_then(|n| n.extract::<u32>().ok())
        .ok_or_else(|| mismatch(py, &alloc::format!("expected {what}: an int from 0 to 2**32 - 1")))
}

/// A non-negative integer that fits `usize`.
pub(crate) fn usize_arg<'py>(obj: &Bound<'py, PyAny>, what: &str) -> Result<usize, Fail<'py>> {
    let py = obj.py();
    int(obj)
        .and_then(|n| n.extract::<usize>().ok())
        .ok_or_else(|| mismatch(py, &alloc::format!("expected {what}: an int from 0 to 2**64 - 1")))
}

/// An integer of any width that is not wider than `max_bits` in Python's `bit_length` (a bound on its allocation:
/// Java's `bitLength`, which the data model bounds, is never greater), or `Err(None)` when it is wider. Read with
/// `int`'s own `bit_length` and `to_bytes`, never through decimal text (superlinear, and capped by Python).
pub(crate) fn bigint<'py>(obj: &Bound<'py, PyAny>, max_bits: u64) -> Result<Option<BigInt>, Fail<'py>> {
    let py = obj.py();
    let n = int(obj).ok_or_else(|| mismatch(py, "expected an int"))?;
    if let Ok(small) = n.extract::<i64>() {
        return Ok(Some(BigInt::from(small)));
    }
    let int_type = INT.get_or_try_init(py, || Ok::<_, PyErr>(py.get_type::<PyInt>().into_any().unbind()))?.bind(py);
    let bits: u64 = int_type.getattr("bit_length")?.call1((n,))?.extract()?;
    if bits > max_bits {
        return Ok(None);
    }
    // the sign takes one more bit
    let length = bits.saturating_add(8) / 8;
    let kwargs = PyDict::new(py);
    kwargs.set_item("signed", true)?;
    let le = int_type.getattr("to_bytes")?.call((n, length, "little"), Some(&kwargs))?;
    Ok(Some(BigInt::from_signed_bytes_le(le.cast::<PyBytes>().map_err(PyErr::from)?.as_bytes())))
}

/// A canonical value: a `dilf4s.Value`, never converted from anything else.
pub(crate) fn value<'py>(obj: &Bound<'py, PyAny>) -> Result<dilf4s_core::Value, Fail<'py>> {
    obj.cast::<PyValue>().map(|v| v.get().0.clone()).map_err(|_| mismatch(obj.py(), "expected a dilf4s.Value"))
}

/// A sequence argument is a `list` or a `tuple` (or a subclass of one), read with the concrete API, so no caller code
/// runs reading it and its count is exact before any item is read. Anything else is a `TypeMismatch`, another
/// sequence (a `range`, a `UserList`) included, as the TypeScript SDK takes arrays only: such a sequence's length and
/// items are its own code's, which could raise, lie or expand a small argument into a large one, so a caller holding
/// one passes `list(it)`, running that code itself.
fn not_items<'py>(py: Python<'py>, what: &str) -> Fail<'py> {
    mismatch(py, &alloc::format!("expected {what}: a list or a tuple"))
}

/// How many items a sequence argument holds, none of them read.
pub(crate) fn count<'py>(obj: &Bound<'py, PyAny>, what: &str) -> Result<usize, Fail<'py>> {
    if let Ok(t) = obj.cast::<PyTuple>() {
        return Ok(t.len());
    }
    obj.cast::<PyList>().map(PyListMethods::len).map_err(|_| not_items(obj.py(), what))
}

/// The items of a sequence argument, read once.
pub(crate) fn items<'py>(obj: &Bound<'py, PyAny>, what: &str) -> Result<Vec<Bound<'py, PyAny>>, Fail<'py>> {
    if let Ok(t) = obj.cast::<PyTuple>() {
        return Ok(t.iter().collect());
    }
    obj.cast::<PyList>().map(|l| l.iter().collect()).map_err(|_| not_items(obj.py(), what))
}

/// The items of a sequence argument holding at most `max`, or the count of one holding more, which is refused by its
/// count with none of its items read: an argument refused for its count alone costs only that.
pub(crate) fn items_within<'py>(
    obj: &Bound<'py, PyAny>,
    what: &str,
    max: usize,
) -> Result<Result<Vec<Bound<'py, PyAny>>, usize>, Fail<'py>> {
    let n = count(obj, what)?;
    if n > max { Ok(Err(n)) } else { items(obj, what).map(Ok) }
}

/// Exactly `N` items (a pair, a triple), or the `TypeMismatch` `expected`, by the count before any item is read.
pub(crate) fn tuple_of<'py, const N: usize>(
    obj: &Bound<'py, PyAny>,
    what: &str,
    expected: &str,
) -> Result<[Bound<'py, PyAny>; N], Fail<'py>> {
    let py = obj.py();
    items_within(obj, what, N)?.ok().and_then(|v| <[_; N]>::try_from(v).ok()).ok_or_else(|| mismatch(py, expected))
}

/// A `dict` entry.
pub(crate) type Entry<'py> = (Bound<'py, PyAny>, Bound<'py, PyAny>);

/// The entries of a `dict`, read once (no `items` a caller's subclass defines runs).
pub(crate) fn entries<'py>(obj: &Bound<'py, PyAny>, what: &str) -> Result<Vec<Entry<'py>>, Fail<'py>> {
    let py = obj.py();
    obj.cast::<PyDict>()
        .map(|d| d.iter().collect())
        .map_err(|_| mismatch(py, &alloc::format!("expected {what}: a dict")))
}

// ------------------------------------------------------------------------------------------------ results

/// A Python `int` of any width, through `int.from_bytes` (never decimal text).
pub(crate) fn py_int<'py>(py: Python<'py>, n: &BigInt) -> PyResult<Bound<'py, PyAny>> {
    if let Ok(small) = i64::try_from(n) {
        return small.into_bound_py_any(py);
    }
    let int_type = INT.get_or_try_init(py, || Ok::<_, PyErr>(py.get_type::<PyInt>().into_any().unbind()))?.bind(py);
    let kwargs = PyDict::new(py);
    kwargs.set_item("signed", true)?;
    int_type.getattr("from_bytes")?.call((PyBytes::new(py, &n.to_signed_bytes_le()), "little"), Some(&kwargs))
}

/// Python `bytes`.
pub(crate) fn py_bytes<'py>(py: Python<'py>, b: &[u8]) -> Bound<'py, PyAny> {
    PyBytes::new(py, b).into_any()
}

/// A Python `str`.
pub(crate) fn py_str<'py>(py: Python<'py>, s: &str) -> Bound<'py, PyAny> {
    PyString::new(py, s).into_any()
}

/// A Python `bool`.
pub(crate) fn py_bool(py: Python<'_>, b: bool) -> Bound<'_, PyAny> {
    PyBool::new(py, b).to_owned().into_any()
}

/// A Python `int` of a `u64`.
pub(crate) fn py_u64(py: Python<'_>, n: u64) -> PyResult<Bound<'_, PyAny>> {
    n.into_bound_py_any(py)
}

/// A Python `tuple`.
pub(crate) fn py_tuple<'py>(py: Python<'py>, items: Vec<Bound<'py, PyAny>>) -> PyResult<Bound<'py, PyAny>> {
    PyTuple::new(py, items).map(Bound::into_any)
}

/// `None`.
pub(crate) fn py_none(py: Python<'_>) -> Bound<'_, PyAny> {
    py.None().into_bound(py)
}
