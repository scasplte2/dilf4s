//! `dilf4s.Value`: a canonical value, the Rust SDK's own (immutable, its parts shared), behind a frozen class.
//!
//! Returning one is O(1) whatever its weight, so a JLVM result built with shared parts reaches Python unexpanded;
//! its accessors read one level at a time. [`Value.of`](PyValue::of) builds one from Python data and
//! [`Value.to_python`](PyValue::to_python) expands one into Python data, the latter within a weight the caller gives.

use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;
use core::hash::{Hash as _, Hasher as _};

use dilf4s_core::value::{MAX_DEPTH, MapShape, Rational, Text};
use dilf4s_core::{CanonicalError, MAX_INTEGER_BITS, MAX_RATIONAL_BITS, Value};
use pyo3::prelude::*;
use pyo3::pyclass::CompareOp;
use pyo3::sync::PyOnceLock;
use pyo3::types::{PyBool, PyByteArray, PyBytes, PyDict, PyInt, PyList, PyMemoryView, PyString, PyTuple, PyType};

use crate::boundary::{
    Fail, bigint, bytes, call, entries, items, mismatch, py_bool, py_bytes, py_int, py_none, py_str, py_tuple, text,
    u64_arg,
};
use crate::errors::{Refusal, too_large};

static FRACTION: PyOnceLock<Py<PyType>> = PyOnceLock::new();

fn fraction_type(py: Python<'_>) -> PyResult<&Bound<'_, PyType>> {
    FRACTION.import(py, "fractions", "Fraction")
}

/// A canonical value.
#[pyclass(frozen, module = "dilf4s._native", name = "Value")]
pub(crate) struct PyValue(pub(crate) Value);

/// A value as a Python object.
pub(crate) fn py_value(py: Python<'_>, v: Value) -> PyResult<Bound<'_, PyAny>> {
    Bound::new(py, PyValue(v)).map(Bound::into_any)
}

fn refuse<'py>(py: Python<'py>, e: &CanonicalError) -> Fail<'py> {
    e.refuse(py)
}

/// `UnpairedSurrogate`: text with a surrogate code point, which no canonical text holds.
fn surrogate(py: Python<'_>) -> Fail<'_> {
    refuse(py, &CanonicalError::UnpairedSurrogate)
}

/// An integer within the data model, refused as every producer refuses a wider one.
fn integer<'py>(obj: &Bound<'py, PyAny>) -> Result<Value, Fail<'py>> {
    let py = obj.py();
    let too_wide = || refuse(py, &CanonicalError::IntegerTooWide { limit: MAX_INTEGER_BITS });
    // Python's bit length is Java's or one more (-2^k), so one more bit is allocated, never one fewer accepted
    let n = bigint(obj, MAX_INTEGER_BITS.saturating_add(1))?.ok_or_else(too_wide)?;
    Value::integer(n).map_err(|_| too_wide())
}

/// The number of a `fractions.Fraction`, which is in lowest terms already.
fn fraction<'py>(obj: &Bound<'py, PyAny>) -> Result<Value, Fail<'py>> {
    let py = obj.py();
    let numerator = obj.getattr("numerator")?;
    let denominator = obj.getattr("denominator")?;
    if denominator.extract::<i64>().is_ok_and(|d| d == 1) {
        return integer(&numerator);
    }
    let too_wide = || refuse(py, &CanonicalError::RationalTooWide { limit: MAX_RATIONAL_BITS });
    let cap = MAX_RATIONAL_BITS.saturating_add(1);
    let n = bigint(&numerator, cap)?.ok_or_else(too_wide)?;
    let d = bigint(&denominator, cap)?.ok_or_else(too_wide)?;
    Value::number(&n, &d).map_err(|e| e.refuse(py))
}

/// A node of Python data still to read, or a container whose parts are read and waiting on the results stack.
enum Frame<'py> {
    Enter(Bound<'py, PyAny>, usize),
    Array(usize, usize),
    Map(usize, Vec<Text>),
}

/// The address of a Python object: a key for the memo of containers already converted, while the input holds them.
fn address(obj: &Bound<'_, PyAny>) -> usize {
    obj.as_ptr().addr()
}

/// Python data as a value, with an explicit stack (never recursive). A container met again is converted once and
/// shared, so the work is the input's size in objects, not its expansion; one met inside itself is a cycle, which no
/// value has, refused as `DepthExceeded`, as is nesting past `MAX_DEPTH`.
fn of<'py>(root: &Bound<'py, PyAny>) -> Result<Value, Fail<'py>> {
    let py = root.py();
    let deep = || refuse(py, &CanonicalError::DepthExceeded { limit: u64::try_from(MAX_DEPTH).unwrap_or(u64::MAX) });
    let fraction_cls = fraction_type(py)?;
    let mut todo = vec![Frame::Enter(root.clone(), 0)];
    let mut done: Vec<Value> = Vec::new();
    let mut memo: BTreeMap<usize, Value> = BTreeMap::new();
    // a key met again, in this map or another, is shared: its text is copied once in all
    let mut keys_seen: BTreeMap<usize, Text> = BTreeMap::new();
    let mut open: alloc::collections::BTreeSet<usize> = alloc::collections::BTreeSet::new();
    while let Some(frame) = todo.pop() {
        match frame {
            Frame::Enter(obj, level) => {
                let key = address(&obj);
                if let Some(v) = memo.get(&key) {
                    done.push(v.clone());
                } else if open.contains(&key) {
                    return Err(deep());
                } else if let Ok(v) = obj.cast::<PyValue>() {
                    done.push(v.get().0.clone());
                } else if obj.is_none() {
                    done.push(Value::Null);
                } else if let Ok(b) = obj.cast::<PyBool>() {
                    done.push(Value::Bool(b.is_true()));
                } else if obj.is_instance_of::<PyInt>() {
                    // numbers are shared like text and bytes: a large one met again is converted once
                    let v = integer(&obj)?;
                    memo.insert(key, v.clone());
                    done.push(v);
                } else if obj.get_type().is(fraction_cls) {
                    let v = fraction(&obj)?;
                    memo.insert(key, v.clone());
                    done.push(v);
                } else if obj.is_instance_of::<PyString>() {
                    // a leaf met again is shared, as a container is: its text is copied once in all
                    let v = Value::text(text(&obj)?.ok_or_else(|| surrogate(py))?);
                    memo.insert(key, v.clone());
                    done.push(v);
                } else if obj.is_instance_of::<PyBytes>()
                    || obj.is_instance_of::<PyByteArray>()
                    || obj.is_instance_of::<PyMemoryView>()
                {
                    let v = Value::bytes(bytes(&obj)?.into_owned());
                    memo.insert(key, v.clone());
                    done.push(v);
                } else if obj.is_instance_of::<PyList>() || obj.is_instance_of::<PyTuple>() {
                    if level >= MAX_DEPTH {
                        return Err(deep());
                    }
                    let parts = items(&obj, "an array")?;
                    open.insert(key);
                    todo.push(Frame::Array(key, parts.len()));
                    todo.extend(parts.into_iter().rev().map(|p| Frame::Enter(p, level.saturating_add(1))));
                } else if obj.is_instance_of::<PyDict>() {
                    if level >= MAX_DEPTH {
                        return Err(deep());
                    }
                    let pairs = entries(&obj, "a map")?;
                    let mut keys = Vec::with_capacity(pairs.len());
                    let mut values = Vec::with_capacity(pairs.len());
                    for (k, v) in pairs {
                        if !k.is_instance_of::<PyString>() {
                            return Err(mismatch(py, "expected a map with str keys"));
                        }
                        let at = address(&k);
                        let key = if let Some(t) = keys_seen.get(&at) {
                            t.clone()
                        } else {
                            let t = Text::from(text(&k)?.ok_or_else(|| surrogate(py))?);
                            keys_seen.insert(at, t.clone());
                            t
                        };
                        keys.push(key);
                        values.push(v);
                    }
                    open.insert(key);
                    todo.push(Frame::Map(key, keys));
                    todo.extend(values.into_iter().rev().map(|p| Frame::Enter(p, level.saturating_add(1))));
                } else {
                    return Err(mismatch(
                        py,
                        "expected None, a bool, an int, a Fraction, a str, bytes, a list, a tuple, a dict or a Value",
                    ));
                }
            }
            Frame::Array(key, n) => {
                let parts = done.split_off(done.len().saturating_sub(n));
                let v = Value::array(parts).map_err(|e| refuse(py, &e))?;
                open.remove(&key);
                memo.insert(key, v.clone());
                done.push(v);
            }
            Frame::Map(key, names) => {
                let parts = done.split_off(done.len().saturating_sub(names.len()));
                // a map of shared keys, none copied (a Python dict has no repeated key)
                let shape = MapShape::new(names).map_err(|e| refuse(py, &e))?;
                let v = shape.map(&parts).map(Value::Map).map_err(|e| refuse(py, &e))?;
                open.remove(&key);
                memo.insert(key, v.clone());
                done.push(v);
            }
        }
    }
    done.pop().ok_or_else(|| mismatch(py, "expected a value"))
}

/// A rational as a `fractions.Fraction`.
fn py_fraction<'py>(py: Python<'py>, r: &Rational) -> PyResult<Bound<'py, PyAny>> {
    fraction_type(py)?.call1((py_int(py, r.numerator())?, py_int(py, r.denominator())?))
}

/// A scalar as Python data; `None` for an array or a map.
fn scalar<'py>(py: Python<'py>, v: &Value) -> PyResult<Option<Bound<'py, PyAny>>> {
    Ok(Some(match v {
        Value::Null => py_none(py),
        Value::Bool(b) => py_bool(py, *b),
        Value::Int(n) => py_int(py, n.as_bigint())?,
        Value::Rational(r) => py_fraction(py, r)?,
        Value::Text(t) => py_str(py, t),
        Value::Bytes(b) => py_bytes(py, b),
        Value::Array(_) | Value::Map(_) => return Ok(None),
    }))
}

enum Out<'v> {
    Enter(&'v Value),
    List(usize),
    Dict(Vec<&'v str>),
}

/// A value as Python data, with an explicit stack: arrays as lists, maps as dicts in canonical key order.
fn to_python<'py>(py: Python<'py>, root: &Value) -> PyResult<Bound<'py, PyAny>> {
    let mut todo = vec![Out::Enter(root)];
    let mut done: Vec<Bound<'py, PyAny>> = Vec::new();
    while let Some(frame) = todo.pop() {
        match frame {
            Out::Enter(Value::Array(a)) => {
                todo.push(Out::List(a.len()));
                todo.extend(a.items().iter().rev().map(Out::Enter));
            }
            Out::Enter(Value::Map(m)) => {
                todo.push(Out::Dict(m.keys().map(dilf4s_core::Text::as_str).collect()));
                let values: Vec<&Value> = m.values().collect();
                todo.extend(values.into_iter().rev().map(Out::Enter));
            }
            Out::Enter(other) => done.push(scalar(py, other)?.unwrap_or_else(|| py_none(py))),
            Out::List(n) => {
                let parts = done.split_off(done.len().saturating_sub(n));
                done.push(PyList::new(py, parts)?.into_any());
            }
            Out::Dict(keys) => {
                let parts = done.split_off(done.len().saturating_sub(keys.len()));
                let d = PyDict::new(py);
                for (k, v) in keys.into_iter().zip(parts) {
                    d.set_item(k, v)?;
                }
                done.push(d.into_any());
            }
        }
    }
    done.pop().ok_or_else(|| pyo3::exceptions::PyRuntimeError::new_err("no value"))
}

#[pymethods]
impl PyValue {
    /// `null`.
    #[staticmethod]
    fn null(py: Python<'_>) -> PyResult<Bound<'_, PyAny>> {
        py_value(py, Value::Null)
    }

    /// `false` or `true`.
    #[staticmethod]
    fn bool(b: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = b.py();
        call(py, || {
            let b = b.cast::<PyBool>().map_err(|_| mismatch(py, "expected a bool"))?;
            Ok(py_value(py, Value::Bool(b.is_true()))?)
        })
    }

    /// An integer, refused as `IntegerTooWide` past `MAX_INTEGER_BITS` (Java's `bitLength`).
    #[staticmethod]
    fn int(n: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = n.py();
        call(py, || Ok(py_value(py, integer(n)?)?))
    }

    /// The number `numerator / denominator`, reduced: an integer when it is one, else a rational; refused when either
    /// is too wide once reduced, or for a zero denominator.
    #[staticmethod]
    fn number(numerator: &Bound<'_, PyAny>, denominator: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = numerator.py();
        call(py, || {
            let n = bigint(numerator, u64::MAX)?.ok_or_else(|| mismatch(py, "expected an int"))?;
            let d = bigint(denominator, u64::MAX)?.ok_or_else(|| mismatch(py, "expected an int"))?;
            Ok(py_value(py, Value::number(&n, &d).map_err(|e| e.refuse(py))?)?)
        })
    }

    /// Text; a surrogate code point is `UnpairedSurrogate`.
    #[staticmethod]
    fn text(s: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = s.py();
        call(py, || Ok(py_value(py, Value::text(text(s)?.ok_or_else(|| surrogate(py))?))?))
    }

    /// A byte string, copied once from a `bytearray` or `memoryview`.
    #[staticmethod]
    fn bytes(b: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = b.py();
        call(py, || Ok(py_value(py, Value::bytes(bytes(b)?.into_owned()))?))
    }

    /// An array of values (a list or tuple of `Value`), within the depth limit.
    #[staticmethod]
    fn array(values: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = values.py();
        call(py, || {
            let parts = items(values, "an array")?.iter().map(crate::boundary::value).collect::<Result<Vec<_>, _>>()?;
            Ok(py_value(py, Value::array(parts).map_err(|e| refuse(py, &e))?)?)
        })
    }

    /// A map of a `dict` of str keys to `Value`s; null values are dropped, and excess depth is `DepthExceeded`.
    #[staticmethod]
    fn map(fields: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = fields.py();
        call(py, || {
            let mut named = Vec::new();
            for (k, v) in entries(fields, "a map")? {
                let key = text(&k).map_err(|_| mismatch(py, "expected a map with str keys"))?;
                named.push((String::from(key.ok_or_else(|| surrogate(py))?), crate::boundary::value(&v)?));
            }
            Ok(py_value(py, Value::map(named).map_err(|e| refuse(py, &e))?)?)
        })
    }

    /// A value of Python data: `None`, `bool`, `int`, `fractions.Fraction`, `str`, `bytes`-like, `list` and `tuple`
    /// (arrays), `dict` with `str` keys (maps; `None` values dropped), and `Value`. A container that occurs more than
    /// once is converted once and shared; a cycle is `DepthExceeded`.
    #[staticmethod]
    fn of(obj: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = obj.py();
        call(py, || Ok(py_value(py, of(obj)?)?))
    }

    /// The kind: `null`, `bool`, `int`, `rational`, `text`, `bytes`, `array` or `map`.
    #[getter]
    fn kind(&self) -> &'static str {
        self.0.kind()
    }

    /// The nesting depth: 0 for a scalar, one more than the deepest element for an array or map.
    #[getter]
    fn depth(&self) -> usize {
        self.0.depth()
    }

    /// The weight: the logical size, counting a shared part every time it occurs (`canonical-encoding.md`).
    #[getter]
    fn weight(&self) -> u64 {
        self.0.weight()
    }

    /// The boolean, if this is one.
    fn as_bool(&self) -> Option<bool> {
        if let Value::Bool(b) = self.0 { Some(b) } else { None }
    }

    /// The integer, if this is one.
    fn as_int<'py>(&self, py: Python<'py>) -> PyResult<Option<Bound<'py, PyAny>>> {
        self.0.as_int().map(|n| py_int(py, n)).transpose()
    }

    /// The rational (never an integer), if this is one, as a `fractions.Fraction`.
    fn as_rational<'py>(&self, py: Python<'py>) -> PyResult<Option<Bound<'py, PyAny>>> {
        if let Value::Rational(r) = &self.0 { py_fraction(py, r).map(Some) } else { Ok(None) }
    }

    /// The text, if this is text.
    fn as_text(&self) -> Option<&str> {
        self.0.as_str()
    }

    /// The bytes, if this is a byte string.
    fn as_bytes<'py>(&self, py: Python<'py>) -> Option<Bound<'py, PyAny>> {
        self.0.as_bytes().map(|b| py_bytes(py, b))
    }

    /// The elements, if this is an array.
    fn as_array<'py>(&self, py: Python<'py>) -> PyResult<Option<Bound<'py, PyAny>>> {
        let Value::Array(a) = &self.0 else {
            return Ok(None);
        };
        let parts = a.items().iter().map(|v| py_value(py, v.clone())).collect::<PyResult<Vec<_>>>()?;
        py_tuple(py, parts).map(Some)
    }

    /// The entries in canonical key order, if this is a map, as a new `dict`.
    fn as_map<'py>(&self, py: Python<'py>) -> PyResult<Option<Bound<'py, PyAny>>> {
        let Value::Map(m) = &self.0 else {
            return Ok(None);
        };
        let d = PyDict::new(py);
        for (k, v) in m.entries() {
            d.set_item(k.as_str(), py_value(py, v.clone())?)?;
        }
        Ok(Some(d.into_any()))
    }

    /// The value at `key`, if this is a map holding it. A key that is not a `str` finds nothing.
    fn get<'py>(&self, py: Python<'py>, key: &Bound<'py, PyAny>) -> PyResult<Option<Bound<'py, PyAny>>> {
        let Value::Map(m) = &self.0 else {
            return Ok(None);
        };
        let Ok(Some(k)) = text(key) else {
            return Ok(None);
        };
        m.get(k).map(|v| py_value(py, v.clone())).transpose()
    }

    /// The value as Python data (arrays as lists, maps as dicts, rationals as `Fraction`s), if it weighs at most
    /// `max_weight`: a heavier one is `TooLarge`, refused in O(1) before anything is built.
    fn to_python(&self, max_weight: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
        let py = max_weight.py();
        call(py, || {
            let limit = u64_arg(max_weight, "the weight limit")?;
            if self.0.weight() > limit {
                return Err(too_large(py, limit));
            }
            Ok(to_python(py, &self.0)?)
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

    /// O(1): the kind, weight and depth, which equal values share, and a scalar's first bytes.
    fn __hash__(&self) -> u64 {
        let mut h = Fnv(0xcbf2_9ce4_8422_2325);
        self.0.kind().hash(&mut h);
        self.0.weight().hash(&mut h);
        self.0.depth().hash(&mut h);
        match &self.0 {
            Value::Bool(b) => b.hash(&mut h),
            Value::Text(t) => t.as_bytes().iter().take(64).for_each(|b| b.hash(&mut h)),
            Value::Bytes(b) => b.iter().take(64).for_each(|x| x.hash(&mut h)),
            Value::Int(n) => n.as_bigint().to_signed_bytes_le().iter().take(64).for_each(|b| b.hash(&mut h)),
            _ => {}
        }
        h.finish() >> 1
    }

    fn __repr__(&self) -> String {
        alloc::format!("Value({:?})", self.0)
    }
}

/// FNV-1a: a fixed hash, so a value's hash is the same in every process.
struct Fnv(u64);

impl core::hash::Hasher for Fnv {
    fn finish(&self) -> u64 {
        self.0
    }

    fn write(&mut self, bytes: &[u8]) {
        for b in bytes {
            self.0 = (self.0 ^ u64::from(*b)).wrapping_mul(0x0100_0000_01b3);
        }
    }
}

/// `Value` and its parts, for the module.
pub(crate) fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PyValue>()
}
