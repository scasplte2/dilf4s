//! `dilf4s.core`: canonical CBOR, the JSON mapping, hashing, value commitments and previews (dilf4s-core).

use alloc::vec::Vec;

use dilf4s_core::built::{BuildError, Step, build};
use dilf4s_core::commitment::value_commitment;
use dilf4s_core::hash::{DomainTag, HashAlgorithm, HashError};
use dilf4s_core::json::RenderError;
use dilf4s_core::{Preview, Value, cbor, json};
use pyo3::prelude::*;

use crate::boundary::{
    Answer, Fail, Module, Reads, bytes, bytes_len, call, instance, items, items_within, json_text, mismatch, name,
    py_bytes, py_str, py_tuple, py_u64, u32_arg, u64_arg, usize_arg, value as as_value,
};
use crate::errors::{Refusal, preview, too_large};
use crate::value::py_value;

/// A value heavier than this is encoded with the GIL released.
const DETACH_WEIGHT: u64 = 1 << 16;

/// The most times a `built-value` wrap step runs: one past the nesting limit (see `build_value`).
const WRAP_BOUND: u64 = dilf4s_core::generated::limits::CANONICAL_MAX_DEPTH.saturating_add(1);

/// The canonical CBOR of a value: O(its weight), which may be far more than its memory (shared parts).
#[pyfunction]
fn cbor_encode(value: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = value.py();
    call(py, || {
        let v = as_value(value)?;
        let encoded = if v.weight() > DETACH_WEIGHT { py.detach(|| cbor::encode(&v)) } else { cbor::encode(&v) };
        Ok(py_bytes(py, &encoded))
    })
}

/// A limit in bytes: what fits `usize` is the limit itself, and a larger one is past every encoding.
fn limit<'py>(obj: &Bound<'py, PyAny>) -> Result<(usize, u64), Fail<'py>> {
    let max = u64_arg(obj, "the byte limit")?;
    Ok((usize::try_from(max).unwrap_or(usize::MAX), max))
}

/// The canonical CBOR of a value if it is at most `max_bytes` long, else `TooLarge`: O(min(weight, `max_bytes`)).
#[pyfunction]
fn cbor_encode_within(value: &Bound<'_, PyAny>, max_bytes: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = value.py();
    call(py, || {
        let v = as_value(value)?;
        let (max, given) = limit(max_bytes)?;
        let encoded = py.detach(|| cbor::encode_within(&v, max)).map_err(|_| too_large(py, given))?;
        Ok(py_bytes(py, &encoded))
    })
}

/// The length of a value's canonical CBOR if it is at most `max_bytes`, counted without encoding it.
#[pyfunction]
fn cbor_encoded_length_within(value: &Bound<'_, PyAny>, max_bytes: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = value.py();
    call(py, || {
        let v = as_value(value)?;
        let (max, given) = limit(max_bytes)?;
        let n = py.detach(|| cbor::encoded_len_within(&v, max)).map_err(|_| too_large(py, given))?;
        Ok(py_u64(py, u64::try_from(n).unwrap_or(u64::MAX))?)
    })
}

/// The value of canonical CBOR (strict: anything else is refused).
#[pyfunction]
fn cbor_decode(data: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = data.py();
    call(py, || {
        let b = bytes(data)?;
        Ok(py_value(py, cbor::decode(&b).map_err(|e| e.refuse(py))?)?)
    })
}

/// [`cbor_decode`] with a nesting limit below the data model's.
#[pyfunction]
fn cbor_decode_with_limit(data: &Bound<'_, PyAny>, max_depth: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = data.py();
    call(py, || {
        let b = bytes(data)?;
        let depth = usize_arg(max_depth, "the depth limit")?;
        Ok(py_value(py, cbor::decode_with_limit(&b, depth).map_err(|e| e.refuse(py))?)?)
    })
}

/// The value of canonical JSON text (RFC 8259, strict: no duplicate names, bounded numbers).
#[pyfunction]
fn json_parse(text: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = text.py();
    call(py, || {
        let t = json_text(text)?;
        Ok(py_value(py, json::parse(&t).map_err(|e| e.refuse(py))?)?)
    })
}

/// [`json_parse`] with a scale bound of the caller's.
#[pyfunction]
fn json_parse_with_max_scale(text: &Bound<'_, PyAny>, max_scale: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = text.py();
    call(py, || {
        let t = json_text(text)?;
        let scale = u32_arg(max_scale, "the scale bound")?;
        Ok(py_value(py, json::parse_with_max_scale(&t, scale).map_err(|e| e.refuse(py))?)?)
    })
}

/// A value's compact JSON, members in code-point order, if it is at most `max_bytes` long: `TooLarge` past it, and
/// `NotJsonRepresentable` for bytes, a non-terminating rational or an over-long number.
#[pyfunction]
fn json_render(value: &Bound<'_, PyAny>, max_bytes: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = value.py();
    call(py, || {
        let v = as_value(value)?;
        let (max, given) = limit(max_bytes)?;
        match py.detach(|| json::render_within(&v, max)) {
            Ok(s) => Ok(py_str(py, &s)),
            Err(RenderError::TooLarge(_)) => Err(too_large(py, given)),
            Err(RenderError::Canonical(e)) => Err(e.refuse(py)),
        }
    })
}

/// A value's commitment: `0x00 || b` for a byte string `b`, `0x01 || cbor(v)` for any other value.
#[pyfunction]
#[pyo3(name = "value_commitment")]
fn value_commitment_of(value: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = value.py();
    call(py, || {
        let v = as_value(value)?;
        let c = if v.weight() > DETACH_WEIGHT { py.detach(|| value_commitment(&v)) } else { value_commitment(&v) };
        Ok(py_bytes(py, &c))
    })
}

/// A hash algorithm by name (`SHA-256`, `SHA-512`, `SHA3-256`, `SHA3-512`, `Keccak-256`, `BLAKE2b-256`,
/// `BLAKE2b-512`): `UnknownAlgorithm` for any other text.
pub(crate) fn as_algorithm<'py>(obj: &Bound<'py, PyAny>) -> Result<HashAlgorithm, Fail<'py>> {
    let py = obj.py();
    let n = name(obj)?;
    HashAlgorithm::from_name(&n).map_err(|e| e.refuse(py))
}

fn as_tag<'py>(obj: &Bound<'py, PyAny>) -> Result<DomainTag, Fail<'py>> {
    let py = obj.py();
    let n = name(obj)?;
    DomainTag::new(&n).map_err(|e| e.refuse(py))
}

/// The algorithm's name, if it names one.
#[pyfunction]
fn hash_algorithm(name: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = name.py();
    call(py, || Ok(py_str(py, as_algorithm(name)?.name())))
}

/// The tag, if it is a domain tag (`^[a-z0-9]+(/[a-z0-9][a-z0-9._-]*)*/v[0-9]+$`).
#[pyfunction]
fn domain_tag(text: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = text.py();
    call(py, || Ok(py_str(py, as_tag(text)?.as_str())))
}

/// An algorithm's output size, block size and expansion limit, in bytes.
#[pyfunction]
fn hash_sizes(name: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = name.py();
    call(py, || {
        let a = as_algorithm(name)?;
        let sizes = [a.output_size(), a.block_size(), a.expansion_limit()];
        let parts =
            sizes.iter().map(|n| py_u64(py, u64::try_from(*n).unwrap_or(u64::MAX))).collect::<PyResult<Vec<_>>>()?;
        Ok(py_tuple(py, parts)?)
    })
}

/// The hash of `data`.
#[pyfunction]
fn digest(algorithm: &Bound<'_, PyAny>, data: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = data.py();
    call(py, || {
        let d = bytes(data)?;
        let a = as_algorithm(algorithm)?;
        Ok(py_bytes(py, &a.digest(&d)))
    })
}

/// The hash of the concatenation of `parts` (a list or tuple of bytes).
#[pyfunction]
fn hash_parts(algorithm: &Bound<'_, PyAny>, parts: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = parts.py();
    call(py, || {
        let objs = items(parts, "the parts")?;
        // read once each: a shared buffer once in all
        let mut reads = Reads::default();
        let held = objs.iter().map(|o| reads.read(o)).collect::<Result<Vec<_>, _>>()?;
        let a = as_algorithm(algorithm)?;
        let slices: Vec<&[u8]> = held.iter().map(|h| reads.get(h)).collect();
        Ok(py_bytes(py, &a.hash_parts(&slices)))
    })
}

/// `taggedHash(tag, message)`: `H(H(tag) || H(tag) || message)`.
#[pyfunction]
fn tagged_hash(
    algorithm: &Bound<'_, PyAny>,
    tag: &Bound<'_, PyAny>,
    message: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = message.py();
    call(py, || {
        let m = bytes(message)?;
        let a = as_algorithm(algorithm)?;
        let t = as_tag(tag)?;
        Ok(py_bytes(py, &a.tagged_hash(&t, &m)))
    })
}

/// The content hash of a value: `taggedHash(tag, cbor(value))`, streamed (O(weight) time, no encoding held).
#[pyfunction]
fn content_hash(algorithm: &Bound<'_, PyAny>, tag: &Bound<'_, PyAny>, value: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = value.py();
    call(py, || {
        let v = as_value(value)?;
        let a = as_algorithm(algorithm)?;
        let t = as_tag(tag)?;
        let h = if v.weight() > DETACH_WEIGHT { py.detach(|| a.content_hash(&t, &v)) } else { a.content_hash(&t, &v) };
        Ok(py_bytes(py, &h))
    })
}

/// RFC 9380 `expand_message_xmd` of the concatenation of `message` (a list or tuple of bytes) under `dst`.
#[pyfunction]
fn expand_message_xmd(
    algorithm: &Bound<'_, PyAny>,
    message: &Bound<'_, PyAny>,
    dst: &Bound<'_, PyAny>,
    length: &Bound<'_, PyAny>,
) -> PyResult<Py<PyAny>> {
    let py = dst.py();
    call(py, || {
        let objs = items(message, "the message parts")?;
        let mut reads = Reads::default();
        let held = objs.iter().map(|o| reads.read(o)).collect::<Result<Vec<_>, _>>()?;
        let d = bytes(dst)?;
        // a length past usize is past every limit, so it is refused as such
        let n = usize::try_from(u64_arg(length, "the output length")?).unwrap_or(usize::MAX);
        let a = as_algorithm(algorithm)?;
        let slices: Vec<&[u8]> = held.iter().map(|h| reads.get(h)).collect();
        Ok(py_bytes(py, &a.expand_message_xmd(&slices, &d, n).map_err(|e: HashError| e.refuse(py))?))
    })
}

/// A digest's bytes from its hex (an optional `0x`, then exactly twice the output size in hex digits).
#[pyfunction]
fn digest_from_hex(algorithm: &Bound<'_, PyAny>, text: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = text.py();
    call(py, || {
        let t = name(text)?;
        let a = as_algorithm(algorithm)?;
        Ok(py_bytes(py, &a.digest_from_hex(&t).map_err(|e| e.refuse(py))?))
    })
}

/// Text as an error holds it: its first 32 code points and its UTF-8 length (a surrogate counting 3, as U+FFFD).
#[pyfunction]
#[pyo3(name = "preview")]
fn preview_of(text: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = text.py();
    call(py, || Ok(preview(py, &Preview::of(&name(text)?))?))
}

fn build_error<'py>(py: Python<'py>, e: &BuildError) -> Fail<'py> {
    match e {
        BuildError::Canonical(c) => c.refuse(py),
        BuildError::TooManyCopies(n) => {
            match py_u64(py, *n).and_then(|n| instance(py, Module::Refusals, "TooManyCopies", &[("copies", n)])) {
                Ok(e) => Fail::Refused(e),
                Err(e) => Fail::Internal(e),
            }
        }
    }
}

/// A `built-value` recipe of the vector schemas: the leaf (canonical CBOR), then each step, a tuple `("wrap",
/// template, times)` (every text `"$"` in the template's value replaced by the value so far, `times` times over) or
/// `("copies", n)` (an array of `n` of it). Every occurrence is the one shared value.
#[pyfunction]
fn build_value(leaf: &Bound<'_, PyAny>, steps: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = leaf.py();
    call(py, || -> Answer<'_> {
        let leaf = bytes(leaf)?;
        let parts = items(steps, "the steps")?;
        let mut templates: Vec<(Option<&Bound<'_, PyAny>>, u64)> = Vec::new();
        // the first copies step the platform cannot allocate: refused where Rust would reach it (below)
        let mut unfit: Option<(usize, u64)> = None;
        // a step is two or three items: one holding more is refused by its count, unread
        let read: Vec<Vec<Bound<'_, PyAny>>> = parts
            .iter()
            .map(|s| Ok(items_within(s, "a step", 3)?.unwrap_or_default()))
            .collect::<Result<_, Fail<'_>>>()?;
        for step in &read {
            match step.as_slice() {
                [k, t, n] if name(k).is_ok_and(|k| k == "wrap") => {
                    // a template holding "$" in an array or map nests the value one level deeper each time, so
                    // past MAX_DEPTH + 1 times it is refused as too deep; any other template gives the same value
                    // from its first time on: the same result as the count asked for, in bounded work
                    let times = u64_arg(n, "the times")?.min(WRAP_BOUND);
                    bytes_len(t)?;
                    templates.push((Some(t), times));
                }
                [k, n] if name(k).is_ok_and(|k| k == "copies") => {
                    let copies = u64_arg(n, "the copies")?;
                    // the builder allocates the copies' array whole: one it cannot allocate is refused here, as a
                    // count past the platform's, rather than aborting the process in the allocator
                    let fits = usize::try_from(copies).is_ok_and(|c| Vec::<Value>::new().try_reserve_exact(c).is_ok());
                    if !fits && unfit.is_none() {
                        unfit = Some((templates.len(), copies));
                    }
                    templates.push((None, copies));
                }
                _ => {
                    return Err(mismatch(py, "expected (\"wrap\", template, times) or (\"copies\", n)"));
                }
            }
        }
        // every step checked, the templates are read once each (a shared buffer once in all)
        let mut reads = Reads::default();
        let held = templates
            .iter()
            .map(|(t, n)| Ok((t.map(|t| reads.read(t)).transpose()?, *n)))
            .collect::<Result<Vec<_>, Fail<'_>>>()?;
        let steps: Vec<Step<'_>> = held
            .iter()
            .map(|(t, n)| match t {
                Some(t) => Step::Wrap { template: reads.get(t), times: *n },
                None => Step::Copies(*n),
            })
            .collect();
        // a step Rust cannot reach is never refused: the steps before an unallocatable one are built first, so an
        // earlier refusal (a leaf or template that does not decode) comes first, as Rust gives it
        if let Some((at, copies)) = unfit {
            let before = steps.get(..at).unwrap_or(&[]);
            py.detach(|| build(&leaf, before)).map_err(|e| build_error(py, &e))?;
            return Err(build_error(py, &BuildError::TooManyCopies(copies)));
        }
        let v = py.detach(|| build(&leaf, &steps)).map_err(|e| build_error(py, &e))?;
        Ok(py_value(py, v)?)
    })
}

pub(crate) fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(cbor_encode, m)?)?;
    m.add_function(wrap_pyfunction!(cbor_encode_within, m)?)?;
    m.add_function(wrap_pyfunction!(cbor_encoded_length_within, m)?)?;
    m.add_function(wrap_pyfunction!(cbor_decode, m)?)?;
    m.add_function(wrap_pyfunction!(cbor_decode_with_limit, m)?)?;
    m.add_function(wrap_pyfunction!(json_parse, m)?)?;
    m.add_function(wrap_pyfunction!(json_parse_with_max_scale, m)?)?;
    m.add_function(wrap_pyfunction!(json_render, m)?)?;
    m.add_function(wrap_pyfunction!(value_commitment_of, m)?)?;
    m.add_function(wrap_pyfunction!(hash_algorithm, m)?)?;
    m.add_function(wrap_pyfunction!(domain_tag, m)?)?;
    m.add_function(wrap_pyfunction!(hash_sizes, m)?)?;
    m.add_function(wrap_pyfunction!(digest, m)?)?;
    m.add_function(wrap_pyfunction!(hash_parts, m)?)?;
    m.add_function(wrap_pyfunction!(tagged_hash, m)?)?;
    m.add_function(wrap_pyfunction!(content_hash, m)?)?;
    m.add_function(wrap_pyfunction!(expand_message_xmd, m)?)?;
    m.add_function(wrap_pyfunction!(digest_from_hex, m)?)?;
    m.add_function(wrap_pyfunction!(preview_of, m)?)?;
    m.add_function(wrap_pyfunction!(build_value, m)?)?;
    m.add("MAX_DEPTH", dilf4s_core::MAX_DEPTH)?;
    m.add("MAX_INTEGER_BITS", dilf4s_core::MAX_INTEGER_BITS)?;
    m.add("MAX_RATIONAL_BITS", dilf4s_core::MAX_RATIONAL_BITS)?;
    m.add("MAX_WEIGHT", dilf4s_core::MAX_WEIGHT)?;
    m.add("HASH_ALGORITHMS", pyo3::types::PyTuple::new(m.py(), HashAlgorithm::ALL.map(HashAlgorithm::name))?)?;
    Ok(())
}
