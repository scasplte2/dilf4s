//! `dilf4s.jlvm`: the JLVM (dilf4s-jlvm) over its native cryptographic backend.
//!
//! Evaluation releases the GIL: it reads only Rust values (the program, the data and the bindings, all `Send + Sync`
//! and immutable), so other Python threads run meanwhile and none can change what it reads.

use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;
use std::sync::OnceLock;

use dilf4s_core::hash::HashAlgorithm;
use dilf4s_jlvm::{
    Compiled, Config, CryptoFailure, Gas, GasLimit, GasSchedule, Jlvm, JlvmError, Native, Outcome, Program, Value,
    Warning, decode, from_canonical, parse, parse_value,
};
use pyo3::prelude::*;
use pyo3::pyclass::CompareOp;

use crate::boundary::{
    Answer, Fail, Module, bytes, call, entries, gas_limit_arg, instance, mismatch, py_bool, py_bytes, py_tuple, py_u64,
    text, value,
};
use crate::core_api::as_algorithm;
use crate::errors::{Refusal, preview};
use crate::value::py_value;

static JLVM: OnceLock<Result<Jlvm<Native>, CryptoFailure>> = OnceLock::new();

/// The evaluator, built once (its backend decodes SP1's Groth16 verifying key).
fn jlvm() -> PyResult<&'static Jlvm<Native>> {
    JLVM.get_or_init(|| Native::new().map(Jlvm::new))
        .as_ref()
        .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(alloc::format!("the JLVM backend failed: {e:?}")))
}

/// A JLVM program: language version 1, bounded, with one canonical form.
#[pyclass(frozen, module = "dilf4s._native", name = "Program")]
pub(crate) struct PyProgram(Program);

/// A program whose special forms are checked, ready to evaluate any number of times.
#[pyclass(frozen, module = "dilf4s._native", name = "Compiled")]
pub(crate) struct PyCompiled(Compiled);

fn py_program(py: Python<'_>, p: Program) -> PyResult<Bound<'_, PyAny>> {
    Bound::new(py, PyProgram(p)).map(Bound::into_any)
}

fn refuse<'py>(py: Python<'py>, e: &JlvmError) -> Fail<'py> {
    e.refuse(py)
}

/// Program or data text: a surrogate code point, which no JSON text this JLVM reads holds, is `Malformed`.
fn source<'a, 'py>(obj: &'a Bound<'py, PyAny>) -> Result<&'a str, Fail<'py>> {
    let py = obj.py();
    text(obj)?
        .ok_or_else(|| refuse(py, &JlvmError::Malformed { message: String::from("text with an unpaired surrogate") }))
}

#[pymethods]
impl PyProgram {
    /// The canonical form (`shared/schemas/jlvm.cddl`).
    fn canonical(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        call(py, || Ok(py_value(py, self.0.canonical().map_err(|e| refuse(py, &e))?)?))
    }

    /// The canonical CBOR.
    fn encode(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        call(py, || Ok(py_bytes(py, &self.0.encode().map_err(|e| refuse(py, &e))?)))
    }

    /// The identity: the tagged hash (`dilf4s/jlvm/program/v1`) of the canonical CBOR, SHA-256 unless named.
    #[pyo3(signature = (algorithm = None))]
    fn id(&self, py: Python<'_>, algorithm: Option<&Bound<'_, PyAny>>) -> PyResult<Py<PyAny>> {
        call(py, || {
            let a = algorithm.map_or(Ok(HashAlgorithm::Sha256), as_algorithm)?;
            Ok(py_bytes(py, &self.0.id(a).map_err(|e| refuse(py, &e))?))
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

    /// The first 8 bytes of the identity: O(the program's size), which is bounded.
    fn __hash__(&self) -> u64 {
        let id = self.0.id(HashAlgorithm::Sha256).unwrap_or_default();
        id.get(..8).and_then(|b| <[u8; 8]>::try_from(b).ok()).map_or(0, u64::from_le_bytes) >> 1
    }

    fn __repr__(&self) -> String {
        let id = self.0.id(HashAlgorithm::Sha256).map(|b| dilf4s_core::hex::encode(&b)).unwrap_or_default();
        alloc::format!("Program(id={id})")
    }
}

#[pymethods]
impl PyCompiled {
    /// The program.
    #[getter]
    fn program<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        py_program(py, self.0.program().clone())
    }
}

/// Parses a program from its JSON surface text.
#[pyfunction]
fn jlvm_parse(source_text: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = source_text.py();
    call(py, || Ok(py_program(py, parse(source(source_text)?).map_err(|e| refuse(py, &e))?)?))
}

/// Parses input data or a host binding from JSON surface text: the strict parser, then `0x`-hex strings as bytes.
#[pyfunction]
fn jlvm_parse_value(source_text: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = source_text.py();
    call(py, || Ok(py_value(py, parse_value(source(source_text)?).map_err(|e| refuse(py, &e))?)?))
}

/// The program a canonical form (a value) denotes, its refusals in the normative order.
#[pyfunction]
fn jlvm_from_canonical(form: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = form.py();
    call(py, || {
        let v = value(form)?;
        Ok(py_program(py, py.detach(|| from_canonical(&v)).map_err(|e| refuse(py, &e))?)?)
    })
}

/// The program a canonical CBOR encoding denotes.
#[pyfunction]
fn jlvm_decode(data: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = data.py();
    call(py, || {
        let b = bytes(data)?;
        Ok(py_program(py, py.detach(|| decode(&b)).map_err(|e| refuse(py, &e))?)?)
    })
}

/// Checks a program's special forms, once, for any number of evaluations.
#[pyfunction]
fn jlvm_compile(program: &Bound<'_, PyAny>) -> PyResult<Py<PyAny>> {
    let py = program.py();
    call(py, || {
        let p = program.cast::<PyProgram>().map_err(|_| mismatch(py, "expected a dilf4s.jlvm.Program"))?;
        let c = Compiled::new(p.get().0.clone()).map_err(|e| refuse(py, &e))?;
        Ok(Bound::new(py, PyCompiled(c))?.into_any())
    })
}

/// What to evaluate: a program, compiled on the way, or one compiled already.
enum Target<'a> {
    Program(&'a Program),
    Compiled(&'a Compiled),
}

/// `Ok(value)` or `Err(error)` as a field of a Python object.
fn result<'py>(py: Python<'py>, r: &Result<Value, JlvmError>) -> PyResult<Bound<'py, PyAny>> {
    match r {
        Ok(v) => instance(py, Module::Result, "Ok", &[("value", py_value(py, v.clone())?)]),
        Err(e) => instance(py, Module::Result, "Err", &[("error", e.py(py)?)]),
    }
}

fn warning<'py>(py: Python<'py>, w: &Warning) -> PyResult<Bound<'py, PyAny>> {
    match w {
        Warning::AmbiguousPath(p) => instance(py, Module::JlvmTypes, "AmbiguousPath", &[("path", preview(py, p)?)]),
    }
}

fn outcome<'py>(py: Python<'py>, o: &Outcome) -> PyResult<Bound<'py, PyAny>> {
    let warnings = o.warnings.iter().map(|w| warning(py, w)).collect::<PyResult<Vec<_>>>()?;
    instance(
        py,
        Module::JlvmTypes,
        "Outcome",
        &[
            ("gas_used", py_u64(py, o.gas_used.value())?),
            ("result", result(py, &o.result)?),
            ("warnings", py_tuple(py, warnings)?),
        ],
    )
}

/// Evaluates a program (or a compiled one) against `data` (a `Value`), with host `bindings` (a `dict` of names to
/// `Value`s, read as `$name`) and a gas limit (2**24 unless given), on the latest gas schedule. The outcome is the gas
/// used, whether it succeeded or failed (the whole limit when out of gas), the value or the error, and the warnings.
/// A program that does not compile uses no gas. The GIL is released while it runs.
#[pyfunction]
#[pyo3(signature = (program, data, bindings = None, gas_limit = None))]
fn jlvm_evaluate(
    program: &Bound<'_, PyAny>,
    data: &Bound<'_, PyAny>,
    bindings: Option<&Bound<'_, PyAny>>,
    gas_limit: Option<&Bound<'_, PyAny>>,
) -> PyResult<Py<PyAny>> {
    let py = program.py();
    call(py, || -> Answer<'_> {
        let p = program.cast::<PyProgram>().ok();
        let c = program.cast::<PyCompiled>().ok();
        let target = match (&p, &c) {
            (Some(p), _) => Target::Program(&p.get().0),
            (None, Some(c)) => Target::Compiled(&c.get().0),
            (None, None) => return Err(mismatch(py, "expected a dilf4s.jlvm.Program or Compiled")),
        };
        let data = value(data)?;
        let mut env = BTreeMap::new();
        if let Some(b) = bindings.filter(|b| !b.is_none()) {
            for (k, v) in entries(b, "the bindings")? {
                let name = text(&k)
                    .map_err(|_| mismatch(py, "expected bindings with str names"))?
                    .ok_or_else(|| mismatch(py, "expected binding names without surrogates"))?;
                env.insert(String::from(name), value(&v)?);
            }
        }
        let limit = match gas_limit.filter(|g| !g.is_none()).map(gas_limit_arg).transpose()? {
            None => GasLimit::DEFAULT.0.value(),
            Some(Ok(limit)) => limit,
            // a limit Rust's u64 cannot hold is refused as one past 2**53 - 1 is: Limit, with no gas used
            Some(Err(message)) => {
                let refused =
                    Outcome { gas_used: Gas::new(0), result: Err(JlvmError::Limit { message }), warnings: Vec::new() };
                return Ok(outcome(py, &refused)?);
            }
        };
        let config = Config { limit: GasLimit(Gas::new(limit)), schedule: GasSchedule::LATEST };
        let jlvm = jlvm()?;
        let o = py.detach(|| match target {
            Target::Program(p) => jlvm.evaluate(p, &data, &env, &config),
            Target::Compiled(c) => jlvm.evaluate_compiled(c, &data, &env, &config),
        });
        Ok(outcome(py, &o)?)
    })
}

pub(crate) fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PyProgram>()?;
    m.add_class::<PyCompiled>()?;
    m.add_function(wrap_pyfunction!(jlvm_parse, m)?)?;
    m.add_function(wrap_pyfunction!(jlvm_parse_value, m)?)?;
    m.add_function(wrap_pyfunction!(jlvm_from_canonical, m)?)?;
    m.add_function(wrap_pyfunction!(jlvm_decode, m)?)?;
    m.add_function(wrap_pyfunction!(jlvm_compile, m)?)?;
    m.add_function(wrap_pyfunction!(jlvm_evaluate, m)?)?;
    m.add("JLVM_VERSION", dilf4s_jlvm::VERSION)?;
    m.add("JLVM_GAS_SCHEDULE", GasSchedule::LATEST.version)?;
    m.add("JLVM_DEFAULT_GAS_LIMIT", GasLimit::DEFAULT.0.value())?;
    m.add("JLVM_MAX_GAS_LIMIT", GasLimit::MAX.0.value())?;
    m.add("JLVM_MAX_WARNINGS", dilf4s_jlvm::MAX_WARNINGS)?;
    m.add("JLVM_PROGRAM_TAG", dilf4s_jlvm::PROGRAM_TAG)?;
    Ok(())
}
