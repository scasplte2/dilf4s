//! The dilf4s JLVM: a strict, deterministic, gas-metered expression language for ledgers, written in `JsonLogic`
//! syntax (language version 1, gas schedule version 1).
//!
//! The normative specification is `docs/specs/jlvm.md`; the conformance vectors `shared/vectors/jlvm_v1.json` are part
//! of it, and the Scala implementation (`jlvm/`) is the reference. A result is a function of the program, the input
//! data, the host bindings, the language version and the gas schedule; an evaluation reports the gas it used whether it
//! succeeds or fails.
//!
//! - [`parse`] reads a program's JSON surface into a [`Program`], bounded in size and depth, with exactly one canonical
//!   form; [`Compiled::new`] checks its special forms.
//! - [`Jlvm::evaluate`] runs it on an explicit continuation stack, charging every unit of work before doing it.
//! - The cryptographic operations go through a [`CryptoBackend`]; [`Native`] implements it on `dilf4s-signing` and
//!   `dilf4s-proofs`.
//!
//! The crate is `no_std` (it needs `alloc`), pure Rust, and never panics on any input. Nothing it does recurses on
//! program or data structure beyond the bounds those are checked against first (see the module docs).

#![no_std]
#![forbid(unsafe_code)]
#![allow(
    clippy::many_single_char_names,
    reason = "operations name their arguments as the reference and the specification do (k, m, s, a, b, ...)"
)]

extern crate alloc;

mod ast;
mod crypto;
mod error;
mod gas;
mod machine;
mod native;
mod node;
mod op;
mod ops;
mod ratio;
mod surface;
mod values;

use alloc::collections::BTreeMap;
use alloc::string::String;
use alloc::vec::Vec;

pub use ast::{MAX_DEPTH, MAX_SIZE, MAX_SURFACE_DEPTH, PROGRAM_TAG, Program, VERSION};
pub use crypto::{
    CryptoBackend, CryptoFailure, KeyMode, MAX_FIXED_KEY_BYTES, MAX_VARIABLE_KEY_BYTES, MerkleProfile, TrieEntry,
};
pub use dilf4s_core::Value;
pub use error::JlvmError;
pub use gas::{Gas, GasLimit, GasSchedule, OpCost, Rate, SizeUnit};
pub use machine::{MAX_WARNINGS, Warning};
pub use native::Native;
pub use op::Op;
pub use surface::{parse, parse_value};
pub use values::{MAX_NUMBER_BITS, MAX_WEIGHT};

/// How to evaluate: the gas limit and the schedule.
#[derive(Clone, Copy, Debug)]
pub struct Config {
    /// The gas limit.
    pub limit: GasLimit,
    /// The gas schedule.
    pub schedule: GasSchedule,
}

impl Default for Config {
    fn default() -> Self {
        Self { limit: GasLimit::DEFAULT, schedule: GasSchedule::LATEST }
    }
}

/// The result of an evaluation: the gas it used, whether it succeeded or failed; the value or the error; and the
/// warnings. Out of gas, the whole limit is used.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Outcome {
    /// The gas used.
    pub gas_used: Gas,
    /// The value, or the error.
    pub result: Result<Value, JlvmError>,
    /// Lint warnings, distinct, at most [`MAX_WARNINGS`].
    pub warnings: Vec<Warning>,
}

/// A program checked for evaluation: its special forms are well-formed.
#[derive(Debug)]
pub struct Compiled {
    program: Program,
    root: node::Node,
}

impl Compiled {
    /// Checks a program's special forms (`if`, `let`, the callbacks), its `let` names, and its spreads.
    pub fn new(program: Program) -> Result<Self, JlvmError> {
        node::compile(&program).map(|root| Self { program, root })
    }

    /// The program.
    pub const fn program(&self) -> &Program {
        &self.program
    }
}

/// The JLVM, over a cryptographic backend.
#[derive(Clone, Debug, Default)]
pub struct Jlvm<B> {
    backend: B,
}

impl<B: CryptoBackend> Jlvm<B> {
    /// An evaluator whose cryptographic operations run on `backend`.
    pub const fn new(backend: B) -> Self {
        Self { backend }
    }

    /// The backend.
    pub const fn backend(&self) -> &B {
        &self.backend
    }

    /// Evaluates `compiled` against `data`. `env` holds the host bindings, read as `$name`; `let` and callback bindings
    /// shadow them.
    pub fn evaluate_compiled(
        &self,
        compiled: &Compiled,
        data: &Value,
        env: &BTreeMap<String, Value>,
        config: &Config,
    ) -> Outcome {
        self.run(&compiled.root, data, env, config)
    }

    fn run(&self, root: &node::Node, data: &Value, env: &BTreeMap<String, Value>, config: &Config) -> Outcome {
        let ran = machine::Machine::run(config.schedule, &self.backend, root, data, env, config.limit.0);
        Outcome { gas_used: ran.used, result: ran.result, warnings: ran.warnings }
    }

    /// Compiles and evaluates, borrowing the program; a program that does not compile uses no gas.
    pub fn evaluate(&self, program: &Program, data: &Value, env: &BTreeMap<String, Value>, config: &Config) -> Outcome {
        match node::compile(program) {
            Ok(root) => self.run(&root, data, env, config),
            Err(e) => Outcome { gas_used: Gas::ZERO, result: Err(e), warnings: Vec::new() },
        }
    }
}
