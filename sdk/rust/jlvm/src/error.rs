//! Every way a parse or an evaluation can fail (`JlvmError.scala`). Evaluation failures are reported with the gas they
//! used ([`crate::Outcome`]).

use alloc::string::String;
use core::fmt;

use crate::op::Op;

/// Where an operation-level error arose (docs/specs/jlvm.md section 3, `Site` in `Ast.scala`): an operation, or the
/// `var` read of a computed path. It is the error's normative `op` field, so it is always one of these names.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Site {
    /// An operation.
    Op(Op),
    /// The read of a computed `var` path.
    Var,
}

impl Site {
    /// The site's name as the conformance vectors record it: the operation's wire name, or `var`.
    pub const fn wire(self) -> &'static str {
        match self {
            Self::Op(op) => op.wire(),
            Self::Var => "var",
        }
    }
}

impl From<Op> for Site {
    fn from(op: Op) -> Self {
        Self::Op(op)
    }
}

impl fmt::Display for Site {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.wire())
    }
}

/// A parse or evaluation failure. [`JlvmError::kind`] names the variant as the conformance vectors do, and
/// [`JlvmError::site`] gives the normative `op` of the operation-level kinds; messages are not part of conformance.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JlvmError {
    /// Not a program of this language version: bad JSON, an unknown operation, a malformed `var`, an unbound `$name`.
    Malformed(String),
    /// An operation received a value of the wrong kind.
    Type {
        /// Where it arose.
        op: Site,
        /// What was wrong.
        message: String,
    },
    /// An operation received the wrong number of arguments.
    Arity {
        /// Where it arose.
        op: Site,
        /// What was wrong.
        message: String,
    },
    /// Division by zero, an exponent out of range, a non-terminating rational where text is required.
    Arithmetic {
        /// Where it arose.
        op: Site,
        /// What was wrong.
        message: String,
    },
    /// A value of the right kind outside the operation's domain: malformed hex, an empty separator, a wide integer.
    Domain {
        /// Where it arose.
        op: Site,
        /// What was wrong.
        message: String,
    },
    /// Malformed cryptographic input (a well-formed but invalid proof is `false`, not an error).
    Crypto {
        /// Where it arose.
        op: Site,
        /// What was wrong.
        message: String,
    },
    /// A resource bound: value depth or size, program depth or size.
    Limit(String),
    /// The gas limit was reached: the charge that would have exceeded it, and the limit.
    OutOfGas {
        /// The gas the evaluation would have used with the refused charge (saturating), summed only as far as it takes
        /// to pass the limit: informative, not part of conformance.
        required: u64,
        /// The limit.
        available: u64,
    },
}

impl JlvmError {
    /// The error's kind as the conformance vectors name it (`"Type"`, `"OutOfGas"`, ...).
    pub const fn kind(&self) -> &'static str {
        match self {
            Self::Malformed(_) => "Malformed",
            Self::Type { .. } => "Type",
            Self::Arity { .. } => "Arity",
            Self::Arithmetic { .. } => "Arithmetic",
            Self::Domain { .. } => "Domain",
            Self::Crypto { .. } => "Crypto",
            Self::Limit(_) => "Limit",
            Self::OutOfGas { .. } => "OutOfGas",
        }
    }

    /// Where an operation-level error arose: its normative `op`. `None` for `Malformed`, `Limit` and `OutOfGas`.
    pub const fn site(&self) -> Option<Site> {
        match self {
            Self::Type { op, .. }
            | Self::Arity { op, .. }
            | Self::Arithmetic { op, .. }
            | Self::Domain { op, .. }
            | Self::Crypto { op, .. } => Some(*op),
            Self::Malformed(_) | Self::Limit(_) | Self::OutOfGas { .. } => None,
        }
    }

    pub(crate) fn malformed(message: impl Into<String>) -> Self {
        Self::Malformed(message.into())
    }

    pub(crate) fn type_error(op: impl Into<Site>, message: impl Into<String>) -> Self {
        Self::Type { op: op.into(), message: message.into() }
    }

    pub(crate) fn arity(op: impl Into<Site>, message: impl Into<String>) -> Self {
        Self::Arity { op: op.into(), message: message.into() }
    }

    pub(crate) fn arithmetic(op: impl Into<Site>, message: impl Into<String>) -> Self {
        Self::Arithmetic { op: op.into(), message: message.into() }
    }

    pub(crate) fn domain(op: impl Into<Site>, message: impl Into<String>) -> Self {
        Self::Domain { op: op.into(), message: message.into() }
    }

    pub(crate) fn crypto(op: impl Into<Site>, message: impl Into<String>) -> Self {
        Self::Crypto { op: op.into(), message: message.into() }
    }

    pub(crate) fn limit(message: impl Into<String>) -> Self {
        Self::Limit(message.into())
    }
}

impl fmt::Display for JlvmError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Malformed(m) | Self::Limit(m) => write!(f, "{}: {m}", self.kind()),
            Self::Type { op, message }
            | Self::Arity { op, message }
            | Self::Arithmetic { op, message }
            | Self::Domain { op, message }
            | Self::Crypto { op, message } => write!(f, "{} in {op}: {message}", self.kind()),
            Self::OutOfGas { required, available } => {
                write!(f, "out of gas: {required} required, {available} available")
            }
        }
    }
}

impl core::error::Error for JlvmError {}
