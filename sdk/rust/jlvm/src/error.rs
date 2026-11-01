//! Every way a parse or an evaluation can fail (`JlvmError.scala`). Evaluation failures are reported with the gas they
//! used ([`crate::Outcome`]).

use alloc::string::String;
use core::fmt;

/// A parse or evaluation failure. [`JlvmError::kind`] names the variant as the conformance vectors do; messages are not
/// part of conformance.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum JlvmError {
    /// Not a program of this language version: bad JSON, an unknown operation, a malformed `var`, an unbound `$name`.
    Malformed(String),
    /// An operation received a value of the wrong kind.
    Type {
        /// The operation's wire name.
        op: &'static str,
        /// What was wrong.
        message: String,
    },
    /// An operation received the wrong number of arguments.
    Arity {
        /// The operation's wire name.
        op: &'static str,
        /// What was wrong.
        message: String,
    },
    /// Division by zero, an exponent out of range, a non-terminating rational where text is required.
    Arithmetic {
        /// The operation's wire name.
        op: &'static str,
        /// What was wrong.
        message: String,
    },
    /// A value of the right kind outside the operation's domain: malformed hex, an empty separator, a wide integer.
    Domain {
        /// The operation's wire name.
        op: &'static str,
        /// What was wrong.
        message: String,
    },
    /// Malformed cryptographic input (a well-formed but invalid proof is `false`, not an error).
    Crypto {
        /// The operation's wire name.
        op: &'static str,
        /// What was wrong.
        message: String,
    },
    /// A resource bound: value depth or size, program depth or size.
    Limit(String),
    /// The gas limit was reached: the charge that would have exceeded it, and the limit.
    OutOfGas {
        /// The gas the evaluation would have used with the refused charge (saturating).
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

    pub(crate) fn malformed(message: impl Into<String>) -> Self {
        Self::Malformed(message.into())
    }

    pub(crate) fn type_error(op: &'static str, message: impl Into<String>) -> Self {
        Self::Type { op, message: message.into() }
    }

    pub(crate) fn arity(op: &'static str, message: impl Into<String>) -> Self {
        Self::Arity { op, message: message.into() }
    }

    pub(crate) fn arithmetic(op: &'static str, message: impl Into<String>) -> Self {
        Self::Arithmetic { op, message: message.into() }
    }

    pub(crate) fn domain(op: &'static str, message: impl Into<String>) -> Self {
        Self::Domain { op, message: message.into() }
    }

    pub(crate) fn crypto(op: &'static str, message: impl Into<String>) -> Self {
        Self::Crypto { op, message: message.into() }
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
