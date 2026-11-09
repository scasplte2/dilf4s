//! Every way a parse or an evaluation can fail (`JlvmError.scala`). Evaluation failures are reported with the gas they
//! used ([`crate::Outcome`]). The error, its kinds, its [`Site`] and a `Crypto` error's [`CryptoCause`] are generated
//! from `shared/schemas/jlvm-errors.cddl`.

use alloc::string::String;

use crate::op::Op;

pub use dilf4s_core::generated::jlvm_errors::{
    JlvmCryptoCause as CryptoCause, JlvmCryptoCauseKind as CryptoCauseKind, JlvmError, JlvmErrorKind, JlvmSite as Site,
};

/// Where an error arose, as its `op` field holds it: an operation ([`Site::JlvmOp`]) or the `var` read of a computed
/// path ([`Site::Var`]).
pub trait AtSite {
    /// The site.
    fn site(self) -> Site;
}

impl AtSite for Site {
    fn site(self) -> Site {
        self
    }
}

impl AtSite for Op {
    fn site(self) -> Site {
        Site::JlvmOp(self)
    }
}

/// What the evaluator asks of an error beyond its generated fields.
pub trait ErrorSite {
    /// Where an operation-level error arose: its normative `op`. `None` for `Malformed`, `Limit` and `OutOfGas`.
    fn site(&self) -> Option<Site>;
}

impl ErrorSite for JlvmError {
    fn site(&self) -> Option<Site> {
        match self {
            Self::Type { op, .. }
            | Self::Arity { op, .. }
            | Self::Arithmetic { op, .. }
            | Self::Domain { op, .. }
            | Self::Crypto { op, .. } => Some(*op),
            Self::Malformed { .. } | Self::Limit { .. } | Self::OutOfGas { .. } => None,
        }
    }
}

/// The evaluator's constructors of each kind.
pub(crate) trait Raise {
    fn malformed(message: impl Into<String>) -> Self;
    fn type_error(op: impl AtSite, message: impl Into<String>) -> Self;
    fn arity(op: impl AtSite, message: impl Into<String>) -> Self;
    fn arithmetic(op: impl AtSite, message: impl Into<String>) -> Self;
    fn domain(op: impl AtSite, message: impl Into<String>) -> Self;
    fn crypto(op: impl AtSite, cause: Option<CryptoCause>, message: impl Into<String>) -> Self;
    fn limit(message: impl Into<String>) -> Self;
}

impl Raise for JlvmError {
    fn malformed(message: impl Into<String>) -> Self {
        Self::Malformed { message: message.into() }
    }

    fn type_error(op: impl AtSite, message: impl Into<String>) -> Self {
        Self::Type { op: op.site(), message: message.into() }
    }

    fn arity(op: impl AtSite, message: impl Into<String>) -> Self {
        Self::Arity { op: op.site(), message: message.into() }
    }

    fn arithmetic(op: impl AtSite, message: impl Into<String>) -> Self {
        Self::Arithmetic { op: op.site(), message: message.into() }
    }

    fn domain(op: impl AtSite, message: impl Into<String>) -> Self {
        Self::Domain { op: op.site(), message: message.into() }
    }

    fn crypto(op: impl AtSite, cause: Option<CryptoCause>, message: impl Into<String>) -> Self {
        Self::Crypto { op: op.site(), cause, message: message.into() }
    }

    fn limit(message: impl Into<String>) -> Self {
        Self::Limit { message: message.into() }
    }
}
