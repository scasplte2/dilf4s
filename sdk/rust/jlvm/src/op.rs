//! The operations of language version 1, by wire name: [`Op`] is generated from `shared/schemas/jlvm.cddl`
//! (`jlvm-op`), with its `wire()`, `from_wire` (its own names only) and `ALL`. A program naming anything else does not
//! parse, so adding an operation in a later version never changes what an existing program means.

pub use dilf4s_core::generated::jlvm::JlvmOp as Op;

/// What the evaluator knows of each operation beyond its name (`Op` in `Ast.scala`).
pub trait OpClass: Copy {
    /// The operation a surface name denotes: its own name, or one of the aliases `getKey`, `setKey`, `deleteKey` and
    /// `size`, which the parser normalises so they never change a program's identity.
    fn from_name(name: &str) -> Option<Self>;

    /// The operations written with a single non-array argument to take that value's elements as their arguments
    /// (`{"+": {"values": x}}`). Whether an operation spreads is fixed by the syntax, never by an evaluated value.
    fn is_variadic(self) -> bool;

    /// The cryptographic operations, which the machine hands to a [`crate::CryptoBackend`] (`CryptoOps.scala`).
    fn is_crypto(self) -> bool;

    /// Whether the operation applies to evaluated arguments (`PureOps.apply` is defined): everything except the lazy
    /// and binding forms, the callbacks, the lookups and the cryptographic operations. `count` is also a callback when
    /// written with two arguments; the compiler decides that by syntax.
    fn is_strict(self) -> bool;
}

impl OpClass for Op {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "getKey" => Some(Self::Get),
            "setKey" => Some(Self::Set),
            "deleteKey" => Some(Self::Unset),
            "size" => Some(Self::Length),
            other => Self::from_wire(other),
        }
    }

    fn is_variadic(self) -> bool {
        matches!(self, Self::Add | Self::Mul | Self::Min | Self::Max | Self::Cat | Self::Merge)
    }

    fn is_crypto(self) -> bool {
        matches!(
            self,
            Self::Ed25519Verify
                | Self::Secp256k1Verify
                | Self::Bip340Verify
                | Self::Ecrecover
                | Self::BlsVerify
                | Self::BlsAggregateVerify
                | Self::Poseidon
                | Self::PmtVerify
                | Self::Groth16Verify
                | Self::EcvrfVerify
                | Self::Bn254Add
                | Self::Bn254Mul
                | Self::Bn254Pairing
                | Self::SchnorrVerify
                | Self::SigmaVerify
                | Self::MerkleVerify
                | Self::MerkleConsistencyVerify
                | Self::TrieVerify
                | Self::TriePrefixVerify
                | Self::TrieRangeVerify
        )
    }

    fn is_strict(self) -> bool {
        !self.is_crypto()
            && !matches!(
                self,
                Self::If
                    | Self::And
                    | Self::Or
                    | Self::Default
                    | Self::Let
                    | Self::Map
                    | Self::Filter
                    | Self::Reduce
                    | Self::All
                    | Self::Some
                    | Self::None
                    | Self::Find
                    | Self::Missing
                    | Self::MissingSome
            )
    }
}

/// Names that are not operations although they read like one, each with what to write instead (`Parser.Removed`).
pub(crate) fn removed(name: &str) -> Option<&'static str> {
    match name {
        "prove_dlog_verify" => Some("use a one-leaf sigma_verify over {\"type\": \"dlog\"} (docs/specs/sigma.md)"),
        "prove_dhtuple_verify" => {
            Some("use a one-leaf sigma_verify over {\"type\": \"dhtuple\"} (docs/specs/sigma.md)")
        }
        "mpt_verify" | "smt_verify" => Some("use trie_verify (docs/specs/trie.md)"),
        "mpt_prefix_verify" => Some("use trie_prefix_verify (docs/specs/trie.md)"),
        "noop" => Some("omit it: it does nothing"),
        _ => None,
    }
}

/// Names that may not head an array, which would read as a call: `["+", 1, 2]`, `["var", "a"]` (`Parser.callName`).
pub(crate) fn call_name(s: &str) -> bool {
    s == "var" || Op::from_name(s).is_some() || removed(s).is_some()
}

#[cfg(test)]
mod tests {
    use alloc::vec::Vec;

    use super::{Op, OpClass};
    use crate::error::JlvmError;

    /// The forms the compiler builds itself (`node::compile`): the lazy and binding forms, the callbacks and the lookups.
    const FORMS: [Op; 14] = [
        Op::If,
        Op::And,
        Op::Or,
        Op::Default,
        Op::Let,
        Op::Map,
        Op::Filter,
        Op::Reduce,
        Op::All,
        Op::Some,
        Op::None,
        Op::Find,
        Op::Missing,
        Op::MissingSome,
    ];

    /// The generated `ALL` is exactly the operations the evaluator dispatches: each is a form, strict or cryptographic,
    /// never two, and the strict and cryptographic dispatchers each have an arm for every one of theirs.
    #[test]
    fn every_operation_has_one_handler() {
        let backend = crate::Native::new().unwrap();
        for op in Op::ALL {
            let classes = [op.is_strict(), op.is_crypto(), FORMS.contains(&op)].into_iter().filter(|c| *c).count();
            assert_eq!(classes, 1, "{op:?} is in {classes} classes");
            if op.is_strict() {
                let r = crate::ops::apply(op, Vec::new());
                assert!(
                    !matches!(&r, Err(JlvmError::Malformed { message }) if message.ends_with("has no strict form")),
                    "{op:?} has no strict handler"
                );
            }
            if op.is_crypto() {
                let r = crate::crypto::apply(&backend, op, &[]);
                assert!(
                    !matches!(&r, Err(JlvmError::Crypto { message, .. }) if message == "not a cryptographic operation"),
                    "{op:?} has no cryptographic handler"
                );
            }
        }
        assert!(FORMS.iter().all(|f| Op::ALL.contains(f)));
    }
}
