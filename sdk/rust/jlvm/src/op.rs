//! The operations of language version 1, by wire name (`Ast.scala` `Op`). A program naming anything else does not
//! parse, so adding an operation in a later version never changes what an existing program means.

macro_rules! ops {
    ($($(#[$doc:meta])* $name:ident => $wire:literal,)*) => {
        /// An operation of language version 1.
        #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum Op {
            $($(#[$doc])* $name,)*
        }

        impl Op {
            /// Every operation, in declaration order.
            pub const ALL: &'static [Self] = &[$(Self::$name,)*];

            /// The operation's own name (the canonical form records only this).
            pub const fn wire(self) -> &'static str {
                match self {
                    $(Self::$name => $wire,)*
                }
            }

            /// The operation with this own name; aliases are not accepted (the canonical form).
            pub fn from_canonical(name: &str) -> Option<Self> {
                match name {
                    $($wire => Some(Self::$name),)*
                    _ => None,
                }
            }
        }
    };
}

ops! {
    /// `if`
    If => "if",
    /// `and`
    And => "and",
    /// `or`
    Or => "or",
    /// `!`
    Not => "!",
    /// `!!`
    Truthy => "!!",
    /// `default`
    Default => "default",
    /// `let`
    Let => "let",
    /// `==`
    Eq => "==",
    /// `!=`
    Ne => "!=",
    /// `===`
    StrictEq => "===",
    /// `!==`
    StrictNe => "!==",
    /// `<`
    Lt => "<",
    /// `<=`
    Le => "<=",
    /// `>`
    Gt => ">",
    /// `>=`
    Ge => ">=",
    /// `+`
    Add => "+",
    /// `-`
    Sub => "-",
    /// `*`
    Mul => "*",
    /// `/`
    Div => "/",
    /// `%`
    Mod => "%",
    /// `min`
    Min => "min",
    /// `max`
    Max => "max",
    /// `abs`
    Abs => "abs",
    /// `round`
    Round => "round",
    /// `round_half_up`
    RoundHalfUp => "round_half_up",
    /// `floor`
    Floor => "floor",
    /// `ceil`
    Ceil => "ceil",
    /// `pow`
    Pow => "pow",
    /// `map`
    Map => "map",
    /// `filter`
    Filter => "filter",
    /// `reduce`
    Reduce => "reduce",
    /// `all`
    All => "all",
    /// `some`
    Some => "some",
    /// `none`
    None => "none",
    /// `find`
    Find => "find",
    /// `count`
    Count => "count",
    /// `merge`
    Merge => "merge",
    /// `in`
    In => "in",
    /// `intersect`
    Intersect => "intersect",
    /// `unique`
    Unique => "unique",
    /// `slice`
    Slice => "slice",
    /// `reverse`
    Reverse => "reverse",
    /// `flatten`
    Flatten => "flatten",
    /// `length`
    Length => "length",
    /// `cat`
    Cat => "cat",
    /// `substr`
    Substr => "substr",
    /// `lower`
    Lower => "lower",
    /// `upper`
    Upper => "upper",
    /// `join`
    Join => "join",
    /// `split`
    Split => "split",
    /// `trim`
    Trim => "trim",
    /// `startsWith`
    StartsWith => "startsWith",
    /// `endsWith`
    EndsWith => "endsWith",
    /// `keys`
    Keys => "keys",
    /// `values`
    Values => "values",
    /// `entries`
    Entries => "entries",
    /// `get`
    Get => "get",
    /// `has`
    Has => "has",
    /// `set`
    Set => "set",
    /// `unset`
    Unset => "unset",
    /// `exists`
    Exists => "exists",
    /// `missing`
    Missing => "missing",
    /// `missing_some`
    MissingSome => "missing_some",
    /// `typeof`
    TypeOf => "typeof",
    /// `hex`
    Hex => "hex",
    /// `unhex`
    Unhex => "unhex",
    /// `hex_to_int`
    HexToInt => "hex_to_int",
    /// `bytes_to_int`
    BytesToInt => "bytes_to_int",
    /// `int_to_bytes`
    IntToBytes => "int_to_bytes",
    /// `sha256`
    Sha256 => "sha256",
    /// `sha3_256`
    Sha3_256 => "sha3_256",
    /// `keccak256`
    Keccak256 => "keccak256",
    /// `blake2b256`
    Blake2b256 => "blake2b256",
    /// `ed25519_verify`
    Ed25519Verify => "ed25519_verify",
    /// `secp256k1_verify`
    Secp256k1Verify => "secp256k1_verify",
    /// `bip340_verify`
    Bip340Verify => "bip340_verify",
    /// `ecrecover`
    Ecrecover => "ecrecover",
    /// `bls_verify`
    BlsVerify => "bls_verify",
    /// `bls_aggregate_verify`
    BlsAggregateVerify => "bls_aggregate_verify",
    /// `poseidon`
    Poseidon => "poseidon",
    /// `pmt_verify`
    PmtVerify => "pmt_verify",
    /// `groth16_verify`
    Groth16Verify => "groth16_verify",
    /// `ecvrf_verify`
    EcvrfVerify => "ecvrf_verify",
    /// `bn254_add`
    Bn254Add => "bn254_add",
    /// `bn254_mul`
    Bn254Mul => "bn254_mul",
    /// `bn254_pairing`
    Bn254Pairing => "bn254_pairing",
    /// `schnorr_verify`
    SchnorrVerify => "schnorr_verify",
    /// `sigma_verify`
    SigmaVerify => "sigma_verify",
    /// `merkle_verify`
    MerkleVerify => "merkle_verify",
    /// `merkle_consistency_verify`
    MerkleConsistencyVerify => "merkle_consistency_verify",
    /// `trie_verify`
    TrieVerify => "trie_verify",
    /// `trie_prefix_verify`
    TriePrefixVerify => "trie_prefix_verify",
    /// `trie_range_verify`
    TrieRangeVerify => "trie_range_verify",
}

impl Op {
    /// The operation a surface name denotes: its own name, or one of the aliases `getKey`, `setKey`, `deleteKey` and
    /// `size`, which the parser normalises so they never change a program's identity.
    pub fn from_wire(name: &str) -> Option<Self> {
        match name {
            "getKey" => Some(Self::Get),
            "setKey" => Some(Self::Set),
            "deleteKey" => Some(Self::Unset),
            "size" => Some(Self::Length),
            other => Self::from_canonical(other),
        }
    }

    /// The operations written with a single non-array argument to take that value's elements as their arguments
    /// (`{"+": {"values": x}}`). Whether an operation spreads is fixed by the syntax, never by an evaluated value.
    pub const fn is_variadic(self) -> bool {
        matches!(self, Self::Add | Self::Mul | Self::Min | Self::Max | Self::Cat | Self::Merge)
    }

    /// The cryptographic operations, which the machine hands to a [`crate::CryptoBackend`] (`CryptoOps.scala`).
    pub const fn is_crypto(self) -> bool {
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

    /// Whether the operation applies to evaluated arguments (`PureOps.apply` is defined): everything except the lazy
    /// and binding forms, the callbacks, the lookups and the cryptographic operations. `count` is also a callback when
    /// written with two arguments; the compiler decides that by syntax.
    pub const fn is_strict(self) -> bool {
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
    s == "var" || Op::from_wire(s).is_some() || removed(s).is_some()
}
