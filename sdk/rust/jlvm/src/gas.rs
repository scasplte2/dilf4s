//! Gas: amounts, the versioned schedule and how arguments are measured (`Gas.scala`).

use dilf4s_core::Value;

use crate::op::Op;
use crate::values;

/// The largest amount: the reference's `Long.MaxValue`, at which every amount saturates.
const MAX: u64 = 0x7fff_ffff_ffff_ffff;

/// An amount of gas: non-negative, saturating at `2^63 - 1` instead of overflowing, so a pathological cost can only
/// ever exceed the limit, never wrap around below it.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Gas(u64);

impl Gas {
    /// No gas.
    pub const ZERO: Self = Self(0);
    /// The saturation point.
    pub const MAX: Self = Self(MAX);

    /// `amount`, saturated.
    pub const fn new(amount: u64) -> Self {
        Self(if amount > MAX { MAX } else { amount })
    }

    /// The amount.
    pub const fn value(self) -> u64 {
        self.0
    }

    /// The saturating sum.
    #[must_use]
    pub const fn plus(self, other: Self) -> Self {
        Self::new(self.0.saturating_add(other.0))
    }

    /// The saturating product with a count.
    #[must_use]
    pub const fn times(self, n: u64) -> Self {
        Self::new(self.0.saturating_mul(n))
    }
}

/// How an argument is measured.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SizeUnit {
    /// Nothing.
    None,
    /// Top-level entries of arrays and objects (1 for anything else), for operations that touch only the outermost
    /// collection.
    Elements,
    /// The value's weight, for operations that read content.
    Weight,
    /// For a number, the square of its 64-bit limbs, numerator and denominator together; 0 for anything else.
    Limbs2,
    /// [`SizeUnit::Limbs2`] for a non-integer rational, 0 for an integer.
    RationalLimbs2,
    /// For text used as a key, `floor(length / 32)` in UTF-16 units; 0 for anything else.
    Key,
}

impl SizeUnit {
    /// One argument's size in this unit (`GasSchedule.units`).
    pub fn units(self, v: &Value) -> u64 {
        match self {
            Self::None => 0,
            Self::Elements => values::elements(v),
            Self::Weight => v.weight(),
            Self::Limbs2 => limbs(v, true),
            Self::RationalLimbs2 => limbs(v, false),
            Self::Key => values::key_blocks(v),
        }
    }
}

fn limbs(v: &Value, integers: bool) -> u64 {
    let n = match v {
        Value::Int(i) if integers => i.bit_length() / 64,
        Value::Rational(r) => (r.numerator_bit_length() / 64).saturating_add(r.denominator_bit_length() / 64),
        _ => 0,
    };
    n.saturating_mul(n)
}

/// A charge per unit of an argument's size.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Rate {
    /// Gas per unit.
    pub per_unit: u64,
    /// The unit.
    pub unit: SizeUnit,
}

/// What an operation costs: a base charge, plus a charge per unit of each argument's size. Every argument is charged at
/// the operation's rate, unless `by_argument` gives its position a rate of its own; `extra` rates are charged on every
/// argument besides.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OpCost {
    /// The base charge, taken before the arguments are evaluated.
    pub base: u64,
    /// Gas per unit of each argument.
    pub per_unit: u64,
    /// The unit.
    pub unit: SizeUnit,
    /// Positions charged at a rate of their own.
    pub by_argument: Option<(usize, Rate)>,
    /// A further rate charged on every argument.
    pub extra: Option<Rate>,
}

impl OpCost {
    const fn new(base: u64, per_unit: u64, unit: SizeUnit) -> Self {
        Self { base, per_unit, unit, by_argument: None, extra: None }
    }

    const fn at(self, position: usize, per_unit: u64, unit: SizeUnit) -> Self {
        Self { by_argument: Some((position, Rate { per_unit, unit })), ..self }
    }

    const fn plus(self, per_unit: u64, unit: SizeUnit) -> Self {
        Self { extra: Some(Rate { per_unit, unit }), ..self }
    }

    /// The rate of argument `position`.
    pub fn rate(&self, position: usize) -> Rate {
        match self.by_argument {
            Some((p, r)) if p == position => r,
            _ => Rate { per_unit: self.per_unit, unit: self.unit },
        }
    }
}

/// A gas schedule: versioned data. A result is a function of the program, the input, the language version and the
/// schedule version, so every evaluation names its schedule.
#[derive(Clone, Copy, Debug)]
pub struct GasSchedule {
    /// The schedule version.
    pub version: u32,
    costs: fn(Op) -> OpCost,
    /// Each literal node.
    pub literal: Gas,
    /// Each item of an array or map expression, each condition `if` tests, each `let` insertion.
    pub element: Gas,
    /// Each variable read.
    pub var_access: Gas,
    /// Each path segment of a read.
    pub per_path_segment: Gas,
    /// Each run of a callback body.
    pub callback_run: Gas,
    /// Per unit of `(4 + floor(bits / 64))^2` of each number `cat` and `join` render.
    pub rendering: Gas,
}

impl GasSchedule {
    /// Version 1 (`GasSchedule.V1`).
    pub const V1: Self = Self {
        version: 1,
        costs: v1,
        literal: Gas::new(15),
        element: Gas::new(12),
        var_access: Gas::new(18),
        per_path_segment: Gas::new(1),
        callback_run: Gas::new(10),
        rendering: Gas::new(4),
    };

    /// The latest schedule.
    pub const LATEST: Self = Self::V1;

    /// What `op` costs.
    pub fn cost(&self, op: Op) -> OpCost {
        (self.costs)(op)
    }

    /// The base charge of `op`.
    pub fn base(&self, op: Op) -> Gas {
        Gas::new(self.cost(op).base)
    }

    /// The per-unit charge of `op` as gas.
    pub fn per_unit(&self, op: Op) -> Gas {
        Gas::new(self.cost(op).per_unit)
    }

    /// A path parsed at run time (a computed `var` path, a `missing` key) is charged at `split`'s rate per unit of its
    /// weight, before it is parsed.
    pub fn path_text(&self, path: &Value) -> Gas {
        self.per_unit(Op::Split).times(path.weight())
    }

    /// Rendering numbers as decimal text (`cat`, `join`): `rendering` per unit of `(4 + floor(bits/64))^2`, for the
    /// bits of each expansion.
    pub fn rendered<'v>(&self, numbers: impl IntoIterator<Item = &'v Value>) -> Gas {
        numbers.into_iter().fold(Gas::ZERO, |g, v| {
            let limbs = values::rendered_bits(v).checked_div(64).unwrap_or(0).saturating_add(4);
            g.plus(self.rendering.times(limbs.saturating_mul(limbs)))
        })
    }

    /// The size charge of `op` on its evaluated arguments: each argument at its position's rate, plus any extra rate.
    pub fn sized<'v>(&self, op: Op, args: impl IntoIterator<Item = &'v Value>) -> Gas {
        self.sized_within(op, args, Gas::MAX)
    }

    /// [`GasSchedule::sized`], stopping as soon as the sum exceeds `cap` (the gas left): the sum so far, already more
    /// than can be paid, is then the charge. Every argument's size is O(1), and an argument list a caller supplies (a
    /// spread's array) is walked only as far as the gas left can pay for.
    pub fn sized_within<'v>(&self, op: Op, args: impl IntoIterator<Item = &'v Value>, cap: Gas) -> Gas {
        let c = self.cost(op);
        args.into_iter()
            .enumerate()
            .try_fold(Gas::ZERO, |total, (i, v)| {
                let own = c.rate(i);
                let first = total.plus(Gas::new(own.per_unit).times(own.unit.units(v)));
                let sum = c.extra.map_or(first, |r| first.plus(Gas::new(r.per_unit).times(r.unit.units(v))));
                if sum > cap { Err(sum) } else { Ok(sum) }
            })
            .unwrap_or_else(|over| over)
    }
}

/// Schedule version 1, operation by operation (`Gas.scala` `GasSchedule.V1`). An exhaustive match: every operation
/// has a cost, so the reference's fallback (`OpCost(1, 0, None)` for an operation missing from the table) is never
/// used.
#[allow(clippy::match_same_arms, reason = "one row per operation, as in the reference table")]
const fn v1(op: Op) -> OpCost {
    use SizeUnit::{Elements, Key, Limbs2, None, RationalLimbs2, Weight};
    let c = OpCost::new;
    match op {
        Op::If => c(20, 0, None),
        Op::And => c(15, 0, None),
        Op::Or => c(15, 0, None),
        Op::Not => c(40, 0, None),
        Op::Truthy => c(40, 0, None),
        Op::Default => c(15, 0, None),
        Op::Let => c(30, 0, None),
        Op::Eq => c(50, 1, Weight),
        Op::Ne => c(50, 1, Weight),
        Op::StrictEq => c(50, 1, Weight),
        Op::StrictNe => c(50, 1, Weight),
        Op::Lt => c(55, 1, Weight),
        Op::Le => c(55, 1, Weight),
        Op::Gt => c(55, 1, Weight),
        Op::Ge => c(55, 1, Weight),
        // integer sums and products are linear; rational ones, and every quotient, normalise through a quadratic gcd
        Op::Add => c(140, 9, Weight).plus(8, RationalLimbs2),
        Op::Sub => c(140, 10, Weight).plus(8, RationalLimbs2),
        Op::Mul => c(100, 17, Weight).plus(10, RationalLimbs2),
        Op::Div => c(200, 2, Weight).plus(8, Limbs2),
        Op::Mod => c(200, 2, Weight).plus(8, Limbs2),
        Op::Min => c(90, 1, Weight),
        Op::Max => c(90, 1, Weight),
        Op::Abs => c(60, 0, None),
        Op::Round => c(60, 1, Weight),
        Op::RoundHalfUp => c(60, 1, Weight),
        Op::Floor => c(60, 1, Weight),
        Op::Ceil => c(60, 1, Weight),
        // priced at its worst case, a 4096-bit rational result
        Op::Pow => c(50000, 0, None),
        Op::Map => c(40, 8, Elements),
        Op::Filter => c(40, 8, Elements),
        Op::Reduce => c(40, 8, Elements),
        Op::All => c(40, 8, Elements),
        Op::Some => c(40, 8, Elements),
        Op::None => c(40, 8, Elements),
        Op::Find => c(40, 8, Elements),
        Op::Count => c(45, 8, Elements),
        Op::Merge => c(210, 1, Weight),
        Op::In => c(70, 1, Weight),
        Op::Intersect => c(50, 12, Weight),
        Op::Unique => c(50, 18, Weight),
        Op::Slice => c(45, 5, Elements),
        Op::Reverse => c(55, 9, Elements),
        Op::Flatten => c(60, 5, Weight),
        Op::Length => c(45, 1, Weight),
        Op::Cat => c(65, 1, Weight).plus(32, Elements),
        Op::Substr => c(75, 1, Weight),
        Op::Lower => c(45, 1, Weight),
        Op::Upper => c(50, 1, Weight),
        Op::Join => c(110, 2, Weight).plus(32, Elements),
        Op::Split => c(85, 6, Weight),
        Op::Trim => c(75, 1, Weight),
        Op::StartsWith => c(50, 1, Weight),
        Op::EndsWith => c(50, 1, Weight),
        Op::Keys => c(50, 12, Weight),
        Op::Values => c(50, 12, Weight),
        Op::Entries => c(50, 18, Weight),
        Op::Get => c(30, 0, None).at(1, 1, Key),
        Op::Has => c(40, 0, None).at(1, 1, Key),
        Op::Set => c(105, 0, None).at(1, 1, Key),
        Op::Unset => c(65, 0, None).at(1, 1, Key),
        Op::Exists => c(50, 1, Elements),
        Op::Missing => c(50, 20, Elements),
        Op::MissingSome => c(60, 20, Elements),
        Op::TypeOf => c(45, 0, None),
        Op::Hex => c(45, 1, Weight),
        Op::Unhex => c(80, 3, Weight),
        Op::HexToInt => c(80, 3, Weight),
        Op::BytesToInt => c(45, 1, Weight),
        Op::IntToBytes => c(70, 1, Weight),
        Op::Sha256 => c(80, 1, Weight),
        Op::Sha3_256 => c(130, 1, Weight),
        Op::Keccak256 => c(150, 1, Weight),
        Op::Blake2b256 => c(115, 1, Weight),
        Op::Ed25519Verify => c(36000, 1, Weight),
        Op::Secp256k1Verify => c(7200, 1, Weight),
        Op::Bip340Verify => c(8000, 1, Weight),
        Op::Ecrecover => c(30000, 0, None),
        Op::BlsVerify => c(233_000, 1, Weight),
        // the message is only hashed: it pays the hashing rate per unit of weight, as sigma_verify's does
        Op::BlsAggregateVerify => c(230_000, 25000, Elements).at(1, 1, Weight),
        Op::Poseidon => c(1800, 3900, Elements),
        Op::PmtVerify => c(1000, 11000, Elements),
        Op::Groth16Verify => c(218_000, 1, Weight),
        Op::EcvrfVerify => c(150_000, 1, Weight),
        Op::Bn254Add => c(150, 0, None),
        Op::Bn254Mul => c(7700, 0, None),
        Op::Bn254Pairing => c(55000, 128, Weight),
        // the message is only hashed: it pays the hashing rate, not the curve rate
        Op::SchnorrVerify => c(5000, 430, Weight).at(1, 1, Weight),
        Op::SigmaVerify => c(5000, 430, Weight).at(2, 1, Weight),
        Op::MerkleVerify => c(500, 13, Weight),
        Op::MerkleConsistencyVerify => c(500, 13, Weight),
        Op::TrieVerify => c(500, 30, Weight),
        Op::TriePrefixVerify => c(1000, 30, Weight),
        Op::TrieRangeVerify => c(1000, 30, Weight),
    }
}

/// A gas limit for one evaluation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GasLimit(pub Gas);

impl GasLimit {
    /// 2^24, Ethereum's per-transaction cap (EIP-7825).
    pub const DEFAULT: Self = Self(Gas::new(1 << 24));
}

impl Default for GasLimit {
    fn default() -> Self {
        Self::DEFAULT
    }
}
