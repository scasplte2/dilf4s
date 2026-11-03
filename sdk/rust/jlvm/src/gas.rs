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
    /// For a number, the square of its 64-bit limbs, numerator and denominator together, plus [`GCD_LIMBS`] for a
    /// non-integer rational; 0 for anything else.
    Limbs2,
    /// [`SizeUnit::Limbs2`] for a non-integer rational, 0 for an integer.
    RationalLimbs2,
    /// The weight of a non-integer rational, 0 for anything else.
    RationalWeight,
}

/// The fixed cost of a decimal conversion, in limbs ([`GasSchedule::rendered`]).
pub const RENDER_LIMBS: u64 = 6;

/// The fixed cost of a gcd, in limbs ([`SizeUnit::Limbs2`]).
pub const GCD_LIMBS: u64 = 8;

impl SizeUnit {
    /// One argument's size in this unit (`GasSchedule.units`).
    pub fn units(self, v: &Value) -> u64 {
        match self {
            Self::None => 0,
            Self::Elements => values::elements(v),
            Self::Weight => v.weight(),
            Self::Limbs2 => limbs(v, true),
            Self::RationalLimbs2 => limbs(v, false),
            Self::RationalWeight => match v {
                Value::Rational(_) => v.weight(),
                _ => 0,
            },
        }
    }
}

fn limbs(v: &Value, integers: bool) -> u64 {
    let n = match v {
        Value::Int(i) if integers => i.bit_length() / 64,
        Value::Rational(r) => {
            GCD_LIMBS.saturating_add(r.numerator_bit_length() / 64).saturating_add(r.denominator_bit_length() / 64)
        }
        _ => 0,
    };
    n.saturating_mul(n)
}

/// Searching a map (argument 0) for a key (argument 1), as `get`, `has`, `set` and `unset` do: `per_level`, plus
/// `per_block` per [`values::SEARCH_BLOCK`] of the key's UTF-16 length, for each of the search's [`levels`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Search {
    /// Gas per level.
    pub per_level: u64,
    /// Gas per level and block of the key.
    pub per_block: u64,
}

/// A map's depth bound, `ceil(log2(n + 1))` for `n` entries: the height of a perfectly balanced tree.
pub fn depth(entries: u64) -> u64 {
    u64::from(u64::BITS.saturating_sub(entries.leading_zeros()))
}

/// The levels a search of a map of `entries` entries pays for: its [`depth`], plus one for the key itself.
pub fn levels(entries: u64) -> u64 {
    depth(entries).saturating_add(1)
}

impl Search {
    /// The charge on a map and a key: 0 unless they are a map and text.
    pub fn charge(self, map: &Value, key: &Value) -> Gas {
        match (map, key) {
            (Value::Map(m), Value::Text(k)) => {
                let entries = u64::try_from(m.len()).unwrap_or(u64::MAX);
                Gas::new(self.per_block)
                    .times(k.utf16_len() / values::SEARCH_BLOCK)
                    .plus(Gas::new(self.per_level))
                    .times(levels(entries))
            }
            _ => Gas::ZERO,
        }
    }
}

/// Merging maps (`merge` when every argument is a map): `per_level` per entry of the maps after the first, plus
/// `per_block` per [`values::SEARCH_BLOCK`] of their weight, for each of the [`levels`] of all their entries.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Merging {
    /// Gas per level and entry merged in.
    pub per_level: u64,
    /// Gas per level and block of the weight merged in.
    pub per_block: u64,
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
    /// Further rates charged on every argument (at most two).
    pub extra: [Option<Rate>; 2],
    /// The operation searches its map argument for its key argument.
    pub search: Option<Search>,
    /// The operation merges its map arguments.
    pub merging: Option<Merging>,
}

impl OpCost {
    const fn new(base: u64, per_unit: u64, unit: SizeUnit) -> Self {
        Self { base, per_unit, unit, by_argument: None, extra: [None, None], search: None, merging: None }
    }

    const fn merging_maps(self, per_level: u64, per_block: u64) -> Self {
        Self { merging: Some(Merging { per_level, per_block }), ..self }
    }

    const fn searching(self, per_level: u64, per_block: u64) -> Self {
        Self { search: Some(Search { per_level, per_block }), ..self }
    }

    const fn at(self, position: usize, per_unit: u64, unit: SizeUnit) -> Self {
        Self { by_argument: Some((position, Rate { per_unit, unit })), ..self }
    }

    const fn plus(self, per_unit: u64, unit: SizeUnit) -> Self {
        let rate = Some(Rate { per_unit, unit });
        let extra = match self.extra {
            [None, _] => [rate, None],
            [first, _] => [first, rate],
        };
        Self { extra, ..self }
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
    /// Per unit of `(RENDER_LIMBS + floor(bits / 64))^2` of each number `cat` and `join` render.
    pub rendering: Gas,
}

impl GasSchedule {
    /// Version 1 (`GasSchedule.V1`).
    pub const V1: Self = Self {
        version: 1,
        costs: v1,
        literal: Gas::new(7),
        element: Gas::new(8),
        var_access: Gas::new(38),
        per_path_segment: Gas::new(3),
        callback_run: Gas::new(18),
        rendering: Gas::new(2),
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

    /// Rendering numbers as decimal text (`cat`, `join`): `rendering` per unit of `(RENDER_LIMBS + floor(bits/64))^2`, for the
    /// bits of each expansion.
    pub fn rendered<'v>(&self, numbers: impl IntoIterator<Item = &'v Value>) -> Gas {
        numbers.into_iter().fold(Gas::ZERO, |g, v| {
            let limbs = values::rendered_bits(v).checked_div(64).unwrap_or(0).saturating_add(RENDER_LIMBS);
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
    ///
    /// A [`Search`] is charged with the second argument, on the first two; [`Merging`] after the last.
    pub fn sized_within<'v>(&self, op: Op, args: impl IntoIterator<Item = &'v Value>, cap: Gas) -> Gas {
        let c = self.cost(op);
        args.into_iter()
            .enumerate()
            .try_fold(Sizing::default(), |s, (i, v)| {
                let own = c.rate(i);
                let first = s.sum.plus(Gas::new(own.per_unit).times(own.unit.units(v)));
                let each =
                    c.extra.iter().flatten().fold(first, |g, r| g.plus(Gas::new(r.per_unit).times(r.unit.units(v))));
                let sum = match (c.search, s.first) {
                    (Some(search), Some(m)) if i == 1 => each.plus(search.charge(m, v)),
                    _ => each,
                };
                let next = s.with(sum, v);
                if sum > cap { Err(sum) } else { Ok(next) }
            })
            .map_or_else(|over| over, |s| c.merging.map_or(s.sum, |m| s.sum.plus(s.merged(m))))
    }
}

/// What [`GasSchedule::sized_within`] carries from argument to argument: the sum so far, the first argument, and the
/// map entries and weight a [`Merging`] needs.
#[derive(Default)]
struct Sizing<'v> {
    sum: Gas,
    first: Option<&'v Value>,
    all_maps: bool,
    entries: u64,
    later_entries: u64,
    later_weight: u64,
}

impl<'v> Sizing<'v> {
    fn with(&self, sum: Gas, v: &'v Value) -> Self {
        let (is_map, n) = match v {
            Value::Map(m) => (true, u64::try_from(m.len()).unwrap_or(u64::MAX)),
            _ => (false, 0),
        };
        let later = self.first.is_some();
        Self {
            sum,
            first: self.first.or(Some(v)),
            all_maps: is_map && (self.all_maps || !later),
            entries: self.entries.saturating_add(n),
            later_entries: if later { self.later_entries.saturating_add(n) } else { 0 },
            later_weight: if later { self.later_weight.saturating_add(v.weight()) } else { 0 },
        }
    }

    fn merged(&self, m: Merging) -> Gas {
        if !self.all_maps {
            return Gas::ZERO;
        }
        Gas::new(m.per_level)
            .times(self.later_entries)
            .plus(Gas::new(m.per_block).times(self.later_weight / values::SEARCH_BLOCK))
            .times(levels(self.entries))
    }
}

/// Schedule version 1, operation by operation (`Gas.scala` `GasSchedule.V1`). An exhaustive match: every operation
/// has a cost, so the reference's fallback (`OpCost(1, 0, None)` for an operation missing from the table) is never
/// used.
#[allow(clippy::match_same_arms, reason = "one row per operation, as in the reference table")]
const fn v1(op: Op) -> OpCost {
    use SizeUnit::{Elements, Limbs2, None, RationalLimbs2, RationalWeight, Weight};
    let c = OpCost::new;
    match op {
        Op::If => c(20, 0, None),
        Op::And => c(15, 0, None),
        Op::Or => c(15, 0, None),
        Op::Not => c(20, 0, None),
        Op::Truthy => c(20, 0, None),
        Op::Default => c(15, 0, None),
        Op::Let => c(30, 0, None),
        Op::Eq => c(50, 1, Weight),
        Op::Ne => c(50, 1, Weight),
        Op::StrictEq => c(50, 1, Weight),
        Op::StrictNe => c(50, 1, Weight),
        Op::Lt => c(55, 1, Weight).plus(14, RationalWeight),
        Op::Le => c(55, 1, Weight).plus(14, RationalWeight),
        Op::Gt => c(55, 1, Weight).plus(14, RationalWeight),
        Op::Ge => c(55, 1, Weight).plus(14, RationalWeight),
        // integer sums and products are linear; rational ones, and every quotient, normalise through a quadratic gcd
        Op::Add => c(140, 3, Weight).plus(7, RationalLimbs2),
        Op::Sub => c(140, 3, Weight).plus(7, RationalLimbs2),
        Op::Mul => c(100, 10, Weight).plus(7, RationalLimbs2),
        Op::Div => c(340, 2, Weight).plus(8, Limbs2),
        Op::Mod => c(340, 2, Weight).plus(8, Limbs2).plus(8, RationalLimbs2),
        Op::Min => c(90, 1, Weight).plus(14, RationalWeight),
        Op::Max => c(90, 1, Weight).plus(14, RationalWeight),
        Op::Abs => c(60, 0, None).plus(2, RationalLimbs2),
        Op::Round => c(60, 8, Weight),
        Op::RoundHalfUp => c(60, 8, Weight),
        Op::Floor => c(60, 8, Weight),
        Op::Ceil => c(60, 8, Weight),
        // priced at its worst case, a 4096-bit rational result
        Op::Pow => c(20000, 0, None),
        Op::Map => c(40, 8, Elements),
        Op::Filter => c(40, 8, Elements),
        Op::Reduce => c(40, 8, Elements),
        Op::All => c(40, 8, Elements),
        Op::Some => c(40, 8, Elements),
        Op::None => c(40, 8, Elements),
        Op::Find => c(40, 8, Elements),
        Op::Count => c(45, 8, Elements),
        Op::Merge => c(210, 3, Weight).merging_maps(12, 2),
        Op::In => c(70, 1, Weight).plus(1, Elements),
        Op::Intersect => c(50, 17, Weight),
        Op::Unique => c(50, 18, Weight),
        Op::Slice => c(60, 3, Elements),
        Op::Reverse => c(55, 5, Elements),
        Op::Flatten => c(60, 3, Weight),
        Op::Length => c(45, 1, Weight),
        Op::Cat => c(65, 1, Weight).plus(4, Elements),
        Op::Substr => c(75, 1, Weight),
        Op::Lower => c(45, 1, Weight),
        Op::Upper => c(50, 1, Weight),
        Op::Join => c(110, 1, Weight).plus(14, Elements),
        Op::Split => c(85, 6, Weight),
        Op::Trim => c(75, 1, Weight),
        Op::StartsWith => c(50, 1, Weight),
        Op::EndsWith => c(50, 1, Weight),
        Op::Keys => c(90, 4, Weight),
        Op::Values => c(50, 5, Weight),
        Op::Entries => c(120, 10, Weight),
        Op::Get => c(30, 0, None).searching(3, 1),
        Op::Has => c(40, 0, None).searching(3, 1),
        Op::Set => c(60, 0, None).searching(18, 2),
        Op::Unset => c(60, 0, None).searching(18, 2),
        Op::Exists => c(60, 1, Elements),
        Op::Missing => c(50, 20, Elements),
        Op::MissingSome => c(60, 20, Elements),
        Op::TypeOf => c(30, 0, None),
        Op::Hex => c(45, 1, Weight),
        Op::Unhex => c(80, 2, Weight),
        Op::HexToInt => c(80, 6, Weight),
        Op::BytesToInt => c(45, 1, Weight),
        Op::IntToBytes => c(70, 2, Weight),
        Op::Sha256 => c(95, 1, Weight),
        Op::Sha3_256 => c(130, 1, Weight),
        Op::Keccak256 => c(150, 1, Weight),
        Op::Blake2b256 => c(115, 1, Weight),
        Op::Ed25519Verify => c(20000, 1, Weight),
        Op::Secp256k1Verify => c(7200, 1, Weight),
        Op::Bip340Verify => c(9000, 1, Weight),
        Op::Ecrecover => c(32500, 0, None),
        Op::BlsVerify => c(290_000, 1, Weight),
        // the message is only hashed: it pays the hashing rate per unit of weight, as sigma_verify's does
        Op::BlsAggregateVerify => c(250_000, 33000, Elements).at(1, 1, Weight),
        Op::Poseidon => c(1800, 3900, Elements),
        Op::PmtVerify => c(1000, 11000, Elements),
        Op::Groth16Verify => c(370_000, 1, Weight),
        Op::EcvrfVerify => c(130_000, 1, Weight),
        Op::Bn254Add => c(520, 0, None),
        Op::Bn254Mul => c(12500, 0, None),
        Op::Bn254Pairing => c(130_000, 440, Weight),
        // the message is only hashed: it pays the hashing rate, not the curve rate
        Op::SchnorrVerify => c(5000, 450, Weight).at(1, 1, Weight),
        Op::SigmaVerify => c(5000, 450, Weight).at(2, 1, Weight),
        Op::MerkleVerify => c(500, 13, Weight),
        Op::MerkleConsistencyVerify => c(4000, 13, Weight),
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
