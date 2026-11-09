//! Gas arithmetic (`dilf4s-jlvm`, docs/specs/jlvm.md section 7).
//!
//! Every charge is built from [`Gas::new`], [`Gas::plus`] and [`Gas::times`] over counts that are themselves
//! saturating `u64` arithmetic. [`gas_clamp`] shows each is exact arithmetic clamped at `M = Gas::MAX`:
//! `new(a) = min(a, M)`, `new(a).plus(new(b)) = min(a + b, M)` and `new(a).times(n) = min(a n, M)` for every `a`, `b` and
//! `n`. Both sides of each depend on an operand `x` only through `min(x, M)` (a sum or a non-zero multiple of anything
//! at least `M` is at least `M`), and a count saturated at `u64::MAX` is itself at least `M`, so by induction over
//! a charge's formula every charge is its exact value clamped at `M`: saturation never undercharges, it only stands
//! for a charge of `M` or more. [`gas_meter`] then shows the meter's test of a charge decides exactly as unbounded
//! arithmetic would, for every limit below `M`.

use dilf4s_jlvm::{Gas, GasSchedule, Op, RENDER_LIMBS, Value};

use crate::{Broken, holds};

/// `Gas::MAX`, the saturation point.
pub const MAX: u64 = Gas::MAX.value();

/// `x` clamped at [`MAX`].
fn clamp(x: u128) -> u64 {
    u64::try_from(x).map_or(MAX, |x| x.min(MAX))
}

/// `Gas::new(a)`, `Gas::new(a).plus(Gas::new(b))` and `Gas::new(a).times(n)` are `a`, `a + b` and `a n` computed
/// exactly and clamped at [`MAX`].
pub fn gas_clamp(a: u64, b: u64, n: u64) -> Result<(), Broken> {
    let (ga, gb) = (Gas::new(a), Gas::new(b));
    holds(ga.value() == clamp(u128::from(a)), Broken::GasClamp)?;
    // exact: neither reaches u128::MAX
    let sum = u128::from(a).saturating_add(u128::from(b));
    let product = u128::from(a).saturating_mul(u128::from(n));
    holds(ga.plus(gb).value() == clamp(sum), Broken::GasClamp)?;
    holds(ga.times(n).value() == clamp(product), Broken::GasClamp)
}

/// The meter's test of a charge (`machine.rs`, `Step::Charge`: `used.plus(charge) <= limit`, else `OutOfGas`), for gas
/// `used` and a `charge` that stand for exact amounts `used_exact` and `charge_exact` (each clamped at [`MAX`] to give
/// them, as [`gas_clamp`] shows every amount is), decides as unbounded arithmetic would: the charge is paid exactly when
/// `used_exact + charge_exact <= limit`, for every `limit` below [`MAX`]. A limit of [`MAX`] itself is no limit: every
/// charge is then paid, however far past [`MAX`] the exact sum, as in the reference (`Gas.scala`).
pub fn gas_meter(used_exact: u64, charge_exact: u64, limit: u64) -> Result<(), Broken> {
    let paid = Gas::new(used_exact).plus(Gas::new(charge_exact)) <= Gas::new(limit);
    let exact = u128::from(used_exact).saturating_add(u128::from(charge_exact)) <= u128::from(limit);
    holds(limit >= MAX || paid == exact, Broken::GasMeter)
}

/// `GasSchedule::sized_within(op, args, cap)` stops early only on a sum already over `cap`: it is never more than
/// `sized(op, args)`, it is over `cap` exactly when `sized` is, and when `sized` is within `cap` the two are equal.
pub fn gas_cap(op: Op, args: &[Value], cap: Gas) -> Result<(), Broken> {
    let schedule = GasSchedule::LATEST;
    let full = schedule.sized(op, args);
    let capped = schedule.sized_within(op, args, cap);
    holds(capped <= full && (capped > cap) == (full > cap) && (full > cap || capped == full), Broken::GasCap)
}

/// The bits of `v`'s decimal expansion as jlvm.md section 7 bounds them: the integer's, or for a rational `p/q`
/// `bits(p) + floor((7 bits(q) + 2) / 3)`; 0 for anything else.
fn expansion_bits(v: &Value) -> u128 {
    match v {
        Value::Int(i) => u128::from(i.bit_length()),
        Value::Rational(r) => {
            let q = u128::from(r.denominator_bit_length());
            u128::from(r.numerator_bit_length()).saturating_add(q.saturating_mul(7).saturating_add(2) / 3)
        }
        _ => 0,
    }
}

/// `GasSchedule::rendered`, the charge for rendering numbers as decimal text (`cat`, `join`), is
/// `rendering (RENDER_LIMBS + floor(b / 64))^2` for each value of `b` expansion bits, summed exactly and clamped at
/// [`MAX`]. Anything but a number counts as 0 bits, as in the reference; `cat` and `join` pass only the numbers they
/// render.
pub fn gas_rendered(values: &[Value]) -> Result<(), Broken> {
    let schedule = GasSchedule::LATEST;
    let rate = u128::from(schedule.rendering.value());
    // exact: no count here comes near u128::MAX
    let exact = values.iter().map(expansion_bits).fold(0u128, |sum, b| {
        let limbs = (b / 64).saturating_add(u128::from(RENDER_LIMBS));
        sum.saturating_add(rate.saturating_mul(limbs.saturating_mul(limbs)))
    });
    holds(schedule.rendered(values).value() == clamp(exact), Broken::GasRendered)
}
