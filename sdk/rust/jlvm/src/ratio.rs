//! Exact rational arithmetic for the operations (`numerics` `Ratio.scala`): always reduced, with a positive
//! denominator. Not a value type: operations convert to and from [`Value`] at their edges.

use alloc::string::{String, ToString};
use core::cmp::Ordering;

use dilf4s_core::Value;
use dilf4s_core::json::decimal;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{One, Signed, Zero};

// Big-integer arithmetic through methods: it cannot overflow (it allocates), and every division is by a value already
// shown to be non-zero (a zero divisor gives 0 here instead of panicking).
fn mul(a: &BigInt, b: &BigInt) -> BigInt {
    a.checked_mul(b).unwrap_or_default()
}

fn add(a: &BigInt, b: &BigInt) -> BigInt {
    a.checked_add(b).unwrap_or_default()
}

fn sub(a: &BigInt, b: &BigInt) -> BigInt {
    a.checked_sub(b).unwrap_or_default()
}

fn neg(a: &BigInt) -> BigInt {
    sub(&BigInt::zero(), a)
}

/// Truncated quotient and remainder.
fn div_rem(a: &BigInt, b: &BigInt) -> (BigInt, BigInt) {
    if b.is_zero() { (BigInt::zero(), BigInt::zero()) } else { a.div_rem(b) }
}

fn div(a: &BigInt, b: &BigInt) -> BigInt {
    div_rem(a, b).0
}

/// `numerator / denominator`, gcd-reduced, `denominator > 0`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Ratio {
    n: BigInt,
    d: BigInt,
}

impl Ratio {
    pub(crate) fn integer(n: BigInt) -> Self {
        Self { n, d: BigInt::one() }
    }

    pub(crate) fn zero() -> Self {
        Self::integer(BigInt::zero())
    }

    pub(crate) fn one() -> Self {
        Self::integer(BigInt::one())
    }

    /// Callers guarantee `d != 0`.
    fn reduced(n: &BigInt, d: &BigInt) -> Self {
        let g = n.gcd(d);
        if g.is_zero() {
            // only when d == 0, which callers exclude
            return Self::zero();
        }
        let (n, d) = if d.is_negative() { (neg(n), neg(d)) } else { (n.clone(), d.clone()) };
        Self { n: div(&n, &g), d: div(&d, &g) }
    }

    /// `n / d`, or `None` when `d` is zero.
    pub(crate) fn of(n: &BigInt, d: &BigInt) -> Option<Self> {
        (!d.is_zero()).then(|| Self::reduced(n, d))
    }

    /// The number a value holds, if it is one.
    pub(crate) fn from_value(v: &Value) -> Option<Self> {
        match v {
            Value::Int(i) => Some(Self::integer(i.as_bigint().clone())),
            Value::Rational(r) => Some(Self { n: r.numerator().clone(), d: r.denominator().clone() }),
            _ => None,
        }
    }

    /// The value in the single numeric domain: an integer when it is one.
    pub(crate) fn to_value(&self) -> Value {
        if self.d.is_one() {
            Value::int(self.n.clone())
        } else {
            // reduced with d >= 2, so this is a valid rational; the fallback is unreachable
            Value::number(&self.n, &self.d).unwrap_or(Value::Null)
        }
    }

    pub(crate) const fn numerator(&self) -> &BigInt {
        &self.n
    }

    pub(crate) const fn denominator(&self) -> &BigInt {
        &self.d
    }

    pub(crate) fn is_integer(&self) -> bool {
        self.d.is_one()
    }

    pub(crate) fn neg(&self) -> Self {
        Self { n: neg(&self.n), d: self.d.clone() }
    }

    pub(crate) fn abs(&self) -> Self {
        Self { n: self.n.abs(), d: self.d.clone() }
    }

    pub(crate) fn add(&self, o: &Self) -> Self {
        Self::reduced(&add(&mul(&self.n, &o.d), &mul(&o.n, &self.d)), &mul(&self.d, &o.d))
    }

    pub(crate) fn sub(&self, o: &Self) -> Self {
        self.add(&o.neg())
    }

    pub(crate) fn mul(&self, o: &Self) -> Self {
        Self::reduced(&mul(&self.n, &o.n), &mul(&self.d, &o.d))
    }

    pub(crate) fn inverse(&self) -> Option<Self> {
        Self::of(&self.d, &self.n)
    }

    /// `None` on division by zero.
    pub(crate) fn div(&self, o: &Self) -> Option<Self> {
        o.inverse().map(|i| self.mul(&i))
    }

    /// Integer power; a negative exponent inverts, so zero to a negative power is `None` (division by zero).
    pub(crate) fn pow(&self, exponent: i64) -> Option<Self> {
        let e = usize::try_from(exponent.unsigned_abs()).ok()?;
        let base = if exponent >= 0 { Some(self.clone()) } else { self.inverse() }?;
        Some(Self::reduced(&num_traits::pow(base.n, e), &num_traits::pow(base.d, e)))
    }

    pub(crate) fn cmp(&self, o: &Self) -> Ordering {
        mul(&self.n, &o.d).cmp(&mul(&o.n, &self.d))
    }

    /// Round toward zero.
    pub(crate) fn truncate(&self) -> BigInt {
        div(&self.n, &self.d)
    }

    pub(crate) fn floor(&self) -> BigInt {
        let (q, r) = div_rem(&self.n, &self.d);
        if self.n.is_negative() && !r.is_zero() { sub(&q, &BigInt::one()) } else { q }
    }

    pub(crate) fn ceil(&self) -> BigInt {
        let (q, r) = div_rem(&self.n, &self.d);
        if self.n.is_positive() && !r.is_zero() { add(&q, &BigInt::one()) } else { q }
    }

    /// Half away from zero.
    pub(crate) fn round_half_up(&self) -> BigInt {
        let two = BigInt::from(2u8);
        let q = div(&add(&mul(&self.n.abs(), &two), &self.d), &mul(&self.d, &two));
        mul(&q, &self.n.signum())
    }

    /// Half to even.
    pub(crate) fn round_half_even(&self) -> BigInt {
        let lo = self.floor();
        let twice = mul(&sub(&self.n, &mul(&lo, &self.d)), &BigInt::from(2u8));
        let up = |lo: &BigInt| add(lo, &BigInt::one());
        match twice.cmp(&self.d) {
            Ordering::Less => lo,
            Ordering::Equal if lo.is_even() => lo,
            Ordering::Greater | Ordering::Equal => up(&lo),
        }
    }

    /// Truncated remainder `a - b * truncate(a / b)`; `None` on division by zero.
    pub(crate) fn rem(&self, o: &Self) -> Option<Self> {
        self.div(o).map(|q| self.sub(&o.mul(&Self::integer(q.truncate()))))
    }

    /// The exact decimal text, when the value terminates.
    pub(crate) fn to_decimal_string(&self) -> Option<String> {
        match self.to_value() {
            Value::Int(i) => Some(i.to_string()),
            Value::Rational(r) => decimal(&r),
            _ => None,
        }
    }
}
