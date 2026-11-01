//! Operations on already-evaluated values (`PureOps.scala`). Typing is strict: a value of the wrong kind is a `Type`
//! error, never coerced; the one exception is metakit's handling of an absent value (null) in `in`, `intersect`,
//! `startsWith`, `endsWith`, `keys`, `values`, `entries`, `cat` and `join`.
//!
//! Each operation checks its arguments in the reference's order (its `tupled`/`traverse` are fail-fast, left to
//! right), so the first error is the same.

use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::cmp::Ordering;

use alloc::sync::Arc;
use dilf4s_core::hash::HashAlgorithm;

use dilf4s_core::value::Text;
use dilf4s_core::{BigInt, Value};
use num_bigint::Sign;
use num_traits::{Signed, ToPrimitive, Zero};

use crate::error::JlvmError;
use crate::op::Op;
use crate::ratio::Ratio;
use crate::values::{
    self, MAX_NUMBER_BITS, MAX_WEIGHT, bit_length, bounded, compare, count_usize, distinct, integer, kind, num,
    numeric, text, truthy,
};

type Result<T> = core::result::Result<T, JlvmError>;

/// The largest exponent magnitude `pow` accepts.
pub const MAX_EXPONENT: u64 = 999;

/// The largest `int_to_bytes` width.
pub const MAX_INT_BYTES: u64 = 1024;

fn n_args(n: usize) -> u64 {
    u64::try_from(n).unwrap_or(u64::MAX)
}

/// Applies a strict operation (one with [`Op::is_strict`]) other than `+`, `*`, `cat` and `join`'s charged steps, which
/// the machine runs itself; for those it is the uncharged reference.
pub(crate) fn apply(op: Op, args: Vec<Value>) -> Result<Value> {
    match op {
        Op::Not => unary(op, args, |v| Ok(Value::Bool(!truthy(&v)))),
        Op::Truthy => unary(op, args, |v| Ok(Value::Bool(truthy(&v)))),
        Op::Eq | Op::StrictEq => binary(op, args, |a, b| Ok(Value::Bool(a == b))),
        Op::Ne | Op::StrictNe => binary(op, args, |a, b| Ok(Value::Bool(a != b))),
        Op::Lt => ordering(op, &args, Ordering::is_lt, true),
        Op::Le => ordering(op, &args, Ordering::is_le, true),
        Op::Gt => ordering(op, &args, Ordering::is_gt, false),
        Op::Ge => ordering(op, &args, Ordering::is_ge, false),
        Op::Add | Op::Mul => total(op, &args),
        Op::Min => fold(op, &args, |a, b| if a.cmp(&b).is_le() { a } else { b }),
        Op::Max => fold(op, &args, |a, b| if a.cmp(&b).is_ge() { a } else { b }),
        Op::Sub => subtract(op, &args),
        Op::Div => num_binary(op, args, |a, b| a.div(&b).ok_or_else(|| division_by_zero(op))),
        Op::Mod => num_binary(op, args, |a, b| a.rem(&b).ok_or_else(|| division_by_zero(op))),
        Op::Pow => num_binary(op, args, |a, b| power(op, &a, &b)),
        Op::Abs => num_unary(op, args, |r| r.abs()),
        Op::Round => num_unary(op, args, |r| Ratio::integer(r.round_half_even())),
        Op::RoundHalfUp => num_unary(op, args, |r| Ratio::integer(r.round_half_up())),
        Op::Floor => num_unary(op, args, |r| Ratio::integer(r.floor())),
        Op::Ceil => num_unary(op, args, |r| Ratio::integer(r.ceil())),
        Op::Cat => cat(op, args),
        Op::Substr => substr(op, &args),
        Op::Lower => unary(op, args, |v| str(op, v).and_then(|s| text(op, s.to_ascii_lowercase()))),
        Op::Upper => unary(op, args, |v| str(op, v).and_then(|s| text(op, s.to_ascii_uppercase()))),
        Op::Trim => unary(op, args, |v| str(op, v).and_then(|s| text(op, String::from(values::trim(&s))))),
        Op::StartsWith | Op::EndsWith => affix(op, &args),
        Op::Join => joining(args).and_then(|j| joined(&j)),
        Op::Split => split(op, args),
        Op::Length => unary(op, args, |v| length(op, &v)),
        Op::Merge => merge(op, args),
        Op::In => binary(op, args, |n, h| contains(op, &n, &h)),
        Op::Intersect => binary(op, args, |a, b| subset(op, &a, &b)),
        Op::Unique => unary(op, args, |v| arr(op, v).and_then(|vs| values::array(op.wire(), distinct(op, vs)?))),
        Op::Reverse => unary(op, args, |v| {
            arr(op, v).and_then(|mut vs| {
                vs.reverse();
                values::array(op.wire(), vs)
            })
        }),
        Op::Flatten => unary(op, args, |v| arr(op, v).and_then(|vs| values::array(op.wire(), spread_all(vs)))),
        Op::Slice => slice(op, args),
        Op::Count => unary(op, args, |v| arr(op, v).map(|vs| count_usize(vs.len()))),
        Op::Keys | Op::Values | Op::Entries => listing(op, args),
        Op::Get => get(op, &args),
        Op::Has => binary(op, args, |m, k| {
            let o = obj(op, &m)?;
            let key = str_ref(op, &k)?;
            Ok(Value::Bool(o.get(key).is_some()))
        }),
        Op::Set => set(op, args),
        Op::Unset => binary(op, args, |m, k| {
            let o = record(op, m)?;
            let key = text_key(op, k)?;
            Ok(Value::Map(o.removed(&key)))
        }),
        Op::Exists => Ok(Value::Bool(!args.contains(&Value::Null))),
        Op::TypeOf => unary(op, args, |v| text(op, String::from(kind(&v)))),
        Op::Hex => unary(op, args, |v| {
            raw_bytes(op, v).and_then(|b| {
                let mut s = String::from("0x");
                s.push_str(&dilf4s_core::hex::encode(&b));
                text(op, s)
            })
        }),
        Op::Unhex => unary(op, args, |v| hex_bytes(op, v).and_then(|b| bounded(op.wire(), Value::Bytes(b)))),
        Op::HexToInt => unary(op, args, |v| hex_bytes(op, v).and_then(|b| unsigned_int(op, &b))),
        Op::BytesToInt => unary(op, args, |v| raw_bytes(op, v).and_then(|b| unsigned_int(op, &b))),
        Op::IntToBytes => int_to_bytes(op, &args),
        Op::Sha256 => unary(op, args, |v| digest(op, HashAlgorithm::Sha256, &v)),
        Op::Sha3_256 => unary(op, args, |v| digest(op, HashAlgorithm::Sha3_256, &v)),
        Op::Keccak256 => unary(op, args, |v| digest(op, HashAlgorithm::Keccak256, &v)),
        Op::Blake2b256 => unary(op, args, |v| digest(op, HashAlgorithm::Blake2b256, &v)),
        _ => Err(JlvmError::malformed(alloc::format!("{} has no strict form", op.wire()))),
    }
}

// ---- argument shapes

fn unary(op: Op, args: Vec<Value>, f: impl FnOnce(Value) -> Result<Value>) -> Result<Value> {
    let n = args.len();
    match <[Value; 1]>::try_from(args) {
        Ok([a]) => f(a),
        Err(_) => Err(JlvmError::arity(op.wire(), alloc::format!("takes 1 argument, got {n}"))),
    }
}

fn binary(op: Op, args: Vec<Value>, f: impl FnOnce(Value, Value) -> Result<Value>) -> Result<Value> {
    let n = args.len();
    match <[Value; 2]>::try_from(args) {
        Ok([a, b]) => f(a, b),
        Err(_) => Err(JlvmError::arity(op.wire(), alloc::format!("takes 2 arguments, got {n}"))),
    }
}

fn num_unary(op: Op, args: Vec<Value>, f: impl FnOnce(Ratio) -> Ratio) -> Result<Value> {
    unary(op, args, |v| numeric(op, &v).and_then(|r| num(op, &f(r))))
}

fn num_binary(op: Op, args: Vec<Value>, f: impl FnOnce(Ratio, Ratio) -> Result<Ratio>) -> Result<Value> {
    binary(op, args, |a, b| {
        let x = numeric(op, &a)?;
        let y = numeric(op, &b)?;
        f(x, y).and_then(|r| num(op, &r))
    })
}

fn type_error<A>(op: Op, message: String) -> Result<A> {
    Err(JlvmError::type_error(op.wire(), message))
}

fn division_by_zero(op: Op) -> JlvmError {
    JlvmError::arithmetic(op.wire(), "division by zero")
}

fn str(op: Op, v: Value) -> Result<Text> {
    match v {
        Value::Text(s) => Ok(s),
        other => type_error(op, alloc::format!("expected a string, got {}", kind(&other))),
    }
}

fn str_ref(op: Op, v: &Value) -> Result<&str> {
    match v {
        Value::Text(s) => Ok(s.as_str()),
        other => type_error(op, alloc::format!("expected a string, got {}", kind(other))),
    }
}

fn arr(op: Op, v: Value) -> Result<Vec<Value>> {
    match v {
        Value::Array(a) => Ok(a.into_items()),
        other => type_error(op, alloc::format!("expected an array, got {}", kind(&other))),
    }
}

fn arr_ref(op: Op, v: &Value) -> Result<&[Value]> {
    match v {
        Value::Array(a) => Ok(a.items()),
        other => type_error(op, alloc::format!("expected an array, got {}", kind(other))),
    }
}

fn obj(op: Op, v: &Value) -> Result<&dilf4s_core::Map> {
    match v {
        Value::Map(m) => Ok(m),
        other => type_error(op, alloc::format!("expected an object, got {}", kind(other))),
    }
}

fn record(op: Op, v: Value) -> Result<dilf4s_core::Map> {
    match v {
        Value::Map(m) => Ok(m),
        other => type_error(op, alloc::format!("expected an object, got {}", kind(&other))),
    }
}

fn text_key(op: Op, v: Value) -> Result<Text> {
    match v {
        Value::Text(s) => Ok(s),
        other => type_error(op, alloc::format!("expected a string key, got {}", kind(&other))),
    }
}

fn raw_bytes(op: Op, v: Value) -> Result<Arc<[u8]>> {
    match v {
        Value::Bytes(b) => Ok(b),
        other => type_error(op, alloc::format!("expected bytes, got {}", kind(&other))),
    }
}

/// An integer index; out-of-range values saturate, since every use clamps to a collection size anyway.
fn index(op: Op, v: &Value) -> Result<i64> {
    match v {
        Value::Int(i) => Ok(i.to_i64().unwrap_or(if i.is_negative() { i64::MIN } else { i64::MAX })),
        other => type_error(op, alloc::format!("expected an integer, got {}", kind(other))),
    }
}

fn spread_all(vs: Vec<Value>) -> Vec<Value> {
    let mut out = Vec::with_capacity(vs.len());
    for v in vs {
        match v {
            Value::Array(a) => out.extend(a.into_items()),
            other => out.push(other),
        }
    }
    out
}

/// JavaScript `slice` bounds: a negative index counts from the end; the result is clamped to `[0, n]`.
fn bound(i: i64, n: usize) -> usize {
    let len = i64::try_from(n).unwrap_or(i64::MAX);
    let at = if i < 0 { len.saturating_add(i).max(0) } else { i.min(len) };
    usize::try_from(at).unwrap_or(0)
}

// ---- comparison and arithmetic

/// `<` and `<=` also take three arguments, meaning "between".
fn ordering(op: Op, args: &[Value], holds: fn(Ordering) -> bool, between: bool) -> Result<Value> {
    match args {
        [a, b] => compare(op, a, b).map(|c| Value::Bool(holds(c))),
        [a, b, c] if between => {
            let x = compare(op, a, b)?;
            let y = compare(op, b, c)?;
            Ok(Value::Bool(holds(x) && holds(y)))
        }
        _ => Err(JlvmError::arity(op.wire(), if between { "takes 2 or 3 arguments" } else { "takes 2 arguments" })),
    }
}

fn fold(op: Op, args: &[Value], f: impl Fn(Ratio, Ratio) -> Ratio) -> Result<Value> {
    let rs = args.iter().map(|v| numeric(op, v)).collect::<Result<Vec<_>>>()?;
    let r = rs.into_iter().reduce(f).ok_or_else(|| JlvmError::arity(op.wire(), "takes at least 1 argument"))?;
    num(op, &r)
}

/// The operands of `+` or `*`, each within the number bound.
pub(crate) fn operands(op: Op, args: &[Value]) -> Result<Vec<Ratio>> {
    args.iter().map(|v| numeric(op, v)).collect()
}

/// `+` or `*` of no operands.
pub(crate) fn identity(op: Op) -> Value {
    if op == Op::Mul { values::count(1) } else { values::count(0) }
}

/// One step of `+` or `*`: every intermediate is checked, so a fold never builds a number past the bound.
pub(crate) fn combine(op: Op, acc: &Ratio, x: &Ratio) -> Result<Ratio> {
    let r = if op == Op::Mul { acc.mul(x) } else { acc.add(x) };
    num(op, &r).map(|_| r)
}

/// `+` and `*` uncharged, from their identities (the machine folds them in charged steps instead).
fn total(op: Op, args: &[Value]) -> Result<Value> {
    let start = if op == Op::Mul { Ratio::one() } else { Ratio::zero() };
    let r = operands(op, args)?.iter().try_fold(start, |acc, x| combine(op, &acc, x))?;
    num(op, &r)
}

fn subtract(op: Op, args: &[Value]) -> Result<Value> {
    let rs = args.iter().map(|v| numeric(op, v)).collect::<Result<Vec<_>>>()?;
    match rs.as_slice() {
        [a] => num(op, &a.neg()),
        [a, b] => num(op, &a.sub(b)),
        other => Err(JlvmError::arity(op.wire(), alloc::format!("takes 1 or 2 arguments, got {}", other.len()))),
    }
}

/// An integer exponent in `-999..999`; the result's size is checked before it is computed.
fn power(op: Op, base: &Ratio, exponent: &Ratio) -> Result<Ratio> {
    let e = exponent
        .is_integer()
        .then(|| exponent.numerator().to_i64())
        .flatten()
        .filter(|e| e.unsigned_abs() <= MAX_EXPONENT)
        .ok_or_else(|| {
            JlvmError::arithmetic(
                op.wire(),
                alloc::format!("the exponent must be an integer in -{MAX_EXPONENT}..{MAX_EXPONENT}"),
            )
        })?;
    let bits = bit_length(base.numerator()).max(bit_length(base.denominator())).saturating_mul(e.unsigned_abs());
    if bits > MAX_NUMBER_BITS {
        return Err(JlvmError::limit(alloc::format!("{}: the result would exceed {MAX_NUMBER_BITS} bits", op.wire())));
    }
    base.pow(e).ok_or_else(|| division_by_zero(op))
}

// ---- text

/// A piece of `cat` or `join`, with its UTF-8 length: a string (shared, not copied), a number (exact decimal, paid for
/// by the rendering charge) or a boolean; null is empty.
fn piece(op: Op, v: Value) -> Result<(Text, u64)> {
    let rendered = |s: String| {
        let n = n_args(s.len());
        (Text::from(s), n)
    };
    match v {
        Value::Text(s) => {
            let n = n_args(s.len());
            Ok((s, n))
        }
        Value::Null => Ok(rendered(String::new())),
        Value::Bool(b) => Ok(rendered(String::from(if b { "true" } else { "false" }))),
        Value::Int(_) => Ok(rendered(integer(op, &v)?.to_string())),
        Value::Rational(_) => {
            let r = numeric(op, &v)?;
            let s = r.to_decimal_string().ok_or_else(|| {
                JlvmError::arithmetic(
                    op.wire(),
                    alloc::format!("{}/{} has no finite decimal form", r.numerator(), r.denominator()),
                )
            })?;
            Ok(rendered(s))
        }
        other => type_error(op, alloc::format!("cannot render {} as text", kind(&other))),
    }
}

/// The weight of the text `pieces` make with `separators` more bytes, saturating.
fn projected(pieces: &[(Text, u64)], separators: u64) -> u64 {
    pieces.iter().fold(1u64, |n, p| n.saturating_add(p.1)).saturating_add(separators)
}

/// The text of `pieces` joined by `separator`, once its weight `projected` has been checked: the only copy.
fn concatenated(pieces: &[(Text, u64)], separator: &str, projected: u64) -> String {
    let mut out = String::with_capacity(usize::try_from(projected).unwrap_or(0));
    for (i, (p, _)) in pieces.iter().enumerate() {
        if i > 0 {
            out.push_str(separator);
        }
        out.push_str(p);
    }
    out
}

/// A text result's weight, checked before the text is built.
fn fits(op: Op, weight: u64) -> Result<()> {
    if weight <= MAX_WEIGHT {
        Ok(())
    } else {
        Err(JlvmError::limit(alloc::format!("{}: the result would exceed weight {MAX_WEIGHT}", op.wire())))
    }
}

/// The values `cat` or `join` renders as decimal text: their numbers, priced before rendering.
pub(crate) fn rendered(op: Op, args: &[Value]) -> Vec<&Value> {
    fn numbers(vs: &[Value]) -> Vec<&Value> {
        vs.iter().filter(|v| values::is_number(v)).collect()
    }
    match (op, args) {
        (Op::Cat, _)
            if !args.is_empty()
                && (args.iter().all(|v| matches!(v, Value::Array(_)))
                    || args.iter().all(|v| matches!(v, Value::Bytes(_)))) =>
        {
            Vec::new()
        }
        (Op::Cat, _) => numbers(args),
        (Op::Join, [Value::Array(vs), _]) => numbers(vs.items()),
        _ => Vec::new(),
    }
}

/// Concatenates text; when every argument is an array, or every one is bytes, concatenates those instead.
fn cat(op: Op, args: Vec<Value>) -> Result<Value> {
    if !args.is_empty() && args.iter().all(|v| matches!(v, Value::Array(_))) {
        return values::array(op.wire(), spread_all(args));
    }
    if !args.is_empty() && args.iter().all(|v| matches!(v, Value::Bytes(_))) {
        // the size charge paid for every byte, so the concatenation is bounded by gas; its weight is checked once built,
        // as the reference's bytesValue does
        let mut out = Vec::new();
        for v in &args {
            if let Value::Bytes(b) = v {
                out.extend_from_slice(b);
            }
        }
        return bounded(op.wire(), Value::bytes(out));
    }
    let pieces = args.into_iter().map(|v| piece(op, v)).collect::<Result<Vec<_>>>()?;
    let weight = projected(&pieces, 0);
    fits(op, weight)?;
    text(op, concatenated(&pieces, "", weight))
}

/// `[text, start]` or `[text, start, length]`, counted in code points; a negative length drops from the end.
fn substr(op: Op, args: &[Value]) -> Result<Value> {
    match args {
        [s, start, rest @ ..] if rest.len() <= 1 => {
            let t = str_ref(op, s)?;
            let i = index(op, start)?;
            let len = rest.first().map(|l| index(op, l)).transpose()?;
            let n = t.chars().count();
            let from = bound(i, n);
            let to = len.map_or(n, |l| {
                if l < 0 {
                    bound(l, n)
                } else {
                    let room = n.saturating_sub(from);
                    from.saturating_add(usize::try_from(l).unwrap_or(usize::MAX).min(room))
                }
            });
            let taken = to.max(from).saturating_sub(from);
            text(op, t.chars().skip(from).take(taken).collect())
        }
        _ => Err(JlvmError::arity(op.wire(), "takes [text, start] or [text, start, length]")),
    }
}

/// Null on either side means "not present" and gives `false`.
fn affix(op: Op, args: &[Value]) -> Result<Value> {
    match args {
        [Value::Null, _] | [Value::Text(_), Value::Null] => Ok(Value::Bool(false)),
        [s, p] => {
            let a = str_ref(op, s)?;
            let b = str_ref(op, p)?;
            Ok(Value::Bool(if op == Op::StartsWith { a.starts_with(b) } else { a.ends_with(b) }))
        }
        _ => Err(JlvmError::arity(op.wire(), alloc::format!("takes 2 arguments, got {}", args.len()))),
    }
}

/// A `join` whose pieces are rendered and whose result fits: what remains is to pay for the separators and build it.
pub(crate) struct Joining {
    pieces: Vec<(Text, u64)>,
    separator: Text,
}

impl Joining {
    /// The separator bytes the result will copy.
    pub(crate) fn separator_bytes(&self) -> u64 {
        let n = n_args(self.pieces.len());
        if n == 0 { 0 } else { n_args(self.separator.len()).saturating_mul(n.saturating_sub(1)) }
    }
}

/// Validates and renders a `join`'s pieces, and rejects a result past the weight bound, without building it.
pub(crate) fn joining(args: Vec<Value>) -> Result<Joining> {
    let op = Op::Join;
    let n = args.len();
    let Ok([a, sep]) = <[Value; 2]>::try_from(args) else {
        return Err(JlvmError::arity(op.wire(), alloc::format!("takes 2 arguments, got {n}")));
    };
    let vs = arr(op, a)?;
    let separator = match sep {
        Value::Text(s) => s,
        other => return type_error(op, alloc::format!("expected a string, got {}", kind(&other))),
    };
    let pieces = vs.into_iter().map(|v| piece(op, v)).collect::<Result<Vec<_>>>()?;
    let j = Joining { pieces, separator };
    fits(op, projected(&j.pieces, j.separator_bytes()))?;
    Ok(j)
}

/// Builds a checked `join`.
pub(crate) fn joined(j: &Joining) -> Result<Value> {
    let weight = projected(&j.pieces, j.separator_bytes());
    text(Op::Join, concatenated(&j.pieces, &j.separator, weight))
}

fn split(op: Op, args: Vec<Value>) -> Result<Value> {
    binary(op, args, |a, b| {
        let t = str(op, a)?;
        let s = str(op, b)?;
        if s.is_empty() {
            return Err(JlvmError::domain(op.wire(), "the separator must not be empty"));
        }
        let parts = t.split(s.as_str()).map(|p| text(op, String::from(p))).collect::<Result<Vec<_>>>()?;
        values::array(op.wire(), parts)
    })
}

/// Code points of a string, elements of an array, bytes of a byte string, entries of an object.
fn length(op: Op, v: &Value) -> Result<Value> {
    match v {
        Value::Text(s) => Ok(count_usize(s.chars().count())),
        Value::Array(a) => Ok(count_usize(a.items().len())),
        Value::Bytes(b) => Ok(count_usize(b.len())),
        Value::Map(m) => Ok(count_usize(m.len())),
        other => type_error(op, alloc::format!("cannot take the length of {}", kind(other))),
    }
}

// ---- collections

/// Objects merge left to right (last wins); otherwise arrays are concatenated and other values appended.
fn merge(op: Op, args: Vec<Value>) -> Result<Value> {
    if args.iter().all(|v| matches!(v, Value::Map(_))) {
        // the first map as it is, then each later entry set over it (last wins) in O(log n), sharing the rest
        let limit = |e: dilf4s_core::CanonicalError| JlvmError::limit(alloc::format!("{}: {e}", op.wire()));
        let merged = args
            .into_iter()
            .try_fold(None::<dilf4s_core::Map>, |merged, v| match (merged, v) {
                (None, Value::Map(m)) => Ok(Some(m)),
                (Some(acc), Value::Map(m)) => {
                    m.entries().try_fold(acc, |acc, (k, x)| acc.updated(k.clone(), x.clone())).map(Some)
                }
                (merged, _) => Ok(merged),
            })
            .map_err(limit)?;
        let v = match merged {
            Some(m) => Value::Map(m),
            None => Value::map(Vec::new()).map_err(limit)?,
        };
        bounded(op.wire(), v)
    } else {
        values::array(op.wire(), spread_all(args))
    }
}

/// Substring, byte substring, or array membership; a null needle is never present.
fn contains(op: Op, needle: &Value, haystack: &Value) -> Result<Value> {
    match (needle, haystack) {
        (Value::Null, _) => Ok(Value::Bool(false)),
        (Value::Text(n), Value::Text(h)) => Ok(Value::Bool(values::contains_slice(h.as_bytes(), n.as_bytes()))),
        (Value::Bytes(n), Value::Bytes(h)) => Ok(Value::Bool(values::contains_slice(h, n))),
        (n, Value::Array(vs)) => Ok(Value::Bool(vs.items().contains(n))),
        (n, h) => type_error(op, alloc::format!("cannot look for {} in {}", kind(n), kind(h))),
    }
}

/// Whether every element of the first array is in the second; null is the empty set.
fn subset(op: Op, sub: &Value, sup: &Value) -> Result<Value> {
    match (sub, sup) {
        (Value::Null, _) => Ok(Value::Bool(true)),
        (Value::Array(_), Value::Null) => Ok(Value::Bool(false)),
        (a, b) => {
            let xs = arr_ref(op, a)?;
            let ys = arr_ref(op, b)?;
            values::subset(op, xs, ys).map(Value::Bool)
        }
    }
}

/// `[array, start]` or `[array, start, end]`, with JavaScript bounds.
fn slice(op: Op, args: Vec<Value>) -> Result<Value> {
    let n = args.len();
    if !(2..=3).contains(&n) {
        return Err(JlvmError::arity(op.wire(), "takes [array, start] or [array, start, end]"));
    }
    let mut it = args.into_iter();
    let a = it.next().unwrap_or(Value::Null);
    let s = it.next().unwrap_or(Value::Null);
    let e = it.next();
    let vs = arr(op, a)?;
    let from = index(op, &s)?;
    let to = e.as_ref().map(|e| index(op, e)).transpose()?;
    let i = bound(from, vs.len());
    let j = to.map_or(vs.len(), |t| bound(t, vs.len())).max(i);
    values::array(op.wire(), vs.into_iter().skip(i).take(j.saturating_sub(i)).collect())
}

// ---- objects

/// Keys, values or `[key, value]` pairs in code-point key order; null gives null.
fn listing(op: Op, args: Vec<Value>) -> Result<Value> {
    unary(op, args, |v| {
        if v == Value::Null {
            return Ok(Value::Null);
        }
        let m = record(op, v)?;
        // the map keeps canonical order (shorter keys first); these list in code-point order, the order of UTF-8
        // bytes. Keys and values are shared, not copied
        let mut entries: Vec<(&Text, &Value)> = m.entries().collect();
        entries.sort_by(|a, b| a.0.as_str().cmp(b.0.as_str()));
        let key = |k: &Text| bounded(op.wire(), Value::Text(k.clone()));
        match op {
            Op::Keys => {
                let ks = entries.into_iter().map(|(k, _)| key(k)).collect::<Result<Vec<_>>>()?;
                values::array(op.wire(), ks)
            }
            Op::Values => values::array(op.wire(), entries.into_iter().map(|(_, x)| x.clone()).collect()),
            _ => {
                let pairs = entries
                    .into_iter()
                    .map(|(k, x)| key(k).and_then(|t| values::array(op.wire(), alloc::vec![t, x.clone()])))
                    .collect::<Result<Vec<_>>>()?;
                values::array(op.wire(), pairs)
            }
        }
    })
}

fn get(op: Op, args: &[Value]) -> Result<Value> {
    match args {
        [m, k, rest @ ..] if rest.len() <= 1 => {
            let o = obj(op, m)?;
            let key = str_ref(op, k)?;
            Ok(o.get(key).or_else(|| rest.first()).cloned().unwrap_or(Value::Null))
        }
        _ => Err(JlvmError::arity(op.wire(), "takes [object, key] or [object, key, default]")),
    }
}

/// Setting a key to null removes it: canonical maps never hold nulls.
fn set(op: Op, args: Vec<Value>) -> Result<Value> {
    let n = args.len();
    let Ok([m, k, v]) = <[Value; 3]>::try_from(args) else {
        return Err(JlvmError::arity(op.wire(), alloc::format!("takes [object, key, value], got {n} arguments")));
    };
    let o = record(op, m)?;
    let key = text_key(op, k)?;
    // O(log n), sharing every untouched subtree; a null value removes the key
    let updated = o.updated(key, v).map_err(|e| JlvmError::limit(alloc::format!("{}: {e}", op.wire())))?;
    bounded(op.wire(), Value::Map(updated))
}

// ---- bytes

/// Bytes, or text of the form `0x` + lowercase hex.
fn hex_bytes(op: Op, v: Value) -> Result<Arc<[u8]>> {
    match v {
        Value::Bytes(b) => Ok(b),
        Value::Text(s) => values::hex_bytes(&s).map(Arc::from).ok_or_else(|| {
            JlvmError::domain(op.wire(), "expected 0x followed by an even number of lowercase hex digits")
        }),
        other => type_error(op, alloc::format!("expected bytes or hex text, got {}", kind(&other))),
    }
}

/// Hashes bytes, or the UTF-8 encoding of a string.
fn digest(op: Op, h: HashAlgorithm, v: &Value) -> Result<Value> {
    match v {
        Value::Bytes(b) => Ok(Value::bytes(h.digest(b))),
        Value::Text(s) => Ok(Value::bytes(h.digest(s.as_bytes()))),
        other => type_error(op, alloc::format!("expected bytes or a string, got {}", kind(other))),
    }
}

/// A big-endian unsigned integer, within the number bound.
fn unsigned_int(op: Op, b: &[u8]) -> Result<Value> {
    let significant = b.iter().skip_while(|&&x| x == 0).count();
    if n_args(significant).saturating_mul(8) > MAX_NUMBER_BITS {
        return Err(values::too_large(op));
    }
    values::int(op, BigInt::from_bytes_be(Sign::Plus, b))
}

/// Big-endian and unsigned: minimal (`0` is empty), or left-padded to a fixed width.
fn int_to_bytes(op: Op, args: &[Value]) -> Result<Value> {
    let unsigned = |v: &Value| {
        let i = integer(op, v)?;
        if i.is_negative() {
            return Err(JlvmError::domain(op.wire(), "negative integer"));
        }
        Ok(if i.is_zero() { Vec::new() } else { i.to_bytes_be().1 })
    };
    let width = |v: &Value| {
        let n = integer(op, v)?;
        n.to_u64()
            .filter(|w| *w <= MAX_INT_BYTES)
            .ok_or_else(|| JlvmError::domain(op.wire(), alloc::format!("the width must be in 0..{MAX_INT_BYTES}")))
    };
    match args {
        [i] => unsigned(i).map(Value::bytes),
        [i, n] => {
            let raw = unsigned(i)?;
            let w = width(n)?;
            let size = n_args(raw.len());
            if size > w {
                return Err(JlvmError::domain(op.wire(), alloc::format!("the integer does not fit in {w} bytes")));
            }
            let pad = usize::try_from(w.saturating_sub(size)).unwrap_or(0);
            let mut out = alloc::vec![0u8; pad];
            out.extend(raw);
            Ok(Value::bytes(out))
        }
        _ => Err(JlvmError::arity(op.wire(), "takes [integer] or [integer, width]")),
    }
}
