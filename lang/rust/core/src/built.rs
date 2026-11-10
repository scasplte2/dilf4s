//! The `built-value` recipes of the vector schemas (`shared/schemas/refusal-vectors.cddl`, `jlvm-vectors.cddl`): a
//! value with shared parts, far heavier than its description, given as a leaf and steps. Canonical CBOR cannot share
//! structure, so a vector file describes such a value this way, and every reader builds it with this one function.
//!
//! A step [`Step::Wrap`] replaces, `times` times over, every text `"$"` in its template by the value so far; a step
//! [`Step::Copies`] makes an array of `n` of it. Every occurrence is the one value (an `Arc` clone), so building costs
//! the recipe and the copies' array, never the weight. Nothing here encodes the built value. Never recursive: a
//! template is rebuilt with an explicit stack.

use alloc::string::String;
use alloc::vec;
use alloc::vec::Vec;

use crate::cbor;
use crate::error::CanonicalError;
use crate::value::Value;

/// A step, its template as canonical CBOR.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Step<'a> {
    /// Replace every text `"$"` in the template by the value so far, `times` times over.
    Wrap {
        /// The template's canonical CBOR.
        template: &'a [u8],
        /// How many times.
        times: u64,
    },
    /// An array of `n` of the value so far.
    Copies(u64),
}

/// Why a recipe does not build: a leaf or template that is not canonical CBOR, a value past the depth limit, or more
/// copies than this platform can index.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BuildError {
    /// Not canonical CBOR, or a built value past a canonical limit.
    Canonical(CanonicalError),
    /// More copies than a `usize` holds.
    TooManyCopies(u64),
}

impl core::fmt::Display for BuildError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Canonical(e) => e.fmt(f),
            Self::TooManyCopies(n) => write!(f, "{n} copies"),
        }
    }
}

impl core::error::Error for BuildError {}

impl From<CanonicalError> for BuildError {
    fn from(e: CanonicalError) -> Self {
        Self::Canonical(e)
    }
}

/// The value a recipe describes: its leaf (canonical CBOR), then each step in order.
pub fn build(leaf: &[u8], steps: &[Step<'_>]) -> Result<Value, BuildError> {
    steps.iter().try_fold(cbor::decode(leaf)?, |v, step| match step {
        Step::Wrap { template, times } => {
            let t = cbor::decode(template)?;
            (0..*times).try_fold(v, |x, _| fill(&t, &x).map_err(BuildError::from))
        }
        Step::Copies(n) => {
            let n = usize::try_from(*n).map_err(|_| BuildError::TooManyCopies(*n))?;
            Ok(Value::array(vec![v; n])?)
        }
    })
}

/// A node of `template` still to rebuild, or a container whose parts are rebuilt and waiting on the results stack.
enum Frame<'t> {
    Enter(&'t Value),
    Array(usize),
    Map(Vec<String>),
}

/// `template` with every text `"$"` replaced by `shared`, rebuilt bottom-up with an explicit stack.
fn fill(template: &Value, shared: &Value) -> Result<Value, CanonicalError> {
    let mut todo = vec![Frame::Enter(template)];
    let mut done: Vec<Value> = Vec::new();
    while let Some(frame) = todo.pop() {
        match frame {
            Frame::Enter(Value::Text(t)) if t.as_str() == "$" => done.push(shared.clone()),
            Frame::Enter(Value::Array(a)) => {
                todo.push(Frame::Array(a.len()));
                todo.extend(a.items().iter().rev().map(Frame::Enter));
            }
            Frame::Enter(Value::Map(m)) => {
                todo.push(Frame::Map(m.entries().map(|(k, _)| String::from(k.as_str())).collect()));
                let values: Vec<&Value> = m.entries().map(|(_, v)| v).collect();
                todo.extend(values.into_iter().rev().map(Frame::Enter));
            }
            Frame::Enter(other) => done.push(other.clone()),
            Frame::Array(n) => {
                let parts = done.split_off(done.len().saturating_sub(n));
                done.push(Value::array(parts)?);
            }
            Frame::Map(keys) => {
                let parts = done.split_off(done.len().saturating_sub(keys.len()));
                done.push(Value::map(keys.into_iter().zip(parts))?);
            }
        }
    }
    done.pop().ok_or_else(|| CanonicalError::TypeMismatch { reason: String::from("an empty template") })
}

#[cfg(test)]
mod tests {
    use alloc::string::String;
    use alloc::vec;

    use super::{Step, build};
    use crate::cbor;
    use crate::value::Value;

    #[test]
    fn doubling_shares_and_templates_place_the_value() {
        let pair = cbor::encode(&Value::array(vec![Value::text("$"), Value::text("$")]).unwrap());
        let v = build(&cbor::encode(&Value::Null), &[Step::Wrap { template: &pair, times: 40 }]).unwrap();
        assert_eq!(v.weight(), (1u64 << 41) - 1, "2^40 nulls and 2^40 - 1 arrays, built in 40 steps");
        let map = cbor::encode(&Value::map([(String::from("k"), Value::text("$"))]).unwrap());
        let wrapped = build(&cbor::encode(&Value::int(7)), &[Step::Wrap { template: &map, times: 1 }, Step::Copies(3)]);
        let expected = Value::array(vec![Value::map([(String::from("k"), Value::int(7))]).unwrap(); 3]).unwrap();
        assert_eq!(wrapped.unwrap(), expected);
    }
}
