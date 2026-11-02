//! Reading the repository's `shared/vectors` at test time.

#![allow(dead_code, reason = "each test crate uses some of these")]
#![allow(clippy::panic, reason = "tests fail by panicking")]

use std::path::PathBuf;

use dilf4s_core::codec::Model;
use dilf4s_core::{hex, json};

/// The repository's `shared/vectors`.
pub fn vectors_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../shared/vectors")
}

/// The text of one vector file.
pub fn read_vectors(file: &str) -> String {
    std::fs::read_to_string(vectors_dir().join(file)).unwrap_or_else(|e| panic!("{file}: {e}"))
}

/// A vector file parsed with this SDK's JSON parser and decoded into its generated type.
pub fn load<T: Model>(file: &str) -> T {
    let value = json::parse(&read_vectors(file)).unwrap_or_else(|e| panic!("{file} is not canonical JSON: {e}"));
    T::from_value(&value).unwrap_or_else(|e| panic!("{file} does not fit its schema: {e}"))
}

/// Bytes from vector hex.
pub fn bytes(text: &str) -> Vec<u8> {
    hex::decode(text).unwrap_or_else(|e| panic!("{text}: {e}"))
}

/// A step of a `built-value` recipe (shared/schemas/refusal-vectors.cddl).
pub enum Step<'a> {
    /// Replace every text `"$"` in the template (canonical CBOR hex) by the value so far, `times` times over.
    Wrap {
        /// The template.
        template: &'a str,
        /// How many times.
        times: u64,
    },
    /// An array of `n` of the value so far.
    Copies(u64),
}

/// A `built-value`: every occurrence is the one value, shared, so building costs the recipe (and `n` for copies),
/// never the weight. Nothing here encodes the built value.
pub fn build(leaf: &str, steps: &[Step<'_>]) -> dilf4s_core::value::Value {
    use dilf4s_core::cbor;
    let decode = |h: &str| cbor::decode(&bytes(h)).unwrap_or_else(|e| panic!("{h}: {e}"));
    steps.iter().fold(decode(leaf), |v, step| match step {
        Step::Wrap { template, times } => {
            let t = decode(template);
            (0..*times).fold(v, |x, _| fill(&t, &x))
        }
        Step::Copies(n) => {
            let n = usize::try_from(*n).unwrap_or_else(|e| panic!("{n} copies: {e}"));
            dilf4s_core::value::Value::array(vec![v; n]).unwrap_or_else(|e| panic!("copies: {e}"))
        }
    })
}

/// `template` with every text `"$"` replaced by `v`; templates are a few levels deep.
fn fill(template: &dilf4s_core::value::Value, v: &dilf4s_core::value::Value) -> dilf4s_core::value::Value {
    use dilf4s_core::value::Value;
    match template {
        Value::Text(t) if t.as_str() == "$" => v.clone(),
        Value::Array(a) => {
            Value::array(a.items().iter().map(|x| fill(x, v)).collect()).unwrap_or_else(|e| panic!("fill: {e}"))
        }
        Value::Map(m) => Value::map(m.entries().map(|(k, x)| (String::from(k.as_str()), fill(x, v))))
            .unwrap_or_else(|e| panic!("fill: {e}")),
        other => other.clone(),
    }
}
