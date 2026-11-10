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

/// A step of a `built-value` recipe (shared/schemas/refusal-vectors.cddl), its template as canonical CBOR hex.
pub enum Step<'a> {
    /// Replace every text `"$"` in the template by the value so far, `times` times over.
    Wrap {
        /// The template.
        template: &'a str,
        /// How many times.
        times: u64,
    },
    /// An array of `n` of the value so far.
    Copies(u64),
}

/// A `built-value`, built by [`dilf4s_core::built::build`] (the one implementation every reader uses).
pub fn build(leaf: &str, steps: &[Step<'_>]) -> dilf4s_core::value::Value {
    use dilf4s_core::built;
    let templates: Vec<Vec<u8>> = steps
        .iter()
        .map(|s| match s {
            Step::Wrap { template, .. } => bytes(template),
            Step::Copies(_) => Vec::new(),
        })
        .collect();
    let steps: Vec<built::Step<'_>> = steps
        .iter()
        .zip(&templates)
        .map(|(s, t)| match s {
            Step::Wrap { times, .. } => built::Step::Wrap { template: t, times: *times },
            Step::Copies(n) => built::Step::Copies(*n),
        })
        .collect();
    built::build(&bytes(leaf), &steps).unwrap_or_else(|e| panic!("{leaf}: {e}"))
}
