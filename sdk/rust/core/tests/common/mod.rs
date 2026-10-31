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
