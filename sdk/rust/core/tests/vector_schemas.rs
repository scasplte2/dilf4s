//! The generated models hold every vector file: decode then encode gives back the same canonical value, so nothing in
//! a vector file escapes its schema. This exercises the Rust emitter on records, unions with text alternatives,
//! literals, ranges and recursion.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    reason = "tests fail by panicking, and run with std"
)]

mod common;

use std::collections::BTreeSet;

use common::{read_vectors, vectors_dir};
use dilf4s_core::codec::Model;
use dilf4s_core::generated::{
    canonical_vectors, crypto, jlvm, jlvm_vectors, kes_vectors, merkle, merkle_vectors, poseidon_vectors,
    sigma_vectors, signing_vectors, trie_vectors, vrf_vectors,
};
use dilf4s_core::{Value, json};

fn round_trip<T: Model>(value: &Value) -> Result<Value, String> {
    let decoded = T::from_value(value).map_err(|e| e.to_string())?;
    decoded.to_value().map_err(|e| e.to_string())
}

type Check = fn(&Value) -> Result<Value, String>;

const SCHEMAS: [(&str, Check); 10] = [
    ("canonical_encoding_v1.json", round_trip::<canonical_vectors::CanonicalVectors>),
    ("jlvm_v1.json", round_trip::<jlvm_vectors::JlvmVectors>),
    ("kes_v1.json", round_trip::<kes_vectors::KesVectors>),
    ("merkle_v1.json", round_trip::<merkle_vectors::MerkleVectors>),
    ("poseidon_v1.json", round_trip::<poseidon_vectors::PoseidonVectors>),
    ("sigma_dilf4s_v1.json", round_trip::<sigma_vectors::SigmaVectors>),
    ("sigma_v1.json", round_trip::<sigma_vectors::SigmaVectors>),
    ("signing_v1.json", round_trip::<signing_vectors::SigningVectors>),
    ("trie_v1.json", round_trip::<trie_vectors::TrieVectors>),
    ("vrf_v1.json", round_trip::<vrf_vectors::VrfVectors>),
];

#[test]
fn every_vector_file_has_a_schema() {
    let present: BTreeSet<String> = std::fs::read_dir(vectors_dir())
        .expect("shared/vectors")
        .filter_map(|e| e.ok().map(|e| e.file_name().to_string_lossy().into_owned()))
        .filter(|f| std::path::Path::new(f).extension().is_some_and(|x| x == "json"))
        .collect();
    let known: BTreeSet<String> = SCHEMAS.iter().map(|(f, _)| (*f).to_owned()).collect();
    assert_eq!(present, known);
}

#[test]
fn every_vector_file_round_trips_through_its_type() {
    for (file, check) in SCHEMAS {
        let value = json::parse(&read_vectors(file)).unwrap_or_else(|e| panic!("{file}: {e}"));
        let again = check(&value).unwrap_or_else(|e| panic!("{file}: {e}"));
        assert_eq!(json::render(&again), json::render(&value), "{file} changes on a round trip");
    }
}

#[test]
fn a_schema_violation_names_the_failing_path() {
    let Value::Map(file) = json::parse(&read_vectors("merkle_v1.json")).expect("JSON") else { panic!("a map") };
    let not_hex = Value::array(vec![Value::text("zz")]).expect("an array");
    let entries =
        file.into_entries().into_iter().map(|(k, v)| if k == "leaves" { (k, not_hex.clone()) } else { (k, v) });
    let value = Value::map(entries).expect("a map");
    let failure = merkle_vectors::MerkleVectors::from_value(&value).expect_err("not hex");
    assert_eq!(failure.path, vec!["leaves".to_owned(), "[0]".to_owned()], "{failure}");
}

#[test]
fn unions_select_by_keys_and_text() {
    use trie_vectors::{TrieLookupResult, TriePresent};
    let present = TrieLookupResult::TriePresent(TriePresent { present: "00ff".to_owned() });
    let value = present.to_value().expect("encodes");
    assert_eq!(json::render(&value).expect("renders"), r#"{"present":"00ff"}"#);
    assert_eq!(TrieLookupResult::from_value(&value), Ok(present));
    let absent = TrieLookupResult::Absent.to_value().expect("encodes");
    assert_eq!(json::render(&absent).expect("renders"), r#""absent""#);
    assert_eq!(TrieLookupResult::from_value(&Value::text("notCovered")), Ok(TrieLookupResult::NotCovered));
    assert!(TrieLookupResult::from_value(&Value::text("other")).is_err());
}

#[test]
fn an_encoder_refuses_what_its_decoder_would_reject() {
    let inclusion = merkle::MerkleInclusion { index: 1 << 63, size: 1, path: vec![] };
    assert!(inclusion.to_value().is_err(), "tree indices stop at 2^63 - 1");
    let vector = canonical_vectors::CanonicalDigest {
        algorithm: "SHA-256".to_owned(),
        message: "0A".to_owned(),
        digest: String::new(),
    };
    assert!(vector.to_value().is_err(), "hex is lowercase");
    let signature = Value::bytes(vec![0u8; 63]);
    assert!(dilf4s_core::codec::Codec::decode(&crypto::ED25519_SIGNATURE, &signature).is_err());
}

#[test]
fn a_recursive_model_round_trips_through_cbor() {
    let program = r#"{"jlvm":1,"expr":{"op":"+","args":[{"lit":1},{"var":"x","default":{"path":{"lit":"y"}}},{"map":{"k":{}}}]}}"#;
    let parsed = jlvm::JlvmProgram::from_json(program).expect("a program");
    let bytes = parsed.to_cbor().expect("encodes");
    assert_eq!(jlvm::JlvmProgram::from_cbor(&bytes), Ok(parsed));
}
