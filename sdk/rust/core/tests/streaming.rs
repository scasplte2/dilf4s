//! Content hashes and commitments streamed into the hash equal the old path, which builds the whole encoding first and
//! is kept here only as the oracle: for edge cases of every major type and for generated values, under every hash.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects,
    clippy::as_conversions,
    clippy::cast_possible_truncation,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    reason = "tests fail by panicking, and run with std"
)]

use dilf4s_core::commitment::{value_commitment, value_commitment_into};
use dilf4s_core::hash::{
    Blake2b256, Blake2b512, DomainTag, HashAlgorithm, Hasher, Hashing, Keccak256, Sha3_256, Sha3_512, Sha256, Sha512,
    TaggedHash,
};
use dilf4s_core::{BigInt, MAX_DEPTH, Value, cbor};

fn tag() -> DomainTag {
    DomainTag::new("dilf4s/test/streaming/v1").unwrap()
}

/// Streamed equals buffered for `value` under `H`: the content hash, the commitment's tagged hash, and the
/// commitment's plain hash.
fn check<H: Hasher>(name: &str, value: &Value) {
    let t = TaggedHash::<H>::new(tag());
    let encoded = cbor::encode(value);
    assert_eq!(t.content(value), t.hash(&encoded), "{name}: content hash under {}", H::NAME);
    let commitment = value_commitment(value);
    let mut streamed = t.start();
    value_commitment_into(&mut streamed, value);
    assert_eq!(streamed.finish(), t.hash(&commitment), "{name}: commitment hash under {}", H::NAME);
    let mut plain = Hashing::<H>::new();
    plain.update_value(value);
    assert_eq!(plain.finish().as_bytes(), H::hash_parts(&[&encoded]).as_ref(), "{name}: plain hash under {}", H::NAME);
}

fn check_all(name: &str, value: &Value) {
    check::<Sha256>(name, value);
    check::<Sha512>(name, value);
    check::<Sha3_256>(name, value);
    check::<Sha3_512>(name, value);
    check::<Keccak256>(name, value);
    check::<Blake2b256>(name, value);
    check::<Blake2b512>(name, value);
    for a in HashAlgorithm::ALL {
        assert_eq!(
            a.content_hash(&tag(), value),
            a.tagged_hash(&tag(), &cbor::encode(value)),
            "{name}: content_hash under {}",
            a.name()
        );
    }
}

fn nested(depth: usize, leaf: Value, map: bool) -> Value {
    (1..depth).fold(leaf, |v, i| {
        if map && i % 2 == 0 { Value::map([(String::from("k"), v)]).unwrap() } else { Value::array(vec![v]).unwrap() }
    })
}

fn edge_cases() -> Vec<(String, Value)> {
    let mut cases: Vec<(String, Value)> = vec![
        ("null".into(), Value::Null),
        ("false".into(), Value::Bool(false)),
        ("true".into(), Value::Bool(true)),
        ("empty text".into(), Value::text("")),
        ("empty bytes".into(), Value::bytes(Vec::new())),
        ("empty array".into(), Value::array(Vec::new()).unwrap()),
        ("empty map".into(), Value::map(Vec::new()).unwrap()),
        ("non-ascii text".into(), Value::text("é\u{1F600}\u{FFFF}\u{E000}")),
        ("rational".into(), Value::number(&BigInt::from(-7), &BigInt::from(3)).unwrap()),
        (
            "big rational".into(),
            Value::number(&((BigInt::from(1) << 200u32) + 1), &((BigInt::from(1) << 70u32) + 3)).unwrap(),
        ),
        ("long text".into(), Value::text("x".repeat(70_000))),
        ("long bytes".into(), Value::bytes(vec![0xab; 70_000])),
        ("deep arrays".into(), nested(MAX_DEPTH, Value::int(1), false)),
        ("deep maps and arrays".into(), nested(MAX_DEPTH, Value::text("leaf"), true)),
        (
            "keys of every length order".into(),
            Value::map(
                ["", "a", "b", "aa", "z", "zz", "é", "\u{1F600}", "\u{FFFF}", "\u{E000}"]
                    .iter()
                    .enumerate()
                    .map(|(i, k)| (String::from(*k), Value::int(i as u64))),
            )
            .unwrap(),
        ),
    ];
    // every head-size boundary, as an integer (both signs), a length of text and bytes, and a count of an array
    for n in [0u64, 23, 24, 255, 256, 65_535, 65_536, (1 << 32) - 1, 1 << 32, u64::MAX] {
        cases.push((format!("int {n}"), Value::int(n)));
        cases.push((format!("int -1-{n}"), Value::int(-1i128 - i128::from(n))));
    }
    let staged = cbor::STAGED_BYTES;
    for n in [23usize, 24, 255, 256, staged - 1, staged, staged + 1, 65_535, 65_536] {
        cases.push((format!("text of {n}"), Value::text("t".repeat(n))));
        cases.push((format!("bytes of {n}"), Value::bytes(vec![7u8; n])));
        cases.push((format!("array of {n}"), Value::array(vec![Value::Null; n]).unwrap()));
    }
    // bignums around 2^64, and around a whole number of limbs
    for bits in [64u32, 65, 127, 128, 129, 256] {
        let p = BigInt::from(1) << bits;
        for n in [p.clone() - 1, p.clone(), p.clone() + 1, -p.clone() - 1, -p.clone(), -p + 1] {
            cases.push((format!("bignum {n}"), Value::integer(n).unwrap()));
        }
    }
    cases
}

#[test]
fn edge_cases_stream_as_they_buffer() {
    let cases = edge_cases();
    for (name, v) in &cases {
        check_all(name, v);
    }
    eprintln!("streaming: {} edge cases", cases.len());
}

/// A deterministic generator (xorshift64*), so the generated values are the same on every run.
struct Gen(u64);

impl Gen {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 >> 12;
        self.0 ^= self.0 << 25;
        self.0 ^= self.0 >> 27;
        self.0.wrapping_mul(0x2545_f491_4f6c_dd1d)
    }

    fn below(&mut self, n: u64) -> u64 {
        self.next() % n
    }

    fn text(&mut self) -> String {
        const ALPHABET: [char; 8] = ['a', 'b', 'z', 'é', '\u{FFFF}', '\u{E000}', '\u{1F600}', '0'];
        (0..self.below(6)).map(|_| ALPHABET[self.below(8) as usize]).collect()
    }

    fn value(&mut self, depth: usize) -> Value {
        let scalar = depth == 0 || self.below(3) == 0;
        match (scalar, self.below(8)) {
            (true, 0) => Value::Null,
            (true, 1) => Value::Bool(self.below(2) == 0),
            (true, 2) => Value::int(self.next() >> self.below(64)),
            (true, 3) => Value::integer(-(BigInt::from(self.next()) << self.below(130))).unwrap(),
            (true, 4) => {
                Value::number(&BigInt::from(self.next().cast_signed()), &BigInt::from(self.below(1000) + 1)).unwrap()
            }
            (true, 5) => Value::text(self.text()),
            (true, _) => Value::bytes((0..self.below(40)).map(|_| self.next() as u8).collect::<Vec<u8>>()),
            (false, k) if k % 2 == 0 => {
                Value::array((0..self.below(5)).map(|_| self.value(depth - 1)).collect()).unwrap()
            }
            (false, _) => {
                // a repeated key keeps the last value (a map refuses duplicates); null values are dropped
                let entries: std::collections::BTreeMap<String, Value> =
                    (0..self.below(5)).map(|_| (self.text(), self.value(depth - 1))).collect();
                Value::map(entries).unwrap()
            }
        }
    }
}

#[test]
fn generated_values_stream_as_they_buffer() {
    let mut g = Gen(0x9e37_79b9_7f4a_7c15);
    let n = 2_000;
    for i in 0..n {
        let v = g.value(6);
        check_all(&format!("generated #{i}"), &v);
    }
    eprintln!("streaming: {n} generated values");
}
