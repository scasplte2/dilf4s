//! Aliases and fields whose types live in another module compile and round-trip.

#![allow(clippy::expect_used, reason = "tests fail by panicking")]

use dilf4s_core::codec::{Codec, Model};
use dilf4s_fixtures::generated::{fixture_a, fixture_b};

#[test]
fn a_record_through_cross_module_aliases_round_trips() {
    let items = vec![fixture_a::Item { x: 1u8.into() }, fixture_a::Item { x: 2u8.into() }];
    let holder = fixture_b::Holder { r: items.clone(), s: items, t: Some(fixture_a::Item { x: 3u8.into() }) };
    let bytes = holder.to_cbor().expect("encodes");
    assert_eq!(fixture_b::Holder::from_cbor(&bytes), Ok(holder));
}

#[test]
fn an_alias_of_another_modules_alias_uses_its_codec() {
    let value = fixture_b::ITEMS_AGAIN.encode(&vec![fixture_a::Item { x: 7u8.into() }]).expect("encodes");
    assert_eq!(fixture_a::ITEMS.decode(&value).map(|rs| rs.len()), Ok(1));
}
