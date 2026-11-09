//! Generated constants: `u64` values, the same values in the narrower types they fit, and a size bound that names a
//! constant.

use dilf4s_core::Value;
use dilf4s_core::codec::Codec as _;
use dilf4s_fixtures::generated::fixture_limits;

#[test]
fn constants_are_the_schema_values_in_every_type_they_fit() {
    assert_eq!(fixture_limits::MAX_SMALL, 1024);
    assert_eq!(fixture_limits::MAX_WIDE, u64::MAX);
    assert_eq!(fixture_limits::as_u16::MAX_SMALL, 1024);
    assert_eq!(fixture_limits::as_u32::MAX_SMALL, 1024);
    assert_eq!(fixture_limits::as_usize::MAX_SMALL, 1024);
}

#[test]
fn a_size_bound_names_the_constant() {
    let max = fixture_limits::as_usize::MAX_SMALL;
    assert!(fixture_limits::BOUNDED.decode(&Value::bytes(vec![0; max])).is_ok());
    assert!(fixture_limits::BOUNDED.decode(&Value::bytes(vec![0; max + 1])).is_err());
}
