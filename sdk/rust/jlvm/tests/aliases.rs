//! The surface's aliases (jlvm.md section 1) parse to their operations' own names, as the reference's `Ops.named`
//! reads them: the generated `Op::from_wire` takes own names only, so the parser asks `OpClass::from_name`.

#![allow(clippy::panic, reason = "tests fail by panicking")]

use dilf4s_jlvm::{Op, OpClass, parse};

#[test]
fn each_alias_parses_as_its_operation() {
    let m = r#"{"var": "m"}"#;
    for (alias, own, args) in [
        ("getKey", "get", format!("[{m}, \"k\"]")),
        ("setKey", "set", format!("[{m}, \"k\", 1]")),
        ("deleteKey", "unset", format!("[{m}, \"k\"]")),
        ("size", "length", String::from("[{\"var\": \"xs\"}]")),
    ] {
        let a = parse(&format!("{{\"{alias}\": {args}}}")).unwrap_or_else(|e| panic!("{alias}: {e}"));
        let b = parse(&format!("{{\"{own}\": {args}}}")).unwrap_or_else(|e| panic!("{own}: {e}"));
        assert_eq!(a, b, "{alias} is {own}");
        assert_eq!(Op::from_name(alias).map(Op::wire), Some(own));
        assert_eq!(Op::from_wire(alias), None, "the canonical form names {own}, never {alias}");
    }
}
