//! BIP-39 accepts only code points Unicode 15.0 assigns (signing.md section 2.1). The table is
//! `shared/unicode/assigned-15.0.txt`; the vectors pin the refusals.

use dilf4s_signing::keys::unicode15::{ASSIGNED, assigned};

#[test]
fn the_table_is_the_shared_file() {
    let path = concat!(env!("CARGO_MANIFEST_DIR"), "/../../../shared/unicode/assigned-15.0.txt");
    let text = std::fs::read_to_string(path).expect("the shared file");
    let ranges: Vec<(u32, u32)> = text
        .lines()
        .filter(|l| !l.starts_with('#') && !l.trim().is_empty())
        .map(|l| {
            let (a, b) = l.split_once("..").expect("a range");
            (u32::from_str_radix(a, 16).expect("hex"), u32::from_str_radix(b, 16).expect("hex"))
        })
        .collect();
    assert_eq!(ranges.len(), 707, "707 ranges");
    assert_eq!(ranges.as_slice(), ASSIGNED.as_slice(), "the table is the shared file");
}

#[test]
fn assigned_in_15_0_and_not() {
    for cp in [0x41, 0x1e030, 0x1d400, 0xfb01, 0xe000, 0x1f6dc, 0x0010_fffd] {
        assert!(assigned(char::from_u32(cp).expect("a char")), "U+{cp:04X} is assigned in 15.0");
    }
    for cp in [0x1ccd6, 0xfdd0, 0xfffe, 0x0010_ffff] {
        assert!(!assigned(char::from_u32(cp).expect("a char")), "U+{cp:04X} is not assigned in 15.0");
    }
}
