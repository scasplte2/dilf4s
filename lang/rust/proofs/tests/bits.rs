//! Bit strings and the trie's key order.

#![allow(clippy::unwrap_used, clippy::std_instead_of_core, reason = "tests fail by panicking, and run with std")]

use std::cmp::Ordering;

use dilf4s_proofs::Bits;

fn b(s: &str) -> Bits {
    let mut out = Bits::new();
    for c in s.chars() {
        out.push(c == '1');
    }
    out
}

#[test]
fn padding_and_truncation() {
    assert!(Bits::from_padded(3, vec![0b1010_0000]).is_some());
    assert!(Bits::from_padded(3, vec![0b1010_1000]).is_none());
    assert!(Bits::from_padded(8, vec![0xff]).is_some());
    assert!(Bits::from_padded(0, vec![]).is_some());
    assert!(Bits::from_padded(0, vec![0]).is_none());
    let mut x = b("1011011101");
    x.truncate(3);
    assert_eq!(x, b("101"));
    assert_eq!(x.padded_bytes(), &[0b1010_0000]);
    x.extend(&b("11111"));
    assert_eq!(x, b("10111111"));
    assert_eq!(b("101").concat(&b("01")).slice(1, 4), b("010"));
    assert_eq!(Bits::from_bytes(&[0x80]).get(0), Some(true));
    assert_eq!(Bits::from_bytes(&[0x80]).get(8), None);
}

#[test]
fn key_order() {
    let cases = [
        ("", "0", Ordering::Less),
        ("0", "1", Ordering::Less),
        ("01", "0", Ordering::Greater),
        ("10", "011", Ordering::Greater),
        ("11", "11", Ordering::Equal),
    ];
    for (x, y, o) in cases {
        assert_eq!(b(x).key_cmp(&b(y)), o, "{x} vs {y}");
    }
    // for whole bytes, the key order is Vec<u8>'s order
    let keys: [&[u8]; 6] = [b"", b"\x00", b"a", b"ab", b"b", b"\xff"];
    for w in keys.windows(2) {
        assert!(Bits::from_bytes(w[0]).key_lt(&Bits::from_bytes(w[1])));
        assert!(w[0] < w[1]);
    }
    assert_eq!(b("0110").common_prefix_from(1, &b("11"), 0), 2);
}
