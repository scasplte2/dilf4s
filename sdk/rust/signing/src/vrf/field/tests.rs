//! The fixed-width field and both Elligator 2 maps against the `num-bigint` implementation they replaced, kept here
//! as [`reference`]: on the field's edge values, every exceptional branch of the maps, and seeded pseudo-random
//! inputs.

#![allow(
    clippy::unwrap_used,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects,
    reason = "tests fail by panicking; BigUint arithmetic cannot overflow"
)]

use alloc::vec::Vec;

use num_bigint::BigUint;

use super::{
    Fe, J, SQRT_M_J2, SQRT_M1, encode_to_curve_ell2, expand_message_xmd_sha512_48, from_uniform, map_to_curve_ell2,
};

/// The previous `num-bigint` field and maps, unchanged apart from visibility and `encode_to_curve_ell2` split where
/// `u` is computed, so either half can be driven directly.
mod reference {
    use alloc::vec::Vec;

    use curve25519_dalek::edwards::{CompressedEdwardsY, EdwardsPoint};
    use num_bigint::BigUint;

    use crate::util::sha512;
    use crate::vrf::decode_draft03;

    pub(super) fn p() -> BigUint {
        (BigUint::from(1u8) << 255u32) - BigUint::from(19u8)
    }

    const J: u32 = 486_662;

    fn le(bytes: &[u8]) -> BigUint {
        BigUint::from_bytes_le(bytes)
    }

    pub(super) fn le32(x: &BigUint) -> [u8; 32] {
        let mut out = [0u8; 32];
        for (o, b) in out.iter_mut().zip(x.to_bytes_le()) {
            *o = b;
        }
        out
    }

    pub(super) struct Fp {
        pub(super) p: BigUint,
    }

    impl Fp {
        pub(super) fn add(&self, a: &BigUint, b: &BigUint) -> BigUint {
            (a + b) % &self.p
        }
        pub(super) fn sub(&self, a: &BigUint, b: &BigUint) -> BigUint {
            (a + &self.p - (b % &self.p)) % &self.p
        }
        pub(super) fn mul(&self, a: &BigUint, b: &BigUint) -> BigUint {
            (a * b) % &self.p
        }
        pub(super) fn neg(&self, a: &BigUint) -> BigUint {
            self.sub(&BigUint::ZERO, a)
        }
        /// `a^-1`, and `0` for `0`.
        pub(super) fn inv0(&self, a: &BigUint) -> BigUint {
            a.modpow(&(&self.p - BigUint::from(2u8)), &self.p)
        }
        /// Whether `a` is a non-zero non-square (its Legendre symbol is -1).
        pub(super) fn is_non_square(&self, a: &BigUint) -> bool {
            let exp = (&self.p - BigUint::from(1u8)) >> 1u32;
            a.modpow(&exp, &self.p) == &self.p - BigUint::from(1u8)
        }
    }

    /// `sqrt(a)` for `p = 5 mod 8` (RFC 9380 appendix I.2), if `a` is a square.
    pub(super) fn sqrt(f: &Fp, a: &BigUint) -> Option<BigUint> {
        let c1 = (&f.p + BigUint::from(3u8)) >> 3u32;
        let c2 = BigUint::from(2u8).modpow(&((&f.p - BigUint::from(1u8)) >> 2u32), &f.p);
        let t = a.modpow(&c1, &f.p);
        let z = if f.mul(&t, &t) == a % &f.p { t } else { f.mul(&t, &c2) };
        (f.mul(&z, &z) == a % &f.p).then_some(z)
    }

    /// `sqrt(a)` with the given `sgn0` (the parity of the canonical value).
    pub(super) fn sqrt_with_sign(f: &Fp, a: &BigUint, odd: bool) -> Option<BigUint> {
        sqrt(f, a).map(|z| if z.bit(0) == odd { z } else { f.neg(&z) })
    }

    /// RFC 9380 section 5.3.1 `expand_message_xmd` with SHA-512, for at most 64 bytes (one block of output): the
    /// only length `hash_to_field` asks of it here. `dst` is 1 to 255 bytes.
    pub(super) fn expand_message_xmd_sha512(message: &[&[u8]], dst: &[u8], len: u8) -> Vec<u8> {
        let dst_len = [u8::try_from(dst.len()).unwrap_or(u8::MAX)];
        let mut parts: Vec<&[u8]> = Vec::with_capacity(message.len().saturating_add(5));
        let z_pad = [0u8; 128];
        let tail = [0u8, len, 0u8];
        parts.push(&z_pad);
        parts.extend_from_slice(message);
        parts.extend_from_slice(&[&tail, dst, &dst_len]);
        let b0 = sha512(&parts);
        let b1 = sha512(&[&b0, &[1u8], dst, &dst_len]);
        b1.iter().take(usize::from(len)).copied().collect()
    }

    /// RFC 9380 `encode_to_curve` with `edwards25519_XMD:SHA-512_ELL2_NU_`: one field element from 48 uniform bytes,
    /// Elligator 2 onto curve25519 (section 6.7.1, `Z = 2`), the rational map to edwards25519 (appendix D), and the
    /// cofactor cleared. `dst` is 1 to 255 bytes.
    pub(super) fn encode_to_curve_ell2(message: &[&[u8]], dst: &[u8]) -> EdwardsPoint {
        let u = BigUint::from_bytes_be(&expand_message_xmd_sha512(message, dst, 48)) % p();
        map_to_curve_ell2(&u)
    }

    /// The second half of `encode_to_curve_ell2`, from the field element `u` (below `p`).
    pub(super) fn map_to_curve_ell2(u: &BigUint) -> EdwardsPoint {
        let f = Fp { p: p() };
        let one = BigUint::from(1u8);
        let j = BigUint::from(J);
        let u = u.clone();
        // section 6.7.1 with K = 1
        let x1 = match f.mul(&f.neg(&j), &f.inv0(&f.add(&one, &f.mul(&BigUint::from(2u8), &f.mul(&u, &u))))) {
            x if x == BigUint::ZERO => f.neg(&j),
            x => x,
        };
        let g = |x: &BigUint| f.add(&f.add(&f.mul(&f.mul(x, x), x), &f.mul(&j, &f.mul(x, x))), x);
        let x2 = f.sub(&f.neg(&x1), &j);
        let (s, t) = sqrt_with_sign(&f, &g(&x1), true)
            .map(|y| (x1.clone(), y))
            .or_else(|| sqrt_with_sign(&f, &g(&x2), false).map(|y| (x2, y)))
            .unwrap_or((BigUint::ZERO, BigUint::ZERO));
        // appendix D: v = sqrt(-486664) s / t, w = (s - 1) / (s + 1), and (0, 1) where t (s + 1) = 0
        let c1 = sqrt_with_sign(&f, &f.neg(&BigUint::from(J + 2)), false).unwrap_or_default();
        let s1 = f.add(&s, &one);
        let d = f.inv0(&f.mul(&t, &s1));
        let (v, w) = if d == BigUint::ZERO {
            (BigUint::ZERO, one)
        } else {
            (f.mul(&f.mul(&f.mul(&d, &s1), &s), &c1), f.mul(&f.mul(&d, &t), &f.sub(&s, &one)))
        };
        let mut enc = le32(&w);
        if let Some(last) = enc.last_mut() {
            *last |= u8::from(v.bit(0)) << 7;
        }
        // (v, w) is on edwards25519 by construction, so it decodes
        CompressedEdwardsY(enc).decompress().map_or(EdwardsPoint::default(), |pt| pt.mul_by_cofactor())
    }

    /// libsodium's `ge25519_from_uniform` (Cardano draft-03): bit 255 of the input is the sign of `x`, the rest the
    /// Elligator 2 representative `r`; `u = -A / (1 + 2 r^2)`, replaced by `-u - A` if `u (u^2 + A u + 1)` is a
    /// non-square; `y = (u - 1) / (u + 1)` (or 1 when `u + 1 = 0` or the curve value is 0); the cofactor cleared.
    pub(super) fn from_uniform(input: &[u8; 32]) -> EdwardsPoint {
        let f = Fp { p: p() };
        let sign = input.last().map_or(0, |b| b & 0x80);
        let mut r_bytes: Vec<u8> = input.to_vec();
        if let Some(last) = r_bytes.last_mut() {
            *last &= 0x7f;
        }
        let r = le(&r_bytes) % &f.p;
        let one = BigUint::from(1u8);
        let j = BigUint::from(J);
        let rr2 = f.mul(&BigUint::from(2u8), &f.mul(&r, &r));
        let u0 = f.mul(&f.neg(&j), &f.inv0(&f.add(&one, &rr2)));
        let w = |u: &BigUint| f.mul(u, &f.add(&f.add(&f.mul(u, u), &f.mul(&j, u)), &one));
        let u = if f.is_non_square(&w(&u0)) { f.sub(&f.neg(&u0), &j) } else { u0 };
        let u_plus_1 = f.add(&u, &one);
        let y = if u_plus_1 == BigUint::ZERO || w(&u) == BigUint::ZERO {
            one
        } else {
            f.mul(&f.sub(&u, &one), &f.inv0(&u_plus_1))
        };
        let mut enc = le32(&y);
        if let Some(last) = enc.last_mut() {
            *last |= sign;
        }
        // y is canonical and on the curve's u-side by construction; a failure would mean the identity input
        decode_draft03(&enc).map_or(EdwardsPoint::default(), |pt| pt.mul_by_cofactor())
    }
}

use reference::{Fp, le32, p};

fn fp() -> Fp {
    Fp { p: p() }
}

/// The element of a value below `2^255`.
fn fe(x: &BigUint) -> Fe {
    Fe::from_bytes(&le32(x))
}

fn big(x: Fe) -> BigUint {
    BigUint::from_bytes_le(&x.to_bytes())
}

fn encode(p: &curve25519_dalek::EdwardsPoint) -> [u8; 32] {
    p.compress().to_bytes()
}

/// A deterministic byte stream (SHA-512 in counter mode), so failures reproduce.
fn stream(seed: &str, n: usize) -> Vec<[u8; 32]> {
    (0..n)
        .map(|i| {
            let h = crate::util::sha512(&[seed.as_bytes(), &u64::try_from(i).unwrap().to_le_bytes()]);
            h[..32].try_into().unwrap()
        })
        .collect()
}

/// Values below `2^255` worth checking: the field's edges and its ring of small and near-`p` values.
fn edges() -> Vec<BigUint> {
    let p = p();
    let top = (BigUint::from(1u8) << 255u32) - 1u8;
    let mut out: Vec<BigUint> = (0u32..=40).map(BigUint::from).collect();
    out.extend((1u32..=19).map(|k| &p - k));
    out.extend((0u32..19).map(|k| &p + k));
    out.extend([
        top,
        BigUint::from(J),
        BigUint::from(J + 2),
        &p - BigUint::from(J),
        &p - BigUint::from(J + 2),
        (&p - 1u8) >> 1u32,
        (&p + 1u8) >> 1u32,
        BigUint::from(1u8) << 192u32,
        BigUint::from(1u8) << 254u32,
        BigUint::from_bytes_le(&SQRT_M1),
    ]);
    out
}

/// Everything below `2^255`: edges, then pseudo-random values, half of them pushed into `[p, 2^255)` when they fit.
fn values() -> Vec<BigUint> {
    let p = p();
    let top = BigUint::from(1u8) << 255u32;
    let mut out = edges();
    out.extend(stream("field", 400).into_iter().map(|mut b| {
        b[31] &= 0x7f;
        BigUint::from_bytes_le(&b)
    }));
    out.extend(stream("small", 50).into_iter().map(|b| BigUint::from_bytes_le(&b[..4])));
    out.extend(stream("near-p", 50).into_iter().map(|b| (&p + BigUint::from_bytes_le(&b[..2])) % &top));
    out
}

#[test]
fn constants_are_the_ones_the_reference_computes() {
    let f = fp();
    let c2 = BigUint::from(2u8).modpow(&((&f.p - 1u8) >> 2u32), &f.p);
    assert_eq!(BigUint::from_bytes_le(&SQRT_M1), c2, "SQRT_M1 is 2^((p - 1) / 4)");
    assert_eq!(f.mul(&c2, &c2), &f.p - 1u8, "SQRT_M1 squares to -1");
    let c1 = reference::sqrt_with_sign(&f, &f.neg(&BigUint::from(J + 2)), false).unwrap();
    assert_eq!(BigUint::from_bytes_le(&SQRT_M_J2), c1, "SQRT_M_J2 is the even sqrt(-486664)");
    assert_eq!(BigUint::from_bytes_le(&super::TWO_192), BigUint::from(1u8) << 192u32);
}

#[test]
fn decoding_reduces_and_encoding_is_canonical() {
    let f = fp();
    for x in values() {
        assert_eq!(big(fe(&x)), &x % &f.p, "from_bytes then to_bytes of {x:x}");
    }
    // bit 255 is not part of the value
    let mut all = [0xff; 32];
    assert_eq!(big(Fe::from_bytes(&all)), BigUint::from(18u8), "2^255 - 1 is 18");
    all[31] = 0x7f;
    assert_eq!(big(Fe::from_bytes(&all)), BigUint::from(18u8));
    assert_eq!(big(Fe::from_bytes(&le32(&f.p))), BigUint::ZERO, "p is 0");
    assert!(fe(&f.p).is_zero());
    assert!(fe(&f.p).equals(Fe::zero()));
    assert!(fe(&(&f.p + 1u8)).is_odd(), "p + 1 encodes with an even low byte but is 1, which is odd");
}

#[test]
fn parity_is_of_the_canonical_value() {
    let f = fp();
    for x in values() {
        assert_eq!(fe(&x).is_odd(), (&x % &f.p).bit(0), "sgn0 of {x:x}");
    }
}

#[test]
fn arithmetic_agrees_with_the_reference() {
    let f = fp();
    let xs = values();
    let ys: Vec<BigUint> = xs.iter().rev().cloned().collect();
    for (x, y) in xs.iter().zip(&ys) {
        let (a, b) = (fe(x), fe(y));
        assert_eq!(big(a.add(b)), f.add(x, y), "{x:x} + {y:x}");
        assert_eq!(big(a.sub(b)), f.sub(x, y), "{x:x} - {y:x}");
        assert_eq!(big(a.neg()), f.neg(x), "-{x:x}");
        assert_eq!(big(a.mul(b)), f.mul(x, y), "{x:x} * {y:x}");
        assert_eq!(big(a.square()), f.mul(x, x), "{x:x}^2");
    }
}

#[test]
fn exponentiations_agree_with_the_reference() {
    let f = fp();
    let p58 = (&f.p - 5u8) >> 3u32;
    for x in values() {
        let a = fe(&x);
        assert_eq!(big(a.inv0()), f.inv0(&x), "inv0({x:x})");
        assert_eq!(big(a.pow_p58()), x.modpow(&p58, &f.p), "{x:x}^((p - 5) / 8)");
        assert_eq!(a.is_non_square(), f.is_non_square(&x), "is_non_square({x:x})");
        assert_eq!(a.sqrt().map(big), reference::sqrt(&f, &x), "sqrt({x:x})");
        for odd in [false, true] {
            assert_eq!(
                a.sqrt_with_sign(odd).map(big),
                reference::sqrt_with_sign(&f, &x, odd),
                "sqrt_with_sign({x:x}, {odd})"
            );
        }
    }
}

#[test]
fn exponentiation_edge_cases() {
    let f = fp();
    assert!(Fe::zero().inv0().is_zero(), "inv0(0) = 0");
    assert!(fe(&f.p).inv0().is_zero(), "inv0(p) = 0");
    assert!(!Fe::zero().is_non_square(), "0 is not a non-square");
    assert!(Fe::from_u32(2).is_non_square(), "2 is a non-square for p = 5 mod 8");
    assert!(!Fe::one().neg().is_non_square(), "-1 is a square");
    assert!(Fe::from_u32(2).sqrt().is_none(), "sqrt(2) does not exist");
    assert!(Fe::from_u32(2).sqrt_with_sign(false).is_none());
    let i = Fe::one().neg().sqrt_with_sign(false).unwrap();
    assert!(i.equals(Fe::from_bytes(&SQRT_M1)), "the even sqrt(-1) is 2^((p - 1) / 4)");
    // sqrt(0) is 0 for either requested sign: -0 is 0
    assert!(Fe::zero().sqrt_with_sign(false).unwrap().is_zero());
    assert!(Fe::zero().sqrt_with_sign(true).unwrap().is_zero());
    let four = Fe::from_u32(4);
    assert_eq!(big(four.sqrt_with_sign(false).unwrap()), BigUint::from(2u8));
    assert_eq!(big(four.sqrt_with_sign(true).unwrap()), &f.p - 2u8);
}

#[test]
fn the_48_byte_reduction_agrees_with_the_reference() {
    let p = p();
    let be48 = |x: &BigUint| {
        let mut out = [0u8; 48];
        let bytes = x.to_bytes_be();
        out[48 - bytes.len()..].copy_from_slice(&bytes);
        out
    };
    let max = (BigUint::from(1u8) << 384u32) - 1u8;
    let mut cases: Vec<BigUint> = [0u32, 1, 2, 19]
        .into_iter()
        .map(BigUint::from)
        .chain([&p - 1u8, p.clone(), &p + 1u8, &p * 2u8, &p * 2u8 - 1u8, &max / &p * &p, max.clone()])
        .chain([BigUint::from(1u8) << 192u32, (BigUint::from(1u8) << 192u32) - 1u8, BigUint::from(1u8) << 255u32])
        .collect();
    cases.extend(stream("be48", 300).chunks(2).map(|pair| {
        let mut b = [0u8; 48];
        b[..32].copy_from_slice(&pair[0]);
        b[32..].copy_from_slice(&pair[1][..16]);
        BigUint::from_bytes_be(&b)
    }));
    for x in cases {
        assert!(x <= max);
        assert_eq!(big(Fe::from_be48(&be48(&x))), &x % &p, "{x:x} mod p");
    }
}

#[test]
fn expand_message_xmd_agrees_with_the_reference() {
    for (msg, dst) in [(&b""[..], &b"Q"[..]), (b"abc", b"QUUX-V01-CS02-with-expander-SHA512-256"), (b"x", &[7u8; 255])]
    {
        assert_eq!(
            expand_message_xmd_sha512_48(&[msg], dst).to_vec(),
            reference::expand_message_xmd_sha512(&[msg], dst, 48)
        );
    }
}

/// The representatives `r` (draft-03's, or RFC 9380's `u`) with `u0 = -A / (1 + 2 r^2)` (`x1` in RFC 9380) equal to
/// `t`, if any: `r^2 = (-A / t - 1) / 2`.
fn representatives(t: &BigUint) -> Option<[BigUint; 2]> {
    let f = fp();
    let one = BigUint::from(1u8);
    let r2 = f.mul(&f.sub(&f.mul(&f.neg(&BigUint::from(J)), &f.inv0(t)), &one), &f.inv0(&BigUint::from(2u8)));
    reference::sqrt(&f, &r2).map(|r| [f.neg(&r), r])
}

/// `r` with `u0 = 1`: `w(1) = A + 2` is a square, so `u = 1` and `y = 0` (RFC 9380: `s = 1`, so `w = 0`).
fn exceptional_representatives() -> Vec<BigUint> {
    representatives(&BigUint::from(1u8)).into_iter().flatten().collect()
}

fn draft03_inputs() -> Vec<[u8; 32]> {
    let p = p();
    let top = (BigUint::from(1u8) << 255u32) - 1u8;
    let fixed: Vec<BigUint> = [BigUint::ZERO, BigUint::from(1u8), &p - 1u8, p.clone(), &p + 1u8, &p + 18u8, top]
        .into_iter()
        .chain(edges())
        .chain(exceptional_representatives())
        .collect();
    let mut out: Vec<[u8; 32]> = fixed
        .iter()
        .flat_map(|r| {
            let clear = le32(r);
            let mut set = clear;
            set[31] |= 0x80;
            [clear, set]
        })
        .collect();
    out.extend(stream("draft03", 2000));
    out
}

/// Which exceptional branches of the two maps any input reaches, so the inputs above are known to cover them all.
#[test]
fn the_exceptional_branches_reachable_are_covered() {
    let f = fp();
    let one = BigUint::from(1u8);
    let j = BigUint::from(J);
    let w = |u: &BigUint| f.mul(u, &f.add(&f.add(&f.mul(u, u), &f.mul(&j, u)), &one));
    // 1 + 2 r^2 = 0 needs -1/2 to be a square: never, so inv0 never sees 0 there (nor RFC 9380's x1 = 0)
    assert!(f.is_non_square(&f.neg(&f.inv0(&BigUint::from(2u8)))));
    // u + 1 = 0 needs u0 = -1, or the replacement -u0 - A = -1: neither has a representative
    assert!(representatives(&f.neg(&one)).is_none());
    assert!(representatives(&f.sub(&one, &j)).is_none());
    // r = 0 (and p): u0 = -A, w(-A) = -A is a non-square, so u = 0 and w(u) = 0: y = 1 (RFC 9380 u = 0: x2 = 0, t = 0)
    assert!(f.is_non_square(&w(&f.neg(&j))));
    // u0 = 1: w(1) is a square, so u = 1 stays and y = 0
    assert!(!f.is_non_square(&w(&one)));
    assert_eq!(exceptional_representatives().len(), 2);
}

#[test]
fn draft03_from_uniform_agrees_with_the_reference() {
    for input in draft03_inputs() {
        assert_eq!(
            encode(&from_uniform(&input)),
            encode(&reference::from_uniform(&input)),
            "from_uniform({input:02x?})"
        );
    }
}

#[test]
fn ell2_map_agrees_with_the_reference() {
    let p = p();
    let mut us: Vec<BigUint> = values().into_iter().map(|x| x % &p).collect();
    // u = 0 (in values()): x1 = -A, g(-A) is a non-square, so x2 = 0 and t = 0, giving (0, 1)
    us.extend(exceptional_representatives());
    for u in us {
        assert_eq!(
            encode(&map_to_curve_ell2(fe(&u))),
            encode(&reference::map_to_curve_ell2(&u)),
            "map_to_curve_ell2({u:x})"
        );
    }
}

#[test]
fn ell2_encode_to_curve_agrees_with_the_reference() {
    for (i, m) in stream("ell2", 500).iter().enumerate() {
        let dst: &[u8] = if i % 2 == 0 { b"ECVRF_edwards25519_XMD:SHA-512_ELL2_NU_\x04" } else { b"D" };
        assert_eq!(
            encode(&encode_to_curve_ell2(&[m, &m[..i % 32]], dst)),
            encode(&reference::encode_to_curve_ell2(&[m, &m[..i % 32]], dst)),
            "encode_to_curve_ell2 #{i}"
        );
    }
}
