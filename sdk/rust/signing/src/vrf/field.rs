//! GF(2^255 - 19) for the Elligator 2 maps of draft-03 and of RFC 9380, which curve25519-dalek 4 keeps private. The
//! arithmetic is fiat-crypto's formally verified `curve25519` field, the one curve25519-dalek's own `fiat` backend
//! uses: every operation is constant-time. The maps branch on their (public) intermediate values, as the references do.

use alloc::vec::Vec;

use curve25519_dalek::edwards::{CompressedEdwardsY, EdwardsPoint};
use fiat::{fiat_25519_loose_field_element as Loose, fiat_25519_tight_field_element as Tight};
#[cfg(not(target_pointer_width = "64"))]
use fiat_crypto::curve25519_32 as fiat;
#[cfg(target_pointer_width = "64")]
use fiat_crypto::curve25519_64 as fiat;

use super::{decode_draft03, sha512};

/// Curve25519's Montgomery `A`.
const J: u32 = 486_662;

/// `sqrt(-1) = 2^((p - 1) / 4)`, little-endian (RFC 9380 appendix I.2's `c2`).
const SQRT_M1: [u8; 32] = [
    0xb0, 0xa0, 0x0e, 0x4a, 0x27, 0x1b, 0xee, 0xc4, 0x78, 0xe4, 0x2f, 0xad, 0x06, 0x18, 0x43, 0x2f, 0xa7, 0xd7, 0xfb,
    0x3d, 0x99, 0x00, 0x4d, 0x2b, 0x0b, 0xdf, 0xc1, 0x4f, 0x80, 0x24, 0x83, 0x2b,
];

/// The even `sqrt(-486664)`, little-endian (RFC 9380 appendix D's `c1`).
const SQRT_M_J2: [u8; 32] = [
    0x06, 0x7e, 0x45, 0xff, 0xaa, 0x04, 0x6e, 0xcc, 0x82, 0x1a, 0x7d, 0x4b, 0xd1, 0xd3, 0xa1, 0xc5, 0x7e, 0x4f, 0xfc,
    0x03, 0xdc, 0x08, 0x7b, 0xd2, 0xbb, 0x06, 0xa0, 0x60, 0xf4, 0xed, 0x26, 0x0f,
];

/// `2^192`, little-endian.
const TWO_192: [u8; 32] =
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0];

/// A field element within fiat's tight bounds, the input every operation accepts. Its limbs are not canonical, so it
/// is compared, and its parity read, only through [`Fe::to_bytes`].
#[derive(Clone, Copy)]
struct Fe(Tight);

impl Fe {
    /// The value of 32 little-endian bytes with bit 255 cleared: values from `p` to `2^255 - 1` reduce.
    fn from_bytes(bytes: &[u8; 32]) -> Self {
        let mut masked = *bytes;
        if let Some(last) = masked.last_mut() {
            *last &= 0x7f;
        }
        let mut out = Tight(Default::default());
        fiat::fiat_25519_from_bytes(&mut out, &masked);
        Self(out)
    }

    fn from_u32(n: u32) -> Self {
        let mut bytes = [0u8; 32];
        for (o, b) in bytes.iter_mut().zip(n.to_le_bytes()) {
            *o = b;
        }
        Self::from_bytes(&bytes)
    }

    fn zero() -> Self {
        Self::from_u32(0)
    }

    fn one() -> Self {
        Self::from_u32(1)
    }

    /// The canonical (fully reduced) little-endian encoding.
    fn to_bytes(self) -> [u8; 32] {
        let mut out = [0u8; 32];
        fiat::fiat_25519_to_bytes(&mut out, &self.0);
        out
    }

    fn equals(self, that: Self) -> bool {
        self.to_bytes() == that.to_bytes()
    }

    fn is_zero(self) -> bool {
        self.equals(Self::zero())
    }

    /// RFC 9380's `sgn0`: the parity of the canonical value.
    fn is_odd(self) -> bool {
        self.to_bytes().first().is_some_and(|b| b & 1 == 1)
    }

    fn relax(self) -> Loose {
        let mut out = Loose(Default::default());
        fiat::fiat_25519_relax(&mut out, &self.0);
        out
    }

    fn carry(loose: &Loose) -> Self {
        let mut out = Tight(Default::default());
        fiat::fiat_25519_carry(&mut out, loose);
        Self(out)
    }

    fn add(self, that: Self) -> Self {
        let mut out = Loose(Default::default());
        fiat::fiat_25519_add(&mut out, &self.0, &that.0);
        Self::carry(&out)
    }

    fn sub(self, that: Self) -> Self {
        let mut out = Loose(Default::default());
        fiat::fiat_25519_sub(&mut out, &self.0, &that.0);
        Self::carry(&out)
    }

    fn neg(self) -> Self {
        let mut out = Loose(Default::default());
        fiat::fiat_25519_opp(&mut out, &self.0);
        Self::carry(&out)
    }

    fn mul(self, that: Self) -> Self {
        let mut out = Tight(Default::default());
        fiat::fiat_25519_carry_mul(&mut out, &self.relax(), &that.relax());
        Self(out)
    }

    fn square(self) -> Self {
        let mut out = Tight(Default::default());
        fiat::fiat_25519_carry_square(&mut out, &self.relax());
        Self(out)
    }

    /// `self^(2^k)`.
    fn square_n(self, k: u32) -> Self {
        (0..k).fold(self, |x, _| x.square())
    }

    /// `(self^(2^250 - 1), self^11)`: the addition chain of ref10 and curve25519-dalek, shared by every exponent below.
    fn pow22501(self) -> (Self, Self) {
        let t0 = self.square();
        let t2 = self.mul(t0.square_n(2));
        let t3 = t0.mul(t2);
        let t5 = t2.mul(t3.square());
        let t7 = t5.square_n(5).mul(t5);
        let t9 = t7.square_n(10).mul(t7);
        let t11 = t9.square_n(20).mul(t9);
        let t13 = t11.square_n(10).mul(t7);
        let t15 = t13.square_n(50).mul(t13);
        let t17 = t15.square_n(100).mul(t15);
        let t19 = t17.square_n(50).mul(t13);
        (t19, t3)
    }

    /// `self^((p - 5) / 8) = self^(2^252 - 3)`.
    fn pow_p58(self) -> Self {
        let (t19, _) = self.pow22501();
        t19.square_n(2).mul(self)
    }

    /// `self^-1 = self^(p - 2)`, and `0` for `0`.
    fn inv0(self) -> Self {
        let (t19, t3) = self.pow22501();
        t19.square_n(5).mul(t3)
    }

    /// Whether `self` is a non-zero non-square: its Legendre symbol `self^((p - 1) / 2)` is `-1`. `(p - 1) / 2` is
    /// `4 (p - 5) / 8 + 2`.
    fn is_non_square(self) -> bool {
        self.pow_p58().square_n(2).mul(self.square()).equals(Self::one().neg())
    }

    /// `sqrt(self)` for `p = 5 mod 8` (RFC 9380 appendix I.2), if `self` is a square: `t = self^((p + 3) / 8)`, which
    /// is `self^((p - 5) / 8) * self`, or `t sqrt(-1)`.
    fn sqrt(self) -> Option<Self> {
        let t = self.pow_p58().mul(self);
        let z = if t.square().equals(self) { t } else { t.mul(Self::from_bytes(&SQRT_M1)) };
        z.square().equals(self).then_some(z)
    }

    /// `sqrt(self)` with the given `sgn0`.
    fn sqrt_with_sign(self, odd: bool) -> Option<Self> {
        self.sqrt().map(|z| if z.is_odd() == odd { z } else { z.neg() })
    }

    /// A 48-byte big-endian integer reduced mod `p` (RFC 9380 `hash_to_field`'s `OS2IP(tv) mod p`), as
    /// `hi 2^192 + lo` over its two 24-byte halves, each below `p`.
    fn from_be48(bytes: &[u8; 48]) -> Self {
        let (hi, lo) = bytes.split_at(24);
        let le = |half: &[u8]| {
            let mut out = [0u8; 32];
            for (o, b) in out.iter_mut().zip(half.iter().rev()) {
                *o = *b;
            }
            Self::from_bytes(&out)
        };
        le(hi).mul(Self::from_bytes(&TWO_192)).add(le(lo))
    }
}

/// RFC 9380 section 5.3.1 `expand_message_xmd` with SHA-512 and `len_in_bytes = 48`, the one length `hash_to_field`
/// asks of it here. `dst` is 1 to 255 bytes.
fn expand_message_xmd_sha512_48(message: &[&[u8]], dst: &[u8]) -> [u8; 48] {
    let dst_len = [u8::try_from(dst.len()).unwrap_or(u8::MAX)];
    let mut parts: Vec<&[u8]> = Vec::with_capacity(message.len().saturating_add(5));
    let z_pad = [0u8; 128];
    let tail = [0u8, 48, 0u8];
    parts.push(&z_pad);
    parts.extend_from_slice(message);
    parts.extend_from_slice(&[&tail, dst, &dst_len]);
    let b0 = sha512(&parts);
    let b1 = sha512(&[&b0, &[1u8], dst, &dst_len]);
    let mut out = [0u8; 48];
    for (o, b) in out.iter_mut().zip(b1) {
        *o = b;
    }
    out
}

/// RFC 9380 `encode_to_curve` with `edwards25519_XMD:SHA-512_ELL2_NU_`: one field element from 48 uniform bytes, then
/// [`map_to_curve_ell2`]. `dst` is 1 to 255 bytes.
pub(super) fn encode_to_curve_ell2(message: &[&[u8]], dst: &[u8]) -> EdwardsPoint {
    map_to_curve_ell2(Fe::from_be48(&expand_message_xmd_sha512_48(message, dst)))
}

/// Elligator 2 onto curve25519 (RFC 9380 section 6.7.1, `Z = 2`), the rational map to edwards25519 (appendix D), and
/// the cofactor cleared.
fn map_to_curve_ell2(u: Fe) -> EdwardsPoint {
    let one = Fe::one();
    let j = Fe::from_u32(J);
    // section 6.7.1 with K = 1
    let x1 = match j.neg().mul(one.add(Fe::from_u32(2).mul(u.square())).inv0()) {
        x if x.is_zero() => j.neg(),
        x => x,
    };
    let g = |x: Fe| x.square().mul(x).add(j.mul(x.square())).add(x);
    let x2 = x1.neg().sub(j);
    let (s, t) = g(x1)
        .sqrt_with_sign(true)
        .map(|y| (x1, y))
        .or_else(|| g(x2).sqrt_with_sign(false).map(|y| (x2, y)))
        .unwrap_or((Fe::zero(), Fe::zero()));
    // appendix D: v = sqrt(-486664) s / t, w = (s - 1) / (s + 1), and (0, 1) where t (s + 1) = 0
    let s1 = s.add(one);
    let d = t.mul(s1).inv0();
    let (v, w) = if d.is_zero() {
        (Fe::zero(), one)
    } else {
        (d.mul(s1).mul(s).mul(Fe::from_bytes(&SQRT_M_J2)), d.mul(t).mul(s.sub(one)))
    };
    let mut enc = w.to_bytes();
    if let Some(last) = enc.last_mut() {
        *last |= u8::from(v.is_odd()) << 7;
    }
    // (v, w) is on edwards25519 by construction, so it decodes
    CompressedEdwardsY(enc).decompress().map_or(EdwardsPoint::default(), |pt| pt.mul_by_cofactor())
}

/// libsodium's `ge25519_from_uniform` (Cardano draft-03): bit 255 of the input is the sign of `x`, the rest the
/// Elligator 2 representative `r`; `u = -A / (1 + 2 r^2)`, replaced by `-u - A` if `u (u^2 + A u + 1)` is a
/// non-square; `y = (u - 1) / (u + 1)` (or 1 when `u + 1 = 0` or the curve value is 0); the cofactor cleared.
pub(super) fn from_uniform(input: &[u8; 32]) -> EdwardsPoint {
    let sign = input.last().map_or(0, |b| b & 0x80);
    let r = Fe::from_bytes(input);
    let one = Fe::one();
    let j = Fe::from_u32(J);
    let rr2 = Fe::from_u32(2).mul(r.square());
    let u0 = j.neg().mul(one.add(rr2).inv0());
    let w = |u: Fe| u.mul(u.square().add(j.mul(u)).add(one));
    let u = if w(u0).is_non_square() { u0.neg().sub(j) } else { u0 };
    let u_plus_1 = u.add(one);
    let y = if u_plus_1.is_zero() || w(u).is_zero() { one } else { u.sub(one).mul(u_plus_1.inv0()) };
    let mut enc = y.to_bytes();
    if let Some(last) = enc.last_mut() {
        *last |= sign;
    }
    // y is canonical and on the curve's u-side by construction; a failure would mean the identity input
    decode_draft03(&enc).map_or(EdwardsPoint::default(), |pt| pt.mul_by_cofactor())
}

#[cfg(test)]
mod tests;
