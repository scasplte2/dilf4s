//! GF(2^255 - 19) for the Elligator 2 maps of draft-03 and of RFC 9380, which curve25519-dalek 4 keeps private. The
//! arithmetic is fiat-crypto's formally verified `curve25519` field, the one curve25519-dalek's own `fiat` backend
//! uses.
//!
//! Field arithmetic, exponentiation, equality, parity and selection are constant-time (fiat-crypto and `subtle`), and
//! both maps compute every candidate and select between them: neither branches on a field value. Variable time remains
//! only where the data is public: the final point decoding of each map (draft-03's `y < p` check and
//! curve25519-dalek's decompression, which accept every map output by construction, and the branch on that result),
//! the SHA-512 inputs' lengths, and `expand_message_xmd`'s DST length.
//!
//! In the SP1 guest the exponentiation's multiplications go to the `sys_bigint` precompile instead (`zkvm`, issue
//! #69): the same addition chain ([`pow_p58_chain`]) on canonical values, with every product checked. That is the
//! only fallible step, so the maps return [`MulError`]: `Infallible` natively, `zkvm::NonCanonical` in the guest.
//! That path checks each product with a branch, which is not constant-time; the guest only verifies, on public
//! inputs.

use alloc::vec::Vec;
#[cfg(not(all(target_os = "zkvm", target_vendor = "succinct")))]
use core::convert::Infallible;

use curve25519_dalek::edwards::{CompressedEdwardsY, EdwardsPoint};
use fiat::{fiat_25519_loose_field_element as Loose, fiat_25519_tight_field_element as Tight};
#[cfg(not(target_pointer_width = "64"))]
use fiat_crypto::curve25519_32 as fiat;
#[cfg(target_pointer_width = "64")]
use fiat_crypto::curve25519_64 as fiat;
use subtle::{Choice, ConditionallySelectable, ConstantTimeEq};

use super::{decode_draft03, sha512};

/// Why a field multiplication failed: it cannot, natively (fiat-crypto).
#[cfg(not(all(target_os = "zkvm", target_vendor = "succinct")))]
pub(super) type MulError = Infallible;
/// Why a field multiplication failed: in the SP1 guest, `sys_bigint` returned a product not below `p`.
#[cfg(all(target_os = "zkvm", target_vendor = "succinct"))]
pub(super) type MulError = zkvm::NonCanonical;

/// The multiplications [`pow_p58_chain`] is made of, so fiat-crypto and the zkVM run the one addition chain.
trait ChainField: Copy {
    /// Why a multiplication failed.
    type Error;
    /// `self * that`.
    fn chain_mul(self, that: Self) -> Result<Self, Self::Error>;
    /// `self^2`.
    fn chain_square(self) -> Result<Self, Self::Error>;
}

/// `x^(2^k)`.
fn square_n<F: ChainField>(x: F, k: u32) -> Result<F, F::Error> {
    (0..k).try_fold(x, |y, _| y.chain_square())
}

/// `x^((p - 5) / 8) = x^(2^252 - 3)`, through `x^(2^250 - 1)` by the addition chain of ref10 and curve25519-dalek.
fn pow_p58_chain<F: ChainField>(x: F) -> Result<F, F::Error> {
    let t0 = x.chain_square()?;
    let t2 = x.chain_mul(square_n(t0, 2)?)?;
    let t3 = t0.chain_mul(t2)?;
    let t5 = t2.chain_mul(t3.chain_square()?)?;
    let t7 = square_n(t5, 5)?.chain_mul(t5)?;
    let t9 = square_n(t7, 10)?.chain_mul(t7)?;
    let t11 = square_n(t9, 20)?.chain_mul(t9)?;
    let t13 = square_n(t11, 10)?.chain_mul(t7)?;
    let t15 = square_n(t13, 50)?.chain_mul(t13)?;
    let t17 = square_n(t15, 100)?.chain_mul(t15)?;
    let t19 = square_n(t17, 50)?.chain_mul(t13)?;
    square_n(t19, 2)?.chain_mul(x)
}

/// Curve25519's Montgomery `A`.
const J: u32 = 486_662;

/// `sqrt(-1) = 2^((p - 1) / 4)`, little-endian (RFC 9380 appendix I.2's `c2`).
const SQRT_M1: [u8; 32] = [
    0xb0, 0xa0, 0x0e, 0x4a, 0x27, 0x1b, 0xee, 0xc4, 0x78, 0xe4, 0x2f, 0xad, 0x06, 0x18, 0x43, 0x2f, 0xa7, 0xd7, 0xfb,
    0x3d, 0x99, 0x00, 0x4d, 0x2b, 0x0b, 0xdf, 0xc1, 0x4f, 0x80, 0x24, 0x83, 0x2b,
];

/// `2^((p + 3) / 8)`, little-endian (RFC 9380 appendix G.2.1's `c2`).
const TWO_P38: [u8; 32] = [
    0xb1, 0xa0, 0x0e, 0x4a, 0x27, 0x1b, 0xee, 0xc4, 0x78, 0xe4, 0x2f, 0xad, 0x06, 0x18, 0x43, 0x2f, 0xa7, 0xd7, 0xfb,
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

impl ConditionallySelectable for Fe {
    fn conditional_select(a: &Self, b: &Self, choice: Choice) -> Self {
        let mut out = Tight(Default::default());
        fiat::fiat_25519_selectznz(&mut out.0, choice.unwrap_u8(), &a.0.0, &b.0.0);
        Self(out)
    }
}

impl ConstantTimeEq for Fe {
    fn ct_eq(&self, other: &Self) -> Choice {
        self.to_bytes().as_slice().ct_eq(other.to_bytes().as_slice())
    }
}

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

    fn is_zero(self) -> Choice {
        self.ct_eq(&Self::zero())
    }

    /// RFC 9380's `sgn0`: the parity of the canonical value.
    fn is_odd(self) -> Choice {
        Choice::from(self.to_bytes().first().map_or(0, |b| b & 1))
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

    /// `self^((p - 5) / 8)`, by fiat-crypto.
    #[cfg(not(all(target_os = "zkvm", target_vendor = "succinct")))]
    fn pow_p58(self) -> Result<Self, MulError> {
        pow_p58_chain(self)
    }

    /// `self^((p - 5) / 8)`, by `sys_bigint` on the canonical value: one conversion each way around the chain.
    #[cfg(all(target_os = "zkvm", target_vendor = "succinct"))]
    fn pow_p58(self) -> Result<Self, MulError> {
        let x = zkvm::Canonical::from_bytes(&self.to_bytes())?;
        pow_p58_chain(x).map(|y| Self::from_bytes(&y.to_bytes()))
    }

    /// Replaces `0` with `1`, for [`Fe::pow_p58_and_inverse`]'s non-zero inputs.
    fn or_one(self) -> Self {
        Self::conditional_select(&self, &Self::one(), self.is_zero())
    }

    /// `(b^((p - 5) / 8), r^-1)` for non-zero `b` and `r`, from one exponentiation. With `e = (p - 5) / 8`,
    /// `beta_b = (b r^8)^e = b^e r^(p - 5) = b^e r^-4`, so `b^e = beta_b r^4`; and `(b^e)^8 = b^(p - 5) = b^-4`, so
    /// `beta_b (b^e)^7 b^4 r^3 = b^(8e) b^4 r^-4 r^3 = r^-1`.
    fn pow_p58_and_inverse(b: Self, r: Self) -> Result<(Self, Self), MulError> {
        let r2 = r.square();
        let r4 = r2.square();
        let beta_b = b.mul(r4.square()).pow_p58()?;
        let beta = beta_b.mul(r4);
        let beta7 = beta.square().mul(beta).square().mul(beta);
        Ok((beta, beta_b.mul(beta7).mul(b.square().square()).mul(r2.mul(r))))
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
pub(super) fn encode_to_curve_ell2(message: &[&[u8]], dst: &[u8]) -> Result<EdwardsPoint, MulError> {
    map_to_curve_ell2(Fe::from_be48(&expand_message_xmd_sha512_48(message, dst)))
}

/// Elligator 2 onto curve25519 and the rational map to edwards25519, straight-line (RFC 9380 appendices G.2.1 and
/// G.2.2), then the cofactor cleared. One exponentiation yields both G.2.1's square root and the inverse that makes
/// the edwards25519 point affine.
fn map_to_curve_ell2(u: Fe) -> Result<EdwardsPoint, MulError> {
    let (zero, one) = (Fe::zero(), Fe::one());
    let j = Fe::from_u32(J);
    let i = Fe::from_bytes(&SQRT_M1);
    // G.2.1: x1 = x1n / xd = -A / (1 + 2 u^2) and x2 = x2n / xd = 2 u^2 x1; g(x) = gx / gxd with gxd = xd^3
    let tv1 = u.square().add(u.square());
    let xd = tv1.add(one);
    let x1n = j.neg();
    let x2n = x1n.mul(tv1);
    let gxd = xd.square().mul(xd);
    let gx1 = j.mul(tv1).mul(x1n).add(xd.square()).mul(x1n);
    let gx2 = gx1.mul(tv1);
    let tv3 = gxd.square().mul(gxd).mul(gx1);
    // the affine conversion below divides by q = gx xd (xn + xd) of the chosen candidate: invert both candidates' q
    // together (each made non-zero; a zero q is the exceptional case, which needs no inverse)
    let q1 = gx1.mul(xd).mul(x1n.add(xd)).or_one();
    let q2 = gx2.mul(xd).mul(x2n.add(xd)).or_one();
    let (beta, inv_q1q2) = Fe::pow_p58_and_inverse(gxd.square().square().mul(tv3).or_one(), q1.mul(q2))?;
    let y11 = beta.mul(tv3);
    let y1 = Fe::conditional_select(&y11.mul(i), &y11, y11.square().mul(gxd).ct_eq(&gx1));
    let y21 = y11.mul(u).mul(Fe::from_bytes(&TWO_P38));
    let y2 = Fe::conditional_select(&y21.mul(i), &y21, y21.square().mul(gxd).ct_eq(&gx2));
    let e3 = y1.square().mul(gxd).ct_eq(&gx1);
    let xn = Fe::conditional_select(&x2n, &x1n, e3);
    let y = Fe::conditional_select(&y2, &y1, e3);
    let y = Fe::conditional_select(&y, &y.neg(), e3 ^ y.is_odd());
    let inv_q = Fe::conditional_select(&q1, &q2, e3).mul(inv_q1q2);
    // G.2.2: v = sqrt(-486664) xn / (xd y), w = (xn - xd) / (xn + xd), and (0, 1) where xd y (xn + xd) = 0. As
    // y^2 = gx / gxd, 1 / (xd y (xn + xd)) is y gxd / q.
    let n1 = xn.add(xd);
    let inv = y.mul(gxd).mul(inv_q);
    let exceptional = y.is_zero() | n1.is_zero();
    let v = Fe::conditional_select(&Fe::from_bytes(&SQRT_M_J2).mul(xn).mul(n1).mul(inv), &zero, exceptional);
    let w = Fe::conditional_select(&xn.sub(xd).mul(xd).mul(y).mul(inv), &one, exceptional);
    let mut enc = w.to_bytes();
    if let Some(last) = enc.last_mut() {
        *last |= v.is_odd().unwrap_u8() << 7;
    }
    // (v, w) is on edwards25519 by construction, so it decodes
    Ok(CompressedEdwardsY(enc).decompress().map_or(EdwardsPoint::default(), |pt| pt.mul_by_cofactor()))
}

/// libsodium's `ge25519_from_uniform` (Cardano draft-03): bit 255 of the input is the sign of `x`, the rest the
/// Elligator 2 representative `r`; `u = -A / (1 + 2 r^2)`, replaced by `-u - A` if `u (u^2 + A u + 1)` is a
/// non-square; `y = (u - 1) / (u + 1)` (or 1 when `u + 1 = 0` or the curve value is 0); the cofactor cleared.
///
/// Both candidates are fractions over `d = 1 + 2 r^2`: `u = un / d` with `un = -A`, or `A - A d` replaced. Then
/// `u (u^2 + A u + 1) = wn(un) / d^3` with `wn(un) = un (un^2 + A un d + d^2)`, whose Legendre symbol is that of
/// `wn(-A) d`; and `y = (un - d) / (un + d)`. One exponentiation yields the symbol and the inverse of both
/// candidates' denominators.
pub(super) fn from_uniform(input: &[u8; 32]) -> Result<EdwardsPoint, MulError> {
    let sign = input.last().map_or(0, |b| b & 0x80);
    let r = Fe::from_bytes(input);
    let one = Fe::one();
    let j = Fe::from_u32(J);
    let d = r.square().add(r.square()).add(one);
    let wn = |un: Fe| un.mul(un.square().add(j.mul(un).mul(d)).add(d.square()));
    let (un1, un2) = (j.neg(), j.sub(j.mul(d)));
    let (den1, den2) = (un1.add(d).or_one(), un2.add(d).or_one());
    // x = 0 would count as a square, as 0 is not a non-square (it never is 0: d and wn(-A) are non-zero)
    let x = wn(un1).mul(d).or_one();
    let (beta, inv_den12) = Fe::pow_p58_and_inverse(x, den1.mul(den2))?;
    // the Legendre symbol x^((p - 1) / 2) = (x^((p - 5) / 8))^4 x^2
    let replace = beta.square().square().mul(x.square()).ct_eq(&one.neg());
    let un = Fe::conditional_select(&un1, &un2, replace);
    let inv_den = Fe::conditional_select(&den2, &den1, replace).mul(inv_den12);
    let exceptional = un.add(d).is_zero() | wn(un).is_zero();
    let y = Fe::conditional_select(&un.sub(d).mul(inv_den), &one, exceptional);
    let mut enc = y.to_bytes();
    if let Some(last) = enc.last_mut() {
        *last |= sign;
    }
    // y is canonical and on the curve's u-side by construction; a failure would mean the identity input
    Ok(decode_draft03(&enc).map_or(EdwardsPoint::default(), |pt| pt.mul_by_cofactor()))
}

/// fiat-crypto's multiplications, which cannot fail.
#[cfg(not(all(target_os = "zkvm", target_vendor = "succinct")))]
impl ChainField for Fe {
    type Error = Infallible;

    fn chain_mul(self, that: Self) -> Result<Self, Infallible> {
        Ok(self.mul(that))
    }

    fn chain_square(self) -> Result<Self, Infallible> {
        Ok(self.square())
    }
}

#[cfg(test)]
mod tests;
#[cfg(all(target_os = "zkvm", target_vendor = "succinct"))]
mod zkvm;
