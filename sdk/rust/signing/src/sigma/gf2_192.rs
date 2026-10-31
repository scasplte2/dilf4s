//! GF(2^192) modulo the pentanomial `x^192 + x^7 + x^2 + x + 1` (`docs/specs/sigma.md` section 1), in which a
//! threshold node splits its challenge among its children. Bit `i` is the coefficient of `x^i`; the 24-byte encoding
//! is little-endian, so byte `j` holds bits `8j .. 8j+7` (Ergo's `GF2_192`).

/// `x^7 + x^2 + x + 1`: `x^192` reduced.
const PENTANOMIAL: u64 = 0x87;

/// An element: three 64-bit words, least significant first.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) struct Gf2_192([u64; 3]);

impl Gf2_192 {
    /// Zero.
    pub(crate) const ZERO: Self = Self([0; 3]);

    /// The element encoded by 24 little-endian bytes.
    pub(crate) fn from_bytes(bytes: &[u8; 24]) -> Self {
        let mut words = [0u64; 3];
        for (word, chunk) in words.iter_mut().zip(bytes.chunks_exact(8)) {
            let mut le = [0u8; 8];
            for (d, s) in le.iter_mut().zip(chunk) {
                *d = *s;
            }
            *word = u64::from_le_bytes(le);
        }
        Self(words)
    }

    /// The 24-byte little-endian encoding.
    pub(crate) fn to_bytes(self) -> [u8; 24] {
        let mut out = [0u8; 24];
        for (chunk, word) in out.chunks_exact_mut(8).zip(self.0) {
            for (d, s) in chunk.iter_mut().zip(word.to_le_bytes()) {
                *d = s;
            }
        }
        out
    }

    /// The element with value `b`: the points `1..=255` at which threshold children's challenges are evaluated.
    pub(crate) fn from_byte(b: u8) -> Self {
        Self([u64::from(b), 0, 0])
    }

    /// Addition, which is subtraction: XOR.
    pub(crate) fn add(self, that: Self) -> Self {
        let [a0, a1, a2] = self.0;
        let [b0, b1, b2] = that.0;
        Self([a0 ^ b0, a1 ^ b1, a2 ^ b2])
    }

    /// Multiplication by `x`: shift left one bit, folding `x^192` back in as `x^7 + x^2 + x + 1`.
    fn times_x(self) -> Self {
        let [w0, w1, w2] = self.0;
        let carry = w2.wrapping_shr(63);
        Self([
            w0.wrapping_shl(1) ^ (0u64.wrapping_sub(carry) & PENTANOMIAL),
            w1.wrapping_shl(1) | w0.wrapping_shr(63),
            w2.wrapping_shl(1) | w1.wrapping_shr(63),
        ])
    }

    /// Bit `i` (`0..192`).
    fn bit(self, i: u32) -> bool {
        let [w0, w1, w2] = self.0;
        let word = match i / 64 {
            0 => w0,
            1 => w1,
            _ => w2,
        };
        word.wrapping_shr(i % 64) & 1 == 1
    }

    /// Shift-and-add over the bits of `that`, from the highest: each step multiplies the accumulator by `x`.
    pub(crate) fn mul(self, that: Self) -> Self {
        (0..192u32).rev().fold(Self::ZERO, |acc, i| {
            let shifted = acc.times_x();
            if that.bit(i) { shifted.add(self) } else { shifted }
        })
    }
}

/// The polynomial with these coefficients (lowest degree first) at the point `x`, by Horner's rule.
pub(crate) fn evaluate(coefficients: &[Gf2_192], x: u8) -> Gf2_192 {
    let point = Gf2_192::from_byte(x);
    coefficients.iter().rev().fold(Gf2_192::ZERO, |acc, c| acc.mul(point).add(*c))
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unwrap_used, clippy::indexing_slicing, reason = "tests fail by panicking")]

    use super::{Gf2_192, evaluate};

    fn x_pow(n: usize) -> Gf2_192 {
        let x = Gf2_192::from_byte(2);
        (0..n).fold(Gf2_192::from_byte(1), |p, _| p.mul(x))
    }

    #[test]
    fn x_192_reduces_by_the_pentanomial() {
        assert_eq!(x_pow(192), Gf2_192::from_byte(0x87));
        assert_eq!(x_pow(191).to_bytes()[23], 0x80);
    }

    #[test]
    fn bytes_round_trip_little_endian() {
        let bytes: [u8; 24] = core::array::from_fn(|i| u8::try_from(i * 7 + 1).unwrap());
        let e = Gf2_192::from_bytes(&bytes);
        assert_eq!(e.to_bytes(), bytes);
        assert_eq!(e.0[0] & 0xff, 1);
    }

    #[test]
    fn multiplication_commutes_and_distributes() {
        let a = Gf2_192::from_bytes(&core::array::from_fn(|i| u8::try_from(i * 31 % 256).unwrap()));
        let b = Gf2_192::from_bytes(&core::array::from_fn(|i| u8::try_from((i * 17 + 3) % 256).unwrap()));
        let c = Gf2_192::from_bytes(&[0xa5; 24]);
        assert_eq!(a.mul(b), b.mul(a));
        assert_eq!(a.mul(b.add(c)), a.mul(b).add(a.mul(c)));
        assert_eq!(a.mul(b).mul(c), a.mul(b.mul(c)));
    }

    #[test]
    fn horner_evaluates_lowest_coefficient_first() {
        let c0 = Gf2_192::from_bytes(&[1; 24]);
        let c1 = Gf2_192::from_bytes(&[2; 24]);
        // Q(x) = c0 + c1·x; at 0 it is c0, at 1 it is c0 + c1
        assert_eq!(evaluate(&[c0, c1], 0), c0);
        assert_eq!(evaluate(&[c0, c1], 1), c0.add(c1));
        assert_eq!(evaluate(&[c0, c1], 3), c0.add(c1.mul(Gf2_192::from_byte(3))));
    }
}
