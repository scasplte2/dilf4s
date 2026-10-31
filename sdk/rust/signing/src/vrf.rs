//! Verifiable random functions over edwards25519 with SHA-512 (`vrf.md`): three suites, distinct types, none
//! accepting another's proofs.
//!
//! | Suite | Spec | Secret key |
//! |---|---|---|
//! | [`Tai`] | RFC 9381 ECVRF-EDWARDS25519-SHA512-TAI | 32-byte seed |
//! | [`Ell2`] | RFC 9381 ECVRF-EDWARDS25519-SHA512-ELL2 | 32-byte seed |
//! | [`Draft03`] | draft-irtf-cfrg-vrf-03 ECVRF-ED25519-SHA512-Elligator2, as Cardano's libsodium | 64-byte `seed \|\| pk` |
//!
//! Verification returns `beta` or a [`VrfError`]: [`VrfError::Malformed`] for wrong lengths, a rejection kind for
//! everything else. `beta` is only ever returned by proving or by a successful verification.

use alloc::format;
use core::fmt;
use core::marker::PhantomData;

use curve25519_dalek::constants::ED25519_BASEPOINT_POINT;
use curve25519_dalek::edwards::{CompressedEdwardsY, EdwardsPoint};
use curve25519_dalek::scalar::Scalar;

use crate::error::VrfError;
use crate::util::{ED25519_L, canonical_y, le_less_than, sha512};

/// Proof length (`Gamma (32) || c (16) || s (32)`).
pub const PROOF_LENGTH: usize = 80;
/// Output (`beta`) length.
pub const OUTPUT_LENGTH: usize = 64;
/// Verifying key length.
pub const VERIFYING_KEY_LENGTH: usize = 32;

/// The three suites, chosen at run time.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SuiteId {
    /// [`Tai`].
    Tai,
    /// [`Ell2`].
    Ell2,
    /// [`Draft03`].
    Draft03,
}

impl SuiteId {
    /// Every suite.
    pub const ALL: [Self; 3] = [Self::Tai, Self::Ell2, Self::Draft03];

    /// The suite's name, as `vrf_v1.json` spells it.
    pub const fn name(self) -> &'static str {
        match self {
            Self::Tai => "ECVRF-EDWARDS25519-SHA512-TAI",
            Self::Ell2 => "ECVRF-EDWARDS25519-SHA512-ELL2",
            Self::Draft03 => "ECVRF-ED25519-SHA512-Elligator2 (draft-03)",
        }
    }

    /// The suite with this name.
    pub fn from_name(name: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|s| s.name() == name)
    }

    /// The `suite_string` octet.
    pub const fn suite_byte(self) -> u8 {
        match self {
            Self::Tai => 0x03,
            Self::Ell2 | Self::Draft03 => 0x04,
        }
    }

    /// The secret key length.
    pub const fn signing_key_length(self) -> usize {
        match self {
            Self::Tai | Self::Ell2 => 32,
            Self::Draft03 => 64,
        }
    }

    /// Verification over raw encodings, returning `beta`.
    pub fn verify_bytes(self, key: &[u8], alpha: &[u8], proof: &[u8]) -> Result<[u8; 64], VrfError> {
        verify_raw(self, key, alpha, proof)
    }
}

mod sealed {
    pub trait Sealed {}
}

/// A VRF suite (a type-level [`SuiteId`]).
pub trait Suite: sealed::Sealed {
    /// The suite.
    const ID: SuiteId;
}

macro_rules! suite {
    ($(#[$doc:meta])* $ty:ident) => {
        $(#[$doc])*
        #[derive(Clone, Copy, Debug, PartialEq, Eq)]
        pub enum $ty {}
        impl sealed::Sealed for $ty {}
        impl Suite for $ty {
            const ID: SuiteId = SuiteId::$ty;
        }
    };
}

suite!(
    /// ECVRF-EDWARDS25519-SHA512-TAI (RFC 9381, suite `03`).
    Tai
);
suite!(
    /// ECVRF-EDWARDS25519-SHA512-ELL2 (RFC 9381, suite `04`).
    Ell2
);
suite!(
    /// ECVRF-ED25519-SHA512-Elligator2 of draft-irtf-cfrg-vrf-03, as Cardano's libsodium (`crypto_vrf_ietfdraft03`).
    Draft03
);

/// A verifying key of suite `S`: it decodes under the suite's rules and is not of small order.
pub struct VerifyingKey<S: Suite> {
    bytes: [u8; 32],
    point: EdwardsPoint,
    suite: PhantomData<S>,
}

impl<S: Suite> VerifyingKey<S> {
    /// Decodes 32 bytes: a wrong length is `Malformed`; then `InvalidPublicKey` or `SmallOrderPublicKey`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, VrfError> {
        let (bytes, point) = decode_key(S::ID, bytes)?;
        Ok(Self { bytes, point, suite: PhantomData })
    }

    /// The encoding, as given.
    pub const fn as_bytes(&self) -> &[u8; 32] {
        &self.bytes
    }
}

impl<S: Suite> Clone for VerifyingKey<S> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<S: Suite> Copy for VerifyingKey<S> {}
impl<S: Suite> PartialEq for VerifyingKey<S> {
    fn eq(&self, other: &Self) -> bool {
        self.bytes == other.bytes
    }
}
impl<S: Suite> Eq for VerifyingKey<S> {}
impl<S: Suite> fmt::Debug for VerifyingKey<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "VerifyingKey<{}>({:02x?})", S::ID.name(), self.bytes)
    }
}

/// An 80-byte proof `Gamma || c || s`; its fields are checked only by verification.
pub struct Proof<S: Suite> {
    bytes: [u8; 80],
    suite: PhantomData<S>,
}

impl<S: Suite> Proof<S> {
    /// Decodes 80 bytes; a wrong length is `Malformed`.
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, VrfError> {
        Ok(Self { bytes: proof_bytes(S::ID, bytes)?, suite: PhantomData })
    }

    /// The encoding.
    pub const fn as_bytes(&self) -> &[u8; 80] {
        &self.bytes
    }
}

impl<S: Suite> Clone for Proof<S> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<S: Suite> Copy for Proof<S> {}
impl<S: Suite> PartialEq for Proof<S> {
    fn eq(&self, other: &Self) -> bool {
        self.bytes == other.bytes
    }
}
impl<S: Suite> Eq for Proof<S> {}
impl<S: Suite> fmt::Debug for Proof<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Proof<{}>({:02x?})", S::ID.name(), self.bytes)
    }
}

/// The 64-byte VRF output `beta`, from proving or a successful verification only.
pub struct Output<S: Suite> {
    bytes: [u8; 64],
    suite: PhantomData<S>,
}

impl<S: Suite> Output<S> {
    /// The output bytes.
    pub const fn as_bytes(&self) -> &[u8; 64] {
        &self.bytes
    }
}

impl<S: Suite> Clone for Output<S> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<S: Suite> Copy for Output<S> {}
impl<S: Suite> PartialEq for Output<S> {
    fn eq(&self, other: &Self) -> bool {
        self.bytes == other.bytes
    }
}
impl<S: Suite> Eq for Output<S> {}
impl<S: Suite> fmt::Debug for Output<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Output<{}>({:02x?})", S::ID.name(), self.bytes)
    }
}

/// Verification: `beta`, or why the proof is rejected.
pub fn verify<S: Suite>(key: &VerifyingKey<S>, alpha: &[u8], proof: &Proof<S>) -> Result<Output<S>, VrfError> {
    verify_decoded(S::ID, &key.bytes, &key.point, alpha, &proof.bytes).map(|bytes| Output { bytes, suite: PhantomData })
}

/// Verification over raw encodings: the proof length, then the key length, are checked first (`Malformed`); every
/// later failure is a rejection.
pub fn verify_bytes<S: Suite>(key: &[u8], alpha: &[u8], proof: &[u8]) -> Result<Output<S>, VrfError> {
    verify_raw(S::ID, key, alpha, proof).map(|bytes| Output { bytes, suite: PhantomData })
}

fn proof_bytes(suite: SuiteId, bytes: &[u8]) -> Result<[u8; 80], VrfError> {
    <[u8; 80]>::try_from(bytes).map_err(|_| VrfError::Malformed {
        what: proof_what(suite),
        reason: format!("expected {PROOF_LENGTH} bytes, got {}", bytes.len()),
    })
}

const fn proof_what(suite: SuiteId) -> &'static str {
    match suite {
        SuiteId::Tai => "TAI proof",
        SuiteId::Ell2 => "ELL2 proof",
        SuiteId::Draft03 => "draft-03 proof",
    }
}

const fn key_what(suite: SuiteId) -> &'static str {
    match suite {
        SuiteId::Tai => "TAI public key",
        SuiteId::Ell2 => "ELL2 public key",
        SuiteId::Draft03 => "draft-03 public key",
    }
}

fn decode_key(suite: SuiteId, bytes: &[u8]) -> Result<([u8; 32], EdwardsPoint), VrfError> {
    let bytes: [u8; 32] = bytes.try_into().map_err(|_| VrfError::Malformed {
        what: key_what(suite),
        reason: format!("expected 32 bytes, got {}", bytes.len()),
    })?;
    let point = decode(suite, &bytes).ok_or(VrfError::InvalidPublicKey)?;
    if point.is_small_order() {
        return Err(VrfError::SmallOrderPublicKey);
    }
    Ok((bytes, point))
}

fn verify_raw(suite: SuiteId, key: &[u8], alpha: &[u8], proof: &[u8]) -> Result<[u8; 64], VrfError> {
    let proof = proof_bytes(suite, proof)?;
    let (key_bytes, y) = decode_key(suite, key)?;
    verify_decoded(suite, &key_bytes, &y, alpha, &proof)
}

/// The suite's point decoding: strict RFC 8032 for RFC 9381; canonical `y` with negative zero allowed for draft-03.
pub(crate) fn decode(suite: SuiteId, bytes: &[u8; 32]) -> Option<EdwardsPoint> {
    match suite {
        SuiteId::Tai | SuiteId::Ell2 => decode_strict(bytes),
        SuiteId::Draft03 => decode_draft03(bytes),
    }
}

/// RFC 8032 section 5.1.3: canonical `y` and no negative zero (the re-encoding equals the input).
fn decode_strict(bytes: &[u8; 32]) -> Option<EdwardsPoint> {
    CompressedEdwardsY(*bytes).decompress().filter(|p| p.compress().to_bytes() == *bytes)
}

/// libsodium's `ge25519_frombytes` with a canonicity check: `y < p`; `x = 0` with the sign bit decodes to the
/// positive point.
fn decode_draft03(bytes: &[u8; 32]) -> Option<EdwardsPoint> {
    if canonical_y(bytes) { CompressedEdwardsY(*bytes).decompress() } else { None }
}

fn encode(p: &EdwardsPoint) -> [u8; 32] {
    p.compress().to_bytes()
}

/// `c` (16 bytes) as a scalar (it is below 2^128, so canonical).
fn scalar16(c: &[u8; 16]) -> Scalar {
    let mut wide = [0u8; 32];
    for (w, b) in wide.iter_mut().zip(c) {
        *w = *b;
    }
    Scalar::from_bytes_mod_order(wide)
}

fn hash_to_curve(suite: SuiteId, pk: &[u8; 32], alpha: &[u8]) -> Result<EdwardsPoint, VrfError> {
    match suite {
        SuiteId::Tai => hash_to_curve_tai(pk, alpha),
        SuiteId::Ell2 => Ok(hash_to_curve_ell2(pk, alpha)),
        SuiteId::Draft03 => Ok(hash_to_curve_draft03(pk, alpha)),
    }
}

/// RFC 9381 section 5.4.1.1: the first `ctr` whose `SHA-512(03 || 01 || Y || alpha || ctr || 00)[0:32]` strictly
/// decodes, times the cofactor.
fn hash_to_curve_tai(pk: &[u8; 32], alpha: &[u8]) -> Result<EdwardsPoint, VrfError> {
    (0u8..=255)
        .find_map(|ctr| {
            let h = sha512(&[&[0x03, 0x01], pk, alpha, &[ctr, 0x00]]);
            let (candidate, _) = h.split_first_chunk::<32>()?;
            decode_strict(candidate)
        })
        .map(|p| p.mul_by_cofactor())
        .ok_or_else(|| VrfError::HashToCurveFailed { reason: "no counter in 0..255 gave a curve point".into() })
}

/// The ELL2 hash-to-curve DST: `"ECVRF_" || h2c_suite_ID_string || suite_string`.
const ELL2_DST: &[u8] = b"ECVRF_edwards25519_XMD:SHA-512_ELL2_NU_\x04";

/// RFC 9381 section 5.4.1.2: `encode_to_curve(Y || alpha)` of RFC 9380's `edwards25519_XMD:SHA-512_ELL2_NU_`.
fn hash_to_curve_ell2(pk: &[u8; 32], alpha: &[u8]) -> EdwardsPoint {
    EdwardsPoint::encode_to_curve::<sha2::Sha512>(&[pk, alpha], &[ELL2_DST])
}

/// draft-03 (libsodium): `r = SHA-512(04 || 01 || Y || alpha)[0:32]` with bit 255 cleared, then Elligator 2.
fn hash_to_curve_draft03(pk: &[u8; 32], alpha: &[u8]) -> EdwardsPoint {
    let h = sha512(&[&[0x04, 0x01], pk, alpha]);
    let mut r = [0u8; 32];
    for (o, b) in r.iter_mut().zip(h.iter()) {
        *o = *b;
    }
    if let Some(last) = r.last_mut() {
        *last &= 0x7f;
    }
    field::from_uniform(&r)
}

fn challenge(suite: SuiteId, pk: &[u8; 32], h: &[u8; 32], gamma: &[u8; 32], u: &[u8; 32], v: &[u8; 32]) -> [u8; 16] {
    let digest = match suite {
        SuiteId::Tai | SuiteId::Ell2 => sha512(&[&[suite.suite_byte(), 0x02], pk, h, gamma, u, v, &[0x00]]),
        SuiteId::Draft03 => sha512(&[&[0x04, 0x02], h, gamma, u, v]),
    };
    let mut c = [0u8; 16];
    for (o, b) in c.iter_mut().zip(digest.iter()) {
        *o = *b;
    }
    c
}

/// `U = s B - c Y` (and `V = s H - c Gamma`) as each suite's reference evaluates it: literally for RFC 9381,
/// `(-c mod L) P + s Q` for draft-03 (they differ on mixed-order points).
///
/// Every input is public, so this runs in variable time. The multiscalar multiplication computes the exact integer
/// multiples of the canonical scalars, torsion components included: `s Q - c P` is `s Q + c (-P)`.
fn equation(suite: SuiteId, p: &EdwardsPoint, q: &EdwardsPoint, c: &Scalar, s: &Scalar) -> EdwardsPoint {
    use curve25519_dalek::traits::VartimeMultiscalarMul as _;
    match suite {
        SuiteId::Tai | SuiteId::Ell2 => EdwardsPoint::vartime_multiscalar_mul([s, c], [q, &-p]),
        SuiteId::Draft03 => EdwardsPoint::vartime_multiscalar_mul([&(Scalar::ZERO - c), s], [p, q]),
    }
}

fn output(suite: SuiteId, gamma: &EdwardsPoint) -> [u8; 64] {
    let g8 = encode(&gamma.mul_by_cofactor());
    match suite {
        SuiteId::Tai | SuiteId::Ell2 => sha512(&[&[suite.suite_byte(), 0x03], &g8, &[0x00]]),
        SuiteId::Draft03 => sha512(&[&[0x04, 0x03], &g8]),
    }
}

/// Splits a proof into `Gamma`, `c` and `s`.
fn split_proof(proof: &[u8; 80]) -> ([u8; 32], [u8; 16], [u8; 32]) {
    let (g, rest) = proof.split_at(32);
    let (c, s) = rest.split_at(16);
    let (mut gb, mut cb, mut sb) = ([0u8; 32], [0u8; 16], [0u8; 32]);
    gb.copy_from_slice(g);
    cb.copy_from_slice(c);
    sb.copy_from_slice(s);
    (gb, cb, sb)
}

fn verify_decoded(
    suite: SuiteId,
    key: &[u8; 32],
    y: &EdwardsPoint,
    alpha: &[u8],
    proof: &[u8; 80],
) -> Result<[u8; 64], VrfError> {
    let (gb, cb, sb) = split_proof(proof);
    let gamma = decode(suite, &gb).ok_or(VrfError::InvalidGamma)?;
    if !le_less_than(&sb, &ED25519_L) {
        return Err(VrfError::NonCanonicalScalar);
    }
    let s = Scalar::from_bytes_mod_order(sb);
    let c = scalar16(&cb);
    let h = hash_to_curve(suite, &encode(y), alpha)?;
    let u = equation(suite, y, &ED25519_BASEPOINT_POINT, &c, &s);
    let v = equation(suite, &gamma, &h, &c, &s);
    if challenge(suite, key, &encode(&h), &gb, &encode(&u), &encode(&v)) != cb {
        return Err(VrfError::ChallengeMismatch);
    }
    Ok(output(suite, &gamma))
}

/// GF(2^255 - 19) on public data only (variable time), for draft-03's Elligator map, which curve25519-dalek does not
/// expose.
mod field {
    use alloc::vec::Vec;

    use curve25519_dalek::edwards::EdwardsPoint;
    use num_bigint::BigUint;

    use super::decode_draft03;

    fn p() -> BigUint {
        (BigUint::from(1u8) << 255u32) - BigUint::from(19u8)
    }

    const J: u32 = 486_662;

    fn le(bytes: &[u8]) -> BigUint {
        BigUint::from_bytes_le(bytes)
    }

    fn le32(x: &BigUint) -> [u8; 32] {
        let mut out = [0u8; 32];
        for (o, b) in out.iter_mut().zip(x.to_bytes_le()) {
            *o = b;
        }
        out
    }

    struct Fp {
        p: BigUint,
    }

    impl Fp {
        fn add(&self, a: &BigUint, b: &BigUint) -> BigUint {
            (a + b) % &self.p
        }
        fn sub(&self, a: &BigUint, b: &BigUint) -> BigUint {
            (a + &self.p - (b % &self.p)) % &self.p
        }
        fn mul(&self, a: &BigUint, b: &BigUint) -> BigUint {
            (a * b) % &self.p
        }
        fn neg(&self, a: &BigUint) -> BigUint {
            self.sub(&BigUint::ZERO, a)
        }
        /// `a^-1`, and `0` for `0`.
        fn inv0(&self, a: &BigUint) -> BigUint {
            a.modpow(&(&self.p - BigUint::from(2u8)), &self.p)
        }
        /// Whether `a` is a non-zero non-square (its Legendre symbol is -1).
        fn is_non_square(&self, a: &BigUint) -> bool {
            let exp = (&self.p - BigUint::from(1u8)) >> 1u32;
            a.modpow(&exp, &self.p) == &self.p - BigUint::from(1u8)
        }
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

/// Intermediate steps, exposed for the libsodium oracle corpus's helper rows. Not a stable API.
#[doc(hidden)]
pub mod internals {
    use curve25519_dalek::edwards::EdwardsPoint;
    use curve25519_dalek::scalar::Scalar;

    use super::SuiteId;

    /// draft-03 decoding: the point's re-encoding, and whether it is of small order.
    pub fn decode_draft03(bytes: &[u8; 32]) -> Option<([u8; 32], bool)> {
        super::decode_draft03(bytes).map(|p| (p.compress().to_bytes(), p.is_small_order()))
    }

    /// libsodium's `ge25519_from_uniform`, re-encoded.
    pub fn from_uniform_draft03(input: &[u8; 32]) -> [u8; 32] {
        super::field::from_uniform(input).compress().to_bytes()
    }

    /// draft-03's `(-c mod L) P + s Q` on two draft-03 encodings, re-encoded; `None` if either does not decode.
    pub fn equation_draft03(p: &[u8; 32], q: &[u8; 32], c: &[u8; 16], s: &[u8; 32]) -> Option<[u8; 32]> {
        let (p, q) = (super::decode_draft03(p)?, super::decode_draft03(q)?);
        let s = Scalar::from_bytes_mod_order(*s);
        Some(super::equation(SuiteId::Draft03, &p, &q, &super::scalar16(c), &s).compress().to_bytes())
    }

    /// The full draft-03 pipeline on raw inputs: `H || U || V || c || beta`, or `None` where verification would stop
    /// before computing them (the challenge is not compared).
    pub fn pipeline_draft03(pk: &[u8; 32], proof: &[u8; 80], alpha: &[u8]) -> Option<alloc::vec::Vec<u8>> {
        let suite = SuiteId::Draft03;
        let y = super::decode_draft03(pk).filter(|p| !p.is_small_order())?;
        let (gb, cb, sb) = super::split_proof(proof);
        let gamma = super::decode_draft03(&gb)?;
        if !crate::util::le_less_than(&sb, &crate::util::ED25519_L) {
            return None;
        }
        let s = Scalar::from_bytes_mod_order(sb);
        let c = super::scalar16(&cb);
        let h: EdwardsPoint = super::hash_to_curve_draft03(&super::encode(&y), alpha);
        let u = super::equation(suite, &y, &curve25519_dalek::constants::ED25519_BASEPOINT_POINT, &c, &s);
        let v = super::equation(suite, &gamma, &h, &c, &s);
        let (hb, ub, vb) = (super::encode(&h), super::encode(&u), super::encode(&v));
        let challenge = super::challenge(suite, pk, &hb, &gb, &ub, &vb);
        Some(crate::util::concat(&[&hb, &ub, &vb, &challenge, &super::output(suite, &gamma)]))
    }

    /// RFC 9381 ELL2's hash-to-curve (`encode_to_curve(Y || alpha)`), re-encoded.
    pub fn hash_to_curve_ell2(pk: &[u8; 32], alpha: &[u8]) -> [u8; 32] {
        super::hash_to_curve_ell2(pk, alpha).compress().to_bytes()
    }

    /// draft-03's hash-to-curve, re-encoded.
    pub fn hash_to_curve_draft03(pk: &[u8; 32], alpha: &[u8]) -> [u8; 32] {
        super::hash_to_curve_draft03(pk, alpha).compress().to_bytes()
    }

    /// RFC 9380 `encode_to_curve` with `edwards25519_XMD:SHA-512_ELL2_NU_`, re-encoded; `None` unless the DST is 1 to
    /// 255 bytes (curve25519-dalek panics outside that range).
    pub fn encode_to_curve_ell2(message: &[u8], dst: &[u8]) -> Option<[u8; 32]> {
        (1..=255)
            .contains(&dst.len())
            .then(|| EdwardsPoint::encode_to_curve::<sha2::Sha512>(&[message], &[dst]).compress().to_bytes())
    }
}

#[cfg(feature = "sign")]
pub use signing::{SigningKey, from_seed};

#[cfg(feature = "sign")]
mod signing {
    use core::fmt;
    use core::marker::PhantomData;

    use curve25519_dalek::edwards::EdwardsPoint;
    use curve25519_dalek::scalar::Scalar;
    use subtle::ConstantTimeEq as _;
    use zeroize::{Zeroize, ZeroizeOnDrop, Zeroizing};

    use super::{Output, Proof, Suite, SuiteId, VerifyingKey, challenge, encode, hash_to_curve, output};
    use crate::ed25519::expand;
    use crate::error::VrfError;
    use crate::util::sha512;

    /// A VRF secret key: the 32-byte seed (with the seed's public key stored after it for draft-03, as libsodium).
    #[derive(Zeroize, ZeroizeOnDrop)]
    pub struct SigningKey<S: Suite> {
        seed: [u8; 32],
        #[zeroize(skip)]
        public: [u8; 32],
        #[zeroize(skip)]
        suite: PhantomData<S>,
    }

    fn public_of(seed: &[u8; 32]) -> [u8; 32] {
        let (x, _) = expand(seed);
        EdwardsPoint::mul_base(&x).compress().to_bytes()
    }

    impl<S: Suite> SigningKey<S> {
        /// Decodes the suite's secret key: 32 bytes for RFC 9381; `seed || pk` (64) for draft-03, whose `pk` must be
        /// the seed's (`KeyMismatch`). A wrong length is `Malformed`.
        pub fn from_bytes(bytes: &[u8]) -> Result<Self, VrfError> {
            let length = S::ID.signing_key_length();
            if bytes.len() != length {
                return Err(VrfError::Malformed {
                    what: "VRF signing key",
                    reason: alloc::format!("expected {length} bytes, got {}", bytes.len()),
                });
            }
            let (seed, stored) = bytes.split_at(32);
            let seed: [u8; 32] = seed.try_into().map_err(|_| VrfError::KeyMismatch)?;
            let public = public_of(&seed);
            if !stored.is_empty() && stored != public {
                return Err(VrfError::KeyMismatch);
            }
            Ok(Self { seed, public, suite: PhantomData })
        }

        /// The secret key's encoding: the seed, followed by the public key for draft-03.
        pub fn to_bytes(&self) -> Zeroizing<alloc::vec::Vec<u8>> {
            let mut out = Zeroizing::new(self.seed.to_vec());
            if S::ID == SuiteId::Draft03 {
                out.extend_from_slice(&self.public);
            }
            out
        }

        /// The verifying key `Y = x B`.
        pub fn verifying_key(&self) -> VerifyingKey<S> {
            let (x, _) = expand(&self.seed);
            VerifyingKey { bytes: self.public, point: EdwardsPoint::mul_base(&x), suite: PhantomData }
        }

        /// The proof and output for `alpha`. Secret scalars meet only constant-time operations; hash-to-curve sees
        /// public inputs only.
        pub fn prove(&self, alpha: &[u8]) -> Result<(Proof<S>, Output<S>), VrfError> {
            let suite = S::ID;
            let (x, prefix) = expand(&self.seed);
            let x = Zeroizing::new(x);
            let h = hash_to_curve(suite, &self.public, alpha)?;
            let hb = encode(&h);
            let gamma = h * *x;
            let gb = encode(&gamma);
            let k = Zeroizing::new(Scalar::from_bytes_mod_order_wide(&sha512(&[&*prefix, &hb])));
            let c = challenge(suite, &self.public, &hb, &gb, &encode(&EdwardsPoint::mul_base(&k)), &encode(&(h * *k)));
            let s = super::scalar16(&c) * *x + *k;
            let mut bytes = [0u8; 80];
            for (o, b) in bytes.iter_mut().zip(gb.iter().chain(c.iter()).chain(s.to_bytes().iter())) {
                *o = *b;
            }
            Ok((Proof { bytes, suite: PhantomData }, Output { bytes: output(suite, &gamma), suite: PhantomData }))
        }
    }

    impl<S: Suite> Clone for SigningKey<S> {
        fn clone(&self) -> Self {
            Self { seed: self.seed, public: self.public, suite: PhantomData }
        }
    }

    impl<S: Suite> fmt::Debug for SigningKey<S> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "vrf::SigningKey<{}>(<redacted>)", S::ID.name())
        }
    }

    impl<S: Suite> PartialEq for SigningKey<S> {
        fn eq(&self, other: &Self) -> bool {
            self.seed.ct_eq(&other.seed).into()
        }
    }

    impl<S: Suite> Eq for SigningKey<S> {}

    /// A key pair from a 32-byte seed (draft-03 keys become `seed || pk`).
    pub fn from_seed<S: Suite>(seed: &[u8]) -> Result<(SigningKey<S>, VerifyingKey<S>), VrfError> {
        let seed: [u8; 32] = seed.try_into().map_err(|_| VrfError::Malformed {
            what: "VRF seed",
            reason: alloc::format!("expected 32 bytes, got {}", seed.len()),
        })?;
        let sk = SigningKey { seed, public: public_of(&seed), suite: PhantomData };
        let vk = sk.verifying_key();
        Ok((sk, vk))
    }
}
