//! Rejections and library behaviour the implementation relies on: ZIP-215 versus libsodium versus RFC 8032 strict,
//! malformed lengths, high-S, identity keys, cross-scheme Schnorr, redacted secrets, and the RNG conveniences.

#![cfg(feature = "sign")]
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects,
    clippy::std_instead_of_alloc,
    clippy::std_instead_of_core,
    clippy::as_conversions,
    clippy::many_single_char_names,
    clippy::items_after_statements,
    clippy::doc_markdown,
    clippy::cast_possible_truncation,
    clippy::decimal_bitwise_operands,
    reason = "tests fail by panicking, and run with std"
)]

use curve25519_dalek::constants::ED25519_BASEPOINT_POINT;
use curve25519_dalek::edwards::CompressedEdwardsY;
use curve25519_dalek::scalar::Scalar;
use curve25519_dalek::traits::IsIdentity as _;
use dilf4s_core::hash::{DomainTag, Sha256, TaggedHash};
use dilf4s_core::hex;
use dilf4s_signing::{SchemeId, SigningError, bls, ecdsa, ed25519, ristretto255, schnorr};

fn sha512(parts: &[&[u8]]) -> [u8; 64] {
    dilf4s_core::hash::HashAlgorithm::Sha512.digest(&parts.concat()).try_into().unwrap()
}

fn b32(text: &str) -> [u8; 32] {
    hex::decode(text).unwrap().try_into().unwrap()
}

// ------------------------------------------------------------------ derivation paths (signing.md section 3)

#[test]
fn paths_parse_hardened_and_soft_segments_and_reject_malformed_ones() {
    use dilf4s_signing::keys::path;
    let serialized = |p: &str| path::parse(p).unwrap().iter().map(|i| i.serialized()).collect::<Vec<_>>();
    assert_eq!(serialized("m/44'/0/7"), [0x8000_002c, 0, 7]);
    assert_eq!(serialized("m/0h"), [0x8000_0000]);
    assert!(serialized("m").is_empty());
    assert_eq!(serialized("m/2147483647'"), [0xffff_ffff]);
    // an index is only built checked, so bit 31 of the serialised form and is_hardened always agree
    assert_eq!(path::ChildIndex::soft(0x8000_0000).unwrap_err().kind(), "InvalidDerivation");
    assert_eq!(path::ChildIndex::hardened(0x8000_0000).unwrap_err().kind(), "InvalidDerivation");
    for i in [path::ChildIndex::soft(0x7fff_ffff).unwrap(), path::ChildIndex::hardened(0).unwrap()] {
        assert_eq!(i.is_hardened(), i.serialized() >= path::HARDENED_OFFSET, "{i:?}");
    }
    // the same malformed paths the TypeScript SDK rejects: an empty or trailing segment is malformed, never dropped
    for bad in
        ["44'/0", "m/x", "m/2147483648", "m/-1", "m/+1", "m/0/", "m//0", "m/0''", " m/0", "M/0", "m/1e3", "m/0x1"]
    {
        assert_eq!(path::parse(bad).unwrap_err().kind(), "InvalidDerivation", "{bad}");
    }
}

// ------------------------------------------------------------------ curve25519-dalek decoding (ZIP-215's rules)

#[test]
fn dalek_decompress_accepts_non_canonical_y_and_negative_zero() {
    // y = p + 1 is y = 1, the identity
    let p1 = CompressedEdwardsY(b32("eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")).decompress();
    assert!(p1.is_some_and(|p| p.is_identity()));
    // x = 0 with the sign bit set decodes (to the positive point)
    let neg_zero =
        CompressedEdwardsY(b32("0100000000000000000000000000000000000000000000000000000000000080")).decompress();
    assert!(neg_zero.is_some_and(|p| p.is_identity()));
    assert!(
        ed25519::VerifyingKey::from_bytes(&b32("eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"))
            .is_ok()
    );
}

// ------------------------------------------------------------------ ZIP-215 and libsodium constructions

struct Fixture {
    a: Scalar,
    vk: ed25519::VerifyingKey,
    message: &'static [u8],
}

fn fixture() -> Fixture {
    let seed = [0x42u8; 32];
    let mut h = sha512(&[&seed])[..32].to_vec();
    h[0] &= 248;
    h[31] = (h[31] & 127) | 64;
    let a = Scalar::from_bytes_mod_order(h.try_into().unwrap());
    let (sk, vk) = ed25519::from_seed(&seed).unwrap();
    assert_eq!(vk.as_bytes(), &(ED25519_BASEPOINT_POINT * a).compress().to_bytes());
    let _ = sk;
    Fixture { a, vk, message: b"zip215" }
}

/// `S = r + H(R' || A || M) a` with `R' = rB + T` (T of order 8): valid only under the cofactored equation.
fn mixed_order_signature(f: &Fixture) -> Vec<u8> {
    let torsion = CompressedEdwardsY(b32("26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05"))
        .decompress()
        .unwrap();
    assert!(torsion.is_small_order() && !torsion.is_identity());
    let r = Scalar::from_bytes_mod_order_wide(&sha512(&[b"nonce"]));
    let r_prime = (ED25519_BASEPOINT_POINT * r + torsion).compress().to_bytes();
    let k = Scalar::from_bytes_mod_order_wide(&sha512(&[&r_prime, f.vk.as_bytes(), f.message]));
    [r_prime.as_slice(), &(r + k * f.a).to_bytes()].concat()
}

/// `r = 0`: `R` is the identity, encoded non-canonically as `y = p + 1`.
fn non_canonical_r_signature(f: &Fixture) -> Vec<u8> {
    let r = b32("eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f");
    let k = Scalar::from_bytes_mod_order_wide(&sha512(&[&r, f.vk.as_bytes(), f.message]));
    [r.as_slice(), &(k * f.a).to_bytes()].concat()
}

fn dalek_strict(vk: &[u8; 32], message: &[u8], sig: &[u8]) -> bool {
    let vk = ed25519_dalek::VerifyingKey::from_bytes(vk);
    let sig = ed25519_dalek::Signature::from_bytes(sig.try_into().unwrap());
    vk.is_ok_and(|vk| vk.verify_strict(message, &sig).is_ok())
}

#[test]
fn mixed_order_r_zip215_accepts_libsodium_and_strict_reject() {
    let f = fixture();
    let sig = ed25519::Signature::from_bytes(&mixed_order_signature(&f)).unwrap();
    assert!(ed25519::verify(&f.vk, f.message, &sig), "ZIP-215 is cofactored");
    assert!(!ed25519::verify_libsodium(&f.vk, f.message, &sig), "libsodium is cofactorless");
    assert!(!dalek_strict(f.vk.as_bytes(), f.message, sig.as_bytes()), "ed25519-dalek verify_strict is neither rule");
}

#[test]
fn non_canonical_r_zip215_accepts_libsodium_rejects() {
    let f = fixture();
    let sig = ed25519::Signature::from_bytes(&non_canonical_r_signature(&f)).unwrap();
    assert!(ed25519::verify(&f.vk, f.message, &sig), "ZIP-215 accepts a non-canonical R");
    assert!(!ed25519::verify_libsodium(&f.vk, f.message, &sig), "libsodium needs R's exact encoding");
}

#[test]
fn non_canonical_s_is_rejected_by_every_rule() {
    let f = fixture();
    let (sk, _) = ed25519::from_seed(&[0x42; 32]).unwrap();
    let sig = sk.sign(f.message);
    let s = Scalar::from_canonical_bytes(sig.as_bytes()[32..].try_into().unwrap()).unwrap();
    // S + L, as a 256-bit little-endian integer (it fits: S < L < 2^253)
    let l = b32("edd3f55c1a631258d69cf7a2def9de1400000000000000000000000000000010");
    let mut s_plus_l = [0u8; 32];
    let mut carry = 0u16;
    for i in 0..32 {
        let v = u16::from(s.to_bytes()[i]) + u16::from(l[i]) + carry;
        s_plus_l[i] = v as u8;
        carry = v >> 8;
    }
    let bumped = ed25519::Signature::from_bytes(&[&sig.as_bytes()[..32], s_plus_l.as_slice()].concat()).unwrap();
    assert!(ed25519::verify(&f.vk, f.message, &sig) && ed25519::verify_libsodium(&f.vk, f.message, &sig));
    assert!(!ed25519::verify(&f.vk, f.message, &bumped));
    assert!(!ed25519::verify_libsodium(&f.vk, f.message, &bumped));
}

#[test]
fn small_order_and_non_canonical_keys_under_libsodium() {
    // the identity key with S = 0 and R = identity verifies under ZIP-215 (every term vanishes), not libsodium
    let identity = b32("0100000000000000000000000000000000000000000000000000000000000000");
    let sig = [identity.as_slice(), &[0u8; 32]].concat();
    assert_eq!(SchemeId::Ed25519.verify_bytes(&identity, b"Zcash", &sig), Ok(true));
    assert_eq!(SchemeId::Ed25519Libsodium.verify_bytes(&identity, b"Zcash", &sig), Ok(false));
    // an encoding that is no curve point is a malformed key
    let off_curve = b32("0200000000000000000000000000000000000000000000000000000000000000");
    assert!(CompressedEdwardsY(off_curve).decompress().is_none());
    assert!(matches!(SchemeId::Ed25519.verify_bytes(&off_curve, b"", &sig), Err(SigningError::InvalidKey { .. })));
    // lengths
    assert!(matches!(ed25519::verify_bytes(&identity[1..], b"", &sig), Err(SigningError::InvalidEncoding { .. })));
    assert!(matches!(ed25519::verify_bytes(&identity, b"", &sig[1..]), Err(SigningError::InvalidEncoding { .. })));
}

#[test]
fn ed25519_signing_matches_ed25519_dalek() {
    for seed in [[0u8; 32], [0x7f; 32], [0xff; 32]] {
        let (sk, vk) = ed25519::from_seed(&seed).unwrap();
        let theirs = ed25519_dalek::SigningKey::from_bytes(&seed);
        assert_eq!(vk.as_bytes(), theirs.verifying_key().as_bytes());
        use ed25519_dalek::Signer as _;
        assert_eq!(sk.sign(b"abc").as_bytes(), &theirs.sign(b"abc").to_bytes());
    }
}

// ------------------------------------------------------------------ ECDSA

fn high_s(sig: &[u8; 64], order: &str) -> Vec<u8> {
    let n = dilf4s_core::BigInt::from_bytes_be(num_bigint::Sign::Plus, &hex::decode(order).unwrap());
    let s = dilf4s_core::BigInt::from_bytes_be(num_bigint::Sign::Plus, &sig[32..]);
    let (_, high) = (n - s).to_bytes_be();
    let mut out = sig[..32].to_vec();
    out.extend(std::iter::repeat_n(0, 32 - high.len()));
    out.extend(high);
    out
}

#[test]
fn ecdsa_high_s_and_ranges_are_invalid_encodings() {
    let p256_n = "ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551";
    let k256_n = "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141";
    let (sk, vk) = ecdsa::p256::from_seed(&[7; 32]).unwrap();
    let sig = sk.sign(b"m").unwrap();
    assert!(ecdsa::p256::verify(&vk, b"m", &sig));
    let twin = high_s(sig.as_bytes(), p256_n);
    assert!(matches!(ecdsa::p256::Signature::from_bytes(&twin), Err(SigningError::InvalidEncoding { .. })));
    let (sk, vk) = ecdsa::secp256k1::from_seed(&[7; 32]).unwrap();
    let sig = sk.sign(b"m").unwrap();
    assert!(ecdsa::secp256k1::verify(&vk, b"m", &sig));
    let twin = high_s(sig.as_bytes(), k256_n);
    assert!(matches!(ecdsa::secp256k1::Signature::from_bytes(&twin), Err(SigningError::InvalidEncoding { .. })));
    // r = 0, s = 0, r = n
    let zero = [0u8; 64];
    assert!(ecdsa::secp256k1::Signature::from_bytes(&zero).is_err());
    let r_is_n = [hex::decode(k256_n).unwrap(), vec![1; 32]].concat();
    assert!(ecdsa::secp256k1::Signature::from_bytes(&r_is_n).is_err());
    // keys: identity, off-curve x, x >= p, wrong tags, wrong lengths
    assert!(matches!(ecdsa::secp256k1::VerifyingKey::from_bytes(&[0; 33]), Err(SigningError::InvalidKey { .. })));
    let x_is_p =
        [vec![2], hex::decode("fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f").unwrap()].concat();
    assert!(ecdsa::secp256k1::VerifyingKey::from_bytes(&x_is_p).is_err());
    let mut wrong_tag = vk.as_bytes().to_vec();
    wrong_tag[0] = 0x04;
    assert!(ecdsa::secp256k1::VerifyingKey::from_bytes(&wrong_tag).is_err());
    assert!(matches!(ecdsa::p256::VerifyingKey::from_bytes(&[2; 32]), Err(SigningError::InvalidEncoding { .. })));
    // signing keys: zero and n are invalid keys; a wrong length is an encoding error
    assert!(matches!(ecdsa::p256::SigningKey::from_bytes(&[0; 32]), Err(SigningError::InvalidKey { .. })));
    assert!(matches!(
        ecdsa::secp256k1::SigningKey::from_bytes(&hex::decode(k256_n).unwrap()),
        Err(SigningError::InvalidKey { .. })
    ));
    assert!(matches!(ecdsa::p256::SigningKey::from_bytes(&[1; 31]), Err(SigningError::InvalidEncoding { .. })));
}

// ------------------------------------------------------------------ Schnorr

#[test]
fn bip340_and_tagged_schnorr_reject_each_other() {
    let aux = [0x42; 32];
    let (sk, vk) = schnorr::bip340::from_seed(&[3; 32]).unwrap();
    let (tsk, tvk) = schnorr::tagged::from_seed(&[3; 32]).unwrap();
    assert_eq!(vk.as_bytes(), tvk.as_bytes(), "shared key encodings");
    let m32 = [9u8; 32];
    let plain = sk.sign(&m32, &aux).unwrap();
    let tagged = tsk.sign(&m32, &aux).unwrap();
    assert!(schnorr::bip340::verify(&vk, &m32, &plain));
    assert!(schnorr::tagged::verify(&tvk, &m32, &tagged));
    assert_eq!(schnorr::tagged::verify_bytes(tvk.as_bytes(), &m32, plain.as_bytes()), Ok(false));
    assert_eq!(schnorr::bip340::verify_bytes(vk.as_bytes(), &m32, tagged.as_bytes()), Ok(false));
    // the tagged digest is core's TaggedHash with the dilf4s tag
    let tag = TaggedHash::<Sha256>::new(DomainTag::new(schnorr::TAG).unwrap());
    assert_eq!(schnorr::tagged_digest(b"x").as_slice(), tag.hash(b"x").as_bytes());
    assert_eq!(
        schnorr::bip340::verify_bytes(vk.as_bytes(), &schnorr::tagged_digest(&m32), tagged.as_bytes()),
        Ok(true),
        "tagged = BIP-340 over the digest"
    );
    // a BIP-340 message of any other length is malformed at the bytes level
    assert!(matches!(
        SchemeId::Bip340.verify_bytes(vk.as_bytes(), &[0; 31], plain.as_bytes()),
        Err(SigningError::InvalidEncoding { .. })
    ));
    // r >= p and s >= n verify false (only the length is checked)
    let mut r_high = *plain.as_bytes();
    r_high[..32].copy_from_slice(&[0xff; 32]);
    assert_eq!(schnorr::bip340::verify_bytes(vk.as_bytes(), &m32, &r_high), Ok(false));
    let mut s_high = *plain.as_bytes();
    s_high[32..].copy_from_slice(&[0xff; 32]);
    assert_eq!(schnorr::bip340::verify_bytes(vk.as_bytes(), &m32, &s_high), Ok(false));
    // x with no curve point; zero key
    assert!(matches!(schnorr::bip340::VerifyingKey::from_bytes(&[0; 32]), Err(SigningError::InvalidKey { .. })));
    assert!(matches!(schnorr::tagged::SigningKey::from_bytes(&[0; 32]), Err(SigningError::InvalidKey { .. })));
}

/// The BIP-340 reference vectors 0-3 (secret key, aux, message, signature).
#[test]
fn bip340_reference_vectors() {
    let cases = [
        (
            "0000000000000000000000000000000000000000000000000000000000000003",
            "0000000000000000000000000000000000000000000000000000000000000000",
            "0000000000000000000000000000000000000000000000000000000000000000",
            "e907831f80848d1069a5371b402410364bdf1c5f8307b0084c55f1ce2dca821525f66a4a85ea8b71e482a74f382d2ce5ebeee8fdb2172f477df4900d310536c0",
        ),
        (
            "b7e151628aed2a6abf7158809cf4f3c762e7160f38b4da56a784d9045190cfef",
            "0000000000000000000000000000000000000000000000000000000000000001",
            "243f6a8885a308d313198a2e03707344a4093822299f31d0082efa98ec4e6c89",
            "6896bd60eeae296db48a229ff71dfe071bde413e6d43f917dc8dcf8c78de33418906d11ac976abccb20b091292bff4ea897efcb639ea871cfa95f6de339e4b0a",
        ),
        (
            "c90fdaa22168c234c4c6628b80dc1cd129024e088a67cc74020bbea63b14e5c9",
            "c87aa53824b4d7ae2eb035a2b5bbbccc080e76cdc6d1692c4b0b62d798e6d906",
            "7e2d58d8b3bcdf1abadec7829054f90dda9805aab56c77333024b9d0a508b75c",
            "5831aaeed7b44bb74e5eab94ba9d4294c49bcf2a60728d8b4c200f50dd313c1bab745879a5ad954a72c45a91c3a51d3c7adea98d82f8481e0e1e03674a6f3fb7",
        ),
        (
            "0b432b2677937381aef05bb02a66ecd012773062cf3fa2549e44f58ed2401710",
            "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
            "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
            "7eb0509757e246f19449885651611cb965ecc1a187dd51b64fda1edc9637d5ec97582b9cb13db3933705b32ba982af5af25fd78881ebb32771fc5922efc66ea3",
        ),
    ];
    for (sk, aux, m, sig) in cases {
        let (sk, vk) = schnorr::bip340::from_seed(&hex::decode(sk).unwrap()).unwrap();
        let s = sk.sign(&b32(m), &b32(aux)).unwrap();
        assert_eq!(hex::encode(s.as_bytes()), sig);
        assert!(schnorr::bip340::verify(&vk, &b32(m), &s));
    }
}

// ------------------------------------------------------------------ ristretto255 and BLS

#[test]
fn ristretto255_identity_and_non_canonical_encodings() {
    let identity = [0u8; 32];
    assert!(matches!(ristretto255::VerifyingKey::from_bytes(&identity), Err(SigningError::InvalidKey { .. })));
    let (sk, vk) = ristretto255::from_seed(&[1; 32]).unwrap();
    let sig = sk.sign(b"m", &[0x42; 32]);
    assert!(ristretto255::verify(&vk, b"m", &sig));
    let mut r_identity = *sig.as_bytes();
    r_identity[..32].copy_from_slice(&identity);
    assert!(matches!(ristretto255::Signature::from_bytes(&r_identity), Err(SigningError::InvalidEncoding { .. })));
    let mut z_high = *sig.as_bytes();
    z_high[32..].copy_from_slice(&[0xff; 32]);
    assert!(matches!(ristretto255::Signature::from_bytes(&z_high), Err(SigningError::InvalidEncoding { .. })));
    assert!(ristretto255::SigningKey::from_bytes(&[0; 32]).is_err(), "zero scalar");
    assert!(ristretto255::SigningKey::from_bytes(&[0xff; 32]).is_err(), "non-canonical scalar");
    assert!(!ristretto255::verify(&vk, b"n", &sig));
}

#[test]
fn bls_identity_key_and_signature() {
    let identity_g1 = hex::decode(&format!("c0{}", "00".repeat(47))).unwrap();
    assert!(matches!(bls::VerifyingKey::from_bytes(&identity_g1), Err(SigningError::InvalidKey { .. })));
    let identity_g2 = hex::decode(&format!("c0{}", "00".repeat(95))).unwrap();
    let sig = bls::Signature::from_bytes(&identity_g2).unwrap();
    let (_, vk) = bls::from_seed(&[1; 32]).unwrap();
    assert!(!bls::verify(&vk, b"", &sig), "the identity signature decodes but never verifies");
    assert!(!bls::pop_verify(&vk, &sig));
}

// ------------------------------------------------------------------ secrets and randomness

#[test]
fn secret_keys_are_redacted() {
    let seed = [0xab; 32];
    let rendered = [
        format!("{:?}", ed25519::from_seed(&seed).unwrap().0),
        format!("{:?}", ecdsa::p256::from_seed(&seed).unwrap().0),
        format!("{:?}", ecdsa::secp256k1::from_seed(&seed).unwrap().0),
        format!("{:?}", schnorr::bip340::from_seed(&seed).unwrap().0),
        format!("{:?}", schnorr::tagged::from_seed(&seed).unwrap().0),
        format!("{:?}", ristretto255::from_seed(&seed).unwrap().0),
        format!("{:?}", bls::from_seed(&seed).unwrap().0),
        format!("{:?}", dilf4s_signing::keys::slip10::master(&seed).unwrap()),
        format!("{:?}", dilf4s_signing::secret::Secret::new(seed)),
        format!("{:?}", dilf4s_signing::secret::SecretBytes::new(&seed)),
    ];
    for r in rendered {
        assert!(r.contains("redacted"), "{r}");
        assert!(!r.to_lowercase().contains("abab"), "{r}");
        assert!(!r.contains("171, 171"), "{r}");
    }
}

/// A deterministic stand-in for a cryptographic RNG.
struct Counter(u8);

impl rand_core::RngCore for Counter {
    fn next_u32(&mut self) -> u32 {
        u32::from_le_bytes([self.next(); 4])
    }
    fn next_u64(&mut self) -> u64 {
        u64::from_le_bytes([self.next(); 8])
    }
    fn fill_bytes(&mut self, dst: &mut [u8]) {
        for b in dst {
            *b = self.next();
        }
    }
    fn try_fill_bytes(&mut self, dst: &mut [u8]) -> Result<(), rand_core::Error> {
        self.fill_bytes(dst);
        Ok(())
    }
}

impl rand_core::CryptoRng for Counter {}

impl Counter {
    fn next(&mut self) -> u8 {
        self.0 = self.0.wrapping_add(1);
        self.0
    }
}

#[test]
fn rng_conveniences_draw_explicit_randomness() {
    let aux: [u8; 32] = core::array::from_fn(|i| (i + 1) as u8);
    let (sk, vk) = schnorr::tagged::from_seed(&[5; 32]).unwrap();
    let via_rng = sk.sign_with_rng(b"m", &mut Counter(0)).unwrap();
    assert_eq!(via_rng, sk.sign(b"m", &aux).unwrap());
    assert!(schnorr::tagged::verify(&vk, b"m", &via_rng));
    let (rsk, rvk) = ristretto255::from_seed(&[5; 32]).unwrap();
    let r = rsk.sign_with_rng(b"m", &mut Counter(0));
    assert_eq!(r, rsk.sign(b"m", &aux));
    assert!(ristretto255::verify(&rvk, b"m", &r));
    let words = dilf4s_signing::keys::bip39::WordList::english();
    let m = dilf4s_signing::keys::bip39::generate(16, &words, &mut Counter(0)).unwrap();
    assert_eq!(m.len(), 12);
    assert!(dilf4s_signing::keys::bip39::generate(17, &words, &mut Counter(0)).is_err());
}
