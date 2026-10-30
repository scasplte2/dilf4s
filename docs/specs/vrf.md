# dilf4s verifiable random functions (v1)

Status: normative. Conformance vectors: `shared/vectors/vrf_v1.json`. Reference vectors are in `docs/test-vectors/vrf/`:

- RFC 9381 Appendix B.3 and B.4;
- RFC 9380 J.5.2, K.1 and K.3;
- draft-03 Appendix A.4;
- the libsodium oracle corpus.

There are three suites over edwards25519 with SHA-512. They are distinct types, and none accepts another's proofs.

| Suite | Spec | `suite_string` | Secret key | Use |
|---|---|---|---|---|
| ECVRF-EDWARDS25519-SHA512-TAI | RFC 9381 §5.5 | `03` | 32-byte seed | general purpose |
| ECVRF-EDWARDS25519-SHA512-ELL2 | RFC 9381 §5.5 | `04` | 32-byte seed | general purpose (constant-time-friendly hash-to-curve) |
| ECVRF-ED25519-SHA512-Elligator2 | draft-irtf-cfrg-vrf-03 §5.5, as libsodium `crypto_vrf_ietfdraft03` | `04` | 64-byte `seed ‖ pk` | Cardano Praos compatibility |

**Common to all three suites:**

- **Keys:** `x = clamp(SHA-512(seed)[0:32])`, the RFC 8032 secret scalar. The verifying key is `Y = x·B`, 32 bytes.
- **Proof:** `pi = Gamma (32) ‖ c (16) ‖ s (32)`, 80 bytes.
- **Output:** `beta`, 64 bytes.
- **Nonce:** `k = SHA-512(SHA-512(seed)[32:64] ‖ H) mod L`.

`beta` is only ever returned by proving or by a successful verification. Proof-to-hash is never exposed for an
unverified proof.

## 1. RFC 9381 suites (TAI, ELL2)

These follow RFC 9381 §5, with these choices fixed:

- **Decoding.** All point encodings (`Y`, `Gamma` and the TAI candidates) use strict RFC 8032 §5.1.3 decoding. A
  non-canonical `y`, or `x = 0` with the sign bit set, is invalid.
- **Key validation (`validate_key`) is mandatory.** A `Y` of small order (`8·Y = 0`) is rejected. RFC 9381 leaves
  this optional.
- **Challenge.** `c = SHA-512(suite ‖ 02 ‖ Y ‖ H ‖ Gamma ‖ U ‖ V ‖ 00)[0:16]`, where `Y` is the public key's bytes as
  given.
- **Equations.** `U = s·B − c·Y` and `V = s·H − c·Gamma`, evaluated literally. `s ≥ L` is rejected.
- **Output.** `beta = SHA-512(suite ‖ 03 ‖ (8·Gamma) ‖ 00)`.
- **TAI hash-to-curve.** For `ctr` = 0…255, take the first `SHA-512(03 ‖ 01 ‖ Y ‖ alpha ‖ ctr ‖ 00)[0:32]` that
  strictly decodes, and return 8 times that point. If all 256 counters fail, that is an error; the probability is about
  2^-256.
- **ELL2 hash-to-curve.** `encode_to_curve(Y ‖ alpha)` of RFC 9380 suite `edwards25519_XMD:SHA-512_ELL2_NU_`:
  - DST: `"ECVRF_edwards25519_XMD:SHA-512_ELL2_NU_" ‖ 04`;
  - `expand_message_xmd` with SHA-512, 48 bytes;
  - big-endian `mod p`;
  - Elligator 2 with `Z = 2`, then the rational map with `c1 = sqrt(-486664)`, `sgn0(c1) = 0`;
  - cofactor cleared.

## 2. draft-03 (Cardano)

This is byte-compatible with the Cardano fork of libsodium. The 2,048-row oracle corpus and the 3,426 intermediate-step
comparisons in `docs/test-vectors/vrf/draft03-corpus` are normative test data.

- **Decoding.** `y` must be canonical (`y < p`). `x = 0` with the sign bit set is **accepted** and decodes to the
  positive point.
- **Key validation.** `Y` of small order is rejected.
- **Hash-to-curve.**
  - `r = SHA-512(04 ‖ 01 ‖ Y ‖ alpha)[0:32]`, with bit 255 cleared.
  - Elligator 2 (libsodium `ge25519_from_uniform`): `u = −A / (1 + 2r²)`. If `u(u² + Au + 1)` is a non-square, set
    `u = −u − A`.
  - Then `y = (u − 1)/(u + 1)`, or `y = 1` if `u + 1 = 0` or `u(u² + Au + 1) = 0`.
  - The sign of `x` is the input's bit 255 (here 0). Finally clear the cofactor.
  - `Y` is hashed as its re-encoding.
- **Challenge.** `c = SHA-512(04 ‖ 02 ‖ H ‖ Gamma ‖ U ‖ V)[0:16]`. `Y` is not included, there is no trailing octet,
  and `Gamma` is hashed as the bytes given in the proof.
- **Equations.** `U = (−c mod L)·Y + s·B` and `V = (−c mod L)·Gamma + s·H`. These differ from `s·B − c·Y` on
  mixed-order points.
- **Output.** `beta = SHA-512(04 ‖ 03 ‖ (8·Gamma))`.
- **Secret key.** The key is libsodium's 64-byte `seed ‖ pk`, and the stored `pk` must be the seed's.

## 3. Errors

| Error | Meaning |
|---|---|
| Malformed | A key or proof of the wrong length. |
| Rejected: invalid public key | The key does not decode. |
| Rejected: small-order public key | The key is of small order. |
| Rejected: invalid `Gamma` | `Gamma` does not decode. |
| Rejected: non-canonical scalar | `s ≥ L`. |
| Rejected: challenge mismatch | The recomputed challenge differs from `c`. |
