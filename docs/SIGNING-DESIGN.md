# dilf4s Signing Module — Architecture Design

**Author:** CodeBot 🔍 (via James)  
**Date:** 2026-03-22  
**Status:** Draft — pending James's approval to begin implementation

---

## Overview

Add Ed25519, Extended Ed25519, Ed25519 VRF, and forward-secure KES signing
to dilf4s (Distributed Ledger Functionalities for Scala).

Reference implementations: BramblSc, Bifrost (Topl).  
Target spec compatibility: RFC 8032 (Ed25519), draft-irtf-cfrg-vrf-04 (VRF).

---

## Dependencies

### Add (Maven Central)

```
"cafe.cryptography" % "ed25519-elisabeth"    % "0.1.0"  // Ed25519 sign/verify protocol
"cafe.cryptography" % "curve25519-elisabeth"  % "0.1.0"  // EC point arithmetic (VRF needs this)
```

Both are MIT licensed, published by Jack Grigg (cryptography-cafe).

- `ed25519-elisabeth` — full Ed25519 signing: `Ed25519PrivateKey`, `Ed25519ExpandedPrivateKey`,
  `Ed25519PublicKey`, `Ed25519Signature`. Implements RFC 8032 §5.1 correctly.
- `curve25519-elisabeth` — exposes `EdwardsPoint(X,Y,Z,T)`, `FieldElement`, `Scalar`,
  `CompressedEdwardsY`. Public API for point add/sub/negate/multiply, double-scalar-mult,
  compress/decompress. This is what VRF needs (BouncyCastle hides all of this).

### Why Not weavechain/curve25519-elisabeth

James suggested it for optimizations (`addToExtended`, `dblInPlace`, `MulUtils.java`).
These are minor perf gains; the originals on Maven Central are sufficient and better maintained.
Skip the non-standard fork.

### BouncyCastle

**Keep for now** — existing `core` module uses BC for Blake2b hashing, SHA3, and Hex encoding.
The signing module won't touch BC. Could be removed later if we replace hash impls.

### HMAC-SHA-512

`javax.crypto.Mac` with `HmacSHA512` — confirmed working on JDK 21, 64-byte output.
No BC needed for Extended Ed25519's BIP32 key derivation.

---

## Multi-Module Structure

```
dilf4s/
├── lang/scala/build.sbt
├── lang/scala/project/
│   └── Dependencies.scala
│
├── models/                          # Pure data types, zero deps beyond cats
│   └── src/main/scala/xyz/kd5ujc/
│       ├── signing/
│       │   ├── SigningKey.scala
│       │   ├── VerificationKey.scala
│       │   ├── KeyPair.scala
│       │   └── Signature.scala
│       └── kes/
│           └── KesBinaryTree.scala  # ADT: MerkleNode, SigningLeaf, Empty
│
├── lang/scala/shared-test/                     # Test utilities, generators, hex helpers
│   └── src/main/scala/xyz/kd5ujc/
│       └── test/
│           ├── HexUtils.scala
│           └── generators/
│
├── lang/scala/core/                            # Existing: hash, accumulators, storage
│   └── src/                         # (current root src/ moves here)
│
├── signing/                         # Ed25519, Extended Ed25519
│   └── src/
│       ├── main/scala/xyz/kd5ujc/signing/
│       │   ├── Ed25519.scala
│       │   ├── ExtendedEd25519.scala
│       │   └── SignatureScheme.scala
│       └── test/
│
├── vrf/                             # ECVRF-ED25519-SHA512-TAI
│   └── src/
│       ├── main/scala/xyz/kd5ujc/vrf/
│       │   ├── VrfEd25519.scala
│       │   └── EcVrf25519.scala
│       └── test/
│
└── kes/                             # KES Sum + Product compositions
    └── src/
        ├── main/scala/xyz/kd5ujc/kes/
        │   ├── KesSum.scala
        │   ├── KesProduct.scala
        │   └── KesEd25519Blake2b256.scala
        └── test/
```

### Module Dependency Graph

```
models ← shared-test (test scope only)
models ← core (existing code)
models ← signing ← vrf
models ← signing ← kes (also uses core for Blake2b)
```

---

## Implementation Details

### 1. Ed25519 — Thin Scala Wrapper

ed25519-elisabeth already implements the full RFC 8032 protocol. Our wrapper:

```scala
class Ed25519 extends SignatureScheme[Ed25519.SecretKey, Ed25519.PublicKey] {
  def deriveKeyPairFromSeed(seed: Array[Byte]): KeyPair[SecretKey, PublicKey]
  def sign(sk: SecretKey, msg: Array[Byte]): Array[Byte]
  def verify(sig: Array[Byte], msg: Array[Byte], pk: PublicKey): Boolean
  def getVerificationKey(sk: SecretKey): PublicKey
}
```

Internally: `Ed25519PrivateKey.fromByteArray(sk).expand().sign(msg)`, etc.

### 2. Extended Ed25519 — Custom on curve25519-elisabeth

BIP32-Ed25519 (Khovratovich et al.). Not in ed25519-elisabeth; we implement:

- SecretKey = leftKey (32) + rightKey (32) + chainCode (32) = 96 bytes
- `scalarMultBase` via `Constants.ED25519_BASEPOINT_TABLE.multiply(Scalar.fromBits(leftKey))`
- Child key derivation with HMAC-SHA-512 via `javax.crypto.Mac`
- Public key derivation uses `EdwardsPoint.add()` for child public keys

### 3. VRF — ECVRF-ED25519-SHA512-TAI on curve25519-elisabeth

**Target spec:** draft-irtf-cfrg-vrf-04, suite `0x03`

This is the big architectural win. Instead of BramblSc's ~1,500 lines of forked
BouncyCastle internals (EC.scala, X25519Field.scala), we get clean protocol code
using curve25519-elisabeth's public API:

```scala
// Point decode + cofactor multiply (BramblSc needed private BC methods)
val H = new CompressedEdwardsY(hashBytes).decompress()
val cofactorH = H.multiply(Scalar.fromBits(cofactorBytes))

// Double scalar mult for verification: U = s*B - c*Y
val U = EdwardsPoint.vartimeDoubleScalarMultiplyBasepoint(
  cNeg, Y, s  // computes cNeg*Y + s*B
)
```

Protocol steps (~200-300 lines of Scala):
- `ECVRF_hash_to_curve_try_and_increment` — SHA-512, iterate ctr until valid point
- `ECVRF_hash_points` — domain-separated SHA-512 of concatenated point encodings
- `ECVRF_nonce_generation_RFC8032` — deterministic nonce from SK hash
- `ECVRF_prove` / `ECVRF_verify` / `ECVRF_proof_to_hash`

Key parameters:
- `suite_string = 0x03`
- `ptLen = 32`, `n = 16`, `qLen = 32`
- `PI_BYTES = 32 + 16 + 32 = 80`
- `cofactor = 8`
- Little-endian int encoding
- SHA-512 throughout

### 4. KES — Port from Bifrost

KES uses Ed25519 sign/verify as a black box + Blake2b-256 for tree hashing.
`KesBinaryTree` ADT (MerkleNode, SigningLeaf, Empty) is the algorithm's structure.

- **KesSum**: Binary tree key evolution, `O(log T)` key size for `T` time steps
- **KesProduct**: Composition of two KesSum instances for `T₁ × T₂` time steps
- Both delegate to `sSign`/`sVerify`/`sGenKeypair` which use our Ed25519

---

## Test Vector Sources

| Primitive | Source | Location | Count |
|-----------|--------|----------|-------|
| Ed25519 | RFC 8032 §7.1 | ed25519-elisabeth test resources | 7 |
| Ed25519 | Wycheproof | `eddsa_test.json` in ed25519-elisabeth | ~100+ |
| Ed25519 | Zip215 | ed25519-elisabeth test resources | varies |
| Ed25519 | BramblSc (Topl-specific) | `BramblSc/crypto/src/test/resources/signing/Ed25519.json` | varies |
| Extended Ed25519 | BramblSc | `BramblSc/crypto/src/test/resources/signing/ExtendedEd25519.json` | varies |
| VRF (TAI) | IETF draft-04 Appendix A.3 | `docs/specs/draft-irtf-cfrg-vrf-04.txt` lines 2143+ | 3 |
| VRF (TAI) | BramblSc | `BramblSc/crypto/src/test/resources/signing/VrfEd25519.json` | 5 |
| VRF (TAI) | reyzin/ecvrf reference | `/tmp/ecvrf/ed25519.cpp` (C++) | 3 |
| VRF (Elligator2) | IETF draft-04 Appendix A.4 | `docs/specs/draft-irtf-cfrg-vrf-04.txt` | 3 |
| KES Sum | Bifrost | `node-crypto/src/test/scala/.../KesSumSpec.scala` | varies |
| KES Product | Bifrost | `node-crypto/src/test/scala/.../KesProductSpec.scala` | varies |

### Canonical IETF VRF Test Vectors (ECVRF-ED25519-SHA512-TAI)

All use RFC 8032 §7.1 secret keys:

**Vector 1** — empty message:
```
SK:    9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60
PK:    d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a
alpha: (empty)
pi:    9275df67a68c8745c0ff97b48201ee6db447f7c93b23ae24cdc2400f52fdb08a
       1a6ac7ec71bf9c9c76e96ee4675ebff60625af28718501047bfd87b810c2d213
       9b73c23bd69de66360953a642c2a330a
beta:  a64c292ec45f6b252828aff9a02a0fe88d2fcc7f5fc61bb328f03f4c6c0657a9
       d26efb23b87647ff54f71cd51a6fa4c4e31661d8f72b41ff00ac4d2eec2ea7b3
```

**Vector 2** — message `72` (1 byte):
```
SK:    4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb
PK:    3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c
alpha: 72
pi:    84a63e74eca8fdd64e9972dcda1c6f33d03ce3cd4d333fd6cc789db12b5a7b9d
       03f1cb6b2bf7cd81a2a20bacf6e1c04e59f2fa16d9119c73a45a97194b504fb9
       a5c8cf37f6da85e03368d6882e511008
beta:  cddaa399bb9c56d3be15792e43a6742fb72b1d248a7f24fd5cc585b232c26c93
       4711393b4d97284b2bcca588775b72dc0b0f4b5a195bc41f8d2b80b6981c784e
```

**Vector 3** — message `af82` (2 bytes):
```
SK:    c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7
PK:    fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025
alpha: af82
pi:    aca8ade9b7f03e2b149637629f95654c94fc9053c225ec21e5838f193af2b727
       b84ad849b0039ad38b41513fe5a66cdd2367737a84b488d62486bd2fb110b480
       1a46bfca770af98e059158ac563b690f
beta:  d938b2012f2551b0e13a49568612effcbdca2aed5d1d3a13f47e180e01218916
       e049837bd246f66d5058e56d3413dbbbad964f5e9f160a81c9a1355dcd99b453
```

---

## Implementation Order

1. **Models module** — types, ADTs
2. **Signing: Ed25519** — wrap ed25519-elisabeth, RFC 8032 test vectors
3. **Signing: Extended Ed25519** — BIP32-Ed25519, BramblSc test vectors
4. **VRF** — ECVRF on curve25519-elisabeth, IETF + BramblSc test vectors
5. **KES** — port Sum + Product from Bifrost

---

## Reference Repos (local clones)

| Repo | Local Path | Purpose |
|------|-----------|---------|
| dilf4s | `~/repos/dilf4s` | Target project |
| BramblSc | `/tmp/BramblSc` | Reference Ed25519, ExtEd25519, VRF impls |
| Bifrost | `/tmp/Bifrost` | Reference VRF wrapper, KES impls |
| curve25519-elisabeth | `/tmp/curve25519-elisabeth` | EC library (weavechain fork, for reference) |
| ed25519-elisabeth | `/tmp/ed25519-elisabeth` | Ed25519 signing library |
| original-elisabeth | `/tmp/original-elisabeth` | Original cafe.cryptography curve25519 |
| ecvrf | `/tmp/ecvrf` | IETF reference implementation (C++, generates test vectors) |

---

## Key Architectural Decision

**Why curve25519-elisabeth over ported BouncyCastle internals:**

BouncyCastle's Ed25519 implementation makes ALL internal point types private
(`PointExt`, `PointAccum`, field arithmetic). VRF requires raw point operations:
decode → scalar multiply → add → encode → hash of coordinates.

BramblSc solved this by copying ~1,500 lines of BC source into Scala
(`EC.scala`, `X25519Field.scala`, `Ed25519.scala`). This fork became stale.

curve25519-elisabeth provides all needed operations as public API:
- `EdwardsPoint.X/Y/Z/T` — full extended coordinates
- `CompressedEdwardsY.decompress()` — point decode
- `EdwardsPoint.multiply(Scalar)` — scalar mult on arbitrary points
- `EdwardsPoint.add/subtract/negate` — point arithmetic
- `vartimeDoubleScalarMultiplyBasepoint(a, A, b)` — verification
- `FieldElement.isNegative()` — sign bit for encoding

Result: ~200-300 lines of clean VRF protocol code vs ~1,500 lines of copied field math.
