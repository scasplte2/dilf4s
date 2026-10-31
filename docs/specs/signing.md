# dilf4s signatures and key derivation (v1)

Status: normative. Conformance vectors: `shared/vectors/signing_v1.json`.

All byte strings are written in hex. `‖` is concatenation. `LE`/`BE` are little/big-endian. `L` is the order of the
ed25519 / ristretto255 prime-order group, `n` the order of the named elliptic curve.

## 1. Signature schemes

| Scheme | Secret key | Verifying key | Signature | Message input |
|---|---|---|---|---|
| Ed25519 | 32-byte RFC 8032 seed | 32-byte point | 64 `R ‖ S` | raw message |
| Ed25519-BIP32 | 64-byte `kL ‖ kR` | 32-byte point | 64 `R ‖ S` | raw message |
| P-256-ECDSA | 32-byte BE scalar in `[1, n)` | 33-byte compressed SEC1 | 64 `r ‖ s` (BE) | `SHA-256(message)` |
| secp256k1-ECDSA | 32-byte BE scalar in `[1, n)` | 33-byte compressed SEC1 | 64 `r ‖ s` (BE) | `SHA-256(message)` |
| BIP-340 | 32-byte BE scalar in `[1, n)` | 32-byte x-only | 64 `R.x ‖ s` | exactly 32 bytes, as given |
| secp256k1-tagged-Schnorr | 32-byte BE scalar in `[1, n)` | 32-byte x-only | 64 `R.x ‖ s` | `taggedHash_SHA256("dilf4s/sig/bip340/v1", message)` |
| BLS12-381-PoP | 32-byte BE scalar in `[1, r)` | 48-byte compressed G1 | 96-byte compressed G2 | raw message |
| ristretto255-Schnorr | 32-byte LE canonical scalar, non-zero | 32-byte ristretto255 | 64 `R ‖ z` | raw message |

Every decoder checks lengths, and rejects keys and signatures that fail the scheme's validity rules below. The two
error kinds are distinct:

- **A wrong length** is `InvalidEncoding`, for secret keys, verifying keys and signatures alike.
- **A key of the right length that is not a key** is `InvalidKey`: a secret scalar out of range (or an Ed25519-BIP32
  `kL` that is not a multiple of 8 below 2^255), or a verifying key that does not decode or fails validation. That a
  backend library failed to parse it changes nothing: it is still `InvalidKey`.
- A malformed signature is `InvalidEncoding`, under the per-scheme rules below.

The `keyDecoding` vectors record the kind for each scheme.

### 1.1 Ed25519

Signing follows RFC 8032 §5.1.6 (pure Ed25519, no context). The secret key is the 32-byte seed.

**Verification is ZIP-215**, the consensus rule:

- `A` and `R` must decode to curve points. Non-canonical `y` encodings (`y ≥ p`) and the sign bit on `x = 0` are accepted.
- `S` must be canonical (`S < L`).
- Accept if and only if `[8]([S]B − R − [k]A) = 0`, where `k = SHA-512(R ‖ A ‖ M) mod L` over the encodings as given.

A third rule, **libsodium** (`crypto_sign_ed25519_verify_detached`), exists only for compatibility with systems built on
it, such as Cardano KES leaves. It requires all of the following:

- `S < L`;
- `A` with canonical `y` and not of small order;
- `[S]B − [k]A`, computed cofactorless, encodes exactly to `R` and is not of small order.

ZIP-215 fixes the accept set exactly, so every implementation agrees on every signature (batch-compatible). An
RFC 8032 strict verifier, which additionally requires canonical `A` and `R`, may be offered but is not the default.

### 1.2 Ed25519-BIP32 (Khovratovich–Law, as used by Icarus / CIP-0003)

- `kL` is read as a 256-bit LE integer. It must be a multiple of 8 with bit 255 clear. Decoders reject anything else.
- `A = [kL mod L]B`.
- `r = SHA-512(kR ‖ M) mod L`, `R = [r]B`.
- `S = (r + SHA-512(R ‖ A ‖ M) · kL) mod L`.

Signatures verify under §1.1.

### 1.3 ECDSA (P-256, secp256k1)

Nonces are deterministic, per RFC 6979 with HMAC-SHA-256 over the SHA-256 digest of the message.

**Verifying keys** are compressed SEC1: tag `02` or `03`, then `x`, naming a point on the curve. Any other tag is
`InvalidKey`, even where a SEC1 parser would accept it (`00`, `04`, `06` and `07` belong to other lengths, and some
parsers take others). A 65-byte uncompressed key is a wrong length, `InvalidEncoding`.

**Only low-S is valid:**

- The signer outputs `s ≤ n/2`, replacing `s` by `n − s` if needed.
- Decoders and verifiers reject `s > n/2`, so each signature has a single encoding.

### 1.4 BIP-340 and tagged Schnorr

There are two distinct schemes. They share key encodings, but **neither accepts the other's signatures**.

- **BIP-340** is the standard algorithm over a message of exactly 32 bytes. It is byte-compatible with Bitcoin
  (Taproot), Nostr and external wallets. Implementations expose it with a 32-byte message type, so a message of any
  other length is unrepresentable and never silently hashed or padded.
- **secp256k1-tagged-Schnorr** is BIP-340 applied to the tagged digest of a message of any length. The tagged hash
  follows `canonical-encoding.md` §4. This is the dilf4s default for signing dilf4s data.

In both schemes:

- The 32-byte auxiliary randomness `a` is fresh per signature in production.
- The vectors use `a = 42…42` (32 bytes). The BIP-340 vectors sign `SHA-256(m)` for each test message `m`.

### 1.5 ristretto255 Schnorr (FROST ciphersuite)

This is the single-signer form of FROST(ristretto255, SHA-512), RFC 9591. Let `ctx = "FROST-RISTRETTO255-SHA512-v1"`:

- `H2(m) = SHA-512(ctx ‖ "chal" ‖ m) mod L`
- `H3(m) = SHA-512(ctx ‖ "nonce" ‖ m) mod L`

Signing:

- `k = H3(a ‖ sk)`, where `a` is 32 bytes of fresh randomness (`42…42` in the vectors).
- `R = [k]G`.
- `c = H2(R ‖ PK ‖ M)`.
- `z = k + c · sk mod L`.

Verification:

- Decode `PK` and `R` as canonical ristretto255 encodings, **rejecting the identity**.
- `z` must be canonical.
- Accept if and only if `[z]G = R + [c]PK`.

A FROST threshold signature over the group key verifies under the same rule.

### 1.6 BLS12-381 (Ethereum consensus profile)

This is the minimal-pubkey-size variant with the proof-of-possession ciphersuite
`BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_` (draft-irtf-cfrg-bls-signature, as in Eth2). It is byte-compatible with
the ethereum/bls12-381-tests v0.1.2 suite and with tessellation-bls.

- **Public keys.** Must pass KeyValidate: decode, lie in G1, and are **not the identity**.
- **Signatures.** Must decode and lie in G2. The identity decodes but never verifies.
- **Operations.**
  - `Sign` and `Verify`;
  - `Aggregate` (one or more signatures);
  - `FastAggregateVerify` (one message, keys with proofs of possession);
  - `AggregateVerify`;
  - `PopProve` and `PopVerify`, which use DST `BLS_POP_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_`.
- **Seed derivation.** EIP-2333 `derive_master_SK`.

The backend is blst (`tech.pegasys:jblst`), which runs secret-key operations in constant time.

### 1.7 Seeds to keys

`SeedDerivation` maps a 32-byte seed to a key pair:

| Scheme | Mapping |
|---|---|
| Ed25519 | the identity |
| ECDSA and BIP-340 | the BE scalar, which must lie in `[1, n)` |
| ristretto255 | `SHA-512(seed) mod L` (non-zero) |
| BLS12-381 | EIP-2333 `derive_master_SK(seed)` |

## 2. Key derivation

### 2.1 BIP-39

Entropy is 16, 20, 24, 28 or 32 bytes. Each mnemonic word encodes 11 bits of `entropy ‖ SHA-256(entropy)[0 : ENT/32 bits]`.

- **Parsing:** NFKD-normalise, split on whitespace (including U+3000), and look each word up in the 2048-word list.
- **Rejected:** wrong word counts, unknown words and checksum mismatches are all errors.
- **Seed:** `PBKDF2-HMAC-SHA512(NFKD(words joined by " "), "mnemonic" ‖ NFKD(passphrase), 2048, 64)`.

### 2.2 SLIP-10 (Ed25519)

- **Master:** `I = HMAC-SHA512("ed25519 seed", seed)`, where the seed is 16–64 bytes.
- **Child:** `I = HMAC-SHA512(cc, 00 ‖ k ‖ BE32(i))`. Only hardened `i` is allowed.
- In both cases the key is `I[0:32]` (an Ed25519 seed) and the chain code is `I[32:64]`.

### 2.3 BIP32 (secp256k1)

Derivation follows BIP32 for private keys:

- master key `"Bitcoin seed"`;
- hardened data `00 ‖ k ‖ BE32(i)`, normal data `serP(K) ‖ BE32(i)`;
- `k' = I[0:32] + k mod n`.

An index whose `I[0:32] ≥ n`, or whose resulting key is zero, is an error. The caller moves to the next index.
`I[0:32] = 0` is not an error for a child: it adds nothing, so the child's key is the parent's (BIP32 skips only
`I_L ≥ n`). For the master key, `I[0:32]` is the key itself, so 0 is an error.

Public derivation (CKDpub, soft indices only): `I = HMAC-SHA512(cc, serP(K) ‖ BE32(i))`, `K' = K + [I[0:32]]G`.

### 2.4 BIP32-Ed25519 (Icarus, CIP-0003)

**Root.** `PBKDF2-HMAC-SHA512(passphrase, entropy, 4096, 96)`, then clamp:

- `b[0] &= f8`;
- `b[31] &= 1f`;
- `b[31] |= 40`.

The result is `kL ‖ kR ‖ cc`. A 96-byte root may also be supplied directly; it is clamped the same way.

**Child.** Index `i` is `LE32`, and `A` is the parent verifying key:

| | `Z` | chain code |
|---|---|---|
| hardened | `HMAC-SHA512(cc, 00 ‖ kL ‖ kR ‖ i)` | `HMAC-SHA512(cc, 01 ‖ kL ‖ kR ‖ i)[32:64]` |
| soft | `HMAC-SHA512(cc, 02 ‖ A ‖ i)` | `HMAC-SHA512(cc, 03 ‖ A ‖ i)[32:64]` |

The child keys are:

- `kL' = 8 · LE(Z[0:28]) + LE(kL)`;
- `kR' = (LE(Z[32:64]) + LE(kR)) mod 2^256`.

The additions are fixed-width LE byte arithmetic.

**Public derivation (soft only).** `A' = A + [8 · LE(Z[0:28])]B`. `A` must be the canonical encoding.

## 3. Indices and paths

- An index is below 2^31. Hardened indices are serialised with bit 31 set.
- Path syntax is `m/a/b'/c` (or `h` in place of `'`); `m` alone is the master key.
- A segment is one or more ASCII digits `0`–`9`, then optionally `'` or `h`. Leading zeros are allowed (`007` is 7).
- Nothing else is accepted, and nothing is trimmed: whitespace anywhere, a sign (`+5`, `-0`), a non-ASCII digit, an
  empty segment (`m/`, `m//1`, `m/1/`), an uppercase `M` or `H`, or a lone `'` is an error. So is an index of 2^31 or
  more. The `paths` vectors pin these cases.

## 4. Sizes

The sizes of every cryptographic byte string in dilf4s are fixed by the normative schema `shared/schemas/crypto.cddl`,
reproduced verbatim and checked by a test against real outputs of every implementation. Validity rules beyond size
are in the sections above.

```cddl
; dilf4s cryptographic byte strings: their sizes (docs/specs/signing.md, vrf.md, kes.md, zk.md). Normative for size;
; the internal layout and validity rules (curve membership, canonical scalars, low-S, ...) are in the specs.

; Ed25519 (RFC 8032; ZIP-215 verification)
ed25519-signing-key = bytes .size 32
ed25519-verifying-key = bytes .size 32
ed25519-signature = bytes .size 64

; Ed25519-BIP32 (Icarus / CIP-0003): kL || kR, and an extended key's chain code
ed25519-bip32-signing-key = bytes .size 64
chain-code = bytes .size 32

; ECDSA over P-256 and secp256k1: compressed SEC1 keys, compact low-S r || s
ecdsa-signing-key = bytes .size 32
ecdsa-verifying-key = bytes .size 33
ecdsa-signature = bytes .size 64

; BIP-340 and dilf4s tagged Schnorr over secp256k1: x-only keys
schnorr-signing-key = bytes .size 32
schnorr-verifying-key = bytes .size 32
schnorr-signature = bytes .size 64
bip340-message = bytes .size 32

; ristretto255 Schnorr (FROST ciphersuite)
ristretto255-signing-key = bytes .size 32
ristretto255-verifying-key = bytes .size 32
ristretto255-signature = bytes .size 64

; BLS12-381, Ethereum PoP profile (keys in G1, signatures in G2, compressed)
bls-signing-key = bytes .size 32
bls-verifying-key = bytes .size 48
bls-signature = bytes .size 96

; ECVRF (RFC 9381 TAI and ELL2: 32-byte seed keys; Cardano draft-03: 64-byte seed || pk)
vrf-signing-key = bytes .size 32
vrf-draft03-signing-key = bytes .size 64
vrf-verifying-key = bytes .size 32
vrf-proof = bytes .size 80
vrf-output = bytes .size 64

; Cardano Sum6KES (the Praos hot key); other depths are 32 + 96 d (key) and 64 + 64 d (signature)
sum6-kes-signing-key = bytes .size 608
sum6-kes-verifying-key = bytes .size 32
sum6-kes-signature = bytes .size 448

; BIP-39 entropy (16, 20, 24, 28 or 32 bytes; the step of 4 is checked by the domain)
bip39-entropy = bytes .size (16..32)

; Groth16 over BN254 (EIP-197 layout) and the SP1 v6.1.0 wrapper
groth16-proof = bytes .size 256
sp1-proof = bytes .size 356
sp1-program-vkey = bytes .size 32
```
