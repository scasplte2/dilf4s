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
| ristretto255-Schnorr | 32-byte LE canonical scalar, non-zero | 32-byte ristretto255 | 64 `R ‖ z` | raw message |

Every decoder checks lengths, and rejects keys and signatures that fail the scheme's validity rules below.

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

### 1.6 Seeds to keys

`SeedDerivation` maps a 32-byte seed to a key pair:

| Scheme | Mapping |
|---|---|
| Ed25519 | the identity |
| ECDSA and BIP-340 | the BE scalar, which must lie in `[1, n)` |
| ristretto255 | `SHA-512(seed) mod L` (non-zero) |

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
- Path syntax is `m/a/b'/c` (or `h` in place of `'`).
- Out-of-range or malformed segments are errors.
