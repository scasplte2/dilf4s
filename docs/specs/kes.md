# dilf4s key-evolving signatures (v1)

Status: normative. Conformance vectors: `shared/vectors/kes_v1.json`. Reference data for Cardano is in
`docs/test-vectors/kes/cardano/`.

A KES key signs for one period at a time and evolves forward. A key leaked at period `t` cannot sign for any earlier
period. Every construction here uses Ed25519 leaves; a leaf's 32-byte seed is its RFC 8032 secret key. Verifying keys
are fixed for the key's lifetime, and the period is an input to verification.

Keys are immutable values: evolving returns a new key. Erasing the old key's memory is best effort. On the JVM it
amounts to dropping every reference to past keys.

## 1. dilf4s sum composition, `KesSum[H]`

This is the MMM sum composition (Malkin–Micciancio–Miner 2002). Signatures carry a Merkle path, as in Topl/Bifrost.
It is parameterised by a hash `H` whose output has `n ≥ 32` bytes, and by a height `h ≤ 30`, giving `2^h` periods.

### Tagged hashes

All hashes use the tagged form of `canonical-encoding.md` §4 over `H`:

| Function | Definition |
|---|---|
| `expand(r)` | `(seedHash(00 ‖ r)[0:32], seedHash(01 ‖ r)[0:32])` with tag `dilf4s/kes/seed/v1` |
| `leaf(vk)` | tag `dilf4s/kes/leaf/v1` |
| `node(l, r)` | tag `dilf4s/kes/node/v1` over `l ‖ r` |

### Keys

- **Tree.** A height-0 key is a leaf `(seed, vk = Ed25519(seed))`, and its root is `leaf(vk)`. Generating a height-`d`
  key from seed `r`:
  1. Compute `(r0, r1) = expand(r)`.
  2. The active subtree is `gen(r0, d−1)`.
  3. Store `r1` and both subtree roots `L` and `R`.
  4. The root is `node(L, R)`.
- **Verifying key.** The root, `n` bytes.

### Signing and verification

- **Signing at period `t`.** The signature is `vk_leaf (32) ‖ Ed25519 sig (64) ‖ w_0 … w_{h−1}` (`n` bytes each,
  bottom-up). `w_i` is the sibling root at level `i`: `R` if bit `i` of `t` is 0, otherwise `L`. Length: `96 + n·h`.
- **Verification.**
  1. Start with `acc = leaf(vk_leaf)`.
  2. For each level `i`: if bit `i` of `t` is 0, `acc = node(acc, w_i)`; otherwise `acc = node(w_i, acc)`.
  3. Accept if and only if `acc` equals the verifying key and the leaf signature verifies under ZIP-215
     (`signing.md` §1.1).

### Evolution and encoding

- **Evolution `t → t+1`.** At the lowest level where `t+1` reaches the right half:
  1. Regenerate the right subtree from `r1`.
  2. Check that its root equals the stored `R`; a mismatch is an error.
  3. Zero `r1`.

  An exhausted key is an error.
- **Key encoding.**
  - A leaf is `seed ‖ vk`, with `seed` zeroed once erased.
  - A node is `active ‖ r1 ‖ L ‖ R`, with `r1` zeroed once used.
  - The length is `64 + (32 + 2n)·h`.
- **Loading.** A key is loaded together with its period. Loading checks:
  - each leaf's `vk` against its seed;
  - that each `r1` is zero exactly when the period says it is used;
  - each active subtree's root against the stored root.

## 2. dilf4s product composition, `KesProduct[H]`

A super sum tree of height `hs` certifies `2^hs` successive sub sum trees of height `hb`. That gives `2^(hs+hb)`
periods, with `t = t_sup · 2^hb + t_sub`.

- **Generation.**
  1. Compute `(s_sup, c) = expand(seed)`. The super tree is `gen(s_sup, hs)`.
  2. Compute `(s_sub, c') = expand(c)`. The sub tree is `gen(s_sub, hb)`, and `c'` is the next chain seed.
  3. Certify the sub tree.
- **Certification.**
  - The super tree signs `productHash(subRoot)`, with tag `dilf4s/kes/product/v1`, at `t_sup`.
  - Its active leaf is then erased, so a leaked key cannot certify a different sub tree for the same super period.
  - The tag keeps certification signatures distinct from message signatures.
- **Evolution.**
  - Within a sub tree, evolve the sub tree.
  - At the sub tree's end:
    1. Evolve the super tree.
    2. Compute `(s_sub, c'') = expand(c')` and build the new sub tree.
    3. Certify it.
- **Signature.** `superSig ‖ subRoot (n) ‖ subSig`.
- **Verification.** Verify `superSig` over `productHash(subRoot)` at `t_sup` under the verifying key, then `subSig`
  over the message at `t_sub` under `subRoot`.
- **Key encoding.** `super tree ‖ sub tree ‖ chain seed (32) ‖ superSig`. Loading at period `t` also checks that
  `superSig` certifies the sub tree.

## 3. Cardano SumKES, `CardanoSumKes`

This is byte-compatible with cardano-base `Cardano.Crypto.KES.Sum`, `SumKES Blake2b_256` over
`SingleKES Ed25519DSIGN`, for depths 0–7. `Sum6KES` is the Praos hot key, with 64 periods, 448-byte signatures and
608-byte keys.

| Element | Definition |
|---|---|
| Seed expansion | `(Blake2b-256(01 ‖ r), Blake2b-256(02 ‖ r))` |
| Verifying key | `Blake2b-256(vk0 ‖ vk1)` |
| Signature | `sigma ‖ vk0 ‖ vk1`, recursively, `64 + 64d` bytes |
| Signing key | `sk ‖ r1 ‖ vk0 ‖ vk1`, recursively, `32 + 96d` bytes, with `r1` zeroed after use |

Leaves verify under libsodium's Ed25519 rules (`signing.md`: canonical `S`, a canonical non-small-order `A`,
cofactorless), as cardano-node does. The hash is fixed by compatibility.
