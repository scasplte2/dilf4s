# dilf4s zero-knowledge verification (v1)

Status: normative. Conformance vectors: `shared/vectors/zk_v1.json`. Reference vectors are in `docs/test-vectors/zk/`:
go-ethereum's EIP-196/197 precompile vectors, and two real SP1 v6.1.0 proofs.

Everything here is **verification only**. The arithmetic is variable-time `BigInt` over public data: proofs, public
inputs, verifying keys and Merkle data. Never hash or multiply secrets with these functions.

## 1. BN254 (alt_bn128)

BN254 is byte-compatible with the EVM precompiles (EIP-196/197).

- **Field elements.** 32-byte big-endian, and must be below `p`. A value `≥ p` is *malformed*, never reduced.
- **G1.** `x ‖ y`, 64 bytes. `(0, 0)` is infinity; any other point must be on `y² = x³ + 3`.
- **G2.** `x.imag ‖ x.real ‖ y.imag ‖ y.real`, 128 bytes, in EIP-197 order. All zeros is infinity. Any other point
  must be on the twist `y² = x³ + 3/(9+i)` **and** in the order-`r` subgroup (`r·Q = O`).
- **Pairing.** The optimal ate pairing; a product of pairings is checked with one final exponentiation.

## 2. Groth16

A verifying key is `(α ∈ G1, β, γ, δ ∈ G2, IC ∈ G1^(n+1))`.

- **Proof encoding.** 256 bytes: `A (G1) ‖ B (G2) ‖ C (G1)` in the encodings above. `A`, `B` and `C` must not be the
  identity.
- **Public inputs.** Each `xᵢ` must satisfy `0 ≤ xᵢ < r`.
- **Check.** Let `L = IC₀ + Σ xᵢ·ICᵢ₊₁`. Accept if and only if `e(−A, B)·e(α, β)·e(L, γ)·e(C, δ) = 1`.

| Error | Meaning |
|---|---|
| Malformed | Wrong lengths, a non-canonical field element, or the wrong number of inputs. |
| Invalid proof | A point off the curve, outside the subgroup, or the identity; or an input `≥ r`. |
| Pairing check failed | The pairing equation does not hold. |

### SP1 (`SP1VerifierGroth16.sol`, circuit v6.1.0)

- **`proofBytes`.** `selector (4) ‖ exitCode ‖ vkRoot ‖ nonce ‖ proof (8 words)`, with selector `4388a21c`.
- **Required values.** `exitCode = 0` and `vkRoot = 0x002f850e…5352`.
- **Public inputs.** `[programVKey, sha256(publicValues) & (2^253 − 1), exitCode, vkRoot, nonce]`.
- **Verifying key.** The verifying key of the Solidity contract. Its negated `β`, `γ` and `δ` are negated back.

## 3. Poseidon

Poseidon is byte-compatible with circomlib and circomlibjs over the BN254 scalar field `r`.

- **Parameters.** S-box `x⁵`, 8 full rounds, and partial rounds 56/57/56/60 for widths `t` = 2/3/4/5. Constants are
  circomlibjs `poseidon_constants.js`.
- **Hashing.** `hash(x₁..xₙ)` for `1 ≤ n ≤ 4` canonical inputs uses width `t = n + 1` with state `[0, x₁, …, xₙ]`, and
  returns `state[0]`.

## 4. Poseidon Merkle tree

The tree is sparse, with fixed depth `d` and positions in `[0, 2^d)`. It is compatible with metakit and the SP1
shielded-transfer circuit.

- **Leaves.** Canonical field elements; 0 means empty.
- **Nodes.** `node = Poseidon(left, right)`.
- **Empty subtrees.** `zero₀ = 0` and `zeroᵢ₊₁ = Poseidon(zeroᵢ, zeroᵢ)`.
- **Proof.** `(position, siblings)`, with siblings **root-first**. Verification folds from the leaf upward: at level
  `i`, bit `i` of the position chooses whether the path node is the right child.
- **Absence.** An absence proof is an inclusion proof of 0.
