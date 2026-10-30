# dilf4s zero-knowledge verification (v1)

Status: normative. Conformance vectors: `shared/vectors/poseidon_v1.json` (Poseidon in `core.hash`, the tree in `accumulators.poseidon`). Reference vectors are in `docs/test-vectors/zk/`:
go-ethereum's EIP-196/197 precompile vectors, and two real SP1 v6.1.0 proofs.

Everything here is **verification only**. The arithmetic is variable time, over public data: proofs, public
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

- **Parameters.** S-box `x⁵`, 8 full rounds, and partial rounds 56/57/56/60 for widths `t` = 2/3/4/5.
- **Rounds.** Each round adds its `t` round constants, applies the S-box to every element (full rounds) or to element 0
  only (partial rounds), and multiplies by the MDS matrix, `newᵢ = Σⱼ Mᵢⱼ·sⱼ`. The first 4 and last 4 rounds are full.
- **Hashing.** `hash(x₁..xₙ)` for `1 ≤ n ≤ 4` canonical inputs uses width `t = n + 1` with state `[0, x₁, …, xₙ]`, and
  returns `state[0]`.

### Constants

The constants are generated as the Poseidon paper specifies (Grassi et al., eprint 2019/458, Appendix F; the authors'
`generate_parameters_grain.sage`), from a Grain LFSR:

- **Seed.** 80 bits, each field most significant bit first:
  - field type 1 (prime), 2 bits;
  - S-box 0 (`x^α`), 4 bits;
  - field size 254, 12 bits;
  - `t`, 12 bits;
  - full rounds 8, 10 bits;
  - partial rounds, 10 bits;
  - then 30 ones.
- **Steps.** Each step appends `b₆₂ ⊕ b₅₁ ⊕ b₃₈ ⊕ b₂₃ ⊕ b₁₃ ⊕ b₀` and drops `b₀`. The first 160 steps are discarded.
- **Output.** Bits are drawn in pairs: the second is output if the first is 1, and both are discarded otherwise. A
  draw is 254 output bits, most significant first.
- **Round constants.** `(8 + RP)·t` draws in round order. A draw `≥ r` is discarded and drawn again.
- **MDS matrix.** The next `2t` draws, reduced mod `r` (not redrawn), are `x₀..x_{t−1}, y₀..y_{t−1}`. Then
  `Mᵢⱼ = 1/(xᵢ + yⱼ)`.
  - The reference script redraws if the points repeat or some `xᵢ + yⱼ = 0`. It also redraws until the matrix passes
    its invariant-subspace checks (Algorithms 1–3).
  - For every width here the first draw is usable, and it is the matrix circomlib uses. An implementation must fail
    rather than redraw.
  - dilf4s does not reimplement Algorithms 1–3. Their verdict cannot change the matrices, which compatibility fixes.

The constants equal circomlibjs's (`poseidon_constants.js` and `poseidon_constants_opt.js` at commit `48b3ab37`) for
every width, table for table. In the code:
- `PoseidonDerivation` (core tests) derives them.
- `PoseidonConstantsGen` writes the checked-in tables.
- `PoseidonCircomlibCheck` compares a circomlibjs checkout with the derivation:
  `sbt "core/Test/runMain xyz.kd5ujc.core.hash.PoseidonCircomlibCheck <circomlibjs>/src"`.
  circomlibjs is GPL-3.0, so its files are not in this repository. `PoseidonSuite` pins a digest of each width's
  tables as the check prints them.

### Evaluation (informative)

The implementation evaluates the equivalent optimised form (the paper's Appendix B, laid out as circomlibjs's
`poseidon_opt`):
- round constants are moved through the linear layers, leaving one scalar per partial round;
- each partial round's matrix is factored into a sparse one, of `2t − 1` multiplications, and the factors left over are
  folded into one matrix before the first partial round.

The optimised tables are derived from the constants above, and tests check the two forms agree.

## 4. Poseidon Merkle tree

The tree is sparse, with fixed depth `d` and positions in `[0, 2^d)`. It is compatible with metakit and the SP1
shielded-transfer circuit.

- **Leaves.** Canonical field elements; 0 means empty.
- **Nodes.** `node = Poseidon(left, right)`.
- **Empty subtrees.** `zero₀ = 0` and `zeroᵢ₊₁ = Poseidon(zeroᵢ, zeroᵢ)`.
- **Proof.** `(position, siblings)`, with siblings **root-first**. Verification folds from the leaf upward: at level
  `i`, bit `i` of the position chooses whether the path node is the right child.
- **Absence.** An absence proof is an inclusion proof of 0.

### Proof encoding

The proof's wire form is the canonical value below: the normative schema `shared/schemas/poseidon.cddl`, reproduced
verbatim and checked by a test. Field elements are integers in `[0, r)`. Decoders also require `position < 2^depth`,
where depth is the number of siblings; in the Scala API that invariant, and the field range, are carried by the types
(`Fr`, `PoseidonMerkleProof`).

```cddl
; dilf4s Poseidon Merkle tree proofs, v1 (docs/specs/zk.md section 4). Normative for shape.
; Siblings are root-first BN254 scalar-field elements. The tree depth is the number of siblings; the domain decoder
; additionally requires position < 2^depth.

poseidon-merkle-proof = {
  "position": tree-position,
  "siblings": [0*256 field-element],
}

tree-position = 0..115792089237316195423570985008687907853269984665640564039457584007913129639935

field-element = 0..21888242871839275222246405745257275088548364400416034343698204186575808495616
```
