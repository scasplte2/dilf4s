# dilf4s sigma protocols (v1)

Status: normative. Conformance vectors:
- `shared/vectors/sigma_v1.json`: proofs made by Ergo's prover.
- `shared/vectors/sigma_dilf4s_v1.json`: proofs made by ours, each accepted by Ergo's verifier.

Both files carry Ergo's verdict on every case. They are produced by `tools/sigma-vectors` (dev-only, Scala 2.13,
outside the build) over sigmastate 5.0.13.

A sigma proof shows knowledge of secret keys for a tree of statements joined by `and`, `or` and `threshold`, without
revealing which branches the prover knew. The construction is Cramer–Damgård–Schoenmakers composition of Schnorr and
Diffie-Hellman-tuple proofs, made non-interactive by one Fiat-Shamir challenge over the whole tree. Every byte layout
here is Ergo's (`sigmastate` 5.0.13), so proofs verify in both systems in both directions. This implementation is our
own and has had no external audit; the construction has been in production on Ergo since 2019.

## 1. Group, scalars and challenges

| Object | Encoding |
|---|---|
| Group | secp256k1, generator `G`, order `q` |
| Group element | 33 bytes, SEC1 compressed (`02`/`03` ‖ x). The identity is 33 zero bytes. |
| Scalar (response `z`) | 32 bytes, big-endian, `z < q` |
| Challenge | 24 bytes (192-bit soundness) |

**Decoding is canonical.** A group element decodes only from its own encoding: 33 bytes, prefix `02` or `03` with `x`
a valid coordinate below the field modulus, or the 33-zero identity. Programs compare points as bytes (ZeroJoin
compares `c1` and `c2`), so a second encoding of one point is never accepted.

**A challenge has two readings of the same 24 bytes.**
- **As a scalar** at the leaves: big-endian unsigned. It is below 2^192, so below `q`.
- **As an element of GF(2^192)** in thresholds: polynomial basis modulo `x^192 + x^7 + x^2 + x + 1`. Byte `j` holds the
  coefficients of `x^(8j)` to `x^(8j+7)`, least significant bit first (little-endian).
- **XOR** of challenges is bytewise.

## 2. Statements

| Statement | Proves knowledge of `w` with | Commitment from challenge `e`, response `z` |
|---|---|---|
| `dlog(pk)` | `pk = w·G` | `a = z·G − e·pk` |
| `dhtuple(g, h, u, v)` | `u = w·g` and `v = w·h` | `a = z·g − e·u`, `b = z·h − e·v` |

A real prover with nonce `r` commits to `r·G` (`dlog`), or to `(r·g, r·h)` (`dhtuple`), and answers
`z = r + e·w mod q`.

## 3. Propositions

A proposition is a canonical map per node, discriminated by `type`. Each type takes exactly these fields:

| `type` | Fields |
|---|---|
| `dlog` | `pk`: group element (bytes) |
| `dhtuple` | `g`, `h`, `u`, `v`: group elements |
| `and`, `or` | `children`: array of propositions |
| `threshold` | `k`: integer; `children`: array of propositions |

**Constraints**, each a malformed-input error:
- no statement point (`pk`, `g`, `h`, `u`, `v`) is the identity. `dlog(O)` holds for `w = 0`, so anyone could prove it,
  and likewise a tuple with `u` or `v` the identity; an identity base proves nothing. Commitments may be the identity:
  they are computed, never read;
- a connective has 1 to 255 children; an empty one would verify for anyone;
- a threshold has `1 <= k <= n`;
- a proposition nests at most 64 deep and has at most 4096 leaves;
- no field outside its type's row, so the encoding is canonical;
- the canonical form weighs at most 7,900,416 (below), the weight of the heaviest proposition the other constraints
  allow.

**Decoding order.** A decoder refuses a proposition at its first failing check, in this order, so that every
implementation refuses the same input with the same error. No check depends on a part of the value not yet reached,
and no input is decoded in full before it is refused for its size.
1. **Weight.** A value heavier than 7,900,416 is refused before anything is decoded. Weight is the JLVM's measure of
   size (`jlvm.md` §2). Both implementations cache it on every value, so the check is O(1) however much the value
   shares.
2. **Each node, in pre-order, children in array order:**
   - it is a map; its `type` is one of the five; it has exactly its type's fields;
   - a leaf: refused if the proposition already has 4096 leaves. Then its points are decoded, `g`, `h`, `u`, `v` in
     that order, and only then checked against the identity;
   - a threshold: `k` is an integer that fits 32 bits;
   - a connective: `children` is an array of at most 255 elements. Only then are its children decoded.
3. **Each connective, once its children are decoded:** `1 <= k <= n` for a threshold, then at least one child, then
   depth at most 64.

So the width is checked before a connective's children are decoded, and the 4097th leaf is refused before its points
are. An oversized proposition costs at most 4096 leaves' worth of curve work before it is refused.

**The weight bound.** The canonical form's weights, with `k <= 255` (one node, no limb):

| Node | Weight |
|---|---|
| `dlog` | `1 + (5 + 5) + (3 + 34)` = 48 |
| `dhtuple` | `1 + (5 + 8) + 4·(2 + 34)` = 158 |
| `and` | `1 + (5 + 4) + (9 + 1)` = 20, plus its children |
| `or` | 19, plus its children |
| `threshold` | `1 + (5 + 10) + (2 + 1) + (9 + 1)` = 29, plus its children |

These are a map's own node, then `1 + len(key) + weight(value)` per entry. A group element is 34 bytes of weight (1 +
33), and `children` is an array (1, plus its elements).

A proposition with `L` leaves and `C` connectives therefore weighs at most `158·L + 29·C`.
- `L <= 4096`.
- Count the nodes level by level, the root at level 1. Level 2 has at most 255 nodes. No level has more nodes than
  `L`: every connective has a child, so the nodes of one level have disjoint, non-empty sets of leaves. A connective
  has a child one level down, so connectives sit at levels 1 to 63. Hence `C <= 1 + 255 + 61·4096` = 250,112.
- One tree meets both bounds: a `threshold` with `k = 1` over 255 more, which hold 4096 chains between them (16 hold
  17, the rest 16). Each chain is 61 one-child thresholds over a `dhtuple`, which sits at depth 64.

The maximum is `158·4096 + 29·250,112` = 647,168 + 7,253,248 = **7,900,416**. The tests of both implementations
decode that tree, and refuse a `dlog` that weighs 7,900,417 (a key of 7,900,402 bytes) for its weight, not its key.

The verifier and the prover check every constraint again on the proposition they are given, rather than trust its
type: Scala 3 product mirrors build the proposition types around their smart constructors, so a caller could
otherwise pass `dlog` of the identity, an empty `and` or a threshold with `k = 0`, each provable without a secret. The
check also requires each statement point to be a point of secp256k1. It walks the tree with a worklist and stops at
the first violation, so a tree of any depth is rejected without exhausting the stack.

**The tree as written is the statement.** No normalisation is applied: nested `and`s are not flattened, and
`threshold` with `k = 1` or `k = n` is not rewritten as `or` or `and`. Each shape has its own transcript. Ergo's script
reducer normalises before proving, so an Ergo-reduced tree must be written in its reduced form.

**No trivial nodes.** There are no `true` or `false` propositions. Programs compute the remaining statement themselves
(with `if`) and verify only the sigma part.

## 4. The Fiat-Shamir transcript

The prover and the verifier serialize the tree in pre-order, children in array order:

| Node | Bytes |
|---|---|
| leaf | `01` ‖ `len(s)` ‖ `s` ‖ `len(c)` ‖ `c` |
| `and` | `00 00` ‖ `n` |
| `or` | `00 01` ‖ `n` |
| `threshold` | `00 02` ‖ `k` (1 byte) ‖ `n` |

- Lengths and `n` are 2-byte big-endian.
- `c` is the commitment: `a` (33 bytes), or `a ‖ b` (66 bytes).
- `s` is the leaf's statement: Ergo's ErgoTree for `SigmaPropConstant(leaf)`, with constants segregated.
  - `dlog`: `10 01 08 cd ‖ pk ‖ 73 00`, 39 bytes;
  - `dhtuple`: `10 01 08 ce ‖ g ‖ h ‖ u ‖ v ‖ 73 00`, 138 bytes.

The root challenge is `Blake2b-256(transcript ‖ message)`, truncated to its first 24 bytes.

## 5. Proofs

A proof is the root challenge, followed in pre-order by what the verifier cannot compute:

| Node | Bytes after its own challenge (which its parent may compute instead) |
|---|---|
| leaf | `z` (32 bytes) |
| `and` | each child, with no challenge (each child's challenge is the node's) |
| `or` | each child but the last, each preceded by its 24-byte challenge; then the last child, with no challenge |
| `threshold` | coefficients 1 to `n − k` of `Q`, 24 bytes each, lowest first; then each child, with no challenge |

- In an `or`, the last child's challenge is the XOR of the node's challenge and its siblings'.
- In a `threshold`, `Q` is the polynomial of degree `n − k` over GF(2^192) whose coefficient 0 is the node's challenge.
  Child `i` (from 1) gets the challenge `Q(i)`, with `i` read as a field element.

A proposition fixes its proof's length exactly:
`24 + Σ leaves 32 + Σ or-nodes 24·(n−1) + Σ threshold-nodes 24·(n−k)`.

## 6. Verification

1. Check the proof's length against §5.
2. Read the challenges top-down, computing what §5 says the verifier computes, and read each leaf's `z`; require
   `z < q`.
3. Recompute every leaf's commitment from its challenge and response (§2). A commitment is never taken from the
   proof.
4. Serialize the transcript (§4) and accept if and only if its challenge equals the root challenge.

**Malformed input is an error, never `false`:**
- a proposition breaking §3;
- a proof of the wrong length;
- `z ≥ q`;
- a non-canonical point.

A well-formed proof that fails step 4 is `false`.

**Stricter than Ergo.** Each of these is malformed here:
- an identity statement point, which Ergo accepts (and for which a proof is trivial);
- Ergo's verifier ignores trailing proof bytes (the vectors record that it accepts them);
- it only warns on a short proof;
- it places no bound on `z`;
- empty connectives never reach it, because its script reducer removes them.

## 7. Proving

The prover holds secrets for some leaves and proceeds in these steps:

1. **Mark which nodes are real.**
   - A leaf is real if a secret matches it.
   - An `and` is real if all of its children are; an `or` if one is; a `threshold` if `k` are.
   - Under a real `or`, the first real child stays real; under a real `threshold`, the first `k` do. Every other
     subtree is simulated. If the root is not real, the secrets do not satisfy the proposition, and proving fails.
2. **Simulate.**
   - A simulated child of a real node gets a random challenge.
   - Inside a simulated subtree, challenges are split as the verifier would: by XOR for `or`, by a random `Q` with the
     node's challenge at 0 for `threshold`.
   - A simulated leaf draws a random `z` and derives its commitment by §2.
   - A real leaf draws a nonce `r` and commits.
3. **Hash** the transcript with every commitment into the root challenge.
4. **Push the root challenge down the real nodes.**
   - Through an `or`: by XOR with the simulated siblings.
   - Through a `threshold`: by interpolating `Q` through the root's challenge at 0 and the simulated children's
     challenges.
5. **Answer** at each real leaf, `z = r + e·w mod q`.

Simulated and real transcripts are identically distributed, so a proof does not reveal which branches were real.

**Randomness.**
- Every random value is drawn from
  `Blake2b-256(K ‖ purpose ‖ block ‖ node path)`, where
  `K = Blake2b-256("dilf4s/sigma/prover/v1" ‖ seed ‖ secrets ‖ statement ‖ message)` and `seed` is 32 fresh bytes.
  Scalars take two blocks, reduced into `[1, q)`.
- A nonce therefore never repeats across messages, even under a repeated seed, so the key cannot leak that way.
- Simulated challenges cannot be predicted from public data: they depend on the secrets and the seed.
- `SigmaProver.prove` draws the seed from `SecureRandom`. The fixed-seed entry point is test-only.

## 8. Using proofs

- **Fix the statement in the program.** Build the proposition from the program's literals and committed state, never
  from the spender's arguments alone: whoever chooses the statement can choose one they can prove. Rejecting the
  identity does not make every statement safe. Any statement with a public witness can be proven by anyone, such as
  `dlog(G)` (`w = 1`) or a tuple with `u = g`, and no verifier can tell.
- **Bind the message.** The message must commit to what the proof authorises: the update or transaction, its target,
  and a nonce or ordinal. A proof over a reused message replays.
- **Use one `sigma_verify` for any hiding.**
  - `or` or `some` over separate single-key checks reveals which key signed.
  - A single `sigma_verify` over an `or` (a ring signature) hides which key signed.
  - A single `sigma_verify` over a `threshold` (k-of-n multisig) hides which `k` signed.
- **Privacy needs the ledger too.** ZeroJoin's half-mix condition is `or(dhtuple(g, gX, c1, c2), dhtuple(g, gX, c2,
  c1))`. Its privacy also relies on the outputs:
  - two outputs of equal value;
  - both locked by the same full-mix program;
  - holding `(c1, c2)` and `(c2, c1)`.

  A program can check all of these only when the ledger exposes outputs, with their values and program hashes, as
  data. An account model links balances whatever the proof hides.

## 9. Regenerating the vectors

`tools/sigma-vectors` is a dev-only Scala 2.13 project over sigmastate 5.0.13. It pins the released `scrypto` 2.3.0,
as the Ergo node does, because sigma-state 5.0.13 names an unpublished snapshot. Ergo's proofs are randomised, so
regenerating changes `sigma_v1.json`. The order:

1. In `tools/sigma-vectors`: `sbt "run generate <repo>/shared/vectors/sigma_v1.json"`.
2. At the root: `sbt "crypto/Test/runMain xyz.kd5ujc.crypto.sigma.SigmaProofsGen"`, which writes our proofs to
   `target/sigma_dilf4s_proofs.json`.
3. In `tools/sigma-vectors`: `sbt "run check <repo>/target/sigma_dilf4s_proofs.json <repo>/shared/vectors/sigma_dilf4s_v1.json"`.

