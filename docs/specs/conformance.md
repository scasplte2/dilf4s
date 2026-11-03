# dilf4s conformance: refusals (v1)

Status: normative. Conformance vectors: `shared/vectors/refusals_v1.json` (schema
`shared/schemas/refusal-vectors.cddl`). Every other spec's refusals follow this one.

The Scala implementation is the reference; the Rust and TypeScript SDKs, and every later one, must give the same
outcome for the same call. An outcome is a result (a value, `true`, `false`), or a **refusal**. This spec says what of
a refusal is compared across implementations, and how much work may come before one.

## 1. Kinds

- Every refusal has a **kind**: the name of its case in the reference's error type (`VersionedTrieError.TooLarge` is
  `TooLarge`). A refusal that wraps another names both, outer first: `Trie.Encoding` is a versioned-trie refusal
  carrying a trie `Encoding`.
- Implementations must use the same kinds. A language may spell them its own way (Rust enum variants, TS `kind`
  strings), but each must map one-to-one onto the reference's names.
- A spec that defines a refusal names its kind. A malformed input is a refusal; a well-formed input that proves
  something else is a result (`false`), never a refusal. Where a verifier has both a checking form (which reports why)
  and a boolean form, the spec says which kinds the boolean form answers `false` (§7).

## 2. Free text

Messages, `reason`, `what` and every other text field are **not normative**. They are never compared across
languages, and vectors never record them. An implementation may word them as it likes, but it must not render
unbounded caller input or decimal digits of caller numbers into them (bit lengths are fine).

## 3. Normative fields

A kind's normative fields are part of the outcome: every implementation must report them exactly, and vectors pin
them. Every other field is informative. The normative fields are:

| Family | Kind | Normative fields |
|---|---|---|
| any byte budget | `TooLarge` | `limit`: the limit that was passed: a spec constant, or the caller's own limit as the implementation applies it (a limit past an implementation ceiling is that ceiling) |
| versioned trie | `TooLarge` | `limit`: `MaxValueBytes` (2^20) or `MaxChunkBytes` (2^22) in bytes, or `MaxProofWeight` (2^24) in weight, for a delta or proof the store would produce (§5) |
| versioned trie | `TooDeep` | `depth` (the value's depth) and `limit`: `MaxValueDepth` (253) |
| versioned trie | `RootMismatch` | `component`: `root`, `state`, `history` or `parent` |
| Merkle tree, trie | `TooLarge` | `limit`: `MaxProofWeight` (2^24), for claims (§5) or a trie proof a producer would emit |
| Poseidon tree | `Position` | `depth`: the number of siblings (or the tree's depth); the position itself echoes the caller's input and is informative |
| every other kind | | none: the kind alone |

A field is normative only when every implementation can produce it exactly at no more cost than the refusal itself:
a spec constant, a cached property of the input, or an enumerated name. A measured size never is (§4).

## 4. Bounded work before a refusal

A refusal costs no more than the spec says, whatever the input:

- **Byte budgets.** A value over a byte limit (an encoding, a commitment, a JSON rendering, a versioned value or
  snapshot chunk) is refused with kind `TooLarge` and its `limit`: never a measured size or a lower bound on one. A
  value whose weight (`jlvm.md` §2) is over the limit is refused in O(1), since its encoding is never shorter than its
  weight. Otherwise its length is measured, never by building it, and measuring stops at the limit: O(min(weight,
  limit)). An implementation may keep counters internally, but they are not part of the refusal.
- **Depth.** A value deeper than a limit is refused from its cached depth, in O(1).
- **Implementation ceilings.** An SDK whose runtime cannot hold an encoding of any length budgets its unbudgeted forms
  at a ceiling of its own and refuses past it as `TooLarge`, never with an exception: the TypeScript SDK encodes,
  commits and content-hashes within 2^31 - 1 bytes (`cbor.MAX_ENCODED_BYTES`, the largest array the JVM reference can
  build), and renders JSON only within a limit the caller names (at most 2^29 - 24 bytes, its string ceiling). Its
  public leaf and value hashing refuse a commitment past that ceiling as `TooLarge` with the ceiling as its limit. Values
  that large are far past every limit these specs set (§5 caps every claim at 2^24), so no vector depends on a ceiling.
- **Verifier inputs** are bounded by weight before they are decoded (§5).

## 5. Verifier input weight

**`MaxProofWeight` = 2^24** (16,777,216): four times the largest snapshot chunk, so range proofs over large listings
fit, while the worst case stays bounded.

A value with shared parts is small in memory and can weigh up to 2^63 − 1, and decoding walks it as written out
(copying each byte string every time it occurs). So every verifier entry point that takes a caller's canonical value
as its proof input refuses one whose weight is over `MaxProofWeight`, from the cached weight, in O(1), **before**
decoding it. The refusal is the family's malformed-input kind, never `false`:

| Input | Refusal |
|---|---|
| `merkle-inclusion`, `merkle-consistency` proofs (`merkle.md` §4) | `Encoding` |
| `trie-proof` (`trie.md` §5), and so trie lookups, prefix and range queries | `Encoding` |
| `poseidon-merkle-proof` (`zk.md` §4) | `Encoding` |
| versioned-trie proofs and deltas (`versioned-trie.md` §5), and heads decoded from values | `Malformed` |

What a verifier refuses, a producer never makes: the versioned-trie store refuses a commit whose delta would weigh over
`MaxProofWeight`, before anything is written, and never encodes a proof over it (`TooLarge` with that limit). Without
that, an accepted commit could serve a delta no replica decodes.

Snapshot chunks arrive as bytes, bounded by `MaxChunkBytes`, and a value is never heavier than its encoding is long,
so they need no weight check. The JLVM's proof operations surface the refusal as `Crypto` (`jlvm.md` §6).

**Claims are capped too: one rule for every caller value a verifier takes.** The claims a proof is checked against (a
Merkle leaf, a trie value, the values of a claimed listing) are hashed by their commitment, which costs O(their
weight). Together they must weigh at most `MaxProofWeight`: heavier is `TooLarge` with that `limit` (`Crypto` in the
JLVM), refused from their cached weights before any is hashed and before the proof is decoded. A listing's values are
summed, so many entries sharing one heavy value are refused as a whole. In the boolean forms the arguments that say what is
asked are checked first (a Merkle tree head, a trie mode, a key's validity, an entry's shape), then the claims, then a
trie's root and the proof, each step only once the last has passed, so a refused claim costs no decoding.

**Decoding order.** A decoder refuses a record with several faults for the first in a fixed order, the same in every
implementation: a Merkle proof's path digests before its index and sizes; a trie proof's records in order, its
structure before its root (`InvalidProof` before `RootMismatch`); a Merkle path's length before its root; a Poseidon
proof's siblings before its position; the versioned trie's records in the order `versioned-trie.md` §5 gives. The
refusal vectors pin each pair of faults that would otherwise answer differently. A Merkle leaf given as raw bytes (RFC 6962) weighs one
more than its length. The versioned trie's `holds` already answers `false` in O(1) for a claim no commit accepts (over
1 MiB), which is stricter.

What a verifier refuses, no producer emits. A trie proof heavier than `MaxProofWeight` is `TooLarge` where it would be
emitted: `Trie.prove` and `TrieProofs.encode` in Scala, `TrieProof::to_value` in Rust and `TrieProof.encode` in
TypeScript. The versioned trie's store does the same for its proofs and deltas (above).

## 6. Vectors

`shared/vectors/refusals_v1.json` holds calls and their outcomes: `"accepted"`, or `{kind, …normative fields}`. Every
refusal kind a spec defines must have a row. This version gates the verifier families: the Merkle tree, the trie, the
Poseidon tree and the versioned trie. Each of their kinds has a row, in that file or in a family file's `malformed`
section (`merkle_v1.json`, `trie_v1.json`), except the kinds below, which no verifier can return and no SDK implements.
The other families (signing, VRF, KES, sigma, the zk operations, the canonical codecs, the JLVM's own errors) record
some outcomes in their own vector files but are not yet gated kind by kind; they are next.

| Family | Kind | Why it has no row |
|---|---|---|
| versioned trie | `Empty`, `VersionConflict`, `CommitRewrite`, `NotRetained`, `Store` | the store's own refusals: committing and serving, not verifying |
| trie | `MissingNode`, `InvalidNode` | a store read that finds no node, or an invalid one |
| Poseidon tree | `Depth`, `OccupiedPosition` | building a tree, not decoding or verifying a proof |

The reference's `RefusalCoverageSuite` fails if a kind of these families has neither a row nor an entry here.

**Inputs.** A heavy input is a **built value** (`built-value`): a leaf (canonical CBOR) and steps, each a template
(every text `"$"` in it replaced by the value so far, `times` times over) or `copies` (an array of that many of it).
Every occurrence is the one value, so a file of a few kilobytes describes values of any weight; canonical CBOR has no
sharing, so it cannot. A loader builds them with sharing and never encodes, renders or copies one whole.

**Checks.** Each case names a check, the entry point and the order its steps run in; the first refusal is the
outcome, and `"accepted"` means every step succeeded.

| Check | Arguments | Steps |
|---|---|---|
| `vt-value` | `stateKey`, `input` | `checkValue` (versioned-trie.md §4) |
| `vt-chunk` | `zeros` | decode a snapshot chunk of that many zero bytes |
| `vt-delta` | `input` | decode a delta |
| `vt-proof` | `input` | decode a proof |
| `vt-state` | `root`, `input`, `stateKey` | decode the proof, verify the current state against `root`, look up the key |
| `vt-historical` | `root`, `input`, `at`, `stateKey` | decode the proof, verify the state at version `at`, look up the key |
| `vt-replica` | `inputs` | from an empty replica, decode and apply each delta in order |
| `merkle-inclusion` | `root`, `leafHash`, `input` | decode the proof, check it (SHA-256, tagged profile) |
| `merkle-consistency` | `oldRoot`, `root`, `input` | decode the proof, check it |
| `trie-lookup` | `mode`, `root`, `key`, `input` | decode the proof, check it against `root` under `mode`, the key's path, look the key up |
| `poseidon-proof` | `input` | decode the proof |
| `merkle-verify` | `root`, `size`, `claim`, `input` | `merkle_verify`: the head (the root's length), the leaf claim's weight, decode the proof, the size, check it (tagged profile) |
| `trie-verify` | `mode`, `root`, `key`, `claim` (absent: absence), `input` | `trie_verify`: the key's path, the claim's weight, the root's length, decode the proof, check it against `root`, look the key up |
| `vt-commit` | `batch` | commit the batch at version 1 to an empty store: each value's `checkValue`, then the weight of the delta it would serve (no parent; the head's version key and both roots at their widths) against `MaxProofWeight`. An SDK without a store checks the same, building that delta |
| `trie-prefix-verify` | `mode`, `root`, `prefix`, `entries`, `input` | `trie_prefix_verify`: each entry's key and value, the values' weight together, the root's length, decode the proof, check it, list the prefix |

Versioned-trie checks use the default version key (8 bytes) and SHA-256.

## 7. Boolean forms

The checking forms report every kind; the boolean forms (and the JLVM operations, `jlvm.md` §6) answer `false` for a
well-formed proof of something else:

- Merkle: `InvalidProof` and `RootMismatch` are `false`, and so is a proof whose size differs from the trusted head's.
- Trie: `RootMismatch` is `false`. `NotCovered` stays a refusal: it proves neither presence nor absence.
- Versioned trie: `holds` is `false` for a claim no commit accepts (too large or too deep), without hashing it.

Every other kind is a refusal in both forms.
