# dilf4s conformance: refusals (v1)

Status: normative. Conformance vectors: `shared/vectors/refusals_v1.json` (schema
`shared/schemas/refusal-vectors.cddl`) and, for previews (§8), `shared/vectors/diagnostics_v1.json` (schema
`shared/schemas/diagnostic-vectors.cddl`). Every other spec's refusals follow this one.

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
unbounded caller input or decimal digits of caller numbers into them (bit lengths are fine): caller text appears only
as a preview (§8).

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
| Poseidon tree | `Position` | `depth`: the number of siblings (or the tree's depth); the position, held as its bit length (§8), is informative |
| canonical value | `RationalTooWide` | `limit`: `MaxRationalBits` (33219), for a rational part wider than it, from any producer (`canonical-encoding.md` §1); the part's width is not reported |
| canonical value | `IntegerTooWide` | `limit`: `MaxIntegerBits` (33219), for an integer wider than it, from any producer (`canonical-encoding.md` §1); the integer's width is not reported |
| canonical encoding | `DuplicateKey` | `key`: the repeated key's preview, its `head` and `length` (§8) |
| state key | `InvalidSegment`, `SegmentTooLong` | `segment`: the first refused segment's position, from 0 (`versioned-trie.md` §2) |
| JLVM | `Type`, `Arity`, `Arithmetic`, `Domain`, `Crypto` | `op`: the operation's wire name, or `var` (`jlvm.md` §3). `OutOfGas` has none: its `required` may be summed only as far as the limit, and the gas used (the limit) is pinned |
| hash | `InvalidHex` | `offset`: the position of the first character that is not an ASCII hex digit, or the text's length when its digits are odd in number (`canonical-encoding.md` §4); every character before it is ASCII, so it is the same in UTF-8 bytes, UTF-16 units and code points |
| hash | `InvalidExpansion` | `refusal`: `emptyDst` or `outputLength`, checked in that order (`canonical-encoding.md` §4) |
| every other kind | | none: the kind alone |

A field is normative only when every implementation can produce it exactly at no more cost than the refusal itself:
a spec constant, a cached property of the input, or an enumerated name. A measured size never is (§4), but for a
preview's length (§8), measured over text the refusal has already read whole.

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
- **Listing checks.** `trie_prefix_verify` and `trie_range_verify` do work linear in the proof's weight plus the
  lengths of the prefix or bounds and of the claimed keys (`trie.md` §4). They never rebuild each key from the top, never
  compare it with a bound from bit 0, and never build the listing to compare it. So their answer and their refusal
  cost no more than the weight their operands are charged by.
- **Rationals.** Decoding a CBOR rational (`canonical-encoding.md` §2) runs at most one gcd, and only of a numerator and
  denominator each at most `MaxRationalBits` (33219) wide: a wider part is refused as `RationalTooWide` as soon as it
  is read, before the other part is read and before any gcd. A gcd is superlinear in its operands; with the bound, the
  work of decoding is linear in the input (at most one gcd of bounded operands per 8 KiB of rational).
- **Integers.** A CBOR bignum (`canonical-encoding.md` §2) wider than `MaxIntegerBits` (33219) is refused as
  `IntegerTooWide` from its length and first byte, before its integer is built; a rational's part, as
  `RationalTooWide`, the same way. So no decoder builds an integer, or a rational part, wider than 33219 bits, and
  nothing that reads a value later (rendering, arithmetic, typed decoding) works on a wider one. A JSON number is
  checked once it is read: its digit and scale bounds already bound that work.

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
trie's root and the proof, each step only once the last has passed, so a refused claim costs no decoding. A Merkle
leaf given as raw bytes (RFC 6962) weighs one more than its length. The versioned trie's `holds` already answers
`false` in O(1) for a claim no commit accepts (over 1 MiB), which is stricter.

**Decoding order.** A decoder refuses a record with several faults for the first in a fixed order, the same in every
implementation. It runs in two stages: first the record's **shape**, as its schema in `shared/schemas` states it, is
checked whole (any shape fault is the family's malformed kind, `Encoding` or `Malformed`, before any field's meaning is
read); then each field's **meaning**, in this order: a Merkle proof's path digests before its index and sizes; a trie
proof's records in order, its structure before its root (`InvalidProof` before `RootMismatch`); a Merkle path's length
before its root; a Poseidon proof's siblings before its position; the versioned trie's records in the order
`versioned-trie.md` §5 gives. Within canonical CBOR, a bignum's faults come in the order `canonical-encoding.md` §2
gives (the wrapped item's head, that it is a byte string, present in full, in canonical form, then its width), and a rational's faults in the order it
gives: its pair, its numerator then the numerator's width, its denominator then the denominator's width, the
denominator's range, and lowest terms last (so a part too wide is `RationalTooWide` even when the denominator is below 2
or the rational is not in lowest terms; `canonical_encoding_v1.json`'s `cborRejections` pin each pair). The refusal
vectors pin each pair of faults that would otherwise answer differently, shape against meaning as well as meaning
against meaning.

**Codec order.** Canonical CBOR and canonical JSON are refused in the orders `canonical-encoding.md` §2 and §3 state:
one pass in reading order, the first fault met is the refusal, a container that would pass the depth limit is refused
at its head or bracket (an empty one too), and the one deferred check is a JSON member name's unpaired surrogate, at
its object's close. A state key is refused in the order `versioned-trie.md` §2 gives. `canonical_encoding_v1.json` and
`versioned_trie_v1.json` pin each kind and each pair of faults an order could otherwise answer differently. A nested
refusal in `refusals_v1.json` (`Key.SegmentTooLong`, `Trie.Encoding`) records both kinds; its inner kind's normative
fields are pinned by that family's own rows.

What a verifier refuses, no producer emits. A trie proof heavier than `MaxProofWeight` is `TooLarge` where it would be
emitted: `Trie.prove` and `TrieProofs.encode` in Scala, `TrieProof::to_value` in Rust and `TrieProof.encode` in
TypeScript. The versioned trie's store does the same for its proofs and deltas (above).

## 6. Vectors

`shared/vectors/refusals_v1.json` holds calls and their outcomes: `"accepted"`, or `{kind, …normative fields}`. Every
refusal kind a spec defines must have a row. The gated families, and where their rows are:

| Family | Error type | Rows |
|---|---|---|
| Merkle tree, trie, Poseidon tree, versioned trie | `MerkleError`, `TrieError`, `PoseidonMerkleError`, `VersionedTrieError` | `refusals_v1.json`; the `malformed` sections of `merkle_v1.json` and `trie_v1.json` |
| signatures and their keys | `CryptoError` | `signing_v1.json`: `keyDecoding` (the kind of each key decoding) and `verification` (the part refused, `verifyingKey` or `signature`, and its kind) |
| key derivation | `KeyError`, `PathRefusal` | `signing_v1.json`: `keyRefusals` (BIP-39 entropy and phrases, word lists, SLIP-10, BIP32 and public derivation) and `paths` (`InvalidPath`, by refusal) |
| VRF | `VrfError` | `vrf_v1.json`: `rejections` (verification) and `keyDecoding` (signing keys) |
| KES | `KesError` | `kes_v1.json`: `refusals` (parameters, generation, loading, evolution, verification) |
| sigma | `SigmaError` | `sigma_refusals_v1.json`: `cases`, and the reference's outcome on every case of `sigma_v1.json` and `sigma_dilf4s_v1.json` (whose own verdicts are Ergo's) |
| zk | `ZkError`, `Groth16Error` | `zk_v1.json` |
| canonical codecs | `CanonicalError` | `canonical_encoding_v1.json`: `json`, `renderRejections` and `cborRejections` (each kind with its normative `limit`); `TypeMismatch`, a typed encoder's refusal, by the `schema` outcome of `versioned_trie_v1.json`'s `stateKeys` |
| state keys | `StateKeyError` | `versioned_trie_v1.json`: `stateKeys`, `stateKeyBytes` and `namespaces` (each kind with its normative `segment`); `refusals_v1.json`'s `Key.` kinds |
| JLVM | `JlvmError` and its `Site` | `jlvm_v1.json`: each failing case's `outcome` (`error`, and `op` where the kind has one) |
| hashing | `HashError` | `canonical_encoding_v1.json`: `domainTags`, `algorithms` (names), `digestHex` (digest lengths and hex, `InvalidHex` with its normative `offset`) and `expansions` (`expand_message_xmd`, `InvalidExpansion` with its normative `refusal`) |

Each kind of these families has a row, except the kinds below, which no input reaches through an SDK's verifier.

| Family | Kind | Why it has no row |
|---|---|---|
| versioned trie | `Empty`, `VersionConflict`, `CommitRewrite`, `NotRetained`, `Store` | the store's own refusals: committing and serving, not verifying |
| trie | `MissingNode`, `InvalidNode` | a store read that finds no node, or an invalid one |
| Poseidon tree | `Depth`, `OccupiedPosition` | building a tree, not decoding or verifying a proof |
| signatures | `BackendFailure` | a JVM provider's own failure; no SDK has one |
| VRF | `HashToCurveFailed` | no input is known that reaches it (TAI: all 256 counters fail, probability about 2^-256) |

**The gate, in every language.** The reference's `RefusalCoverageSuite` (the verifier families) and
`CryptoRefusalCoverageSuite` (the others) list each error type's cases through the compiler (`Mirror.SumOf`; the path
refusals, a choice of texts, through their generated `values`) and fail if a kind has neither a row nor an entry here.
The crypto families' error types are generated in every language (below), so the SDK's kinds are the schema's: the
Rust tests check that every kind of the generated kind enum (`ALL`) has a row, but the exempt ones, which have none;
the TypeScript tests list each error's kinds in a `Record` keyed by its `kind` union, so a kind added to the schema does
not compile until it is listed, and check its row. The SP1 guest harness runs every verification call of these files in
the guest and checks each refusal by its exact kind: signatures, VRF verification, KES verification and parameters,
sigma (its refusal calls, and every case of the Ergo-checked files) and zk. The guest builds the signing SDK without
its `sign` feature, so key derivation (`KeyError`), VRF signing-key decoding (`KeyMismatch`) and KES generation,
loading and evolution (`Exhausted`, `InconsistentKey`) are checked natively only.

**Generated error types.** `shared/schemas/crypto-errors.cddl` defines each crypto family once: `x-error` is a choice
of maps, each led by a distinct `"kind"` text literal (the reference's case name) and holding that kind's fields, and
`x-error-kind` lists the same kinds in order (`crypto-error`, `key-error`, `vrf-error`, `kes-error`, `sigma-error`,
`zk-error`, `groth16-error`, and the path refusals `path-refusal`). The generator checks the two agree and emits the
family as each language's error type, and the kinds as a closed type:

- Scala: a sealed trait with a case class per kind (a case object without fields), each with its `kind`, exported
  under the family's package (`xyz.kd5ujc.crypto.kes.KesError`);
- Rust: an enum with a variant per kind, `kind()` returning the kind enum, `Display` and `core::error::Error`
  (`dilf4s_core::generated::crypto_errors`, re-exported by `dilf4s-signing` and `dilf4s-proofs`);
- TypeScript: a union of readonly object types on `kind` (`models.cryptoErrors`, re-exported by the signing package).

Every SDK names a family as the reference does (`CryptoError`, not `SigningError`), and a kind added to the schema is a
new case in every language. An error is a value, not a wire format: no codec is generated for a family, and no wire
type may hold one. **Fields** are informative (§2, §3) unless §3 names them: a field holds text (`what`, `reason`,
which the implementation writes, naming caller input only by size or position, or by a preview, §8), an integer (a size,
offset or period, `uint .le 2^63 - 1`: Scala `Long`, Rust `u64`, TypeScript `number`, the type of the SDK's own sizes
and periods), a boolean, a kind, or a **preview**; the generator refuses bytes, collections and records in an error. A
field naming the rule `preview` (`{ "head": text, "length": uint }`, the preview's normative fields, §8) holds the
SDK's own `Preview` (Scala `xyz.kd5ujc.core.canonical.Preview`, Rust `dilf4s_core::Preview`, TypeScript `Preview`):
caller text in an error is typed as a preview, never as text. The rule itself is not generated, and no wire type may
hold a preview. No crypto kind has a normative field, and no crypto `what` or `reason` holds caller text: the one caller
text a crypto family names, a BIP-39 phrase separator parsing does not split on, is `InvalidWordList`'s `separator`, a
preview.

**The hash family.** `shared/schemas/hash-errors.cddl` defines `hash-error` the same way, in every language the hash
API's one error type (Scala `xyz.kd5ujc.core.hash.HashError`, Rust `dilf4s_core::hash::HashError`, TypeScript
`HashError`; a TypeScript function refusing only some kinds returns `HashErrorOf<K>`, its members of those kinds):

| Kind | Fields | Raised by |
|---|---|---|
| `InvalidLength` | `algorithm`, `expected` (the output size), `actual` | a digest from bytes or hex of another length |
| `InvalidHex` | `offset` (normative, §3) | hex decoding, and a digest from hex |
| `InvalidDomainTag` | `tag`, a preview | a domain tag outside the grammar |
| `InvalidExpansion` | `refusal` (normative, §3), `algorithm`, `limit` (the most bytes it expands to) | `expand_message_xmd` |
| `UnknownAlgorithm` | `name`, a preview | a hash function by name |

For a digest's hex longer than its width, `actual` is half the text's length after the prefix, rounded up, in the
language's own string units (UTF-16 in Scala and TypeScript, UTF-8 in Rust): the text is refused before it is read
whole, so its length is not measured in other units; the outcome and the kind are the same everywhere.

**Kinds in the schemas.** Every kind field of these files names its family's kind choice from `crypto-errors.cddl`,
beside its results (`kes-outcome = "accepted" / "rejected" / errors.kes-error-kind`): a choice that names a choice of
texts takes its texts, and is typed by it (Scala and TypeScript a union including the kind type, Rust a variant holding
the kind enum). So each SDK's model of a vector is the error's own kind type, and a vector naming a kind the schema does
not list does not load. A section that only one check fills may name any kind of the family; the gate asks for each
kind's row in whichever section reaches it.

**The hash gate.** The hash rows of `canonical_encoding_v1.json` record `"accepted"` or a kind of
`hash-error-kind` (`hash-outcome`), with `offset` exactly for `InvalidHex` and `refusal` exactly for
`InvalidExpansion`. The reference's `HashRefusalCoverageSuite` lists `HashError`'s cases through the compiler and fails
if one has no row; the Rust (`core/tests/conformance.rs`) and TypeScript (`core/test/conformance.test.ts`) tests check
every row's kind and normative field, and that every kind of `HashErrorKind` (Rust `ALL`, TypeScript a `Record` on the
kind) and every expansion refusal has a row. Every kind is reachable from shared input in every language: each SDK has
the five operations (a digest from hex, a domain tag, a hash function by name, `expand_message_xmd`, and hex).

**The codec families.** `canonical-error-kind` lists every kind of `CanonicalError`; the state keys' `schema` records
`TypeMismatch` (`vt-schema-outcome`). The reference's `CodecRefusalCoverageSuite` gates `CanonicalError` as above,
reading the rows through their generated models; the Rust (`core/tests/conformance.rs`) and TypeScript
(`core/test/conformance.test.ts`) tests map the SDK's error onto `canonical-error-kind` exhaustively, check each row's
kind and `limit`, and check that every kind has a row. `state-key-error-kind` lists every kind of `StateKeyError`: the
reference's `RefusalCoverageSuite` gates it with the verifier families, and the Rust (`proofs/tests/versioned.rs`)
and TypeScript (`proofs/test/versioned.test.ts`) tests do the same with its `segment`.

**The JLVM's gate.** The reference's `JlvmErrorCoverageSuite` lists `JlvmError`'s cases and `Site`'s (an operation,
or `var`) through the compiler and fails if one has no row, or if a row's `op` names no site. The Rust tests map
`JlvmError` onto `jlvm-error-kind` with an exhaustive `match`, check every kind of the generated `ALL` has a row, and
compare every case's kind and `op`; an `op` is present exactly for the kinds that carry one. The TypeScript SDK has no
JLVM evaluator, so its test checks the schema's side only: every kind, listed in a `Record` keyed by the generated
union, has a row, and `op` is present exactly where it should be. The SP1 guest runs, besides its sample, the first
case of each kind and of the `var` site, and compares kind and `op`. `jlvm-error-kind` is a closed choice of texts,
so a vector naming another kind does not load; the `op` is text, since most operation names (`+`, `==`) have no
identifier form for the generated types, and these gates check that each names a site. A JLVM `Crypto` error does
not yet name the refusal it wraps (§1, `jlvm.md` §3).

**Limits.** Every limit the specs state (`MaxValueBytes`, `MaxChunkBytes`, `MaxProofWeight`, the depth, digit, rational,
key, chunk, sigma, KES and JLVM limits) is defined once, in `shared/schemas/limits.cddl`, and generated into each SDK
(`xyz.kd5ujc.models.limits`, `dilf4s_core::generated::limits`, `models.limits`); every implementation reads it from
there. A limit the specs derive from others is stated there as its spec states it, and the reference's `LimitsSuite`
checks the derivation.

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
| `vt-commit` | `batch` | commit the batch at version 1 to an empty store: what each value's depth and weight alone refuse, the weight of the delta it would serve (no parent; the head's version key and both roots at their widths) against `MaxProofWeight`, then each value's `checkValue`. An SDK without a store checks the same, building that delta |
| `trie-prefix-verify` | `mode`, `root`, `prefix`, `entries`, `input` | `trie_prefix_verify`: each entry's key and value, the values' weight together, the root's length, decode the proof, check it, list the prefix |

Versioned-trie checks use the default version key (8 bytes) and SHA-256.

## 7. Boolean forms

The checking forms report every kind; the boolean forms (and the JLVM operations, `jlvm.md` §6) answer `false` for a
well-formed proof of something else:

- Merkle: `InvalidProof` and `RootMismatch` are `false`, and so is a proof whose size differs from the trusted head's.
- Trie: `RootMismatch` is `false`. `NotCovered` stays a refusal: it proves neither presence nor absence.
- Versioned trie: `holds` is `false` for a claim no commit accepts (too large or too deep), without hashing it.

Every other kind is a refusal in both forms.

## 8. Bounded diagnostics

An error built from caller input holds that input as **sizes** (lengths, counts, bit lengths, offsets) or as a
**preview**, never the input itself. What an error holds, and what its message, `toString`, `Display` or `Debug`
renders, is then bounded by constants, however long the input was.

**The preview.** The preview of a text has two normative fields:

- `head`: its first **32 code points**, cut on a code-point boundary, each unpaired surrogate replaced by U+FFFD;
- `length`: the whole text's length in **UTF-8 bytes**, an unpaired surrogate counting 3 (as its replacement does).

It is truncated when the head is shorter than the text (`utf8(head) < length`). Only JSON `\u` escapes produce unpaired
surrogates; they reach a preview through a repeated member name (below), since a repeat is found before the name is
checked. The rendering is informative; every SDK writes the head quoted, with JSON's escapes for `"`, `\` and the
control characters U+0000 to U+001F and U+007F, then `…` when truncated, then the length: `"abc" (UTF-8 length 3)`. A
path segment renders as the escaped head and `…`, unquoted.

The preview type admits no other construction: Scala's `Preview` is a sealed abstract case class built only by
`Preview.of`, Rust's has private fields and `Preview::of`, TypeScript's a private constructor and `Preview.of`. Its head
is a fresh copy, never a view of the caller's text (an engine may keep a slice's whole parent alive).

**Where caller text appears.**

| Error | Field | Holds |
|---|---|---|
| canonical `DuplicateKey` | `key` | the repeated key, previewed |
| canonical `InvalidNumber` | `length`, `offset` (Scala: a `NumberSource`), and a bound's `scale` saturated at the 32-bit range (Rust: `i128`) | the number's size and place, never its digits |
| canonical `UnpairedSurrogate` | none | the kind alone |
| `DecodeFailure` | `path` | each segment (a map key, possibly the caller's, or `[i]`), previewed |
| `DecodeFailure` | `reason` | unknown keys as their count and the first in canonical order (shorter UTF-8 first, then by code point), previewed; a map no alternative fits as its key count; a text that is not the literal, pattern or alternative expected, previewed |
| hash `InvalidDomainTag`, `UnknownAlgorithm` | `tag`, `name` | previewed: the generated field is the SDK's `Preview` (§6) |
| hash `InvalidHex`, `InvalidLength` | `offset`, `actual` | a position and a size, never the text |
| key `InvalidWordList` | `separator` | a phrase separator parsing does not split on, previewed; a separator of more than one character by its length alone, in `reason` |
| versioned trie `StateKeyError` | `key`, `namespace` | the key or namespace, previewed; none for `Empty`, for `TooLong` (refused by its length before it is read) or for `NotUtf8` |
| JLVM `Malformed` | message | operation names, var paths and binding names, previewed |
| JLVM warning `AmbiguousPath` | `path` | the path, previewed: long paths sharing a preview are one warning |
| JLVM `Arithmetic` (`cat`, `join`) | message | a rational with no decimal form by its numerator and denominator bit lengths |
| Poseidon `NonCanonical`, `Position`, `OccupiedPosition` | `bits`, `positionBits` (Rust: `position_bits`; its `NonCanonical` has no field) | the value's or position's bit length, never the number |
| versioned-trie messages (`NotCovered`, `TooLarge`, `TooDeep`, `Malformed`) | `what`, `reason` | a state key or namespace previewed, in its path-segment form |
| Merkle `IndexOutOfRange`, `InvalidSizes` | `index`, `size`, `oldSize`, `newSize` | 64-bit, as the reference's `Long`: TypeScript saturates a wider caller value |
| trie and versioned-trie `RootMismatch` | `expected` | the trusted root's hex at the digest's width; any other width by its length |
| storage `NotFound` (Scala) | `keyBytes`, `prefix` | the key's length, and the hex of at most its first 16 bytes, previewed |
| signing paths (`signing.md` §3) | message | a segment by its position and, past the digit bound, its length |

**Refused before converting.** An input over a length limit is refused by its length before it is split, decoded as
text or hex-encoded: a state key over 256 characters before it is split into segments, trie-key bytes over 256 before
they are read as UTF-8 or ASCII, a storage namespace over 255 characters and a word-list separator of more than one
before they are encoded, a digest's hex with more digits than its width (after an optional `0x`) before it is decoded,
reading at most one digit past the width (a character that is not a hex digit within that reach is `InvalidHex` at its
offset, so the outcome does not depend on how a language measures the rest of the text), a
word list of other than 2048 words before any word is normalised, and an integer wider than a field element before
its bytes are taken. Such a refusal holds the length alone, never a preview, since measuring a preview's
length reads the whole text. A state-key refusal names a segment by its position, never its text.

**Fixed-width keys.** Version keys and top-trie slots have the width the verifier trusts (at most 1024 bytes,
`versioned-trie.md` §3), checked before they are read, so a message may name one in hex: its size is the verifier's
configuration, not the caller's choice.

**Exemptions.** Three things are outside the guarantee, each for a stated reason.

- *Free text and raw-string fields.* Messages are not normative (§2). Generated decoders pass their own messages as
  text, so `DecodeFailure.reason`, `TypeMismatch.reason` and the other free-text fields, and raw-string fields such as
  a `RootMismatch`'s `expected` (built at its call sites by `rootText`, the digest's hex or a wrong width's length),
  stay strings: the type guarantee covers the fields in the table above, and a message quotes caller text only by
  rendering one of them, which every SDK's diagnostics tests check on 1 MiB inputs through the public decoding APIs. A
  codec's expected literal is a schema constant, quoted as it is. The crypto and hash families' error types are
  generated from `crypto-errors.cddl` and `hash-errors.cddl` (§6), which admit text, integers, booleans, kinds and
  previews as fields and never bytes or collections; caller text is a `preview` field, and `what` and `reason` stay
  informative text, written by the implementation from sizes, positions and previews.
  The codec families' free text closes when their error types are generated too (conformance step 2, part B).
- *Bit lengths in TypeScript.* A refused integer is named by its bit length, never its digits. JavaScript has no
  constant-time bit length of a `bigint`, so the TypeScript SDK measures one in a single linear pass over the integer
  (a transient hexadecimal rendering) that the caller already supplied and holds; nothing of it is retained, and the
  cost is linear, never superlinear. Every SDK reports the same exact bit length: a TypeScript-only saturated value
  would make the three disagree.
- *Operator configuration and library text.* A storage backend's path and the messages its native library raises
  (RocksDB's) come from the operator and the library, not from a caller, and are kept as they are, so an operator
  can diagnose them.

**The preview table.** Each row is a JSON member name (as its escaped text) repeated in an object, `{"K":1,"K":2}`:
every implementation refuses it as `DuplicateKey` with this preview. The head is given as escaped text too.

| Name | Head | Length | Truncated |
|---|---|---|---|
| (empty) | (empty) | 0 | no |
| `abc` | `abc` | 3 | no |
| `a` × 32 | `a` × 32 | 32 | no |
| `a` × 33 | `a` × 32 | 33 | yes |
| `a` × 31, `\ud83d\ude00` | the same | 35 | no |
| `a` × 32, `\ud83d\ude00` | `a` × 32 | 36 | yes |
| `a` × 31, `\ud83d\ude00b` | `a` × 31, `\ud83d\ude00` | 36 | yes |
| `\u00e9` × 40 | `\u00e9` × 32 | 80 | yes |
| `\ud800x` | `\ufffdx` | 4 | no |
| `\udc00\ud800` | `\ufffd\ufffd` | 6 | no |
| `a` × 31, `\ud800zz` | `a` × 31, `\ufffd` | 36 | yes |

The reference checks it in `DiagnosticsSuite`, the Rust SDK in `core/tests/diagnostics.rs` and the TypeScript SDK in
`core/test/diagnostics.test.ts`, row for row, from `shared/vectors/diagnostics_v1.json` (schema
`shared/schemas/diagnostic-vectors.cddl`): each case is the JSON text to parse and the preview it is refused with.
