# Schemas

These are CDDL (RFC 8610) definitions, written in the dilf4s canonical profile (see
`models-gen/.../Profile.scala`). They are the single source of truth for shared byte and record formats. The build
generates Scala types and codecs from them into `core` (`xyz.kd5ujc.models.<module>`). The same generator writes the SDK
models, which are committed:

- TypeScript: `sdk/ts/packages/core/src/generated/` (`TsEmitter`);
- Rust: `sdk/rust/core/src/generated/` (`RustEmitter`).

After changing a schema, regenerate them with
`sbt "modelsGen/runMain xyz.kd5ujc.modelsgen.SdkMain shared/schemas sdk/ts/packages/core/src/generated sdk/rust/core/src/generated"`.
`SdkSourcesSuite` fails while the committed SDK models are stale.

| Schema | Defines | Normative text |
|---|---|---|
| `merkle.cddl` | Merkle inclusion and consistency proofs (wire) | `docs/specs/merkle.md` §4 (embedded) |
| `trie.cddl` | Patricia trie proofs (wire shape) | `docs/specs/trie.md` §5 (embedded) |
| `poseidon.cddl` | Poseidon Merkle proofs (wire) | `docs/specs/zk.md` §4 (embedded) |
| `merkle.cddl` (`merkle-log-state`) | the persistent Merkle log's state record | `docs/specs/merkle.md` §5 |
| `storage.cddl` | versioned-store history records | `docs/specs/storage.md` (embedded) |
| `bloom.cddl` | Bloom filter encoding | `docs/specs/bloom.md` (embedded) |
| `crypto.cddl` | sizes of keys, signatures, VRF/KES/zk proofs | `docs/specs/signing.md` §4 (embedded) |
| `versioned-trie.cddl` | versioned-trie heads, proofs and deltas (wire) | `docs/specs/versioned-trie.md` §5 (embedded) |
| `limits.cddl` | every spec limit, as constants (see "constants" below) | each limit's own spec; `docs/specs/conformance.md` §6 |
| `crypto-errors.cddl` | the crypto error families and their kinds (see "error families" below) | `docs/specs/conformance.md` §6 |
| `hash-errors.cddl` | the hash error family and its kinds, and the `preview` rule | `docs/specs/conformance.md` §6, `docs/specs/canonical-encoding.md` §4 |
| `codec-errors.cddl` | the canonical codecs' and state keys' error families (`CanonicalError`, `StateKeyError`) and their reasons | `docs/specs/conformance.md` §6, `docs/specs/canonical-encoding.md` |
| `jlvm.cddl` (`jlvm-op`) | the JLVM's operations, a closed list of named texts | `docs/specs/jlvm.md` §3 |
| `proofs-errors.cddl` | the proof structures' error families (`MerkleError`, `TrieError`, `VersionedTrieError`, `PoseidonError`, `PoseidonMerkleError`), `version-key-hex` and `vt-root-component` | `docs/specs/conformance.md` §3, §6 and §8, `docs/specs/versioned-trie.md` §2 |
| `store-errors.cddl` | the reference store's error family (`StoreError`), which a versioned trie's `Store` holds | `docs/specs/storage.md`, `docs/specs/conformance.md` §6 |
| `jlvm-errors.cddl` | the JLVM's error family, its sites and a `Crypto` error's cause | `docs/specs/jlvm.md` §3, `docs/specs/conformance.md` §6 |
| `zk-jlvm.cddl` | the statement the SP1 guest commits for a proved JLVM call | `docs/specs/zk-jlvm.md` §2 (embedded) |
| `*-vectors.cddl` | The conformance vector files in `shared/vectors/` | the matching spec |

Every vector file in `shared/vectors/` must have a schema here. `VectorSchemasSuite` checks this, and checks that each
file round-trips through its schema without loss.

## The profile

- **Allowed:**
  - maps with text-literal keys (each required or `?`), and `{ * tstr => T }`;
  - integers and ranges, and labelled integer enumerations `&(a: 1)`. `uint`, `nint` and `int` follow RFC 8610: major
    type 0 (0..2^64−1), major type 1 (−2^64..−1), or either (−2^64..2^64−1). The canonical data model's integers are
    unbounded, so a field that may need more than 64 bits must say so with an explicit range (as `poseidon.cddl` does);
  - text, bytes and sized strings (a text `.size` counts UTF-8 bytes, as RFC 8610 defines it);
  - `text .regexp` with one of the shared patterns `([0-9a-f]{2})*`, `0x([0-9a-f]{2})*`, `0|[1-9][0-9]*` and the
    versioned-trie state-key grammar `[a-z0-9][a-z0-9._-]*(/[a-z0-9][a-z0-9._-]*)*`. Each SDK
    implements these as named validators rather than carrying a regular-expression engine, so a new pattern means a new
    validator in every SDK core (`Profile.Patterns`);
  - homogeneous arrays;
  - literal values;
  - choices of named map rules and text literals that can be told apart by their keys. A choice of text literals only
    (an error kind, such as `kes-error-kind`) is a closed type in every SDK; Rust lists its values in `ALL`, Scala in
    `values`. A choice may name such a choice of texts (`kes-outcome = "accepted" / "rejected" /
    errors.kes-error-kind`): it takes those texts, and is typed by the named choice (Rust: a variant holding its enum);
  - **error families**: a choice of inline maps, each led by a distinct `"kind"` text literal in PascalCase
    (`kes-error = { "kind": "Malformed", "what": text, "reason": text // "kind": "Exhausted", ... }`), beside a rule
    `kes-error-kind` that lists the same kinds in order. It is an error type (Scala a sealed trait, Rust an enum with
    `kind()`, `Display` and `Error`, TypeScript a union on `kind`), never a wire type: no codec is generated, and no
    wire type may hold one. Its fields are text, integers (TypeScript: a `number` within the signed 64-bit range, a
    `bigint` past it, such as a plain `uint`), booleans, kinds, previews, or another error family (a reason with fields
    of its own, `InvalidNumber`'s `bound`), never one that holds the family back;
  - **named texts**: a choice of rules that are each one text literal (`jlvm-op = op-not / op-eq`, `op-not = "!"`) is a
    closed type whose members are named by their rules, the prefix they all share up to a `-` removed and the rest
    split at `-` (`op-strict-eq` is `StrictEq`, `op-sha3_256` `Sha3_256`), so a text need not be an identifier: Scala an
    `enum` carrying `wire` (and `fromWire`), Rust an enum with `wire()`, `from_wire` and `ALL`, TypeScript a union of
    the texts. A choice mixing such rules with anything else is refused; a field naming one of the rules is its
    literal. Nothing is emitted for the member rules themselves;
  - **previews**: a rule named `preview`, whose body must be `{ "head": text, "length": uint }` (the preview's
    normative fields, `docs/specs/conformance.md` §8), is each SDK's own `Preview` (Scala
    `xyz.kd5ujc.core.canonical.Preview`, Rust `c::Preview`, TypeScript `Preview` from `preview.ts`). It is not
    generated; an error's field naming it holds a Preview, so caller text in an error is typed, never raw text. No wire
    type (record, alias, element or choice alternative) may hold one;
  - **constants**: a rule whose whole body is a `uint` literal (`max-depth = 256`). A constant is a value, not a type:
    a range or `.size` bound may name it (`0..max-depth`, `bytes .size (0..max-value-bytes)`), a field or element may
    not. Each language gets it as a constant: Scala a `final val` (`Int`, else `Long`, else `BigInt`), TypeScript an
    `export const` (a `number` up to 2^53 − 1, else a `bigint`), Rust a `u64` plus the same value in each narrower type
    it fits (`as_u16`, `as_u32`, and `as_usize` up to 2^32 − 1), so no site casts. CDDL occurrence bounds
    (`[1*1024 x]`) take literals only, so a bound that is a limit there is pinned by a test against the constant.
- **Comments are documentation.** The `;` comment lines directly above a rule, or above a map entry that starts its
  line (an error kind's `"kind"` entry, or a field), become that type's, kind's or field's documentation in every
  language (Scaladoc, Rust doc comments, TSDoc). A blank line or a `;#` pragma ends the block, and a comment after code
  on its line documents nothing, so a file's header is not the first rule's documentation.
- **Rejected at generation time:** integer map keys, floats, tags, groups, inline choices and ambiguous alternatives.
- **What generated decoders check:** shape and ranges only. Domain invariants, such as trie segment padding or Poseidon
  `position < 2^depth`, stay with the domain decoders and types.
