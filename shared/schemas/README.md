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
  - `text .regexp` with one of the shared patterns `([0-9a-f]{2})*`, `0x([0-9a-f]{2})*` and `0|[1-9][0-9]*`. Each SDK
    implements these as named validators rather than carrying a regular-expression engine, so a new pattern means a new
    validator in every SDK core (`Profile.Patterns`);
  - homogeneous arrays;
  - literal values;
  - choices of named map rules and text literals that can be told apart by their keys.
- **Rejected at generation time:** integer map keys, floats, tags, groups, inline choices and ambiguous alternatives.
- **What generated decoders check:** shape and ranges only. Domain invariants, such as trie segment padding or Poseidon
  `position < 2^depth`, stay with the domain decoders and types.
