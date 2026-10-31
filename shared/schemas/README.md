# Schemas

These are CDDL (RFC 8610) definitions, written in the dilf4s canonical profile (see
`models-gen/.../CanonicalEmitter.scala`). They are the single source of truth for shared byte and record formats. The
build generates Scala types and codecs from them into `core` (`xyz.kd5ujc.models.<module>`), and other-language SDKs generate or
validate against the same files.

| Schema | Defines | Normative text |
|---|---|---|
| `merkle.cddl` | Merkle inclusion and consistency proofs (wire) | `docs/specs/merkle.md` §4 (embedded) |
| `trie.cddl` | Patricia trie proofs (wire shape) | `docs/specs/trie.md` §5 (embedded) |
| `poseidon.cddl` | Poseidon Merkle proofs (wire) | `docs/specs/zk.md` §4 (embedded) |
| `merkle.cddl` (`merkle-log-state`) | the persistent Merkle log's state record | `docs/specs/merkle.md` §5 |
| `storage.cddl` | versioned-store history records | `docs/specs/storage.md` (embedded) |
| `bloom.cddl` | Bloom filter encoding | `docs/specs/bloom.md` (embedded) |
| `crypto.cddl` | sizes of keys, signatures, VRF/KES/zk proofs | `docs/specs/signing.md` §4 (embedded) |
| `*-vectors.cddl` | The conformance vector files in `shared/vectors/` | the matching spec |

Every vector file in `shared/vectors/` must have a schema here. `VectorSchemasSuite` checks this, and checks that each
file round-trips through its schema without loss.

## The profile

- **Allowed:**
  - maps with text-literal keys (each required or `?`), and `{ * tstr => T }`;
  - integers and ranges, and labelled integer enumerations `&(a: 1)`;
  - text, `text .regexp`, bytes and sized strings;
  - homogeneous arrays;
  - literal values;
  - choices of named map rules and text literals that can be told apart by their keys.
- **Rejected at generation time:** integer map keys, floats, tags, groups, inline choices and ambiguous alternatives.
- **What generated decoders check:** shape and ranges only. Domain invariants, such as trie segment padding or Poseidon
  `position < 2^depth`, stay with the domain decoders and types.
