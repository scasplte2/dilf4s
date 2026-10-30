# dilf4s Merkle tree (v1)

Status: normative. Conformance vectors: `shared/vectors/merkle_v1.json`.

## 1. Construction

The tree is the RFC 9162 §2.1 (Certificate Transparency v2) left-balanced binary Merkle tree over an ordered list of `n` leaves `D[0..n)`:

```
MTH({})        = emptyHash
MTH({d0})      = leafHash(d0)
MTH(D[n]), n>1 = nodeHash(MTH(D[0:k]), MTH(D[k:n]))   where k is the largest power of two < n
```

Equivalently, build level by level: pair adjacent nodes, and **promote an unpaired last node unchanged** to the next level. Nodes are never duplicated. A tree is therefore never equal to the same tree with its last leaf repeated, unlike Bitcoin-style trees (CVE-2012-2459), and unlike metakit's tree, where `[a,b,c]` and `[a,b,c,c]` share a root.

## 2. Hashing

Domain separation uses tagged hashing (`docs/specs/canonical-encoding.md` §4) with hash function `H`:

| Function | Definition |
|---|---|
| `leafHash(d)` | `taggedHash_H("dilf4s/merkle/leaf/v1", d)` |
| `nodeHash(l, r)` | `taggedHash_H("dilf4s/merkle/node/v1", l ‖ r)` |
| `emptyHash` | `taggedHash_H("dilf4s/merkle/empty/v1", "")` |

A leaf's `d` is the canonical encoding of the leaf value, or opaque bytes when the leaf is bytes.

An `rfc6962` profile exists only for interoperability tests: `leafHash(d) = H(0x00 ‖ d)`, `nodeHash(l, r) = H(0x01 ‖ l ‖ r)`, `emptyHash = H("")`.

## 3. Proofs

- **Inclusion proof** for leaf `m` in a tree of size `n`: the RFC 9162 §2.1.3.1 audit path. Verification follows §2.1.3.2 exactly. As in RFC 9162, the verifier must take the tree size **and** root from the same trusted tree head: the algorithm binds the leaf to `(size, root)` together, not to the size alone. A root from a smaller tree can satisfy a proof that claims a larger size.
- **Consistency proof** between sizes `0 < m <= n`: the RFC 9162 §2.1.4.1 path. Verification follows §2.1.4.2 exactly. When `m == n` the proof is empty and the roots must be equal. Size 0 is consistent with every tree, given an empty proof and `emptyHash` as the old root.

## 4. Proof encoding

Proofs are canonical values (`docs/specs/canonical-encoding.md`). The normative schema is `shared/schemas/merkle.cddl`. It is reproduced here verbatim and checked by a test; generated types and codecs for it live in `xyz.kd5ujc.models.merkle`:

```cddl
; dilf4s Merkle tree proofs, v1 (docs/specs/merkle.md section 4). Normative.
; Proofs are canonical values (docs/specs/canonical-encoding.md); digests are opaque byte strings.

merkle-inclusion = {
  "index": tree-index,
  "size": tree-index,
  "path": [* bytes],
}

merkle-consistency = {
  "old": tree-index,
  "new": tree-index,
  "path": [* bytes],
}

tree-index = 0..9223372036854775807

; The persistent Merkle log's state record (section 5): tree size and a generation counter that every commit
; increments. Commits compare-and-write this record, so concurrent writers cannot both succeed.
merkle-log-state = {
  "size": tree-index,
  "generation": tree-index,
}
```

## 5. Storage (informative)

The persistent Merkle log stores only perfect subtree hashes, keyed by `(level, index)` as fixed-width
order-preserving integers. It also stores one `merkle-log-state` record (§4 schema) holding the tree size and a
generation counter. Every commit compare-and-writes the state record, so of two concurrent writers at most one succeeds.
