# dilf4s authenticated dictionary: binary Patricia Merkle trie (v1, draft)

Status: normative (v1), reviewed 2026-10-29. Replaces the 16-ary Merkle-Patricia trie and the separate sparse Merkle tree for the generic
fixed-length and variable-length key cases. (The Poseidon sparse Merkle tree used by zk opcodes is specified separately.)

## 1. Keys and modes

A key is a byte string supplied by the caller. The trie never hashes keys; callers who want uniformly distributed keys
(for example to bound depth against adversarial inputs) hash them before inserting.

| Mode | Keys | Notes |
|---|---|---|
| `fixed(n)` | exactly `n` bytes, `1 <= n <= 1024` | no key is a prefix of another, so branch nodes never carry a value |
| `variable(max)` | `0..max` bytes, `max <= 65535` | a key may be a prefix of another; it is stored in the value slot of the branch at its end |

A key's **path** is its bits, most significant bit first: byte 0 bit 7, byte 0 bit 6, …, giving `8 * len` bits.

## 2. Nodes

Runs of bits shared by every key below a node are compressed into that node's **segment**; there are no separate
extension nodes.

```
Leaf(segment, valueHash)
Branch(segment, valueHash?, left?, right?)     -- left follows bit 0, right follows bit 1
```

**Branch invariant:** a branch has **two** children, or **one** child **and** a value. In `fixed` mode every branch
has two children and no value. With maximal segments this makes the shape a function of the key set alone: nodes
exist exactly where keys diverge or where a key ends and others continue, so the root does not depend on insertion
order.

The bits of a node's subtree are as follows:
- **Leaf:** the path bits remaining below its parent, after the parent's branch bit. A leaf's key ends exactly at the end of its segment.
- **Branch:** its segment, then the branch bit that chooses the child. Its value slot, which only exists in variable mode, holds the key that ends exactly after the segment. A missing child means no key continues with that bit.

The empty trie has no nodes.

**Segments** are encoded as `{ "n": bitLength, "b": bytes }`. The bits sit in `b` MSB-first, padded with zero bits to a whole byte, and any padding bit that isn't zero is invalid.

## 3. Hashing

With hash function `H` and tagged hashing (`docs/specs/canonical-encoding.md` §4):

| Item | Hash |
|---|---|
| value | `valueHash(b) = taggedHash_H("dilf4s/trie/value/v1", b)` over the value's bytes `b`: `commit(v)` (below) for a value `v` (`Trie.putCanonical`), or the bytes themselves for a trie of opaque byte values (`Trie.putValue`, as the conformance vectors do) |
| leaf | `taggedHash_H("dilf4s/trie/leaf/v1", encode({ "s": segment, "v": valueHash }))` |
| branch | `taggedHash_H("dilf4s/trie/branch/v1", encode({ "s": segment, "v"?: valueHash, "l"?: leftHash, "r"?: rightHash }))` (absent fields are omitted) |
| empty trie root | `taggedHash_H("dilf4s/trie/empty/v1", "")` |

The root of a non-empty trie is the hash of its top node. Values are stored outside the nodes; a node commits only to the value's hash.

## 4. Proofs: partial tries

A proof is a **partial trie**, a tree of:

```
Stub(hash)                                     -- an unexplored subtree, given only by its hash
Leaf(segment, valueHash)
Branch(segment, valueHash?, left, right)       -- children are partial nodes
Empty                                          -- only as the whole proof of an empty trie
```

**Verification against `root`:**
1. Recompute the partial trie's hash: a stub contributes its hash, and other nodes hash per §3. The result must equal `root`.
2. Check the structure: every branch satisfies the branch invariant (§2), no `Empty` appears below the top, the mode's rules hold, segment padding is canonical, and the bit depth of every path stays within the mode's key length.

**Answering a lookup** for key `k` after verification: walk `k`'s path from the top node.
- **Mismatch.** If `k`'s bits differ from a node's segment, or `k` ends inside the segment: **absent**.
- **Leaf.** If the segment is fully consumed at a leaf: **present** with the leaf's value hash when `k` also ends there, otherwise **absent**.
- **Branch.** If the segment is fully consumed at a branch:
  - if `k` ends there, it's **present** with the slot's value hash, or **absent** if the slot is empty;
  - otherwise take the next bit and continue into that child, or answer **absent** if the branch has no child for that bit.
- **Stub.** Reaching a stub means the proof **does not cover** `k`. That's a distinct result, never "absent".
- **Empty.** An `Empty` proof means every key is absent.

**Prefix query** for prefix `p`: walk to the node whose subtree holds exactly the keys starting with `p`. If any stub lies inside that subtree, the proof doesn't cover `p`. Otherwise every key/value-hash pair under `p` is listed, and the list is **complete**, because the expanded subtree hashes to the committed value.

**Range query** for `[from, until)`, with `until` absent meaning unbounded. Keys are compared in bitwise lexicographic
order, a proper prefix before its extensions. Walk every subtree whose keys can fall in the range:
- a stub there means the proof doesn't cover the range;
- otherwise every key/value-hash pair with `from <= key < until` is listed, in key order.

The list is **complete**, including when it is empty, because every subtree that could hold such a key is expanded.
`from` and `until` are bit strings and need not be keys.

A single-key proof is the partial trie expanded along that key's path, with every sibling as a stub. Batch, prefix, range and mixed proofs are the union of their paths, so shared nodes appear once.

## 5. Proof encoding

A proof is a canonical **array** of node records in pre-order: each branch record is followed by the records of its
present children, left before right. Keeping the encoding flat means proof depth is never bounded by the canonical
model's nesting limit, so deep variable-mode tries stay provable.

The normative shape is `shared/schemas/trie.cddl`. It is reproduced here verbatim and checked by a test; generated wire types live in `xyz.kd5ujc.models.trie`:

```cddl
; dilf4s binary Patricia trie proofs, v1 (docs/specs/trie.md sections 2 and 5). Normative for shape.
; A proof is a flat array of node records in pre-order. Invariants the shape cannot express (zero padding bits,
; len(b) == ceil(n / 8), pre-order structure, masks consistent with present children, no trailing records) are
; checked by the domain decoder, not by these types.

trie-proof = [* trie-record]

trie-record = trie-stub / trie-leaf / trie-branch

trie-stub = { "h": bytes }

trie-leaf = {
  "s": trie-segment,
  "v": bytes,
}

trie-branch = {
  "s": trie-segment,
  ? "v": bytes,
  "c": trie-children,
}

trie-children = &(left: 1, right: 2, both: 3)

trie-segment = {
  "n": 0..9223372036854775807,
  "b": bytes,
}

; A node commitment (hashed under the leaf or branch tag, section 3) and a stored node record (section 6). A leaf
; has exactly "s" and "v"; a branch has "s" and either two of "l"/"r", or one of them and "v". The shape cannot tell
; the two apart, and need not: the hash tag (or, in storage, the presence of children) does.
trie-node = {
  "s": trie-segment,
  ? "v": bytes,
  ? "l": bytes,
  ? "r": bytes,
}
```

Decoders rebuild the tree with an explicit stack and reject trailing records, missing children, a mask inconsistent
with the branch invariant, and any record that matches none of the three shapes.

**Value commitment.** A value `v` becomes leaf or value bytes as `commit(v)`:
- `0x00 ‖ b` for a byte string `b`;
- `0x01 ‖ cbor(v)` for any other value (`canonical-encoding.md` §2).

The tag keeps the mapping injective. Untagged, the bytes `0x6161` and the text `"a"` (whose CBOR is `0x6161`) would
commit identically, and a proof of one would verify a claim of the other.

Values travel separately. To check a value `x` against a proven `valueHash`, compute `valueHash` of its bytes and
compare.

**Privacy note.** A value hash does not hide a low-entropy value (a boolean, a small amount, an enum state): it can be
recovered by hashing candidates. Proofs also reveal the key bits along the queried paths, including a neighbouring
key's prefix in an absence proof. A salted value mode (hash over a per-entry random salt plus the value, salt held by
the client) is a possible extension; it is not part of v1.

## 6. Storage (informative)

Nodes are stored by digest and shared across versions, each as a 4-byte big-endian reference count followed by the canonical CBOR of its `trie-node` record (re-hashed and checked when loaded). Each retained version records its root. A node's reference count is the number of live parents plus the number of retained versions whose root it is. A commit writes its new nodes and increments the counts of the existing nodes they reference, all in one atomic batch. Pruning a version decrements its root's count, and any node whose count reaches zero is deleted and its children decremented, iteratively, in the same batch.
