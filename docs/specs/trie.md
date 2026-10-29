# dilf4s authenticated dictionary: binary Patricia Merkle trie (v1, draft)

Status: draft for review. Replaces the 16-ary Merkle-Patricia trie and the separate sparse Merkle tree for the generic
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

The trie is a crit-bit tree: every branch has exactly two non-empty children, and runs of bits shared by every key below
a node are compressed into that node's **segment**.

```
Leaf(segment, valueHash)
Branch(segment, valueHash?, left, right)      -- left follows bit 0, right follows bit 1
```

The bits of a node's subtree are as follows:
- **Leaf:** the path bits remaining below its parent, after the parent's branch bit. A leaf's key ends exactly at the end of its segment.
- **Branch:** its segment, then the branch bit that chooses the child. Its value slot, which only exists in variable mode, holds the key that ends exactly after the segment.

The empty trie has no nodes.

**Segments** are encoded as `{ "n": bitLength, "b": bytes }`. The bits sit in `b` MSB-first, padded with zero bits to a whole byte, and any padding bit that isn't zero is invalid.

## 3. Hashing

With hash function `H` and tagged hashing (`docs/specs/canonical-encoding.md` §4):

| Item | Hash |
|---|---|
| value | `valueHash(v) = taggedHash_H("dilf4s/trie/value/v1", encode(v))`, where `encode` is the canonical encoding, or raw bytes for byte values |
| leaf | `taggedHash_H("dilf4s/trie/leaf/v1", encode({ "s": segment, "v": valueHash }))` |
| branch | `taggedHash_H("dilf4s/trie/branch/v1", encode({ "s": segment, "v"?: valueHash, "l": leftHash, "r": rightHash }))` |
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
2. Check the structure: no branch has an `Empty` child, the mode's rules hold, segment padding is canonical, and the bit depth of every path stays within the mode's key length.

**Answering a lookup** for key `k` after verification: walk `k`'s path from the top node.
- **Mismatch.** If `k`'s bits differ from a node's segment, or `k` ends inside the segment: **absent**.
- **Leaf.** If the segment is fully consumed at a leaf: **present** with the leaf's value hash when `k` also ends there, otherwise **absent**.
- **Branch.** If the segment is fully consumed at a branch:
  - if `k` ends there, it's **present** with the slot's value hash, or **absent** if the slot is empty;
  - otherwise take the next bit and continue into that child.
- **Stub.** Reaching a stub means the proof **does not cover** `k`. That's a distinct result, never "absent".
- **Empty.** An `Empty` proof means every key is absent.

**Prefix query** for prefix `p`: walk to the node whose subtree holds exactly the keys starting with `p`. If any stub lies inside that subtree, the proof doesn't cover `p`. Otherwise every key/value-hash pair under `p` is listed, and the list is **complete**, because the expanded subtree hashes to the committed value.

A single-key proof is the partial trie expanded along that key's path, with every sibling as a stub. Batch, prefix and mixed proofs are the union of their paths, so shared nodes appear once.

## 5. Proof encoding

A proof is a canonical value:

```
stub   = { "h": bytes }
leaf   = { "s": segment, "v": bytes }
branch = { "s": segment, "v"?: bytes, "l": node, "r": node }
empty  = {}
```

Values travel separately. To check a value `x` against a proven `valueHash`, compute `valueHash(x)` and compare.

## 6. Storage (informative)

Nodes are stored by digest and shared across versions. Each retained version records its root. A node's reference count is the number of live parents plus the number of retained versions whose root it is. A commit writes its new nodes and increments the counts of the existing nodes they reference, all in one atomic batch. Pruning a version decrements its root's count, and any node whose count reaches zero is deleted and its children decremented, iteratively, in the same batch.
