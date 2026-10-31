# dilf4s storage formats (v1)

Status: normative for record values; informative for key layout.

## 1. Keys

Keys are **binary and order-preserving**, not CBOR, so that range scans follow key order:

- **Integers:** big-endian with the sign bit flipped (8 bytes for 64-bit, 4 for 32-bit), so negative values sort before
  positive ones.
- **Text:** UTF-8.
- **Digests:** raw bytes.
- **Composite keys:** a fixed-width component followed by the rest.
- **Variable-length components inside a composite key:** escape `00` as `00 FF` and end with `00 00`. This keeps
  lexicographic order, and no key is a prefix of another.

Every key is placed under a namespace prefix: a one-byte length followed by the UTF-8 name (1..255 bytes), so no namespace is a prefix of another.

## 2. Versioned store

A versioned store keeps several kinds of entry:

- the current value of each key;
- a per-version write log;
- per-key history, at `history ‖ terminated(key) ‖ version`;
- two metadata markers, `latest` and `floor`, each a big-endian unsigned 64-bit version.

A history entry is the following record, written in canonical CBOR (`docs/specs/canonical-encoding.md`). This is the
normative schema `shared/schemas/storage.cddl`, reproduced verbatim:

```cddl
; dilf4s storage records (docs/specs/storage.md). Keys are not CBOR: they are order-preserving binary encodings.

; A VersionedStore history record: the value of one key before and after the write at one version. An absent
; field means the key had no value.
versioned-record = {
  ? "b": bytes,
  ? "a": bytes,
}
```

Reading key `k` at version `v` takes the first history record of `k` after `v`. Its `"b"` field is the value at `v`. If
there is no such record, the current value applies.

Once history is pruned through a floor version `f > 0`, the log entries of `f` itself are gone with it. The change
into `f` is therefore reported as pruned, never as an empty change.

## 3. Staged writes (informative)

Every mutation of a versioned store or a trie store (`docs/specs/trie.md` §6) is a single compare-and-write
(`writeIf(guards, batch)`). Its guards are the store's version records as read when the change was computed, and its
batch is every write the change makes. Each store also offers the change **staged**: the guards, the batch and the
version after it, computed without writing.

- **Composition.** Stores sharing one backend combine their stages into one `writeIf`, with the union of the guards
  and the concatenation of the batches. Either every store moves or none does, so a crash can never leave one store
  ahead of another. `docs/specs/versioned-trie.md` commits its four stores this way.
- **Drafts.** A stage can be built on another stage that has not been written. It reads through the writes below it
  (current values, trie nodes and their reference counts, the latest root) and carries their guards and batch, so
  writing the top of a chain writes the whole chain or nothing. A chain is therefore a speculative sequence of
  versions that costs no writes until it is adopted.
- **Conflicts.** If another writer moved the store after the stage was computed, the guards no longer match and the
  write is refused with a version conflict. Nothing is written.
