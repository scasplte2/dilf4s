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
