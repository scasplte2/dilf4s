# dilf4s Bloom filter (v1)

Status: normative.

A filter has `m` bits and `k` hash functions:

- `m` is a power of two in `64..2^24`;
- `k` is in `1..32`;
- the 2048-bit, 4-hash ledger filter is the default.

**Indices.** For an item `x`, let `d = taggedHash_H("dilf4s/bloom/v1", x)` (`canonical-encoding.md` §4). Then:

- `h1` = the first 8 bytes of `d`, big-endian unsigned;
- `h2` = the next 8 bytes, big-endian unsigned, with the lowest bit set;
- the indices are `(h1 + i·h2) mod m` for `i = 0..k-1`.

Bits are numbered MSB-first. Typed values are added as the canonical CBOR of their canonical value.

**Encoding.** The filter's canonical value is the following record. This is the normative schema
`shared/schemas/bloom.cddl`, reproduced verbatim. The decoder also requires `m` to be a power of two and
`len(b) = m / 8`:

```cddl
; dilf4s Bloom filter (docs/specs/bloom.md). "m" is the number of bits, a power of two (checked by the domain
; decoder); "k" the number of hash functions; "b" the m bits, MSB-first, as m / 8 bytes.

bloom-filter = {
  "m": 64..16777216,
  "k": 1..32,
  "b": bytes,
}
```
