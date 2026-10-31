# dilf4s canonical encoding and tagged hashing (v1)

Status: normative. Every dilf4s implementation (Scala reference, TS, Rust, Go, Java, Python) MUST produce identical bytes for identical values. The conformance vectors in `shared/vectors/canonical_encoding_v1.json` are part of this specification.

## 1. Data model

A canonical value is exactly one of:

| Kind | Domain |
|---|---|
| null | |
| boolean | `true`, `false` |
| integer | any integer (unbounded) |
| rational | `n/d` with `d ≥ 2` and `gcd(\|n\|, d) = 1` |
| text | a sequence of Unicode scalar values (no surrogate code points) |
| bytes | a finite byte sequence |
| array | a finite sequence of canonical values |
| map | a finite set of `(text key, canonical value)` entries with unique keys and **no null values** |

The data model has no floating-point numbers, no NaN or infinity, no negative zero, no undefined, and no other tags.

**Absent equals null.** A map never contains a null value. Constructing a map drops entries whose value is null, so adding an optional field whose value is "none" leaves the encoding, and therefore every hash, unchanged. Nulls inside arrays are kept, because position is meaningful there.

**Integers and rationals are one numeric domain.** A number whose value is an integer is always the integer kind. The rational kind only ever holds non-integers.

For other-language implementers, here is the same model in CDDL (RFC 8610). This block is **informative**: it is not
in `shared/schemas`, because tags and recursion are outside the code generator's profile. The prose above, and the
deterministic encoding rules in §2, are normative.

```text
canonical-value = null / bool / int / rational / tstr / bstr
                / [* canonical-value]
                / { * tstr => canonical-value }        ; unique keys, no null values
rational = #6.30([numerator: int, denominator: uint])  ; denominator >= 2, gcd(|n|, d) = 1
```

## 2. Encoding: deterministic CBOR profile

Values encode with RFC 8949 CBOR under the following rules. Any other encoding of the same value is **invalid**, and decoders MUST reject it.

| Kind | Encoding |
|---|---|
| null | `0xf6` |
| false / true | `0xf4` / `0xf5` |
| integer `0 ≤ n < 2^64` | major type 0, argument `n` |
| integer `-2^64 ≤ n < 0` | major type 1, argument `-1 - n` |
| integer `n ≥ 2^64` | tag 2 followed by a byte string holding the big-endian magnitude of `n` with no leading zero bytes |
| integer `n < -2^64` | tag 3 followed by a byte string holding the big-endian `-1 - n` with no leading zero bytes |
| rational `n/d` | tag 30 followed by a 2-element array `[n, d]`, each encoded as an integer |
| text | major type 3, then the UTF-8 bytes (well-formed, shortest form, no surrogates) |
| bytes | major type 2 |
| array | major type 4, definite length |
| map | major type 5, definite length, keys in **ascending bytewise lexicographic order of their encoded form** (RFC 8949 §4.2.1); no duplicate keys |

Further rules:
- **Arguments** (lengths, integers, tag numbers) use the shortest form: values `< 24` are immediate, then 1, 2, 4 and 8 bytes for `< 2^8`, `< 2^16`, `< 2^32` and `< 2^64`.
- **Not allowed:** indefinite-length items, floats (major type 7 additional information 25/26/27), simple values other than false/true/null, and tags other than 2, 3 and 30.
- **Tags 2 and 3** are only valid when the value is outside the major type 0/1 range.
- **Decoders** MUST reject trailing bytes after the top-level item and MUST enforce a maximum nesting depth. The default limit is 256 and counts arrays and maps, empty ones included: a scalar has depth 0, and an array or map has depth one more than its deepest element (1 when empty).

## 3. JSON mapping

JSON is the human-facing and API format. It converts to the data model as follows.

**Numbers** are read **exactly from their source text** as a terminating decimal and converted to an integer or a rational. For example, `0.1` becomes 1/10, `1.0` becomes the integer 1, `-0` becomes the integer 0, and `9007199254740993` stays exact.
- Implementations MUST NOT route numbers through IEEE-754 doubles.
- A decimal whose effective scale `|fraction digits - exponent|` exceeds 10000 is rejected, to bound memory.

**Strings** become text. A string containing unpaired surrogates is rejected.

**Objects** become maps, with null-valued members dropped. Duplicate member names are rejected.

**Arrays** become arrays, and `null`, `true` and `false` map to themselves. The data model's bytes kind has no JSON form, so it only arises from typed data.

Converting back to JSON (for display only) works as follows:
- Output is compact (no whitespace), and object members are written in ascending Unicode code-point order of their names.
- Integers are written as exact decimal digits.
- A rational is written as an exact decimal when it terminates. Otherwise it can't be written as JSON and conversion fails.
- Bytes can't be written as JSON, and conversion fails.

## 4. Tagged hashing

```
taggedHash_H(tag, msg) = H( H(utf8(tag)) || H(utf8(tag)) || msg )
contentHash_H(tag, value) = taggedHash_H(tag, encode(value))
```

- `H` is the hash function in use: SHA-256 by default; SHA-512, SHA3-256, SHA3-512, Keccak-256, BLAKE2b-256 and
  BLAKE2b-512 are also defined. Keccak-256 is the pre-standard Keccak padding (as Ethereum uses it), not SHA3-256;
  BLAKE2b-256 is BLAKE2b with a 32-byte output parameter, not a truncated BLAKE2b-512. The conformance vectors give a
  plain digest and tagged hashes for each.
- `tag` is a domain tag matching `^[a-z0-9]+(/[a-z0-9][a-z0-9._-]*)*/v[0-9]+$`, for example `dilf4s/mpt/leaf/v1`.

Every structure and message type has its own tag, so a digest produced in one context can never be valid in another. The tag ends with a version, so a format change takes a new tag rather than silently changing bytes.

This is the BIP-340 tagged-hash construction, extended from SHA-256 to any `H`.
