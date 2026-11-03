# dilf4s canonical encoding and tagged hashing (v1)

Status: normative. Every dilf4s implementation (Scala reference, TS, Rust, Go, Java, Python) MUST produce identical bytes for identical values. The conformance vectors in `shared/vectors/canonical_encoding_v1.json` are part of this specification.
Refusals (their kinds, normative fields and the work allowed before them) follow `conformance.md`.

## 1. Data model

A canonical value is exactly one of:

| Kind | Domain |
|---|---|
| null | |
| boolean | `true`, `false` |
| integer | any integer (unbounded) |
| rational | `n/d` with `d ≥ 2`, `gcd(\|n\|, d) = 1`, and `n` and `d` each at most `MaxRationalBits` wide |
| text | a sequence of Unicode scalar values (no surrogate code points) |
| bytes | a finite byte sequence |
| array | a finite sequence of canonical values |
| map | a finite set of `(text key, canonical value)` entries with unique keys and **no null values** |

The data model has no floating-point numbers, no NaN or infinity, no negative zero, no undefined, and no other tags.

**Absent equals null.** A map never contains a null value. Constructing a map drops entries whose value is null, so adding an optional field whose value is "none" leaves the encoding, and therefore every hash, unchanged. Nulls inside arrays are kept, because position is meaningful there.

**Integers and rationals are one numeric domain.** A number whose value is an integer is always the integer kind. The rational kind only ever holds non-integers.

**`MaxRationalBits` = 33220** (`canonical-max-rational-bits` in `shared/schemas/limits.cddl`): a rational's numerator
and its denominator each have a `bitLength` of at most 33220 (`bitLength` is Java's `BigInteger.bitLength`, `jlvm.md`
§2: for `n < 0` it is the bit length of `-n - 1`, so `-2^33220` is 33220 bits wide and `-2^33220 - 1` is 33221). 33220 is the bit length of `10^10000`, the bound on a JSON
number's digits and scale (§3), so every rational JSON reads is within it, and every JLVM number (4096 bits) is too.
Integers have no such bound. The reason is cost: a rational is valid only in lowest terms, and checking that takes a gcd,
which is superlinear in its operands. With the bound, decoding a rational costs at most one gcd of operands at most
33220 bits wide, and a rational that wide takes over 8 KiB to encode, so decoding stays linear in its input; without it
a rational of n bytes would cost O(n²).

The bound is a property of the value model, not only of decoding: **no canonical value has a rational part wider than
33220 bits**, so every value that can be built encodes to bytes every decoder accepts, and a value committed or stored
can always be read back. Every producer refuses a wider rational, after reducing it (`2^33221/6` is `2^33220/3`, still
too wide; `2^33221/2^33220` is the integer 2), with kind `RationalTooWide` and normative field `limit` = 33220
(`conformance.md` §3):

| Producer | Refusal |
|---|---|
| CBOR decoding (§2) | `RationalTooWide`, before any gcd |
| building a number from a numerator and denominator (Scala `CanonicalValue.number` and `Canonical[Ratio]`, Rust `Value::number`, TypeScript `Value.number`) | `RationalTooWide` (Rust `NumberError::RationalTooWide`, beside `DivisionByZero`) |
| building a rational exactly as given (Rust `Rational::new`, TypeScript `RationalValue.of`) | none: no rational (`None`, `undefined`), as for one not in lowest terms |
| JSON (§3) | never reached: the digit and scale bounds keep both parts within `10^10000` |
| JLVM arithmetic (`jlvm.md` §2) | never reached: every number an operation reads or produces is within 4096 bits, or it is a `Limit` error |

Each SDK's rational type has no other constructor, so no wider rational can be made without one of these refusing it.

For other-language implementers, here is the same model in CDDL (RFC 8610). This block is **informative**: it is not
in `shared/schemas`, because tags and recursion are outside the code generator's profile. The prose above, and the
deterministic encoding rules in §2, are normative.

```text
canonical-value = null / bool / int / rational / tstr / bstr
                / [* canonical-value]
                / { * tstr => canonical-value }        ; unique keys, no null values
rational = #6.30([numerator: int, denominator: uint])  ; denominator >= 2, gcd(|n|, d) = 1,
                                                       ; bitLength of each <= 33220 (MaxRationalBits)
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
- **Rationals** are checked in this order, and the first failure is the refusal: tag 30 wraps a 2-element array
  (`NonCanonical`); the numerator is a canonical integer (`NonCanonical`) at most `MaxRationalBits` wide
  (`RationalTooWide`, §1); the same for the denominator; the denominator is at least 2
  (`NonCanonical`); and last, `gcd(|n|, d) = 1` (`NonCanonical`). A part's width is checked as soon as it is read, so a
  part too wide is refused before the next is read and before any gcd, and a decoder runs at most one gcd per rational.

## 3. JSON mapping

JSON is the human-facing and API format. It converts to the data model as follows.

**Numbers** are read **exactly from their source text** as a terminating decimal and converted to an integer or a rational. For example, `0.1` becomes 1/10, `1.0` becomes the integer 1, `-0` becomes the integer 0, and `9007199254740993` stays exact.
- Implementations MUST NOT route numbers through IEEE-754 doubles.
- A decimal whose effective scale `|fraction digits - exponent|` exceeds 10000 is rejected, to bound memory.
- A number's significand (its integer and fraction digits together, leading zeros aside) and its exponent (leading
  zeros aside) each have at most 10000 digits; a longer one is rejected. Decimal conversion costs more than linear time
  in the digits, so both are counted on the source text before any conversion. The bound equals the scale bound, so
  every decimal of an accepted scale has a form within it (`0.` then 10000 zeros then `1e1` is 10^-10000).

**Strings** become text. A string containing unpaired surrogates is rejected.

**Objects** become maps, with null-valued members dropped. Duplicate member names are rejected.

**Arrays** become arrays, and `null`, `true` and `false` map to themselves. The data model's bytes kind has no JSON form, so it only arises from typed data.

Converting back to JSON (for display only) works as follows:
- Output is compact (no whitespace), and object members are written in ascending Unicode code-point order of their names.
- Integers are written as exact decimal digits.
- A rational is written as an exact decimal when it terminates. Otherwise it can't be written as JSON and conversion fails.
- A number is written only when its decimal is within the bounds above (at most 10000 significant digits, and a scale
  of at most 10000), so that every rendering reads back; otherwise conversion fails. `10^10000` (10001 digits) and
  `1/2^10001` (scale 10001) can't be written. A short form can read beyond them (`1e10000` is `10^10000`), so reading
  JSON and writing it back can fail; writing and reading back cannot. An integer wider than 33220 bits (the width of
  `10^10000`) is refused before any conversion; no rational's numerator or denominator is that wide (§1), and a
  denominator `2^a 5^b` with `max(a, b) <= 10000` is at most `10^10000`, so its factoring is bounded too.
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
