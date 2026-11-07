# dilf4s canonical encoding and tagged hashing (v1)

Status: normative. Every dilf4s implementation (Scala reference, TS, Rust, Go, Java, Python) MUST produce identical bytes for identical values. The conformance vectors in `shared/vectors/canonical_encoding_v1.json` are part of this specification.
Refusals (their kinds, normative fields and the work allowed before them) follow `conformance.md`.

## 1. Data model

A canonical value is exactly one of:

| Kind | Domain |
|---|---|
| null | |
| boolean | `true`, `false` |
| integer | an integer at most `MaxIntegerBits` (33219 bits) wide |
| rational | `n/d` with `d ≥ 2`, `gcd(\|n\|, d) = 1`, and `n` and `d` each at most `MaxRationalBits` (33219 bits) wide |
| text | a sequence of Unicode scalar values (no surrogate code points) |
| bytes | a finite byte sequence |
| array | a finite sequence of canonical values |
| map | a finite set of `(text key, canonical value)` entries with unique keys and **no null values** |

The data model has no floating-point numbers, no NaN or infinity, no negative zero, no undefined, and no other tags.

**Absent equals null.** A map never contains a null value. Constructing a map drops entries whose value is null, so adding an optional field whose value is "none" leaves the encoding, and therefore every hash, unchanged. Nulls inside arrays are kept, because position is meaningful there.

**Integers and rationals are one numeric domain.** A number whose value is an integer is always the integer kind. The rational kind only ever holds non-integers.

**Number width.** Every integer, and a rational's numerator and its denominator, has a `bitLength` of at most
33219: **`MaxIntegerBits` = 33219** (`canonical-max-integer-bits` in `shared/schemas/limits.cddl`) and
**`MaxRationalBits` = 33219** (`canonical-max-rational-bits`), two names for one width. `bitLength` is Java's
`BigInteger.bitLength` (`jlvm.md` §2): for `n < 0` it is the bit length of `-n - 1`, so `2^33219 - 1` and `-2^33219` are
33219 bits wide and `2^33219` and `-2^33219 - 1` are 33220. 33219 is the widest bit length whose every integer has at
most 10000 digits (`2^33219 < 10^10000 < 2^33220`), the JSON digit bound (§3): so every integer renders as JSON and
reads back. `8e9999` is an integer and `9e9999` (33220 bits) is not; `10^10000` is not. Every JLVM number (4096 bits)
is far inside the width.

The width bounds work. A rational is valid only in lowest terms, and checking that takes a gcd, superlinear in its
operands: with the bound, decoding a rational costs at most one gcd of operands at most 33219 bits wide, and a rational
that wide takes over 8 KiB to encode, so decoding stays linear in its input; without it a rational of n bytes would cost
O(n²). And without it an integer could be as wide as its encoding allows, and everything that reads one (rendering it,
converting it to a decimal, arithmetic, typed decoding) would do work its producer chose.

The bound is a property of the value model, not only of decoding: **no canonical value holds an integer, or a rational
part, wider than 33219 bits**, so every value that can be built encodes to bytes every decoder accepts, and a value
committed or stored can always be read back. Every producer refuses a wider one, with kind `IntegerTooWide` or
`RationalTooWide` and normative field `limit` = 33219 (`conformance.md` §3). A number is checked after it is reduced
(`2^33220/6` is `2^33219/3`, still too wide; `2^33220/2^33219` is the integer 2; `2^33220/2` is the integer `2^33219`,
too wide):

| Producer | Refusal |
|---|---|
| CBOR decoding (§2) | `IntegerTooWide` for a tag 2 or 3 bignum, `RationalTooWide` for a rational's part, each from its length before it is built and before any gcd |
| building an integer (Scala `CanonicalValue.int` and `Canonical[BigInt]`, Rust `Value::integer` and `Int::new`, TypeScript `Value.int` and `IntValue.of`) | `IntegerTooWide` (Rust `IntegerTooWide`) |
| building a number from a numerator and denominator (Scala `CanonicalValue.number` and `Canonical[Ratio]`, Rust `Value::number`, TypeScript `Value.number`) | `IntegerTooWide` or `RationalTooWide` (Rust `NumberError`, beside `DivisionByZero`) |
| building a rational exactly as given (Rust `Rational::new`, TypeScript `RationalValue.of`) | none: no rational (`None`, `undefined`), as for one not in lowest terms |
| JSON (§3) | `IntegerTooWide` or `RationalTooWide`, once the number is read: within the digit and scale bounds an integer reaches about `10^20000` (`9…9e10000`) and a denominator `10^10000` (`0.0…01` at scale 10000) |
| typed data (codecs for an integer range) | the range first (`TypeMismatch`), then `IntegerTooWide` for a range wider than the bound |
| JLVM arithmetic (`jlvm.md` §2) | never reached: every number an operation reads or produces is within 4096 bits, or it is a `Limit` error |

Integers that fit a primitive type are built without a check, since every one fits (Scala `CInt(Long)`, Rust
`Value::int` and `From` for every primitive integer up to 128 bits). Each SDK's integer and rational types have no
other constructor, so no wider number can be made without one of these refusing it, and in Scala and Rust the compiler
rejects an unchecked integer.

TypeScript's types are checked only at compile time, so its SDK also checks at run time what a caller passes past
them (from JavaScript, or a cast), and refuses an argument of the wrong type with `TypeMismatch`, never a throw or a
value read as another type. Each argument is checked in O(1) before it is read:

- the constructors: `Value.int` and `IntValue.of`, `Value.bool` and `BoolValue.of`, `Value.bytes` and
  `BytesValue.of`, `Value.text` and `TextValue.of`, `Value.number` and `RationalValue.number`, and
  `RationalValue.decimal`, whose scale must be an integer. `Value.array` and `Value.map` check each item or entry in
  the one pass that reads it;
- decoding and parsing: `cbor.decode`, `codec.fromCbor`, `codec.fromJson`, `json.parse` (and its scale bound), and
  each codec's `decode`, `decodeWithin`, `fields` and `selectAlternative`;
- encoding, rendering and committing: `cbor.encode`, `cbor.encodeWithin`, `cbor.encodedLengthWithin`, each codec's
  `encode` and `codec.toCbor` (a hand-made codec that gives what is not a value is refused by its place),
  `json.renderWithin` and `json.renderInto` (its sink too), `valueCommitment`, `valueCommitmentWithin` and
  `hashCommitment` (its algorithm and prefix too);
- helpers: `json.decimal`, `cbor.magnitudeBytes`, `bitLength` and `utf8Length`;
- the hash API (§4): `digest`, `hashParts`, `startHash` and its hash's `update`, `outputSize`, `blockSize`,
  `expansionLimit`, `parseHashAlgorithm`, `digestFromBytes`, `digestFromHex`, `expandMessageXmd` (its output length
  too: a non-number is a `TypeMismatch`, a number out of range `outputLength`), `DomainTag.of`, `TaggedHash.of` and its
  `hash`, `content` and `contentWithin`, `taggedHash`, `contentHash`, `toHex` and `fromHex`. An algorithm that is not
  a string is a `TypeMismatch`; a string naming none is `UnknownAlgorithm`, as in every SDK.

Where a result has no room for a refusal, the answer is still defined and reads nothing of the argument: `equals` is
false when either side is not a value, `MapValue.get` finds nothing for a key that is not text, `decodeUtf8` gives
`undefined` for what is not bytes, and the diagnostics `inspect`, `describeFailure` and `Preview.of` give fixed
placeholders (`<not a canonical value>`, `<not a decode failure>`, and a preview with the head `<not text>` and a
length of 0).

The TypeScript proofs and signing packages (`@kd5ujc/dilf4s-proofs`, `@kd5ujc/dilf4s-signing`) hold every exported
function and method to the same rule: each argument is checked once, in O(1) (an array once per item, read by index),
and one of the wrong type is a `TypeMismatch` in the function's result, beside the function's own error family. A
verification never answers for such an argument: a message that is not bytes is refused, never read as empty, and a
claimed value that is not a canonical value (an object shaped like the null value included) is never a claim of
absence. A key, signature, proof, profile, mode or index must be one the package made (a private brand, stamped only
by a constructor given the package's token, so one built by calling a constructor from JavaScript fails the check)
and, for keys and signatures, of the scheme at hand. Such a built instance keeps none of its arguments (every field
is a fixed default, so it never reports a caller's algorithm or scheme), and each of its methods with room for a
refusal refuses it. A plain object a caller passes (a Merkle or versioned head, a
proof, delta, chunk or snapshot) has each field read once into a copy the package owns. The query of a listing check
(a prefix, or a range's bounds; for a versioned trie, the version asked about or a range's versions, encoded once)
is copied before the claim or the proof is read, so their getters cannot change what is asked. Where a result has no room for a refusal, `equals` is false for anything not of its class. A message to sign is copied once before it is read, since
a signer reads it more than once and shared memory cannot be told apart reliably.

A revoked Proxy (or a Proxy whose target is one) is refused like any other argument of the wrong type, as a
`TypeMismatch`, in all three packages: every array and object reader first asks `Array.isArray` through core's one
guard, whose catch sees only the engine's refusal of such an object (the call runs no caller code), and a function a
caller passes (an iterator's methods, a version key's, a source of randomness) is checked the same way each time it is
called, since caller code may revoke it meanwhile. For the same reason every read of a caller's object or array after
its first goes through the guard too (a getter or trap an earlier read ran may have revoked it), and what such a read
gives is refused, never kept or taken for an absent field. It and signing's
boundary around noble are the SDK's only catches. A check that passes allocates at most one O(1) plain view of each
caller byte argument (the view is what keeps a library such as noble from reading a subclass's own `length` or
`subarray`) and the copies of what is kept; refusals and boolean verdicts are shared, frozen results.

Caller bytes are read through the typed array's internal slots, so a subclass that overrides `length`, its accessors
or its iterator is not believed. `cbor.decode` reads its input in place when its memory is an `ArrayBuffer` of the
caller's realm, which nothing can change while the decoder runs, and copies it once first when it is a
`SharedArrayBuffer` (another thread may write it) or another realm's; each decoded byte string is copied once into its
value, which holds no view of the input. Scala's and Rust's types rule these arguments out, so this refusal is the
TypeScript SDK's alone and has no conformance vector.

For other-language implementers, here is the same model in CDDL (RFC 8610). This block is **informative**: it is not
in `shared/schemas`, because tags and recursion are outside the code generator's profile. The prose above, and the
deterministic encoding rules in §2, are normative.

```text
canonical-value = null / bool / int / rational / tstr / bstr
                / [* canonical-value]
                / { * tstr => canonical-value }        ; unique keys, no null values
rational = #6.30([numerator: int, denominator: uint])  ; denominator >= 2, gcd(|n|, d) = 1,
                                                       ; bitLength of each <= 33219 (MaxRationalBits)
                                                       ; every int: bitLength <= 33219 (MaxIntegerBits)
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
- **Bignums** (tags 2 and 3) are checked in this order, and the first failure is the refusal: the item the tag wraps
  has a well-formed head (its own refusal: `UnexpectedEnd` when truncated, `Unsupported` for a float), of a byte
  string (`NonCanonical`), present in full (`UnexpectedEnd`), non-empty with no leading zero byte and longer than 8
  bytes (`NonCanonical`), and at most `MaxIntegerBits` wide (`IntegerTooWide`, §1). The width is read from the length
  alone, before the integer is built: Java's `bitLength` of `m` and of `-1 - m` are both `m`'s, so either tag's width
  is `8 (len - 1)` plus the bit length of the first byte, and a bignum of 4153 bytes is within the bound exactly when its
  first byte is below `0x08`. A rational's part is checked the same way, against `MaxRationalBits` (`RationalTooWide`).
- **Rationals** are checked in this order, and the first failure is the refusal: tag 30 wraps a 2-element array
  (`NonCanonical`); the numerator is a canonical integer (`NonCanonical`) at most `MaxRationalBits` wide
  (`RationalTooWide`, §1); the same for the denominator; the denominator is at least 2
  (`NonCanonical`); and last, `gcd(|n|, d) = 1` (`NonCanonical`). A part's width is checked as soon as it is read, from
  its length before it is built, so a part too wide is refused before the next is read and before any gcd, and a
  decoder runs at most one gcd per rational.
- **Refusal order.** A decoder reads the encoding once, item by item in encoding order (a container's items before
  what follows it), and the refusal is the first fault it meets; the kinds are `CanonicalError`'s
  (`docs/specs/conformance.md` §1). For each item, in order:
  1. its head: the initial byte present (`UnexpectedEnd`); major type 7 with additional information 24 to 31 (a float,
     or a simple value with a one-byte argument) `Unsupported`; additional information 24 to 27 with its argument
     bytes present (`UnexpectedEnd`) and in shortest form (`NonCanonical`); 28 to 30 (reserved) and 31 (indefinite
     length) `Unsupported`;
  2. by its major type: a simple value other than false, true and null `Unsupported`; a byte or text string present in
     full (`UnexpectedEnd`), and text well-formed UTF-8 (`InvalidUtf8`); an array or map whose count fits the bytes
     left at one byte per array element and two per map entry (`UnexpectedEnd`, before anything is allocated), then the depth limit
     (`DepthExceeded`, an empty array or map included); a tag other than 2, 3 and 30 `Unsupported`, and tags 2, 3 and
     30 in the bignum and rational orders above;
  3. in a map, each key once it is read whole: text (`Unsupported`), after the previous key in encoded order
     (`NonCanonical`); then each value once it is read whole: not null (`NonCanonical`).

  After the top-level item, any byte left is `TrailingBytes`. `canonical_encoding_v1.json`'s `cborRejections` pin
  each kind and each pair of faults that an order could otherwise answer differently.
- **Encoding within a limit.** `encodeWithin(v, maxBytes)` is the encoding when it is at most `maxBytes` bytes long,
  and otherwise refuses with `TooLarge` and the limit as applied (`conformance.md` §3, §4): a negative limit is 0, and
  a limit past an implementation ceiling is that ceiling. Nothing past the limit is written first. No encoding is
  shorter than its value's weight (`jlvm.md` §2), so a weight over the limit is refused at once, in O(1); otherwise the
  work is O(min(weight, limit)). `encodedLengthWithin` measures the same length, with the same refusal, without
  encoding. An SDK that builds an encoding in one array encodes within its ceiling, and its `encode` is
  `encodeWithin` at it: 2^31 - 9 bytes (`MaxEncodedBytes`, the JVM's largest array) in the reference and the
  TypeScript SDK (`conformance.md` §4). `canonical_encoding_v1.json`'s `limits` pin an encoding at its length and one
  byte under it, refused from the weight and refused as measured.

## 3. JSON mapping

JSON is the human-facing and API format. It converts to the data model as follows.

**Numbers** are read **exactly from their source text** as a terminating decimal and converted to an integer or a rational. For example, `0.1` becomes 1/10, `1.0` becomes the integer 1, `-0` becomes the integer 0, and `9007199254740993` stays exact.
- Implementations MUST NOT route numbers through IEEE-754 doubles.
- A decimal whose effective scale `|fraction digits - exponent|` exceeds 10000 is rejected, to bound memory.
- A number's significand (its integer and fraction digits together, leading zeros aside) and its exponent (leading
  zeros aside) each have at most 10000 digits; a longer one is rejected. Decimal conversion costs more than linear time
  in the digits, so both are counted on the source text before any conversion. The bound equals the scale bound, so
  every decimal of an accepted scale has a form within it (`0.` then 10000 zeros then `2e1` is 2/10^10000).
- A number past the data model's width is refused once it is read (`IntegerTooWide`, `RationalTooWide`, §1), after
  the digit and scale bounds, which bound the work but not the value: `9e9999` and 10000 nines are too wide an
  integer, and `0.` then 9999 zeros then `1` (1/10^10000) too wide a denominator, while `0.` then 9999 zeros then `2`
  (1/(2^9999 5^10000)) is within it; `1e10001` is past the scale bound (`InvalidNumber`).

**Strings** become text. A string containing unpaired surrogates is rejected.

**Objects** become maps, with null-valued members dropped. Duplicate member names are rejected.

**Arrays** become arrays, and `null`, `true` and `false` map to themselves. The data model's bytes kind has no JSON form, so it only arises from typed data.

**Text, not bytes.** The parser reads text. Whitespace is RFC 8259's four characters only: space, tab, line feed and
carriage return. Anything else between tokens is a syntax error (`JsonSyntax`): a byte-order mark U+FEFF (at the
start too: an SDK that turns bytes into text for the parser must keep a leading U+FEFF, as `TextDecoder` by default
does not), the other Unicode spaces such as U+00A0, and the controls U+000B and U+000C.

**Refusal order.** The parser reads the text once, left to right, and the refusal is the first fault it meets, with
one exception, a member name's surrogates (the last item):

1. syntax (`JsonSyntax`): a character that cannot start or continue a value where it stands, an unterminated string,
   an unescaped U+0000 to U+001F in a string, an unknown or truncated escape, or anything after the top-level value;
2. depth (`DepthExceeded`): a `[` or `{` opened while 256 containers are open, empty or not, is refused at its bracket,
   before anything after it is read (`[` × 256 then `{}` is refused, `[` × 255 then `{}` accepted), as the CBOR
   decoder counts depth (§2);
3. a number, once its text `-? digits (. digits)? ([eE] [+-]? digits)?` is read: malformed (`01`, `1.`, `-`:
   `InvalidNumber`), then its significand's digits, its exponent's digits and its scale (each `InvalidNumber`), then
   its value's width (`IntegerTooWide`, `RationalTooWide`);
4. a string value's unpaired surrogate (`UnpairedSurrogate`), at its closing quote;
5. a member name repeated in its object (`DuplicateKey`), as soon as the repeat is read, before its `:`. Names are
   compared as the text their escapes decode to, unpaired surrogates included (`"a"` repeats `"a"`);
6. a member name's unpaired surrogate (`UnpairedSurrogate`), only when its object closes: after every value in the
   object and every repeat of a name in it have been read. So a repeated name with an unpaired surrogate is
   `DuplicateKey`, with its preview (`docs/specs/conformance.md` §8), and an object closes before what follows it is
   read: `{"a":{"\udc00":1},"a":2}` is `UnpairedSurrogate`.

So `{"\ud800":1,"\ud800":2}` is `DuplicateKey`, `{"\udc00":01}` `InvalidNumber`, `{"\udc00":1` `JsonSyntax`, and
`{"\udc00":` then 300 nested arrays `DepthExceeded`. `canonical_encoding_v1.json` pins each of them, and each pair of
faults that an order could otherwise answer differently.

Converting back to JSON (for display only) works as follows:
- Output is compact (no whitespace), and object members are written in ascending Unicode code-point order of their names.
- Integers are written as exact decimal digits, always: every integer has at most 10000 digits (§1), so it reads back.
- A rational is written as an exact decimal when it terminates. Otherwise it can't be written as JSON and conversion fails.
- A rational is written only when its decimal is within the bounds above (at most 10000 significant digits, and a scale
  of at most 10000), so that every rendering reads back; otherwise conversion fails. `1/2^10001` (scale 10001) and
  `(2^33219 - 1)/2` (10001 significant digits) can't be written. Writing and reading back never fails, and neither does
  reading and writing back: every number JSON reads is within the width (§1), and a decimal read within the digit and
  scale bounds is written within them.
- A rational's scale is found from its denominator's factors before any digit is produced: the twos are its low zero
  bits, and the power of five is estimated from the bit length and checked by exponentiation, at most four big-number
  divisions and exponentiations whatever the denominator, never one division per factor. A scale past the bound
  (`1/2^33218`, `1/5^14306`) is refused then.
- Bytes can't be written as JSON, and conversion fails.
- **Rendering within a limit.** `renderWithin(v, maxBytes)` is the rendering when it is at most `maxBytes` UTF-8 bytes
  long. It writes once, in order, and refuses at the first fault it meets: a part that would pass the limit is
  `TooLarge` with the limit as applied (a negative limit is 0, and none is above `MaxRenderBytes`), and what JSON cannot
  write is its `CanonicalError` kind (`NotJsonRepresentable`), so `[1, h'01']` is `TooLarge` within 4 bytes and
  `NotJsonRepresentable` within 5. A rendering is never shorter than its value's weight, so a part whose weight is over
  what is left is refused at once. `MaxRenderBytes` is 2^29 - 24 bytes: V8's longest string is 2^29 - 24 UTF-16
  units, and a rendering has no more units than UTF-8 bytes. The reference's `render` is `renderWithin` at it
  (`conformance.md` §4). `canonical_encoding_v1.json`'s `limits` pin a rendering at its length and one byte under it,
  an escaped text, and both orders of the first fault.

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
- `encode(value)` is streamed into the hash as it is written, never built whole, and it is bounded as a built
  encoding is: an SDK with an encoding ceiling refuses a content hash past it as `TooLarge` (§2, `conformance.md` §4),
  before the hash absorbs a byte.

Every structure and message type has its own tag, so a digest produced in one context can never be valid in another. The tag ends with a version, so a format change takes a new tag rather than silently changing bytes.

This is the BIP-340 tagged-hash construction, extended from SHA-256 to any `H`.

**Names, digests and hex.** Each SDK takes a hash function by the name above (`SHA-256`, `BLAKE2b-256`, exactly as
written); any other name is refused as `UnknownAlgorithm`. A digest of `H` is exactly `H`'s output size in bytes
(`InvalidLength` otherwise). Hex, as the vectors and the hash API read it, is ASCII digits and `a`-`f` in either case,
two per byte, nothing else (no whitespace, separators or other scripts' digits); a digest's hex may start with `0x`
(lowercase `x`). Hex is refused as `InvalidHex` at the first character that is not a hex digit, or else, for an odd
number of digits, at the text's length; the offset counts any `0x`. A digest's hex with more digits than twice the
output size is `InvalidLength`, refused reading at most one digit past that width (so a non-digit within it is
`InvalidHex` first), before the text is decoded.

**`expand_message_xmd`.** RFC 9380 §5.3.1 over any `H` above, its `s_in_bytes` the input block size (the rate of a
sponge: SHA-256 64, SHA-512 128, SHA3-256 and Keccak-256 136, SHA3-512 72, BLAKE2b 128). A DST over 255 bytes is
first reduced to `H("H2C-OVERSIZE-DST-" || DST)` (§5.3.3). It is refused as `InvalidExpansion`, before anything is
hashed, for an empty DST (`emptyDst`), then for a length outside 1 to `min(255 * output size, 65535)`
(`outputLength`).

Every refusal of this section is a kind of the generated hash family (`shared/schemas/hash-errors.cddl`,
`conformance.md` §6); the vectors pin each, with the outcome of every name, digest hex and expansion row.
