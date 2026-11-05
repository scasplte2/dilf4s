# dilf4s JLVM (language v1, gas schedule v1)

Status: normative. Every operation of language version 1 is specified and available.
Conformance vectors: `shared/vectors/jlvm_v1.json` (schema `shared/schemas/jlvm-vectors.cddl`). Each case is a program
and its data, with the outcome (a value's canonical CBOR, or an error kind) and the gas used under schedule v1. Most
are metakit's own test programs, rewritten into this surface and stored as its text. They agree with metakit's
results, except where §8 says otherwise.
Refusals (their kinds, normative fields and the work allowed before them) follow `conformance.md`.

The JLVM is a strict, metered expression language for ledgers, written in JsonLogic syntax. A result is a function of
exactly five things:

- the program;
- the input data;
- the host bindings;
- the language version;
- the gas-schedule version.

## 1. Programs

**Surface.** Programs are written as JSON. The text must first pass `CanonicalJson.parse`
(`canonical-encoding.md` §3), which bounds depth, refuses a repeated member name and requires exact numbers. It is
the only JSON parser that reads a program: the accepted text is then split into its structure, with null members and
member order still visible, and each scalar, member name and literal is read by `CanonicalJson.parse` from its own
span of the text. Where a string or an integer is looked for (an array's first element, a `var` path), only a
string's or a number's span is read, so reading costs at most twice the text however deeply it nests. Then:

- **Objects.**
  - A single-key object whose key is an operation of this version is that operation. Its value gives the arguments:
    - an array is the argument list, and never spreads;
    - for a **variadic** operation (`+`, `*`, `min`, `max`, `cat`, `merge`), any other value is an expression whose
      value must be an array, and its elements are the arguments: `{"+": {"values": x}}` sums the values of `x`.
      This is decided by the syntax alone. A literal that is not an array, or an object expression, does not compile;
      any other non-array value is a `Type` error;
    - for any other operation, a non-array value is the single argument.
  - Three keywords are not operations:
    - `{"literal": x}` quotes `x`, which is not evaluated;
    - `{"object": {...}}` is a map expression with any keys, including one key that names an operation;
    - `{"var": p}` and `{"var": [p, default]}` read a variable (§4.1).
  - A single key starting with `_` is a map expression. No operation's name starts with `_`; the prefix is the
    namespace of host triggers (`_result`, `_emit`, `_spawn`).
  - Any other single-key object is a parse error.
  - Any other object is a map expression, whose fields are evaluated.
- **Aliases.** `getKey`, `setKey`, `deleteKey` and `size` are accepted for `get`, `set`, `unset` and `length`. The
  canonical form records the operation's own name, so an alias never changes a program's identity.
- **Arrays** are evaluated element-wise. There are no array-form operations.
  - An array whose first element is text naming an operation, or `var`, is a parse error, so text shaped like a call
    is never read as data by accident. The error names the call form, `{"+": [...]}`.
  - Data of that shape is quoted, `{"literal": ["+", 1, 2]}`, or given a non-text first element, as in
    `[{"literal": "+"}, {"var": "x"}]`. Rendering quotes such a first element itself.
  - The segments array of a computed `var` path (§4.1) is exempt, since it is never code.
- **JSON `null`** is the null literal.
- **Numbers**, as literals and as `var` paths, are read as canonical JSON numbers (`canonical-encoding.md` §3): at
  most 10,000 significant digits and a scale of at most 10,000, both checked on the text before any conversion.
- **Strings** of the form `0x` followed by an even number of lowercase hex digits are byte strings (§2). Every other
  string is text.

Some names read like operations but are not, and are parse errors whose message names the operation to use:
- `prove_dlog_verify` and `prove_dhtuple_verify`: a one-leaf `sigma_verify` (`sigma.md`);
- `mpt_verify` and `smt_verify`: `trie_verify`;
- `mpt_prefix_verify`: `trie_prefix_verify`;
- `noop`: nothing; omit it.

**Rendering** (`Parser.render`) writes a program's surface text, emitting `{"object": ...}` for each single-key map
expression. It is exact: parsing the rendered text gives the same program. A literal holding `0x`-hex text, or a
non-terminating rational, has no surface form and is an error.

**Canonical form and identity.**
- A program is the tagged syntax tree of `shared/schemas/jlvm.cddl`, not its JSON text.
- Its identity is `taggedHash_H("dilf4s/jlvm/program/v1", cbor(program))` (`canonical-encoding.md` §4).
- **One encoding per meaning.** Decoding a program's canonical form gives that program back, so programs that behave
  differently never share an identity. A syntax tree is therefore a program only if:
  - each `var` path is what its text reads back as. A data path segment holding `.` or empty, a data path whose first
    segment starts with `$`, or a binding name or segment holding `.` or empty has no text form; such keys are read
    with the exact-segment form `{"var": [[...]]}`;
  - no default is null. Canonical maps drop null, so a null default would encode as no default, which differs for
    an unbound name (§4.1). The parser reads `{"var": [p, null]}` as no default.
- **Depth.** A program's canonical form must fit the value depth bound of `canonical-encoding.md`, 256 levels, and
  so must the JSON text `Parser.render` writes for it, as `CanonicalJson.parse` counts depth. So every program has an
  identity and an exact rendering. An operation takes two levels (its map and its argument array), so at most 127
  operations nest, and fewer around deep literals and defaults.
  - Text, structure or canonical form too deep is `Limit`, however the bound is reached.
  - metakit counted evaluated nodes at run time, so an untaken deep branch never counted. Here the program is bounded
    before it runs (§8 #22).
- **Size.** Compilation uses no gas, so a program's size is bounded before anything else is done: at most 2^18
  (`Program.MaxSize`), counting one per expression, plus the weight of each literal and default, plus the length of
  each object key, binding name and path segment, plus one per segment. A larger program is `Limit`. `let` carries its
  scope forward in one pass, in a sorted set of names. Checking the canonical form builds sorted maps of literal keys,
  compared and never hashed, so compilation is O(n log n) rather than linear in this size. The largest conformance
  program is under 2^14. The host bounds the JSON text itself.

**Compilation.** Before evaluation, a program is checked into its evaluation form. These are parse errors
(`Malformed`), reported with no gas used:
- the shape of `if`, `let` and every callback form;
- binding names, and a plain path naming a `let` binding in scope;
- an array headed by an operation's name (above);
- a name that reads like an operation but is not (above);
- duplicate bindings;
- a spread whose argument can never be an array.

The canonical form also rejects a spread of a non-variadic operation, and a spread whose argument is an array
expression, so that every program has exactly one surface form.

## 2. Values

**The value domain.** Values are the canonical data model (`canonical-encoding.md` §1):
- null, booleans and numbers;
- well-formed text and byte strings;
- arrays;
- text-keyed maps, which never hold null (setting a key to null removes it).

There is one number kind: exact integers and exact rationals, so `2.0` *is* `2`.

**Weight.** A value's weight is a portable measure of its size, exactly:

| Value | Weight |
|---|---|
| null, boolean | 1 |
| integer `n` | `1 + ⌊bitLength(n)/64⌋` |
| rational `n/d` | `2 + ⌊bitLength(n)/64⌋ + ⌊bitLength(d)/64⌋` |
| text | `1 +` its length in UTF-8 bytes (not UTF-16 units: a surrogate pair is 4) |
| byte string | `1 +` its length in bytes |
| array | `1 + Σ` the weights of its elements |
| map | `1 + Σ (1 + key length + weight of the value)` over its entries, the key's length in UTF-8 bytes |

- `bitLength` is Java's `BigInteger.bitLength`: the bits of the minimal two's-complement form, without the sign bit.
  For `n >= 0` that is the bit length of `n`; for `n < 0` it is the bit length of `-n - 1`, so `-2^63` has 63 bits
  (weight 1) and `-2^63 - 1` has 64 (weight 2).
- Shared sub-values count every time they occur, so the weight is the size of the value written out in full, and it
  bounds the work of any full traversal.
- The total saturates at `2^63 - 1`.

A value's canonical CBOR (`canonical-encoding.md`) is never shorter than its weight: every node has a head of at least
one byte, text and byte strings add their length, and an integer takes at least `1 + ⌊bitLength/64⌋` bytes.

**Bounds**, checked on every value an operation reads or produces:

| Bound | Value | Applies to | Error |
|---|---|---|---|
| Number size | 4096 bits | numerator and denominator | `Limit` |
| Result weight | 2^24 | results | `Limit` |
| Depth | 256 | values | `Limit` |

**Truthiness.** The falsy values are null, `false`, `0`, `""`, empty bytes, `[]` and `{}`. Every other value is truthy,
including every non-integer rational.

**Kinds** (`typeof`): `null`, `boolean`, `number`, `string`, `bytes`, `array`, `object`.

**Equality.**
- `==`, `===`, `!=` and `!==` are structural equality of canonical values.
- There is no coercion: `"1" == 1` is false, and `null !== null` is false.

**Ordering.**
- `<`, `<=`, `>` and `>=` compare a number with a number exactly, and text with text by Unicode code point (which is
  the order of the UTF-8 bytes).
- Anything else is a `Type` error.
- `<` and `<=` also take three arguments: `a < b < c`.

**Text.**
- `length` and `substr` count code points.
- `lower` and `upper` map **ASCII letters only**: `A`–`Z` to `a`–`z`, and back. Every other code point is unchanged,
  so the result has the same length, and no Unicode case table or version is involved: every implementation agrees.
  Text is still any Unicode, and no normalisation is applied, so `é` (one code point) and `e` + U+0301 stay distinct.
  Unicode case mapping is not portable: language runtimes ship different Unicode versions, and Final_Sigma needs
  context that runtimes decide differently.
- `trim` removes the `White_Space` code points from both ends: U+0009–000D, 0020, 0085, 00A0, 1680, 2000–200A, 2028,
  2029, 202F, 205F, 3000.

**Rendering numbers** (`cat`, `join`): integers in decimal, and terminating rationals as exact plain decimals
(`-2.5`). A non-terminating rational (`1/3`) is an `Arithmetic` error.

## 3. Errors

An evaluation ends with a value or one of the following errors. **Either way it reports the gas it used** (§7).

| Error | Meaning |
|---|---|
| `Malformed` | Not a valid program, or a `$name` that is not bound and has no default. |
| `Type` | An argument of the wrong kind. |
| `Arity` | The wrong number of arguments. |
| `Arithmetic` | Division by zero, a bad exponent, or a non-terminating rendering. |
| `Domain` | The right kind but out of domain: malformed hex, an empty separator, a negative width. |
| `Crypto` | Malformed cryptographic input. A well-formed but invalid proof is `false`, not an error. |
| `Limit` | A bound of §2. |
| `OutOfGas(required, available)` | The gas limit was reached. |

## 4. Evaluation

Arguments are evaluated left to right, depth first. Only the forms below are lazy.

### 4.1 Variables

**Defaults.** A default is a literal value and is never evaluated. An object or array default must be quoted,
`{"var": ["x", {"literal": {...}}]}`, because unquoted it could be mistaken for code.

**Data paths.**
- `{"var": "a.b.0"}` reads the input data.
- Each segment is an object key, or an array index written in decimal without leading zeros.
- `""` is the whole data.
- An absent value is null, and then the default if one is given.

**Inside a callback** (decision C1), a plain path first looks in the enclosing callbacks' frames, innermost first,
then in the data:
- each callback contributes a frame. In `reduce` the frame binds `current` and `accumulator`. In every other
  callback, the frame is the element, and binds that element's keys when it is an object;
- the path is read from the innermost frame that binds its first segment; otherwise from the data;
- `""` is the innermost element (`current` in `reduce`);
- arrays are never concatenated, and a primitive element is reachable only as `""` or `$current`.

This is metakit's merged scope, without its silent collisions:
- when the frame that resolved a path is not the only one that binds its first segment (an outer frame or the data
  binds it too), the evaluation records an `AmbiguousPath` warning;
- warnings never change the result or the gas. At most 64 distinct ones are kept;
- finding ambiguity means every frame is searched for the first segment, so a read pays that segment's count once
  per enclosing frame (§7);
- `$current`, `$index` and `$accumulator` read the same values with no ambiguity.

**Computed paths.** In `{"var": e}` and `{"var": [e, default]}`, an object or array `e` is an expression computing the
path:
- if it evaluates to text, that text is read as a `var` path, `$` included. A malformed path, such as an empty
  segment, is a `Domain` error;
- if it evaluates to an array, its items are the exact segments of a data path: text is a key and may contain `.`; an
  integer is an index (on an object, the key that is its decimal form, so `-1` reads `"-1"`). An integer segment must
  fit in 64 bits, or it is a `Domain` error: it is checked before it is rendered. A key can therefore never redirect
  the read into another field, and the array form never reads a binding;
- any other value is a `Type` error;
- `{"var": [["a", {"var": "k"}]]}` is the array form. A literal path is quoted: `{"var": {"literal": "a.b"}}`.
- Rendering writes an object path with no default bare, `{"var": {"cat": [...]}}`, and an array path or any default
  as the argument list, so a computed path adds one JSON level, as in the canonical form.

**Bound paths.**
- `{"var": "$name.path"}` reads a binding.
- A binding is one of:
  - a host binding, supplied with the evaluation (e.g. `$ordinal`);
  - a `let` name;
  - a callback name: `$current`, `$index` or `$accumulator`.
- Inner bindings shadow outer ones.
- An unbound name is `Malformed`, unless a default is given.
- The host supplies its context this way (decision B): for example, the ottochain adapter binds `ordinal`,
  `epochProgress` and `lastSnapshotHash`, rather than putting them in the data.
- Host bindings must be identical on every node that evaluates the program: consensus context such as the snapshot
  ordinal, never wall-clock time or any other per-node value. Deadlines are counted in ordinals; a time a caller
  asserts is part of the input data.

### 4.2 Lazy forms

**`if`** takes `[c1, t1, (c2, t2)..., else]`, or the same without `else`: at least 2 arguments. The result is the
first `ti` whose `ci` is truthy, otherwise `else`, or null when there is no `else` (standard JsonLogic). Only the taken
branch is evaluated.

**`and`, `or`, `default`** evaluate their arguments left to right and stop early:

| Form | Stops at | If none decides | With no arguments |
|---|---|---|---|
| `and` | the first falsy value, which it returns | the last value | `true` |
| `or` | the first truthy value, which it returns | the last value | `false` |
| `default` | the first truthy value, which it returns | `null` (metakit) | `null` |

**`let`** takes `[[[name, expr], ...], body]`, or `[{name: expr, ...}, body]` with the bindings in code-point order of
the names.
- Each binding sees the earlier ones, and the body sees all of them.
- A name is non-empty, has no `.`, and has no leading `$`.
- A name may not be bound twice in one `let`.
- A binding is read only as `$name`.
  - A plain path whose first segment names a binding in scope is a parse error, so `{"var": "x"}` never reads the
    data where the binding `x` was meant.
  - The data's `x` is read with the exact-segment form `{"var": [["x"]]}`.
  - A callback element's `x` is read as `$current.x`.

### 4.3 Callbacks

**Collections.** The collection is evaluated first. An array iterates. Null is an empty collection (metakit). Anything
else is a `Type` error.

**Bindings.** The body runs once per element, with `$current` (the element) and `$index` bound. Plain paths also see the
element (§4.1).

**Forms.**
- `map`
- `filter`
- `all`: false on an empty array; stops at the first falsy body.
- `none`: stops at the first truthy body.
- `find`: the first element whose body is truthy, else null; stops there.
- `count [array, body]`: the number of truthy bodies.
- `some [array, body]` or `some [array, body, min]`: at least `min` truthy bodies (default 1); stops as soon as `min`
  is reached.
- `reduce [array, body, initial]`: also binds `$accumulator`; the body's value is the next accumulator. `initial` is
  evaluated after the array.
- `reduce [array, body]`: the first element is the initial accumulator, and the body runs from the second (`$index`
  1). An empty array gives null (metakit).

**Syntactic forms.** Whether `count` is a callback is decided by syntax: `count [array]` is the length.

### 4.4 Lookups

`missing [keys...]` (or `missing [[keys...]]`) returns the keys whose data path is absent.

`missing_some [min, [keys]]` returns `[]` if at least `min` keys are present, and otherwise the missing keys.

Their paths resolve as plain `var` paths do: inside a callback the element comes first (§4.1). They never read
bindings.

## 5. Strict operations

Every argument is evaluated, then the operation is applied. There is no coercion, and no unwrapping of an evaluated
array argument; only the syntax spreads (§1).

**The one exception: absence.** Where metakit treated null as "absent", that behaviour is kept:
- `in`: a null needle gives `false`;
- `intersect`: a null subset gives `true`, and a null superset gives `false`;
- `startsWith` and `endsWith`: a null string or a null affix gives `false`;
- `keys`, `values` and `entries` of null give null;
- `cat` and `join` render null as empty.

### 5.1 Logic, comparison and arithmetic

| Operation | Semantics |
|---|---|
| `!`, `!!` | Negated truthiness, truthiness. |
| `==` `===` `!=` `!==` | Structural equality (§2). |
| `<` `<=` `>` `>=` | Ordering (§2). |
| `+` `*` | Exact sum or product. Of no arguments, 0 or 1, so a spread over an empty array is defined. |
| `min` `max` | Exact minimum or maximum, of at least 1 argument. |
| `-` | Negation (1 argument) or difference (2). |
| `/` | Exact division. Zero is an `Arithmetic` error. |
| `%` | Truncating remainder: `x - y·trunc(x/y)`, with the sign of `x`. |
| `abs` | Absolute value. |
| `round` | Half to even. |
| `round_half_up` | Half away from zero. |
| `floor` `ceil` | Floor and ceiling. |
| `pow` | Integer exponent in `-999..999`. The result size is checked **before** computing it. |

### 5.2 Text

| Operation | Semantics |
|---|---|
| `cat` | Concatenates strings, numbers (§2) and booleans; null is empty. When **every** argument is an array, it concatenates the arrays instead; when every argument is bytes, the bytes (as Ethereum's `abi.encodePacked` does for byte strings). |
| `substr [s, start]`, `substr [s, start, len]` | Counted in code points. A negative `start` counts from the end. A negative `len` drops that many from the end. |
| `lower` `upper` `trim` | §2. |
| `startsWith` `endsWith` | Prefix or suffix test. |
| `join [array, sep]` | The pieces as rendered by `cat`, joined by `sep`. |
| `split [s, sep]` | Every piece between separators, empty ones included. An empty `sep` is a `Domain` error. |
| `length` | Code points of text, elements of an array, bytes of a byte string, entries of an object. |

### 5.3 Arrays

| Operation | Semantics |
|---|---|
| `merge` | All objects: merged left to right, the last key wins. Otherwise array arguments are concatenated and other values appended. |
| `in [needle, haystack]` | Substring, byte substring or array membership. The search is linear-time. |
| `intersect [a, b]` | Whether every element of `a` is in `b`. |
| `unique` | First occurrences, in order. |
| `reverse` | Reverses the array. |
| `flatten` | Flattens one level. |
| `slice [a, start]`, `slice [a, start, end]` | JavaScript bounds. |
| `count [array]` | The length. |
| `exists` | Whether no argument is null. |

### 5.4 Objects

| Operation | Semantics |
|---|---|
| `keys` `values` `entries` | In code-point order of the keys. |
| `get [o, k]`, `get [o, k, default]` | The value at `k`, else null or the default. |
| `has [o, k]` | Whether `k` is present. |
| `set [o, k, v]` | `o` with `k` set to `v` (null removes it). |
| `unset [o, k]` | `o` without `k`. |
| `typeof` | §2. |

### 5.5 Bytes and hashing

| Operation | Semantics |
|---|---|
| `hex` | Bytes to `0x` text. |
| `unhex` | Bytes, or `0x` lowercase hex text, to bytes. |
| `hex_to_int` | The same input as `unhex`, read as an unsigned big-endian integer. |
| `bytes_to_int` | Bytes, read as an unsigned big-endian integer. |
| `int_to_bytes [n]`, `int_to_bytes [n, width]` | Unsigned, big-endian, minimal (`0` is empty), or left-padded to `width` (`0..1024`). |
| `sha256` `sha3_256` `blake2b256` `keccak256` | The 32-byte digest of bytes, or of the UTF-8 encoding of text. `keccak256` is Ethereum's Keccak-256, with the original padding, not SHA3-256's. |

**Byte strings are flat.** Every byte string an operation produces is one contiguous array, so reading a byte costs the
same however the string was built. `cat` copies its arguments into a fresh array. Its size charge, 1 per unit of each
argument's weight, already pays for every byte copied, and the result's weight is checked before the copy. This matters
on the JVM, whose byte strings (scodec `ByteVector`) are otherwise ropes: a loop that prepends would build a rope about
`sqrt(2 × gas)` levels deep, and each later byte read would walk, and recurse through, every level. Hosts pass flat
byte strings in data and bindings, as decoding a flat buffer gives.

## 6. Cryptographic operations

The backends are loaded once, at the edge (`Jlvm.load`). Every node therefore evaluates with the same complete set:
no operation's availability depends on the classpath.

All inputs are bytes. Malformed input is a `Crypto` error; a well-formed signature that does not verify is `false`.

| Operation | Arguments | Scheme |
|---|---|---|
| `ed25519_verify` | `[key32, message, sig64]` | Ed25519 under ZIP-215 rules (`signing.md`) |
| `secp256k1_verify` | `[key33, message, sig64]` | ECDSA over SHA-256 of the message; compact `r‖s`, low-S |
| `bip340_verify` | `[key32, message32, sig64]` | Standard BIP-340. The message must be exactly 32 bytes |
| `bls_verify` | `[key48, message, sig96]` | BLS12-381, minimal-pubkey-size, proof-of-possession ciphersuite |
| `bls_aggregate_verify` | `[[key48, ...], message, sig96]` | Fast aggregate verification: one message, every key |
| `ecrecover` | `[digest32, sig65]` | Ethereum's signer recovery. `sig65` is `r ‖ s ‖ v`, with `v` in {27, 28} or {0, 1}. The result is the signer's 20-byte address. A signature that recovers no key is a `Crypto` error, never a value: Ethereum's precompile returns the zero address, and a null would likewise equal an unset owner, so a comparison would accept any signature. It recovers no key when `r` or `s` is zero or not below the order, `s` is above half the order (EIP-2's twin signature: OpenZeppelin's `ECDSA.recover` rule, stricter than the precompile), or `r` is not an x-coordinate. Other sizes, or another `v`, are malformed. |

`bls_aggregate_verify` assumes every key carries a proof of possession, checked when the key was admitted. Without
that check, a rogue key can forge an aggregate.

The BN254 family follows metakit's byte formats. G1 points are 64 bytes `x‖y`, and `(0,0)` is the point at infinity.
G2 points are 128 bytes, imaginary part first (EVM order). Scalars and field elements are 32 bytes and must be
canonical (less than the modulus).

| Operation | Arguments | Result |
|---|---|---|
| `poseidon` | 1–4 field elements, as separate arguments or one array | 32 bytes |
| `pmt_verify` | `[root, leaf, index, [sibling, ...]]`, at most 256 siblings, root-first | boolean |
| `groth16_verify` | `[vkey32, publicValues, proof]` (an SP1 Groth16 proof) | boolean |
| `ecvrf_verify` | `[key32, alpha, proof80]` (RFC 9381, Edwards25519 TAI) | `{valid: true, beta}` or `{valid: false}` |
| `bn254_add` | `[g1, g1]` | `g1` |
| `bn254_mul` | `[g1, scalar32]`, with the scalar reduced mod r | `g1` |
| `bn254_pairing` | at most 64 `[g1, g2]` pairs, as separate arguments or one array; no pairs is `true` | boolean |

`bn254_pairing` is priced by weight, so both argument forms cost the same.

The BN254 operations and `groth16_verify` run on gnark-crypto, through Besu's native build of the EIP-196/197
precompiles. It is loaded with the other native backends (`Jlvm.load`), pinned by checksum and vendored
(`vendor/README.md`). Every point is validated (canonical coordinates, on the curve, G2 in the subgroup) before any
arithmetic. A pure-Scala engine with the same semantics is kept as the reference, and the test suites check gnark
against it.

**Sigma protocols** over secp256k1, byte-compatible with Ergo (`sigma.md`, normative). They prove knowledge of secret
keys for a tree of `and`, `or` and `threshold` over Schnorr and Diffie-Hellman-tuple statements, without revealing
which branches the prover knew. This is what ring signatures, hidden-signer k-of-n multisig and ZeroJoin need.

| Operation | Arguments | Result |
|---|---|---|
| `sigma_verify` | `[proposition, proof, message]` | boolean |
| `schnorr_verify` | `[key33, message, proof56]`: `sigma_verify` of one `dlog` leaf (Ergo's P2PK signature) | boolean |

- A proposition is a map per node (`sigma.md` §3): `{"type": "dlog", "pk": key33}`,
  `{"type": "dhtuple", "g", "h", "u", "v"}`, `{"type": "and" | "or", "children": [...]}`, and
  `{"type": "threshold", "k": k, "children": [...]}`. Programs build it like any other value, so they choose the
  statement at run time, e.g. with `if` over a deadline.
- The message must bind what the proof authorises (the update, with a nonce or ordinal), or the proof replays.
- Combining single-key checks with `or` or `some` proves that a particular key signed. Only one `sigma_verify` over
  an `or` or `threshold` hides which.
- Gas is priced by weight: about 45,000 per `dlog` leaf and 92,000 per `dhtuple` leaf, with proposition and proof
  together. The message pays the hashing rate, 1 per unit of weight, as `blake2b256` does.

**Authenticated structures**: dilf4s's Merkle trees (`merkle.md`) and binary Patricia tries (`trie.md`), over SHA-256.
Proofs are their canonical records (`merkle-inclusion`, `merkle-consistency`, `trie-proof`), passed as values.
A versioned trie (`versioned-trie.md`) is verified with these same trie ops. Its §9 gives the programs for:
- current keys and absence;
- namespace slices;
- version inclusion and absence;
- a key at a past version;
- the latest version at or before `t`.
- **Leaves and values commit by `commit(v)` (`merkle.md` §2):** `0x00 ‖ b` for bytes, `0x01 ‖ cbor(v)` otherwise. A
  proof of the bytes `0x6161` therefore never verifies the text `"a"`. The library builds these with
  `MerkleTree.leaf` and `Trie.putCanonical`.
- **There is no raw reading.** If a program could read a leaf or value either as its own bytes or by `commit(v)`
  under one hashing, whoever chose the reading could make a proof of the bytes `0x6161` verify the text `"a"`.
  Structures built with the byte-level APIs (`MerkleHashing.leaf`, `Trie.putValue`) are for library use. The one
  exception is the `"rfc6962"` Merkle profile, for external logs: it takes leaves as raw bytes, and must be bytes, but
  its hashing differs throughout, so no root verifies under both readings.

| Operation | Arguments | Result |
|---|---|---|
| `merkle_verify` | `[head, leaf, proof]`, or `[head, leaf, proof, "rfc6962"]` (RFC 6962 hashing, raw byte leaves) | boolean |
| `merkle_consistency_verify` | `[oldHead, newHead, proof]`, or with `"rfc6962"` | boolean |
| `trie_verify` | `[mode, root, key, value, proof]`; a null `value` claims absence | boolean |
| `trie_prefix_verify` | `[mode, root, prefix, entries, proof]` | boolean |
| `trie_range_verify` | `[mode, root, from, until, entries, proof]`; `until` null is unbounded | boolean |

- **A head is `{root, size}`**, taken from one trusted tree head. A proof whose own size differs from the head's is
  `false`: a root from a smaller tree can satisfy a proof that claims a larger size (`merkle.md` §3).
- **A mode is `{"fixed": n}` or `{"variable": max}`.** A key of the wrong length for the mode is a `Crypto` error.
- **Entries are `[key, value]` pairs in key order** (bitwise, a prefix before its extensions). True means they are
  exactly the trie's entries under the prefix, or in `[from, until)`.
- **"Not covered" is an error.** A proof that does not reach the queried key, or reaches a stub inside the queried
  prefix or range, is a `Crypto` error, never `false`: it establishes neither presence nor absence.
- **Gas** is priced by weight. As measured: about 8 per unit of weight for a Merkle proof (inclusion or
  consistency), and 15 for a trie proof, whose records are larger and hashed through CBOR. A listing check
  (`trie_prefix_verify`, `trie_range_verify`) is linear in its proof's weight and its bounds: 17 per unit of weight
  over a base of 20,000.
- **Claims heavier than `MaxProofWeight`** together (a leaf, a value, a listing's values; `conformance.md` §5) are a
  `Crypto` error, refused from their weights before any is hashed and before the proof is decoded (after the key and
  the entries' shapes are checked).
- **A proof heavier than `MaxProofWeight`** (2^24, `conformance.md` §5) is a `Crypto` error, refused from its weight
  before it is decoded, as every malformed proof is. The size charge comes first, so at the default limit such a proof
  is `OutOfGas` (its weight alone costs over 2^24); a host limit that pays for it reaches the refusal. No result is
  heavier than 2^24 (§2), so only the input data or a host binding can carry one.

## 7. Gas

**Schedules.** A schedule is versioned data (`Gas.scala`, `GasSchedule.V1`). It holds:
- for every operation, a base charge, a per-unit charge and a size unit;
- optionally, a rate of their own for particular argument positions, and further rates charged on every argument;
- the scalar charges `literal`, `element`, `varAccess`, `perPathSegment`, `callbackRun` and `rendering`.

**What gas pays for.** Gas meters the work a validating node does to execute a program, on every supported node backend:
the JVM (the reference, the primary server backend) and Rust (`sdk/rust/jlvm`). Prover, SP1-guest and client costs play
no part. Every charge is at least the work's cost on the **slowest** backend, so no backend is underpaid and no
operation is cheaper than its work (a denial-of-service vector).

**The anchor.** `secp256k1_verify` of an 18-byte message is the unit everything is priced against: its own charge
(`7200 + weight`, 7,318 gas) is what it costs on each backend. Every other charge is the work's time **as a ratio to
the anchor's on the same backend**, times the anchor's charge, taken at the higher of the two backends' ratios. The JVM
runs the anchor in libsecp256k1 (native, through ACINQ's JNI bindings) and Rust in k256 (pure Rust), so each backend's
gas runs at its own rate (on the reference machine, about 240 gas per µs on the JVM and 120 on Rust), and an operation
is priced by how much slower than secp256k1 it is where it is slowest.

**Calibration** (`benchmarks`: `GasCases`, `GasRatios`, `ratios.py`; `sdk/rust/jlvm/examples/gas_ratios.rs`). Both
harnesses time the same cases (each a program, a baseline that evaluates the same argument reads, and data, exported
by `GasRatios export`), through the evaluator on the direct driver:
- **Rounds.** A case is timed in rounds in one thread: `probe, s1, s2, reference, s2, s1, probe`, where `(s1, s2)` is
  (anchor, case) or (case, anchor), chosen at random. A round is a palindrome a few milliseconds long, so drift that
  is linear across it cancels in each ratio. The anchor, the case and the reference (`sha256` of 1024 bytes) are each
  timed net of their baselines.
- **Probes.** The probe is a fixed compute-bound kernel (a chain of 2^17 xorshift steps). A round whose two probes
  differ by more than 10% (a migration or a clock change inside it) is rejected; so is one whose probe is more than 3
  MADs from its cluster's median.
- **Clusters.** Rounds are split by probe time into two clusters when their centres differ by more than 10%
  (performance and efficiency cores), and a case's ratio is the higher of its clusters'.
- **Batches.** Each of anchor, case and reference is timed in batches of at least 250 µs. A batch's size doubles from
  1 until the fastest of three batches at that size takes that long, so one interrupted batch cannot leave a case
  timed one evaluation at a time.
- **Reference configuration.** The prices hold for a node that runs the JVM as calibrated: OpenJDK 21 pinned to one
  CPU with `-Xms4g -Xmx4g -XX:+UseSerialGC -XX:+AlwaysPreTouch` (`GasRatios.Reference`; `benchmarks/calibrate.sh`
  runs the calibration). Before it measures, `GasRatios measure` checks the settings in effect, whatever flags set
  them: Java 21, the HotSpot heap sizes, pre-touch and collector, no `ActiveProcessorCount`, and a CPU affinity of
  exactly one CPU. It refuses any other configuration and names this one in its output. The serial collector runs
  every collection on the measuring core, inside the round of the allocation that caused it, so collection is paid
  by the work that allocated and no collector thread competes with a round. The heap is fixed and touched in full at
  start, so it never resizes and no round meets a first-touch page fault. 4 GiB holds the calibration's live set
  (about 1.4 GiB after each young collection: maps of 2^20 entries, texts of 64,000 units) in the old generation
  with room, so no full collection runs while rounds are timed. Rust runs the release build, pinned to one CPU. A
  node that runs the JVM otherwise (another collector, a smaller heap, collector threads sharing its cores) is
  outside this guarantee: its costs can differ in either direction, and it must measure its own.
- **Statistics.** A case's ratio in a process is the median over its rounds, with a bootstrap 95% interval, cross-
  checked against `case/reference × reference/anchor`. The JVM runs ten processes and Rust three, more where a
  price-setting case's processes are more than 5% apart. A process's heap and code layout move an operation by up to
  tens of percent, and each process is an independent start of the reference configuration: whichever layout it
  gets, a node starting the same way can get. So a case's ratio is the **highest** of its processes, not a
  percentile: a state seen in one start of ten is a state about one node in ten is in. `ratios.py` also reports the
  second-highest process and how many lie within 5% of the highest, so a lone slow process stands out. A slow
  result is kept even when the case is cheaper re-run alone, for the same reason. Re-runs alone only add
  measurements, for cases added after a full run. A case is converged when its interval is within ±5% and the
  cross-check agrees within 5%.
- **Pricing.** A base and per-unit rates are fitted to cases at small and large sizes and at adversarial shapes (keys
  sharing long prefixes, deep maps, worst-case substring searches, long trie keys, ground VRF counters), so that each
  charge is at least the measured cost. Where the data lie far below a charge, the charge is lowered to within about
  2× of it. A per-unit rate of 1 is the floor for work that grows with its input.

**The default limit** is 2^24 = 16,777,216 gas, Ethereum's per-transaction cap (EIP-7825): about 70 ms of work on the
JVM and 140 ms on Rust on the reference machine. Hosts pass their own limit per evaluation.

**Size units.** An operation's size charge is summed over its evaluated arguments, each at its position's rate (by
default the operation's). For example, `sigma_verify` and `schnorr_verify` charge the message at the hashing rate,
1 per unit of weight, and the proposition and proof at the curve rate; `bls_aggregate_verify` charges each key as an
element and the message at the hashing rate, since hashing it to G2 is linear in its length. Each argument is measured in its rate's unit:
- `Elements`: the top-level entries of arrays and objects, and 1 for anything else;
- `Weight`: the weight (§2);
- `Limbs2`: for a number, the square of its 64-bit limbs, numerator and denominator together (`⌊bits/64⌋` each), plus 8
  (`GcdLimbs`, the fixed cost of a gcd) for a non-integer rational: `(8 + L)^2`; 0 for anything else;
- `RationalLimbs2`: `Limbs2` for a non-integer rational, 0 for an integer;
- `RationalWeight`: the weight of a non-integer rational, 0 for anything else (comparing two rationals multiplies
  across);
- `Arguments`: 1 for every argument, whatever it holds (`cat` takes and walks each argument, even an empty array);
- `None`: zero.

**Map searches.** `get`, `has`, `set` and `unset` search their map (argument 0) for their key (argument 1). Besides
the base, each pays `levels × (perLevel + perBlock × ⌊length/128⌋)`, where `length` is the key's in UTF-16 units and
`levels = ⌈log2(n + 1)⌉ + 1` for a map of `n` entries: the height of a perfectly balanced tree of `n`, and one more for
the key itself. A search compares the key with one entry's per level, and each comparison reads the keys' common
prefix, so a key sharing a long prefix with the map's costs per level. The rates absorb the trees' real heights (the
JVM's red-black tree is at most twice as tall as balanced, Rust's AVL tree at most 1.45 times) and the JVM's `set` and
`unset`, which search twice. The charge is 0 when argument 0 is not a map or argument 1 not text (the operation then
fails with `Type`).

**Merging maps.** When every argument of `merge` is a map, each entry of the maps after the first is set into the
result, which holds at most all their entries. Besides the size charge, `merge` then pays
`levels × (perLevel × entries + perBlock × ⌊weight/128⌋)`, where `entries` and `weight` are those of the maps after the
first, and `levels = ⌈log2(N + 1)⌉ + 1` for the `N` entries of all of them.

The gcd that normalises a result is quadratic in its operands' size. `/` and `%` therefore also charge `Limbs2`, and
`+`, `-` and `*` also charge `RationalLimbs2`: integer sums and products are linear. `%` of rationals divides,
multiplies and subtracts, so it charges `RationalLimbs2` as well; so does `abs` (the Rust node re-checks that its result
is in lowest terms). `<`, `<=`, `>`, `>=`, `min` and `max` charge `RationalWeight`: two rationals are compared by
multiplying across (`min` and `max` return the argument they choose as it is, never reduced again).
`+`, `-`, `*`, `min` and `max` also charge per operand (`Elements`, which is 1 for a number): converting and combining
an operand has a fixed cost, which a spread over thousands of operands multiplies. `pow` is priced at its worst case,
a 4096-bit rational result.

**Work an argument's size does not bound** has charges of its own, each taken before the work:
- **Accumulators.** `+` and `*` fold left, and each step is the binary operation on the accumulator and the next
  operand. The operands are paid by the size charge; each step after the first also pays the size charge of its
  accumulator, as if it were an argument. Every intermediate is checked against the number bound (§2), so `*` of 10,000
  twos is `Limit` at the 4097th bit, not a 10,000-bit product.
- **Rendering numbers** (`cat`, `join`): `rendering` per unit of `(6 + ⌊b/64⌋)^2` for each number rendered, where `b`
  bounds the bits of its decimal expansion: the numerator's bits for an integer, and `bits(p) + ⌊(7 bits(q) + 2)/3⌋`
  for a rational `p/q` (the expansion of `p/(2^a 5^c)` is `p 2^(s-a) 5^(s-c)`, `s = max(a, c)`). The conversion is
  quadratic in the expansion, which can be far longer than the number's weight: `1/2^4095` weighs 66 and renders as
  4,097 characters. Measured at about 1 gas a unit for the widest numbers; the 6 limbs are the fixed cost of a conversion (about 100 gas
  for a small integer).
- **Pieces.** `cat` also charges per element of each argument and per argument, and `join` per element of each
  argument, at the rates in the table: a piece costs a few gas besides its bytes, and `cat` walks every argument, even
  an empty array.
- **Separators.** `join` renders its pieces, checks the result's weight (saturating) against the bound, which is
  `Limit` before anything is built, and then charges `perUnit(join)` per byte of separator the result will copy.
- **Paths parsed at run time** (a computed `var` path given as text, a `missing` or `missing_some` key) are split on
  `.`: `perUnit(split) × weight(path)` before parsing. A path has at most as many segments as its weight, so this also
  pays for walking them.
- **Deduplicating** (`unique`, `intersect`) sorts by a total order instead of hashing: kinds, then scalars structurally
  and collections by their canonical encoding. O(n log n), at the operations' calibrated rates, whatever the values.

GasCalibration measures these with the argument `metering`.

**Every charge is taken before the work it pays for.** If a charge would exceed the limit:
- the evaluation stops with `OutOfGas(used + charge, limit)`;
- the gas used is the whole limit;
- no part of the unpaid work is done.

**Charge points:**

| Node | Charges, in order |
|---|---|
| Literal | `literal`. |
| Variable | `varAccess + perPathSegment × segments`. A `$name` counts as one segment, and every segment counts once more per 32 UTF-16 units of its length (it is compared on a hit, and rendered in a warning). A non-empty plain path inside callbacks may consult every enclosing frame, and each compares the first segment with its element (work that grows with the length the keys share), so the first segment's count is added once more per enclosing frame. A computed path is evaluated first. Text then pays `perUnit(split) × weight` before it is parsed, and then the access and the segments it gives; an array pays the access and its segments (counted the same way) before they are converted. |
| Array or map expression | `literal + element × n`, then each item. |
| Strict operation | `base(op)`, then each argument, then `perUnit(op) × size(arguments)`, then the operation, with the charges above for `+`, `*`, `cat` and `join`. A spread first pays `element` per element of its array (each is an argument), then is charged the same way, on those elements. |
| Lookup | `base(op)`, then each argument, then `perUnit(op) × size(arguments)` plus, for each key, `perUnit(split) × weight(key)` and, per enclosing frame, `perPathSegment` × the segment count of the whole key text (as `var` pays for its first segment; the whole text bounds the first segment, and prices the key without parsing it), then the lookup. |
| `if` | `base(if)`, then for each condition tested `element` and the condition, then the branch taken. |
| `and`, `or`, `default` | `base(op)`, then only the subexpressions actually evaluated. |
| `let` | `base(let) + element × Σ (1 + ⌊len(name)/32⌋)` over the bindings (each binding is an insertion into the scope, and comparing names costs like a path segment), then the bindings and the body. |
| Callback | `base(op)`, then the collection, then `perUnit(op) × elements`, then for each body run: `callbackRun` plus the body. For `some` with a quorum, `min` is evaluated after the collection. For `reduce`, `initial` is evaluated after the collection. |

A program that does not compile uses no gas.

**`ecvrf_verify`** runs RFC 9381's try-and-increment hash-to-curve before it checks the proof, and an attacker can
choose `alpha`: one whose first `k` counters fail takes about `2^k` attempts to find, and then serves forever. Its base
is priced for 64 counters, a number no search for `alpha` reaches; honest inputs need about two. Its size charge pays
for hashing `alpha` once: an implementation hashes the prefix `suite || 01 || Y || alpha` once and finishes a copy of
that state for each counter, since hashing `alpha` again per counter would make a ground `alpha` cost its weight 64
times.

**The table.** Every operation's charges in schedule v1 (the charge points above say when each is taken):

<!-- gas-table: generated from GasSchedule.V1 by GasTableGen; do not edit -->

Schedule v1. Scalar charges: `literal` 10, `element` 11, `varAccess` 70, `perPathSegment` 4, `callbackRun` 18, `rendering` 2.

| Operation | Base | Per unit | Other rates |
|---|---|---|---|
| `if` | 20 |  |  |
| `and` | 15 |  |  |
| `or` | 15 |  |  |
| `!` | 25 |  |  |
| `!!` | 25 |  |  |
| `default` | 15 |  |  |
| `let` | 30 |  |  |
| `==` | 50 | 1 per `Weight` |  |
| `!=` | 50 | 1 per `Weight` |  |
| `===` | 50 | 1 per `Weight` |  |
| `!==` | 50 | 1 per `Weight` |  |
| `<` | 65 | 1 per `Weight` | plus 14 per `RationalWeight` |
| `<=` | 65 | 1 per `Weight` | plus 14 per `RationalWeight` |
| `>` | 65 | 1 per `Weight` | plus 14 per `RationalWeight` |
| `>=` | 65 | 1 per `Weight` | plus 14 per `RationalWeight` |
| `+` | 40 | 4 per `Weight` | plus 7 per `RationalLimbs2`; plus 40 per `Elements` |
| `-` | 40 | 3 per `Weight` | plus 7 per `RationalLimbs2`; plus 60 per `Elements` |
| `*` | 40 | 10 per `Weight` | plus 7 per `RationalLimbs2`; plus 40 per `Elements` |
| `/` | 340 | 2 per `Weight` | plus 9 per `Limbs2` |
| `%` | 340 | 2 per `Weight` | plus 9 per `Limbs2`; plus 8 per `RationalLimbs2` |
| `min` | 30 | 1 per `Weight` | plus 14 per `RationalWeight`; plus 35 per `Elements` |
| `max` | 30 | 1 per `Weight` | plus 14 per `RationalWeight`; plus 35 per `Elements` |
| `abs` | 60 |  | plus 2 per `RationalLimbs2` |
| `round` | 60 | 8 per `Weight` |  |
| `round_half_up` | 140 | 8 per `Weight` |  |
| `floor` | 60 | 8 per `Weight` |  |
| `ceil` | 60 | 8 per `Weight` |  |
| `pow` | 20000 |  |  |
| `map` | 40 | 15 per `Elements` |  |
| `filter` | 40 | 15 per `Elements` |  |
| `reduce` | 40 | 8 per `Elements` |  |
| `all` | 40 | 8 per `Elements` |  |
| `some` | 40 | 8 per `Elements` |  |
| `none` | 40 | 8 per `Elements` |  |
| `find` | 40 | 8 per `Elements` |  |
| `count` | 45 | 2 per `Elements` |  |
| `merge` | 210 | 8 per `Weight` | merging: 12 per level and entry, 2 per level and weight block |
| `in` | 70 | 1 per `Weight` | plus 1 per `Elements` |
| `intersect` | 50 | 17 per `Weight` |  |
| `unique` | 60 | 23 per `Weight` |  |
| `slice` | 120 | 7 per `Elements` |  |
| `reverse` | 55 | 12 per `Elements` |  |
| `flatten` | 70 | 7 per `Weight` |  |
| `length` | 45 | 1 per `Weight` |  |
| `cat` | 65 | 1 per `Weight` | plus 4 per `Elements`; plus 14 per `Arguments` |
| `substr` | 75 | 1 per `Weight` |  |
| `lower` | 45 | 1 per `Weight` |  |
| `upper` | 50 | 1 per `Weight` |  |
| `join` | 110 | 1 per `Weight` | plus 14 per `Elements` |
| `split` | 85 | 6 per `Weight` |  |
| `trim` | 75 | 1 per `Weight` |  |
| `startsWith` | 50 | 1 per `Weight` |  |
| `endsWith` | 50 | 1 per `Weight` |  |
| `keys` | 130 | 5 per `Weight` |  |
| `values` | 80 | 7 per `Weight` |  |
| `entries` | 160 | 11 per `Weight` |  |
| `get` | 40 |  | search: 3 per level, 1 per level and key block |
| `has` | 40 |  | search: 3 per level, 1 per level and key block |
| `set` | 60 |  | search: 18 per level, 2 per level and key block |
| `unset` | 60 |  | search: 18 per level, 2 per level and key block |
| `exists` | 60 | 1 per `Elements` |  |
| `missing` | 65 | 20 per `Elements` |  |
| `missing_some` | 80 | 20 per `Elements` |  |
| `typeof` | 60 |  |  |
| `hex` | 45 | 1 per `Weight` |  |
| `unhex` | 80 | 2 per `Weight` |  |
| `hex_to_int` | 80 | 4 per `Weight` |  |
| `bytes_to_int` | 45 | 1 per `Weight` |  |
| `int_to_bytes` | 70 | 2 per `Weight` |  |
| `sha256` | 95 | 1 per `Weight` |  |
| `sha3_256` | 130 | 1 per `Weight` |  |
| `keccak256` | 150 | 1 per `Weight` |  |
| `blake2b256` | 115 | 1 per `Weight` |  |
| `ed25519_verify` | 20000 | 1 per `Weight` |  |
| `secp256k1_verify` | 7200 | 1 per `Weight` |  |
| `bip340_verify` | 9000 | 1 per `Weight` |  |
| `ecrecover` | 32500 |  |  |
| `bls_verify` | 290000 | 1 per `Weight` |  |
| `bls_aggregate_verify` | 250000 | 33000 per `Elements` | argument 1: 1 per `Weight` |
| `poseidon` | 1800 | 3900 per `Elements` |  |
| `pmt_verify` | 1000 | 11000 per `Elements` |  |
| `groth16_verify` | 480000 | 1 per `Weight` |  |
| `ecvrf_verify` | 130000 | 1 per `Weight` |  |
| `bn254_add` | 700 |  |  |
| `bn254_mul` | 14500 |  |  |
| `bn254_pairing` | 135000 | 470 per `Weight` |  |
| `schnorr_verify` | 5000 | 450 per `Weight` | argument 1: 1 per `Weight` |
| `sigma_verify` | 5000 | 450 per `Weight` | argument 2: 1 per `Weight` |
| `merkle_verify` | 1000 | 8 per `Weight` |  |
| `merkle_consistency_verify` | 500 | 8 per `Weight` |  |
| `trie_verify` | 500 | 15 per `Weight` |  |
| `trie_prefix_verify` | 20000 | 17 per `Weight` |  |
| `trie_range_verify` | 20000 | 17 per `Weight` |  |

<!-- gas-table: end -->

**Drivers.** Three drivers run the same small-step rules:
- **direct (production):** the explicit continuation stack as one pure tail-recursive function of (step, stack,
  meter). Constant host stack;
- **stack machine (reference):** the same stack, run by `tailRecM` over cats-mtl `Stateful` and `Raise`. About 12×
  slower per step: each step passes through `EitherT` and `State`;
- **recursive (reference):** nested computations that mirror the program.

A property test checks that all three agree on value, error, gas and warnings, at random gas limits.

## 8. Deliberate changes from metakit

The parser reads only the surface of §1. Programs written for metakit are ported by rewriting their text into it
(as the conformance vectors and the ottochain corpus in `jlvm/src/test/resources/programs/ottochain` were).

| # | metakit | Change here | Why |
|---|---|---|---|
| 1 | Coercion: `"1" == 1`, arithmetic on strings, booleans and null. | No coercion. | Silent coercion hides bugs. |
| 2 | Unwraps a single array argument for `+`, `*`, `min`, `max`, `merge`, `exists`, depending on its evaluated value. | Spreading is decided by the syntax (§1), for the variadic operations only. `exists` never spreads. `count [array]` and `missing [[keys]]` remain as defined forms. | The value no longer decides the meaning: `{"cat": [[1, 2]]}` is `[1, 2]`, never `"12"`. |
| 3 | Ints and floats are distinct (`1.5 + 0.5` is a float `2`). | One exact number kind. | Exactness, and one equality. |
| 4 | `round` rounds half away from zero. | `round` is half-even; `round_half_up` is explicit, and is what a ported metakit `round` becomes. | Unbiased rounding by default. |
| 5 | `and`, `or` and `default` evaluate everything; `all`, `some` and `none` never stop early. | Lazy, and stop at the first decisive element. | Gas pays only for work done. |
| 6 | Callback scope merges the element into the data; arrays are concatenated. | The element is a frame looked up before the data; arrays are not concatenated; collisions are warned about; `$current`, `$index` and `$accumulator` are unambiguous. | Silent collisions. |
| 7 | `length` and `substr` count UTF-16 code units; `trim` uses Java's notion of whitespace; `lower` and `upper` use the JVM's default locale. | Code points; Unicode `White_Space`; ASCII-only case mapping. | Portability. |
| 8 | Gas is unchecked `Long`, priced after the fact, and absent on failure. | Saturating, charged before the work, sized by weight, reported on failure. | Fairness, and DoS bounds. |
| 9 | `cat` on arrays is an error. | Concatenates when every argument is an array. | Programs use it. |
| 10 | Unbounded numbers and values. | The bounds of §2. | DoS. |
| 11 | Every single-key object naming an operation is code, so `{"count": 3}` as data is impossible. | `{"object": {...}}` builds any map. | The `count` ambiguity. |
| 12 | `var` paths may be computed only as text. | Text, or an array of exact segments. | Keys holding `.` cannot redirect a read. |
| 13 | `+` and `*` of nothing are errors. | 0 and 1. | Spreads over empty collections. |
| 14 | Sigma protocols on BN254 G1 with metakit's own transcript, GF(2^8) per byte for thresholds, and standalone `prove_dlog_verify` / `prove_dhtuple_verify`. | Ergo's construction on secp256k1 (`sigma.md`), cross-checked against Ergo both ways. `schnorr_verify` is a one-leaf `sigma_verify`; the standalone leaf ops are gone, since a one-leaf `sigma_verify` does the same. | Ergo's construction has been in production since 2019, and Ergo's own prover and verifier check ours. Standalone leaves combined with `or` look like rings but hide nothing. |
| 15 | `typeof` names `int`, `float`, `bool`, `map`. | `number`, `boolean`, `object` (§2), with one exact number kind; `bytes` is new. | One number kind; JSON's names. |
| 16 | `keys`, `values` and `entries` with no argument give null; `missing_some` with one argument treats it as the keys. | `Arity` errors. | A call with missing arguments is a mistake, not a query. |
| 17 | `substr` and `slice` indices beyond 64 bits are errors. | Clamped like any other out-of-range index (JavaScript bounds). | One rule for every index. |
| 18 | A map may hold null values (`{"a": null}`, or an invalid `ecvrf_verify`'s `beta: null`). | Null values are dropped from maps (`canonical-encoding.md`, dropNulls): absent and null read the same. | One encoding per value, and fields can be added without changing existing hashes. |
| 19 | A BLS signature of the right length that does not decode is `false`. | A `Crypto` error, as for every other scheme: malformed input is an error, a well-formed invalid proof is `false` (§3). | One rule for every verifier. |
| 20 | An array headed by an operation's name is a call (`["+", 1, 2]`); `let` names are read with plain paths. | A parse error in each case (§1, §4.2). | Text shaped like a call, or a plain read of a bound name, never silently reads data. |
| 21 | `prove_dlog_verify`, `prove_dhtuple_verify`, `mpt_verify`, `mpt_prefix_verify`, `smt_verify`, `noop`. | Not operations; the parse error names the operation to use (§1). | Replaced by `sigma_verify` (§8 #14) and the binary trie (`trie.md`). `noop` did nothing. |
| 22 | Evaluation depth is capped at run time (about 255 evaluated levels; an untaken branch never counts). | The program is bounded before it runs: its canonical form, and its rendered JSON, must fit 256 levels (§1), about 127 nested operations. Every depth violation is `Limit`. | Every program has an identity and an exact rendering; depth is known before any gas is spent. |
