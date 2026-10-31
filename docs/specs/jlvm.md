# dilf4s JLVM (language v1, gas schedule v1)

Status: normative. Every operation of language version 1 is specified and available.
Conformance vectors: `shared/vectors/jlvm_v1.json` (schema `shared/schemas/jlvm-vectors.cddl`). Each case is a program
and its data, with the outcome (a value's canonical CBOR, or an error kind) and the gas used under schedule v1. Most
are metakit's own test programs, rewritten into this surface and stored as its text. They agree with metakit's
results, except where §8 says otherwise.

The JLVM is a strict, metered expression language for ledgers, written in JsonLogic syntax. A result is a function of
exactly five things:

- the program;
- the input data;
- the host bindings;
- the language version;
- the gas-schedule version.

## 1. Programs

**Surface.** Programs are written as JSON. The text must first pass `CanonicalJson.parse`
(`canonical-encoding.md` §3), which bounds depth and requires exact numbers. Then:

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
  scope forward in one pass, in a sorted set of names. Checking the canonical form still builds hashed maps of literal
  keys, so compilation is not strictly linear in the worst case, but it is bounded by this size. The largest conformance
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

**Weight.** A value's weight is a portable measure of its size:
- 1 per node;
- plus the UTF-8 length of each text and each map key;
- plus the length of each byte string;
- plus the 64-bit limbs of each number, `⌊bitLength/64⌋` for the numerator and for the denominator.

Shared sub-values count every time they occur, and the total saturates.

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
- `lower` and `upper` are the Unicode default full case mappings of **Unicode 15.0**, independent of locale. A vector
  pins the version: Garay, added in 16.0, does not map.
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
- **Gas** is priced by weight. As measured: about 13 per unit of weight for Merkle proofs, and 30 for trie proofs,
  whose records are larger and hashed through CBOR.

## 7. Gas

**Schedules.** A schedule is versioned data (`Gas.scala`, `GasSchedule.V1`). It holds:
- for every operation, a base charge, a per-unit charge and a size unit;
- optionally, a rate of their own for particular argument positions, and further rates charged on every argument;
- the scalar charges `literal`, `element`, `varAccess`, `perPathSegment`, `callbackRun` and `rendering`.

**Calibration.** Schedule v1 prices work at **150 gas per µs**, on the direct driver. Every operation and node charge
was fitted to measured time at a small and a large size (`benchmarks`: `GasCalibration`). A per-unit rate of 1 is the
floor for work that grows with its input. The cryptographic operations sit at the same rate; the BN254 ones land near
Ethereum's EIP-1108 prices.

**The default limit** is 2^24 = 16,777,216 gas, Ethereum's per-transaction cap (EIP-7825): about 110 ms of work. Hosts
pass their own limit per evaluation.

**Size units.** An operation's size charge is summed over its evaluated arguments, each at its position's rate (by
default the operation's). For example, `sigma_verify` and `schnorr_verify` charge the message at the hashing rate,
1 per unit of weight, and the proposition and proof at the curve rate. Each argument is measured in its rate's unit:
- `Elements`: the top-level entries of arrays and objects, and 1 for anything else;
- `Weight`: the weight (§2);
- `Limbs2`: for a number, the square of its 64-bit limbs, numerator and denominator together (`⌊bits/64⌋` each); 0
  for anything else;
- `RationalLimbs2`: `Limbs2` for a non-integer rational, 0 for an integer;
- `Key`: for text, `⌊length/32⌋` in UTF-16 units; 0 for anything else. `get`, `has`, `set` and `unset` charge their key
  at 1 per unit: a hit compares the whole key, so a long key costs more, and keys under 32 units nothing extra;
- `None`: zero.

The gcd that normalises a result is quadratic in its operands' size. `/` and `%` therefore also charge `Limbs2`, and
`+`, `-` and `*` also charge `RationalLimbs2`: integer sums and products are linear. `pow` is priced at its
worst case, a 4096-bit rational result.

**Work an argument's size does not bound** has charges of its own, each taken before the work:
- **Accumulators.** `+` and `*` fold left, and each step is the binary operation on the accumulator and the next
  operand. The operands are paid by the size charge; each step after the first also pays the size charge of its
  accumulator, as if it were an argument. Every intermediate is checked against the number bound (§2), so `*` of 10,000
  twos is `Limit` at the 4097th bit, not a 10,000-bit product.
- **Rendering numbers** (`cat`, `join`): `rendering` per unit of `(4 + ⌊b/64⌋)^2` for each number rendered, where `b`
  bounds the bits of its decimal expansion: the numerator's bits for an integer, and `bits(p) + ⌊(7 bits(q) + 2)/3⌋`
  for a rational `p/q` (the expansion of `p/(2^a 5^c)` is `p 2^(s-a) 5^(s-c)`, `s = max(a, c)`). The conversion is
  quadratic in the expansion, which can be far longer than the number's weight: `1/2^4095` weighs 66 and renders as
  4,097 characters. Measured at about 1.5–2 gas a unit; the 4 limbs are the fixed cost of a conversion.
- **Pieces.** `cat` and `join` also charge 32 per element of each argument: a piece costs about 30 gas besides its bytes.
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
| Variable | `varAccess + perPathSegment × segments`. A `$name` counts as one segment, and every segment counts once more per 32 UTF-16 units of its length (it is compared on a hit, and rendered in a warning). A non-empty plain path inside callbacks also counts each enclosing frame as a segment, since it may consult them all. A computed path is evaluated first. Text then pays `perUnit(split) × weight` before it is parsed, and then the access and the segments it gives; an array pays the access and its segments (counted the same way) before they are converted. |
| Array or map expression | `literal + element × n`, then each item. |
| Strict operation | `base(op)`, then each argument, then `perUnit(op) × size(arguments)`, then the operation, with the charges above for `+`, `*`, `cat` and `join`. A spread is charged the same way, on the elements of its array. |
| Lookup | `base(op)`, then each argument, then `perUnit(op) × size(arguments)` plus, for each key, `perUnit(split) × weight(key)` and `perPathSegment` per enclosing frame (as `var` pays), then the lookup. |
| `if` | `base(if)`, then for each condition tested `element` and the condition, then the branch taken. |
| `and`, `or`, `default` | `base(op)`, then only the subexpressions actually evaluated. |
| `let` | `base(let) + element × Σ (1 + ⌊len(name)/32⌋)` over the bindings (each binding is an insertion into the scope, and comparing names costs like a path segment), then the bindings and the body. |
| Callback | `base(op)`, then the collection, then `perUnit(op) × elements`, then for each body run: `callbackRun` plus the body. For `some` with a quorum, `min` is evaluated after the collection. For `reduce`, `initial` is evaluated after the collection. |

A program that does not compile uses no gas.

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
| 7 | `length` and `substr` count UTF-16 code units; `trim` uses Java's notion of whitespace. | Code points; Unicode `White_Space`. | Portability. |
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
