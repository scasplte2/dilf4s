# dilf4s JLVM (language v1, gas schedule v1)

Status: normative, in progress. Every operation is specified except some of the cryptographic ones (§6), which are
listed but not yet available. The decisions still open are marked **Open**.

The JLVM is a strict, metered expression language for ledgers, written in JsonLogic syntax. A result is a function of
exactly five things:

- the program;
- the input data;
- the host bindings;
- the language version;
- the gas-schedule version.

## 1. Programs

**Surface.** Programs are written as JSON. The text must first pass `CanonicalJson.parse`
(`canonical-encoding.md` §6), which bounds depth and requires exact numbers. Then:

- **Objects.**
  - A single-key object whose key is an operation of this version is that operation.
    - Its value is the argument list. A non-array value is a single argument.
    - `{"literal": x}` quotes `x`, which is not evaluated.
    - `{"var": p}` and `{"var": [p, default]}` read a variable (§4.1).
  - Any other single-key object is a parse error.
  - Any other object is a map expression, whose fields are evaluated.
  - **Open (decision A):** an evaluating object constructor for single-key maps.
- **Arrays** are evaluated element-wise. There are no array-form operations.
- **JSON `null`** is the null literal.
- **Strings** of the form `0x` followed by an even number of lowercase hex digits are byte strings (§2). Every other
  string is text.

The parser's `Lenient` mode reads an unknown single-key object as a map expression, as metakit did. It exists only to
migrate older programs.

**Canonical form and identity.**
- A program is the tagged syntax tree of `shared/schemas/jlvm.cddl`, not its JSON text.
- Its identity is `taggedHash_H("dilf4s/jlvm/program/v1", cbor(program))` (`canonical-encoding.md` §4).
- Expressions nest at most 256 deep: an operation's arguments, an array's items and a map's fields are each one level
  deeper.

**Compilation.** Before evaluation, a program is checked into its evaluation form. These are parse errors
(`Malformed`), reported with no gas used:
- the shape of `if`, `let` and every callback form;
- binding names;
- duplicate bindings.

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

**Data paths.**
- `{"var": "a.b.0"}` reads the input data.
- Each segment is an object key, or an array index written in decimal without leading zeros.
- `""` is the whole data.
- An absent value is null, and then the default if one is given.

**Bound paths.**
- `{"var": "$name.path"}` reads a binding.
- A binding is one of:
  - a host binding, supplied with the evaluation (e.g. `$ordinal`);
  - a `let` name;
  - a callback name: `$current`, `$index` or `$accumulator`.
- Inner bindings shadow outer ones.
- An unbound name is `Malformed`, unless a default is given.
- **Open (decisions B, C):** whether plain paths inside callbacks also see the element (metakit's merged scope).

### 4.2 Lazy forms

**`if`** takes `[c1, t1, (c2, t2)..., else]`, an odd count of at least 3. The result is the first `ti` whose `ci` is
truthy, otherwise `else`. Only the taken branch is evaluated.

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

### 4.3 Callbacks

**Collections.** The collection is evaluated first. An array iterates. Null is an empty collection (metakit). Anything
else is a `Type` error.

**Bindings.** The body runs once per element, with `$current` (the element) and `$index` bound.

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

**Syntactic forms.** Whether `count` is a callback is decided by syntax: `count [array]` is the length.

### 4.4 Lookups

`missing [keys...]` (or `missing [[keys...]]`) returns the keys whose data path is absent.

`missing_some [min, [keys]]` returns `[]` if at least `min` keys are present, and otherwise the missing keys.

Both read the input data, never bindings.

## 5. Strict operations

Every argument is evaluated, then the operation is applied. There is no coercion and no unwrapping of a single array
argument.

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
| `+` `*` | Exact sum or product, of at least 1 argument. |
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
| `cat` | Concatenates strings, numbers (§2) and booleans; null is empty. When **every** argument is an array, it concatenates the arrays instead. |
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
| `sha256` `sha3_256` `blake2b256` | The 32-byte digest of bytes, or of the UTF-8 encoding of text. |

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

**Not yet available.** Evaluating these is a `Crypto` error:
- Sigma protocols (**unaudited**): `schnorr_verify`, `prove_dlog_verify`, `prove_dhtuple_verify`, `sigma_verify`.
- Merkle and trie proofs (**Open**: argument design): `merkle_verify`, `merkle_consistency_verify`, `trie_verify`,
  `trie_prefix_verify`, `trie_range_verify`.

## 7. Gas

**Schedules.** A schedule is versioned data (`Gas.scala`, `GasSchedule.V1`). It holds:
- for every operation, a base charge, a per-unit charge and a size unit;
- the scalar charges `literal`, `element`, `varAccess`, `perPathSegment` and `callbackRun`.

**Size units.** An operation's size is summed over all its evaluated arguments:
- `Elements`: the top-level entries of arrays and objects, and 1 for anything else;
- `Weight`: the weight (§2);
- `None`: zero.

**Every charge is taken before the work it pays for.** If a charge would exceed the limit:
- the evaluation stops with `OutOfGas(used + charge, limit)`;
- the gas used is the whole limit;
- no part of the unpaid work is done.

**Charge points:**

| Node | Charges, in order |
|---|---|
| Literal | `literal`. |
| Variable | `varAccess + perPathSegment × segments`. A `$name` counts as one segment. |
| Array or map expression | `literal + element × n`, then each item. |
| Strict operation, lookup | `base(op)`, then each argument, then `perUnit(op) × size(arguments)`, then the operation. |
| `if`, `and`, `or`, `default`, `let` | `base(op)`, then only the subexpressions actually evaluated. |
| Callback | `base(op)`, then the collection, then `perUnit(op) × elements`, then for each body run: `callbackRun` plus the body. For `some` with a quorum, `min` is evaluated after the collection. For `reduce`, `initial` is evaluated after the collection. |

A program that does not compile uses no gas.

**Drivers.** Two drivers run the same small-step rules:
- the stack machine (production): an explicit continuation stack, constant host stack;
- the recursive driver (reference).

A property test checks that they agree on value, error and gas at random gas limits.

## 8. Deliberate changes from metakit

| # | metakit | Change here | Why |
|---|---|---|---|
| 1 | Coercion: `"1" == 1`, arithmetic on strings, booleans and null. | No coercion. | Silent coercion hides bugs. |
| 2 | Unwraps a single array argument for `+`, `*`, `min`, `max`, `merge`, `exists`. | No unwrapping. `count [array]` and `missing [[keys]]` remain as defined forms. | Ambiguous argument shapes. |
| 3 | Ints and floats are distinct (`1.5 + 0.5` is a float `2`). | One exact number kind. | Exactness, and one equality. |
| 4 | `round` rounds half away from zero. | `round` is half-even; `round_half_up` is explicit. | Unbiased rounding by default. |
| 5 | `and`, `or` and `default` evaluate everything; `all`, `some` and `none` never stop early. | Lazy, and stop at the first decisive element. | Gas pays only for work done. |
| 6 | Callback scope merges the element into the data. | `$current`, `$index` and `$accumulator` bindings. | Silent collisions. **Open (decision C).** |
| 7 | `length` and `substr` count UTF-16 code units; `trim` uses Java's notion of whitespace. | Code points; Unicode `White_Space`. | Portability. |
| 8 | Gas is unchecked `Long`, priced after the fact, and absent on failure. | Saturating, charged before the work, sized by weight, reported on failure. | Fairness, and DoS bounds. |
| 9 | `cat` on arrays is an error. | Concatenates when every argument is an array. | Programs use it. |
| 10 | Unbounded numbers and values. | The bounds of §2. | DoS. |
