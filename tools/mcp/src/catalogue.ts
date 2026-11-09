/**
 * The operations of language version 1 as an agent writes them: for each, what it does, how many arguments it takes,
 * what each may hold, an example, and its gas cost. The list is the generated one (`ALL_OPS`), and the costs are the
 * schedule's own (`SCHEDULE_V1`); the arities and kinds are written here, from jlvm.md sections 4 to 6 and the
 * handlers. The tests check the arities and examples against the engine (every example runs, and every arity outside
 * the range is refused as the engine refuses it); the kinds are documented, not checked against the engine.
 */
import { ALL_OPS, CRYPTO_OPS, FORM_OPS, isVariadic, SCHEDULE_V1, type JlvmOp, type OpCost } from "@kd5ujc/dilf4s-jlvm";

/**
 * What an argument may hold, by `typeof`'s names (jlvm.md section 2), with `integer` for a number that must be whole
 * and `any` for every kind.
 */
export type Kind = "null" | "boolean" | "integer" | "number" | "string" | "bytes" | "array" | "object" | "any";

/** One argument position. */
export interface Param {
  readonly name: string;
  readonly kinds: readonly Kind[];
  /** May be left out (it is last, or before other optional ones). */
  readonly optional?: true;
  /** Repeats: every further argument is one of these (as a group, with the other repeating ones). */
  readonly repeats?: true;
}

/** A call that runs: a program and its data, both surface JSON. */
export interface Example {
  readonly program: string;
  readonly data: string;
}

/** How an operation is called. */
export interface Signature {
  readonly summary: string;
  /** The number of arguments: from `min` to `max`, or any number from `min` when `max` is null. */
  readonly arity: { readonly min: number; readonly max: number | null };
  readonly params: readonly Param[];
  readonly example: Example;
}

const ANY: readonly Kind[] = ["any"];
const p = (name: string, ...kinds: Kind[]): Param => ({ name, kinds });
const opt = (name: string, ...kinds: Kind[]): Param => ({ name, kinds, optional: true });
const rep = (name: string, ...kinds: Kind[]): Param => ({ name, kinds, repeats: true });
const sig = (summary: string, arity: readonly [number, number | null], params: readonly Param[], program: string, data = "null"): Signature => ({
  summary,
  arity: { min: arity[0], max: arity[1] },
  params,
  example: { program, data },
});

const XS = '{"xs": [1, 2, 3]}';
const unaryNumber = (summary: string, op: string, x: string): Signature => sig(summary, [1, 1], [p("x", "number")], `{"${op}": [${x}]}`);
const comparison = (summary: string, op: string): Signature => sig(summary, [2, 2], [p("a", "any"), p("b", "any")], `{"${op}": [{"var": "a"}, [1, 2]]}`, '{"a": [1, 2]}');
const ordering = (summary: string, op: string, between: boolean): Signature =>
  sig(
    summary,
    [2, between ? 3 : 2],
    between ? [p("a", "number", "string"), p("b", "number", "string"), opt("c", "number", "string")] : [p("a", "number", "string"), p("b", "number", "string")],
    between ? `{"${op}": [1, {"var": "x"}, 10]}` : `{"${op}": [{"var": "x"}, 2]}`,
    '{"x": 5}',
  );
const callback = (summary: string, op: string, body: string): Signature => sig(summary, [2, 2], [p("collection", "array", "null"), p("body", ...ANY)], `{"${op}": [{"var": "xs"}, ${body}]}`, XS);
const digest = (name: string): Signature => sig(`The 32-byte ${name} digest of bytes, or of a string's UTF-8 encoding.`, [1, 1], [p("input", "bytes", "string")], `{"${name === "SHA-256" ? "sha256" : name === "SHA3-256" ? "sha3_256" : name === "Keccak-256" ? "keccak256" : "blake2b256"}": ["abc"]}`);

const B32 = `0x${"00".repeat(32)}`;

/** Every operation's signature: a record over the generated `JlvmOp`, so a missing or an unknown one does not compile. */
const SIGNATURES: Readonly<Record<JlvmOp, Signature>> = {
  if: sig(
    "[c1, t1, (c2, t2)..., else]: the first ti whose ci is truthy, else `else`, or null without it. Lazy: only the conditions tested and the branch taken are evaluated.",
    [2, null],
    [rep("condition", ...ANY), rep("then", ...ANY), opt("else", ...ANY)],
    '{"if": [{"<": [{"var": "x"}, 0]}, "negative", "non-negative"]}',
    '{"x": -3}',
  ),
  and: sig("The first falsy value, else the last; true with no arguments. Lazy: stops at the first falsy value.", [0, null], [rep("value", ...ANY)], '{"and": [true, {"var": "ok"}]}', '{"ok": 1}'),
  or: sig("The first truthy value, else the last; false with no arguments. Lazy: stops at the first truthy value.", [0, null], [rep("value", ...ANY)], '{"or": [{"var": "a"}, "fallback"]}', '{"a": ""}'),
  "!": sig("Negated truthiness. Falsy are null, false, 0, \"\", empty bytes, [] and {}.", [1, 1], [p("value", ...ANY)], '{"!": [0]}'),
  "!!": sig("Truthiness.", [1, 1], [p("value", ...ANY)], '{"!!": ["x"]}'),
  default: sig("The first truthy value, else null. Lazy.", [0, null], [rep("value", ...ANY)], '{"default": [{"var": "name"}, "anonymous"]}', '{"name": ""}'),
  let: sig(
    "[[[name, expr], ...], body], or [{name: expr, ...}, body]: binds each name (read as {\"var\": \"$name\"}) for the later bindings and the body.",
    [2, 2],
    [p("bindings", "array", "object"), p("body", ...ANY)],
    '{"let": [[["unit", {"var": "price"}], ["n", 3]], {"*": [{"var": "$unit"}, {"var": "$n"}]}]}',
    '{"price": 7}',
  ),
  "==": comparison("Structural equality of canonical values; no coercion.", "=="),
  "!=": comparison("Structural inequality.", "!="),
  "===": comparison("Structural equality (the same as ==).", "==="),
  "!==": comparison("Structural inequality (the same as !=).", "!=="),
  "<": ordering("Less than: numbers with numbers, strings with strings (by code point); three arguments test a < b < c.", "<", true),
  "<=": ordering("Less than or equal; three arguments test a <= b <= c.", "<=", true),
  ">": ordering("Greater than.", ">", false),
  ">=": ordering("Greater than or equal.", ">=", false),
  "+": sig("The exact sum; 0 of no arguments. Variadic: {\"+\": e} sums the elements of the array e evaluates to.", [0, null], [rep("x", "number")], '{"+": [1, {"var": "x"}, 0.5]}', '{"x": 2}'),
  "-": sig("Negation of one argument, or the difference of two.", [1, 2], [p("a", "number"), opt("b", "number")], '{"-": [10, {"var": "x"}]}', '{"x": 4}'),
  "*": sig("The exact product; 1 of no arguments. Variadic.", [0, null], [rep("x", "number")], '{"*": [3, {"var": "x"}]}', '{"x": 1.5}'),
  "/": sig("Exact division: 1/3 is a rational. By zero, an Arithmetic error.", [2, 2], [p("a", "number"), p("b", "number")], '{"/": [1, 3]}'),
  "%": sig("Truncating remainder, x - y·trunc(x/y), with the sign of x.", [2, 2], [p("x", "number"), p("y", "number")], '{"%": [-7, 3]}'),
  min: sig("The exact minimum, of at least one argument. Variadic.", [1, null], [rep("x", "number")], '{"min": [3, 1, 2]}'),
  max: sig("The exact maximum, of at least one argument. Variadic.", [1, null], [rep("x", "number")], '{"max": [1, 2.5, -3]}'),
  abs: unaryNumber("Absolute value.", "abs", "-2.5"),
  round: unaryNumber("Rounds half to even.", "round", "2.5"),
  round_half_up: unaryNumber("Rounds half away from zero.", "round_half_up", "2.5"),
  floor: unaryNumber("Floor.", "floor", "-1.5"),
  ceil: unaryNumber("Ceiling.", "ceil", "-1.5"),
  pow: sig("base^exponent, the exponent an integer in -999..999 (otherwise an Arithmetic error); the result's size is checked first.", [2, 2], [p("base", "number"), p("exponent", "number")], '{"pow": [2, 10]}'),
  map: callback("Each element through the body, which reads $current, $index and the element's keys. Null is an empty collection.", "map", '{"*": [{"var": "$current"}, 2]}'),
  filter: callback("The elements whose body is truthy.", "filter", '{">": [{"var": "$current"}, 1]}'),
  reduce: sig(
    "[array, body, initial]: folds the elements, the body reading $accumulator and $current; without initial, the first element starts and an empty array gives null.",
    [2, 3],
    [p("collection", "array", "null"), p("body", ...ANY), opt("initial", ...ANY)],
    '{"reduce": [{"var": "xs"}, {"+": [{"var": "$accumulator"}, {"var": "$current"}]}, 0]}',
    XS,
  ),
  all: callback("Whether the body is truthy for every element; false for an empty array. Stops early.", "all", '{">": [{"var": "$current"}, 0]}'),
  some: sig(
    "[array, body] or [array, body, min]: whether the body is truthy for at least min elements (1 unless given). Stops early.",
    [2, 3],
    [p("collection", "array", "null"), p("body", ...ANY), opt("min", "integer")],
    '{"some": [{"var": "xs"}, {">": [{"var": "$current"}, 1]}, 2]}',
    XS,
  ),
  none: callback("Whether the body is truthy for no element. Stops early.", "none", '{">": [{"var": "$current"}, 5]}'),
  find: callback("The first element whose body is truthy, else null. Stops there.", "find", '{">": [{"var": "$current"}, 1]}'),
  count: sig(
    "[array]: its length; [array, body]: how many elements the body is truthy for (a callback, decided by the syntax).",
    [1, 2],
    [p("collection", "array"), opt("body", ...ANY)],
    '{"count": [{"var": "xs"}]}',
    XS,
  ),
  merge: sig("All objects: merged left to right, the last key winning. Otherwise arrays are concatenated and other values appended. Variadic.", [0, null], [rep("value", ...ANY)], '{"merge": [{"var": "a"}, {"var": "b"}]}', '{"a": {"x": 1}, "b": {"y": 2}}'),
  in: sig(
    "[needle, haystack]: a substring of a string, a byte substring of bytes, or a member of an array (structural equality); a null needle is false.",
    [2, 2],
    [p("needle", ...ANY), p("haystack", "string", "bytes", "array")],
    '{"in": ["b", "abc"]}',
  ),
  intersect: sig("Whether every element of the first array is in the second; a null first is true, a null second false.", [2, 2], [p("subset", "array", "null"), p("superset", "array", "null")], '{"intersect": [[1, 2], {"var": "xs"}]}', XS),
  unique: sig("The first occurrence of each element, in order.", [1, 1], [p("array", "array")], '{"unique": [[1, 2, 1, 3]]}'),
  slice: sig("[array, start] or [array, start, end], with JavaScript's bounds (negative counts from the end).", [2, 3], [p("array", "array"), p("start", "integer"), opt("end", "integer")], '{"slice": [{"var": "xs"}, 1]}', XS),
  reverse: sig("The array reversed.", [1, 1], [p("array", "array")], '{"reverse": [{"var": "xs"}]}', XS),
  flatten: sig("Flattens one level.", [1, 1], [p("array", "array")], '{"flatten": [[[1, 2], 3, [4]]]}'),
  length: sig("Code points of a string, elements of an array, bytes of bytes, entries of an object. Alias: size.", [1, 1], [p("value", "string", "array", "bytes", "object")], '{"length": ["héllo"]}'),
  cat: sig(
    "Concatenates strings, numbers (exact decimals), booleans and null (empty); when every argument is an array, the arrays; when every one is bytes, the bytes. Variadic.",
    [0, null],
    [rep("piece", "string", "number", "boolean", "null", "array", "bytes")],
    '{"cat": ["total: ", {"var": "x"}]}',
    '{"x": 2.5}',
  ),
  substr: sig("[text, start] or [text, start, length], in code points; a negative start counts from the end, a negative length drops from it.", [2, 3], [p("text", "string"), p("start", "integer"), opt("length", "integer")], '{"substr": ["ledger", 1, 3]}'),
  lower: sig("ASCII lower case; every other code point is unchanged.", [1, 1], [p("text", "string")], '{"lower": ["ABC"]}'),
  upper: sig("ASCII upper case.", [1, 1], [p("text", "string")], '{"upper": ["abc"]}'),
  join: sig("[array, separator]: the pieces as cat renders them, joined.", [2, 2], [p("array", "array"), p("separator", "string")], '{"join": [{"var": "xs"}, "-"]}', XS),
  split: sig("[text, separator]: every piece between separators, empty ones included; an empty separator is a Domain error.", [2, 2], [p("text", "string"), p("separator", "string")], '{"split": ["a,b,,c", ","]}'),
  trim: sig("Without leading and trailing Unicode White_Space.", [1, 1], [p("text", "string")], '{"trim": ["  x  "]}'),
  startsWith: sig("Whether the text starts with the prefix; null on either side is false.", [2, 2], [p("text", "string", "null"), p("prefix", "string", "null")], '{"startsWith": ["ledger", "led"]}'),
  endsWith: sig("Whether the text ends with the suffix; null on either side is false.", [2, 2], [p("text", "string", "null"), p("suffix", "string", "null")], '{"endsWith": ["ledger", "ger"]}'),
  keys: sig("An object's keys, in code-point order; null gives null.", [1, 1], [p("object", "object", "null")], '{"keys": [{"var": ""}]}', '{"b": 1, "a": 2}'),
  values: sig("An object's values, in code-point order of their keys; null gives null.", [1, 1], [p("object", "object", "null")], '{"values": [{"var": ""}]}', '{"b": 1, "a": 2}'),
  entries: sig("An object's [key, value] pairs, in code-point order of the keys; null gives null.", [1, 1], [p("object", "object", "null")], '{"entries": [{"var": ""}]}', '{"b": 1, "a": 2}'),
  get: sig("[object, key] or [object, key, default]: the value at the key, else null or the default. Alias: getKey.", [2, 3], [p("object", "object"), p("key", "string"), opt("default", ...ANY)], '{"get": [{"var": ""}, "a", 0]}', '{"a": 5}'),
  has: sig("Whether the object holds the key.", [2, 2], [p("object", "object"), p("key", "string")], '{"has": [{"var": ""}, "a"]}', '{"a": 5}'),
  set: sig("[object, key, value]: the object with the key set; a null value removes it. Alias: setKey.", [3, 3], [p("object", "object"), p("key", "string"), p("value", ...ANY)], '{"set": [{"var": ""}, "b", 6]}', '{"a": 5}'),
  unset: sig("The object without the key. Alias: deleteKey.", [2, 2], [p("object", "object"), p("key", "string")], '{"unset": [{"var": ""}, "a"]}', '{"a": 5, "b": 6}'),
  exists: sig("Whether no argument is null.", [0, null], [rep("value", ...ANY)], '{"exists": [{"var": "a"}, {"var": "b"}]}', '{"a": 1}'),
  missing: sig(
    "[key, ...] or [[key, ...]]: the data paths among the keys that are absent from the data (inside a callback, the element first).",
    [0, null],
    [rep("key", "string", "array")],
    '{"missing": ["a", "b.c"]}',
    '{"a": 1}',
  ),
  missing_some: sig("[min, [key, ...]]: [] when at least min of the keys are present, else the absent ones.", [2, 2], [p("min", "integer"), p("keys", "array")], '{"missing_some": [1, ["a", "b"]]}', '{"a": 1}'),
  typeof: sig("The kind: null, boolean, number, string, bytes, array or object.", [1, 1], [p("value", ...ANY)], '{"typeof": [{"var": ""}]}', '"0x00"'),
  hex: sig("Bytes as 0x text.", [1, 1], [p("bytes", "bytes")], '{"hex": ["0xc0ffee"]}'),
  unhex: sig("Bytes, or 0x lowercase hex text, as bytes; other text is a Domain error.", [1, 1], [p("input", "bytes", "string")], '{"unhex": [{"var": "h"}]}', '{"h": "0x00ff"}'),
  hex_to_int: sig("Bytes, or 0x hex text, read as an unsigned big-endian integer.", [1, 1], [p("input", "bytes", "string")], '{"hex_to_int": ["0x0100"]}'),
  bytes_to_int: sig("Bytes read as an unsigned big-endian integer.", [1, 1], [p("bytes", "bytes")], '{"bytes_to_int": ["0x0100"]}'),
  int_to_bytes: sig(
    "[n] or [n, width]: n >= 0 as unsigned big-endian bytes, minimal (0 is empty) or left-padded to width (0..1024).",
    [1, 2],
    [p("n", "integer"), opt("width", "integer")],
    '{"int_to_bytes": [256, 4]}',
  ),
  sha256: digest("SHA-256"),
  sha3_256: digest("SHA3-256"),
  keccak256: digest("Keccak-256"),
  blake2b256: digest("BLAKE2b-256"),
  ed25519_verify: sig(
    "[key32, message, signature64]: Ed25519 under ZIP-215 rules. Malformed input is a Crypto error; a signature that does not verify is false.",
    [3, 3],
    [p("key", "bytes"), p("message", "bytes"), p("signature", "bytes")],
    '{"ed25519_verify": [{"var": "key"}, {"var": "message"}, {"var": "signature"}]}',
    `{"key": "0xedffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f", "message": "0x5a63617368", "signature": "0x01${"00".repeat(63)}"}`,
  ),
  secp256k1_verify: sig(
    "[key33, message, signature64]: ECDSA over SHA-256 of the message; compact r||s, low-S.",
    [3, 3],
    [p("key", "bytes"), p("message", "bytes"), p("signature", "bytes")],
    '{"secp256k1_verify": [{"var": "key"}, {"var": "message"}, {"var": "signature"}]}',
    '{"key": "0x024d4b6cd1361032ca9bd2aeb9d900aa4d45d9ead80ac9423374c451a7254d0766", "message": "0x7472616e7366657220313020746f20626f62", "signature": "0xc318b82b06e5974f2641ce82a7774c10d1c27b60bb1697c670488c7c4169640947d474aad18aef03eb3860fc3f86d638c232a7f9b7e0a24ebf11e14ef0041900"}',
  ),
  bip340_verify: sig(
    "[key32, message32, signature64]: BIP-340 Schnorr; the message must be exactly 32 bytes.",
    [3, 3],
    [p("key", "bytes"), p("message", "bytes"), p("signature", "bytes")],
    '{"bip340_verify": [{"var": "key"}, {"var": "message"}, {"var": "signature"}]}',
    '{"key": "0x531fe6068134503d2723133227c867ac8fa6c83c537e9a44c3c5bdbdcb1fe337", "message": "0x0707070707070707070707070707070707070707070707070707070707070707", "signature": "0x7490acf84b919d4439b51a748a510c41e9e88f4b771ee49b12498a402d00c5535e55d609ea34510bf2a4b57405a0837e1fa3ac89117300b13d50be81284bc1b5"}',
  ),
  ecrecover: sig(
    "[digest32, signature65]: Ethereum's signer recovery, r||s||v with v in {27, 28} or {0, 1}; the 20-byte address. A signature that recovers no key is a Crypto error.",
    [2, 2],
    [p("digest", "bytes"), p("signature", "bytes")],
    `{"ecrecover": ["${B32}", "0x${"00".repeat(64)}1b"]}`,
  ),
  bls_verify: sig(
    "[key48, message, signature96]: BLS12-381, minimal-pubkey-size, proof-of-possession ciphersuite.",
    [3, 3],
    [p("key", "bytes"), p("message", "bytes"), p("signature", "bytes")],
    '{"bls_verify": ["0x864350e49b8b46468478af45cfb9c167357c8701e0c7a2dcd401028de506080f632400e2059ab58532eb2b3912078d80", "0x636f6e7374656c6c6174696f6e2d736e617073686f742d30783031", "0xa816e2440371eea63b85484f0111914874974cfb8f83833b214ba365bc1bc46cfd070d75c8decb6e9d9bcea0e2a2b92214cfe0bed5c00a7702741a2e92186454f76ba5e4e86804908e7a2f38a0f123941b3513bff5a4af6951c6c7a8e61b04ee"]}',
  ),
  bls_aggregate_verify: sig(
    "[[key48, ...], message, signature96]: fast aggregate verification, one message signed by every key (each key's proof of possession checked when it was admitted).",
    [3, 3],
    [p("keys", "array"), p("message", "bytes"), p("signature", "bytes")],
    '{"bls_aggregate_verify": [["0xa491d1b0ecd9bb917989f0e74f0dea0422eac4a873e5e2644f368dffb9a6e20fd6e10c1b77654d067c0618f6e5a7f79a", "0xb301803f8b5ac4a1133581fc676dfedc60d891dd5fa99028805e5ea5b08d3491af75d0707adab3b70c6a6a580217bf81", "0xb53d21a4cfd562c469cc81514d4ce5a6b577d8403d32a394dc265dd190b47fa9f829fdd7963afdf972e5e77854051f6f"], "0xabababababababababababababababababababababababababababababababab", "0x9712c3edd73a209c742b8250759db12549b3eaf43b5ca61376d9f30e2747dbcf842d8b2ac0901d2a093713e20284a7670fcf6954e9ab93de991bb9b313e664785a075fc285806fa5224c82bde146561b446ccfc706a64b8579513cfc4ff1d930"]}',
  ),
  poseidon: sig(
    "Poseidon over BN254 of 1 to 4 field elements (32 bytes each, below r), as separate arguments or one array: 32 bytes.",
    [1, 4],
    [rep("input", "bytes", "array")],
    `{"poseidon": ["0x${"00".repeat(31)}01"]}`,
  ),
  pmt_verify: sig(
    "[root, leaf, index, [sibling, ...]]: a Poseidon Merkle tree inclusion, at most 256 siblings, root first.",
    [4, 4],
    [p("root", "bytes"), p("leaf", "bytes"), p("index", "integer"), p("siblings", "array")],
    `{"pmt_verify": ["${B32}", "${B32}", 0, []]}`,
  ),
  groth16_verify: sig(
    "[vkey32, publicValues, proof]: an SP1 Groth16 proof of a program (by its verifying-key hash) with these public values (docs/specs/zk.md).",
    [3, 3],
    [p("vkey", "bytes"), p("publicValues", "bytes"), p("proof", "bytes")],
    `{"groth16_verify": ["${B32}", "0x", "0x"]}`,
  ),
  ecvrf_verify: sig(
    "[key32, alpha, proof80]: RFC 9381 ECVRF-EDWARDS25519-SHA512-TAI; {valid: true, beta} or {valid: false}.",
    [3, 3],
    [p("key", "bytes"), p("alpha", "bytes"), p("proof", "bytes")],
    '{"ecvrf_verify": ["0x3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c", "0x72", "0xf3141cd382dc42909d19ec5110469e4feae18300e94f304590abdced48aed5933bf0864a62558b3ed7f2fea45c92a465301b3bbf5e3e54ddf2d935be3b67926da3ef39226bbc355bdc9850112c8f4b02"]}',
  ),
  bn254_add: sig(
    "[g1, g1]: the sum of two BN254 G1 points, 64 bytes x||y each, (0,0) the point at infinity.",
    [2, 2],
    [p("a", "bytes"), p("b", "bytes")],
    `{"bn254_add": [{"var": "g"}, {"var": "g"}]}`,
    `{"g": "0x${"00".repeat(31)}01${"00".repeat(31)}02"}`,
  ),
  bn254_mul: sig("[g1, scalar32]: a G1 point times a scalar, reduced mod r.", [2, 2], [p("point", "bytes"), p("scalar", "bytes")], `{"bn254_mul": ["0x${"00".repeat(31)}01${"00".repeat(31)}02", "0x${"00".repeat(31)}02"]}`),
  bn254_pairing: sig(
    "At most 64 [g1, g2] pairs (G2 128 bytes, imaginary part first; more is a Crypto error), as separate arguments or one array: whether the product of their pairings is 1. No pairs is true.",
    [0, 64],
    [rep("pair", "array")],
    '{"bn254_pairing": []}',
  ),
  schnorr_verify: sig(
    "[key33, message, proof56]: a one-leaf sigma_verify of a dlog proposition (Ergo's P2PK signature, docs/specs/sigma.md).",
    [3, 3],
    [p("key", "bytes"), p("message", "bytes"), p("proof", "bytes")],
    `{"schnorr_verify": ["0x02${"00".repeat(31)}01", "0x6869", "0x${"00".repeat(56)}"]}`,
  ),
  sigma_verify: sig(
    "[proposition, proof, message]: a sigma protocol proof over secp256k1, byte-compatible with Ergo. A proposition is {type: dlog, pk}, {type: dhtuple, g, h, u, v}, {type: and | or, children} or {type: threshold, k, children}.",
    [3, 3],
    [p("proposition", "object"), p("proof", "bytes"), p("message", "bytes")],
    '{"sigma_verify": [{"type": "dlog", "pk": {"var": "pk"}}, "0x", "0x6869"]}',
    `{"pk": "0x02${"00".repeat(31)}01"}`,
  ),
  merkle_verify: sig(
    "[head, leaf, proof] or with \"rfc6962\" (any other fourth argument, or a fifth, is a Domain error): whether the proof shows the leaf (committed as merkle.md section 2 says) in the tree of the trusted head {root, size}.",
    [3, 4],
    [p("head", "object"), p("leaf", ...ANY), p("proof", ...ANY), opt("profile", "string")],
    '{"merkle_verify": [{"var": "head"}, {"var": "leaf"}, {"var": "proof"}]}',
    '{"head": {"root": "0x22187c2eed7374813f9b811b31f35a1b6c45dd1c0914c0814024f83dabdeba19", "size": 5}, "leaf": "0x10", "proof": {"index": 2, "path": ["0xa4468dd5a65dc2dc9ddcf5217277d117dc759b26fca3b67e1fd1cb033735c6d6", "0x9b725927062ea8f90e58d47f79007fbb1d96ea0c91f5fb0323d8be331704048e", "0x639da757a29b401080c9d6c16bf95d069ffc4fa42f3b15d1af585102095fe41b"], "size": 5}}',
  ),
  merkle_consistency_verify: sig(
    "[oldHead, newHead, proof] or with \"rfc6962\": whether the proof shows the older tree is a prefix of the newer.",
    [3, 4],
    [p("oldHead", "object"), p("newHead", "object"), p("proof", ...ANY), opt("profile", "string")],
    '{"merkle_consistency_verify": [{"var": "old"}, {"var": "new"}, {"var": "proof"}]}',
    '{"old": {"root": "0x59c04ec13f9fba50a5bff434a4c0f69ea391381e23ab52d15065b5f83fd58275", "size": 2}, "new": {"root": "0x1c702077ae5a63db9b6911b4ca96cdc039fa85babeb67b46208cbe6dded5dda0", "size": 5}, "proof": {"new": 5, "old": 2, "path": ["0x685203768bf33fea6c9643e96fee66496f6be68a2b6df5707f1493cbf00020cb", "0x9cafc54b5a2e771a5e78afcfbc3d44160ecde8658dbc6063d530ab3523f9d758"]}}',
  ),
  trie_verify: sig(
    "[mode, root, key, value, proof]: whether the binary Patricia trie proof shows the key holding the value (null claims absence); a mode is {fixed: n} or {variable: max}, written {\"object\": {\"fixed\": n}} since a one-key object is otherwise read as an operation. A proof that does not reach the key is a Crypto error.",
    [5, 5],
    [p("mode", "object"), p("root", "bytes"), p("key", "bytes"), p("value", ...ANY), p("proof", ...ANY)],
    `{"trie_verify": [{"object": {"fixed": 2}}, "${B32}", "0x0000", "0x01", []]}`,
  ),
  trie_prefix_verify: sig(
    "[mode, root, prefix, entries, proof]: whether the [key, value] entries, in key order, are exactly the trie's under the prefix.",
    [5, 5],
    [p("mode", "object"), p("root", "bytes"), p("prefix", "bytes"), p("entries", "array"), p("proof", ...ANY)],
    `{"trie_prefix_verify": [{"object": {"fixed": 2}}, "${B32}", "0x00", [], []]}`,
  ),
  trie_range_verify: sig(
    "[mode, root, from, until, entries, proof]: whether the entries are exactly the trie's in [from, until); a null until is unbounded.",
    [6, 6],
    [p("mode", "object"), p("root", "bytes"), p("from", "bytes"), p("until", "bytes", "null"), p("entries", "array"), p("proof", ...ANY)],
    `{"trie_range_verify": [{"object": {"fixed": 2}}, "${B32}", "0x0000", null, [], []]}`,
  ),
};

/** The surface aliases (jlvm.md section 1), each naming an operation by another name. */
const ALIASES: Readonly<Partial<Record<JlvmOp, readonly string[]>>> = { get: ["getKey"], set: ["setKey"], unset: ["deleteKey"], length: ["size"] };

/** How an operation is evaluated: a form the compiler builds (lazy, binding or callback), a strict operation, or a cryptographic one. */
export type OpClass = "form" | "strict" | "crypto";

const FORMS: ReadonlySet<string> = new Set<string>(FORM_OPS);
const CRYPTO: ReadonlySet<string> = new Set<string>(CRYPTO_OPS);

/** An operation's gas cost, as the schedule holds it (jlvm.md section 7). */
export interface Cost {
  readonly base: number;
  readonly perUnit: number;
  readonly unit: string;
  readonly byArgument?: { readonly position: number; readonly perUnit: number; readonly unit: string };
  readonly extra: readonly { readonly perUnit: number; readonly unit: string }[];
  readonly search?: { readonly perLevel: number; readonly perBlock: number };
  readonly merging?: { readonly perLevel: number; readonly perBlock: number };
}

/** An operation in the catalogue. */
export interface Entry extends Signature {
  readonly op: JlvmOp;
  readonly aliases: readonly string[];
  readonly class: OpClass;
  readonly variadic: boolean;
  readonly cost: Cost;
}

/** A schedule's cost as plain data. */
export function costOf(c: OpCost): Cost {
  return {
    base: c.base,
    perUnit: c.perUnit,
    unit: c.unit,
    ...(c.byArgument === undefined ? {} : { byArgument: { position: c.byArgument[0], perUnit: c.byArgument[1].perUnit, unit: c.byArgument[1].unit } }),
    extra: c.extra.map((r) => ({ perUnit: r.perUnit, unit: r.unit })),
    ...(c.search === undefined ? {} : { search: { perLevel: c.search.perLevel, perBlock: c.search.perBlock } }),
    ...(c.merging === undefined ? {} : { merging: { perLevel: c.merging.perLevel, perBlock: c.merging.perBlock } }),
  };
}

/** Every operation, in the schema's order. */
export const CATALOGUE: readonly Entry[] = ALL_OPS.map((op) => ({
  op,
  aliases: ALIASES[op] ?? [],
  class: FORMS.has(op) ? "form" : CRYPTO.has(op) ? "crypto" : "strict",
  variadic: isVariadic(op),
  ...SIGNATURES[op],
  cost: costOf(SCHEDULE_V1.costs[op]),
}));
