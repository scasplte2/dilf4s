/**
 * The operations of language version 1, by wire name: {@link JlvmOp} is generated from `shared/schemas/jlvm.cddl`
 * (`jlvm-op`), and {@link ALL_OPS} is the generated codec's own list of them. A program naming anything else does not
 * parse, so adding an operation in a later version never changes what an existing program means.
 */
import { models } from "@kd5ujc/dilf4s-core";

/** An operation of language version 1, by its own wire name (never an alias). */
export type JlvmOp = models.jlvm.JlvmOp;

/**
 * Every operation, in the schema's order: a frozen copy of the generated codec's text alternatives, taken once when
 * this module loads, so no caller can change it, and changing the codec's own list after cannot change it either.
 * Every lookup here reads module-private copies, never an exported list.
 */
export const ALL_OPS: readonly JlvmOp[] = Object.freeze(
  ((): JlvmOp[] => {
    const shape = models.jlvm.JlvmOp.shape;
    // the generated union of texts is exactly `JlvmOp`: its alternatives are the type's members
    return shape.kind === "union" ? [...(shape.texts as readonly JlvmOp[])] : [];
  })(),
);

/** The forms the compiler builds itself: the lazy and binding forms, the callbacks and the lookups (`node.ts`). Frozen. */
export const FORM_OPS = Object.freeze(["if", "and", "or", "default", "let", "map", "filter", "reduce", "all", "some", "none", "find", "missing", "missing_some"] as const satisfies readonly JlvmOp[]);

/** A form the compiler builds itself. */
export type FormOp = (typeof FORM_OPS)[number];

/** The cryptographic and proof operations (jlvm.md section 6). Frozen. */
export const CRYPTO_OPS = Object.freeze([
  "ed25519_verify",
  "secp256k1_verify",
  "bip340_verify",
  "ecrecover",
  "bls_verify",
  "bls_aggregate_verify",
  "poseidon",
  "pmt_verify",
  "groth16_verify",
  "ecvrf_verify",
  "bn254_add",
  "bn254_mul",
  "bn254_pairing",
  "schnorr_verify",
  "sigma_verify",
  "merkle_verify",
  "merkle_consistency_verify",
  "trie_verify",
  "trie_prefix_verify",
  "trie_range_verify",
] as const satisfies readonly JlvmOp[]);

/** A cryptographic or proof operation. */
export type CryptoOp = (typeof CRYPTO_OPS)[number];

/**
 * An operation applied to evaluated arguments (jlvm.md section 5): every operation that is neither a form nor
 * cryptographic. `count` is also a callback when written with two arguments; the compiler decides that by syntax.
 */
export type StrictOp = Exclude<JlvmOp, FormOp | CryptoOp>;

/** The operations written with a single non-array argument to take that value's elements as their arguments. */
export type VariadicOp = "+" | "*" | "min" | "max" | "cat" | "merge";

// module-private: built once from the frozen lists, and reachable through no export
const BY_NAME: ReadonlyMap<string, JlvmOp> = new Map<string, JlvmOp>([
  ...ALL_OPS.map((op) => [op, op] as const),
  // the surface's aliases (jlvm.md section 1), normalised so they never change a program's identity
  ["getKey", "get"],
  ["setKey", "set"],
  ["deleteKey", "unset"],
  ["size", "length"],
]);

const FORMS: ReadonlySet<string> = new Set<string>(FORM_OPS);
const CRYPTO: ReadonlySet<string> = new Set<string>(CRYPTO_OPS);
const VARIADIC: ReadonlySet<string> = new Set<string>(["+", "*", "min", "max", "cat", "merge"] satisfies VariadicOp[]);

/** Whether `x` is a string: every predicate here asks this first, so nothing else (a Proxy included) is ever read. */
const isString = (x: unknown): x is string => typeof x === "string";

/**
 * The operation a surface name denotes: its own name, or one of the aliases `getKey`, `setKey`, `deleteKey` and `size`.
 * Anything that is not a string names none.
 */
export function opNamed(name: string): JlvmOp | undefined {
  return isString(name) ? BY_NAME.get(name) : undefined;
}

/** The operation with this own wire name: aliases are not wire names, so the canonical form never holds one. */
export function opFromWire(name: string): JlvmOp | undefined {
  const op = opNamed(name);
  return op === name ? op : undefined;
}

/** Whether `op` is a form the compiler builds itself: an own wire name in the forms' set. */
export function isForm(op: JlvmOp): op is FormOp {
  return isString(op) && FORMS.has(op);
}

/** Whether `op` is cryptographic: an own wire name in the cryptographic set. */
export function isCrypto(op: JlvmOp): op is CryptoOp {
  return isString(op) && CRYPTO.has(op);
}

/**
 * Whether `op` applies to evaluated arguments: an own wire name of this language version that is neither a form nor
 * cryptographic. An alias, an unknown name or anything that is not a string is not.
 */
export function isStrict(op: JlvmOp): op is StrictOp {
  return opFromWire(op) !== undefined && !FORMS.has(op) && !CRYPTO.has(op);
}

/** Whether `op` spreads a single non-array argument: decided by the syntax, never by an evaluated value. */
export function isVariadic(op: JlvmOp): op is VariadicOp {
  return isString(op) && VARIADIC.has(op);
}

/** Names that are not operations although they read like one, each with what to write instead (`Parser.Removed`). */
export function removed(name: string): string | undefined {
  switch (name) {
    case "prove_dlog_verify":
      return 'use a one-leaf sigma_verify over {"type": "dlog"} (docs/specs/sigma.md)';
    case "prove_dhtuple_verify":
      return 'use a one-leaf sigma_verify over {"type": "dhtuple"} (docs/specs/sigma.md)';
    case "mpt_verify":
    case "smt_verify":
      return "use trie_verify (docs/specs/trie.md)";
    case "mpt_prefix_verify":
      return "use trie_prefix_verify (docs/specs/trie.md)";
    case "noop":
      return "omit it: it does nothing";
    default:
      return undefined;
  }
}

/** Names that may not head an array, which would read as a call: `["+", 1, 2]`, `["var", "a"]` (`Parser.callName`). */
export function callName(s: string): boolean {
  return s === "var" || opNamed(s) !== undefined || removed(s) !== undefined;
}
