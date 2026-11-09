/**
 * Gas: amounts, the versioned schedule and how arguments are measured (`Gas.scala`, jlvm.md section 7). The schedule's
 * constants are the spec's table: gas prices the work of a validating node, and this package sets no prices.
 *
 * An amount is a `number`, saturating at {@link GAS_CAP} (2^53) instead of overflowing, as the reference's saturates
 * at `Long.MaxValue`: a limit is at most `Number.MAX_SAFE_INTEGER`, below the cap, so a saturated charge still only
 * ever exceeds the limit, and every amount that can be paid is exact. A sum or a product of two whole numbers below
 * 2^53 whose true value is below 2^53 is exact in a double, and one whose true value is not rounds to at least 2^53,
 * so clamping after each operation saturates exactly.
 */
import type { JValue } from "./jvalue.js";
import type { JlvmOp } from "./op.js";
import { elements, renderedBits, SEARCH_BLOCK } from "./values.js";

/** The saturation point of every amount: 2^53, above every limit. */
export const GAS_CAP = 2 ** 53;

/** The largest gas limit: `Number.MAX_SAFE_INTEGER`, below {@link GAS_CAP}. */
export const MAX_GAS_LIMIT = Number.MAX_SAFE_INTEGER;

/** 2^24, Ethereum's per-transaction cap (EIP-7825): the default limit. */
export const DEFAULT_GAS_LIMIT = 2 ** 24;

/** The saturating sum. */
export const plus = (a: number, b: number): number => Math.min(a + b, GAS_CAP);

/** The saturating product. */
export const times = (a: number, b: number): number => Math.min(a * b, GAS_CAP);

/** A bigint size (a weight) as an amount, saturated. */
export const amount = (n: bigint): number => (n >= BigInt(GAS_CAP) ? GAS_CAP : Number(n));

/** How an argument is measured. */
export type SizeUnit = "None" | "Elements" | "Weight" | "Limbs2" | "RationalLimbs2" | "RationalWeight" | "Arguments";

/** The fixed cost of a decimal conversion, in limbs ({@link GasSchedule.rendered}). */
export const RENDER_LIMBS = 6;

/** The fixed cost of a gcd, in limbs (`Limbs2`). */
export const GCD_LIMBS = 8;

function limbs(v: JValue, integers: boolean): number {
  const n =
    v.kind === "int" && integers
      ? Math.floor(v.bitLength / 64)
      : v.kind === "rational"
        ? GCD_LIMBS + Math.floor(v.numeratorBitLength / 64) + Math.floor(v.denominatorBitLength / 64)
        : 0;
  return times(n, n);
}

/** One argument's size in `unit` (`GasSchedule.units`): O(1), from cached sizes. */
export function units(unit: SizeUnit, v: JValue): number {
  switch (unit) {
    case "None":
      return 0;
    case "Elements":
      return elements(v);
    case "Weight":
      return amount(v.weight);
    case "Limbs2":
      return limbs(v, true);
    case "RationalLimbs2":
      return limbs(v, false);
    case "Arguments":
      return 1;
    case "RationalWeight":
      return v.kind === "rational" ? amount(v.weight) : 0;
  }
}

/** A charge per unit of an argument's size. */
export interface Rate {
  readonly perUnit: number;
  readonly unit: SizeUnit;
}

/**
 * Searching a map (argument 0) for a key (argument 1), as `get`, `has`, `set` and `unset` do: `perLevel`, plus
 * `perBlock` per {@link SEARCH_BLOCK} of the key's UTF-16 length, for each of the search's {@link levels}.
 */
export interface Search {
  readonly perLevel: number;
  readonly perBlock: number;
}

/**
 * Merging maps (`merge` when every argument is a map): `perLevel` per entry of the maps after the first, plus
 * `perBlock` per {@link SEARCH_BLOCK} of their weight, for each of the {@link levels} of all their entries.
 */
export interface Merging {
  readonly perLevel: number;
  readonly perBlock: number;
}

/**
 * What an operation costs: a base charge, plus a charge per unit of each argument's size. Every argument is charged at
 * the operation's rate, unless `byArgument` gives its position a rate of its own; `extra` rates are charged on every
 * argument besides.
 */
export interface OpCost {
  readonly base: number;
  readonly perUnit: number;
  readonly unit: SizeUnit;
  readonly byArgument?: readonly [number, Rate];
  readonly extra: readonly Rate[];
  readonly search?: Search;
  readonly merging?: Merging;
}

/** A map's depth bound, `ceil(log2(n + 1))` for `n` entries: the height of a perfectly balanced tree. */
export function depth(entries: number): number {
  let d = 0;
  for (let n = entries; n >= 1; n = Math.floor(n / 2)) d++;
  return d;
}

/** The levels a search of a map of `entries` entries pays for: its {@link depth}, plus one for the key itself. */
export const levels = (entries: number): number => depth(entries) + 1;

/** A {@link Search}'s charge on a map and a key: 0 unless they are a map and text. */
function searchCharge(s: Search, map: JValue, key: JValue): number {
  if (map.kind !== "map" || key.kind !== "text") return 0;
  return times(plus(times(s.perBlock, Math.floor(key.value.length / SEARCH_BLOCK)), s.perLevel), levels(map.size));
}

const c = (base: number, perUnit: number, unit: SizeUnit): OpCost => ({ base, perUnit, unit, extra: [] });
const and = (cost: OpCost, perUnit: number, unit: SizeUnit): OpCost => ({ ...cost, extra: [...cost.extra, { perUnit, unit }] });
const at = (cost: OpCost, position: number, perUnit: number, unit: SizeUnit): OpCost => ({ ...cost, byArgument: [position, { perUnit, unit }] });
const searching = (cost: OpCost, perLevel: number, perBlock: number): OpCost => ({ ...cost, search: { perLevel, perBlock } });
const mergingMaps = (cost: OpCost, perLevel: number, perBlock: number): OpCost => ({ ...cost, merging: { perLevel, perBlock } });

/**
 * Schedule version 1, operation by operation (`Gas.scala` `GasSchedule.V1`, jlvm.md section 7's table). Keyed by
 * every generated operation, so an operation without a cost does not compile.
 */
const V1: Readonly<Record<JlvmOp, OpCost>> = {
  if: c(20, 0, "None"),
  and: c(15, 0, "None"),
  or: c(15, 0, "None"),
  "!": c(25, 0, "None"),
  "!!": c(25, 0, "None"),
  default: c(15, 0, "None"),
  let: c(30, 0, "None"),
  "==": c(50, 1, "Weight"),
  "!=": c(50, 1, "Weight"),
  "===": c(50, 1, "Weight"),
  "!==": c(50, 1, "Weight"),
  "<": and(c(65, 1, "Weight"), 14, "RationalWeight"),
  "<=": and(c(65, 1, "Weight"), 14, "RationalWeight"),
  ">": and(c(65, 1, "Weight"), 14, "RationalWeight"),
  ">=": and(c(65, 1, "Weight"), 14, "RationalWeight"),
  // integer sums and products are linear; rational ones, and every quotient, normalise through a quadratic gcd
  "+": and(and(c(40, 4, "Weight"), 7, "RationalLimbs2"), 40, "Elements"),
  "-": and(and(c(40, 3, "Weight"), 7, "RationalLimbs2"), 60, "Elements"),
  "*": and(and(c(40, 10, "Weight"), 7, "RationalLimbs2"), 40, "Elements"),
  "/": and(c(340, 2, "Weight"), 9, "Limbs2"),
  "%": and(and(c(340, 2, "Weight"), 9, "Limbs2"), 8, "RationalLimbs2"),
  min: and(and(c(30, 1, "Weight"), 14, "RationalWeight"), 35, "Elements"),
  max: and(and(c(30, 1, "Weight"), 14, "RationalWeight"), 35, "Elements"),
  abs: and(c(60, 0, "None"), 2, "RationalLimbs2"),
  round: c(60, 8, "Weight"),
  round_half_up: c(140, 8, "Weight"),
  floor: c(60, 8, "Weight"),
  ceil: c(60, 8, "Weight"),
  // priced at its worst case, a 4096-bit rational result
  pow: c(20000, 0, "None"),
  map: c(40, 15, "Elements"),
  filter: c(40, 15, "Elements"),
  reduce: c(40, 8, "Elements"),
  all: c(40, 8, "Elements"),
  some: c(40, 8, "Elements"),
  none: c(40, 8, "Elements"),
  find: c(40, 8, "Elements"),
  count: c(45, 2, "Elements"),
  merge: mergingMaps(c(210, 8, "Weight"), 12, 2),
  in: and(c(70, 1, "Weight"), 1, "Elements"),
  intersect: c(50, 17, "Weight"),
  unique: c(60, 23, "Weight"),
  slice: c(120, 7, "Elements"),
  reverse: c(55, 12, "Elements"),
  flatten: c(70, 7, "Weight"),
  length: c(45, 1, "Weight"),
  cat: and(and(c(65, 1, "Weight"), 4, "Elements"), 14, "Arguments"),
  substr: c(75, 1, "Weight"),
  lower: c(45, 1, "Weight"),
  upper: c(50, 1, "Weight"),
  join: and(c(110, 1, "Weight"), 14, "Elements"),
  split: c(85, 6, "Weight"),
  trim: c(75, 1, "Weight"),
  startsWith: c(50, 1, "Weight"),
  endsWith: c(50, 1, "Weight"),
  keys: c(130, 5, "Weight"),
  values: c(80, 7, "Weight"),
  entries: c(160, 11, "Weight"),
  get: searching(c(40, 0, "None"), 3, 1),
  has: searching(c(40, 0, "None"), 3, 1),
  set: searching(c(60, 0, "None"), 18, 2),
  unset: searching(c(60, 0, "None"), 18, 2),
  exists: c(60, 1, "Elements"),
  missing: c(65, 20, "Elements"),
  missing_some: c(80, 20, "Elements"),
  typeof: c(60, 0, "None"),
  hex: c(45, 1, "Weight"),
  unhex: c(80, 2, "Weight"),
  hex_to_int: c(80, 4, "Weight"),
  bytes_to_int: c(45, 1, "Weight"),
  int_to_bytes: c(70, 2, "Weight"),
  sha256: c(95, 1, "Weight"),
  sha3_256: c(130, 1, "Weight"),
  keccak256: c(150, 1, "Weight"),
  blake2b256: c(115, 1, "Weight"),
  ed25519_verify: c(20000, 1, "Weight"),
  secp256k1_verify: c(7200, 1, "Weight"),
  bip340_verify: c(9000, 1, "Weight"),
  ecrecover: c(32500, 0, "None"),
  bls_verify: c(290_000, 1, "Weight"),
  // the message is only hashed: it pays the hashing rate per unit of weight, as sigma_verify's does
  bls_aggregate_verify: at(c(250_000, 33000, "Elements"), 1, 1, "Weight"),
  poseidon: c(1800, 3900, "Elements"),
  pmt_verify: c(1000, 11000, "Elements"),
  groth16_verify: c(480_000, 1, "Weight"),
  ecvrf_verify: c(130_000, 1, "Weight"),
  bn254_add: c(700, 0, "None"),
  bn254_mul: c(14500, 0, "None"),
  bn254_pairing: c(135_000, 470, "Weight"),
  // the message is only hashed: it pays the hashing rate, not the curve rate
  schnorr_verify: at(c(5000, 450, "Weight"), 1, 1, "Weight"),
  sigma_verify: at(c(5000, 450, "Weight"), 2, 1, "Weight"),
  merkle_verify: c(1000, 8, "Weight"),
  merkle_consistency_verify: c(500, 8, "Weight"),
  trie_verify: c(500, 15, "Weight"),
  trie_prefix_verify: c(20000, 17, "Weight"),
  trie_range_verify: c(20000, 17, "Weight"),
};

/** A gas schedule: versioned data. A result is a function of the program, the input, the language version and the schedule version. */
export interface GasSchedule {
  /** The schedule version. */
  readonly version: number;
  /** What each operation costs. */
  readonly costs: Readonly<Record<JlvmOp, OpCost>>;
  /** Each literal node. */
  readonly literal: number;
  /** Each item of an array or map expression, each condition `if` tests, each `let` insertion. */
  readonly element: number;
  /** Each variable read. */
  readonly varAccess: number;
  /** Each path segment of a read. */
  readonly perPathSegment: number;
  /** Each run of a callback body. */
  readonly callbackRun: number;
  /** Per unit of `(RENDER_LIMBS + floor(bits / 64))^2` of each number `cat` and `join` render. */
  readonly rendering: number;
}

/**
 * `o` and every object and array it holds, frozen: a schedule is shared by every evaluation, so no caller can change
 * one by assigning to a nested record. An explicit stack; the schedule is a tree of plain records.
 */
function frozen<T extends object>(o: T): T {
  const todo: object[] = [o];
  for (let x = todo.pop(); x !== undefined; x = todo.pop()) {
    Object.freeze(x);
    for (const v of Object.values(x)) if (typeof v === "object" && v !== null && !Object.isFrozen(v)) todo.push(v as object);
  }
  return o;
}

/**
 * Version 1 (`GasSchedule.V1`): frozen throughout, its costs, rates, searches and merges included. `evaluate` takes
 * only a published schedule, by identity, so a schedule a caller builds, or copies, is refused.
 */
export const SCHEDULE_V1: GasSchedule = frozen({
  version: 1,
  costs: V1,
  literal: 10,
  element: 11,
  varAccess: 70,
  perPathSegment: 4,
  callbackRun: 18,
  rendering: 2,
});

/** The latest schedule. */
export const LATEST_SCHEDULE: GasSchedule = SCHEDULE_V1;

/** The rate of argument `position`. */
function rate(cost: OpCost, position: number): Rate {
  return cost.byArgument?.[0] === position ? cost.byArgument[1] : { perUnit: cost.perUnit, unit: cost.unit };
}

/** The base charge of `op`. */
export const base = (s: GasSchedule, op: JlvmOp): number => s.costs[op].base;

/** The per-unit charge of `op`. */
export const perUnit = (s: GasSchedule, op: JlvmOp): number => s.costs[op].perUnit;

/** A path parsed at run time (a computed `var` path, a `missing` key) is charged at `split`'s rate per unit of its weight. */
export const pathText = (s: GasSchedule, path: JValue): number => times(perUnit(s, "split"), amount(path.weight));

/** Rendering numbers as decimal text (`cat`, `join`): `rendering` per unit of `(RENDER_LIMBS + floor(bits/64))^2`. */
export function rendered(s: GasSchedule, numbers: readonly JValue[]): number {
  return numbers.reduce((g, v) => {
    const l = Math.floor(renderedBits(v) / 64) + RENDER_LIMBS;
    return plus(g, times(s.rendering, times(l, l)));
  }, 0);
}

/** The size charge of `op` on its evaluated arguments: each argument at its position's rate, plus any extra rate. */
export const sized = (s: GasSchedule, op: JlvmOp, args: readonly JValue[]): number => sizedWithin(s, op, args, GAS_CAP);

/**
 * {@link sized}, stopping as soon as the sum exceeds `cap` (the gas left): the sum so far, already more than can be
 * paid, is then the charge. Every argument's size is O(1), and an argument list a caller supplies (a spread's array)
 * is walked only as far as the gas left can pay for. A {@link Search} is charged with the second argument, on the
 * first two; {@link Merging} after the last.
 */
export function sizedWithin(s: GasSchedule, op: JlvmOp, args: readonly JValue[], cap: number): number {
  const cost = s.costs[op];
  let sum = 0;
  let first: JValue | undefined;
  let allMaps = false;
  let entries = 0;
  let laterEntries = 0;
  let laterWeight = 0;
  for (const [i, v] of args.entries()) {
    const own = rate(cost, i);
    sum = plus(sum, times(own.perUnit, units(own.unit, v)));
    for (const r of cost.extra) sum = plus(sum, times(r.perUnit, units(r.unit, v)));
    if (cost.search !== undefined && i === 1 && first !== undefined) sum = plus(sum, searchCharge(cost.search, first, v));
    if (sum > cap) return sum;
    const [isMap, n] = v.kind === "map" ? [true, v.size] : [false, 0];
    const later = first !== undefined;
    allMaps = isMap && (allMaps || !later);
    entries = plus(entries, n);
    if (later) {
      laterEntries = plus(laterEntries, n);
      laterWeight = plus(laterWeight, amount(v.weight));
    }
    first ??= v;
  }
  if (cost.merging === undefined || !allMaps) return sum;
  const m = cost.merging;
  return plus(sum, times(plus(times(m.perLevel, laterEntries), times(m.perBlock, Math.floor(laterWeight / SEARCH_BLOCK))), levels(entries)));
}
