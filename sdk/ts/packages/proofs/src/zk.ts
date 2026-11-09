/**
 * BN254 (`alt_bn128`) group operations on exact-width bytes, byte-compatible with the EVM precompiles (EIP-196/197,
 * docs/specs/zk.md section 1), on noble's curve arithmetic and pairing, as the Rust SDK's `zk` module is on
 * substrate-bn's:
 *
 * - **Field elements** are 32 bytes big-endian and must be below `p`; a value `>= p` is malformed, never reduced.
 * - **G1** is `x || y`, 64 bytes; `(0, 0)` is the identity; any other point must be on `y^2 = x^3 + 3`.
 * - **G2** is `x.imag || x.real || y.imag || y.real`, 128 bytes; all zeros is the identity; any other point must be on
 *   the twist and in the order-`r` subgroup.
 *
 * A wrong length or a non-canonical coordinate is `Malformed`; a point off the curve or twist, or outside the subgroup,
 * is `InvalidPoint`. Every point is checked here before noble sees it, so no noble call can throw on what it is given.
 * What is not a `Uint8Array` (or an array of pairs) is a `TypeMismatch`. **Public data only**: the arithmetic is variable
 * time.
 */
import { err, ok, type Err, type models, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { ownCopy, readGuarded } from "@kd5ujc/dilf4s-core/internal";
import { bn254 } from "@noble/curves/bn254.js";

import { expectedAt, NOT_BYTES, readArray, readPair } from "./check.js";

/** Why a BN254 operation refused its input (generated from `shared/schemas/crypto-errors.cddl`). */
export type ZkError = models.cryptoErrors.ZkError;

/** A G1 point's width. */
export const G1_BYTES = 64;
/** A G2 point's width. */
export const G2_BYTES = 128;
/** A field element's or scalar's width. */
export const WORD_BYTES = 32;

const G1 = bn254.G1.Point;
const G2 = bn254.G2.Point;
const { Fp, Fp2, Fp12 } = bn254.fields;
const R = bn254.fields.Fr.ORDER;
const B1 = G1.CURVE().b;
const B2 = G2.CURVE().b;

type G1Point = InstanceType<typeof G1>;
type G2Point = InstanceType<typeof G2>;

const malformed = (reason: string): Err<ZkError> => err({ kind: "Malformed", reason });
const invalid = (reason: string): Err<ZkError> => err({ kind: "InvalidPoint", reason });

/** `x` copied, if it is a `Uint8Array` of `width` bytes: measured first, so a longer one is never copied. */
function exact(x: unknown, width: number, role: string): Result<Uint8Array, ZkError | TypeMismatch> {
  const own = ownCopy(x, width);
  if (own === undefined) return err(NOT_BYTES);
  const got = typeof own === "number" ? own : own.length;
  return typeof own !== "number" && got === width ? ok(own) : malformed(`${role} must be ${width.toString()} bytes, got ${got.toString()}`);
}

function word(bytes: Uint8Array, i: number): bigint {
  let n = 0n;
  for (let j = i * WORD_BYTES; j < (i + 1) * WORD_BYTES; j++) n = (n << 8n) | BigInt(bytes[j] ?? 0);
  return n;
}

function field(bytes: Uint8Array, i: number, role: string): Result<bigint, ZkError> {
  const x = word(bytes, i);
  return x < Fp.ORDER ? ok(x) : malformed(`${role} is not a canonical field element`);
}

/** A G1 point from 64 checked bytes. */
function g1Of(bytes: Uint8Array): Result<G1Point, ZkError> {
  const x = field(bytes, 0, "G1 x");
  if (!x.ok) return x;
  const y = field(bytes, 1, "G1 y");
  if (!y.ok) return y;
  if (x.value === 0n && y.value === 0n) return ok(G1.ZERO);
  // G1 has prime order (cofactor 1): a point on the curve is in the group
  const onCurve = Fp.eql(Fp.sqr(y.value), Fp.add(Fp.mul(Fp.sqr(x.value), x.value), B1));
  return onCurve ? ok(new G1(x.value, y.value, Fp.ONE)) : invalid("G1 point is not on the curve");
}

/** A G2 point from 128 checked bytes, in EIP-197 order. */
function g2Of(bytes: Uint8Array): Result<G2Point, ZkError> {
  const xi = field(bytes, 0, "G2 x.imag");
  if (!xi.ok) return xi;
  const xr = field(bytes, 1, "G2 x.real");
  if (!xr.ok) return xr;
  const yi = field(bytes, 2, "G2 y.imag");
  if (!yi.ok) return yi;
  const yr = field(bytes, 3, "G2 y.real");
  if (!yr.ok) return yr;
  const x = Fp2.create({ c0: xr.value, c1: xi.value });
  const y = Fp2.create({ c0: yr.value, c1: yi.value });
  if (Fp2.is0(x) && Fp2.is0(y)) return ok(G2.ZERO);
  if (!Fp2.eql(Fp2.sqr(y), Fp2.add(Fp2.mul(Fp2.sqr(x), x), B2))) return invalid("G2 point is not on the twist");
  // the twist has no point of order 2 (its order is odd), so y is not zero and the constructor accepts it
  const p = new G2(x, y, Fp2.ONE);
  return p.isTorsionFree() ? ok(p) : invalid("G2 point is not in the order-r subgroup");
}

/** A G1 point's 64 bytes; the identity is all zeros. */
function encodeG1(p: G1Point): Uint8Array {
  const out = new Uint8Array(G1_BYTES);
  if (p.is0()) return out;
  const { x, y } = p.toAffine();
  out.set(Fp.toBytes(x), 0);
  out.set(Fp.toBytes(y), WORD_BYTES);
  return out;
}

/** A G1 point from its 64 bytes; anything else is a refusal. */
function decodeG1(bytes: Uint8Array): Result<G1Point, ZkError | TypeMismatch> {
  const b = exact(bytes, G1_BYTES, "a G1 point");
  return b.ok ? g1Of(b.value) : b;
}

/** `a + b` (EIP-196 `ECADD`): `a` decoded first. */
export function add(a: Uint8Array, b: Uint8Array): Result<Uint8Array, ZkError | TypeMismatch> {
  const p = decodeG1(a);
  if (!p.ok) return p;
  const q = decodeG1(b);
  return q.ok ? ok(encodeG1(p.value.add(q.value))) : q;
}

/** `k * p` (EIP-196 `ECMUL`), for any 256-bit `k`: G1 has prime order `r`, so this is `(k mod r) * p`. */
export function mul(p: Uint8Array, k: Uint8Array): Result<Uint8Array, ZkError | TypeMismatch> {
  const point = decodeG1(p);
  if (!point.ok) return point;
  const scalar = exact(k, WORD_BYTES, "a scalar");
  if (!scalar.ok) return scalar;
  // reduced below r, as multiplyUnsafe requires; zero gives the identity
  return ok(encodeG1(point.value.multiplyUnsafe(word(scalar.value, 0) % R)));
}

/**
 * Whether the product of `e(g1, g2)` over the `[g1, g2]` pairs is one (EIP-197); no pairs is true. Every point is
 * decoded and checked, in order, before any pairing is computed. What is not an array of two-element arrays of
 * `Uint8Array`s is a `TypeMismatch`.
 */
export function pairing(pairs: readonly (readonly [Uint8Array, Uint8Array])[]): Result<boolean, ZkError | TypeMismatch> {
  const given = readArray(pairs, "an array of [g1, g2] pairs", (x, i) => {
    const pair = readPair(x);
    if (pair === undefined) return err(expectedAt("a [Uint8Array, Uint8Array] pair", i));
    // each read guarded (a Proxy's trap may revoke the pair) and copied before the next read runs caller code
    const a = ownCopy(readGuarded(pair, 0), G1_BYTES);
    if (a === undefined) return err(expectedAt("a [Uint8Array, Uint8Array] pair", i));
    const b = ownCopy(readGuarded(pair, 1), G2_BYTES);
    return b === undefined ? err(expectedAt("a [Uint8Array, Uint8Array] pair", i)) : ok([a, b] as const);
  });
  if (!given.ok) return given;
  const live: { g1: G1Point; g2: G2Point }[] = [];
  for (const [a, b] of given.value) {
    const g1 = typeof a === "number" || a.length !== G1_BYTES ? malformed(`a G1 point must be ${G1_BYTES.toString()} bytes, got ${(typeof a === "number" ? a : a.length).toString()}`) : g1Of(a);
    if (!g1.ok) return g1;
    const g2 = typeof b === "number" || b.length !== G2_BYTES ? malformed(`a G2 point must be ${G2_BYTES.toString()} bytes, got ${(typeof b === "number" ? b : b.length).toString()}`) : g2Of(b);
    if (!g2.ok) return g2;
    // e(O, Q) = e(P, O) = 1, so pairs with an identity drop out of the product (noble refuses them)
    if (!g1.value.is0() && !g2.value.is0()) live.push({ g1: g1.value, g2: g2.value });
  }
  return ok(live.length === 0 || Fp12.eql(bn254.pairingBatch(live), Fp12.ONE));
}
