/**
 * BN254 points from exact-width bytes (docs/specs/zk.md section 1), on noble's curves: what `zk.ts`'s group operations
 * and `groth16.ts` decode, as the Rust SDK's `zk` module decodes for both. Every point is checked here before noble sees
 * it, so no noble call can throw on what it is given. Internal: not exported from the package, and only functions and
 * numbers leave this module (noble's classes and fields stay behind it).
 */
import { err, ok, type Err, type models, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { ownCopy } from "@kd5ujc/dilf4s-core/internal";
import { bn254 } from "@noble/curves/bn254.js";

import { NOT_BYTES } from "./check.js";

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
const B1 = G1.CURVE().b;
const B2 = G2.CURVE().b;

export type G1Point = InstanceType<typeof G1>;
export type G2Point = InstanceType<typeof G2>;

export const malformed = (reason: string): Err<ZkError> => err({ kind: "Malformed", reason });
const invalid = (reason: string): Err<ZkError> => err({ kind: "InvalidPoint", reason });

/** `x` copied, if it is a `Uint8Array` of `width` bytes: measured first, so a longer one is never copied. */
export function exact(x: unknown, width: number, role: string): Result<Uint8Array, ZkError | TypeMismatch> {
  const own = ownCopy(x, width);
  if (own === undefined) return err(NOT_BYTES);
  const got = typeof own === "number" ? own : own.length;
  return typeof own !== "number" && got === width ? ok(own) : malformed(`${role} must be ${width.toString()} bytes, got ${got.toString()}`);
}

/** The `i`th 32-byte word of `bytes`, big-endian. */
export function word(bytes: Uint8Array, i: number): bigint {
  let n = 0n;
  for (let j = i * WORD_BYTES; j < (i + 1) * WORD_BYTES; j++) n = (n << 8n) | BigInt(bytes[j] ?? 0);
  return n;
}

/** The `i`th word of `bytes` as a field element: one `>= p` is malformed, never reduced. */
export function field(bytes: Uint8Array, i: number, role: string): Result<bigint, ZkError> {
  const x = word(bytes, i);
  return x < Fp.ORDER ? ok(x) : malformed(`${role} is not a canonical field element`);
}

/** `-y` modulo `p`, for a canonical `y`. */
export const negate = (y: bigint): bigint => Fp.neg(y);

/** The G1 point `(x, y)`, for canonical coordinates: `(0, 0)` is the identity, any other point must be on the curve. */
export function g1At(x: bigint, y: bigint): Result<G1Point, ZkError> {
  if (x === 0n && y === 0n) return ok(G1.ZERO);
  // G1 has prime order (cofactor 1): a point on the curve is in the group
  const onCurve = Fp.eql(Fp.sqr(y), Fp.add(Fp.mul(Fp.sqr(x), x), B1));
  return onCurve ? ok(new G1(x, y, Fp.ONE)) : invalid("G1 point is not on the curve");
}

/** A G1 point from 64 checked bytes. */
export function g1Of(bytes: Uint8Array): Result<G1Point, ZkError> {
  const x = field(bytes, 0, "G1 x");
  if (!x.ok) return x;
  const y = field(bytes, 1, "G1 y");
  return y.ok ? g1At(x.value, y.value) : y;
}

/** A G2 point from 128 checked bytes, in EIP-197 order. */
export function g2Of(bytes: Uint8Array): Result<G2Point, ZkError> {
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

/** Whether the product of `e(g1, g2)` over checked points is one; no pairs is true. */
export function pairingIsOne(pairs: readonly (readonly [G1Point, G2Point])[]): boolean {
  // e(O, Q) = e(P, O) = 1, so pairs with an identity drop out of the product (noble refuses them)
  const live = pairs.filter(([p, q]) => !p.is0() && !q.is0()).map(([g1, g2]) => ({ g1, g2 }));
  return live.length === 0 || Fp12.eql(bn254.pairingBatch(live), Fp12.ONE);
}

/** A G1 point's 64 bytes; the identity is all zeros. */
export function encodeG1(p: G1Point): Uint8Array {
  const out = new Uint8Array(G1_BYTES);
  if (p.is0()) return out;
  const { x, y } = p.toAffine();
  out.set(Fp.toBytes(x), 0);
  out.set(Fp.toBytes(y), WORD_BYTES);
  return out;
}
