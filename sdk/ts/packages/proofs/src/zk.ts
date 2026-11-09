/**
 * BN254 (`alt_bn128`) group operations on exact-width bytes, byte-compatible with the EVM precompiles (EIP-196/197,
 * docs/specs/zk.md section 1), and Groth16 verification ({@link groth16}, section 2), on noble's curve arithmetic and
 * pairing, as the Rust SDK's `zk` module is on substrate-bn's:
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
import { err, ok, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { ownCopy, readGuarded } from "@kd5ujc/dilf4s-core/internal";
import { bn254 } from "@noble/curves/bn254.js";

import { encodeG1, exact, g1Of, g2Of, G1_BYTES, G2_BYTES, malformed, pairingIsOne, word, WORD_BYTES, type G1Point, type G2Point, type ZkError } from "./bn254.js";
import { expectedAt, readArray, readPair } from "./check.js";

export { G1_BYTES, G2_BYTES, WORD_BYTES, type ZkError } from "./bn254.js";
export * as groth16 from "./groth16.js";

const R = bn254.fields.Fr.ORDER;

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
  const points: (readonly [G1Point, G2Point])[] = [];
  for (const [a, b] of given.value) {
    const g1 = typeof a === "number" || a.length !== G1_BYTES ? malformed(`a G1 point must be ${G1_BYTES.toString()} bytes, got ${(typeof a === "number" ? a : a.length).toString()}`) : g1Of(a);
    if (!g1.ok) return g1;
    const g2 = typeof b === "number" || b.length !== G2_BYTES ? malformed(`a G2 point must be ${G2_BYTES.toString()} bytes, got ${(typeof b === "number" ? b : b.length).toString()}`) : g2Of(b);
    if (!g2.ok) return g2;
    points.push([g1.value, g2.value]);
  }
  return ok(pairingIsOne(points));
}
