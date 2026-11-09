/**
 * Groth16 over BN254 (docs/specs/zk.md section 2), and SP1's Groth16 wrapper ({@link sp1}), on the points `zk.ts`
 * checks and noble's pairing, as the Rust SDK's `zk::groth16` is on substrate-bn's.
 *
 * A proof is 256 bytes, `A (G1) || B (G2) || C (G1)`; `A`, `B` and `C` must not be the identity, and each public input
 * must be in `[0, r)`. With `L = IC_0 + sum x_i IC_(i+1)`, a multi-scalar multiplication, the proof is accepted if and
 * only if `e(-A, B) e(alpha, beta) e(L, gamma) e(C, delta) = 1`. The checks run in the reference's order, so each
 * refusal is the same {@link Groth16Error} kind:
 * - `Malformed`: a wrong length, a non-canonical field element, the wrong number of inputs, or an identity in a key;
 * - `InvalidProof`: a point off the curve, outside the subgroup or the identity, or an input not in Fr;
 * - `PairingCheckFailed`: the equation does not hold;
 * - `Rejected`: SP1's framing (its selector, exit code or VK root).
 *
 * What is not of an argument's type is a `TypeMismatch`. The work is bounded by the proof and the key, and by the
 * inputs, one scalar each. **Public data only**: the arithmetic is variable time.
 */
import { err, ok, type Err, type models, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { digestOf, mint, ownBytes, ownCopy } from "@kd5ujc/dilf4s-core/internal";
import { pippenger } from "@noble/curves/abstract/curve.js";
import { bn254 } from "@noble/curves/bn254.js";

import { field, g1At, g1Of, g2Of, G1_BYTES, G2_BYTES, negate, pairingIsOne, word, WORD_BYTES, type G1Point, type G2Point, type ZkError } from "./bn254.js";
import { bigintArg, bytesArg, expected, NOT_BYTES, readArray } from "./check.js";

/** Why a proof was not accepted (generated from `shared/schemas/crypto-errors.cddl`). */
export type Groth16Error = models.cryptoErrors.Groth16Error;

/** A proof's width. */
export const PROOF_BYTES = 256;

const G1 = bn254.G1.Point;
const G2 = bn254.G2.Point;
const R = bn254.fields.Fr.ORDER;

/**
 * The token this module's constructor takes, private to it: a constructor is private to TypeScript alone, so JavaScript
 * can call one; without the token it keeps none of its arguments and stamps no brand (core's `mint`), so no check here
 * accepts what it builds.
 */
const TOKEN: unique symbol = Symbol("dilf4s-proofs: groth16");
type Token = typeof TOKEN;
const KEY = mint();

const malformed = (reason: string): Err<Groth16Error> => err({ kind: "Malformed", reason });
const invalid = (reason: string): Err<Groth16Error> => err({ kind: "InvalidProof", reason });
const rejected = (reason: string): Err<Groth16Error> => err({ kind: "Rejected", reason });

/** A BN254 refusal inside a Groth16 check: a malformed encoding is malformed, an invalid point an invalid proof. */
const groth16Of = (e: ZkError): Err<Groth16Error> => (e.kind === "Malformed" ? malformed(e.reason) : invalid(e.reason));

/** Bytes as core's boundary measured them: the bytes, or the length of bytes past their bound (never copied). */
type Measured = Uint8Array | number;

const lengthOf = (b: Measured): number => (typeof b === "number" ? b : b.length);

/** A key's points, every one valid and finite. */
interface Points {
  readonly alpha: G1Point;
  readonly beta: G2Point;
  readonly gamma: G2Point;
  readonly delta: G2Point;
  readonly ic0: G1Point;
  readonly ic: readonly G1Point[];
}

/** What a constructor called without the token keeps: no point of the caller's. */
const NO_POINTS: Points = Object.freeze({ alpha: G1.BASE, beta: G2.BASE, gamma: G2.BASE, delta: G2.BASE, ic0: G1.BASE, ic: Object.freeze([]) });

let pointsOf!: (vk: VerifyingKey) => Points;
let isKey!: (x: unknown) => x is VerifyingKey;
let createKey!: (points: Points) => VerifyingKey;

const NOT_A_KEY = expected("a groth16.VerifyingKey");

/** A verifying key `(alpha, beta, gamma, delta, IC)`, its points checked when it is built: see {@link VerifyingKey.of}. */
export class VerifyingKey {
  readonly #points: Points;
  /** The number of public inputs a proof under this key takes: `IC`'s points after `IC_0`. */
  readonly publicInputs: number;
  private constructor(token: Token, points: Points) {
    this.#points = token === TOKEN ? points : NO_POINTS;
    this.publicInputs = this.#points.ic.length;
    if (token === TOKEN) KEY.stamp(this);
    Object.freeze(this);
  }

  static {
    pointsOf = (vk) => vk.#points;
    isKey = (x): x is VerifyingKey => KEY.has(x);
    createKey = (points) => new VerifyingKey(TOKEN, points);
  }

  /**
   * A key from its encoded points: `alpha` (G1); `beta`, `gamma` and `delta` (G2); and `ic`, `IC_0` then one G1 point
   * per public input. Every point must be a valid, finite point: an identity in a key could cancel terms of the pairing
   * equation, so that some proof would verify for every input. A wrong width, a non-canonical coordinate or an identity
   * is `Malformed`; a point off the curve or twist, or outside the subgroup, is `InvalidProof`. Each argument is read
   * once, through core's byte boundary, and `ic` (an array) item by item, each copied before the next is read; then the
   * points are checked in the reference's order: `ic` holds `IC_0`, then `alpha`, `beta`, `gamma`, `delta` and `ic`.
   */
  static of(alpha: Uint8Array, beta: Uint8Array, gamma: Uint8Array, delta: Uint8Array, ic: readonly Uint8Array[]): Result<VerifyingKey, Groth16Error | TypeMismatch> {
    const points = readArray(ic, "an array of Uint8Array", (x) => {
      const b = ownCopy(x, G1_BYTES);
      return b === undefined ? err(NOT_BYTES) : ok(b);
    });
    if (!points.ok) return points;
    // read after `ic`, whose reads may run caller code, and checked before any further caller code runs
    const read = [ownBytes(alpha, G1_BYTES), ownBytes(beta, G2_BYTES), ownBytes(gamma, G2_BYTES), ownBytes(delta, G2_BYTES)] as const;
    const [a, b, c, d] = read;
    if (a === undefined || b === undefined || c === undefined || d === undefined) return err(NOT_BYTES);
    return keyOf(a, b, c, d, points.value);
  }
}
Object.freeze(VerifyingKey);
Object.freeze(VerifyingKey.prototype);

/** A key's point: `width` bytes, a valid point (`decode`), and not the identity. */
function finite<P extends G1Point | G2Point>(b: Measured, width: number, role: string, decode: (bytes: Uint8Array) => Result<P, ZkError>): Result<P, Groth16Error> {
  if (typeof b === "number" || b.length !== width) return malformed(`the key's ${role} must be ${width.toString()} bytes, got ${lengthOf(b).toString()}`);
  const p = decode(b);
  if (!p.ok) return groth16Of(p.error);
  return p.value.is0() ? malformed(`the key's ${role} is the identity`) : p;
}

/** The key of measured points, checked as `VerifyingKey::from_bytes` checks them, in its order. */
function keyOf(alpha: Measured, beta: Measured, gamma: Measured, delta: Measured, ic: readonly Measured[]): Result<VerifyingKey, Groth16Error> {
  const [first, ...rest] = ic;
  if (first === undefined) return malformed("IC needs IC_0");
  const a = finite(alpha, G1_BYTES, "alpha", g1Of);
  if (!a.ok) return a;
  const b = finite(beta, G2_BYTES, "beta", g2Of);
  if (!b.ok) return b;
  const g = finite(gamma, G2_BYTES, "gamma", g2Of);
  if (!g.ok) return g;
  const d = finite(delta, G2_BYTES, "delta", g2Of);
  if (!d.ok) return d;
  const ic0 = finite(first, G1_BYTES, "IC_0", g1Of);
  if (!ic0.ok) return ic0;
  const points: G1Point[] = [];
  for (const x of rest) {
    const p = finite(x, G1_BYTES, "an IC point", g1Of);
    if (!p.ok) return p;
    points.push(p.value);
  }
  return ok(createKey(Object.freeze({ alpha: a.value, beta: b.value, gamma: g.value, delta: d.value, ic0: ic0.value, ic: Object.freeze(points) })));
}

/** A proof that passed its own checks: `A`'s canonical coordinates, and `B`'s and `C`'s bytes, not yet decoded. */
interface Parsed {
  readonly ax: bigint;
  readonly ay: bigint;
  readonly b: Uint8Array;
  readonly c: Uint8Array;
}

/**
 * A proof's own checks, as the reference makes them before it reads the inputs (Rust `zk/groth16.rs` lines 102-113):
 * its width; then `A`, `B` and `C` not all-zero bytes, in that order; then `A`'s coordinates canonical, which negating
 * it needs. Whether `A` is on the curve, and `B` and `C` at all, are checked with the pairing's points.
 */
function parse(proof: Measured): Result<Parsed, Groth16Error> {
  if (typeof proof === "number" || proof.length !== PROOF_BYTES) return malformed(`a proof is ${PROOF_BYTES.toString()} bytes, got ${lengthOf(proof).toString()}`);
  const a = proof.subarray(0, G1_BYTES);
  const b = proof.subarray(G1_BYTES, G1_BYTES + G2_BYTES);
  const c = proof.subarray(G1_BYTES + G2_BYTES);
  for (const [p, role] of [[a, "A"], [b, "B"], [c, "C"]] as const) {
    if (p.every((x) => x === 0)) return invalid(`${role} is the identity`);
  }
  const ax = field(a, 0, "A.x");
  if (!ax.ok) return groth16Of(ax.error);
  const ay = field(a, 1, "A.y");
  return ay.ok ? ok({ ax: ax.value, ay: ay.value, b, c }) : groth16Of(ay.error);
}

/** The refusal of `n` inputs under `points`, or `undefined` when it is their number. */
const countError = (points: Points, n: number): Groth16Error | undefined =>
  n === points.ic.length ? undefined : { kind: "Malformed", reason: `expected ${points.ic.length.toString()} public inputs, got ${n.toString()}` };

/**
 * The rest of the check, in the reference's order: the inputs' number, then each in `[0, r)`; `L`; then `-A`, `B` and
 * `C` decoded, in that order; then the pairing equation.
 */
function check(points: Points, proof: Parsed, inputs: readonly bigint[]): Result<undefined, Groth16Error> {
  const count = countError(points, inputs.length);
  if (count !== undefined) return err(count);
  if (!inputs.every((x) => x >= 0n && x < R)) return invalid("a public input is not in Fr");
  // every point is a valid G1 point and every scalar below r, as pippenger requires: it cannot throw
  const l = pippenger(G1, [...points.ic], [...inputs]).add(points.ic0);
  const negA = g1At(proof.ax, negate(proof.ay));
  if (!negA.ok) return groth16Of(negA.error);
  const b = g2Of(proof.b);
  if (!b.ok) return groth16Of(b.error);
  const c = g1Of(proof.c);
  if (!c.ok) return groth16Of(c.error);
  const pairs = [[negA.value, b.value], [points.alpha, points.beta], [l, points.gamma], [c.value, points.delta]] as const;
  return pairingIsOne(pairs) ? ok(undefined) : err({ kind: "PairingCheckFailed" });
}

/**
 * Verifies `proof` for `inputs` (each a `bigint` in `[0, r)`) under `vk`: `undefined` when it is accepted, else why
 * not. `proof` is read and copied first, and refused on its own checks before `inputs` is read; `inputs`, an array, has
 * its length refused before any of its items is read.
 */
export function verify(vk: VerifyingKey, proof: Uint8Array, inputs: readonly bigint[]): Result<undefined, Groth16Error | TypeMismatch> {
  if (!isKey(vk)) return err(NOT_A_KEY);
  const own = ownCopy(proof, PROOF_BYTES);
  if (own === undefined) return err(NOT_BYTES);
  const parsed = parse(own);
  if (!parsed.ok) return parsed;
  const points = pointsOf(vk);
  const xs = readArray<bigint, Groth16Error | TypeMismatch>(inputs, "an array of bigint", bigintArg, (n) => countError(points, n));
  return xs.ok ? check(points, parsed.value, xs.value) : xs;
}

// ------------------------------------------------------------------ SP1

/** SP1's verifier selector, `bytes4(verifierHash)`: `proofBytes`' first four bytes, big-endian. */
const SELECTOR = 0x4388a21c;
/** `selector (4) || exitCode || vkRoot || nonce`, before the proof's eight words. */
const HEADER_BYTES = 4 + WORD_BYTES * 3;
/** `proofBytes`' width. */
const SP1_PROOF_BYTES = HEADER_BYTES + PROOF_BYTES;
/** The recursion VK root an accepted proof commits to. */
const VK_ROOT = 0x002f850ee998974d6cc00e50cd0814b098c05bfade466d28573240d057f25352n;
/** `2^253 - 1`: the public values' digest, its top three bits cleared, is an element of Fr. */
const DIGEST_MASK = (1n << 253n) - 1n;

// the verifying key of SP1VerifierGroth16.sol (circuit v6.1.0) as EIP-197 words (a G2 point imaginary part first); the
// contract stores beta, gamma and delta negated, and these are negated back (Rust's `sp1` module holds the same bytes)
const ALPHA = [0x21c7d728a5fd961fc179ec5eab938f564deba5b271e1c90c2c29a79648418fc1n, 0x2317bd9b644830ef5ffec1c2d46b5442aa8029d41eb58fb8fc939fb03365e00en] as const;
const BETA = [
  0x1c3c9339849225980c7d3f824f80d19e2a9c2554b6ab2160fa9635528f693fc0n,
  0x0d964538da2653f2e62499571e6c78afb8909d3ea8107f306bd6928253680a3an,
  0x2c8e3c3c5153ef35c01baddac0d2e24dfee9dd922d819aae54fa90e175f9625en,
  0x2677b02e199903c8131b8e9fafaa7d5b0eeda406cf3d190e01cfdce881ab30can,
] as const;
const GAMMA = [
  0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2n,
  0x1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6edn,
  0x090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975bn,
  0x12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daan,
] as const;
const DELTA = [
  0x11b7e9276171bb0efd647fc63e38bbfba3076f20daca8cd52bcc7284d9b1c6ebn,
  0x1723616533dd6ae53502c9c506a81f23f543d68750b5133ebfbe1f4746b3b011n,
  0x0c4684810202d51d39e2b093a0a9187afacb75e203000170f23687b2cc786b26n,
  0x05865c701a782e7a81549613e8f8832f6a6d86a809ca794fd7e99b54e626080cn,
] as const;
const IC = [
  [0x2cd6bf7f164af0b6b0bbbe0fdcb06ee0c1ba07f8e6eb2f9f3943a90cb1d40290n, 0x0a76f7e6e8a78d71649eb7e3132018885e2f57ccca64394738e746c63c9fa83bn],
  [0x0f5460f3b7221705435e745da21e276536379c0113c13c4255e7ae101f1e90bfn, 0x022b4d6c4b7dbdeed528d5abe6eddd6f2bd4ae4b9b0450dc18901fc727da799cn],
  [0x0b0ae6e491bc04c544da9e8cd4857d201b4cfa0222dbe96aac97f044fdf1c922n, 0x123e064d2ffdb7f8094b727cde1c0beea6d4d551e5743a336804f8effb7f07acn],
  [0x097c875a6ebd0999b06e7267ff3d8a6bf859bb9635abae07cb6b3534ba409a83n, 0x2d84b73fb0f397e356b524f2d0e9ddc9d0f6ac75e566041147ee8554879183ecn],
  [0x1807204ddcd27506ba72e17b55227b0bf310136ecb40c74acd52f3ccfbcba9f7n, 0x011078b5c7767fe73bb54241dcd143ee805ea4383d48d46bced129482c8c2021n],
  [0x008c7b7c98d78c07a2c4be5f6be7082ba41021611f9a2dfc016f8bbb37d36been, 0x09ce6d246baeac24b5a404ca175c2cc4a974e217176fe2ec2a6c22eb60fd65c1n],
] as const;

/** Words as their encoding: 32 bytes each, big-endian. */
function encodeWords(ws: readonly bigint[]): Uint8Array {
  const out = new Uint8Array(ws.length * WORD_BYTES);
  for (const [i, w] of ws.entries()) {
    for (let j = 0; j < WORD_BYTES; j++) out[(i + 1) * WORD_BYTES - 1 - j] = Number((w >> BigInt(8 * j)) & 0xffn);
  }
  return out;
}

/**
 * The verifying key of `SP1VerifierGroth16.sol` (circuit v6.1.0), decoded and checked as {@link VerifyingKey.of}
 * checks a key: build it once and pass it to {@link Sp1Verifier.verifyWith} where proofs are verified repeatedly.
 */
function verifyingKey(): Result<VerifyingKey, Groth16Error> {
  return keyOf(encodeWords(ALPHA), encodeWords(BETA), encodeWords(GAMMA), encodeWords(DELTA), IC.map(encodeWords));
}

/** `sha256(publicValues)`, its top three bits cleared: the public input, an element of Fr. */
const digestInput = (publicValues: Uint8Array): bigint => word(digestOf("SHA-256", publicValues), 0) & DIGEST_MASK;

/** `sha256(publicValues)` with its top three bits cleared, the proof's second public input. */
function hashPublicValues(publicValues: Uint8Array): Result<bigint, TypeMismatch> {
  const v = bytesArg(publicValues);
  return v.ok ? ok(digestInput(v.value)) : v;
}

/**
 * The SP1 checks, in the reference's order (Rust `zk/groth16.rs` lines 219-239): `programVKey`'s and `proofBytes`'
 * widths, the selector, the exit code and the VK root, then the proof for the public inputs `[programVKey,
 * sha256(publicValues) mod 2^253, exitCode, vkRoot, nonce]`.
 */
function sp1Check(points: Points, program: Measured, publicValues: Uint8Array, framed: Measured): Result<undefined, Groth16Error> {
  if (typeof program === "number" || program.length !== WORD_BYTES) return malformed(`programVKey must be ${WORD_BYTES.toString()} bytes, got ${lengthOf(program).toString()}`);
  if (typeof framed === "number" || framed.length !== SP1_PROOF_BYTES) return malformed(`proofBytes must be ${SP1_PROOF_BYTES.toString()} bytes, got ${lengthOf(framed).toString()}`);
  const selector = framed.subarray(0, 4).reduce((n, x) => n * 256 + x, 0);
  if (selector !== SELECTOR) return rejected("wrong verifier selector");
  const header = framed.subarray(4, HEADER_BYTES);
  const [exitCode, vkRoot, nonce] = [word(header, 0), word(header, 1), word(header, 2)];
  if (exitCode !== 0n) return rejected("non-zero exit code");
  if (vkRoot !== VK_ROOT) return rejected("wrong VK root");
  const proof = parse(framed.subarray(HEADER_BYTES));
  return proof.ok ? check(points, proof.value, [word(program, 0), digestInput(publicValues), exitCode, vkRoot, nonce]) : proof;
}

/**
 * Verifies an SP1 Groth16 proof that the program with key `programVKey` (32 bytes) committed `publicValues`, under
 * `vk` (built by {@link verifyingKey}). Each argument is read once, through core's byte boundary, before any is checked;
 * no caller code runs while they are in use.
 */
function verifyWith(vk: VerifyingKey, programVKey: Uint8Array, publicValues: Uint8Array, proofBytes: Uint8Array): Result<undefined, Groth16Error | TypeMismatch> {
  if (!isKey(vk)) return err(NOT_A_KEY);
  const program = ownBytes(programVKey, WORD_BYTES);
  if (program === undefined) return err(NOT_BYTES);
  const values = bytesArg(publicValues);
  if (!values.ok) return values;
  const framed = ownBytes(proofBytes, SP1_PROOF_BYTES);
  return framed === undefined ? err(NOT_BYTES) : sp1Check(pointsOf(vk), program, values.value, framed);
}

/** {@link verifyWith} under the key {@link verifyingKey} builds, for this call alone. */
function verifySp1(programVKey: Uint8Array, publicValues: Uint8Array, proofBytes: Uint8Array): Result<undefined, Groth16Error | TypeMismatch> {
  const vk = verifyingKey();
  return vk.ok ? verifyWith(vk.value, programVKey, publicValues, proofBytes) : vk;
}

/** SP1's Groth16 verifier. */
export interface Sp1Verifier {
  /** The verifier selector, `bytes4(verifierHash)`: `proofBytes`' first four bytes, big-endian. */
  readonly SELECTOR: number;
  /** `proofBytes`' width: `selector (4) || exitCode || vkRoot || nonce || proof (8 words)`. */
  readonly PROOF_BYTES: number;
  /** The recursion VK root an accepted proof commits to. */
  readonly VK_ROOT: bigint;
  /** The verifying key of `SP1VerifierGroth16.sol` (circuit v6.1.0): build it once where proofs are verified repeatedly. */
  readonly verifyingKey: () => Result<VerifyingKey, Groth16Error>;
  /** `sha256(publicValues)` with its top three bits cleared: the proof's second public input. */
  readonly hashPublicValues: (publicValues: Uint8Array) => Result<bigint, TypeMismatch>;
  /** Whether the program with key `programVKey` committed `publicValues`, under a key it builds. */
  readonly verify: (programVKey: Uint8Array, publicValues: Uint8Array, proofBytes: Uint8Array) => Result<undefined, Groth16Error | TypeMismatch>;
  /** {@link Sp1Verifier.verify} under a key already built by {@link Sp1Verifier.verifyingKey}. */
  readonly verifyWith: (vk: VerifyingKey, programVKey: Uint8Array, publicValues: Uint8Array, proofBytes: Uint8Array) => Result<undefined, Groth16Error | TypeMismatch>;
}

/**
 * SP1's Groth16 wrapper (`SP1VerifierGroth16.sol`, circuit v6.1.0): `proofBytes = selector (4) || exitCode || vkRoot ||
 * nonce || proof (8 words)`, public inputs `[programVKey, sha256(publicValues) mod 2^253, exitCode, vkRoot, nonce]`.
 */
export const sp1: Sp1Verifier = Object.freeze({
  SELECTOR,
  PROOF_BYTES: SP1_PROOF_BYTES,
  VK_ROOT,
  verifyingKey,
  hashPublicValues,
  verify: verifySp1,
  verifyWith,
});
