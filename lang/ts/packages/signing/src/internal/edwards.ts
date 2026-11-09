/** edwards25519 helpers over noble's `ed25519.Point`: decoding profiles, scalars and multiplications. */
import { ed25519 } from "@noble/curves/ed25519.js";
import { sha512 } from "@noble/hashes/sha2.js";

import { attempt } from "./attempt.js";
import { concat, leToBig, bigToLE, equal, mod } from "./bytes.js";

export const Point = ed25519.Point;
export type EdPoint = ReturnType<typeof ed25519.Point.fromAffine>;

/** The field prime `p = 2^255 - 19` and the prime group order `L`. */
export const P: bigint = Point.Fp.ORDER;
export const L: bigint = Point.Fn.ORDER;

/** `y` of an encoding (bit 255 cleared), as an integer that may be `>= p`. */
function encodedY(b: Uint8Array): bigint {
  const c = Uint8Array.from(b);
  c[31] = (c[31] ?? 0) & 0x7f;
  return leToBig(c);
}

export const canonicalY = (b: Uint8Array): boolean => b.length === 32 && encodedY(b) < P;

/** ZIP-215 decoding: any `y` (including `y >= p`) and negative zero, rejecting encodings not on the curve. */
export function decodeZip215(b: Uint8Array): EdPoint | undefined {
  return b.length === 32 ? attempt(() => Point.fromBytes(b, true)) : undefined;
}

/** RFC 8032 section 5.1.3 strict decoding: the re-encoding must equal the input (canonical `y`, no negative zero). */
export function decodeStrict(b: Uint8Array): EdPoint | undefined {
  const pt = decodeZip215(b);
  return pt !== undefined && equal(pt.toBytes(), b) ? pt : undefined;
}

/** libsodium's draft-03 decoding: canonical `y`; `x = 0` with the sign bit set decodes to the positive point. */
export function decodeDraft03(b: Uint8Array): EdPoint | undefined {
  return canonicalY(b) ? decodeZip215(b) : undefined;
}

export const isSmallOrder = (pt: EdPoint): boolean => pt.clearCofactor().is0();

/** `[s]B` for a secret scalar (reduced mod L), constant-time in noble; zero gives the identity. */
export function mulBase(s: bigint): EdPoint {
  const r = mod(s, L);
  return r === 0n ? Point.ZERO : Point.BASE.multiply(r);
}

/** `[s]Q` for a secret scalar (reduced mod L) and any point. */
export function mulSecret(q: EdPoint, s: bigint): EdPoint {
  const r = mod(s, L);
  return r === 0n ? Point.ZERO : q.multiply(r);
}

/**
 * `[s]Q` on public data, for `0 <= s < L`. Exact on points of any order (it is a true scalar multiplication, not a
 * reduction to the prime-order subgroup).
 */
export const mulPublic = (q: EdPoint, s: bigint): EdPoint => q.multiplyUnsafe(s);

export const sha512Parts = (...parts: readonly Uint8Array[]): Uint8Array => sha512(concat(...parts));

/** `SHA-512(parts) mod L`. */
export const hashToScalar = (...parts: readonly Uint8Array[]): bigint => mod(leToBig(sha512Parts(...parts)), L);

export const scalarBytes = (s: bigint): Uint8Array => bigToLE(mod(s, L), 32);

/** The RFC 8032 key expansion of a 32-byte seed: the clamped secret scalar and the nonce prefix. */
export function expandSeed(seed: Uint8Array): { readonly scalar: bigint; readonly prefix: Uint8Array } {
  const h = sha512(seed);
  const a = h.slice(0, 32);
  a[0] = (a[0] ?? 0) & 0xf8;
  a[31] = ((a[31] ?? 0) & 0x7f) | 0x40;
  return { scalar: mod(leToBig(a), L), prefix: h.slice(32, 64) };
}
