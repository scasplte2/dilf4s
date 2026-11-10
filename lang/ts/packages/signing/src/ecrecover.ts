/**
 * Ethereum's signer recovery (`ecrecover`, yellow paper Appendix F) over secp256k1: the address whose key made an ECDSA
 * signature over a 32-byte digest (`jlvm.md` section 6). Verification only: every input is public.
 *
 * Stricter than the EVM precompile, as OpenZeppelin's `ECDSA.recover` is: a high-`s` signature (EIP-2) recovers nothing,
 * so a signature cannot be re-encoded into a second valid one. Written on noble's group arithmetic, as the Rust SDK's
 * `ecrecover.rs` is on k256's, so the rules here are this package's own and do not depend on a recovery routine's checks.
 */
import { err, ok, type Result } from "@kd5ujc/dilf4s-core";
import { digestOf } from "@kd5ujc/dilf4s-core/internal";
import { secp256k1 } from "@noble/curves/secp256k1.js";

import { invalidEncoding } from "./errors.js";
import { attempt } from "./internal/attempt.js";
import { beToBig } from "./internal/bytes.js";
import { sized, type CryptoRefusal } from "./scheme.js";

/** A digest's width. */
export const ECRECOVER_DIGEST_BYTES = 32;
/** A signature's width: `r || s || v`. */
export const ECRECOVER_SIGNATURE_BYTES = 65;
/** An address's width. */
export const ECRECOVER_ADDRESS_BYTES = 20;

const Point = secp256k1.Point;
const Fn = Point.Fn;
const N = Fn.ORDER;
/** `s` above this is high (EIP-2). */
const HALF_N = N >> 1n;

/**
 * The signer's address of `signature` (`r || s || v`, `v` in {27, 28} or {0, 1}) over `digest`.
 *
 * Wrong sizes or another `v` are malformed (`InvalidEncoding`), checked in that order: the digest, then the signature,
 * then `v`. A well-formed signature that recovers no key is `undefined`: `r` or `s` zero or not below the order, a high
 * `s`, an `r` that is no point's x-coordinate, or a recovered identity. What is not a `Uint8Array` is a `TypeMismatch`.
 */
export function ecrecover(digest: Uint8Array, signature: Uint8Array): Result<Uint8Array | undefined, CryptoRefusal> {
  const d = sized("ecrecover digest", ECRECOVER_DIGEST_BYTES, digest);
  if (!d.ok) return d;
  const s = sized("ecrecover signature", ECRECOVER_SIGNATURE_BYTES, signature);
  if (!s.ok) return s;
  const v = s.value[ECRECOVER_SIGNATURE_BYTES - 1] ?? 0xff;
  const parity = v === 0 || v === 27 ? 0 : v === 1 || v === 28 ? 1 : undefined;
  if (parity === undefined) return err(invalidEncoding("ecrecover signature", `v is 27 or 28 (or 0 or 1), got ${v.toString()}`));
  const q = recovered(d.value, s.value.subarray(0, 32), s.value.subarray(32, 64), parity);
  return ok(q === undefined ? undefined : address(q));
}

/** `Q = r^-1 (s R - e G)`, where `R` has x-coordinate `r` and the given y parity; `undefined` where no key is recovered. */
function recovered(digest: Uint8Array, rBytes: Uint8Array, sBytes: Uint8Array, parity: number): InstanceType<typeof Point> | undefined {
  const [r, s] = [beToBig(rBytes), beToBig(sBytes)];
  if (r === 0n || r >= N || s === 0n || s > HALF_N) return undefined;
  // r < n < p, so its bytes are a field element; it is an x-coordinate when x^3 + 7 is a square (noble throws otherwise)
  const prefixed = new Uint8Array(33);
  prefixed[0] = 0x02 + parity;
  prefixed.set(rBytes, 1);
  const big = attempt(() => Point.fromBytes(prefixed));
  if (big === undefined) return undefined;
  const e = Fn.create(beToBig(digest));
  const rInv = Fn.inv(r);
  // both scalars are reduced, so below n, as multiplyUnsafe requires; zero gives the identity
  const q = big.multiplyUnsafe(Fn.mul(s, rInv)).add(Point.BASE.multiplyUnsafe(Fn.neg(Fn.mul(e, rInv))));
  return q.is0() ? undefined : q;
}

/** An Ethereum address: the last 20 bytes of Keccak-256 over the uncompressed key's `x || y`. */
function address(key: InstanceType<typeof Point>): Uint8Array {
  const encoded = key.toBytes(false);
  return digestOf("Keccak-256", encoded.subarray(1)).slice(32 - ECRECOVER_ADDRESS_BYTES);
}
