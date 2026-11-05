/**
 * Byte and integer helpers shared by the schemes. A caller's `Uint8Array` may be a subclass whose `length`, accessors
 * or iterator are its own, so each helper reads bytes through core's `plainBytes` (the array's internal slots), and
 * reads what is not bytes as empty.
 */
import { plainBytes, takeBytes } from "@kd5ujc/dilf4s-core";

const EMPTY = new Uint8Array(0);

/** `b` as a plain view over its own memory, its length the array's own; empty if it is not bytes. */
export const view = (b: Uint8Array): Uint8Array => plainBytes(b) ?? EMPTY;

/** A fresh plain copy, from the array's internal slots (core's `takeBytes`). */
export function copy(bytes: Uint8Array): Uint8Array {
  return takeBytes(bytes) ?? new Uint8Array(0);
}

export function concat(...parts: readonly Uint8Array[]): Uint8Array {
  const views = parts.map(view);
  let n = 0;
  for (const p of views) n += p.length;
  const out = new Uint8Array(n);
  let at = 0;
  for (const p of views) {
    out.set(p, at);
    at += p.length;
  }
  return out;
}

/** Equality of public data. */
export function equal(a: Uint8Array, b: Uint8Array): boolean {
  const [x, y] = [view(a), view(b)];
  if (x.length !== y.length) return false;
  for (let i = 0; i < x.length; i++) if (x[i] !== y[i]) return false;
  return true;
}

/** Equality whose running time depends only on the lengths: for secrets. */
export function ctEqual(a: Uint8Array, b: Uint8Array): boolean {
  const [x, y] = [view(a), view(b)];
  if (x.length !== y.length) return false;
  let diff = 0;
  for (let i = 0; i < x.length; i++) diff |= (x[i] ?? 0) ^ (y[i] ?? 0);
  return diff === 0;
}

export function isZero(b: Uint8Array): boolean {
  let acc = 0;
  for (const x of view(b)) acc |= x;
  return acc === 0;
}

export function beToBig(b: Uint8Array): bigint {
  let n = 0n;
  for (const x of view(b)) n = (n << 8n) | BigInt(x);
  return n;
}

export function leToBig(b: Uint8Array): bigint {
  const v = view(b);
  let n = 0n;
  for (let i = v.length - 1; i >= 0; i--) n = (n << 8n) | BigInt(v[i] ?? 0);
  return n;
}

/** `n` as `len` big-endian bytes; `n` must fit. */
export function bigToBE(n: bigint, len: number): Uint8Array {
  const out = new Uint8Array(len);
  let v = n;
  for (let i = len - 1; i >= 0; i--) {
    out[i] = Number(v & 0xffn);
    v >>= 8n;
  }
  return out;
}

/** `n` as `len` little-endian bytes; `n` must fit. */
export function bigToLE(n: bigint, len: number): Uint8Array {
  const out = new Uint8Array(len);
  let v = n;
  for (let i = 0; i < len; i++) {
    out[i] = Number(v & 0xffn);
    v >>= 8n;
  }
  return out;
}

export function u32be(i: number): Uint8Array {
  return Uint8Array.of((i >>> 24) & 0xff, (i >>> 16) & 0xff, (i >>> 8) & 0xff, i & 0xff);
}

export function u32le(i: number): Uint8Array {
  return Uint8Array.of(i & 0xff, (i >>> 8) & 0xff, (i >>> 16) & 0xff, (i >>> 24) & 0xff);
}

/** A non-negative remainder. */
export function mod(a: bigint, m: bigint): bigint {
  const r = a % m;
  return r < 0n ? r + m : r;
}

export function powMod(base: bigint, exp: bigint, m: bigint): bigint {
  let result = 1n;
  let b = mod(base, m);
  let e = exp;
  while (e > 0n) {
    if (e & 1n) result = (result * b) % m;
    b = (b * b) % m;
    e >>= 1n;
  }
  return result;
}

const HEX = "0123456789abcdef";

export function hex(bytes: Uint8Array): string {
  let out = "";
  for (const b of view(bytes)) out += (HEX[b >> 4] ?? "") + (HEX[b & 15] ?? "");
  return out;
}
