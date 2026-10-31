/** Byte-string helpers shared by the verifiers. */
import { digest, hashParts, type HashAlgorithm } from "@kd5ujc/dilf4s-core";

/** A fresh plain copy: `slice` on a Node `Buffer` would share memory. */
export function copyBytes(bytes: Uint8Array): Uint8Array {
  return Uint8Array.from(bytes);
}

export function bytesEqual(a: Uint8Array, b: Uint8Array): boolean {
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i++) if (a[i] !== b[i]) return false;
  return true;
}

/** Unsigned lexicographic order, a proper prefix first (Scala's `Cbor.compareBytes`). */
export function compareBytes(a: Uint8Array, b: Uint8Array): number {
  const n = Math.min(a.length, b.length);
  for (let i = 0; i < n; i++) {
    const x = a[i] ?? 0;
    const y = b[i] ?? 0;
    if (x !== y) return x - y;
  }
  return a.length - b.length;
}

export function concatBytes(parts: readonly Uint8Array[]): Uint8Array {
  let n = 0;
  for (const p of parts) n += p.length;
  const out = new Uint8Array(n);
  let at = 0;
  for (const p of parts) {
    out.set(p, at);
    at += p.length;
  }
  return out;
}

/** The bytes of an ASCII string (domain tags, state keys). */
export function ascii(s: string): Uint8Array {
  const out = new Uint8Array(s.length);
  for (let i = 0; i < s.length; i++) out[i] = s.charCodeAt(i) & 0x7f;
  return out;
}

/** Whether every byte is ASCII. */
export function isAscii(bytes: Uint8Array): boolean {
  return bytes.every((b) => b < 0x80);
}

/** The string of ASCII bytes. */
export function fromAscii(bytes: Uint8Array): string {
  let s = "";
  for (const b of bytes) s += String.fromCharCode(b);
  return s;
}

/**
 * `taggedHash_H(tag, msg) = H(H(tag) || H(tag) || msg)` (canonical-encoding.md section 4) for a constant tag. Core's
 * `TaggedHash` needs a validated `DomainTag`, whose factory returns a `Result`; the library's own tags are literals
 * that `test/tags.test.ts` checks against the grammar, so they are hashed directly.
 */
export class ConstantTag {
  readonly #tagHash: Uint8Array;
  constructor(
    readonly algorithm: HashAlgorithm,
    readonly tag: string,
  ) {
    this.#tagHash = digest(algorithm, ascii(tag));
    Object.freeze(this);
  }

  hash(message: Uint8Array): Uint8Array {
    return hashParts(this.algorithm, [this.#tagHash, this.#tagHash, message]);
  }

  hashParts(parts: readonly Uint8Array[]): Uint8Array {
    return hashParts(this.algorithm, [this.#tagHash, this.#tagHash, ...parts]);
  }
}
