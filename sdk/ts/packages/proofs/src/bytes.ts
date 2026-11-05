/**
 * Byte-string helpers shared by the verifiers. A caller's `Uint8Array` may be a subclass whose `length`, accessors or
 * iterator are its own, so each helper reads bytes through core's `plainBytes` (the array's internal slots), and reads
 * what is not bytes as empty: the public entry points refuse such input first, where their result has room.
 */
import { type cbor, digest, hashCommitment, hashParts, plainBytes, takeBytes, type HashAlgorithm, type Result, type Value } from "@kd5ujc/dilf4s-core";

const EMPTY = new Uint8Array(0);

/** `b` as a plain view over its own memory, its length the array's own; empty if it is not bytes. */
export const view = (b: Uint8Array): Uint8Array => plainBytes(b) ?? EMPTY;

/** A fresh plain copy, from the array's internal slots (core's `takeBytes`): `slice` on a Node `Buffer` would share memory. */
export function copyBytes(bytes: Uint8Array): Uint8Array {
  return takeBytes(bytes) ?? new Uint8Array(0);
}

export function bytesEqual(a: Uint8Array, b: Uint8Array): boolean {
  const [x, y] = [view(a), view(b)];
  if (x.length !== y.length) return false;
  for (let i = 0; i < x.length; i++) if (x[i] !== y[i]) return false;
  return true;
}

/** Unsigned lexicographic order, a proper prefix first (Scala's `Cbor.compareBytes`). */
export function compareBytes(a: Uint8Array, b: Uint8Array): number {
  const [p, q] = [view(a), view(b)];
  const n = Math.min(p.length, q.length);
  for (let i = 0; i < n; i++) {
    const x = p[i] ?? 0;
    const y = q[i] ?? 0;
    if (x !== y) return x - y;
  }
  return p.length - q.length;
}

export function concatBytes(parts: readonly Uint8Array[]): Uint8Array {
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

/** The bytes of an ASCII string (domain tags, state keys). */
export function ascii(s: string): Uint8Array {
  const out = new Uint8Array(s.length);
  for (let i = 0; i < s.length; i++) out[i] = s.charCodeAt(i) & 0x7f;
  return out;
}

/** Whether every byte is ASCII. */
export function isAscii(bytes: Uint8Array): boolean {
  return view(bytes).every((b) => b < 0x80);
}

/** The string of ASCII bytes. */
export function fromAscii(bytes: Uint8Array): string {
  let s = "";
  for (const b of view(bytes)) s += String.fromCharCode(b);
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

  /** A copy of the tag hash: a hash is never handed this instance's own state. */
  #tag(): Uint8Array {
    return copyBytes(this.#tagHash);
  }

  hash(message: Uint8Array): Uint8Array {
    const t = this.#tag();
    return hashParts(this.algorithm, [t, t, message]);
  }

  /**
   * The tagged hash of `commit(value)`, streamed into the hash (core's `hashCommitment`, with memory as core's
   * `cbor.ts` `STAGED_BYTES` states), with `valueCommitment`'s ceiling and refusals (a value that is not a canonical
   * value is a `TypeMismatch`).
   */
  commitment(value: Value): Result<Uint8Array, cbor.EncodeError> {
    const t = this.#tag();
    return hashCommitment(this.algorithm, [t, t], value);
  }

  hashParts(parts: readonly Uint8Array[]): Uint8Array {
    const t = this.#tag();
    return hashParts(this.algorithm, [t, t, ...parts]);
  }
}
