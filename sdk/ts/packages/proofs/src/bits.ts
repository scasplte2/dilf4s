/**
 * Bit strings, most significant bit first (docs/specs/trie.md section 1), and the bitwise key order of section 4, where
 * a proper prefix sorts before its extensions. A bit string is packed into bytes with zero padding bits, so two equal
 * bit strings have equal bytes.
 */
export interface Bits {
  readonly bytes: Uint8Array;
  readonly length: number;
}

export const EMPTY_BITS: Bits = { bytes: new Uint8Array(0), length: 0 };

/** The bits of a byte string (shares `bytes`: callers pass bytes they own). */
export function bitsOf(bytes: Uint8Array): Bits {
  return { bytes, length: bytes.length * 8 };
}

export function bit(s: Bits, i: number): boolean {
  return (((s.bytes[i >> 3] ?? 0) >> (7 - (i & 7))) & 1) === 1;
}

function setBit(out: Uint8Array, i: number): void {
  out[i >> 3] = (out[i >> 3] ?? 0) | (0x80 >> (i & 7));
}

/** Bits `from` (inclusive) to `to` (exclusive) of `s`. */
export function slice(s: Bits, from: number, to: number): Bits {
  const n = Math.max(0, to - from);
  if (from % 8 === 0 && n % 8 === 0) return { bytes: s.bytes.slice(from >> 3, (from + n) >> 3), length: n };
  const out = new Uint8Array((n + 7) >> 3);
  for (let i = 0; i < n; i++) if (bit(s, from + i)) setBit(out, i);
  return { bytes: out, length: n };
}

export function drop(s: Bits, n: number): Bits {
  return slice(s, n, s.length);
}

/** The concatenation of `parts`. */
export function concat(parts: readonly Bits[]): Bits {
  let n = 0;
  for (const p of parts) n += p.length;
  const out = new Uint8Array((n + 7) >> 3);
  let at = 0;
  for (const p of parts) {
    // whole bytes, shifted into place: padding bits are zero, as are the output's bits beyond `at`
    const [base, shift] = [at >> 3, at & 7];
    const used = (p.length + 7) >> 3;
    for (let j = 0; j < used; j++) {
      const v = p.bytes[j] ?? 0;
      out[base + j] = (out[base + j] ?? 0) | (v >> shift);
      if (shift !== 0 && base + j + 1 < out.length) out[base + j + 1] = (out[base + j + 1] ?? 0) | ((v << (8 - shift)) & 0xff);
    }
    at += p.length;
  }
  return { bytes: out, length: n };
}

/** `s` followed by one bit. */
export function append(s: Bits, b: boolean): Bits {
  const out = new Uint8Array((s.length + 8) >> 3);
  out.set(s.bytes.subarray(0, (s.length + 7) >> 3));
  if (b) setBit(out, s.length);
  return { bytes: out, length: s.length + 1 };
}

/** The length of the longest common prefix of `a` from bit `ia` and `b` from bit `ib`. */
export function commonPrefixFrom(a: Bits, ia: number, b: Bits, ib: number): number {
  const n = Math.min(a.length - ia, b.length - ib);
  let i = 0;
  // with equal alignment, bit by bit to a byte boundary, then whole bytes at a time
  if (ia % 8 === ib % 8) {
    while (i < n && (ia + i) % 8 !== 0 && bit(a, ia + i) === bit(b, ib + i)) i++;
    if ((ia + i) % 8 === 0) while (i + 8 <= n && a.bytes[(ia + i) >> 3] === b.bytes[(ib + i) >> 3]) i += 8;
  }
  while (i < n && bit(a, ia + i) === bit(b, ib + i)) i++;
  return i;
}

export function commonPrefix(a: Bits, b: Bits): number {
  return commonPrefixFrom(a, 0, b, 0);
}

export function equals(a: Bits, b: Bits): boolean {
  return a.length === b.length && commonPrefix(a, b) === a.length;
}

export function startsWith(s: Bits, prefix: Bits): boolean {
  return prefix.length <= s.length && commonPrefix(s, prefix) === prefix.length;
}

/** Whether `s` from bit `from` starts with `prefix`. */
export function startsWithAt(s: Bits, from: number, prefix: Bits): boolean {
  return prefix.length <= s.length - from && commonPrefixFrom(s, from, prefix, 0) === prefix.length;
}

/** The bytes of a whole-byte bit string. */
export function toBytes(s: Bits): Uint8Array {
  return s.bytes.slice(0, s.length >> 3);
}

// ------------------------------------------------------------------ key order (trie.md section 4)

/** `a < b` in bitwise lexicographic order, a proper prefix first. */
export function lt(a: Bits, b: Bits): boolean {
  const c = commonPrefix(a, b);
  if (c === a.length) return a.length < b.length;
  if (c === b.length) return false;
  return !bit(a, c);
}

export function lteq(a: Bits, b: Bits): boolean {
  return equals(a, b) || lt(a, b);
}

/** Whether the subtree of keys starting with `prefix` meets `[from, until)`. */
export function intersects(prefix: Bits, from: Bits, until: Bits | undefined): boolean {
  const whollyBelow = !startsWith(from, prefix) && lt(prefix, from);
  const whollyAbove = until !== undefined && lteq(until, prefix);
  return !whollyBelow && !whollyAbove;
}

export function within(key: Bits, from: Bits, until: Bits | undefined): boolean {
  return lteq(from, key) && (until === undefined || lt(key, until));
}
