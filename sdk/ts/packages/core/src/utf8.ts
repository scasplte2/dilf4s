/**
 * UTF-8 and UTF-16 well-formedness, done by hand: `TextEncoder` silently replaces lone surrogates with U+FFFD and
 * `TextDecoder` drops a leading byte-order mark, either of which would change bytes without an error.
 */

const isHigh = (u: number): boolean => u >= 0xd800 && u <= 0xdbff;
const isLow = (u: number): boolean => u >= 0xdc00 && u <= 0xdfff;

/** Whether every surrogate in `s` is part of a high/low pair. */
export function isWellFormed(s: string): boolean {
  for (let i = 0; i < s.length; i++) {
    const u = s.charCodeAt(i);
    if (isHigh(u)) {
      if (i + 1 >= s.length || !isLow(s.charCodeAt(i + 1))) return false;
      i++;
    } else if (isLow(u)) {
      return false;
    }
  }
  return true;
}

/** The UTF-8 encoding of well-formed text (check with {@link isWellFormed} first). */
export function encodeUtf8(s: string): Uint8Array {
  const out: number[] = [];
  for (const ch of s) {
    const c = ch.codePointAt(0) ?? 0;
    if (c < 0x80) out.push(c);
    else if (c < 0x800) out.push(0xc0 | (c >> 6), 0x80 | (c & 0x3f));
    else if (c < 0x10000) out.push(0xe0 | (c >> 12), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f));
    else out.push(0xf0 | (c >> 18), 0x80 | ((c >> 12) & 0x3f), 0x80 | ((c >> 6) & 0x3f), 0x80 | (c & 0x3f));
  }
  return Uint8Array.from(out);
}

/** The UTF-8 length of well-formed text, without encoding it. */
export function utf8Length(s: string): number {
  let n = 0;
  for (const ch of s) {
    const c = ch.codePointAt(0) ?? 0;
    n += c < 0x80 ? 1 : c < 0x800 ? 2 : c < 0x10000 ? 3 : 4;
  }
  return n;
}

const byteAt = (a: Uint8Array, i: number): number => a[i] ?? -1;
const cont = (a: Uint8Array, i: number): boolean => (byteAt(a, i) & 0xc0) === 0x80 && byteAt(a, i) >= 0;
const within = (a: Uint8Array, i: number, lo: number, hi: number): boolean => byteAt(a, i) >= lo && byteAt(a, i) <= hi;

/** The width of the well-formed UTF-8 sequence at `i` (RFC 3629: shortest form, no surrogates), or 0. */
function sequenceWidth(a: Uint8Array, i: number): number {
  const b = byteAt(a, i);
  if (b < 0) return 0;
  if (b < 0x80) return 1;
  if (b >= 0xc2 && b <= 0xdf && cont(a, i + 1)) return 2;
  if (b === 0xe0 && within(a, i + 1, 0xa0, 0xbf) && cont(a, i + 2)) return 3;
  if (((b >= 0xe1 && b <= 0xec) || b === 0xee || b === 0xef) && cont(a, i + 1) && cont(a, i + 2)) return 3;
  if (b === 0xed && within(a, i + 1, 0x80, 0x9f) && cont(a, i + 2)) return 3;
  if (b === 0xf0 && within(a, i + 1, 0x90, 0xbf) && cont(a, i + 2) && cont(a, i + 3)) return 4;
  if (b >= 0xf1 && b <= 0xf3 && cont(a, i + 1) && cont(a, i + 2) && cont(a, i + 3)) return 4;
  if (b === 0xf4 && within(a, i + 1, 0x80, 0x8f) && cont(a, i + 2) && cont(a, i + 3)) return 4;
  return 0;
}

/** The text of well-formed UTF-8, or `undefined` if `a` is not well-formed. */
export function decodeUtf8(a: Uint8Array): string | undefined {
  const parts: string[] = [];
  const codes: number[] = [];
  let i = 0;
  while (i < a.length) {
    const w = sequenceWidth(a, i);
    if (w === 0) return undefined;
    const b0 = byteAt(a, i);
    const c =
      w === 1
        ? b0
        : w === 2
          ? ((b0 & 0x1f) << 6) | (byteAt(a, i + 1) & 0x3f)
          : w === 3
            ? ((b0 & 0x0f) << 12) | ((byteAt(a, i + 1) & 0x3f) << 6) | (byteAt(a, i + 2) & 0x3f)
            : ((b0 & 0x07) << 18) | ((byteAt(a, i + 1) & 0x3f) << 12) | ((byteAt(a, i + 2) & 0x3f) << 6) | (byteAt(a, i + 3) & 0x3f);
    codes.push(c);
    if (codes.length >= 4096) parts.push(String.fromCodePoint(...codes.splice(0)));
    i += w;
  }
  parts.push(String.fromCodePoint(...codes));
  return parts.join("");
}

/** Code-point order of two well-formed strings (UTF-16 `<` orders surrogate pairs below U+E000..U+FFFF). */
export function compareCodePoints(a: string, b: string): number {
  const ia = a[Symbol.iterator]();
  const ib = b[Symbol.iterator]();
  for (;;) {
    const x = ia.next();
    const y = ib.next();
    if (x.done === true || y.done === true) return (x.done === true ? 0 : 1) - (y.done === true ? 0 : 1);
    const cx = x.value.codePointAt(0) ?? 0;
    const cy = y.value.codePointAt(0) ?? 0;
    if (cx !== cy) return cx - cy;
  }
}

/** Bytewise order of two byte strings: unsigned, a prefix first. */
export function compareBytes(a: Uint8Array, b: Uint8Array): number {
  const n = Math.min(a.length, b.length);
  for (let i = 0; i < n; i++) {
    const d = (a[i] ?? 0) - (b[i] ?? 0);
    if (d !== 0) return d;
  }
  return a.length - b.length;
}
