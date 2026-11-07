/** Hexadecimal text, as the conformance vectors write bytes. */
import { plainBytes } from "./bytes.js";
import type { TypeMismatch } from "./error.js";
import type { HashErrorOf } from "./hash.js";
import { err, ok, type Result } from "./result.js";

const NOT_BYTES: TypeMismatch = Object.freeze({ kind: "TypeMismatch", reason: "expected a Uint8Array" });
const NOT_A_STRING: TypeMismatch = Object.freeze({ kind: "TypeMismatch", reason: "expected a string" });

const DIGITS = "0123456789abcdef";

/** Lowercase hexadecimal of `bytes`, unchecked: for this SDK's own plain arrays. */
export function toHexOf(bytes: Uint8Array): string {
  let out = "";
  for (const b of bytes) out += (DIGITS[b >> 4] ?? "") + (DIGITS[b & 15] ?? "");
  return out;
}

/**
 * Lowercase hexadecimal of `bytes`, read through a plain view of their own memory (their own `length` and iterator are
 * not believed). What is not a `Uint8Array` (from a JavaScript caller, or a cast) is a `TypeMismatch`, unread.
 */
export function toHex(bytes: Uint8Array): Result<string, TypeMismatch> {
  const view = plainBytes(bytes);
  return view === undefined ? err(NOT_BYTES) : ok(toHexOf(view));
}

/** The value of an ASCII hex digit's code unit, or -1. */
const nibble = (c: number): number => (c >= 0x30 && c <= 0x39 ? c - 0x30 : c >= 0x61 && c <= 0x66 ? c - 0x57 : c >= 0x41 && c <= 0x46 ? c - 0x37 : -1);

/** The offset in `text`, from `from` to `until`, of the first code unit that is not an ASCII hex digit, or -1. */
export function firstNonDigit(text: string, from: number, until: number): number {
  for (let i = from; i < Math.min(until, text.length); i++) if (nibble(text.charCodeAt(i)) < 0) return i;
  return -1;
}

/**
 * The bytes of hexadecimal text: ASCII digits and `a`-`f` in either case, two per byte. Refused as `InvalidHex` at the
 * first character that is not a hex digit, or else, for an odd number of digits, at the text's length: every character
 * before either is ASCII, so the offset is the same in UTF-16 units, UTF-8 bytes and code points
 * (docs/specs/canonical-encoding.md section 4). A JavaScript caller's argument that is not a string is a
 * `TypeMismatch`, unread.
 */
export function fromHex(text: string): Result<Uint8Array, HashErrorOf<"InvalidHex"> | TypeMismatch> {
  if (typeof (text as unknown) !== "string") return err(NOT_A_STRING);
  const bad = firstNonDigit(text, 0, text.length);
  if (bad >= 0) return err({ kind: "InvalidHex", offset: bad });
  if (text.length % 2 !== 0) return err({ kind: "InvalidHex", offset: text.length });
  const out = new Uint8Array(text.length / 2);
  for (let i = 0; i < out.length; i++) out[i] = (nibble(text.charCodeAt(2 * i)) << 4) | nibble(text.charCodeAt(2 * i + 1));
  return ok(out);
}
