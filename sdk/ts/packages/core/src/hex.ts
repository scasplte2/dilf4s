/** Hexadecimal text, as the conformance vectors write bytes. */
import { err, ok, type Result } from "./result.js";

export interface InvalidHex {
  readonly kind: "InvalidHex";
  readonly offset: number;
}

/** Lowercase hexadecimal of `bytes`. */
export function toHex(bytes: Uint8Array): string {
  let out = "";
  for (const b of bytes) out += b.toString(16).padStart(2, "0");
  return out;
}

/** The bytes of hexadecimal text (either case), which must have an even number of digits. */
export function fromHex(text: string): Result<Uint8Array, InvalidHex> {
  if (text.length % 2 !== 0) return err({ kind: "InvalidHex", offset: text.length });
  const out = new Uint8Array(text.length / 2);
  for (let i = 0; i < out.length; i++) {
    const pair = text.slice(2 * i, 2 * i + 2);
    if (!/^[0-9a-fA-F]{2}$/.test(pair)) return err({ kind: "InvalidHex", offset: 2 * i });
    out[i] = Number.parseInt(pair, 16);
  }
  return ok(out);
}
