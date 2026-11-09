/**
 * How large a text input is, for the caps: its UTF-8 length, plus, for every JSON number in it, its exponent's
 * magnitude. Reading a number converts it exactly (canonical-encoding.md section 3), so `8e9999` (six characters)
 * becomes a 33,216-bit integer, and a text of them would cost far more to read than its length says: a megabyte of
 * them takes minutes to parse. Counted this way, a number costs what its plain decimal would, so the work of reading a
 * text is bounded by its measure, however its numbers are written.
 *
 * The scan reads JSON's strings as the parser does (a quote opens and closes one, a backslash escapes the next code
 * unit), so it finds every number the parser converts. It does not check the text: the parser does that afterwards.
 */
import { models } from "@kd5ujc/dilf4s-core";

/**
 * The most an exponent counts: one past the largest scale. The parser refuses a number of a larger scale from its
 * digits alone, before converting it, so a larger exponent costs no more.
 */
export const EXPONENT_CEILING = models.limits.CANONICAL_MAX_SCALE + 1;

const QUOTE = 0x22;
const BACKSLASH = 0x5c;
const MINUS = 0x2d;
const PLUS = 0x2b;
const DOT = 0x2e;
const ZERO = 0x30;

const isDigit = (c: number): boolean => c >= ZERO && c <= 0x39;

/** The end of the run of digits from `i`. */
function digitsEnd(text: string, i: number): number {
  let p = i;
  for (; p < text.length && isDigit(text.charCodeAt(p)); p++);
  return p;
}

/** The UTF-8 length of the character at `i`, and the code units it spans: an unpaired surrogate counts 3, as core's `utf8Length` counts it. */
function character(text: string, i: number): readonly [number, number] {
  const c = text.charCodeAt(i);
  if (c < 0x80) return [1, 1];
  if (c < 0x800) return [2, 1];
  const d = text.charCodeAt(i + 1);
  return c >= 0xd800 && c <= 0xdbff && d >= 0xdc00 && d <= 0xdfff ? [4, 2] : [3, 1];
}

/**
 * The number token starting at `i` (a `-` or a digit, outside a string), as JSON's grammar reads it: where it ends
 * (always past `i`), and what its exponent adds, at most {@link EXPONENT_CEILING}.
 */
function numberAt(text: string, i: number): readonly [number, number] {
  const integerEnd = digitsEnd(text, text.charCodeAt(i) === MINUS ? i + 1 : i);
  const fractionEnd = text.charCodeAt(integerEnd) === DOT ? digitsEnd(text, integerEnd + 1) : integerEnd;
  const e = text.charCodeAt(fractionEnd);
  if (e !== 0x65 && e !== 0x45) return [fractionEnd, 0];
  const sign = text.charCodeAt(fractionEnd + 1);
  const start = sign === PLUS || sign === MINUS ? fractionEnd + 2 : fractionEnd + 1;
  const end = digitsEnd(text, start);
  let first = start;
  for (; first < end && text.charCodeAt(first) === ZERO; first++);
  // more than five significant digits is past the ceiling: never converted here either
  const magnitude = end - first > 5 ? EXPONENT_CEILING : Number(`0${text.slice(first, end)}`);
  return [end, Math.min(magnitude, EXPONENT_CEILING)];
}

/**
 * The measure of `text`: its UTF-8 length plus its numbers' exponents. The scan stops once the measure is past
 * `limit`, returning a value past it, so it costs O(min(length, limit)); a text with more code units than `limit` is
 * past it at once, as no code unit is less than a byte.
 */
export function measure(text: string, limit: number): number {
  if (text.length > limit) return text.length;
  let total = 0;
  let inString = false;
  for (let i = 0; i < text.length && total <= limit; ) {
    const c = text.charCodeAt(i);
    if (!inString && (c === MINUS || isDigit(c))) {
      const [end, exponent] = numberAt(text, i);
      total += end - i + exponent;
      i = end;
      continue;
    }
    if (c === QUOTE) inString = !inString;
    // an escape and the code unit it escapes are read together, so an escaped quote never closes the string
    const escaped = inString && c === BACKSLASH && i + 1 < text.length;
    const [bytes, units] = character(text, escaped ? i + 1 : i);
    total += escaped ? bytes + 1 : bytes;
    i += escaped ? units + 1 : units;
  }
  return total;
}
