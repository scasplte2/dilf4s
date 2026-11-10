/**
 * The JSON mapping (docs/specs/canonical-encoding.md section 3).
 *
 * {@link parse} is a strict RFC 8259 parser that reads numbers exactly from their source text (never through a
 * double, unlike `JSON.parse`), rejects duplicate member names and unpaired surrogates, and keeps its own frame stack,
 * so nesting is a typed error rather than a stack overflow. {@link render} is the display view, and fails for values
 * JSON cannot represent.
 */
import { twosAndFives } from "./bigint.js";
import { byteLimit, Meter } from "./budget.js";
import type { CanonicalError, JsonSyntaxReason, NotJson, TooLarge, TypeMismatch } from "./error.js";
import { CANONICAL_MAX_DIGITS, CANONICAL_MAX_JSON_EXPONENTS, CANONICAL_MAX_SCALE } from "./generated/limits.js";
import { KeySet } from "./keyset.js";
import { Preview } from "./preview.js";
import { err, ok, type Result } from "./result.js";
import { arrayOfValues, BoolValue, codePointView, IntValue, isValue, mapOfValues, MAX_DEPTH, NullValue, RationalValue, TextValue, type Entry, type Value } from "./value.js";

const INT32_MAX = 2n ** 31n - 1n;
const INT32_MIN = -(2n ** 31n);

/** The largest accepted `|fraction digits - exponent|` of a JSON number, bounding the `10^scale` it materialises. */
export const DEFAULT_MAX_SCALE = CANONICAL_MAX_SCALE;

/**
 * The most significant digits a JSON number's significand (integer and fraction digits, leading zeros aside) or
 * exponent may have, read or written. Decimal conversion is superlinear, so the bound is checked on the text before
 * any conversion; it equals the largest scale, so every decimal of an accepted scale has a form within it.
 */
export const MAX_DIGITS = CANONICAL_MAX_DIGITS;

/**
 * The most a text's numbers may spend on their exponents, summed: each costs its exponent's magnitude, at most
 * `DEFAULT_MAX_SCALE + 1`. An exponent makes a short text stand for a long value (`8e9999` is six characters and 10,000
 * digits), so this bounds what exponents add to the work of reading a text (canonical-encoding.md section 3).
 */
export const MAX_JSON_EXPONENTS = CANONICAL_MAX_JSON_EXPONENTS;

const leadingZeros = (s: string): number => s.length - s.replace(/^0+/, "").length;

type Frame = { readonly object: false; readonly items: Value[] } | { readonly object: true; readonly entries: [string, Value][]; readonly seen: KeySet; key: string };

type State = { readonly step: "value" } | { readonly step: "key" } | { readonly step: "deliver"; readonly value: Value };

const syntax = (offset: number, reason: JsonSyntaxReason): CanonicalError => ({ kind: "JsonSyntax", offset, reason });

const isDigit = (c: string | undefined): boolean => c !== undefined && c >= "0" && c <= "9";

/**
 * Parses JSON text into a canonical value. `maxScale` may lower the scale bound, never raise it: the spec accepts no
 * JSON number of a scale past {@link DEFAULT_MAX_SCALE} (canonical-encoding.md section 3). Text that is not a string,
 * or a scale bound that is not an integer from 0 to {@link DEFAULT_MAX_SCALE}, is a `TypeMismatch`, before anything
 * is read.
 */
export function parse(text: string, maxScale: number = DEFAULT_MAX_SCALE): Result<Value, CanonicalError> {
  if (typeof (text as unknown) !== "string") return err({ kind: "TypeMismatch", reason: "the JSON text is not a string" });
  if (!Number.isSafeInteger(maxScale) || maxScale < 0 || maxScale > DEFAULT_MAX_SCALE) {
    return err({ kind: "TypeMismatch", reason: `the scale bound is not an integer from 0 to ${DEFAULT_MAX_SCALE.toString()}` });
  }
  return new Parser(text, maxScale).run();
}

class Parser {
  /** What the numbers read so far have spent on their exponents. */
  private spent = 0;

  constructor(
    private readonly s: string,
    private readonly maxScale: number,
  ) {}

  private skipWs(i: number): number {
    let p = i;
    for (;;) {
      const c = this.s[p];
      if (c !== " " && c !== "\t" && c !== "\n" && c !== "\r") return p;
      p++;
    }
  }

  private digits(i: number): number {
    let p = i;
    while (isDigit(this.s[p])) p++;
    return p;
  }

  run(): Result<Value, CanonicalError> {
    const stack: Frame[] = [];
    let pos = 0;
    let state: State = { step: "value" };
    for (;;) {
      switch (state.step) {
        case "value": {
          const p = this.skipWs(pos);
          const c = this.s[p];
          if ((c === "{" || c === "[") && stack.length >= MAX_DEPTH) {
            // a container opened inside MAX_DEPTH open ones is refused at its bracket, an empty one too
            // (canonical-encoding.md section 3): the first fault in reading order
            return err({ kind: "DepthExceeded", limit: MAX_DEPTH });
          } else if (c === "{" || c === "[") {
            const q = this.skipWs(p + 1);
            const object = c === "{";
            if (this.s[q] === (object ? "}" : "]")) {
              const empty = object ? mapOfValues([]) : arrayOfValues([]);
              if (!empty.ok) return empty;
              state = { step: "deliver", value: empty.value };
              pos = q + 1;
            } else {
              stack.push(object ? { object: true, entries: [], seen: new KeySet(), key: "" } : { object: false, items: [] });
              state = object ? { step: "key" } : { step: "value" };
              pos = q;
            }
          } else if (c !== undefined) {
            const v = this.scalar(p);
            if (!v.ok) return v;
            state = { step: "deliver", value: v.value.value };
            pos = v.value.end;
          } else {
            return err(syntax(p, "expectedValue"));
          }
          break;
        }
        case "key": {
          const p = this.skipWs(pos);
          if (this.s[p] !== '"') return err(syntax(p, "expectedMemberName"));
          const k = this.string(p);
          if (!k.ok) return k;
          const frame = stack.at(-1);
          if (!frame?.object) return err(syntax(p, "expectedValue"));
          if (frame.seen.has(k.value.text)) return err({ kind: "DuplicateKey", key: Preview.of(k.value.text) });
          const colon = this.skipWs(k.value.end);
          if (this.s[colon] !== ":") return err(syntax(colon, "expectedColon"));
          frame.seen.add(k.value.text);
          frame.key = k.value.text;
          state = { step: "value" };
          pos = colon + 1;
          break;
        }
        case "deliver": {
          const p = this.skipWs(pos);
          const frame = stack.at(-1);
          const c = this.s[p];
          if (frame === undefined) return p === this.s.length ? ok(state.value) : err(syntax(p, "trailingContent"));
          if (!frame.object) {
            if (c !== "," && c !== "]") return err(syntax(p, "expectedCommaOrBracket"));
            frame.items.push(state.value);
            if (c === ",") {
              state = { step: "value" };
            } else {
              stack.pop();
              const arr = arrayOfValues(frame.items);
              if (!arr.ok) return arr;
              state = { step: "deliver", value: arr.value };
            }
          } else {
            if (c !== "," && c !== "}") return err(syntax(p, "expectedCommaOrBrace"));
            frame.entries.push([frame.key, state.value]);
            if (c === ",") {
              state = { step: "key" };
            } else {
              stack.pop();
              const map = mapOfValues(frame.entries);
              if (!map.ok) return map;
              state = { step: "deliver", value: map.value };
            }
          }
          pos = p + 1;
          break;
        }
      }
    }
  }

  private scalar(p: number): Result<{ readonly value: Value; readonly end: number }, CanonicalError> {
    const c = this.s[p];
    if (c === '"') {
      const str = this.string(p);
      if (!str.ok) return str;
      const t = TextValue.of(str.value.text);
      return t.ok ? ok({ value: t.value, end: str.value.end }) : t;
    }
    if (c === "t" && this.s.startsWith("true", p)) return ok({ value: BoolValue.true, end: p + 4 });
    if (c === "f" && this.s.startsWith("false", p)) return ok({ value: BoolValue.false, end: p + 5 });
    if (c === "n" && this.s.startsWith("null", p)) return ok({ value: NullValue.instance, end: p + 4 });
    if (c === "-" || isDigit(c)) return this.number(p);
    return err(syntax(p, "expectedValue"));
  }

  /**
   * `-? (0 | [1-9][0-9]*) (. [0-9]+)? ([eE] [+-]? [0-9]+)?`, converted exactly. Its digits are counted, then its scale
   * bounded, before any conversion: a refusal by a bound gives lengths, never the digits.
   */
  private number(p: number): Result<{ readonly value: Value; readonly end: number }, CanonicalError> {
    const s = this.s;
    const negative = s[p] === "-";
    const intStart = negative ? p + 1 : p;
    const intEnd = this.digits(intStart);
    const hasFrac = s[intEnd] === ".";
    const fracEnd = hasFrac ? this.digits(intEnd + 1) : intEnd;
    const hasExp = s[fracEnd] === "e" || s[fracEnd] === "E";
    const expSign = s[fracEnd + 1];
    const expStart = hasExp && (expSign === "+" || expSign === "-") ? fracEnd + 2 : fracEnd + 1;
    const end = hasExp ? this.digits(expStart) : fracEnd;
    const intDigits = s.slice(intStart, intEnd);
    const fracDigits = hasFrac ? s.slice(intEnd + 1, fracEnd) : "";
    const expDigits = hasExp ? s.slice(expStart, end) : "0";
    const malformed =
      intDigits.length === 0 || (intDigits.length > 1 && intDigits.startsWith("0")) || (hasFrac && fracDigits.length === 0) || (hasExp && expDigits.length === 0);
    // a refusal describes the number by its length and offset, never its text, whose size the caller chose
    const described = { length: end - p, offset: p } as const;
    if (malformed) return err({ kind: "InvalidNumber", ...described });
    // the integer digits have no leading zeros but a lone 0, after which the fraction's leading zeros are not counted
    const significand = intDigits === "0" ? fracDigits.length - leadingZeros(fracDigits) : intDigits.length + fracDigits.length;
    const exponentDigits = expDigits.length - leadingZeros(expDigits);
    // the significand's digits first, then the exponent's, as every SDK reports them
    const digits = significand > MAX_DIGITS ? significand : exponentDigits;
    if (digits > MAX_DIGITS) return err({ kind: "InvalidNumber", ...described, bound: { kind: "Digits", digits, limit: MAX_DIGITS } });
    // an exponent of more than 18 digits is past every scale a text this long can reach: saturate rather than convert
    const magnitude = exponentDigits > 18 ? 10n ** 19n : BigInt(expDigits);
    const exponent = magnitude * (hasExp && expSign === "-" ? -1n : 1n);
    const scale = BigInt(fracDigits.length) - exponent;
    if ((scale < 0n ? -scale : scale) > BigInt(this.maxScale)) return err({ kind: "InvalidNumber", ...described, bound: { kind: "Scale", scale: Number(scale > INT32_MAX ? INT32_MAX : scale < INT32_MIN ? INT32_MIN : scale), limit: this.maxScale } });
    // its exponent's magnitude, at most one past the largest scale, charged to the text's budget before converting
    const total = this.spent + Number(magnitude > BigInt(DEFAULT_MAX_SCALE + 1) ? BigInt(DEFAULT_MAX_SCALE + 1) : magnitude);
    if (total > MAX_JSON_EXPONENTS) return err({ kind: "InvalidNumber", ...described, bound: { kind: "Exponents", exponents: total, limit: MAX_JSON_EXPONENTS } });
    this.spent = total;
    const unscaled = BigInt(intDigits + fracDigits) * (negative ? -1n : 1n);
    // a decimal's denominator is 10^scale: reduced by its primes 2 and 5 alone, never by a general gcd
    // a positive exponent can take an integer past MAX_INTEGER_BITS (9e9999): refused, as every producer does
    const value = scale >= 0n ? RationalValue.decimal(unscaled, Number(scale)) : IntValue.of(unscaled * 10n ** -scale);
    return value.ok ? ok({ value: value.value, end }) : value;
  }

  /** A JSON string from its opening quote; escapes may produce unpaired surrogates, which the caller rejects. */
  private string(open: number): Result<{ readonly text: string; readonly end: number }, CanonicalError> {
    const s = this.s;
    const parts: string[] = [];
    let segment = open + 1;
    let i = open + 1;
    for (;;) {
      const c = s[i];
      if (c === undefined) return err(syntax(i, "unterminatedString"));
      if (c === '"') {
        parts.push(s.slice(segment, i));
        return ok({ text: parts.join(""), end: i + 1 });
      }
      if (c < " ") return err(syntax(i, "controlCharacter"));
      if (c !== "\\") {
        i++;
        continue;
      }
      parts.push(s.slice(segment, i));
      const e = s[i + 1];
      const simple: Record<string, string> = { '"': '"', "\\": "\\", "/": "/", b: "\b", f: "\f", n: "\n", r: "\r", t: "\t" };
      const mapped = e === undefined ? undefined : simple[e];
      if (mapped !== undefined) {
        parts.push(mapped);
        i += 2;
      } else if (e === "u" && /^[0-9a-fA-F]{4}$/.test(s.slice(i + 2, i + 6))) {
        parts.push(String.fromCharCode(Number.parseInt(s.slice(i + 2, i + 6), 16)));
        i += 6;
      } else {
        return err(syntax(i, "invalidEscape"));
      }
      segment = i;
    }
  }
}
Object.freeze(Parser);
Object.freeze(Parser.prototype);

// ------------------------------------------------------------------------------------------------ render

/** `text`, a plain decimal, when its significant digits and scale are within the bounds {@link parse} accepts. */
function within(text: string): string | undefined {
  const unsigned = text.startsWith("-") ? text.slice(1) : text;
  const point = unsigned.indexOf(".");
  const intPart = point < 0 ? unsigned : unsigned.slice(0, point);
  const fracPart = point < 0 ? "" : unsigned.slice(point + 1);
  const significand = intPart === "0" ? fracPart.length - leadingZeros(fracPart) : intPart.length - leadingZeros(intPart) + fracPart.length;
  return significand <= MAX_DIGITS && fracPart.length <= DEFAULT_MAX_SCALE ? text : undefined;
}

/**
 * An integer's decimal, which {@link parse} reads back: total, since every integer is within `MAX_INTEGER_BITS`, so
 * it has at most {@link MAX_DIGITS} digits and converting it is bounded work.
 */
function integerText(n: IntValue): string {
  return n.value.toString();
}

/** Why a rational has no JSON decimal: none terminates, or it is past the bounds {@link parse} accepts. */
export interface NoDecimal {
  readonly kind: "NotJsonRepresentable";
  readonly what: Extract<NotJson, "non-terminating rational" | "over-long number">;
}

/** Why {@link decimal} gave no decimal: the rational has none, or the argument is not a rational. */
export type DecimalError = NoDecimal | TypeMismatch;

const nonTerminating: NoDecimal = Object.freeze({ kind: "NotJsonRepresentable", what: "non-terminating rational" });
const overLong: NoDecimal = Object.freeze({ kind: "NotJsonRepresentable", what: "over-long number" });

/** {@link decimal} of a rational this package holds, unchecked: what {@link renderInto} writes. */
function decimalText(r: RationalValue): Result<string, NoDecimal> {
  const [factors] = twosAndFives(r.denominator);
  if (factors === undefined) return err(nonTerminating);
  const scale = Math.max(...factors);
  if (scale > DEFAULT_MAX_SCALE) return err(overLong);
  const scaled = r.numerator * (10n ** BigInt(scale) / r.denominator);
  const digits = (scaled < 0n ? -scaled : scaled).toString().padStart(scale + 1, "0");
  const text = within(`${scaled < 0n ? "-" : ""}${digits.slice(0, digits.length - scale)}.${digits.slice(digits.length - scale)}`);
  return text === undefined ? err(overLong) : ok(text);
}

/**
 * The exact decimal of a rational, within the bounds {@link parse} accepts: `NotJsonRepresentable` with
 * `"non-terminating rational"` when its denominator has prime factors other than 2 and 5, or `"over-long number"` past
 * the bounds, both found from the denominator's factors (`twosAndFives`) before any digit is produced. An argument that
 * is not a `RationalValue` made by this package (see `isValue`) is a `TypeMismatch`, never read through its
 * `numerator` and `denominator`.
 */
export function decimal(r: RationalValue): Result<string, DecimalError> {
  const given: unknown = r;
  return isValue(given) && given.kind === "rational" ? decimalText(given) : err({ kind: "TypeMismatch", reason: "expected a RationalValue" });
}

function quote(s: string): string {
  let out = '"';
  for (const ch of s) {
    switch (ch) {
      case '"':
        out += '\\"';
        break;
      case "\\":
        out += "\\\\";
        break;
      case "\n":
        out += "\\n";
        break;
      case "\r":
        out += "\\r";
        break;
      case "\t":
        out += "\\t";
        break;
      case "\b":
        out += "\\b";
        break;
      case "\f":
        out += "\\f";
        break;
      default:
        out += ch < " " ? `\\u${(ch.codePointAt(0) ?? 0).toString(16).padStart(4, "0")}` : ch;
    }
  }
  return out + '"';
}

/** The UTF-8 length of {@link quote}`(s)`, counted without building it, so a budget is checked before escaping. */
function quotedLength(s: string): number {
  let n = 2;
  for (const ch of s) {
    const c = ch.codePointAt(0) ?? 0;
    const escaped = ch === '"' || ch === "\\" || ch === "\n" || ch === "\r" || ch === "\t" || ch === "\b" || ch === "\f";
    n += escaped ? 2 : c < 0x20 ? 6 : c < 0x80 ? 1 : c < 0x800 ? 2 : c < 0x10000 ? 3 : 4;
  }
  return n;
}

/**
 * The longest JSON {@link renderWithin} produces, in UTF-8 bytes, whatever limit it is given. A UTF-16 code unit takes
 * at least one UTF-8 byte, so the text stays within every engine's string limit (V8's is `2^29 - 24` code units):
 * rendering never throws. There is no render without a limit: a caller names one, and a larger one is this.
 */
export const MAX_RENDER_BYTES = 2 ** 29 - 24;

/** An array, or an object's members in code-point order of their names, being rendered: the parts and the next one. */
type Rendering = { readonly object: false; readonly items: readonly Value[]; at: number } | { readonly object: true; readonly members: readonly Entry[]; at: number };

/** Why a value was not rendered: JSON cannot represent it, or its JSON is longer than the limit. */
export type RenderError = CanonicalError | TooLarge;

/**
 * Compact JSON with object members in code-point order of their names, if it is at most `maxBytes` long in UTF-8 (a
 * fraction rounds down, `NaN` or a negative limit is 0, and no limit is above {@link MAX_RENDER_BYTES}, past which no
 * string can hold the text). Fails for bytes, non-terminating rationals and over-long numbers, and with `TooLarge` at
 * the first part found past the limit, whichever comes first.
 *
 * O(min(weight, `maxBytes`)) apart from putting each object's members in order, which is linear in the object's weight
 * (its code-point view, which a map built from entries keeps): a value's JSON is at least as long in UTF-8 as its
 * weight (each node writes at least a byte, a text or key its UTF-8 and quotes, a number of `l` whole limbs more than
 * `l` digits), so each part is refused in O(1) when its weight is over what is left, and every part rendered fits.
 * Iterative, with an explicit stack of at most {@link MAX_DEPTH} open containers.
 */
export function renderWithin(value: Value, maxBytes: number): Result<string, RenderError> {
  const out: string[] = [];
  const written = renderInto(value, maxBytes, out, (pieces, piece) => {
    pieces.push(piece);
    return pieces;
  });
  return written.ok ? ok(written.value.join("")) : written;
}

/**
 * Where rendered text goes, as a state `S` threaded through every write: {@link renderInto} uses each state once,
 * passing on the one a write returns, so a sink may append in place.
 */
export type TextSink<S> = (s: S, piece: string) => S;

/**
 * {@link renderWithin} into any {@link TextSink}, starting from `s`: the rendering reaches the sink in order, each of
 * its characters once, in pieces that are a scalar, a quoted text or key, or punctuation, never a container's text.
 * A value that is not a canonical value, a limit that is not a number, or a sink that is not a function is a
 * `TypeMismatch`, before anything is written.
 */
export function renderInto<S>(value: Value, maxBytes: number, s: S, sink: TextSink<S>): Result<S, RenderError> {
  // what a caller can pass past the types is refused in O(1), never read: the parts of a value are its own
  if (!isValue(value)) return err({ kind: "TypeMismatch", reason: "expected a canonical value" });
  if (typeof (maxBytes as unknown) !== "number") return err({ kind: "TypeMismatch", reason: "the byte limit is not a number" });
  if (typeof (sink as unknown) !== "function") return err({ kind: "TypeMismatch", reason: "the sink is not a function" });
  const meter = new Meter(Math.min(byteLimit(maxBytes), MAX_RENDER_BYTES));
  let out = s;
  const stack: Rendering[] = [];
  /** Adds the piece `build` makes, of `bytes` UTF-8 bytes, if they fit: nothing is built before it is paid for. */
  const emit = (bytes: number, build: () => string): boolean => {
    const fits = meter.take(bytes);
    if (fits) out = sink(out, build());
    return fits;
  };
  const ascii = (piece: string): boolean => emit(piece.length, () => piece);
  /** A value's JSON, or its opening bracket with its parts now open: `undefined`, or why it failed. */
  const node = (v: Value): RenderError | undefined => {
    if (!meter.admits(v.weight)) return meter.refusal();
    switch (v.kind) {
      case "null":
        return ascii("null") ? undefined : meter.refusal();
      case "bool":
        return ascii(v.value ? "true" : "false") ? undefined : meter.refusal();
      case "int":
        return ascii(integerText(v)) ? undefined : meter.refusal();
      case "rational": {
        const d = decimalText(v);
        if (!d.ok) return d.error;
        return ascii(d.value) ? undefined : meter.refusal();
      }
      case "text": {
        const s = v.value;
        return emit(quotedLength(s), () => quote(s)) ? undefined : meter.refusal();
      }
      case "bytes":
        return { kind: "NotJsonRepresentable", what: "bytes" };
      case "array":
        if (!ascii("[")) return meter.refusal();
        stack.push({ object: false, items: v.items, at: 0 });
        return undefined;
      case "map":
        if (!ascii("{")) return meter.refusal();
        // members in code-point order of their names, the map's own view of its entries (linear in its weight)
        stack.push({ object: true, members: codePointView(v), at: 0 });
        return undefined;
    }
  };
  const first = node(value);
  if (first !== undefined) return err(first);
  for (let top = stack.at(-1); top !== undefined; top = stack.at(-1)) {
    const i = top.at++;
    const comma = i > 0 ? "," : "";
    if (top.object) {
      const m = top.members[i];
      if (m === undefined) {
        if (!ascii("}")) return err(meter.refusal());
        stack.pop();
        continue;
      }
      const key = m.key;
      if (!emit(comma.length + quotedLength(key) + 1, () => `${comma}${quote(key)}:`)) return err(meter.refusal());
      const e = node(m.value);
      if (e !== undefined) return err(e);
    } else {
      const item = top.items[i];
      if (item === undefined) {
        if (!ascii("]")) return err(meter.refusal());
        stack.pop();
        continue;
      }
      if (i > 0 && !ascii(",")) return err(meter.refusal());
      const e = node(item);
      if (e !== undefined) return err(e);
    }
  }
  return ok(out);
}
