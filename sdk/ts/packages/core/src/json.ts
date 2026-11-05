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
import type { CanonicalError, TooLarge } from "./error.js";
import { CANONICAL_MAX_DIGITS, CANONICAL_MAX_SCALE } from "./generated/limits.js";
import { KeySet } from "./keyset.js";
import { Preview } from "./preview.js";
import { err, ok, type Result } from "./result.js";
import { compareCodePoints } from "./utf8.js";
import { ArrayValue, BoolValue, entriesView, IntValue, MapValue, MAX_DEPTH, NullValue, RationalValue, TextValue, type Entry, type Value } from "./value.js";

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

const leadingZeros = (s: string): number => s.length - s.replace(/^0+/, "").length;

type Frame = { readonly object: false; readonly items: Value[] } | { readonly object: true; readonly entries: [string, Value][]; readonly seen: KeySet; key: string };

type State = { readonly step: "value" } | { readonly step: "key" } | { readonly step: "deliver"; readonly value: Value };

const syntax = (offset: number, reason: string): CanonicalError => ({ kind: "JsonSyntax", offset, reason });

const isDigit = (c: string | undefined): boolean => c !== undefined && c >= "0" && c <= "9";

/** Parses JSON text into a canonical value. */
export function parse(text: string, maxScale: number = DEFAULT_MAX_SCALE): Result<Value, CanonicalError> {
  return new Parser(text, maxScale).run();
}

class Parser {
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
              const empty = object ? MapValue.of([]) : ArrayValue.of([]);
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
            return err(syntax(p, "expected a value"));
          }
          break;
        }
        case "key": {
          const p = this.skipWs(pos);
          if (this.s[p] !== '"') return err(syntax(p, "expected a member name"));
          const k = this.string(p);
          if (!k.ok) return k;
          const frame = stack.at(-1);
          if (!frame?.object) return err(syntax(p, "expected a value"));
          if (frame.seen.has(k.value.text)) return err({ kind: "DuplicateKey", key: Preview.of(k.value.text) });
          const colon = this.skipWs(k.value.end);
          if (this.s[colon] !== ":") return err(syntax(colon, "expected ':'"));
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
          if (frame === undefined) return p === this.s.length ? ok(state.value) : err(syntax(p, "unexpected trailing content"));
          if (!frame.object) {
            if (c !== "," && c !== "]") return err(syntax(p, "expected ',' or ']'"));
            frame.items.push(state.value);
            if (c === ",") {
              state = { step: "value" };
            } else {
              stack.pop();
              const arr = ArrayValue.of(frame.items);
              if (!arr.ok) return arr;
              state = { step: "deliver", value: arr.value };
            }
          } else {
            if (c !== "," && c !== "}") return err(syntax(p, "expected ',' or '}'"));
            frame.entries.push([frame.key, state.value]);
            if (c === ",") {
              state = { step: "key" };
            } else {
              stack.pop();
              const map = MapValue.of(frame.entries);
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
    return err(syntax(p, "expected a value"));
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
    const digits = Math.max(significand, exponentDigits);
    if (digits > MAX_DIGITS) return err({ kind: "InvalidNumber", ...described, digits });
    // an exponent of more than 18 digits is past every scale a text this long can reach: saturate rather than convert
    const magnitude = exponentDigits > 18 ? 10n ** 19n : BigInt(expDigits);
    const exponent = magnitude * (hasExp && expSign === "-" ? -1n : 1n);
    const scale = BigInt(fracDigits.length) - exponent;
    if ((scale < 0n ? -scale : scale) > BigInt(this.maxScale)) return err({ kind: "InvalidNumber", ...described, scale: Number(scale > INT32_MAX ? INT32_MAX : scale < INT32_MIN ? INT32_MIN : scale) });
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
      if (c === undefined) return err(syntax(i, "unterminated string"));
      if (c === '"') {
        parts.push(s.slice(segment, i));
        return ok({ text: parts.join(""), end: i + 1 });
      }
      if (c < " ") return err(syntax(i, "unescaped control character in string"));
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
        return err(syntax(i, "invalid escape"));
      }
      segment = i;
    }
  }
}

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

/**
 * The exact decimal of a rational, within the bounds {@link parse} accepts: `"non-terminating rational"` when its
 * denominator has prime factors other than 2 and 5, `"over-long number"` past the bounds, both found from the
 * denominator's factors (`twosAndFives`) before any digit is produced.
 */
export function decimalOf(r: RationalValue): Result<string, "non-terminating rational" | "over-long number"> {
  const [factors] = twosAndFives(r.denominator);
  if (factors === undefined) return err("non-terminating rational");
  const scale = Math.max(...factors);
  if (scale > DEFAULT_MAX_SCALE) return err("over-long number");
  const scaled = r.numerator * (10n ** BigInt(scale) / r.denominator);
  const digits = (scaled < 0n ? -scaled : scaled).toString().padStart(scale + 1, "0");
  const text = within(`${scaled < 0n ? "-" : ""}${digits.slice(0, digits.length - scale)}.${digits.slice(digits.length - scale)}`);
  return text === undefined ? err("over-long number") : ok(text);
}

/** The exact decimal of a rational, when it has one within the bounds {@link parse} accepts. */
export function decimal(r: RationalValue): string | undefined {
  const d = decimalOf(r);
  return d.ok ? d.value : undefined;
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
 * O(min(weight, `maxBytes`)) apart from sorting object members: a value's JSON is at least as long in UTF-8 as its
 * weight (each node writes at least a byte, a text or key its UTF-8 and quotes, a number of `l` whole limbs more than
 * `l` digits), so each part is refused in O(1) when its weight is over what is left, and every part rendered fits.
 * Iterative, with an explicit stack of at most {@link MAX_DEPTH} open containers.
 */
export function renderWithin(value: Value, maxBytes: number): Result<string, RenderError> {
  const meter = new Meter(Math.min(byteLimit(maxBytes), MAX_RENDER_BYTES));
  const out: string[] = [];
  const stack: Rendering[] = [];
  /** Adds the piece `build` makes, of `bytes` UTF-8 bytes, if they fit: nothing is built before it is paid for. */
  const emit = (bytes: number, build: () => string): boolean => {
    const fits = meter.take(bytes);
    if (fits) out.push(build());
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
        const d = decimalOf(v);
        if (!d.ok) return { kind: "NotJsonRepresentable", what: d.error };
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
        stack.push({ object: true, members: [...entriesView(v)].sort((a, b) => compareCodePoints(a.key, b.key)), at: 0 });
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
      if (!ascii(comma)) return err(meter.refusal());
      const e = node(item);
      if (e !== undefined) return err(e);
    }
  }
  return ok(out.join(""));
}
