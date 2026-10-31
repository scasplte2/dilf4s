/**
 * The JSON mapping (docs/specs/canonical-encoding.md section 3).
 *
 * {@link parse} is a strict RFC 8259 parser that reads numbers exactly from their source text (never through a
 * double, unlike `JSON.parse`), rejects duplicate member names and unpaired surrogates, and keeps its own frame stack,
 * so nesting is a typed error rather than a stack overflow. {@link render} is the display view, and fails for values
 * JSON cannot represent.
 */
import type { CanonicalError } from "./error.js";
import { err, ok, type Result } from "./result.js";
import { compareCodePoints } from "./utf8.js";
import { ArrayValue, BoolValue, MapValue, MAX_DEPTH, NullValue, RationalValue, TextValue, type Value } from "./value.js";

/** The largest accepted `|fraction digits - exponent|` of a JSON number, bounding the `10^scale` it materialises. */
export const DEFAULT_MAX_SCALE = 10000;

type Frame = { readonly object: false; readonly items: Value[] } | { readonly object: true; readonly entries: [string, Value][]; readonly seen: Set<string>; key: string };

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
          if (c === "{" || c === "[") {
            const q = this.skipWs(p + 1);
            const object = c === "{";
            if (this.s[q] === (object ? "}" : "]")) {
              const empty = object ? MapValue.of([]) : ArrayValue.of([]);
              if (!empty.ok) return empty;
              state = { step: "deliver", value: empty.value };
              pos = q + 1;
            } else if (stack.length >= MAX_DEPTH) {
              return err({ kind: "DepthExceeded", limit: MAX_DEPTH });
            } else {
              stack.push(object ? { object: true, entries: [], seen: new Set(), key: "" } : { object: false, items: [] });
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
          if (frame.seen.has(k.value.text)) return err({ kind: "DuplicateKey", key: k.value.text });
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

  /** `-? (0 | [1-9][0-9]*) (. [0-9]+)? ([eE] [+-]? [0-9]+)?`, converted exactly; the scale is bounded before `10^scale`. */
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
    const source = s.slice(p, end);
    const intDigits = s.slice(intStart, intEnd);
    const fracDigits = hasFrac ? s.slice(intEnd + 1, fracEnd) : "";
    const expDigits = hasExp ? s.slice(expStart, end) : "0";
    const malformed =
      intDigits.length === 0 || (intDigits.length > 1 && intDigits.startsWith("0")) || (hasFrac && fracDigits.length === 0) || (hasExp && expDigits.length === 0);
    if (malformed) return err({ kind: "InvalidNumber", source });
    const exponent = BigInt(expDigits) * (hasExp && expSign === "-" ? -1n : 1n);
    const scale = BigInt(fracDigits.length) - exponent;
    if ((scale < 0n ? -scale : scale) > BigInt(this.maxScale)) return err({ kind: "InvalidNumber", source, scale });
    const unscaled = BigInt(intDigits + fracDigits) * (negative ? -1n : 1n);
    const value = scale >= 0n ? RationalValue.number(unscaled, 10n ** scale) : RationalValue.number(unscaled * 10n ** -scale, 1n);
    return value === undefined ? err({ kind: "InvalidNumber", source }) : ok({ value, end });
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

/** The exact decimal of a rational, when its denominator has no prime factors but 2 and 5. */
export function decimal(r: RationalValue): string | undefined {
  let rest = r.denominator;
  let twos = 0;
  let fives = 0;
  while (rest % 2n === 0n) {
    rest /= 2n;
    twos++;
  }
  while (rest % 5n === 0n) {
    rest /= 5n;
    fives++;
  }
  if (rest !== 1n) return undefined;
  const scale = Math.max(twos, fives);
  const scaled = r.numerator * (10n ** BigInt(scale) / r.denominator);
  const digits = (scaled < 0n ? -scaled : scaled).toString().padStart(scale + 1, "0");
  return `${scaled < 0n ? "-" : ""}${digits.slice(0, digits.length - scale)}.${digits.slice(digits.length - scale)}`;
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

/** Compact JSON with object members in code-point order of their names. Fails for bytes and non-terminating rationals. */
export function render(value: Value): Result<string, CanonicalError> {
  switch (value.kind) {
    case "null":
      return ok("null");
    case "bool":
      return ok(value.value ? "true" : "false");
    case "int":
      return ok(value.value.toString());
    case "rational": {
      const d = decimal(value);
      return d === undefined ? err({ kind: "NotJsonRepresentable", what: "non-terminating rational" }) : ok(d);
    }
    case "text":
      return ok(quote(value.value));
    case "bytes":
      return err({ kind: "NotJsonRepresentable", what: "bytes" });
    case "array": {
      const parts: string[] = [];
      for (const item of value.items) {
        const r = render(item);
        if (!r.ok) return r;
        parts.push(r.value);
      }
      return ok(`[${parts.join(",")}]`);
    }
    case "map": {
      const parts: string[] = [];
      const sorted = [...value.entries()].sort(([a], [b]) => compareCodePoints(a, b));
      for (const [k, v] of sorted) {
        const r = render(v);
        if (!r.ok) return r;
        parts.push(`${quote(k)}:${r.value}`);
      }
      return ok(`{${parts.join(",")}}`);
    }
  }
}
