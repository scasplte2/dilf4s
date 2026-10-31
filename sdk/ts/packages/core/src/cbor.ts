/** The dilf4s deterministic CBOR profile (docs/specs/canonical-encoding.md section 2). */
import type { CanonicalError } from "./error.js";
import { err, ok, type Result } from "./result.js";
import { compareBytes, decodeUtf8 } from "./utf8.js";
import { ArrayValue, BoolValue, BytesValue, IntValue, MapValue, MAX_DEPTH, NullValue, RationalValue, TextValue, type Value } from "./value.js";

const TAG_POSITIVE_BIGNUM = 2n;
const TAG_NEGATIVE_BIGNUM = 3n;
const TAG_RATIONAL = 30n;
const TWO_TO_64 = 1n << 64n;

// ------------------------------------------------------------------------------------------------ encoding

/** Appends bytes to a growable buffer. */
class Sink {
  #buf = new Uint8Array(64);
  #len = 0;

  private reserve(n: number): void {
    if (this.#len + n <= this.#buf.length) return;
    const next = new Uint8Array(Math.max(this.#buf.length * 2, this.#len + n));
    next.set(this.#buf.subarray(0, this.#len));
    this.#buf = next;
  }

  byte(b: number): void {
    this.reserve(1);
    this.#buf[this.#len++] = b;
  }

  bytes(b: Uint8Array): void {
    this.reserve(b.length);
    this.#buf.set(b, this.#len);
    this.#len += b.length;
  }

  result(): Uint8Array {
    return this.#buf.slice(0, this.#len);
  }
}

/** The big-endian bytes of a non-negative integer, with no leading zeros (empty for zero). */
export function magnitudeBytes(n: bigint): Uint8Array {
  const out: number[] = [];
  for (let m = n; m > 0n; m >>= 8n) out.push(Number(m & 0xffn));
  return Uint8Array.from(out.reverse());
}

/** The shortest-form initial byte and argument (`0 <= argument < 2^64`). */
function head(out: Sink, major: number, argument: bigint): void {
  const mt = major << 5;
  const fixed = (len: number): void => {
    for (let i = len - 1; i >= 0; i--) out.byte(Number((argument >> BigInt(8 * i)) & 0xffn));
  };
  if (argument < 24n) out.byte(mt | Number(argument));
  else if (argument < 0x100n) {
    out.byte(mt | 0x18);
    fixed(1);
  } else if (argument < 0x10000n) {
    out.byte(mt | 0x19);
    fixed(2);
  } else if (argument < 0x100000000n) {
    out.byte(mt | 0x1a);
    fixed(4);
  } else {
    out.byte(mt | 0x1b);
    fixed(8);
  }
}

function integer(out: Sink, n: bigint): void {
  const [major, tag, magnitude] = n < 0n ? [1, TAG_NEGATIVE_BIGNUM, -1n - n] : [0, TAG_POSITIVE_BIGNUM, n];
  if (magnitude < TWO_TO_64) head(out, major, magnitude);
  else {
    const bytes = magnitudeBytes(magnitude);
    head(out, 6, tag);
    head(out, 2, BigInt(bytes.length));
    out.bytes(bytes);
  }
}

/** Recursion is bounded: a {@link Value} never nests deeper than {@link MAX_DEPTH}. */
function write(out: Sink, v: Value): void {
  switch (v.kind) {
    case "null":
      out.byte(0xf6);
      return;
    case "bool":
      out.byte(v.value ? 0xf5 : 0xf4);
      return;
    case "int":
      integer(out, v.value);
      return;
    case "rational":
      head(out, 6, TAG_RATIONAL);
      head(out, 4, 2n);
      integer(out, v.numerator);
      integer(out, v.denominator);
      return;
    case "text": {
      const utf8 = v.utf8();
      head(out, 3, BigInt(utf8.length));
      out.bytes(utf8);
      return;
    }
    case "bytes":
      head(out, 2, BigInt(v.length));
      out.bytes(v.bytes);
      return;
    case "array":
      head(out, 4, BigInt(v.items.length));
      for (const item of v.items) write(out, item);
      return;
    case "map":
      // entries are held in canonical key order
      head(out, 5, BigInt(v.size));
      for (const [key, value] of v.encodedEntries()) {
        head(out, 3, BigInt(key.length));
        out.bytes(key);
        write(out, value);
      }
      return;
  }
}

/** The canonical encoding of `value`. Total: every {@link Value} has exactly one. */
export function encode(value: Value): Uint8Array {
  const out = new Sink();
  write(out, value);
  return out.result();
}

// ------------------------------------------------------------------------------------------------ decoding

interface Head {
  readonly major: number;
  readonly argument: bigint;
  readonly end: number;
}

type Item =
  | { readonly open: false; readonly value: Value; readonly end: number }
  | { readonly open: true; readonly map: boolean; readonly count: number; readonly end: number };

interface PendingKey {
  readonly key: string;
  readonly start: number;
  readonly end: number;
}

type Frame =
  | { readonly map: false; remaining: number; readonly items: Value[] }
  | { readonly map: true; remaining: number; readonly entries: [string, Value][]; lastKey: Uint8Array | undefined; pending: PendingKey | undefined };

const nonCanonical = (offset: number, reason: string): CanonicalError => ({ kind: "NonCanonical", offset, reason });
const unsupported = (offset: number, reason: string): CanonicalError => ({ kind: "Unsupported", offset, reason });
const unexpectedEnd = (offset: number): CanonicalError => ({ kind: "UnexpectedEnd", offset });

/**
 * Strictly decodes one canonical value: any other encoding of a value, trailing bytes, or nesting deeper than
 * `maxDepth` (at most {@link MAX_DEPTH}) is an error. When it succeeds, `encode(decode(b)) == b`. The decoder keeps
 * its own frame stack, so input of any nesting is a typed error, never a stack overflow.
 */
export function decode(bytes: Uint8Array, maxDepth: number = MAX_DEPTH): Result<Value, CanonicalError> {
  return new Decoder(bytes, Math.min(maxDepth, MAX_DEPTH)).run();
}

class Decoder {
  readonly #stack: Frame[] = [];

  constructor(
    private readonly input: Uint8Array,
    private readonly maxDepth: number,
  ) {}

  run(): Result<Value, CanonicalError> {
    let pos = 0;
    for (;;) {
      const item = this.readItem(pos);
      if (!item.ok) return item;
      const it = item.value;
      let value: Value;
      if (!it.open) value = it.value;
      else if (this.#stack.length >= this.maxDepth) return err({ kind: "DepthExceeded", limit: this.maxDepth });
      else if (it.count === 0) {
        const empty = it.map ? MapValue.of([]) : ArrayValue.of([]);
        if (!empty.ok) return empty;
        value = empty.value;
      } else {
        this.#stack.push(
          it.map ? { map: true, remaining: it.count, entries: [], lastKey: undefined, pending: undefined } : { map: false, remaining: it.count, items: [] },
        );
        pos = it.end;
        continue;
      }
      const next = this.deliver(value, pos, it.end);
      if (!next.ok) return next;
      if (next.value.done) return ok(next.value.value);
      pos = next.value.pos;
    }
  }

  /** Hands a complete value (encoded at `start..end`) to the enclosing frame, closing every frame it completes. */
  private deliver(value: Value, start: number, end: number): Result<{ readonly done: true; readonly value: Value } | { readonly done: false; readonly pos: number }, CanonicalError> {
    let current = value;
    let from = start;
    for (;;) {
      const frame = this.#stack.at(-1);
      if (frame === undefined) return end === this.input.length ? ok({ done: true, value: current }) : err({ kind: "TrailingBytes", offset: end });
      if (!frame.map) {
        frame.items.push(current);
        frame.remaining--;
      } else if (frame.pending === undefined) {
        if (current.kind !== "text") return err(unsupported(from, "map keys must be text"));
        const keyBytes = this.input.subarray(from, end);
        if (frame.lastKey !== undefined && compareBytes(frame.lastKey, keyBytes) >= 0) {
          return err(nonCanonical(from, "map keys must be unique and in ascending encoded order"));
        }
        frame.pending = { key: current.value, start: from, end };
        return ok({ done: false, pos: end });
      } else {
        if (current.kind === "null") return err(nonCanonical(from, "map values must not be null (null and absent are the same value)"));
        frame.entries.push([frame.pending.key, current]);
        frame.lastKey = this.input.subarray(frame.pending.start, frame.pending.end);
        frame.pending = undefined;
        frame.remaining--;
      }
      if (frame.remaining > 0) return ok({ done: false, pos: end });
      this.#stack.pop();
      const closed = frame.map ? MapValue.of(frame.entries) : ArrayValue.of(frame.items);
      if (!closed.ok) return closed;
      current = closed.value;
      from = end;
    }
  }

  private readItem(pos: number): Result<Item, CanonicalError> {
    const h = this.readHead(pos);
    if (!h.ok) return h;
    const { major, argument, end } = h.value;
    const scalar = (value: Value, at: number): Result<Item, CanonicalError> => ok({ open: false, value, end: at });
    switch (major) {
      case 0:
        return scalar(IntValue.of(argument), end);
      case 1:
        return scalar(IntValue.of(-1n - argument), end);
      case 2: {
        const s = this.span(end, argument);
        return s.ok ? scalar(BytesValue.of(s.value.bytes), s.value.end) : s;
      }
      case 3: {
        const s = this.span(end, argument);
        if (!s.ok) return s;
        const text = decodeUtf8(s.value.bytes);
        if (text === undefined) return err({ kind: "InvalidUtf8", offset: end });
        const t = TextValue.of(text);
        return t.ok ? scalar(t.value, s.value.end) : t;
      }
      case 4:
      case 5: {
        const perEntry = major === 4 ? 1n : 2n;
        if (argument > BigInt(this.input.length - end) / perEntry) return err(unexpectedEnd(end));
        return ok({ open: true, map: major === 5, count: Number(argument), end });
      }
      case 6: {
        const t = this.readTagged(pos, argument, end);
        return t.ok ? scalar(t.value.value, t.value.end) : t;
      }
      default:
        switch (argument) {
          case 20n:
            return scalar(BoolValue.false, end);
          case 21n:
            return scalar(BoolValue.true, end);
          case 22n:
            return scalar(NullValue.instance, end);
          default:
            return err(unsupported(pos, `simple value ${argument.toString()}`));
        }
    }
  }

  private readHead(pos: number): Result<Head, CanonicalError> {
    const initial = this.input[pos];
    if (initial === undefined) return err(unexpectedEnd(pos));
    const major = initial >> 5;
    const info = initial & 0x1f;
    const sized = (len: number, minimum: bigint): Result<Head, CanonicalError> => {
      const s = this.span(pos + 1, BigInt(len));
      if (!s.ok) return s;
      let n = 0n;
      for (const b of s.value.bytes) n = (n << 8n) | BigInt(b);
      return n < minimum ? err(nonCanonical(pos, `argument ${n.toString()} not in shortest form`)) : ok({ major, argument: n, end: s.value.end });
    };
    if (info < 24) return ok({ major, argument: BigInt(info), end: pos + 1 });
    if (major === 7) return err(unsupported(pos, `floating point or simple value (additional information ${info.toString()})`));
    switch (info) {
      case 24:
        return sized(1, 24n);
      case 25:
        return sized(2, 0x100n);
      case 26:
        return sized(4, 0x10000n);
      case 27:
        return sized(8, 0x100000000n);
      case 31:
        return err(unsupported(pos, "indefinite length"));
      default:
        return err(unsupported(pos, `reserved additional information ${info.toString()}`));
    }
  }

  private readTagged(pos: number, tag: bigint, end: number): Result<{ readonly value: Value; readonly end: number }, CanonicalError> {
    if (tag === TAG_POSITIVE_BIGNUM || tag === TAG_NEGATIVE_BIGNUM) {
      const b = this.readBignum(pos, tag, end);
      return b.ok ? ok({ value: IntValue.of(b.value.value), end: b.value.end }) : b;
    }
    if (tag === TAG_RATIONAL) return this.readRational(pos, end);
    return err(unsupported(pos, `tag ${tag.toString()}`));
  }

  private readBignum(pos: number, tag: bigint, end: number): Result<{ readonly value: bigint; readonly end: number }, CanonicalError> {
    const h = this.readHead(end);
    if (!h.ok) return h;
    if (h.value.major !== 2) return err(nonCanonical(pos, "bignum tag must wrap a byte string"));
    const s = this.span(h.value.end, h.value.argument);
    if (!s.ok) return s;
    const b = s.value.bytes;
    if (b.length === 0 || b[0] === 0) return err(nonCanonical(pos, "bignum magnitude must be non-empty without leading zeros"));
    if (b.length <= 8) return err(nonCanonical(pos, "bignum used for a value that fits major type 0/1"));
    let magnitude = 0n;
    for (const x of b) magnitude = (magnitude << 8n) | BigInt(x);
    return ok({ value: tag === TAG_POSITIVE_BIGNUM ? magnitude : -1n - magnitude, end: s.value.end });
  }

  private readInteger(pos: number): Result<{ readonly value: bigint; readonly end: number }, CanonicalError> {
    const h = this.readHead(pos);
    if (!h.ok) return h;
    const { major, argument, end } = h.value;
    if (major === 0) return ok({ value: argument, end });
    if (major === 1) return ok({ value: -1n - argument, end });
    if (major === 6 && (argument === TAG_POSITIVE_BIGNUM || argument === TAG_NEGATIVE_BIGNUM)) return this.readBignum(pos, argument, end);
    return err(nonCanonical(pos, "expected an integer"));
  }

  private readRational(pos: number, end: number): Result<{ readonly value: Value; readonly end: number }, CanonicalError> {
    const pair = this.readHead(end);
    if (!pair.ok) return pair;
    if (pair.value.major !== 4 || pair.value.argument !== 2n) return err(nonCanonical(pos, "rational must wrap a 2-element array"));
    const num = this.readInteger(pair.value.end);
    if (!num.ok) return num;
    const den = this.readInteger(num.value.end);
    if (!den.ok) return den;
    if (den.value.value < 2n) return err(nonCanonical(pos, "rational denominator must be >= 2"));
    const r = RationalValue.of(num.value.value, den.value.value);
    return r === undefined ? err(nonCanonical(pos, "rational must be reduced")) : ok({ value: r, end: den.value.end });
  }

  /** `len` bytes from `pos`, and the offset after them. */
  private span(pos: number, len: bigint): Result<{ readonly bytes: Uint8Array; readonly end: number }, CanonicalError> {
    if (len > BigInt(this.input.length - pos)) return err(unexpectedEnd(pos));
    const end = pos + Number(len);
    return ok({ bytes: this.input.subarray(pos, end), end });
  }
}
