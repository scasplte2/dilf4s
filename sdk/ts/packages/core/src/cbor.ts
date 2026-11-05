/** The dilf4s deterministic CBOR profile (docs/specs/canonical-encoding.md section 2). */
import { byteLimit, checkedLimit, Meter, tooLarge } from "./budget.js";
import { isLocalBuffer, plainBytes } from "./bytes.js";
import type { CanonicalError, TooLarge, TypeMismatch } from "./error.js";
import { toHex } from "./hex.js";
import { err, ok, type Result } from "./result.js";
import { compareBytes, utf8Text } from "./utf8.js";
import {
  arrayOfValues,
  BoolValue,
  bytesOfValues,
  bytesView,
  entriesView,
  IntValue,
  mapOfValues,
  MAX_DEPTH,
  MAX_INTEGER_BITS,
  MAX_RATIONAL_BITS,
  NullValue,
  RationalValue,
  TextValue,
  utf8View,
  type Entry,
  type Value,
} from "./value.js";

export { MAX_INTEGER_BITS, MAX_RATIONAL_BITS } from "./value.js";

/**
 * How a decoder builds a bignum's magnitude from its bytes, once their form and width are checked: one conversion from
 * hexadecimal, linear in the length (shifting in a byte at a time would be quadratic). A test counts its calls.
 */
export const bignums = {
  magnitude: (b: Uint8Array): bigint => BigInt(`0x${toHex(b)}`),
};

/**
 * The Java `bitLength` of a bignum's integer from its magnitude's bytes, `b[0]` non-zero: 8 bits per byte after the
 * first, and the first byte's own. `m` and `-1 - m` have the same `bitLength`, so it holds for either tag.
 */
const magnitudeBits = (b: Uint8Array): number => 8 * (b.length - 1) + 32 - Math.clz32(b[0] ?? 0);

const TAG_POSITIVE_BIGNUM = 2n;
const TAG_NEGATIVE_BIGNUM = 3n;
const TAG_RATIONAL = 30n;

// ------------------------------------------------------------------------------------------------ encoding

/** Where the bytes of a walk that keeps them go. */
interface Target {
  byte(b: number): void;
  bytes(b: Uint8Array): void;
}

/** A growable buffer that never grows past `cap` bytes. */
class Growing implements Target {
  #buf: Uint8Array;
  #len = 0;

  constructor(private readonly cap: number) {
    this.#buf = new Uint8Array(Math.min(64, cap));
  }

  private reserve(n: number): void {
    if (this.#len + n <= this.#buf.length) return;
    const next = new Uint8Array(Math.min(Math.max(this.#buf.length * 2, this.#len + n), this.cap));
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
    return this.#len === this.#buf.length ? this.#buf : this.#buf.slice(0, this.#len);
  }
}

/** Where encoded bytes go: room for `n` more is asked before they are produced, and a refusal stops the encoding. */
interface Out {
  room(n: number): boolean;
  byte(b: number): void;
  bytes(b: Uint8Array): void;
}

/** An incremental hash, as far as an encoding streamed into it needs one. */
export interface Absorber {
  update(data: Uint8Array): unknown;
}

/**
 * The bytes the walk stages before handing them to a hash in one `update`: a fixed size, whatever the value's. Heads
 * and an integer's bytes come one at a time and are gathered here; a text or byte string is copied through, so a hash
 * (whose methods any caller can replace on its prototype) is only ever handed this staging array, never a value's own.
 * Besides it, the walk holds one thing proportional to the value: a bignum's hexadecimal digits, while it writes that
 * one integer (`integer`; JavaScript offers no other linear way to read a bigint's bytes), which the value holds whole.
 */
export const STAGED_BYTES = 1024;

/**
 * Feeds an {@link Absorber} as the walk writes: it holds at most {@link STAGED_BYTES} of the encoding, staged, and
 * nothing else of it (the hash itself buffers at most a block). `peak` is the most it ever staged.
 */
export class Feeding implements Target {
  readonly #staged = new Uint8Array(STAGED_BYTES);
  #len = 0;
  #peak = 0;

  constructor(private readonly into: Absorber) {}

  get peak(): number {
    return this.#peak;
  }

  byte(b: number): void {
    if (this.#len === STAGED_BYTES) this.flush();
    this.#staged[this.#len++] = b;
    if (this.#len > this.#peak) this.#peak = this.#len;
  }

  bytes(b: Uint8Array): void {
    for (let at = 0; at < b.length; ) {
      if (this.#len === STAGED_BYTES) this.flush();
      const n = Math.min(b.length - at, STAGED_BYTES - this.#len);
      this.#staged.set(b.subarray(at, at + n), this.#len);
      this.#len += n;
      at += n;
      if (this.#len > this.#peak) this.#peak = this.#len;
    }
  }

  /** Hands the staged bytes over; call once the walk is done, before taking the digest. */
  flush(): void {
    if (this.#len > 0) this.into.update(this.#staged.subarray(0, this.#len));
    this.#len = 0;
  }
}

/** Bytes counted against a limit, and kept only when there is somewhere to keep them (never more than the limit). */
class Budgeted implements Out {
  constructor(
    readonly meter: Meter,
    private readonly out: Target | undefined,
  ) {}
  room(n: number): boolean {
    return this.meter.take(n);
  }
  byte(b: number): void {
    this.out?.byte(b);
  }
  bytes(b: Uint8Array): void {
    this.out?.bytes(b);
  }
}

/** The value of a lowercase hexadecimal digit's character code. */
const nibble = (c: number): number => (c <= 0x39 ? c - 0x30 : c - 0x57);

/** Writes the `length` big-endian bytes of the hexadecimal `hex` (at most `2 * length` digits), linear in `length`. */
function hexBytes(write: (b: number, i: number) => void, hex: string, length: number): void {
  const digits = hex.padStart(2 * length, "0");
  for (let i = 0; i < length; i++) write((nibble(digits.charCodeAt(2 * i)) << 4) | nibble(digits.charCodeAt(2 * i + 1)), i);
}

/**
 * The big-endian bytes of a non-negative integer, with no leading zeros (empty for zero), linear in its size. A value
 * that is not a `bigint`, or a negative one, is a `TypeMismatch` (a negative one had been read as zero).
 */
export function magnitudeBytes(n: bigint): Result<Uint8Array, TypeMismatch> {
  if (typeof (n as unknown) !== "bigint" || n < 0n) return err({ kind: "TypeMismatch", reason: "expected a non-negative bigint" });
  if (n === 0n) return ok(new Uint8Array(0));
  const hex = n.toString(16);
  const out = new Uint8Array(Math.ceil(hex.length / 2));
  hexBytes(
    (b, i) => {
      out[i] = b;
    },
    hex,
    out.length,
  );
  return ok(out);
}

/** The additional information of an argument `size` bytes long (1, 2, 4 or 8). */
const argumentInfo = (size: number): number => (size === 1 ? 0x18 : size === 2 ? 0x19 : size === 4 ? 0x1a : 0x1b);

/** The shortest-form initial byte and argument (`0 <= argument < 2^64`). */
function head(out: Out, major: number, argument: bigint): boolean {
  const mt = major << 5;
  const size = argument < 24n ? 0 : argument < 0x100n ? 1 : argument < 0x10000n ? 2 : argument < 0x100000000n ? 4 : 8;
  if (!out.room(1 + size)) return false;
  out.byte(size === 0 ? mt | Number(argument) : mt | argumentInfo(size));
  for (let i = size - 1; i >= 0; i--) out.byte(Number((argument >> BigInt(8 * i)) & 0xffn));
  return true;
}

/**
 * An integer of `bits` (Java) bits: CBOR carries `n` for `n >= 0` and `-1 - n` for `n < 0`, whose bit length is
 * `bits`, so a bignum's length is known, and checked against the budget, before any of its bytes is produced. They are
 * then streamed from its hexadecimal form, linear in its size.
 */
function integer(out: Out, n: bigint, bits: number): boolean {
  const negative = n < 0n;
  const magnitude = negative ? -1n - n : n;
  if (bits <= 64) return head(out, negative ? 1 : 0, magnitude);
  const length = Math.ceil(bits / 8);
  if (!head(out, 6, negative ? TAG_NEGATIVE_BIGNUM : TAG_POSITIVE_BIGNUM) || !head(out, 2, BigInt(length)) || !out.room(length)) return false;
  hexBytes((b) => {
    out.byte(b);
  }, magnitude.toString(16), length);
  return true;
}

/** A byte or text string: its head, then its bytes. */
function span(out: Out, major: number, bytes: Uint8Array): boolean {
  if (!head(out, major, BigInt(bytes.length)) || !out.room(bytes.length)) return false;
  out.bytes(bytes);
  return true;
}

function single(out: Out, b: number): boolean {
  if (!out.room(1)) return false;
  out.byte(b);
  return true;
}

/** An array or map being written: its parts, and the next one. */
type Open = { readonly map: false; readonly items: readonly Value[]; at: number } | { readonly map: true; readonly entries: readonly Entry[]; at: number };

/** Writes `v`'s first bytes: all of a scalar, or a container's head (its parts are then open on `stack`). */
function node(out: Out, v: Value, stack: Open[]): boolean {
  switch (v.kind) {
    case "null":
      return single(out, 0xf6);
    case "bool":
      return single(out, v.value ? 0xf5 : 0xf4);
    case "int":
      return integer(out, v.value, v.bitLength);
    case "rational":
      return head(out, 6, TAG_RATIONAL) && head(out, 4, 2n) && integer(out, v.numerator, v.numeratorBitLength) && integer(out, v.denominator, v.denominatorBitLength);
    case "text":
      return span(out, 3, utf8View(v));
    case "bytes":
      return span(out, 2, bytesView(v));
    case "array":
      if (!head(out, 4, BigInt(v.length))) return false;
      stack.push({ map: false, items: v.items, at: 0 });
      return true;
    case "map":
      // entries are held in canonical key order
      if (!head(out, 5, BigInt(v.size))) return false;
      stack.push({ map: true, entries: entriesView(v), at: 0 });
      return true;
  }
}

/**
 * Writes `value`, or stops at the first refusal. Never recursive: an explicit stack holds one open container per level
 * (at most {@link MAX_DEPTH}). Every node writes its first byte before its parts, which the budget relies on.
 */
function write(out: Out, value: Value): boolean {
  const stack: Open[] = [];
  if (!node(out, value, stack)) return false;
  for (let top = stack.at(-1); top !== undefined; top = stack.at(-1)) {
    const i = top.at++;
    if (top.map) {
      const e = top.entries[i];
      if (e === undefined) stack.pop();
      else if (!span(out, 3, e.utf8) || !node(out, e.value, stack)) return false;
    } else {
      const item = top.items[i];
      if (item === undefined) stack.pop();
      else if (!node(out, item, stack)) return false;
    }
  }
  return true;
}

/**
 * The longest encoding {@link encode} produces: 2^31 - 1 bytes, the largest array the JVM reference can build, and
 * within every engine's typed-array limit. A longer encoding is a typed refusal, never an allocation failure (below it,
 * memory may still run out: that is the host's, as for any allocation).
 */
export const MAX_ENCODED_BYTES = 2 ** 31 - 1;

/**
 * Why a value was not encoded: its encoding is longer than the limit (`TooLarge`), or an argument is not of its type
 * (`TypeMismatch`: a value that is not a canonical value, or a limit that is not a `number`), which is checked in O(1)
 * before anything is read.
 */
export type EncodeError = TooLarge | TypeMismatch;

/**
 * The canonical encoding of `value`: {@link encodeWithin} at {@link MAX_ENCODED_BYTES}. Every {@link Value} has exactly
 * one, but a value built with shared parts (`[x, x]` nested `k` deep weighs about `2^k`) can encode to far more than it
 * occupies: that is refused as `TooLarge`, from the weight alone when it is over. O(min(weight, the limit)): encode a
 * caller's value within a limit of the caller's own. What is not a canonical value is a `TypeMismatch`.
 */
export function encode(value: Value): Result<Uint8Array, EncodeError> {
  return encodeWithin(value, MAX_ENCODED_BYTES);
}

/**
 * The walk of {@link encodeWithin}, {@link encodedLengthWithin} and {@link encodeIntoWithin}: one walk and one budget,
 * keeping the bytes, only counting them, or feeding them on. It returns the length.
 */
function within(value: Value, limit: number, target: Target | undefined): Result<number, TooLarge> {
  if (value.weight > BigInt(limit)) return err(tooLarge(limit));
  const out = new Budgeted(new Meter(limit), target);
  return write(out, value) ? ok(out.meter.used) : err(out.meter.refusal());
}

/**
 * The canonical encoding of `value`, if it is at most `maxBytes` long (see `byteLimit` for odd limits); it never holds
 * more than `maxBytes`.
 *
 * O(min(weight, `maxBytes`)), and no allocation beyond the output: every encoding is at least as long as its value's
 * weight (each node's head is a byte, text and byte strings add their length, a number of `l` whole limbs takes at
 * least `l` bytes more), so a weight over the limit is refused at once; each node visited writes a byte before its
 * parts, so the walk stops within `maxBytes + 1` nodes; and a string's or number's length is checked against the
 * budget before its bytes are produced. A value that is not a canonical value (see `isValue`), or a limit that is not a
 * `number`, is a `TypeMismatch`, checked in O(1) before anything is allocated.
 */
export function encodeWithin(value: Value, maxBytes: number): Result<Uint8Array, EncodeError> {
  const limit = checkedLimit(value, maxBytes);
  if (!limit.ok) return limit;
  const kept = new Growing(limit.value);
  const w = within(value, limit.value, kept);
  return w.ok ? ok(kept.result()) : w;
}

/**
 * The length of `value`'s canonical encoding, if it is at most `maxBytes`, counted without encoding it, with the bounds
 * and refusals of {@link encodeWithin}.
 */
export function encodedLengthWithin(value: Value, maxBytes: number): Result<number, EncodeError> {
  const limit = checkedLimit(value, maxBytes);
  return limit.ok ? within(value, limit.value, undefined) : limit;
}

/**
 * Streams `value`'s canonical encoding into `feeding` (and so into its hash) as {@link encodeWithin} would write it,
 * with the same budget and the same refusal at the same point, without building it: the same walk, feeding a hash
 * instead of a buffer. Returns the length. On a refusal, the hash has absorbed a prefix of the encoding: discard it.
 * Its arguments are not checked: its callers check them first (`checkedLimit`).
 */
export function encodeIntoWithin(feeding: Feeding, value: Value, maxBytes: number): Result<number, TooLarge> {
  const w = within(value, byteLimit(maxBytes), feeding);
  feeding.flush();
  return w;
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
 * its own frame stack, so input of any nesting is a typed error, never a stack overflow. `bytes` that is not a
 * `Uint8Array`, or a `maxDepth` that is not a number, is a `TypeMismatch`.
 *
 * The bytes are read through a plain view of their own memory (`plainBytes`): a subclass's `length`, accessors or
 * iterator are never consulted. They are read in place when that memory is an `ArrayBuffer` of this realm, which
 * nothing can change while the decoder runs (it runs no caller code); a `SharedArrayBuffer`, which another thread may
 * write at any time, or another realm's buffer is copied once first. Each byte string decoded is copied once into its
 * value, which then holds no view of the input.
 */
export function decode(bytes: Uint8Array, maxDepth: number = MAX_DEPTH): Result<Value, CanonicalError> {
  if (typeof (maxDepth as unknown) !== "number" || Number.isNaN(maxDepth)) return err({ kind: "TypeMismatch", reason: "the depth limit is not a number" });
  const view = plainBytes(bytes);
  if (view === undefined) return err({ kind: "TypeMismatch", reason: "expected a Uint8Array" });
  // read in place unless another thread, or another realm, may hold the memory: then one copy, read alone
  const input = isLocalBuffer(view) ? view : new Uint8Array(view);
  return new Decoder(input, Math.min(maxDepth, MAX_DEPTH)).run();
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
        const empty = it.map ? mapOfValues([]) : arrayOfValues([]);
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
      const closed = frame.map ? mapOfValues(frame.entries) : arrayOfValues(frame.items);
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
      case 1: {
        const n = IntValue.of(major === 0 ? argument : -1n - argument);
        return n.ok ? scalar(n.value, end) : n;
      }
      case 2: {
        const s = this.span(end, argument);
        if (!s.ok) return s;
        // the one copy each byte string needs: its value holds no view of the input
        return scalar(bytesOfValues(s.value.bytes), s.value.end);
      }
      case 3: {
        const s = this.span(end, argument);
        if (!s.ok) return s;
        const text = utf8Text(s.value.bytes);
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
      const b = this.readBignum(pos, tag, end, MAX_INTEGER_BITS, { kind: "IntegerTooWide", limit: MAX_INTEGER_BITS });
      if (!b.ok) return b;
      const n = IntValue.of(b.value.value);
      return n.ok ? ok({ value: n.value, end: b.value.end }) : n;
    }
    if (tag === TAG_RATIONAL) return this.readRational(pos, end);
    return err(unsupported(pos, `tag ${tag.toString()}`));
  }

  /**
   * A bignum's integer, in order: a byte string (`NonCanonical`), present in full (`UnexpectedEnd`), in canonical form
   * (`NonCanonical`), and at most `limit` wide (`tooWide`), all before the integer is built.
   */
  private readBignum(
    pos: number,
    tag: bigint,
    end: number,
    limit: number,
    tooWide: CanonicalError,
  ): Result<{ readonly value: bigint; readonly end: number }, CanonicalError> {
    const h = this.readHead(end);
    if (!h.ok) return h;
    if (h.value.major !== 2) return err(nonCanonical(pos, "bignum tag must wrap a byte string"));
    const s = this.span(h.value.end, h.value.argument);
    if (!s.ok) return s;
    const b = s.value.bytes;
    if (b.length === 0 || b[0] === 0) return err(nonCanonical(pos, "bignum magnitude must be non-empty without leading zeros"));
    if (b.length <= 8) return err(nonCanonical(pos, "bignum used for a value that fits major type 0/1"));
    if (magnitudeBits(b) > limit) return err(tooWide);
    const magnitude = bignums.magnitude(b);
    return ok({ value: tag === TAG_POSITIVE_BIGNUM ? magnitude : -1n - magnitude, end: s.value.end });
  }


  /**
   * In order: the pair, the numerator and its width, the denominator and its width, the denominator's range, and last
   * the one gcd that checks lowest terms (docs/specs/conformance.md section 5).
   */
  private readRational(pos: number, end: number): Result<{ readonly value: Value; readonly end: number }, CanonicalError> {
    const pair = this.readHead(end);
    if (!pair.ok) return pair;
    if (pair.value.major !== 4 || pair.value.argument !== 2n) return err(nonCanonical(pos, "rational must wrap a 2-element array"));
    const num = this.readPart(pair.value.end);
    if (!num.ok) return num;
    const den = this.readPart(num.value.end);
    if (!den.ok) return den;
    if (den.value.value < 2n) return err(nonCanonical(pos, "rational denominator must be >= 2"));
    const r = RationalValue.of(num.value.value, den.value.value);
    return r === undefined ? err(nonCanonical(pos, "rational must be reduced")) : ok({ value: r, end: den.value.end });
  }

  /**
   * A rational's numerator or denominator, refused when wider than {@link MAX_RATIONAL_BITS}: a bignum's width is
   * checked from its length, before it is built and before any gcd, so a wide part costs no more than reading it.
   */
  private readPart(pos: number): Result<{ readonly value: bigint; readonly end: number }, CanonicalError> {
    const h = this.readHead(pos);
    if (!h.ok) return h;
    const { major, argument, end } = h.value;
    if (major === 0) return ok({ value: argument, end });
    if (major === 1) return ok({ value: -1n - argument, end });
    if (major === 6 && (argument === TAG_POSITIVE_BIGNUM || argument === TAG_NEGATIVE_BIGNUM))
      return this.readBignum(pos, argument, end, MAX_RATIONAL_BITS, { kind: "RationalTooWide", limit: MAX_RATIONAL_BITS });
    return err(nonCanonical(pos, "expected an integer"));
  }

  /** `len` bytes from `pos`, and the offset after them. */
  private span(pos: number, len: bigint): Result<{ readonly bytes: Uint8Array; readonly end: number }, CanonicalError> {
    if (len > BigInt(this.input.length - pos)) return err(unexpectedEnd(pos));
    const end = pos + Number(len);
    return ok({ bytes: this.input.subarray(pos, end), end });
  }
}
