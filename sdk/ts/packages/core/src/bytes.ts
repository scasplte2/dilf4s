/**
 * Caller bytes, read through `%TypedArray%.prototype`'s own getters (internal to this package): what a caller passes as
 * a `Uint8Array` may be another realm's, a `Buffer`, or a subclass whose `length`, `byteOffset`, `buffer`, accessors or
 * iterator are its own, so each is read from the array's internal slots, never from its properties.
 */

import { isArrayBufferBranded } from "./guard.js";

/** `%TypedArray%.prototype`, which every typed array's prototype inherits from. */
const typedArrayPrototype: object = Object.getPrototypeOf(Uint8Array.prototype) as object;

/** `%TypedArray%.prototype`'s own getter for `name`: it reads the array's internal slots, which nothing overrides. */
const typedArrayGetter = (name: PropertyKey): ((this: unknown) => unknown) | undefined =>
  Reflect.get(Object.getOwnPropertyDescriptor(typedArrayPrototype, name) ?? {}, "get") as ((this: unknown) => unknown) | undefined;

/** `Uint8Array.prototype`'s `Symbol.toStringTag` getter, from `%TypedArray%`: it reads an array's internal type name. */
const typedArrayName = typedArrayGetter(Symbol.toStringTag);
const typedArrayBuffer = typedArrayGetter("buffer");
const typedArrayOffset = typedArrayGetter("byteOffset");
const typedArrayLength = typedArrayGetter("length");

/**
 * Whether `x` is a `Uint8Array` (a `Buffer` included), from any realm, in O(1). It reads the typed array's internal
 * name through `%TypedArray%`'s own getter, which nothing can fake: unlike `instanceof`, it accepts another realm's
 * array and runs no Proxy trap, and unlike `Object.prototype.toString` or `constructor.name`, an object claiming the
 * name (by `Symbol.toStringTag`, or a renamed constructor) is not one.
 */
export function isBytes(x: unknown): x is Uint8Array {
  return typedArrayName === undefined ? x instanceof Uint8Array : typedArrayName.call(x) === "Uint8Array";
}

/**
 * `x` as a plain `Uint8Array` over the same memory, if it is bytes (see {@link isBytes}), in O(1). Its buffer, offset
 * and length are read through `%TypedArray%.prototype`'s own getters, from the array's internal slots: a subclass, or an
 * own property, that overrides `length`, `byteOffset` or `buffer` is not believed, and the view has no such overrides
 * (nor a `Buffer`'s methods). A detached or out-of-bounds array has length 0, and is read as empty.
 */
export function plainBytes(x: unknown): Uint8Array | undefined {
  if (!isBytes(x)) return undefined;
  if (typedArrayBuffer === undefined || typedArrayOffset === undefined || typedArrayLength === undefined) return x;
  const length = typedArrayLength.call(x) as number;
  return length === 0 ? new Uint8Array(0) : new Uint8Array(typedArrayBuffer.call(x) as ArrayBufferLike, typedArrayOffset.call(x) as number, length);
}

/**
 * `x`'s internal length, if it is bytes (see {@link isBytes}), in O(1) and with nothing allocated: read through
 * `%TypedArray%.prototype`'s own getter, never its `length` property. Code that reads only `x`'s indices up to this
 * length can use `x` itself without a view: a typed array's indexed reads go to its memory, and no subclass, own
 * property or prototype can change them (a detached or out-of-bounds array has length 0).
 */
export function bytesLength(x: unknown): number | undefined {
  if (!isBytes(x)) return undefined;
  const length: unknown = typedArrayLength?.call(x);
  return typeof length === "number" ? length : new Uint8Array(x).length;
}

/**
 * Whether `view`'s memory is an `ArrayBuffer` of this realm, which only code running in this thread can change: not a
 * `SharedArrayBuffer` (another thread may write it at any time) nor another realm's buffer. It reads the buffer through
 * `%TypedArray%.prototype`'s own getter, compares its prototype, and asks the engine's own `ArrayBuffer` brand check
 * (core's guard), so a `SharedArrayBuffer` given `ArrayBuffer.prototype` is shared memory still. Nothing throws and no
 * caller code runs.
 */
export function isLocalBuffer(view: Uint8Array): boolean {
  const buffer: unknown = typedArrayBuffer === undefined ? undefined : typedArrayBuffer.call(view);
  return typeof buffer === "object" && buffer !== null && Object.getPrototypeOf(buffer) === ArrayBuffer.prototype && isArrayBufferBranded(buffer);
}

/**
 * A fresh plain copy of caller bytes, or `undefined` if they are not bytes (see {@link isBytes}). The `Uint8Array`
 * constructor, given a typed array, copies from its internal slots in one block: a subclass's `length`, accessors or
 * iterator are never consulted. A detached or out-of-bounds array, which that constructor throws on, has length 0 (read
 * through `%TypedArray%.prototype`'s own getter) and is copied as empty. `slice` would not do: on a Node `Buffer` it
 * returns a view sharing memory.
 */
export function takeBytes(x: unknown): Uint8Array | undefined {
  if (!isBytes(x)) return undefined;
  return typedArrayLength?.call(x) === 0 ? new Uint8Array(0) : new Uint8Array(x);
}

/** `ArrayBuffer.prototype`'s own `resizable` getter (ES2024), read once when this module loads; absent in older engines. */
const arrayBufferResizable: unknown = Reflect.get(Object.getOwnPropertyDescriptor(ArrayBuffer.prototype, "resizable") ?? {}, "get");
const NO_ARGUMENTS: readonly unknown[] = Object.freeze([]);

/**
 * Whether `view`'s memory nothing but this thread's own code can change, nor resize: an `ArrayBuffer` of this realm
 * ({@link isLocalBuffer}: the engine's brand check, never a prototype, so no `SharedArrayBuffer`) that is not
 * resizable. A view of it keeps its bytes and its length while the SDK runs without calling caller code.
 */
export function isStableBuffer(view: Uint8Array): boolean {
  const buffer: unknown = typedArrayBuffer?.call(view);
  if (typeof buffer !== "object" || buffer === null || Object.getPrototypeOf(buffer) !== ArrayBuffer.prototype) return false;
  // a buffer's brand and resizability are fixed when it is made: one found stable once is stable for good, so its
  // later arguments take one WeakSet lookup instead of the brand checks
  if (STABLE.has(buffer)) return true;
  if (!isArrayBufferBranded(buffer)) return false;
  // an ArrayBuffer (brand-checked above), for which the getter only reads an internal slot
  const stable = typeof arrayBufferResizable !== "function" || Reflect.apply(arrayBufferResizable, buffer, NO_ARGUMENTS) === false;
  if (stable) STABLE.add(buffer);
  return stable;
}

/** The buffers {@link isStableBuffer} has found stable: held weakly, never keeping one alive. */
const STABLE = new WeakSet();

/** The bound of a caller byte argument with no length limit of its own (a message, data to hash). */
export const UNBOUNDED: number = Number.MAX_SAFE_INTEGER;

/**
 * **The boundary** through which public code turns a caller's byte argument into bytes it reads
 * (docs/specs/canonical-encoding.md section 1, "Caller bytes"): `x` measured by its internal length (one fixed-length
 * view of its memory, never its own `length`), refused past `max` without anything copied, and then
 * - the view itself when its memory is stable ({@link isStableBuffer}: this realm's own, unshared, non-resizable
 *   `ArrayBuffer`), which nothing changes while the SDK runs without calling caller code: nothing copied;
 * - else one copy of that same view (shared, resizable or another realm's memory), so every later read sees one
 *   snapshot.
 * The result is `undefined` for what is not a `Uint8Array`, the length (a number) for one longer than `max`, and else
 * the bytes, which internals read from then on, never the caller's object. A caller whose code may run while the bytes
 * are still in use (a getter read later, a function called) takes {@link ownCopy} instead.
 */
export function ownBytes(x: unknown, max: number): Uint8Array | number | undefined {
  const view = plainBytes(x);
  if (view === undefined) return undefined;
  if (view.length > max) return view.length;
  // an empty view is a fresh array of its own (plainBytes makes one): nothing in it can change
  return view.length === 0 || isStableBuffer(view) ? view : copyBytes(view);
}

/**
 * {@link ownBytes} for bytes kept, or still in use while caller code runs (a later getter or trap, a caller's
 * function): always one copy of the one view measured, so nothing the caller does afterwards changes them.
 */
export function ownCopy(x: unknown, max: number): Uint8Array | number | undefined {
  const view = plainBytes(x);
  if (view === undefined) return undefined;
  return view.length > max ? view.length : copyBytes(view);
}

/** {@link ownBytes} with no bound: the bytes, or `undefined` for what is not a `Uint8Array`. */
export function ownAny(x: unknown): Uint8Array | undefined {
  const b = ownBytes(x, UNBOUNDED);
  return typeof b === "number" ? undefined : b;
}

/** {@link ownCopy} with no bound: a copy, or `undefined` for what is not a `Uint8Array`. */
export function ownCopyAny(x: unknown): Uint8Array | undefined {
  const b = ownCopy(x, UNBOUNDED);
  return typeof b === "number" ? undefined : b;
}

/** A fresh copy of one of this package's own plain arrays (never a caller's: see {@link takeBytes}). */
export function copyBytes(bytes: Uint8Array): Uint8Array {
  return new Uint8Array(bytes);
}
