/**
 * Caller bytes, read through `%TypedArray%.prototype`'s own getters (internal to this package): what a caller passes as
 * a `Uint8Array` may be another realm's, a `Buffer`, or a subclass whose `length`, `byteOffset`, `buffer`, accessors or
 * iterator are its own, so each is read from the array's internal slots, never from its properties.
 */

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
 * Whether `view`'s memory is an `ArrayBuffer` of this realm, which only code running in this thread can change: not a
 * `SharedArrayBuffer` (another thread may write it at any time) nor another realm's buffer. It reads the buffer through
 * `%TypedArray%.prototype`'s own getter and compares its prototype, so nothing throws and no caller code runs. A
 * `SharedArrayBuffer` given `ArrayBuffer.prototype` passes: only the caller's own threads can race on it.
 */
export function isLocalBuffer(view: Uint8Array): boolean {
  const buffer: unknown = typedArrayBuffer === undefined ? undefined : typedArrayBuffer.call(view);
  return typeof buffer === "object" && buffer !== null && Object.getPrototypeOf(buffer) === ArrayBuffer.prototype;
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

/** A fresh copy of one of this package's own plain arrays (never a caller's: see {@link takeBytes}). */
export function copyBytes(bytes: Uint8Array): Uint8Array {
  return new Uint8Array(bytes);
}
