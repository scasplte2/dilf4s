/** Caller text as a diagnostic may show it (docs/specs/conformance.md section 8). */

/** The most code points a {@link Preview} keeps. */
export const MAX_PREVIEW_CODE_POINTS = 32;

const isSurrogate = (c: number): boolean => c >= 0xd800 && c <= 0xdfff;
const width = (c: number): number => (c < 0x80 ? 1 : c < 0x800 ? 2 : c < 0x10000 ? 3 : 4);

// JSON's escapes for a head of at most MAX_PREVIEW_CODE_POINTS code points
const escaped = (s: string): string =>
  Array.from(s, (c) => {
    const u = c.charCodeAt(0);
    return c === '"' || c === "\\" ? `\\${c}` : u < 0x20 || u === 0x7f ? `\\u${u.toString(16).padStart(4, "0")}` : c;
  }).join("");

/** `#truncated in x`: whether `x` is a {@link Preview}, set once by its static block. */
let previewBrand!: (x: object) => boolean;

/** Whether `x` is a {@link Preview} built by {@link Preview.of}, in O(1): no Proxy trap runs. Internal to this package. */
export const isPreview = (x: unknown): x is Preview => typeof x === "object" && x !== null && previewBrand(x);

/**
 * Caller text as an error may hold it: its first {@link MAX_PREVIEW_CODE_POINTS} code points, each unpaired surrogate
 * replaced by U+FFFD, and its whole `length` in UTF-8 bytes, an unpaired surrogate counting 3. The constructor is
 * private and {@link Preview.of} truncates, so an error holding a preview holds at most that much of what the caller
 * sent, however long the text was. The head is built from its code points, a fresh string: never a slice, which an
 * engine may keep as a view of the whole text.
 */
export class Preview {
  // #private state makes the class nominal: an object of the same shape is not a Preview, so only `of` builds one
  readonly #truncated: boolean;

  private constructor(
    readonly head: string,
    readonly length: number,
    keptBytes: number,
  ) {
    this.#truncated = keptBytes < length;
    Object.freeze(this);
  }

  /**
   * The preview of `text`: O(MAX_PREVIEW_CODE_POINTS) to copy, and one scan, without allocation, to measure. A value
   * that is not a string (from a JavaScript caller, or a cast) is never read: its preview is {@link Preview.notText},
   * whose head `<not text>` with a length of 0 no text's preview has.
   */
  static of(text: string): Preview {
    if (typeof (text as unknown) !== "string") return Preview.notText;
    const kept: number[] = [];
    let length = 0;
    let keptBytes = 0;
    for (let i = 0; i < text.length; ) {
      const c = text.codePointAt(i) ?? 0xfffd;
      if (kept.length < MAX_PREVIEW_CODE_POINTS) {
        kept.push(isSurrogate(c) ? 0xfffd : c);
        keptBytes += width(c);
      }
      length += width(c);
      i += c > 0xffff ? 2 : 1;
    }
    return new Preview(String.fromCodePoint(...kept), length, keptBytes);
  }

  /** The preview of something that is not text: the head `<not text>`, a length of 0, not truncated. */
  static readonly notText: Preview = new Preview("<not text>", 0, 0);

  static {
    previewBrand = (x) => #truncated in x;
  }

  /** Whether the text was longer than {@link head}. */
  get truncated(): boolean {
    return this.#truncated;
  }

  /** The head with JSON's escapes, then `…` when truncated: a path segment's form. */
  short(): string {
    return `${escaped(this.head)}${this.truncated ? "…" : ""}`;
  }

  /** The head quoted, `…` when truncated, and the length: `"abc" (UTF-8 length 3)`. */
  toString(): string {
    return `"${escaped(this.head)}"${this.truncated ? "…" : ""} (UTF-8 length ${this.length.toString()})`;
  }
}
