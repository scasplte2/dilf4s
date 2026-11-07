/** BIP-39 (`signing.md` 2.1): entropy to mnemonic and back, and mnemonic to seed. */
import { err, ok, Preview, type Result, type TypeMismatch } from "@kd5ujc/dilf4s-core";
import { isArrayGuarded, isRevokedProxy, mint, readGuarded } from "@kd5ujc/dilf4s-core/internal";
import { pbkdf2 } from "@noble/hashes/pbkdf2.js";
import { sha256, sha512 } from "@noble/hashes/sha2.js";
import { randomBytes, utf8ToBytes } from "@noble/hashes/utils.js";

import type { KeyError } from "./errors.js";
import { INSPECT, ownSecret, secretBytes, type Secret } from "./keys.js";
import { expected, NOT_A_NUMBER, NOT_A_STRING } from "./internal/check.js";
import { INTERNAL, type Internal } from "./internal/token.js";
import { ENGLISH } from "./wordlists/english.js";

const nfkd = (s: string): string => s.normalize("NFKD");

/** Unpaired surrogates have no UTF-8 encoding (encoders substitute U+FFFD or `?`, so seeds would diverge): rejected. */
const LONE_SURROGATE = /[\uD800-\uDBFF](?![\uDC00-\uDFFF])|(?<![\uD800-\uDBFF])[\uDC00-\uDFFF]/;
export const wellFormed = (s: string): boolean => !LONE_SURROGATE.test(s);

/** The separators of a phrase: ASCII whitespace (as Java's `\s`) and the ideographic space U+3000. */
const SEPARATORS = /[ \t\n\v\f\r\u3000]+/;

/** A BIP-39 refusal, or an argument of the wrong type. */
export type Bip39Refusal = KeyError | TypeMismatch;

const invalidMnemonic = (reason: string): KeyError => ({ kind: "InvalidMnemonic", reason });

const WORD_LIST = mint();
const MNEMONIC = mint();
let wordsOf!: (x: unknown) => WordList | undefined;
let mnemonicWords!: (x: unknown) => readonly string[] | undefined;
const invalidEntropy = (reason: string): KeyError => ({ kind: "InvalidEntropy", reason });

/** A validated BIP-39 word list: exactly 2048 distinct NFKD-normalised words, and the separator phrases use. */
export class WordList {
  readonly #words: readonly string[];
  readonly #index: ReadonlyMap<string, number>;
  private constructor(
    token: Internal,
    words: readonly string[],
    readonly separator: string,
  ) {
    // without the token none of the caller's arguments is kept: every field is a fixed default
    if (token !== INTERNAL) this.separator = " ";
    this.#words = Object.freeze(token === INTERNAL ? [...words] : []);
    this.#index = new Map(this.#words.map((w, i) => [w, i]));
    if (token === INTERNAL) WORD_LIST.stamp(this);
    Object.freeze(this);
  }

  static {
    wordsOf = (x) => (WORD_LIST.has(x) ? (x as WordList) : undefined);
  }

  /** A list from 2048 words (each NFKD-normalised here); `separator` joins phrases (U+3000 for Japanese). */
  static of(words: readonly string[], separator = " "): Result<WordList, KeyError> {
    // the separator joins generated phrases, so it must be one that parsing splits on
    // a separator is one UTF-16 unit: a longer one is refused by its length, before it is read or previewed
    // a JavaScript caller's separator that is not a string is refused by its type, unread
    if (typeof (separator as unknown) !== "string") return err({ kind: "InvalidWordList", reason: "the separator is not a string" });
    if (separator.length !== 1) return err({ kind: "InvalidWordList", reason: `a separator of ${separator.length.toString()} characters is not one phrases are split on` });
    if (!SEPARATORS.test(separator)) return err({ kind: "InvalidWordList", reason: "the separator is not one phrases are split on", separator: Preview.of(separator) });
    // a JavaScript caller's words that are not an array are refused by their type, in O(1) and unread
    if (!isArrayGuarded(words)) return err({ kind: "InvalidWordList", reason: "the words are not an array" });
    // the count is checked before any word is normalised, read once and written only as a number (a Proxy's array
    // could answer anything): never the caller's text
    const count: unknown = words.length;
    if (count !== 2048) {
      return err({ kind: "InvalidWordList", reason: `expected 2048 words, got ${typeof count === "number" ? count.toString() : "no count"}` });
    }
    // each word is read by its index, once: one that is not a string is refused by its type, never converted
    const normalised: string[] = [];
    for (let i = 0; i < 2048; i++) {
      // guarded: an earlier word's read (a Proxy's trap) may have revoked the array; REVOKED is no string
      const w: unknown = readGuarded(words, i);
      if (typeof w !== "string") return err({ kind: "InvalidWordList", reason: `word ${i.toString()} is not a string` });
      normalised.push(nfkd(w));
    }
    const valid = normalised.every((w) => w.length > 0 && wellFormed(w) && !SEPARATORS.test(w));
    const distinct = new Set(normalised).size;
    if (!valid || distinct !== 2048)
      return err({ kind: "InvalidWordList", reason: `expected 2048 distinct words without separators, got ${distinct.toString()} distinct` });
    return ok(new WordList(INTERNAL, normalised, separator));
  }

  /** The embedded English list. */
  static readonly english: WordList = new WordList(INTERNAL, ENGLISH, " ");

  get words(): readonly string[] {
    return this.#words;
  }

  /** The index of `word`, if the list holds it; what is not a string is found nowhere (and never converted). */
  indexOf(word: string): number | undefined {
    return this.#index.get(word);
  }
}

/** A BIP-39 mnemonic phrase: secret, so it never prints. */
export class Mnemonic {
  readonly #words: readonly string[];
  constructor(
    token: Internal,
    words: readonly string[],
    readonly wordList: WordList,
  ) {
    // without the token none of the caller's arguments is kept: every field is a fixed default
    if (token !== INTERNAL) this.wordList = WordList.english;
    this.#words = Object.freeze(token === INTERNAL ? [...words] : []);
    if (token === INTERNAL) MNEMONIC.stamp(this);
    Object.freeze(this);
  }

  static {
    mnemonicWords = (x) => (MNEMONIC.has(x) ? (x as Mnemonic).#words : undefined);
  }

  get words(): readonly string[] {
    return this.#words;
  }

  /** The words joined by the list's separator. */
  get phrase(): string {
    return this.#words.join(this.wordList.separator);
  }

  toString(): string {
    return `Mnemonic(<${String(this.#words.length)} words redacted>)`;
  }

  toJSON(): string {
    return this.toString();
  }

  [INSPECT](): string {
    return this.toString();
  }
}

const ENTROPY_SIZES = [16, 20, 24, 28, 32];
const WORD_COUNTS = [12, 15, 18, 21, 24];

/** The bits of `bytes`, most significant first. */
function bitsOf(bytes: Uint8Array): number[] {
  const out: number[] = [];
  for (const b of bytes) for (let i = 7; i >= 0; i--) out.push((b >> i) & 1);
  return out;
}

/** A word list made by `WordList.of` (or the English one), or a `TypeMismatch`. */
const wordListArg = (x: unknown): Result<WordList, TypeMismatch> => {
  const w = wordsOf(x);
  return w === undefined ? err(expected("a WordList")) : ok(w);
};

/**
 * 16, 20, 24, 28 or 32 bytes of entropy to 12 to 24 words (ENT/32 checksum bits from SHA-256). The entropy is a
 * `Secret` or bytes measured by their internal length and copied (a subclass claiming 16 bytes while holding 2 gave a
 * two-word mnemonic); what is neither, or a word list not made by `WordList.of`, is a `TypeMismatch`.
 */
export function toMnemonic(entropy: Uint8Array | Secret, words: WordList = WordList.english): Result<Mnemonic, Bip39Refusal> {
  const ent = secretBytes(entropy);
  if (ent === undefined) return err(expected("a Uint8Array or a Secret"));
  const list = wordListArg(words);
  if (!list.ok) return list;
  return mnemonicOf(ent, list.value);
}

function mnemonicOf(ent: Uint8Array, words: WordList): Result<Mnemonic, KeyError> {
  if (!ENTROPY_SIZES.includes(ent.length))
    return err(invalidEntropy(`entropy must be 16..32 bytes in steps of 4, got ${String(ent.length)}`));
  const bits = [...bitsOf(ent), ...bitsOf(sha256(ent)).slice(0, ent.length / 4)];
  const out: string[] = [];
  for (let i = 0; i < bits.length; i += 11) {
    const index = bits.slice(i, i + 11).reduce((acc, b) => acc * 2 + b, 0);
    out.push(words.words[index] ?? "");
  }
  return ok(new Mnemonic(INTERNAL, out, words));
}

/**
 * A fresh mnemonic from `entropyBytes` (16, 20, 24, 28 or 32) of randomness (by default `@noble/hashes` `randomBytes`).
 * A count that is not a number, a word list not made by `WordList.of`, a `random` that is not a function, or randomness
 * it gives that is not bytes, is a `TypeMismatch`; randomness of another length than asked is `InvalidEntropy`.
 */
export function generateMnemonic(
  entropyBytes: number,
  words: WordList = WordList.english,
  random: (n: number) => Uint8Array = randomBytes,
): Result<Mnemonic, Bip39Refusal> {
  if (typeof (entropyBytes as unknown) !== "number") return err(NOT_A_NUMBER);
  const list = wordListArg(words);
  if (!list.ok) return list;
  if (typeof (random as unknown) !== "function" || isRevokedProxy(random)) return err(expected("a function"));
  if (!ENTROPY_SIZES.includes(entropyBytes)) return err(invalidEntropy(`entropy must be 16..32 bytes in steps of 4, got ${String(entropyBytes)}`));
  const ent = secretBytes(random(entropyBytes));
  if (ent === undefined) return err(expected("random bytes"));
  return ent.length === entropyBytes ? mnemonicOf(ent, list.value) : err(invalidEntropy(`the randomness is ${String(ent.length)} bytes, not ${String(entropyBytes)}`));
}

/**
 * Parses a phrase (NFKD, whitespace or U+3000 separators) and checks word count, membership and checksum. A phrase that
 * is not a string (it used to be converted, running its `toString`), or a word list not made by `WordList.of`, is a
 * `TypeMismatch`.
 */
export function toEntropy(phrase: string, words: WordList = WordList.english): Result<{ readonly mnemonic: Mnemonic; readonly entropy: Secret }, Bip39Refusal> {
  if (typeof (phrase as unknown) !== "string") return err(NOT_A_STRING);
  const list = wordListArg(words);
  if (!list.ok) return list;
  return entropyOf(phrase, list.value);
}

function entropyOf(phrase: string, words: WordList): Result<{ readonly mnemonic: Mnemonic; readonly entropy: Secret }, KeyError> {
  if (!wellFormed(phrase)) return err(invalidMnemonic("phrase is not well-formed Unicode"));
  const tokens = nfkd(phrase)
    .split(SEPARATORS)
    .filter((t) => t.length > 0);
  if (!WORD_COUNTS.includes(tokens.length)) return err(invalidMnemonic(`expected 12, 15, 18, 21 or 24 words, got ${String(tokens.length)}`));
  const bits: number[] = [];
  for (const t of tokens) {
    const i = words.indexOf(t);
    if (i === undefined) return err(invalidMnemonic("phrase contains a word not in the list"));
    for (let b = 10; b >= 0; b--) bits.push((i >> b) & 1);
  }
  const checksumBits = tokens.length / 3;
  const entBits = bits.length - checksumBits;
  const entropy = new Uint8Array(entBits / 8);
  for (let i = 0; i < entropy.length; i++) entropy[i] = bits.slice(8 * i, 8 * i + 8).reduce((acc, b) => acc * 2 + b, 0);
  const expected = bitsOf(sha256(entropy)).slice(0, checksumBits);
  if (expected.some((b, i) => b !== bits[entBits + i])) return err(invalidMnemonic("checksum mismatch"));
  return ok({ mnemonic: new Mnemonic(INTERNAL, tokens, words), entropy: ownSecret(entropy) });
}

/**
 * The 64-byte seed: `PBKDF2-HMAC-SHA512(NFKD(words joined by " "), "mnemonic" || NFKD(passphrase), 2048, 64)`. A
 * mnemonic not made by this package, or a passphrase that is not a string, is a `TypeMismatch`.
 */
export function mnemonicToSeed(mnemonic: Mnemonic, passphrase = ""): Result<Secret, Bip39Refusal> {
  const words = mnemonicWords(mnemonic);
  if (words === undefined) return err(expected("a Mnemonic"));
  if (typeof (passphrase as unknown) !== "string") return err(NOT_A_STRING);
  if (!wellFormed(passphrase)) return err(invalidMnemonic("passphrase is not well-formed Unicode"));
  const password = utf8ToBytes(nfkd(words.join(" ")));
  const salt = utf8ToBytes(`mnemonic${nfkd(passphrase)}`);
  return ok(ownSecret(pbkdf2(sha512, password, salt, { c: 2048, dkLen: 64 })));
}
