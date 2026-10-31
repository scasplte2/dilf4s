/** BIP-39 (`signing.md` 2.1): entropy to mnemonic and back, and mnemonic to seed. */
import { err, ok, type Result } from "@kd5ujc/dilf4s-core";
import { pbkdf2 } from "@noble/hashes/pbkdf2.js";
import { sha256, sha512 } from "@noble/hashes/sha2.js";
import { randomBytes, utf8ToBytes } from "@noble/hashes/utils.js";

import type { KeyError } from "./errors.js";
import { INSPECT, Secret } from "./keys.js";
import { INTERNAL, type Internal } from "./internal/token.js";
import { ENGLISH } from "./wordlists/english.js";

const nfkd = (s: string): string => s.normalize("NFKD");

/** Unpaired surrogates have no UTF-8 encoding (encoders substitute U+FFFD or `?`, so seeds would diverge): rejected. */
const LONE_SURROGATE = /[\uD800-\uDBFF](?![\uDC00-\uDFFF])|(?<![\uD800-\uDBFF])[\uDC00-\uDFFF]/;
export const wellFormed = (s: string): boolean => !LONE_SURROGATE.test(s);

/** The separators of a phrase: ASCII whitespace (as Java's `\s`) and the ideographic space U+3000. */
const SEPARATORS = /[ \t\n\v\f\r\u3000]+/;

const invalidMnemonic = (reason: string): KeyError => ({ kind: "InvalidMnemonic", reason });
const invalidEntropy = (reason: string): KeyError => ({ kind: "InvalidEntropy", reason });

/** A validated BIP-39 word list: exactly 2048 distinct NFKD-normalised words, and the separator phrases use. */
export class WordList {
  readonly #words: readonly string[];
  readonly #index: ReadonlyMap<string, number>;
  private constructor(
    words: readonly string[],
    readonly separator: string,
  ) {
    this.#words = Object.freeze([...words]);
    this.#index = new Map(words.map((w, i) => [w, i]));
    Object.freeze(this);
  }

  /** A list from 2048 words (each NFKD-normalised here); `separator` joins phrases (U+3000 for Japanese). */
  static of(words: readonly string[], separator = " "): Result<WordList, KeyError> {
    const normalised = words.map(nfkd);
    const valid = normalised.every((w) => w.length > 0 && wellFormed(w) && !SEPARATORS.test(w));
    if (!valid || normalised.length !== 2048 || new Set(normalised).size !== 2048)
      return err({ kind: "InvalidWordList", reason: `expected 2048 distinct words without separators, got ${String(words.length)}` });
    return ok(new WordList(normalised, separator));
  }

  /** The embedded English list. */
  static readonly english: WordList = new WordList(ENGLISH, " ");

  get words(): readonly string[] {
    return this.#words;
  }

  indexOf(word: string): number | undefined {
    return this.#index.get(word);
  }
}

/** A BIP-39 mnemonic phrase: secret, so it never prints. */
export class Mnemonic {
  readonly #words: readonly string[];
  constructor(
    _token: Internal,
    words: readonly string[],
    readonly wordList: WordList,
  ) {
    this.#words = Object.freeze([...words]);
    Object.freeze(this);
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

/** 16, 20, 24, 28 or 32 bytes of entropy to 12 to 24 words (ENT/32 checksum bits from SHA-256). */
export function toMnemonic(entropy: Uint8Array | Secret, words: WordList = WordList.english): Result<Mnemonic, KeyError> {
  const ent = entropy instanceof Secret ? entropy.bytes : entropy;
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

/** A fresh mnemonic from `entropyBytes` (16, 20, 24, 28 or 32) of randomness (by default `@noble/hashes` `randomBytes`). */
export function generateMnemonic(
  entropyBytes: number,
  words: WordList = WordList.english,
  random: (n: number) => Uint8Array = randomBytes,
): Result<Mnemonic, KeyError> {
  return ENTROPY_SIZES.includes(entropyBytes)
    ? toMnemonic(random(entropyBytes), words)
    : err(invalidEntropy(`entropy must be 16..32 bytes in steps of 4, got ${String(entropyBytes)}`));
}

/** Parses a phrase (NFKD, whitespace or U+3000 separators) and checks word count, membership and checksum. */
export function toEntropy(phrase: string, words: WordList = WordList.english): Result<{ readonly mnemonic: Mnemonic; readonly entropy: Secret }, KeyError> {
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
  return ok({ mnemonic: new Mnemonic(INTERNAL, tokens, words), entropy: Secret.of(entropy) });
}

/** The 64-byte seed: `PBKDF2-HMAC-SHA512(NFKD(words joined by " "), "mnemonic" || NFKD(passphrase), 2048, 64)`. */
export function mnemonicToSeed(mnemonic: Mnemonic, passphrase = ""): Result<Secret, KeyError> {
  if (!wellFormed(passphrase)) return err(invalidMnemonic("passphrase is not well-formed Unicode"));
  const password = utf8ToBytes(nfkd(mnemonic.words.join(" ")));
  const salt = utf8ToBytes(`mnemonic${nfkd(passphrase)}`);
  return ok(Secret.of(pbkdf2(sha512, password, salt, { c: 2048, dkLen: 64 })));
}
