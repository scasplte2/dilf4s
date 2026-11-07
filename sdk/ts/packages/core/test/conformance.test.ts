import { runInNewContext } from "node:vm";

import { describe, expect, it, test } from "vitest";

import {
  cbor,
  type CanonicalError,
  contentHash,
  digest,
  digestFromBytes,
  digestFromHex,
  DomainTag,
  expandMessageXmd,
  type ExpansionRefusal,
  fromHex,
  type HashAlgorithm,
  type HashError,
  type HashErrorKind,
  json,
  models,
  parseHashAlgorithm,
  Preview,
  type Result,
  taggedHash,
  toHex,
  Value,
  valueCommitment,
} from "../src/index.js";
import { expectOk, loadVectors } from "./vectors.js";

const vectors = loadVectors("canonical_encoding_v1.json", models.canonicalVectors.CanonicalVectors);
const stateKeys = loadVectors("versioned_trie_v1.json", models.versionedTrieVectors.VersionedTrieVectors);

/** A refusal as a row records it: its kind, and its normative limit when it has one (conformance.md section 3). */
const refusal = (e: CanonicalError): { error: string; limit?: bigint } =>
  e.kind === "RationalTooWide" || e.kind === "IntegerTooWide" ? { error: e.kind, limit: BigInt(e.limit) } : { error: e.kind };
const row = (c: { readonly error: string; readonly limit?: bigint }): { error: string; limit?: bigint } =>
  c.limit === undefined ? { error: c.error } : { error: c.error, limit: c.limit };
const merkle = loadVectors("merkle_v1.json", models.merkleVectors.MerkleVectors);
const tag = expectOk(DomainTag.of(vectors.contentHashTag), "the content hash tag is a domain tag");
const contentAlgorithm = known(vectors.contentHashAlgorithm);
const bytes = (hex: string): Uint8Array => expectOk(fromHex(hex), "hex");

describe("canonical_encoding_v1.json", () => {
  const accepted = vectors.json.flatMap((c) => ("cbor" in c ? [c] : []));
  const rejected = vectors.json.flatMap((c) => ("error" in c ? [c] : []));

  it.each(accepted.map((c) => [c.json.slice(0, 60), c] as const))("JSON %s encodes, hashes and renders as the reference does", (_, c) => {
    const value = expectOk(json.parse(c.json), "parses");
    expect(toHex(expectOk(cbor.encode(value), "encodes")), "canonical CBOR").toBe(c.cbor);
    expect(toHex(expectOk(contentHash(contentAlgorithm, tag, value), "encodes")), "content hash").toBe(c.contentHash);
    expect(expectOk(json.renderWithin(value, json.MAX_RENDER_BYTES), "renders"), "display JSON").toBe(c.render);
    const decoded = expectOk(cbor.decode(bytes(c.cbor)), "the CBOR decodes");
    expect(toHex(expectOk(cbor.encode(decoded), "encodes")), "decode then encode is the identity").toBe(c.cbor);
  });

  it.each(rejected.map((c) => [c.json.slice(0, 60), c] as const))("JSON %s is rejected with the reference's error kind", (_, c) => {
    const r = json.parse(c.json);
    expect(r.ok ? "accepted" : refusal(r.error)).toEqual(row(c));
  });

  it("the byte-order mark rows reach the parser with their mark (no decoder stripped it)", () => {
    // TextDecoder drops a leading U+FEFF by default: the rows hold it inside a string, and this checks it arrived
    const marked = rejected.filter((c) => c.json.charCodeAt(0) === 0xfeff);
    expect(marked.map((c) => c.json)).toEqual(["﻿1", "﻿{}"]);
    expect(marked.map((c) => c.error)).toEqual(["JsonSyntax", "JsonSyntax"]);
  });

  it.each(vectors.model.map((c) => [c.value, c] as const))("model value %s decodes, re-encodes and hashes", (_, c) => {
    const value = expectOk(cbor.decode(bytes(c.cbor)), "decodes");
    expect(toHex(expectOk(cbor.encode(value), "encodes"))).toBe(c.cbor);
    expect(toHex(expectOk(contentHash(contentAlgorithm, tag, value), "encodes"))).toBe(c.contentHash);
  });

  it.each(vectors.limits.map((c) => [`${c.name} within ${c.maxBytes.toString()}`, c] as const))("%s: the reference's outcome", (_, c) => {
    const v = expectOk(cbor.decode(bytes(c.value)), "decodes");
    const max = Number(c.maxBytes);
    const r: Result<string, json.RenderError> =
      c.encoding === "cbor" ? ((e) => (e.ok ? { ok: true, value: toHex(e.value) } : e))(cbor.encodeWithin(v, max)) : json.renderWithin(v, max);
    const outcome = r.ok ? "accepted" : r.error.kind === "TooLarge" ? { kind: "TooLarge", limit: BigInt(r.error.limit) } : { kind: r.error.kind };
    expect(outcome).toEqual(c.outcome);
    expect(r.ok ? r.value : undefined).toBe(c.output);
  });

  it.each(vectors.renderRejections.map((c) => [c.cbor.slice(0, 40), c] as const))("CBOR %s does not render to JSON", (_, c) => {
    const v = expectOk(cbor.decode(bytes(c.cbor)), "decodes");
    const r = json.renderWithin(v, json.MAX_RENDER_BYTES);
    expect(r.ok ? "rendered" : r.error.kind === "TooLarge" ? "TooLarge" : refusal(r.error)).toEqual(row(c));
  });

  it.each(vectors.cborRejections.map((c) => [c.cbor.slice(0, 40), c] as const))("CBOR %s is rejected with the reference's error kind", (_, c) => {
    const r = cbor.decode(bytes(c.cbor));
    expect(r.ok ? "accepted" : refusal(r.error)).toEqual(row(c));
  });

  test("coverage (conformance.md section 6): every CanonicalError kind has a row", () => {
    // a Record over the SDK's kinds: a kind added to CanonicalError does not compile until it is listed here
    const coverage: Record<CanonicalError["kind"], models.canonicalVectors.CanonicalErrorKind> = {
      UnpairedSurrogate: "UnpairedSurrogate",
      DuplicateKey: "DuplicateKey",
      RationalTooWide: "RationalTooWide",
      IntegerTooWide: "IntegerTooWide",
      JsonSyntax: "JsonSyntax",
      InvalidNumber: "InvalidNumber",
      NotJsonRepresentable: "NotJsonRepresentable",
      TypeMismatch: "TypeMismatch",
      UnexpectedEnd: "UnexpectedEnd",
      TrailingBytes: "TrailingBytes",
      DepthExceeded: "DepthExceeded",
      NonCanonical: "NonCanonical",
      Unsupported: "Unsupported",
      InvalidUtf8: "InvalidUtf8",
    };
    // and every kind the schema names is one of the SDK's
    const toSdk = (k: models.canonicalVectors.CanonicalErrorKind): CanonicalError["kind"] => k;
    expect(toSdk("TypeMismatch")).toBe("TypeMismatch");
    const typed = stateKeys.stateKeys.flatMap((c) => (c.schema === "TypeMismatch" ? ["TypeMismatch"] : []));
    const pinned = new Set<string>([...rejected.map((c) => c.error), ...vectors.renderRejections.map((c) => c.error), ...vectors.cborRejections.map((c) => c.error), ...typed]);
    expect(Object.values(coverage).filter((k) => !pinned.has(k))).toEqual([]);
  });

  it.each(vectors.taggedHash.map((c) => [c.algorithm, c.message, c] as const))("tagged %s of %s", (_, __, c) => {
    const t = expectOk(DomainTag.of(c.tag), "tag");
    expect(toHex(taggedHash(known(c.algorithm), t, bytes(c.message)))).toBe(c.digest);
  });

  it.each(vectors.digests.map((c) => [c.algorithm, c.message.length / 2, c] as const))("%s of %i bytes", (_, __, c) => {
    expect(toHex(digest(known(c.algorithm), bytes(c.message)))).toBe(c.digest);
  });

  it.each(vectors.domainTags.map((c) => [JSON.stringify(c.tag), c] as const))("domain tag %s", (_, c) => {
    expect(outcome(DomainTag.of(c.tag))).toEqual({ outcome: c.outcome });
  });

  it.each(vectors.algorithms.map((c) => [JSON.stringify(c.name), c] as const))("hash function name %s", (_, c) => {
    const r = parseHashAlgorithm(c.name);
    expect(outcome(r)).toEqual({ outcome: c.outcome });
    if (r.ok) expect(r.value).toBe(c.name);
  });

  it.each(vectors.digestHex.map((c) => [c.algorithm, JSON.stringify(c.hex).slice(0, 40), c] as const))("%s digest from hex %s", (_, __, c) => {
    const r = digestFromHex(known(c.algorithm), c.hex);
    expect(outcome(r)).toEqual(c.offset === undefined ? { outcome: c.outcome } : { outcome: c.outcome, offset: c.offset });
    if (r.ok) expect(toHex(r.value)).toBe((c.hex.startsWith("0x") ? c.hex.slice(2) : c.hex).toLowerCase());
  });

  it.each(vectors.expansions.map((c) => [c.algorithm, c.dst.length / 2, c.length, c] as const))(
    "%s expand_message_xmd, a %i-byte DST, %i bytes",
    (_, __, ___, c) => {
      const r = expandMessageXmd(known(c.algorithm), bytes(c.message), bytes(c.dst), Number(c.length));
      expect(outcome(r)).toEqual(c.refusal === undefined ? { outcome: c.outcome } : { outcome: c.outcome, refusal: c.refusal });
      expect(r.ok ? toHex(r.value) : undefined, "an output exactly when accepted").toBe(c.output);
    },
  );

  test("coverage (conformance.md section 6): every HashError kind has a row, and every expansion refusal", () => {
    // a Record over the SDK's kinds: a kind added to hash-errors.cddl does not compile until it is listed here
    const coverage: Record<HashErrorKind, true> = { InvalidLength: true, InvalidHex: true, InvalidDomainTag: true, InvalidExpansion: true, UnknownAlgorithm: true };
    const refusals: Record<ExpansionRefusal, true> = { emptyDst: true, outputLength: true };
    const pinned = new Set<string>([...vectors.domainTags, ...vectors.algorithms, ...vectors.digestHex, ...vectors.expansions].map((c) => c.outcome));
    expect(Object.keys(coverage).filter((k) => !pinned.has(k))).toEqual([]);
    const refused = new Set(vectors.expansions.flatMap((c) => (c.refusal === undefined ? [] : [c.refusal])));
    expect(Object.keys(refusals).filter((k) => !refused.has(k as ExpansionRefusal))).toEqual([]);
  });
});

describe("a JavaScript caller's wrongly typed argument to the hash API is a typed refusal, never a throw", () => {
  // the argument as any type, as a JavaScript caller may pass it
  const as = (x: unknown): never => x as never;
  const hostile = { toString: (): string => { throw new Error("read"); } };
  const sha = "SHA-256";
  it("names, tags and hex that are not strings", () => {
    expect(parseHashAlgorithm(as(42))).toEqual({ ok: false, error: { kind: "UnknownAlgorithm", name: Preview.of("(number)") } });
    expect(DomainTag.of(as(hostile))).toEqual({ ok: false, error: { kind: "InvalidDomainTag", tag: Preview.of("(object)") } });
    expect(fromHex(as(hostile))).toEqual({ ok: false, error: { kind: "InvalidHex", offset: 0 } });
    expect(digestFromHex(sha, as(null))).toEqual({ ok: false, error: { kind: "InvalidHex", offset: 0 } });
  });
  it("algorithms that are not one of the seven", () => {
    const unknown = { ok: false, error: { kind: "UnknownAlgorithm", name: Preview.of("SHA-1") } };
    expect(digestFromHex(as("SHA-1"), "00")).toEqual(unknown);
    expect(digestFromBytes(as("SHA-1"), new Uint8Array(32))).toEqual(unknown);
    expect(expandMessageXmd(as("SHA-1"), new Uint8Array(0), Uint8Array.of(1), 32)).toEqual(unknown);
    expect(digestFromBytes(as(hostile), new Uint8Array(32))).toEqual({ ok: false, error: { kind: "UnknownAlgorithm", name: Preview.of("(object)") } });
  });
  it("bytes that are not a Uint8Array, and lengths that are not numbers", () => {
    const fake = { length: 32, 0: 1 };
    expect(digestFromBytes(sha, as(fake))).toEqual({ ok: false, error: { kind: "InvalidLength", algorithm: sha, expected: 32, actual: 0 } });
    expect(digestFromBytes(sha, as("x".repeat(32)))).toEqual({ ok: false, error: { kind: "InvalidLength", algorithm: sha, expected: 32, actual: 0 } });
    const refused = (refusal: ExpansionRefusal) => ({ ok: false, error: { kind: "InvalidExpansion", refusal, algorithm: sha, limit: 8160 } });
    expect(expandMessageXmd(sha, new Uint8Array(0), as(fake), 32)).toEqual(refused("emptyDst"));
    expect(expandMessageXmd(sha, new Uint8Array(0), Uint8Array.of(1), as("32"))).toEqual(refused("outputLength"));
    expect(expandMessageXmd(sha, new Uint8Array(0), Uint8Array.of(1), Number.NaN)).toEqual(refused("outputLength"));
    expect(expandMessageXmd(sha, new Uint8Array(0), Uint8Array.of(1), 1.5)).toEqual(refused("outputLength"));
    // a string DST is not bytes: emptyDst stands in until the result types widen (#116)
    expect(expandMessageXmd(sha, new Uint8Array(0), as("dst"), 32)).toEqual(refused("emptyDst"));
    // an object claiming the typed array's name is not one
    const claimed = { [Symbol.toStringTag]: "Uint8Array", length: 32, 0: 1 };
    expect(digestFromBytes(sha, as(claimed))).toEqual({ ok: false, error: { kind: "InvalidLength", algorithm: sha, expected: 32, actual: 0 } });
    expect(expandMessageXmd(sha, new Uint8Array(0), as(claimed), 32)).toEqual(refused("emptyDst"));
  });
  it("a message that is not a Uint8Array throws noble's TypeError: the result type has no room for it until #116", () => {
    expect(() => expandMessageXmd(sha, as("abc"), Uint8Array.of(1), 32)).toThrow(TypeError);
  });
  it("a Buffer and another realm's Uint8Array are bytes", () => {
    const expected = expectOk(expandMessageXmd(sha, Uint8Array.of(1, 2), Uint8Array.of(7, 8, 9), 32), "plain");
    const buffer = Buffer.from(new Uint8Array(32).fill(5));
    const foreign = runInNewContext("new Uint8Array(32).fill(5)") as Uint8Array;
    for (const b of [buffer, foreign]) {
      const d = expectOk(digestFromBytes(sha, b), "a digest");
      expect(Object.getPrototypeOf(d), "a fresh plain array").toBe(Uint8Array.prototype);
      expect(Array.from(d)).toEqual(Array(32).fill(5));
    }
    const dsts = [Buffer.from([7, 8, 9]), runInNewContext("Uint8Array.of(7, 8, 9)") as Uint8Array];
    for (const dst of dsts) expect(expectOk(expandMessageXmd(sha, Uint8Array.of(1, 2), dst, 32), "expands")).toEqual(expected);
  });
  it("a Uint8Array's length is its own: a subclass or an own accessor cannot change it", () => {
    class Liar extends Uint8Array {}
    Object.defineProperty(Liar.prototype, "length", { get: () => 32 });
    const liar = new Liar(5);
    expect(liar.length, "the lie").toBe(32);
    expect(digestFromBytes(sha, liar)).toEqual({ ok: false, error: { kind: "InvalidLength", algorithm: sha, expected: 32, actual: 5 } });
    const own = new Uint8Array(5);
    Object.defineProperty(own, "length", { get: () => 32 });
    expect(digestFromBytes(sha, own)).toEqual({ ok: false, error: { kind: "InvalidLength", algorithm: sha, expected: 32, actual: 5 } });
    // a DST claiming bytes it does not have is empty; one hiding the bytes it has expands as those bytes
    const empty = new Liar(0);
    expect(expandMessageXmd(sha, new Uint8Array(0), empty, 32)).toEqual({
      ok: false,
      error: { kind: "InvalidExpansion", refusal: "emptyDst", algorithm: sha, limit: 8160 },
    });
    const hiding = Uint8Array.of(7, 8, 9);
    Object.defineProperty(hiding, "length", { get: () => 0 });
    expect(expandMessageXmd(sha, Uint8Array.of(1, 2), hiding, 32)).toEqual(expandMessageXmd(sha, Uint8Array.of(1, 2), Uint8Array.of(7, 8, 9), 32));
  });
});

/** A hash call's outcome as a row records it: "accepted", or the kind, with its normative offset or refusal. */
function outcome(r: Result<unknown, HashError>): { outcome: string; offset?: bigint; refusal?: ExpansionRefusal } {
  if (r.ok) return { outcome: "accepted" };
  const e = r.error;
  return e.kind === "InvalidHex" ? { outcome: e.kind, offset: BigInt(e.offset) } : e.kind === "InvalidExpansion" ? { outcome: e.kind, refusal: e.refusal } : { outcome: e.kind };
}

function known(name: string): HashAlgorithm {
  return expectOk(parseHashAlgorithm(name), "a known algorithm");
}

describe("versioned_trie_v1.json state keys", () => {
  const { StateKey } = models.versionedTrie;
  it.each(stateKeys.stateKeys.map((c) => [JSON.stringify(c.key).slice(0, 40), c] as const))("state key %s matches the schema pattern as the reference does", (_, c) => {
    const decoded = StateKey.decode(expectOk(Value.text(c.key), "text"));
    expect(decoded.ok).toBe(c.schema === "accepted");
    const encoded = StateKey.encode(c.key);
    expect(encoded.ok ? "accepted" : encoded.error.kind, "the encoder agrees, refusing as TypeMismatch").toBe(c.schema);
  });
});

describe("merkle_v1.json value commitments", () => {
  const leafTag = expectOk(DomainTag.of("dilf4s/merkle/leaf/v1"), "leaf tag");
  it.each(merkle.valueCommitments.map((c) => [c.cbor, c] as const))("value %s commits as the reference does", (_, c) => {
    const value = expectOk(cbor.decode(bytes(c.cbor)), "decodes");
    const commitment = expectOk(valueCommitment(value), "encodes");
    expect(toHex(commitment), "commitment").toBe(c.commitment);
    expect(toHex(taggedHash("SHA-256", leafTag, commitment)), "leaf hash").toBe(c.leafHash);
  });
});
