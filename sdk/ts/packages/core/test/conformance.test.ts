import { describe, expect, it, test } from "vitest";

import {
  cbor,
  type CanonicalError,
  contentHash,
  digest,
  digestFromHex,
  DomainTag,
  expandMessageXmd,
  type ExpansionRefusal,
  fromHex,
  type HashAlgorithm,
  hashAlgorithm,
  type HashError,
  type HashErrorKind,
  json,
  models,
  parseHashAlgorithm,
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
const contentAlgorithm = hashAlgorithm(vectors.contentHashAlgorithm) ?? "SHA-256";
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
    const algorithm = hashAlgorithm(c.algorithm);
    expect(algorithm, "a known algorithm").toBeDefined();
    if (algorithm === undefined) return;
    const t = expectOk(DomainTag.of(c.tag), "tag");
    expect(toHex(taggedHash(algorithm, t, bytes(c.message)))).toBe(c.digest);
  });

  it.each(vectors.digests.map((c) => [c.algorithm, c.message.length / 2, c] as const))("%s of %i bytes", (_, __, c) => {
    const algorithm = hashAlgorithm(c.algorithm);
    expect(algorithm, "a known algorithm").toBeDefined();
    if (algorithm === undefined) return;
    expect(toHex(digest(algorithm, bytes(c.message)))).toBe(c.digest);
  });

  it.each(vectors.domainTags.map((c) => [JSON.stringify(c.tag), c] as const))("domain tag %s", (_, c) => {
    expect(outcome(DomainTag.of(c.tag))).toEqual({ outcome: c.outcome });
  });

  it.each(vectors.algorithms.map((c) => [JSON.stringify(c.name), c] as const))("hash function name %s", (_, c) => {
    const r = parseHashAlgorithm(c.name);
    expect(outcome(r)).toEqual({ outcome: c.outcome });
    if (r.ok) expect(r.value).toBe(c.name);
    expect(hashAlgorithm(c.name), "the lookup without the refusal agrees").toBe(r.ok ? r.value : undefined);
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
