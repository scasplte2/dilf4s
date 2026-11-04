import { describe, expect, it } from "vitest";

import { cbor, contentHash, digest, DomainTag, fromHex, hashAlgorithm, json, models, taggedHash, toHex, Value, valueCommitment } from "../src/index.js";
import { expectOk, loadVectors } from "./vectors.js";

const vectors = loadVectors("canonical_encoding_v1.json", models.canonicalVectors.CanonicalVectors);
const merkle = loadVectors("merkle_v1.json", models.merkleVectors.MerkleVectors);
const tag = expectOk(DomainTag.of(vectors.contentHashTag), "the content hash tag is a domain tag");
const contentAlgorithm = hashAlgorithm(vectors.contentHashAlgorithm) ?? "SHA-256";
const bytes = (hex: string): Uint8Array => expectOk(fromHex(hex), "hex");

describe("canonical_encoding_v1.json", () => {
  const accepted = vectors.json.flatMap((c) => ("render" in c ? [c] : []));
  const unrenderable = vectors.json.flatMap((c) => ("renderError" in c ? [c] : []));
  const rejected = vectors.json.flatMap((c) => ("error" in c ? [c] : []));

  it.each(accepted.map((c) => [c.json.slice(0, 60), c] as const))("JSON %s encodes, hashes and renders as the reference does", (_, c) => {
    const value = expectOk(json.parse(c.json), "parses");
    expect(toHex(expectOk(cbor.encode(value), "encodes")), "canonical CBOR").toBe(c.cbor);
    expect(toHex(expectOk(contentHash(contentAlgorithm, tag, value), "encodes")), "content hash").toBe(c.contentHash);
    expect(expectOk(json.renderWithin(value, json.MAX_RENDER_BYTES), "renders"), "display JSON").toBe(c.render);
    const decoded = expectOk(cbor.decode(bytes(c.cbor)), "the CBOR decodes");
    expect(toHex(expectOk(cbor.encode(decoded), "encodes")), "decode then encode is the identity").toBe(c.cbor);
  });

  it.each(unrenderable.map((c) => [c.json.slice(0, 60), c] as const))("JSON %s encodes and hashes as the reference does, and does not render", (_, c) => {
    const value = expectOk(json.parse(c.json), "parses");
    expect(toHex(expectOk(cbor.encode(value), "encodes")), "canonical CBOR").toBe(c.cbor);
    expect(toHex(expectOk(contentHash(contentAlgorithm, tag, value), "encodes")), "content hash").toBe(c.contentHash);
    const r = json.renderWithin(value, json.MAX_RENDER_BYTES);
    expect(r.ok ? "rendered" : r.error.kind, "display JSON").toBe(c.renderError);
    const decoded = expectOk(cbor.decode(bytes(c.cbor)), "the CBOR decodes");
    expect(toHex(expectOk(cbor.encode(decoded), "encodes")), "decode then encode is the identity").toBe(c.cbor);
  });

  it.each(rejected.map((c) => [c.json.slice(0, 60), c] as const))("JSON %s is rejected with the reference's error kind", (_, c) => {
    const r = json.parse(c.json);
    expect(r.ok ? "accepted" : r.error.kind).toBe(c.error);
  });

  it.each(vectors.model.map((c) => [c.value, c] as const))("model value %s decodes, re-encodes and hashes", (_, c) => {
    const value = expectOk(cbor.decode(bytes(c.cbor)), "decodes");
    expect(toHex(expectOk(cbor.encode(value), "encodes"))).toBe(c.cbor);
    expect(toHex(expectOk(contentHash(contentAlgorithm, tag, value), "encodes"))).toBe(c.contentHash);
  });

  it.each(vectors.renderRejections.map((c) => [c.cbor.slice(0, 40), c] as const))("CBOR %s does not render to JSON", (_, c) => {
    const v = expectOk(cbor.decode(bytes(c.cbor)), "decodes");
    const r = json.renderWithin(v, json.MAX_RENDER_BYTES);
    expect(r.ok ? "rendered" : r.error.kind).toBe(c.error);
  });

  it.each(vectors.cborRejections.map((c) => [c.cbor.slice(0, 40), c] as const))("CBOR %s is rejected with the reference's error kind", (_, c) => {
    const r = cbor.decode(bytes(c.cbor));
    expect(r.ok ? "accepted" : r.error.kind).toBe(c.error);
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
    expect(DomainTag.of(c.tag).ok).toBe(c.valid);
  });
});

describe("versioned_trie_v1.json state keys", () => {
  const vt = loadVectors("versioned_trie_v1.json", models.versionedTrieVectors.VersionedTrieVectors);
  const { StateKey } = models.versionedTrie;
  it.each(vt.stateKeys.map((c) => [JSON.stringify(c.key).slice(0, 40), c] as const))("state key %s matches the schema pattern as the reference does", (_, c) => {
    const decoded = StateKey.decode(expectOk(Value.text(c.key), "text"));
    expect(decoded.ok).toBe(c.schema);
    expect(StateKey.encode(c.key).ok, "the encoder agrees").toBe(c.schema);
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
