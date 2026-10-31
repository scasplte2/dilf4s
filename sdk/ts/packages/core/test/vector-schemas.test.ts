import { describe, expect, it } from "vitest";

import { describeFailure, err, json, models, type Result, type Value } from "../src/index.js";
import type { Codec } from "../src/codec.js";
import { expectOk, readVectors, vectorFiles } from "./vectors.js";

/** Decodes `value` with `codec` and encodes it back. */
function roundTrip<T>(codec: Codec<T>): (value: Value) => Result<Value, string> {
  return (value) => {
    const decoded = codec.decode(value);
    if (!decoded.ok) return err(describeFailure(decoded.error));
    const again = codec.encode(decoded.value);
    return again.ok ? again : err(again.error.kind);
  };
}

/** Every vector file and the generated type of its schema (shared/schemas/<name>-vectors.cddl). */
const schemas: Readonly<Record<string, (value: Value) => Result<Value, string>>> = {
  "canonical_encoding_v1.json": roundTrip(models.canonicalVectors.CanonicalVectors),
  "jlvm_v1.json": roundTrip(models.jlvmVectors.JlvmVectors),
  "kes_v1.json": roundTrip(models.kesVectors.KesVectors),
  "merkle_v1.json": roundTrip(models.merkleVectors.MerkleVectors),
  "poseidon_v1.json": roundTrip(models.poseidonVectors.PoseidonVectors),
  "sigma_dilf4s_v1.json": roundTrip(models.sigmaVectors.SigmaVectors),
  "sigma_v1.json": roundTrip(models.sigmaVectors.SigmaVectors),
  "signing_v1.json": roundTrip(models.signingVectors.SigningVectors),
  "trie_v1.json": roundTrip(models.trieVectors.TrieVectors),
  "vrf_v1.json": roundTrip(models.vrfVectors.VrfVectors),
};

describe("the generated models hold every vector file", () => {
  it("every vector file in shared/vectors has a schema", () => {
    expect(vectorFiles()).toEqual(Object.keys(schemas).sort());
  });

  it.each(Object.entries(schemas))("%s decodes into its type and re-encodes to the same value", (file, check) => {
    const value = expectOk(json.parse(readVectors(file)), "canonical JSON");
    const again = expectOk(check(value), file);
    expect(expectOk(json.render(again), "renders")).toBe(expectOk(json.render(value), "renders"));
  });

  it("a schema violation names the failing path", () => {
    const bad = readVectors("merkle_v1.json").replace(/"leaves"\s*:\s*\[\s*"/, '"leaves": ["zz');
    const decoded = models.merkleVectors.MerkleVectors.decode(expectOk(json.parse(bad), "canonical JSON"));
    expect(decoded.ok ? "decoded" : describeFailure(decoded.error)).toMatch(/^leaves\.\[0\]: /);
  });

  it("unions are told apart by keys when encoding, as when decoding", () => {
    const { TrieLookupResult } = models.trieVectors;
    const present = expectOk(TrieLookupResult.encode({ present: "00ff" }), "record alternative");
    expect(expectOk(json.render(present), "renders")).toBe('{"present":"00ff"}');
    expect(expectOk(json.render(expectOk(TrieLookupResult.encode("absent"), "text alternative")), "renders")).toBe('"absent"');
    const back = TrieLookupResult.decode(present);
    expect(back.ok && back.value).toEqual({ present: "00ff" });
  });

  it("an encoder refuses what its decoder would reject", () => {
    expect(models.merkle.TreeIndex.encode(-1n).ok).toBe(false);
    expect(models.merkle.TreeIndex.encode(2n ** 63n).ok).toBe(false);
    expect(models.crypto.Ed25519Signature.encode(new Uint8Array(63)).ok).toBe(false);
    expect(models.canonicalVectors.Hex.encode("0A").ok).toBe(false);
  });

  it("CDDL integers follow RFC 8610: uint is major type 0", () => {
    const { TrieBits } = models.trieVectors;
    expect(TrieBits.encode({ bits: 2n ** 64n - 1n, bytes: "" }).ok, "2^64 - 1 is a uint").toBe(true);
    expect(TrieBits.encode({ bits: 2n ** 64n, bytes: "" }).ok, "2^64 is not").toBe(false);
    const wide = expectOk(json.parse('{"bits":18446744073709551616,"bytes":""}'), "canonical JSON");
    expect(TrieBits.decode(wide).ok, "nor does it decode").toBe(false);
  });

  it("a recursive model round-trips through CBOR", () => {
    const program = '{"jlvm":1,"expr":{"op":"+","args":[{"lit":1},{"var":"x","default":{"path":{"lit":"y"}}},{"map":{"k":{}}}]}}';
    const value = expectOk(json.parse(program), "canonical JSON");
    const decoded = models.jlvm.JlvmProgram.decode(value);
    if (!decoded.ok) throw new Error(describeFailure(decoded.error));
    expect(expectOk(json.render(expectOk(models.jlvm.JlvmProgram.encode(decoded.value), "encodes")), "renders")).toBe(
      expectOk(json.render(value), "renders"),
    );
  });
});
