import { expect, test } from "vitest";

import { BytesValue, models } from "@kd5ujc/dilf4s-core";

import {
  bip340,
  blsPop,
  CardanoSumKes,
  ecvrfDraft03,
  ecvrfTai,
  ed25519,
  icarus,
  Message32,
  p256Ecdsa,
  ristretto255Schnorr,
  secp256k1Ecdsa,
  taggedSchnorr,
  toEntropy,
} from "../src/index.js";
import { expectOk } from "./vectors.js";

/** `signing.md` section 4: every real output fits its normative size in crypto.cddl (the generated codecs). */
test("real outputs have the sizes of shared/schemas/crypto.cddl", () => {
  const c = models.crypto;
  const fits = (codec: { decode: (v: BytesValue) => { ok: boolean } }, bytes: Uint8Array, what: string): void => {
    expect(codec.decode(BytesValue.of(bytes)).ok, `${what} (${String(bytes.length)} bytes)`).toBe(true);
  };
  const seed = new Uint8Array(32).fill(7);
  const msg = Uint8Array.of(1, 2, 3);
  const aux = new Uint8Array(32);

  const ed = expectOk(ed25519.fromSeed(seed), "ed");
  fits(c.Ed25519SigningKey, ed.signing.bytes, "ed25519 signing key");
  fits(c.Ed25519VerifyingKey, ed.verifying.bytes, "ed25519 verifying key");
  fits(c.Ed25519Signature, ed25519.sign(ed.signing, msg).bytes, "ed25519 signature");

  const root = expectOk(icarus.fromEntropy(expectOk(toEntropy("abandon ".repeat(11) + "about"), "e").entropy), "icarus");
  fits(c.Ed25519Bip32SigningKey, root.key.bytes, "ed25519-bip32 signing key");
  fits(c.ChainCode, root.chainCode, "chain code");

  const k1 = expectOk(secp256k1Ecdsa.fromSeed(seed), "secp256k1");
  fits(c.EcdsaSigningKey, k1.signing.bytes, "secp256k1 signing key");
  fits(c.EcdsaVerifyingKey, k1.verifying.bytes, "secp256k1 verifying key");
  fits(c.EcdsaSignature, secp256k1Ecdsa.sign(k1.signing, msg).bytes, "secp256k1 signature");
  const p = expectOk(p256Ecdsa.fromSeed(seed), "p256");
  fits(c.EcdsaSigningKey, p.signing.bytes, "p256 signing key");
  fits(c.EcdsaVerifyingKey, p.verifying.bytes, "p256 verifying key");
  fits(c.EcdsaSignature, p256Ecdsa.sign(p.signing, msg).bytes, "ecdsa signature");

  const s = expectOk(bip340.fromSeed(seed), "bip340");
  const m32 = expectOk(Message32.of(new Uint8Array(32)), "m32");
  fits(c.Bip340Message, m32.bytes, "bip340 message");
  fits(c.SchnorrSigningKey, s.signing.bytes, "schnorr signing key");
  fits(c.SchnorrVerifyingKey, s.verifying.bytes, "schnorr verifying key");
  fits(c.SchnorrSignature, expectOk(bip340.sign(s.signing, m32, aux), "sig").bytes, "bip340 signature");
  const t = expectOk(taggedSchnorr.fromSeed(seed), "tagged");
  fits(c.SchnorrSignature, expectOk(taggedSchnorr.sign(t.signing, msg, aux), "sig").bytes, "tagged signature");

  const r = expectOk(ristretto255Schnorr.fromSeed(seed), "ristretto");
  fits(c.Ristretto255SigningKey, r.signing.bytes, "ristretto signing key");
  fits(c.Ristretto255VerifyingKey, r.verifying.bytes, "ristretto verifying key");
  fits(c.Ristretto255Signature, expectOk(ristretto255Schnorr.sign(r.signing, msg, aux), "sig").bytes, "ristretto signature");

  const b = expectOk(blsPop.fromSeed(seed), "bls");
  fits(c.BlsSigningKey, b.signing.bytes, "bls signing key");
  fits(c.BlsVerifyingKey, b.verifying.bytes, "bls verifying key");
  fits(c.BlsSignature, blsPop.sign(b.signing, msg).bytes, "bls signature");

  const v = expectOk(ecvrfTai.fromSeed(seed), "vrf");
  const proved = expectOk(ecvrfTai.prove(v.signing, msg), "prove");
  fits(c.VrfSigningKey, v.signing.bytes, "vrf signing key");
  fits(c.VrfVerifyingKey, v.verifying.bytes, "vrf verifying key");
  fits(c.VrfProof, proved.proof.bytes, "vrf proof");
  fits(c.VrfOutput, proved.output.bytes, "vrf output");
  fits(c.VrfDraft03SigningKey, expectOk(ecvrfDraft03.fromSeed(seed), "draft03").signing.bytes, "draft-03 signing key");

  const k = expectOk(CardanoSumKes.sum6.generate(seed), "sum6");
  fits(c.Sum6KesSigningKey, k.bytes, "sum6 signing key");
  fits(c.Sum6KesVerifyingKey, k.verifyingKey, "sum6 verifying key");
  fits(c.Sum6KesSignature, k.sign(msg), "sum6 signature");
  fits(c.Bip39Entropy, expectOk(toEntropy("abandon ".repeat(11) + "about"), "e").entropy.bytes, "bip39 entropy");
});
