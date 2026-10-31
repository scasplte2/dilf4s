import { readdirSync } from "node:fs";
import { inspect } from "node:util";

import { describe, expect, test } from "vitest";

import { digest, models, toHex, type Result } from "@kd5ujc/dilf4s-core";

import {
  bip32Secp256k1,
  ChildIndex,
  ed25519Bip32,
  ExtendedKey,
  generateMnemonic,
  icarus,
  mnemonicToSeed,
  parsePath,
  secp256k1Ecdsa,
  Secret,
  slip10Ed25519,
  toEntropy,
  toMnemonic,
  WordList,
  type KeyError,
} from "../src/index.js";
import { ENGLISH } from "../src/wordlists/english.js";
import { expectOk, h, loadVectors, readDoc, readResource, resourcesDir, utf8 } from "./vectors.js";

const vectors = loadVectors("signing_v1.json", models.signingVectors.SigningVectors);
const kind = <T>(r: Result<T, KeyError | { kind: string }>): string => (r.ok ? "ok" : r.error.kind);
const path = (p: string): readonly ChildIndex[] => expectOk(parsePath(p), p);
const nfkd = (s: string): string => s.normalize("NFKD");

/** A bundled word list as Scala loads it: lines, trimmed, NFKD, without empties. */
function wordList(language: string): WordList {
  const words = readResource(`bip-0039/${language}.txt`)
    .split("\n")
    .map((w) => nfkd(w.trim()))
    .filter((w) => w.length > 0);
  return expectOk(WordList.of(words, language === "japanese" ? "　" : " "), language);
}

describe("BIP-39", { timeout: 60_000 }, () => {
  test("the embedded English list is crypto/src/main/resources/bip-0039/english.txt, byte for byte", () => {
    const file = readResource("bip-0039/english.txt");
    expect(toHex(digest("SHA-256", utf8(ENGLISH.join("\n"))))).toBe(toHex(digest("SHA-256", utf8(file))));
    expect(WordList.english.words.length).toBe(2048);
  });

  test("signing_v1.json bip39: entropy to phrase to entropy, and the seed", () => {
    expect(vectors.bip39.length).toBe(6);
    for (const v of vectors.bip39) {
      const m = expectOk(toMnemonic(h(v.entropy)), "mnemonic");
      expect(m.phrase).toBe(v.mnemonic);
      expect(toHex(expectOk(toEntropy(v.mnemonic), "entropy").entropy.bytes)).toBe(v.entropy);
      expect(toHex(expectOk(mnemonicToSeed(m, v.passphrase), "seed").bytes)).toBe(v.seed);
    }
  });

  test("Trezor vectors (passphrase TREZOR)", () => {
    const trezor = [
      [
        "00000000000000000000000000000000",
        "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
        "c55257c360c07c72029aebc1b53c05ed0362ada38ead3e3e9efa3708e53495531f09a6987599d18264c1e1c92f2cf141630c7a3c4ab7c81b2f001698e7463b04",
      ],
      [
        "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f",
        "legal winner thank year wave sausage worth useful legal winner thank yellow",
        "2e8905819b8723fe2c1d161860e5ee1830318dbf49a83bd451cfb8440c28bd6fa457fe1296106559a3c80937a1c1069be3a3a5bd381ee6260e8d9739fce1f607",
      ],
      [
        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
        "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo vote",
        "dd48c104698c30cfe2b6142103248622fb7bb0ff692eebb00089b32d22484e1613912f0a5b694407be899ffd31ed3992c456cdf60f5d4564b8ba3f05a69890ad",
      ],
    ] as const;
    for (const [entropy, phrase, seed] of trezor) {
      const m = expectOk(toMnemonic(h(entropy)), "m");
      expect(m.phrase).toBe(phrase);
      expect(toHex(expectOk(toEntropy(phrase), "e").entropy.bytes)).toBe(entropy);
      expect(toHex(expectOk(mnemonicToSeed(m, "TREZOR"), "seed").bytes)).toBe(seed);
    }
  });

  test("Japanese vectors (ideographic spaces, NFKD passphrase) through a caller-supplied WordList", () => {
    const words = wordList("japanese");
    const pass = "㍍ガバヴァぱばぐゞちぢ十人十色";
    const japanese = [
      [
        "00000000000000000000000000000000",
        "あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あおぞら",
        "a262d6fb6122ecf45be09c50492b31f92e9beb7d9a845987a02cefda57a15f9c467a17872029a9e92299b5cbdf306e3a0ee620245cbd508959b6cb7ca637bd55",
      ],
      [
        "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f",
        "そつう　れきだい　ほんやく　わかす　りくつ　ばいか　ろせん　やちん　そつう　れきだい　ほんやく　わかめ",
        "aee025cbe6ca256862f889e48110a6a382365142f7d16f2b9545285b3af64e542143a577e9c144e101a6bdca18f8d97ec3366ebf5b088b1c1af9bc31346e60d9",
      ],
      [
        "000000000000000000000000000000000000000000000000",
        "あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あいこくしん　あらいぐま",
        "d99e8f1ce2d4288d30b9c815ae981edd923c01aa4ffdc5dee1ab5fe0d4a3e13966023324d119105aff266dac32e5cd11431eeca23bbd7202ff423f30d6776d69",
      ],
    ] as const;
    for (const [entropy, phrase, seed] of japanese) {
      const m = expectOk(toMnemonic(h(entropy), words), "m");
      expect(m.phrase).toBe(phrase.split("　").map(nfkd).join("　"));
      const parsed = expectOk(toEntropy(phrase, words), "e");
      expect(toHex(parsed.entropy.bytes)).toBe(entropy);
      expect(toHex(expectOk(mnemonicToSeed(parsed.mnemonic, pass), "seed").bytes)).toBe(seed);
      expect(toHex(expectOk(mnemonicToSeed(m, pass), "seed").bytes)).toBe(seed);
    }
  });

  test("every bundled word list loads and round-trips", () => {
    const languages = readdirSync(`${resourcesDir}bip-0039`).filter((f) => f.endsWith(".txt"));
    expect(languages.length).toBe(10);
    for (const f of languages) {
      const words = wordList(f.replace(".txt", ""));
      const m = expectOk(toMnemonic(h("7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f"), words), f);
      expect(toHex(expectOk(toEntropy(m.phrase, words), f).entropy.bytes)).toBe("7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f");
    }
  });

  test("mnemonic to entropy (BramblSc Bip39MnemonicToEntropy.json)", () => {
    const vs = JSON.parse(readDoc("Bip39MnemonicToEntropy.json")) as { inputs: { mnemonic: string }; outputs: { entropy: string } }[];
    expect(vs.length).toBe(25);
    for (const v of vs) expect(toHex(expectOk(toEntropy(v.inputs.mnemonic), v.inputs.mnemonic).entropy.bytes)).toBe(v.outputs.entropy);
  });

  test("bad checksum, unknown word, wrong length, bad entropy, bad lists and lone surrogates are typed errors", () => {
    const abandon = (n: number): string => Array<string>(n).fill("abandon").join(" ");
    expect(kind(toEntropy(abandon(12)))).toBe("InvalidMnemonic");
    expect(kind(toEntropy(`${abandon(11)} abou`))).toBe("InvalidMnemonic");
    expect(kind(toEntropy(abandon(11)))).toBe("InvalidMnemonic");
    expect(kind(toMnemonic(new Uint8Array(17)))).toBe("InvalidEntropy");
    expect(kind(generateMnemonic(17))).toBe("InvalidEntropy");
    expect(kind(toEntropy(`  ${abandon(11)}   about \t`))).toBe("ok");
    // U+2028 is whitespace to JavaScript's \s, not to Java's: not a separator here
    expect(kind(toEntropy(`${abandon(11)}\u2028about`))).toBe("InvalidMnemonic");
    expect(kind(WordList.of(ENGLISH.slice(1)))).toBe("InvalidWordList");
    expect(kind(WordList.of([...ENGLISH.slice(1), "ability"]))).toBe("InvalidWordList");
    expect(kind(WordList.of([...ENGLISH.slice(1), "two words"]))).toBe("InvalidWordList");
    expect(kind(WordList.of(ENGLISH, ","))).toBe("InvalidWordList");
    expect(kind(WordList.of(ENGLISH, "  "))).toBe("InvalidWordList");
    const m = expectOk(toMnemonic(new Uint8Array(16)), "m");
    expect(kind(mnemonicToSeed(m, "\uD800"))).toBe("InvalidMnemonic");
    expect(kind(icarus.fromEntropy(new Uint8Array(16), "x\uDC00"))).toBe("InvalidDerivation");
  });

  test("generated mnemonics have the requested length and validate", () => {
    for (const n of [16, 20, 24, 28, 32]) {
      const m = expectOk(generateMnemonic(n), String(n));
      expect(m.words.length).toBe((n * 3) / 4);
      expect(toEntropy(m.phrase).ok).toBe(true);
    }
  });

  test("secrets and mnemonics are redacted", () => {
    const m = expectOk(toMnemonic(new Uint8Array(16)), "m");
    const s = Secret.of(h("deadbeef"));
    const text = [String(m), JSON.stringify(m), inspect(m), String(s), JSON.stringify({ s }), inspect(s)].join(" ");
    expect(text).not.toContain("abandon");
    expect(text).not.toContain("deadbeef");
  });
});

describe("paths", { timeout: 60_000 }, () => {
  test("hardened and soft segments parse; malformed and out-of-range ones are errors", () => {
    expect(path("m/44'/0/7").map((i) => i.serialized)).toEqual([0x8000002c, 0, 7]);
    expect(path("m/0h").map((i) => i.serialized)).toEqual([0x80000000]);
    expect(path("m")).toEqual([]);
    expect(path("m/2147483647'").map((i) => i.serialized)).toEqual([0xffffffff]);
    for (const bad of ["44'/0", "m/x", "m/2147483648", "m/-1", "m/+1", "m/0/", "m//0", "m/0''", " m/0", "M/0", "m/1e3", "m/0x1"])
      expect(kind(parsePath(bad)), bad).toBe("InvalidDerivation");
    expect(kind(ChildIndex.soft(2 ** 31))).toBe("InvalidDerivation");
    expect(kind(ChildIndex.hardened(-1))).toBe("InvalidDerivation");
    expect(kind(ChildIndex.soft(1.5))).toBe("InvalidDerivation");
  });
});

describe("SLIP-10 Ed25519", { timeout: 60_000 }, () => {
  test("signing_v1.json slip10Ed25519", () => {
    const v = vectors.slip10Ed25519;
    const master = expectOk(slip10Ed25519.master(h(v.seed)), "master");
    expect(toHex(master.key.bytes)).toBe(v.master.key);
    expect(toHex(master.chainCode)).toBe(v.master.chainCode);
    const child = expectOk(slip10Ed25519.derive(h(v.seed), path(v.path)), "child");
    expect(toHex(child.key.bytes)).toBe(v.child.key);
    expect(toHex(child.chainCode)).toBe(v.child.chainCode);
  });

  test("SLIP-10 test vector 1; soft indices and bad seeds are rejected", () => {
    const seed = h("000102030405060708090a0b0c0d0e0f");
    const m = expectOk(slip10Ed25519.derive(seed, []), "m");
    expect(toHex(m.key.bytes)).toBe("2b4be7f19ee27bbf30c667b642d5f4aa69fd169872f8fc3059c08ebae2eb19e7");
    expect(toHex(m.chainCode)).toBe("90046a93de5380a72b5e45010748567d5ea02bbf6522f979e05c0d8d8ca9fffb");
    const c = expectOk(slip10Ed25519.derive(seed, path("m/0'")), "m/0'");
    expect(toHex(c.key.bytes)).toBe("68e0fe46dfb67e368c75379acec591dad19df3cde26e63b93a8e704f1dade7a3");
    expect(toHex(c.chainCode)).toBe("8b59aa11380b624e81507a27fedda59fea6d0b779a778918a2fd3590e16e9c69");
    expect(kind(slip10Ed25519.derive(seed, path("m/0")))).toBe("InvalidDerivation");
    expect(kind(slip10Ed25519.master(new Uint8Array(15)))).toBe("InvalidDerivation");
    expect(kind(slip10Ed25519.master(new Uint8Array(65)))).toBe("InvalidDerivation");
  });
});

describe("BIP32 secp256k1", { timeout: 60_000 }, () => {
  test("signing_v1.json bip32Secp256k1, private and public derivation", () => {
    const v = vectors.bip32Secp256k1;
    const master = expectOk(bip32Secp256k1.master(h(v.seed)), "master");
    expect(toHex(master.key.bytes)).toBe(v.master.key);
    expect(toHex(master.chainCode)).toBe(v.master.chainCode);
    const child = expectOk(bip32Secp256k1.derive(h(v.seed), path(v.path)), "child");
    expect(toHex(child.key.bytes)).toBe(v.child.key);
    expect(toHex(child.chainCode)).toBe(v.child.chainCode);
    expect(toHex(secp256k1Ecdsa.publicKey(child.key).bytes)).toBe(v.verifyingKey);

    const p = v.publicDerivation;
    const parent = expectOk(bip32Secp256k1.derive(h(v.seed), path(p.parentPath)), "parent");
    expect(toHex(secp256k1Ecdsa.publicKey(parent.key).bytes)).toBe(p.parentVerifyingKey);
    expect(toHex(parent.chainCode)).toBe(p.parentChainCode);
    let acc = { key: expectOk(secp256k1Ecdsa.verifyingKey(h(p.parentVerifyingKey)), "vk"), chainCode: h(p.parentChainCode) };
    for (const i of path(p.path)) acc = expectOk(bip32Secp256k1.publicChild(acc.key, acc.chainCode, i), "public child");
    expect(toHex(acc.key.bytes)).toBe(p.verifyingKey);
    expect(toHex(acc.chainCode)).toBe(p.chainCode);
  });

  test("BIP32 test vector 1, and CKDpub matches CKDpriv", () => {
    const seed = h("000102030405060708090a0b0c0d0e0f");
    const at = (p: string) => expectOk(bip32Secp256k1.derive(seed, path(p)), p);
    expect(toHex(at("m").key.bytes)).toBe("e8f32e723decf4051aefac8e2c93c9c5b214313817cdb01a1494b917c8436b35");
    expect(toHex(at("m").chainCode)).toBe("873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d508");
    expect(toHex(at("m/0'").key.bytes)).toBe("edb2e14f9ee77d26dd93b4ecede8d16ed408ce149b6cd80b0715a2d911a0afea");
    expect(toHex(at("m/0'").chainCode)).toBe("47fdacbd0f1097043b78c63c20c34ef4ed9a111d980047ad16282c7ae6236141");
    expect(toHex(at("m/0'/1").key.bytes)).toBe("3c6cb8d0f6a264c91ea8b5030fadaa8e538b020f0a387421a12de9319dc93368");
    expect(toHex(at("m/0'/1").chainCode)).toBe("2a7857631386ba23dacac34180dd1983734e444fdbf774041578e9b6adb37c19");
    const parent = at("m/0'");
    const pub = expectOk(bip32Secp256k1.publicChild(secp256k1Ecdsa.publicKey(parent.key), parent.chainCode, expectOk(ChildIndex.soft(1), "1")), "pub");
    expect(toHex(pub.key.bytes)).toBe("03501e454bf00751f24b1b489aa925215d66af2234e3891c3b21a52bedb3cd711c");
    expect(toHex(pub.key.bytes)).toBe(toHex(secp256k1Ecdsa.publicKey(at("m/0'/1").key).bytes));
    expect(toHex(pub.chainCode)).toBe(toHex(at("m/0'/1").chainCode));
    expect(kind(bip32Secp256k1.publicChild(secp256k1Ecdsa.publicKey(parent.key), parent.chainCode, expectOk(ChildIndex.hardened(1), "1'")))).toBe(
      "InvalidDerivation",
    );
    // HMAC takes a key of any length, so a chain code of the wrong length must be refused before it is used
    for (const cc of [new Uint8Array(0), parent.chainCode.subarray(0, 31), new Uint8Array(33)])
      expect(kind(bip32Secp256k1.publicChild(secp256k1Ecdsa.publicKey(parent.key), cc, expectOk(ChildIndex.soft(1), "1"))), String(cc.length)).toBe("InvalidDerivation");
  });
});

describe("BIP32-Ed25519 (Icarus)", { timeout: 60_000 }, () => {
  test("signing_v1.json ed25519Bip32: root, account, address, keys, signatures and public derivation", () => {
    const v = vectors.ed25519Bip32;
    const root = expectOk(icarus.fromEntropy(h(v.entropy), v.passphrase), "root");
    expect(toHex(root.key.bytes)).toBe(v.root.key);
    expect(toHex(root.chainCode)).toBe(v.root.chainCode);
    const account = expectOk(icarus.derive(root, path(v.accountPath)), "account");
    expect(toHex(account.key.bytes)).toBe(v.account.key);
    expect(toHex(account.chainCode)).toBe(v.account.chainCode);
    expect(toHex(ed25519Bip32.publicKey(account.key).bytes)).toBe(v.accountVerifyingKey);
    const address = expectOk(icarus.derive(account, path(v.addressPath)), "address");
    expect(toHex(address.key.bytes)).toBe(v.address.key);
    expect(toHex(address.chainCode)).toBe(v.address.chainCode);
    const addressVk = ed25519Bip32.publicKey(address.key);
    expect(toHex(addressVk.bytes)).toBe(v.addressVerifyingKey);
    expect(v.signatures.length).toBe(3);
    for (const s of v.signatures) {
      expect(toHex(ed25519Bip32.sign(address.key, h(s.message)).bytes)).toBe(s.signature);
      expect(ed25519Bip32.verifyBytes(addressVk.bytes, h(s.message), h(s.signature))).toEqual({ ok: true, value: true });
    }
    let acc = { key: ed25519Bip32.publicKey(account.key), chainCode: account.chainCode };
    for (const i of path(v.addressPath)) acc = expectOk(icarus.publicChild(acc.key, acc.chainCode, i), "public child");
    expect(toHex(acc.key.bytes)).toBe(v.addressVerifyingKey);
    expect(toHex(acc.chainCode)).toBe(v.address.chainCode);
    const soft0 = expectOk(ChildIndex.soft(0), "0");
    for (const cc of [new Uint8Array(0), account.chainCode.subarray(0, 31), new Uint8Array(33)])
      expect(kind(icarus.publicChild(ed25519Bip32.publicKey(account.key), cc, soft0)), String(cc.length)).toBe("InvalidDerivation");
  });

  test("child key derivation (Brambl Bip32Ed25519Ckd.json), private and public", () => {
    interface Ckd {
      inputs: { rootSecretKey: string; path: [string, number][] };
      outputs: { childSecretKey: string; childVerificationKey: string };
    }
    const vs = JSON.parse(readDoc("Bip32Ed25519Ckd.json")) as Ckd[];
    expect(vs.length).toBe(10);
    for (const v of vs) {
      const rootBytes = h(v.inputs.rootSecretKey);
      const root = expectOk(ExtendedKey.of(expectOk(ed25519Bip32.signingKey(rootBytes.subarray(0, 64)), "root"), rootBytes.subarray(64)), "root");
      const p = v.inputs.path.map(([k, i]) => expectOk(k === "hard" ? ChildIndex.hardened(i) : ChildIndex.soft(i), "index"));
      const child = expectOk(icarus.derive(root, p), "child");
      expect(toHex(child.key.bytes) + toHex(child.chainCode)).toBe(v.outputs.childSecretKey);
      expect(toHex(ed25519Bip32.publicKey(child.key).bytes) + toHex(child.chainCode)).toBe(v.outputs.childVerificationKey);
      // public derivation from the deepest hardened ancestor through the trailing soft indices
      const cut = p.map((i) => i.hardened).lastIndexOf(true) + 1;
      const anchor = expectOk(icarus.derive(root, p.slice(0, cut)), "anchor");
      let acc = { key: ed25519Bip32.publicKey(anchor.key), chainCode: anchor.chainCode };
      for (const i of p.slice(cut)) acc = expectOk(icarus.publicChild(acc.key, acc.chainCode, i), "public");
      expect(toHex(acc.key.bytes) + toHex(acc.chainCode)).toBe(v.outputs.childVerificationKey);
    }
  });

  test("Icarus master keys from mnemonics (Brambl Bip32Ed25519Icarus.json) and the CIP-0003 reference", () => {
    const vs = JSON.parse(readDoc("Bip32Ed25519Icarus.json")) as { inputs: { mnemonic: string; password: string }; outputs: { extendedEd25519: string } }[];
    expect(vs.length).toBe(32);
    for (const v of vs) {
      const entropy = expectOk(toEntropy(v.inputs.mnemonic), "entropy").entropy;
      const key = expectOk(icarus.fromEntropy(entropy, v.inputs.password), "icarus");
      expect(toHex(key.key.bytes) + toHex(key.chainCode)).toBe(v.outputs.extendedEd25519);
    }
    const cip3 = expectOk(icarus.fromEntropy(expectOk(toEntropy("eight country switch draw meat scout mystery blade tip drift useless good keep usage title"), "e").entropy), "cip3");
    expect(toHex(cip3.key.bytes) + toHex(cip3.chainCode)).toBe(
      "c065afd2832cd8b087c4d9ab7011f481ee1e0721e78ea5dd609f3ab3f156d245d176bd8fd4ec60b4731c3918a2a72a0226c0cd119ec35b47e4d55884667f552a23f7fdcd4a10c6cd2c7393ac61d877873e248f417634aa3d812af327ffe9d620",
    );
  });

  test("hardened public derivation, non-canonical keys, bad roots and unclamped keys are rejected", () => {
    const root = expectOk(icarus.fromEntropy(new Uint8Array(16)), "root");
    const vk = ed25519Bip32.publicKey(root.key);
    expect(kind(icarus.publicChild(vk, root.chainCode, expectOk(ChildIndex.hardened(0), "0'")))).toBe("InvalidDerivation");
    // y = p + 1 decodes (to the identity) under ZIP-215 but is not canonical
    const nonCanonical = expectOk(ed25519Bip32.verifyingKey(h("eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f")), "vk");
    expect(kind(icarus.publicChild(nonCanonical, root.chainCode, expectOk(ChildIndex.soft(0), "0")))).toBe("InvalidDerivation");
    expect(kind(icarus.root(new Uint8Array(95)))).toBe("InvalidDerivation");
    const raw = new Uint8Array(96).fill(0xff);
    const clamped = expectOk(icarus.root(raw), "clamped");
    expect(raw[0]).toBe(0xff);
    expect(toHex(clamped.key.bytes.subarray(0, 1)) + toHex(clamped.key.bytes.subarray(31, 32))).toBe("f85f");
    expect(kind(ExtendedKey.of(root.key, new Uint8Array(31)))).toBe("InvalidDerivation");
    expect(String(root)).not.toContain(toHex(root.chainCode));
  });
});
