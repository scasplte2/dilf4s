package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.kd5ujc.dilf4s.V.*;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.text.Normalizer;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import xyz.kd5ujc.dilf4s.error.CryptoError;
import xyz.kd5ujc.dilf4s.error.KesError;
import xyz.kd5ujc.dilf4s.error.KeyError;
import xyz.kd5ujc.dilf4s.error.SigmaError;
import xyz.kd5ujc.dilf4s.error.VrfError;

/**
 * The signing, VRF, KES and sigma vectors through the Java API: every row of every scheme this layer has. The
 * secp256k1 and BLS12-381 rows are counted and left out (their backends load only in an effect, see {@link Signing}).
 */
public class SigningVectorsTest {

  private static Value.Map signing;

  @BeforeAll
  static void load() throws IOException {
    signing = file("signing_v1.json");
  }

  private interface Verify {
    Result<Boolean, CryptoError> apply(byte[] key, byte[] message, byte[] signature);
  }

  private static Map<String, Verify> verifiers() {
    final Backends b = V.backends();
    return Map.of(
      "Ed25519", Ed25519::verify,
      "Ed25519-libsodium", Ed25519::verifyLibsodium,
      "P-256-ECDSA", P256::verify,
      "ristretto255-Schnorr", Ristretto255::verify,
      "secp256k1-ECDSA", (k, m, s) -> Secp256k1.ecdsaVerify(b, k, m, s),
      "BIP-340", (k, m, s) -> Secp256k1.bip340Verify(b, k, m, s),
      "secp256k1-tagged-Schnorr", (k, m, s) -> Secp256k1.taggedVerify(b, k, m, s),
      "BLS12-381-PoP", (k, m, s) -> Bls.verify(b, k, m, s)
    );
  }

  @Test
  void verification() {
    final Map<String, Verify> verifiers = verifiers();
    final List<Value.Map> rows = rows(signing, "verification");
    final List<Value.Map> ours = rows.stream().filter(r -> verifiers.containsKey(str(r, "scheme"))).toList();
    assertEquals(453, rows.size());
    assertEquals(rows.size(), ours.size(), "every row is a scheme here");
    assertAll(ours.stream().map(r -> () -> {
      final String what = str(r, "scheme") + " " + str(r, "name");
      final Result<Boolean, CryptoError> got = verifiers.get(str(r, "scheme")).apply(hexAt(r, "verifyingKey"), hexAt(r, "message"), hexAt(r, "signature"));
      final Value result = get(r, "result");
      if (result instanceof Value.Text t) {
        assertEquals(t.value().equals("valid"), ok(got, what), what);
      } else {
        assertEquals(str(map(result), "kind"), err(got, what).kind(), what);
      }
    }));
  }

  private record Scheme(Function<byte[], Result<Signing.KeyPair, CryptoError>> fromSeed, BiFunction<byte[], byte[], Result<byte[], CryptoError>> sign) {}

  @Test
  void signing() {
    final byte[] randomness = hexAt(signing, "randomness");
    final Backends b = V.backends();
    final Map<String, Scheme> schemes = Map.of(
      "Ed25519", new Scheme(Ed25519::fromSeed, Ed25519::sign),
      "P-256-ECDSA", new Scheme(P256::fromSeed, P256::sign),
      "ristretto255-Schnorr", new Scheme(Ristretto255::fromSeed, (k, m) -> Ristretto255.sign(k, m, randomness)),
      "secp256k1-ECDSA", new Scheme(x -> Secp256k1.ecdsaFromSeed(b, x), (k, m) -> Secp256k1.ecdsaSign(b, k, m)),
      "BIP-340", new Scheme(x -> Secp256k1.schnorrFromSeed(b, x), (k, m) -> Secp256k1.bip340Sign(b, k, m, randomness)),
      "secp256k1-tagged-Schnorr", new Scheme(x -> Secp256k1.schnorrFromSeed(b, x), (k, m) -> Secp256k1.taggedSign(b, k, m, randomness)),
      "BLS12-381-PoP", new Scheme(x -> Bls.fromSeed(b, x), (k, m) -> Bls.sign(b, k, m))
    );
    final List<Value.Map> rows = rows(signing, "signing").stream().filter(r -> schemes.containsKey(str(r, "scheme"))).toList();
    assertEquals(7, rows.size());
    assertAll(rows.stream().flatMap(r -> rows(r, "keys").stream().map(k -> (Executable) () -> {
      final Scheme s = schemes.get(str(r, "scheme"));
      final Signing.KeyPair pair = ok(s.fromSeed().apply(hexAt(k, "seed")), str(r, "scheme"));
      same(hexAt(k, "signingKey"), pair.signingKey(), str(r, "scheme") + ": the signing key");
      same(hexAt(k, "verifyingKey"), pair.verifyingKey(), str(r, "scheme") + ": the verifying key");
      assertAll(rows(k, "signatures").stream().map(sig -> () ->
        same(hexAt(sig, "signature"), ok(s.sign().apply(pair.signingKey(), hexAt(sig, "message")), "sign"), str(r, "scheme") + ": a signature")));
    })));
  }

  private static Bip39.WordList english() throws IOException {
    try (InputStream in = Bip39.class.getResourceAsStream("/bip-0039/english.txt")) {
      final List<String> words = Arrays.stream(new String(in.readAllBytes(), StandardCharsets.UTF_8).split("\n"))
        .map(w -> Normalizer.normalize(w.trim(), Normalizer.Form.NFKD))
        .filter(w -> !w.isEmpty())
        .toList();
      return ok(Bip39.wordList(Bip39.Language.ENGLISH, words), "english");
    }
  }

  @Test
  void bip39() throws IOException {
    final Bip39.WordList words = english();
    final List<Value.Map> rows = rows(signing, "bip39");
    // a floor, so rows another change adds never break this test
    assertTrue(rows.size() >= 7, rows.size() + " bip39 rows");
    assertAll(rows.stream().map(r -> () -> {
      final Bip39.Mnemonic m = ok(Bip39.toMnemonic(hexAt(r, "entropy"), words), "mnemonic");
      assertEquals(str(r, "mnemonic"), m.phrase(), "the phrase");
      final Bip39.Decoded d = ok(Bip39.toEntropy(str(r, "mnemonic"), words), "entropy");
      same(hexAt(r, "entropy"), d.entropy(), "the entropy");
      same(hexAt(r, "seed"), ok(Bip39.seed(m, str(r, "passphrase")), "seed"), "the seed");
    }));
  }

  /** The keyRefusals rows of BIP-39 phrases and passphrases holding a code point Unicode 15.0 does not assign. */
  @Test
  void bip39Refusals() throws IOException {
    final Bip39.WordList words = english();
    final List<Value.Map> rows = rows(signing, "keyRefusals").stream().filter(r -> str(r, "check").startsWith("bip39-")).toList();
    assertTrue(rows.stream().anyMatch(r -> str(r, "check").equals("bip39-seed")), "bip39-seed rows");
    assertAll(rows.stream().map(r -> () -> {
      final Result<?, KeyError> got = switch (str(r, "check")) {
        case "bip39-entropy" -> Bip39.toMnemonic(hexAt(r, "entropy"), words);
        case "bip39-phrase" -> Bip39.toEntropy(str(r, "phrase"), words);
        case "bip39-seed" -> Bip39.toEntropy(str(r, "phrase"), words).flatMap(d -> Bip39.seed(d.mnemonic(), str(r, "passphrase")));
        default -> throw new AssertionError("unknown check " + str(r, "check"));
      };
      assertEquals(Optional.of(str(r, "kind")), got.failure().map(KeyError::kind), str(r, "check") + " " + str(r, "name"));
    }));
    // a lone surrogate, which JSON vectors cannot carry
    final Bip39.Mnemonic m = ok(Bip39.toMnemonic(new byte[16], words), "mnemonic");
    assertEquals(Optional.of("InvalidPassphrase"), Bip39.seed(m, "dilf4s\ud800").failure().map(KeyError::kind), "a lone surrogate");
  }

  /** Icarus's secret, PBKDF2-HMAC-SHA512(passphrase, entropy, 4096, 96), by BouncyCastle's generator (the caller's part). */
  static byte[] icarusSecret(byte[] entropy, String passphrase) {
    final org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator g =
      new org.bouncycastle.crypto.generators.PKCS5S2ParametersGenerator(new org.bouncycastle.crypto.digests.SHA512Digest());
    g.init(passphrase.getBytes(StandardCharsets.UTF_8), entropy, 4096);
    return ((org.bouncycastle.crypto.params.KeyParameter) g.generateDerivedParameters(96 * 8)).getKey();
  }

  @Test
  void derivation() {
    final Value.Map slip = map(get(signing, "slip10Ed25519"));
    final Signing.ExtendedKey child = ok(Signing.slip10Ed25519(hexAt(slip, "seed"), ok(Signing.path(str(slip, "path")), "path")), "slip10");
    final Value.Map expected = map(get(slip, "child"));
    same(hexAt(expected, "key"), child.key(), "SLIP-10 child key");
    same(hexAt(expected, "chainCode"), child.chainCode(), "SLIP-10 child chain code");

    final Value.Map icarus = map(get(signing, "ed25519Bip32"));
    final Signing.ExtendedKey root = ok(Icarus.root(icarusSecret(hexAt(icarus, "entropy"), str(icarus, "passphrase"))), "icarus root");
    same(hexAt(map(get(icarus, "root")), "key"), root.key(), "Icarus root key");
    final Signing.ExtendedKey account = ok(Icarus.derive(root, ok(Signing.path(str(icarus, "accountPath")), "path")), "account");
    same(hexAt(map(get(icarus, "account")), "key"), account.key(), "Icarus account key");
    same(hexAt(icarus, "accountVerifyingKey"), ok(Icarus.publicKey(account.key()), "pk"), "Icarus account verifying key");

    final List<Value.Map> paths = rows(signing, "paths");
    assertEquals(54, paths.size());
    assertAll(paths.stream().map(p -> () -> {
      final Value result = get(p, "result");
      final var parsed = Signing.path(str(p, "path"));
      if (result instanceof Value.Map indices) {
        final List<Long> serialized = ok(parsed, str(p, "path")).stream().map(i -> i.hardened() ? i.value() | 0x80000000L : i.value()).toList();
        assertEquals(items(get(indices, "indices")).stream().map(i -> big(i).longValueExact()).toList(), serialized, str(p, "path"));
      } else {
        final var e = err(parsed, str(p, "path"));
        assertTrue(e instanceof xyz.kd5ujc.dilf4s.error.KeyError.InvalidPath ip && ip.refusal().wire().equals(text(result)), str(p, "path") + ": " + e);
      }
    }));
  }

  @Test
  void bip32AndBlsPop() {
    final Backends b = V.backends();
    final Value.Map bip32 = map(get(signing, "bip32Secp256k1"));
    final Signing.ExtendedKey child = ok(Secp256k1.bip32(b, hexAt(bip32, "seed"), ok(Signing.path(str(bip32, "path")), "path")), "bip32");
    final Signing.ExtendedKey master = ok(Secp256k1.bip32(b, hexAt(bip32, "seed"), List.of()), "master");
    final Value.Map pub = map(get(bip32, "publicDerivation"));
    final List<Signing.ChildIndex> soft = ok(Signing.path(str(pub, "path")), "public path");
    final Signing.PublicChild derived = soft.stream().reduce(
      new Signing.PublicChild(hexAt(pub, "parentVerifyingKey"), hexAt(pub, "parentChainCode")),
      (parent, i) -> ok(Secp256k1.bip32PublicChild(b, parent.verifyingKey(), parent.chainCode(), i), "public child"),
      (x, y) -> y
    );
    final List<Value.Map> pops = rows(signing, "blsPop");
    assertAll(Stream.concat(
      Stream.of(
        () -> same(hexAt(map(get(bip32, "master")), "key"), master.key(), "the BIP-32 master key"),
        () -> same(hexAt(map(get(bip32, "master")), "chainCode"), master.chainCode(), "the BIP-32 master chain code"),
        () -> same(hexAt(map(get(bip32, "child")), "key"), child.key(), "the BIP-32 child key"),
        () -> same(hexAt(map(get(bip32, "child")), "chainCode"), child.chainCode(), "the BIP-32 child chain code"),
        () -> same(hexAt(bip32, "verifyingKey"), ok(Secp256k1.ecdsaPublicKey(b, child.key()), "pk"), "the child's verifying key"),
        () -> same(hexAt(pub, "verifyingKey"), derived.verifyingKey(), "the public derivation's key"),
        () -> same(hexAt(pub, "chainCode"), derived.chainCode(), "the public derivation's chain code"),
        () -> assertEquals(3, pops.size(), "the PoP rows")
      ),
      pops.stream().map(r -> (Executable) () -> {
        same(hexAt(r, "verifyingKey"), ok(Bls.publicKey(b, hexAt(r, "signingKey")), "pk"), "the BLS verifying key");
        same(hexAt(r, "proof"), ok(Bls.popProve(b, hexAt(r, "signingKey")), "pop"), "the proof of possession");
        assertEquals(true, ok(Bls.popVerify(b, hexAt(r, "verifyingKey"), hexAt(r, "proof")), "pop verify"), "the proof verifies");
      })
    ));
  }

  @Test
  void vrf() throws IOException {
    final Value.Map vrf = file("vrf_v1.json");
    final Map<String, Vrf.Suite> suites = Map.of(
      Vrf.Suite.TAI.wire(), Vrf.Suite.TAI, Vrf.Suite.ELL2.wire(), Vrf.Suite.ELL2, Vrf.Suite.DRAFT03.wire(), Vrf.Suite.DRAFT03);
    final List<Value.Map> rejections = rows(vrf, "rejections");
    assertAll(Stream.concat(
      rows(vrf, "suites").stream().flatMap(s -> rows(s, "keys").stream().flatMap(k -> rows(k, "proofs").stream().map(p -> (Executable) () -> {
        final Vrf.Suite suite = suites.get(str(s, "suite"));
        final Signing.KeyPair pair = ok(Vrf.fromSeed(suite, hexAt(k, "seed")), "seed");
        same(hexAt(k, "verifyingKey"), pair.verifyingKey(), "the verifying key");
        final Vrf.Proof proof = ok(Vrf.prove(suite, pair.signingKey(), hexAt(p, "alpha")), "prove");
        same(hexAt(p, "pi"), proof.pi(), "pi");
        same(hexAt(p, "beta"), proof.beta(), "beta");
        same(hexAt(p, "beta"), ok(Vrf.verify(suite, pair.verifyingKey(), hexAt(p, "alpha"), proof.pi()), "verify"), "verified beta");
      }))),
      rejections.stream().map(r -> (Executable) () -> {
        final Result<byte[], VrfError> got = Vrf.verify(suites.get(str(r, "suite")), hexAt(r, "verifyingKey"), hexAt(r, "alpha"), hexAt(r, "pi"));
        final String expected = str(r, "result");
        assertEquals(expected, got.isOk() ? "valid" : err(got, "vrf").kind(), str(r, "suite") + " " + str(r, "name"));
      })
    ));
    assertEquals(33, rejections.size());
  }

  @Test
  void kes() throws IOException {
    final Value.Map kes = file("kes_v1.json");
    final List<Value.Map> vectors = rows(kes, "vectors");
    assertEquals(4, vectors.size());
    assertAll(vectors.stream().map(v -> () -> {
      final Kes.Construction c = construction(str(v, "construction"), map(get(v, "parameters")));
      final Kes.Key key = ok(Kes.generate(c, hexAt(v, "seed")), "generate");
      same(hexAt(v, "verifyingKey"), key.verifyingKey(), str(v, "construction") + ": the verifying key");
      assertAll(rows(v, "periods").stream().map(p -> () -> {
        final Kes.Key at = ok(Kes.load(c, hexAt(p, "key"), num(p, "period")), "load");
        final byte[] sig = ok(Kes.sign(c, at, hexAt(v, "message")), "sign");
        same(hexAt(p, "signature"), sig, str(v, "construction") + ": the signature at " + num(p, "period"));
        assertEquals(true, ok(Kes.verify(c, key.verifyingKey(), num(p, "period"), hexAt(v, "message"), sig), "verify"), "verifies");
      }));
    }));
    final List<Value.Map> parameters = rows(kes, "refusals").stream().filter(r -> str(r, "check").equals("parameters")).toList();
    assertAll(parameters.stream().map(r -> () -> {
      final Result<Kes.Shape, KesError> shape = Kes.shape(construction(str(r, "construction"), map(get(r, "parameters"))));
      assertEquals(str(r, "outcome"), shape.isOk() ? "accepted" : err(shape, "shape").kind(), str(r, "name"));
    }));
  }

  private static Kes.Construction construction(String name, Value.Map p) {
    return switch (name) {
      case "dilf4s-sum" -> new Kes.Construction.Sum(ok(Hashing.algorithm(str(p, "hash")), "hash"), (int) num(p, "height"));
      case "dilf4s-product" -> new Kes.Construction.Product(ok(Hashing.algorithm(str(p, "hash")), "hash"), (int) num(p, "superHeight"), (int) num(p, "subHeight"));
      case "cardano-sum" -> new Kes.Construction.CardanoSum((int) num(p, "depth"));
      default -> org.junit.jupiter.api.Assertions.fail("no construction " + name);
    };
  }

  @Test
  void sigma() throws IOException {
    final Value.Map refusals = file("sigma_refusals_v1.json");
    final List<Value.Map> rows = rows(refusals, "cases");
    assertEquals(20, rows.size());
    assertAll(rows.stream().map(r -> () -> {
      final Result<Boolean, SigmaError> got = Sigma.verify(cbor(str(r, "proposition")), hexAt(r, "proof"), hexAt(r, "message"));
      final String outcome = str(r, "outcome");
      final String actual = got.fold(SigmaError::kind, b -> b ? "accepted" : "rejected");
      assertEquals(outcome, actual, str(r, "name"));
    }));
  }
}
