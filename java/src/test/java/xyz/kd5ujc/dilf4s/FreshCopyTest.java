package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.RecordComponent;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

/**
 * Every {@code byte[]} out of the API is a fresh array, and every one in is copied: Java arrays alias, so a caller
 * that changes an array it passed or got changes nothing the API holds. Every method whose signature returns bytes is
 * listed here and checked, so a new one fails until it is.
 */
public class FreshCopyTest {

  private static boolean bytesIn(Type t) {
    return switch (t) {
      case Class<?> c -> c == byte[].class;
      case ParameterizedType p -> Arrays.stream(p.getActualTypeArguments()).anyMatch(FreshCopyTest::bytesIn);
      default -> false;
    };
  }

  /** {@code supply} twice: distinct arrays of the same bytes, and changing one changes neither the other nor a third. */
  private static Executable fresh(String what, Supplier<byte[]> supply) {
    return () -> {
      final byte[] a = supply.get();
      final byte[] b = supply.get();
      assertNotSame(a, b, what + ": a fresh array each time");
      assertArrayEquals(a, b, what);
      final byte[] before = a.clone();
      Arrays.fill(a, (byte) 0x5a);
      assertArrayEquals(before, supply.get(), what + ": changing a returned array changes nothing");
    };
  }

  private static byte[] sha(byte[] b) {
    return V.ok(Hashing.digest(Hashing.Algorithm.SHA_256, b), "digest").bytes();
  }

  private static final Set<String> COVERED = Set.of(
    "Cbor.encode", "Cbor.encodeWithin", "ValueCommitment.bytes", "ValueCommitment.bytesWithin", "Hashing.expandMessageXmd",
    "Hashing$Digest.bytes", "Value$Bytes.bytes", "Ed25519.sign", "Ed25519.publicKey", "Ed25519.checkVerifyingKey",
    "P256.sign", "P256.publicKey", "P256.checkVerifyingKey", "Ristretto255.sign", "Ristretto255.publicKey",
    "Ristretto255.checkVerifyingKey", "Icarus.sign", "Icarus.publicKey", "Ecrecover.recover", "Ecrecover.address",
    "Vrf.verify", "Vrf.publicKey", "Vrf.checkVerifyingKey", "Kes.sign", "Merkle.leafHash", "Merkle.valueLeafHash", "Merkle.root",
    "Trie.valueHash", "Trie.root", "VersionedTrie.versionKey", "VersionedTrie.verifyVersion", "VersionedTrie.verifyAsOf",
    "VersionedTrie$State.lookup", "VersionedTrie$State.namespace", "VersionedTrie$State.range", "Zk.add", "Zk.mul", "Jlvm.encode",
    "Secp256k1.ecdsaSign", "Secp256k1.ecdsaPublicKey", "Secp256k1.ecdsaCheckVerifyingKey", "Secp256k1.bip340Sign",
    "Secp256k1.schnorrPublicKey", "Secp256k1.schnorrCheckVerifyingKey", "Secp256k1.taggedSign", "Bls.sign", "Bls.publicKey",
    "Bls.checkVerifyingKey", "Bls.aggregate", "Bls.popProve", "Bls.eip2333Master", "Bls.eip2333Child", "Bip39.seed"
  );

  @Test
  void everyMethodReturningBytesIsChecked() throws Exception {
    final List<String> returning = Api.publicClasses().stream()
      .filter(c -> !c.isRecord())
      .flatMap(c -> Arrays.stream(c.getDeclaredMethods())
        .filter(m -> java.lang.reflect.Modifier.isPublic(m.getModifiers()) && !m.isSynthetic() && bytesIn(m.getGenericReturnType()))
        .map(m -> c.getName().replace("xyz.kd5ujc.dilf4s.", "") + "." + m.getName()))
      .distinct()
      .sorted()
      .toList();
    assertEquals(COVERED.stream().sorted().toList(), returning, "the methods returning bytes, each checked below");
  }

  @Test
  void methodsReturnFreshArrays() {
    final Value bytes = V.bytesValue(new byte[] {1, 2, 3});
    final byte[] seed = new byte[32];
    final Signing.KeyPair ed = V.ok(Ed25519.fromSeed(seed), "ed");
    final Signing.KeyPair p256 = V.ok(P256.fromSeed(sha(seed)), "p256");
    final Signing.KeyPair rist = V.ok(Ristretto255.fromSeed(seed), "rist");
    final Signing.ExtendedKey icarus = V.ok(Icarus.root(new byte[96]), "icarus");
    final Kes.Construction kes = new Kes.Construction.Sum(Hashing.Algorithm.BLAKE2B_256, 1);
    final Kes.Key kesKey = V.ok(Kes.generate(kes, seed), "kes");
    final Trie.KeyMode mode = V.ok(Trie.fixed(1), "mode");
    final byte[] g1 = V.hex("0000000000000000000000000000000000000000000000000000000000000001" + "0000000000000000000000000000000000000000000000000000000000000002");
    // r is the generator's x-coordinate, s is 1: a signature that recovers a key for any digest but 1
    final byte[] sig65 = V.hex("79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798" + "00".repeat(31) + "01" + "1b");
    final byte[] digest = V.hex("00".repeat(31) + "02");
    // any 2048 distinct words make a list
    final Bip39.WordList words = V.ok(Bip39.wordList(Bip39.Language.ENGLISH, java.util.stream.IntStream.range(0, 2048).mapToObj(i -> "w" + i).toList()), "words");
    final Bip39.Mnemonic mnemonic = V.ok(Bip39.toMnemonic(new byte[16], words), "mnemonic");
    assertAll(
      fresh("Bip39.seed", () -> V.ok(Bip39.seed(mnemonic, ""), "x")),
      fresh("Cbor.encode", () -> V.ok(Cbor.encode(bytes), "x")),
      fresh("Cbor.encodeWithin", () -> V.ok(Cbor.encodeWithin(bytes, 100), "x")),
      fresh("ValueCommitment.bytes", () -> V.ok(ValueCommitment.bytes(bytes), "x")),
      fresh("ValueCommitment.bytesWithin", () -> V.ok(ValueCommitment.bytesWithin(bytes, 100), "x")),
      fresh("Hashing.expandMessageXmd", () -> V.ok(Hashing.expandMessageXmd(Hashing.Algorithm.SHA_256, new byte[0], new byte[] {1}, 16), "x")),
      fresh("Digest.bytes", () -> sha(new byte[0])),
      fresh("Digest.bytes, one digest", ((Supplier<Supplier<byte[]>>) () -> {
        final Hashing.Digest d = V.ok(Hashing.digest(Hashing.Algorithm.SHA_256, new byte[0]), "d");
        return d::bytes;
      }).get()),
      fresh("Value.Bytes.bytes", ((Value.Bytes) bytes)::bytes),
      fresh("Ed25519.sign", () -> V.ok(Ed25519.sign(ed.signingKey(), new byte[0]), "x")),
      fresh("Ed25519.publicKey", () -> V.ok(Ed25519.publicKey(ed.signingKey()), "x")),
      fresh("Ed25519.checkVerifyingKey", () -> V.ok(Ed25519.checkVerifyingKey(ed.verifyingKey()), "x")),
      fresh("P256.sign", () -> V.ok(P256.sign(p256.signingKey(), new byte[0]), "x")),
      fresh("P256.publicKey", () -> V.ok(P256.publicKey(p256.signingKey()), "x")),
      fresh("P256.checkVerifyingKey", () -> V.ok(P256.checkVerifyingKey(p256.verifyingKey()), "x")),
      fresh("Ristretto255.sign", () -> V.ok(Ristretto255.sign(rist.signingKey(), new byte[0], new byte[32]), "x")),
      fresh("Ristretto255.publicKey", () -> V.ok(Ristretto255.publicKey(rist.signingKey()), "x")),
      fresh("Ristretto255.checkVerifyingKey", () -> V.ok(Ristretto255.checkVerifyingKey(rist.verifyingKey()), "x")),
      fresh("Icarus.sign", () -> V.ok(Icarus.sign(icarus.key(), new byte[0]), "x")),
      fresh("Icarus.publicKey", () -> V.ok(Icarus.publicKey(icarus.key()), "x")),
      fresh("Ecrecover.address", () -> V.ok(Ecrecover.address(V.hex("04" + "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798" + "483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8")), "x")),
      () -> assertTrue(V.ok(Ecrecover.recover(digest, sig65), "x").isPresent(), "the signature recovers a key"),
      fresh("Ecrecover.recover", () -> V.ok(Ecrecover.recover(digest, sig65), "x").orElseGet(() -> new byte[0])),
      fresh("Vrf.publicKey", () -> V.ok(Vrf.publicKey(Vrf.Suite.TAI, seed), "x")),
      fresh("Vrf.checkVerifyingKey", () -> V.ok(Vrf.checkVerifyingKey(Vrf.Suite.TAI, ed.verifyingKey()), "x")),
      fresh("Vrf.verify", () -> V.ok(Vrf.verify(Vrf.Suite.TAI, ed.verifyingKey(), new byte[0], V.ok(Vrf.prove(Vrf.Suite.TAI, seed, new byte[0]), "pi").pi()), "x")),
      fresh("Kes.sign", () -> V.ok(Kes.sign(kes, kesKey, new byte[0]), "x")),
      fresh("Merkle.leafHash", () -> V.ok(Merkle.leafHash(Merkle.Profile.DILF4S, Hashing.Algorithm.SHA_256, new byte[0]), "x")),
      fresh("Merkle.valueLeafHash", () -> V.ok(Merkle.valueLeafHash(Hashing.Algorithm.SHA_256, bytes), "x")),
      fresh("Merkle.root", () -> V.ok(Merkle.root(Merkle.Profile.RFC6962, Hashing.Algorithm.SHA_256, List.of(sha(new byte[0]))), "x")),
      fresh("Trie.valueHash", () -> V.ok(Trie.valueHash(Hashing.Algorithm.SHA_256, bytes), "x")),
      fresh("Trie.root", () -> V.ok(Trie.root(Hashing.Algorithm.SHA_256, mode, List.of(Map.entry(new byte[] {1}, sha(new byte[0])))), "x")),
      fresh("VersionedTrie.versionKey", () -> V.ok(VersionedTrie.versionKey(8, 1L), "x")),
      fresh("Zk.add", () -> V.ok(Zk.add(g1, g1), "x")),
      fresh("Zk.mul", () -> V.ok(Zk.mul(g1, new byte[32]), "x")),
      fresh("Jlvm.encode", () -> V.ok(Jlvm.encode(V.ok(Jlvm.parse("1"), "p")), "x"))
    );
  }

  @Test
  void nativeBackendsReturnFreshArrays() {
    final Backends b = V.backends();
    final byte[] seed = new byte[32];
    seed[31] = 1;
    final Signing.KeyPair ecdsa = V.ok(Secp256k1.ecdsaFromSeed(b, seed), "ecdsa");
    final Signing.KeyPair schnorr = V.ok(Secp256k1.schnorrFromSeed(b, seed), "schnorr");
    final Signing.KeyPair bls = V.ok(Bls.fromSeed(b, seed), "bls");
    final byte[] blsSig = V.ok(Bls.sign(b, bls.signingKey(), new byte[0]), "bls sig");
    assertAll(
      fresh("Secp256k1.ecdsaSign", () -> V.ok(Secp256k1.ecdsaSign(b, ecdsa.signingKey(), new byte[0]), "x")),
      fresh("Secp256k1.ecdsaPublicKey", () -> V.ok(Secp256k1.ecdsaPublicKey(b, ecdsa.signingKey()), "x")),
      fresh("Secp256k1.ecdsaCheckVerifyingKey", () -> V.ok(Secp256k1.ecdsaCheckVerifyingKey(b, ecdsa.verifyingKey()), "x")),
      fresh("Secp256k1.bip340Sign", () -> V.ok(Secp256k1.bip340Sign(b, schnorr.signingKey(), new byte[32], new byte[32]), "x")),
      fresh("Secp256k1.schnorrPublicKey", () -> V.ok(Secp256k1.schnorrPublicKey(b, schnorr.signingKey()), "x")),
      fresh("Secp256k1.schnorrCheckVerifyingKey", () -> V.ok(Secp256k1.schnorrCheckVerifyingKey(b, schnorr.verifyingKey()), "x")),
      fresh("Secp256k1.taggedSign", () -> V.ok(Secp256k1.taggedSign(b, schnorr.signingKey(), new byte[0], new byte[32]), "x")),
      fresh("Bls.sign", () -> V.ok(Bls.sign(b, bls.signingKey(), new byte[0]), "x")),
      fresh("Bls.publicKey", () -> V.ok(Bls.publicKey(b, bls.signingKey()), "x")),
      fresh("Bls.checkVerifyingKey", () -> V.ok(Bls.checkVerifyingKey(b, bls.verifyingKey()), "x")),
      fresh("Bls.aggregate", () -> V.ok(Bls.aggregate(b, List.of(blsSig, blsSig)), "x")),
      fresh("Bls.popProve", () -> V.ok(Bls.popProve(b, bls.signingKey()), "x")),
      fresh("Bls.eip2333Master", () -> V.ok(Bls.eip2333Master(b, seed), "x")),
      fresh("Bls.eip2333Child", () -> V.ok(Bls.eip2333Child(b, V.ok(Bls.eip2333Master(b, seed), "m"), 0L), "x"))
    );
  }

  @Test
  void versionedTrieReturnsFreshArrays() throws java.io.IOException {
    final Value.Map v = V.file("versioned_trie_v1.json");
    final byte[] root = V.hexAt(v, "root");
    final List<Value.Map> proofs = V.rows(v, "proofs");
    final Value state = V.cbor(V.str(proofs.get(0), "cbor"));
    final Value namespace = V.cbor(V.str(proofs.get(1), "cbor"));
    final Value version = V.cbor(V.str(proofs.get(3), "cbor"));
    final Value asOf = V.cbor(V.str(proofs.get(5), "cbor"));
    final Hashing.Algorithm sha = Hashing.Algorithm.SHA_256;
    final VersionedTrie.State s = V.ok(VersionedTrie.verifyState(8, sha, root, state), "state");
    final VersionedTrie.State n = V.ok(VersionedTrie.verifyState(8, sha, root, namespace), "namespace");
    assertAll(
      fresh("VersionedTrie.State.lookup", () -> V.ok(s.lookup("fiber/1"), "x").orElseGet(() -> new byte[0])),
      () -> assertTrue(V.ok(s.lookup("fiber/1"), "x").isPresent(), "fiber/1 is present"),
      fresh("VersionedTrie.State.namespace", () -> V.ok(n.namespace("fiber"), "x").get(0).getValue()),
      fresh("VersionedTrie.State.range", () -> V.ok(n.range("fiber/1", Optional.of("fiber/2")), "x").get(0).getValue()),
      fresh("VersionedTrie.verifyVersion", () -> V.ok(VersionedTrie.verifyVersion(8, sha, root, version, 255), "x").orElseGet(() -> new byte[0])),
      () -> assertTrue(V.ok(VersionedTrie.verifyVersion(8, sha, root, version, 255), "x").isPresent(), "version 255 is committed"),
      fresh("VersionedTrie.verifyAsOf", () -> V.ok(VersionedTrie.verifyAsOf(8, sha, root, asOf, 299), "x").map(Map.Entry::getValue).orElseGet(() -> new byte[0])),
      () -> assertTrue(V.ok(VersionedTrie.verifyAsOf(8, sha, root, asOf, 299), "x").isPresent(), "an answer as of 299")
    );
  }

  /** A sample of each component type: arrays and lists of arrays are what is checked, the rest fills the record. */
  private static Object sample(RecordComponent c) {
    final Class<?> t = c.getType();
    if (t == byte[].class) return new byte[] {1, 2, 3};
    if (t == List.class && bytesIn(c.getGenericType())) return new java.util.ArrayList<>(List.of(new byte[] {4, 5}));
    if (t == long.class) return 1L;
    if (t == int.class) return 1;
    if (t == boolean.class) return false;
    if (t == List.class) return List.of();
    if (t == Optional.class) return Optional.empty();
    if (t == String.class) return "";
    return null;
  }

  @Test
  void recordsCopyInAndOut() throws Exception {
    final List<Class<?>> records = Api.publicClasses().stream()
      .filter(Class::isRecord)
      .filter(c -> Arrays.stream(c.getRecordComponents()).anyMatch(rc -> bytesIn(rc.getGenericType())))
      .toList();
    assertTrue(records.size() >= 10, records.size() + " records hold bytes: " + records);
    assertAll(records.stream().flatMap(c -> {
      final RecordComponent[] components = c.getRecordComponents();
      final Object[] args = Arrays.stream(components).map(FreshCopyTest::sample).toArray();
      final Object record = construct(c, components, args);
      return Arrays.stream(components).filter(rc -> bytesIn(rc.getGenericType())).map(rc -> (Executable) () -> {
        final int i = Arrays.asList(components).indexOf(rc);
        final Method accessor = rc.getAccessor();
        final Object out1 = accessor.invoke(record);
        final Object out2 = accessor.invoke(record);
        final String what = c.getSimpleName() + "." + rc.getName();
        if (args[i] instanceof byte[] in) {
          final byte[] a = (byte[]) out1;
          assertNotSame(in, a, what + ": not the array passed in");
          assertNotSame(a, out2, what + ": a fresh array each time");
          Arrays.fill(in, (byte) 9);
          Arrays.fill(a, (byte) 9);
          assertArrayEquals(new byte[] {1, 2, 3}, (byte[]) accessor.invoke(record), what + ": changing either array changes nothing");
        } else {
          @SuppressWarnings("unchecked")
          final List<byte[]> in = (List<byte[]>) args[i];
          @SuppressWarnings("unchecked")
          final List<byte[]> a = (List<byte[]>) out1;
          assertNotSame(in.get(0), a.get(0), what + ": not the arrays passed in");
          assertNotSame(a.get(0), ((List<?>) out2).get(0), what + ": fresh arrays each time");
          in.get(0)[0] = 9;
          in.add(new byte[] {7});
          a.get(0)[0] = 9;
          @SuppressWarnings("unchecked")
          final List<byte[]> again = (List<byte[]>) accessor.invoke(record);
          assertEquals(1, again.size(), what + ": changing the list passed in changes nothing");
          assertArrayEquals(new byte[] {4, 5}, again.get(0), what + ": changing either array changes nothing");
        }
      });
    }));
  }

  private static Object construct(Class<?> c, RecordComponent[] components, Object[] args) {
    try {
      return c.getDeclaredConstructor(Arrays.stream(components).map(RecordComponent::getType).toArray(Class<?>[]::new)).newInstance(args);
    } catch (ReflectiveOperationException e) {
      throw new IllegalStateException(c.getName(), e);
    }
  }
}
