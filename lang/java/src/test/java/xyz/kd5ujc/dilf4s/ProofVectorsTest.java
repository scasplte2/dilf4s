package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.kd5ujc.dilf4s.V.*;

import java.io.IOException;
import java.math.BigInteger;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.OptionalLong;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import xyz.kd5ujc.dilf4s.error.StateKeyError;
import xyz.kd5ujc.dilf4s.error.VersionedTrieError;

/** The proof vectors (Merkle, trie, versioned trie, Poseidon, BN254 and SP1) through the Java API. */
public class ProofVectorsTest {

  private static final Hashing.Algorithm SHA256 = Hashing.Algorithm.SHA_256;

  @Test
  void merkle() throws IOException {
    final Value.Map m = file("merkle_v1.json");
    final List<byte[]> leaves = items(get(m, "leaves")).stream().map(v -> hex(text(v))).toList();
    final List<byte[]> hashes = items(get(m, "leafHashes")).stream().map(v -> hex(text(v))).toList();
    final List<Value.Map> trees = rows(m, "trees");
    assertEquals(9, trees.size());
    assertAll(Stream.of(
      Stream.<Executable>of(() -> assertAll(IntStream.range(0, leaves.size()).mapToObj(i -> () ->
        same(hashes.get(i), ok(Merkle.leafHash(Merkle.Profile.DILF4S, SHA256, leaves.get(i)), "leaf"), "leaf hash " + i)))),
      trees.stream().map(t -> (Executable) () -> {
        final int size = (int) num(t, "size");
        final List<byte[]> dilf4s = hashes.subList(0, size);
        final List<byte[]> rfc = leaves.subList(0, size).stream().map(l -> ok(Merkle.leafHash(Merkle.Profile.RFC6962, SHA256, l), "rfc leaf")).toList();
        final byte[] root = ok(Merkle.root(Merkle.Profile.DILF4S, SHA256, dilf4s), "root");
        final byte[] rfcRoot = ok(Merkle.root(Merkle.Profile.RFC6962, SHA256, rfc), "rfc root");
        same(hexAt(t, "root"), root, "root of " + size);
        same(hexAt(t, "rfc6962Root"), rfcRoot, "RFC 6962 root of " + size);
        assertAll(
          proofs(t, "inclusion", Merkle.Profile.DILF4S, dilf4s, root),
          proofs(t, "rfc6962Inclusion", Merkle.Profile.RFC6962, rfc, rfcRoot),
          consistency(t, "consistency", Merkle.Profile.DILF4S, dilf4s, root),
          consistency(t, "rfc6962Consistency", Merkle.Profile.RFC6962, rfc, rfcRoot)
        );
      }),
      rows(m, "valueCommitments").stream().map(c -> (Executable) () -> {
        final Value v = cbor(str(c, "cbor"));
        same(hexAt(c, "commitment"), ok(ValueCommitment.bytes(v), "commitment"), "commitment of " + str(c, "cbor"));
        same(hexAt(c, "leafHash"), ok(Merkle.valueLeafHash(SHA256, v), "value leaf"), "leaf hash of " + str(c, "cbor"));
      }),
      rows(m, "malformed").stream().map(x -> (Executable) () -> {
        final Value v = cbor(str(x, "cbor"));
        final String kind = str(x, "kind").equals("inclusion")
          ? err(Merkle.decodeInclusion(SHA256, v), str(x, "name")).kind()
          : err(Merkle.decodeConsistency(SHA256, v), str(x, "name")).kind();
        assertEquals(str(x, "error"), kind, str(x, "name"));
      })
    ).flatMap(s -> s));
  }

  private static Executable proofs(Value.Map t, String key, Merkle.Profile profile, List<byte[]> hashes, byte[] root) {
    return () -> assertAll(IntStream.range(0, items(get(t, key)).size()).mapToObj(i -> () -> {
      final Merkle.InclusionProof p = ok(Merkle.inclusionProof(profile, SHA256, hashes, i), "inclusion");
      assertEquals(text(items(get(t, key)).get(i)), hex(ok(Cbor.encode(ok(Merkle.encode(SHA256, p), "encode")), "cbor")), key + " " + i);
      assertEquals(p, ok(Merkle.decodeInclusion(SHA256, cbor(text(items(get(t, key)).get(i)))), "decode"), key + " " + i + " reads back");
      assertEquals(true, ok(Merkle.verifyInclusion(profile, SHA256, root, hashes.get(i), p), "verify"), key + " " + i + " verifies");
      if (profile == Merkle.Profile.DILF4S && hashes.size() > 1) {
        assertEquals(false, ok(Merkle.verifyInclusion(profile, SHA256, root, hashes.get((i + 1) % hashes.size()), p), "verify"), key + " " + i + ": another leaf");
      }
    }));
  }

  private static Executable consistency(Value.Map t, String key, Merkle.Profile profile, List<byte[]> hashes, byte[] root) {
    return () -> assertAll(IntStream.range(0, items(get(t, key)).size()).mapToObj(j -> () -> {
      final Merkle.ConsistencyProof p = ok(Merkle.consistencyProof(profile, SHA256, hashes, j), "consistency");
      assertEquals(text(items(get(t, key)).get(j)), hex(ok(Cbor.encode(ok(Merkle.encode(SHA256, p), "encode")), "cbor")), key + " " + j);
      final byte[] old = ok(Merkle.root(profile, SHA256, hashes.subList(0, j)), "old root");
      assertEquals(true, ok(Merkle.verifyConsistency(profile, SHA256, old, root, p), "verify"), key + " from " + j + " verifies");
    }));
  }

  private static Trie.KeyMode mode(Value.Map m) {
    return m.get("fixed").isPresent() ? ok(Trie.fixed((int) num(m, "fixed")), "fixed") : ok(Trie.variable((int) num(m, "variable")), "variable");
  }

  private static List<Trie.Entry> listing(Value v) {
    return items(v).stream().map(V::map).map(e -> new Trie.Entry(hexAt(e, "key"), hexAt(e, "valueHash"))).toList();
  }

  @Test
  void trie() throws IOException {
    final Value.Map t = file("trie_v1.json");
    final Trie.KeyMode any = ok(Trie.fixed(2), "fixed");
    same(hexAt(t, "emptyRoot"), ok(Trie.root(SHA256, any, List.of()), "empty"), "the empty root");
    final List<Value.Map> fixtures = rows(t, "fixtures");
    assertEquals(7, fixtures.size());
    assertAll(Stream.concat(
      fixtures.stream().map(f -> (Executable) () -> {
        final Trie.KeyMode mode = mode(map(get(f, "mode")));
        final List<Map.Entry<byte[], byte[]>> entries = rows(f, "entries").stream().map(e -> Map.entry(hexAt(e, "key"), hexAt(e, "valueHash"))).toList();
        final byte[] root = ok(Trie.root(SHA256, mode, entries), "root");
        same(hexAt(f, "root"), root, str(f, "name") + ": the root");
        final Value.Map proof = map(get(f, "proof"));
        final Trie.Verified verified = ok(Trie.check(SHA256, mode, root, cbor(str(proof, "cbor"))), str(f, "name") + ": the proof");
        assertAll(
          () -> assertAll(rows(proof, "lookups").stream().map(l -> () -> {
            final Trie.Lookup got = ok(verified.lookup(hexAt(l, "key")), "lookup");
            final Value r = get(l, "result");
            final Trie.Lookup expected = r instanceof Value.Map p
              ? new Trie.Lookup.Present(hexAt(p, "present"))
              : text(r).equals("absent") ? new Trie.Lookup.Absent() : new Trie.Lookup.NotCovered();
            assertEquals(expected, got, str(f, "name") + ": lookup " + str(l, "key"));
          })),
          () -> assertAll(rows(proof, "prefixListings").stream().map(p ->
            () -> assertEquals(Optional.of(listing(get(p, "entries"))), ok(verified.prefix(hexAt(p, "prefix")), "prefix"), str(f, "name") + ": prefix " + str(p, "prefix")))),
          () -> assertAll(rows(proof, "rangeListings").stream().map(r -> () -> {
            final Optional<byte[]> until = r.get("until").map(u -> hex(text(u)));
            final Value result = get(r, "result");
            final Optional<List<Trie.Entry>> expected = result instanceof Value.Map e ? Optional.of(listing(get(e, "entries"))) : Optional.empty();
            assertEquals(expected, ok(verified.range(hexAt(r, "from"), until), "range"), str(f, "name") + ": range from " + str(r, "from"));
          }))
        );
      }),
      rows(t, "malformed").stream().map(x -> (Executable) () ->
        assertEquals(str(x, "error"), err(Trie.check(SHA256, mode(map(get(x, "mode"))), hexAt(x, "root"), cbor(str(x, "cbor"))), str(x, "name")).kind(), str(x, "name")))
    ));
  }

  @Test
  void versionedTrie() throws IOException {
    final Value.Map v = file("versioned_trie_v1.json");
    final int width = (int) num(v, "width");
    final byte[] root = hexAt(v, "root");
    final Value.Map head = map(get(v, "head"));
    final List<Value.Map> proofs = rows(v, "proofs");
    assertEquals(10, proofs.size());
    assertAll(Stream.of(
      proofs.stream().map(p -> (Executable) () -> {
        final Value proof = cbor(str(p, "cbor"));
        final String name = str(p, "name");
        switch (str(p, "kind")) {
          case "state", "historical" -> {
            final VersionedTrie.State state = str(p, "kind").equals("state")
              ? ok(VersionedTrie.verifyState(width, SHA256, root, proof), name)
              : ok(VersionedTrie.verifyHistoricalKey(width, SHA256, root, proof, num(p, "at")), name);
            assertAll(
              () -> assertAll(p.get("claims").map(V::items).orElse(List.of()).stream().map(V::map).map(c -> () ->
                assertEquals(true, ok(state.holds(str(c, "key"), c.get("value").map(x -> cbor(text(x)))), name + " " + str(c, "key")), name + ": " + str(c, "key")))),
              () -> assertAll(p.get("listings").map(V::items).orElse(List.of()).stream().map(V::map).map(l -> () -> {
                final List<String> expected = rows(l, "entries").stream()
                  .map(e -> str(e, "key") + "=" + hex(ok(Trie.valueHash(SHA256, cbor(str(e, "value"))), "value hash"))).toList();
                final List<String> got = ok(state.namespace(str(l, "namespace")), "namespace").stream().map(e -> e.getKey() + "=" + hex(e.getValue())).toList();
                assertEquals(expected, got, name + ": namespace " + str(l, "namespace"));
              }))
            );
          }
          case "version" -> {
            final Optional<byte[]> got = ok(VersionedTrie.verifyVersion(width, SHA256, root, proof, num(p, "at")), name);
            final Optional<String> expected = map(get(p, "answer")).get("stateRoot").map(V::text);
            assertEquals(expected, got.map(V::hex), name);
          }
          case "asOf" -> {
            final Optional<Map.Entry<Long, byte[]>> got = ok(VersionedTrie.verifyAsOf(width, SHA256, root, proof, num(p, "at")), name);
            final Value.Map answer = map(get(p, "answer"));
            assertEquals(answer.get("version").map(x -> big(x).longValueExact()), got.map(Map.Entry::getKey), name + ": the version");
            assertEquals(answer.get("stateRoot").map(V::text), got.map(e -> hex(e.getValue())), name + ": the state root");
          }
          case "head" -> {
            final VersionedTrie.Head h = ok(VersionedTrie.verifyHead(width, SHA256, root, proof), name);
            assertEquals(new VersionedTrie.Head(num(head, "version"), hexAt(head, "stateRoot"), hexAt(head, "historyRoot")), h, name);
          }
          case "versions" -> {
            final OptionalLong until = p.get("until").map(u -> OptionalLong.of(big(u).longValueExact())).orElse(OptionalLong.empty());
            final List<Map.Entry<Long, byte[]>> claimed = rows(p, "versions").stream().map(c -> Map.entry(num(c, "version"), hexAt(c, "stateRoot"))).toList();
            assertEquals(true, ok(VersionedTrie.verifyVersions(width, SHA256, root, proof, num(p, "at"), until, claimed), name), name);
          }
          default -> org.junit.jupiter.api.Assertions.fail("no proof kind " + str(p, "kind"));
        }
      }),
      rows(v, "stateKeys").stream().map(k -> (Executable) () -> {
        final Result<String, StateKeyError> got = VersionedTrie.stateKey(str(k, "key"));
        assertEquals(kindOf(get(k, "outcome")), got.isOk() ? "accepted" : err(got, "key").kind(), str(k, "key"));
      }),
      rows(v, "namespaces").stream().map(n -> (Executable) () -> {
        final Result<String, StateKeyError> got = VersionedTrie.namespace(str(n, "namespace"));
        assertEquals(kindOf(get(n, "outcome")), got.isOk() ? "accepted" : err(got, "namespace").kind(), str(n, "namespace"));
      }),
      rows(v, "versionKeys").stream().map(k -> (Executable) () -> {
        final BigInteger version = big(get(k, "version"));
        final Result<byte[], VersionedTrieError> got = VersionedTrie.version(width, hexAt(k, "key")).flatMap(x -> VersionedTrie.versionKey(width, x));
        final String outcome = kindOf(get(k, "outcome"));
        if (outcome.equals("accepted")) {
          assertEquals(str(k, "key"), hex(ok(VersionedTrie.versionKey(width, version.longValueExact()), "key")), "the key of " + version);
          assertEquals(version.longValueExact(), ok(VersionedTrie.version(width, hexAt(k, "key")), "version"), "the version of " + str(k, "key"));
        } else {
          assertEquals(outcome, err(got, "version key").kind(), str(k, "key"));
        }
      })
    ).flatMap(s -> s));
  }

  @Test
  void poseidon() throws IOException {
    final Value.Map p = file("poseidon_v1.json");
    final Value.Map merkle = map(get(p, "merkle"));
    final int depth = (int) num(merkle, "depth");
    final List<Map.Entry<BigInteger, BigInteger>> leaves = map(get(merkle, "leaves")).entries().entrySet().stream()
      .map(e -> Map.entry(new BigInteger(e.getKey()), new BigInteger(text(e.getValue())))).toList();
    final BigInteger root = new BigInteger(str(merkle, "root"));
    assertAll(Stream.of(
      rows(p, "poseidon").stream().map(r -> (Executable) () -> {
        final List<BigInteger> inputs = items(get(r, "inputs")).stream().map(x -> new BigInteger(text(x))).toList();
        assertEquals(new BigInteger(str(r, "output")), ok(Poseidon.hash(inputs), "hash"), "Poseidon " + inputs);
      }),
      Stream.<Executable>of(() -> assertEquals(root, ok(Poseidon.treeRoot(depth, leaves), "root"), "the tree's root")),
      rows(merkle, "proofs").stream().map(r -> (Executable) () -> {
        final BigInteger position = new BigInteger(str(r, "position"));
        final Poseidon.MerkleProof proof = new Poseidon.MerkleProof(position, items(get(r, "siblings")).stream().map(x -> new BigInteger(text(x))).toList());
        assertEquals(proof, ok(Poseidon.treeProof(depth, leaves, position), "proof"), "the proof of " + position);
        final BigInteger leaf = leaves.stream().filter(e -> e.getKey().equals(position)).map(Map.Entry::getValue).findFirst().orElse(BigInteger.ZERO);
        assertEquals(true, ok(Poseidon.verifyInclusion(leaf, proof, root), "verify"), "the proof of " + position + " verifies");
        // a root, like a leaf, is a field element: one outside the field is refused, never a mismatch
        assertAll(Stream.of(Poseidon.MODULUS, BigInteger.ONE.negate()).flatMap(bad -> Stream.of(
          () -> assertEquals("Encoding", err(Poseidon.verifyInclusion(leaf, proof, bad), "a bad root").kind(), "inclusion under root " + bad),
          () -> assertEquals("Encoding", err(Poseidon.verifyAbsence(proof, bad), "a bad root").kind(), "absence under root " + bad),
          () -> assertEquals("Encoding", err(Poseidon.verifyInclusion(bad, proof, root), "a bad leaf").kind(), "inclusion of leaf " + bad)
        )));
        assertEquals(proof, ok(Poseidon.decode(ok(Poseidon.encode(proof), "encode")), "decode"), "its canonical form reads back");
      }),
      rows(p, "builds").stream().map(b -> (Executable) () -> {
        final List<Map.Entry<BigInteger, BigInteger>> entries = rows(b, "entries").stream()
          .map(e -> Map.entry(new BigInteger(str(e, "position")), new BigInteger(str(e, "leaf")))).toList();
        assertEquals(new BigInteger(str(b, "root")), ok(Poseidon.treeRoot((int) num(b, "depth"), entries), "build"), "a build of depth " + num(b, "depth"));
      })
    ).flatMap(s -> s));
  }

  @Test
  void zk() throws IOException {
    final Value.Map z = file("zk_v1.json");
    assertAll(Stream.of(
      rows(z, "add").stream().map(r -> (Executable) () -> point(r, Zk.add(hexAt(r, "a"), hexAt(r, "b")))),
      rows(z, "mul").stream().map(r -> (Executable) () -> point(r, Zk.mul(hexAt(r, "point"), hexAt(r, "scalar")))),
      rows(z, "pairing").stream().map(r -> (Executable) () -> {
        final var got = Zk.pairing(rows(r, "pairs").stream().map(x -> Map.entry(hexAt(x, "g1"), hexAt(x, "g2"))).toList());
        final Value.Map result = map(get(r, "result"));
        if (result.get("value").isPresent()) {
          assertEquals(bool(result, "value"), ok(got, str(r, "name")), str(r, "name"));
        } else {
          assertEquals(str(result, "error"), err(got, str(r, "name")).kind(), str(r, "name"));
        }
      }),
      rows(z, "sp1Groth16").stream().map(r -> (Executable) () -> {
        final var got = Zk.sp1Verify(hexAt(r, "programVKey"), hexAt(r, "publicValues"), hexAt(r, "proofBytes"));
        assertEquals(str(r, "result"), got.isOk() ? "verified" : err(got, str(r, "name")).kind(), str(r, "name"));
      })
    ).flatMap(s -> s));
  }

  private static void point(Value.Map r, Result<byte[], xyz.kd5ujc.dilf4s.error.ZkError> got) {
    final Value.Map result = map(get(r, "result"));
    if (result.get("point").isPresent()) {
      assertEquals(str(result, "point"), hex(ok(got, str(r, "name"))), str(r, "name"));
    } else {
      assertEquals(str(result, "error"), err(got, str(r, "name")).kind(), str(r, "name"));
    }
  }

  @Test
  void sigma() throws IOException {
    final Value.Map s = file("sigma_v1.json");
    final List<Value.Map> cases = rows(s, "cases");
    assertEquals(120, cases.size());
    assertAll(cases.stream().map(c -> () -> {
      final Value proposition = ok(Jlvm.parseValue(Json.render(get(c, "proposition")).fold(e -> "null", x -> x)), "proposition");
      final var got = Sigma.verify(proposition, hexAt(c, "proof"), hexAt(c, "message"));
      assertTrue(got.isOk() || !str(c, "kind").equals("valid"), str(c, "name") + " " + str(c, "kind") + ": " + got);
      got.success().ifPresent(b -> assertEquals(str(c, "kind").equals("valid"), b, str(c, "name") + " " + str(c, "kind")));
    }));
  }
}
