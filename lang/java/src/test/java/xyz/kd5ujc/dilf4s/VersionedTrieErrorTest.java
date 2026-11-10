package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static xyz.kd5ujc.dilf4s.V.*;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import xyz.kd5ujc.dilf4s.error.RootComponent;
import xyz.kd5ujc.dilf4s.error.VersionedTrieError;

final class VersionedTrieErrorTest {

  @Test
  void versionRefusalsRenderTheirTypedDetails() {
    assertEquals(new VersionedTrieError.InvalidVersion("a Long version key is 1..8 bytes, not 0"),
      err(VersionedTrie.versionKey(0, 1), "invalid width"));
    assertEquals(new VersionedTrieError.InvalidVersion("version 256 is outside 0..255"),
      err(VersionedTrie.versionKey(1, 256), "outside range"));
    assertEquals(new VersionedTrieError.InvalidVersion("a version key is 8 bytes, got 7"),
      err(VersionedTrie.version(8, new byte[7]), "wrong encoded width"));
  }

  @Test
  void verificationRefusalsRenderTheirTypedDetails() throws IOException {
    final Value.Map vectors = file("versioned_trie_v1.json");
    final Value.Map row = rows(vectors, "proofs").getFirst();
    assertEquals("current-keys", str(row, "name"));
    final Value proof = cbor(str(row, "cbor"));
    final byte[] root = hexAt(vectors, "root");
    final Hashing.Algorithm hash = Hashing.Algorithm.SHA_256;
    assertEquals(new VersionedTrieError.RootMismatch(RootComponent.ROOT, "00".repeat(32), str(vectors, "root")),
      err(VersionedTrie.verifyState(8, hash, new byte[32], proof), "wrong trusted root"));
    assertEquals(new VersionedTrieError.Malformed("the proof has no history proof"),
      err(VersionedTrie.verifyHead(8, hash, root, proof), "missing history"));
    final VersionedTrie.State state = ok(VersionedTrie.verifyState(8, hash, root, proof), "current state");
    assertEquals(new VersionedTrieError.NotCovered("key meta"), err(state.lookup("meta"), "uncovered key"));
  }
}
