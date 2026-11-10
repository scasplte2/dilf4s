package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.kd5ujc.dilf4s.V.*;

import java.io.IOException;
import java.util.List;
import java.util.function.Function;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import xyz.kd5ujc.dilf4s.error.CanonicalError;
import xyz.kd5ujc.dilf4s.error.HashError;
import xyz.kd5ujc.dilf4s.error.RenderError;
import xyz.kd5ujc.dilf4s.error.TooLarge;

/** {@code shared/vectors/canonical_encoding_v1.json}, every section, through the Java API. */
public class CanonicalVectorsTest {

  private static Value.Map vectors;
  private static Hashing.Algorithm contentAlgorithm;
  private static Hashing.DomainTag contentTag;

  @BeforeAll
  static void load() throws IOException {
    vectors = file("canonical_encoding_v1.json");
    contentAlgorithm = ok(Hashing.algorithm(str(vectors, "contentHashAlgorithm")), "the content hash algorithm");
    contentTag = ok(Hashing.domainTag(str(vectors, "contentHashTag")), "the content hash tag");
  }

  private static void check(String section, int expected, Function<Value.Map, Executable> row) {
    final List<Value.Map> rows = rows(vectors, section);
    assertEquals(expected, rows.size(), section + " rows");
    assertAll(section, rows.stream().map(row));
  }

  private static String contentHash(Value v) {
    return ok(Hashing.contentHash(contentAlgorithm, contentTag, v), "content hash").hex();
  }

  @Test
  void json() {
    check("json", 138, r -> () -> {
      final String text = str(r, "json");
      final Result<Value, CanonicalError> parsed = Json.parse(text);
      if (r.get("error").isPresent()) {
        final CanonicalError e = err(parsed, text);
        assertEquals(str(r, "error"), e.kind(), "the refusal of " + text);
        r.get("limit").ifPresent(l -> assertEquals(big(l).longValueExact(), limitOf(e), "the limit of " + text));
      } else {
        final Value v = ok(parsed, text);
        assertEquals(str(r, "cbor"), hex(ok(Cbor.encode(v), text)), "the CBOR of " + text);
        assertEquals(str(r, "contentHash"), contentHash(v), "the content hash of " + text);
        assertEquals(v, cbor(str(r, "cbor")), "the CBOR of " + text + " reads back");
        assertEquals(str(r, "render"), ok(Json.render(v), text), "the rendering of " + text);
      }
    });
  }

  private static long limitOf(CanonicalError e) {
    return switch (e) {
      case CanonicalError.IntegerTooWide w -> w.limit();
      case CanonicalError.RationalTooWide w -> w.limit();
      case CanonicalError.DepthExceeded d -> d.limit();
      default -> -1L;
    };
  }

  @Test
  void model() {
    check("model", 11, r -> () -> {
      final Value v = cbor(str(r, "cbor"));
      assertEquals(str(r, "cbor"), hex(ok(Cbor.encode(v), "encode")), str(r, "value"));
      assertEquals(str(r, "contentHash"), contentHash(v), str(r, "value"));
    });
  }

  @Test
  void renderRejections() {
    check("renderRejections", 3, r -> () -> {
      final RenderError e = err(Json.render(cbor(str(r, "cbor"))), "render");
      assertTrue(e instanceof RenderError.Canonical c && c.error().kind().equals(str(r, "error")), "refused as " + str(r, "error") + ": " + e);
    });
  }

  @Test
  void limits() {
    check("limits", 19, r -> () -> {
      final Value v = cbor(str(r, "value"));
      final long max = num(r, "maxBytes");
      final Value outcome = get(r, "outcome");
      final boolean asCbor = str(r, "encoding").equals("cbor");
      if (outcome instanceof Value.Text t && t.value().equals("accepted")) {
        final String out = asCbor ? hex(ok(Cbor.encodeWithin(v, max), str(r, "name"))) : ok(Json.renderWithin(v, max), str(r, "name"));
        assertEquals(str(r, "output"), out, str(r, "name"));
      } else {
        final String kind = kindOf(outcome);
        final String got = asCbor
          ? err(Cbor.encodeWithin(v, max), str(r, "name")).kind()
          : switch (err(Json.renderWithin(v, max), str(r, "name"))) {
            case RenderError.Canonical c -> c.error().kind();
            case TooLarge l -> l.kind();
          };
        assertEquals(kind, got, str(r, "name"));
        map(outcome).get("limit").ifPresent(l -> {
          final long applied = asCbor
            ? ((TooLarge) err(Cbor.encodeWithin(v, max), "limit")).limit()
            : ((TooLarge) err(Json.renderWithin(v, max), "limit")).limit();
          assertEquals(big(l).longValueExact(), applied, str(r, "name") + ": the limit");
        });
      }
    });
  }

  @Test
  void cborRejections() {
    check("cborRejections", 60, r -> () -> {
      final CanonicalError e = err(Cbor.decode(hex(str(r, "cbor"))), str(r, "cbor"));
      assertEquals(str(r, "error"), e.kind(), "the refusal of " + str(r, "cbor"));
      r.get("limit").ifPresent(l -> assertEquals(big(l).longValueExact(), limitOf(e), "the limit"));
    });
  }

  @Test
  void hashing() {
    check("digests", 21, r -> () -> {
      final Hashing.Algorithm a = ok(Hashing.algorithm(str(r, "algorithm")), "algorithm");
      assertEquals(str(r, "digest"), ok(Hashing.digest(a, hexAt(r, "message")), "digest").hex(), str(r, "algorithm"));
    });
    check("taggedHash", 21, r -> () -> {
      final Hashing.Algorithm a = ok(Hashing.algorithm(str(r, "algorithm")), "algorithm");
      final Hashing.DomainTag tag = ok(Hashing.domainTag(str(r, "tag")), "tag");
      assertEquals(str(r, "digest"), ok(Hashing.taggedHash(a, tag, hexAt(r, "message")), "tagged").hex(), str(r, "algorithm"));
    });
  }

  @Test
  void names() {
    check("domainTags", 18, r -> () -> {
      final Result<Hashing.DomainTag, HashError> tag = Hashing.domainTag(str(r, "tag"));
      final String outcome = str(r, "outcome");
      assertEquals(outcome, tag.isOk() ? "accepted" : err(tag, "tag").kind(), str(r, "tag"));
    });
    check("algorithms", 21, r -> () -> {
      final Result<Hashing.Algorithm, HashError> a = Hashing.algorithm(str(r, "name"));
      assertEquals(str(r, "outcome"), a.isOk() ? "accepted" : err(a, "name").kind(), str(r, "name"));
      a.success().ifPresent(x -> assertEquals(str(r, "name"), x.wire()));
    });
    check("digestHex", 38, r -> () -> {
      final Hashing.Algorithm a = ok(Hashing.algorithm(str(r, "algorithm")), "algorithm");
      final Result<Hashing.Digest, HashError> d = Hashing.digestFromHex(a, str(r, "hex"));
      final String outcome = str(r, "outcome");
      if (outcome.equals("accepted")) {
        assertEquals(str(r, "hex").replaceFirst("^0x", "").toLowerCase(java.util.Locale.ROOT), ok(d, "hex").hex(), str(r, "hex"));
      } else {
        final HashError e = err(d, str(r, "hex"));
        assertEquals(outcome, e.kind(), str(r, "hex"));
        r.get("offset").ifPresent(o -> assertEquals(big(o).longValueExact(), ((HashError.InvalidHex) e).offset(), "the offset"));
      }
    });
  }

  @Test
  void expansions() {
    check("expansions", 26, r -> () -> {
      final Hashing.Algorithm a = ok(Hashing.algorithm(str(r, "algorithm")), "algorithm");
      final Result<byte[], HashError> x = Hashing.expandMessageXmd(a, hexAt(r, "message"), hexAt(r, "dst"), num(r, "length"));
      if (str(r, "outcome").equals("accepted")) {
        assertEquals(str(r, "output"), hex(ok(x, "expansion")), "expansion");
      } else {
        final HashError e = err(x, "expansion");
        assertEquals(str(r, "outcome"), e.kind(), "refusal");
        assertEquals(str(r, "refusal"), ((HashError.InvalidExpansion) e).refusal().wire(), "the reason");
      }
    });
  }
}
