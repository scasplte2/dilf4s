package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static xyz.kd5ujc.dilf4s.V.*;

import org.junit.jupiter.api.Test;

import xyz.kd5ujc.dilf4s.error.JlvmError;

/**
 * The deepest inputs, first, in a JVM of their own (build.sbt forks each suite) at the 1 MB thread stack the README's
 * guarantee names: so the reference's recursive program reader runs cold, at its deepest, and returns a typed result.
 * Below about 1 MB it can overflow (java/README.md, "Thread stacks").
 */
public class DeepInputTest {

  private static String nested(int depth) {
    return "[".repeat(depth) + "0" + "]".repeat(depth);
  }

  @Test
  void theDeepestInputsReturnTypedResults() {
    // the first call in this JVM: the surface reader at the canonical depth bound, refused by the program's own bound
    final JlvmError surface = err(Jlvm.parse(nested(Value.MAX_DEPTH)), "a program at the surface bound");
    final JlvmError past = err(Jlvm.parse(nested(Value.MAX_DEPTH + 1)), "a program past the surface bound");
    final Jlvm.Program deepest = ok(Jlvm.parse(nested(126)), "the deepest program");
    final byte[] encoded = ok(Jlvm.encode(deepest), "its encoding");
    assertAll(
      () -> assertEquals("Limit", surface.kind(), "a program nested to the surface bound is Limit"),
      () -> assertEquals("Limit", past.kind(), "past the surface bound too"),
      () -> assertEquals(deepest, ok(Jlvm.decode(encoded), "decode"), "the deepest program decodes"),
      () -> assertEquals(deepest, ok(Jlvm.fromCanonical(ok(Jlvm.toCanonical(deepest), "canonical")), "fromCanonical"), "and reads from its form"),
      () -> assertEquals(Value.MAX_DEPTH, ok(Jlvm.parseValue(nested(Value.MAX_DEPTH)), "data").depth(), "data at the bound"),
      () -> assertEquals("DepthExceeded", err(Json.parse(nested(Value.MAX_DEPTH + 1)), "JSON").kind(), "JSON past the bound"),
      () -> assertEquals(
        "DepthExceeded",
        err(Cbor.decode(hex("81".repeat(Value.MAX_DEPTH + 1) + "00")), "CBOR").kind(),
        "CBOR past the bound"
      ),
      () -> assertEquals(Value.MAX_DEPTH + 1L, (long) ok(Cbor.encode(ok(Json.parse(nested(Value.MAX_DEPTH)), "json")), "cbor").length, "the deepest value encodes"),
      () -> assertEquals("DepthExceeded", err(Json.parse(nested(100_000)), "very deep JSON").kind(), "JSON nested 100,000 deep: refused by depth"),
      () -> assertEquals("Limit", err(Jlvm.parse(nested(100_000)), "a very deep program").kind(), "a program nested 100,000 deep: Limit"),
      // the domain-tag grammar recurses per segment: cold at 1 MB it holds to about 1,245 (README, "Thread stacks")
      () -> assertEquals("a" + "/a".repeat(1_000) + "/v1", ok(Hashing.domainTag("a" + "/a".repeat(1_000) + "/v1"), "a long tag").value(), "a tag of 1,000 segments"),
      () -> assertEquals("InvalidDomainTag", err(Hashing.domainTag("a" + "/a".repeat(1_000) + "/x1"), "a long bad tag").kind(), "a bad tag of 1,000 segments")
    );
  }
}
