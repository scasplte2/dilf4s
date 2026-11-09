package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HexFormat;
import java.util.List;
import java.util.Optional;
import java.util.stream.IntStream;

/** Reading the shared vectors through the Java API itself: the files are parsed by {@link Json}. */
final class V {
  private V() {}

  static Value.Map file(String name) throws IOException {
    final Path path = Path.of(System.getProperty("dilf4s.vectors"), name);
    return map(ok(Json.parse(Files.readString(path)), name));
  }

  /** The backends as production loads them ({@link Backends#load}): every suite that needs them runs on these. */
  static Backends backends() {
    return ok(Backends.load(), "the native backends");
  }

  static <T, E> T ok(Result<T, E> r, String what) {
    return switch (r) {
      case Result.Ok<T, E> o -> o.value();
      case Result.Err<T, E> e -> fail(what + ": expected a success, got " + e.error());
    };
  }

  static <T, E> E err(Result<T, E> r, String what) {
    return switch (r) {
      case Result.Ok<T, E> o -> fail(what + ": expected a refusal, got " + o.value());
      case Result.Err<T, E> e -> e.error();
    };
  }

  static Value.Map map(Value v) {
    return v instanceof Value.Map m ? m : fail("expected a map, got " + v);
  }

  static Value.Array arr(Value v) {
    return v instanceof Value.Array a ? a : fail("expected an array, got " + v);
  }

  static Value get(Value.Map m, String key) {
    return m.get(key).orElseGet(() -> fail("no " + key + " in " + m));
  }

  static Optional<Value> opt(Value.Map m, String key) {
    return m.get(key);
  }

  static String str(Value.Map m, String key) {
    return text(get(m, key));
  }

  static String text(Value v) {
    return v instanceof Value.Text t ? t.value() : fail("expected text, got " + v);
  }

  static BigInteger big(Value v) {
    return v instanceof Value.Int i ? i.value() : fail("expected an integer, got " + v);
  }

  static long num(Value.Map m, String key) {
    return big(get(m, key)).longValueExact();
  }

  static boolean bool(Value.Map m, String key) {
    return get(m, key) instanceof Value.Bool b ? b.value() : fail("expected a boolean at " + key);
  }

  static byte[] hex(String s) {
    return HexFormat.of().parseHex(s.startsWith("0x") ? s.substring(2) : s);
  }

  static byte[] hexAt(Value.Map m, String key) {
    return hex(str(m, key));
  }

  static String hex(byte[] b) {
    return HexFormat.of().formatHex(b);
  }

  static List<Value.Map> rows(Value.Map m, String key) {
    return arr(get(m, key)).items().stream().map(V::map).toList();
  }

  static List<Value> items(Value v) {
    return arr(v).items();
  }

  static Value cbor(String hex) {
    return ok(Cbor.decode(hex(hex)), "CBOR " + hex);
  }

  static Value textValue(String s) {
    return ok(Value.text(s), "text");
  }

  static Value bytesValue(byte[] b) {
    return ok(Value.bytes(b), "bytes");
  }

  /** The kind of a vector's outcome: the text itself, or a map's {@code kind}. */
  static String kindOf(Value outcome) {
    return outcome instanceof Value.Text t ? t.value() : str(map(outcome), "kind");
  }

  static void same(byte[] expected, byte[] actual, String what) {
    assertEquals(hex(expected), hex(actual), what);
  }

  // ---- built values (shared/schemas/refusal-vectors.cddl, built-value): a leaf, then steps that wrap it in a
  // template (each "$" is the value so far, shared) or make an array of copies of it

  private static final Value HOLE = textValue("$");

  static Value built(Value recipe) {
    final Value.Map r = map(recipe);
    final Value leaf = cbor(str(r, "leaf"));
    final List<Value.Map> steps = opt(r, "steps").map(V::items).orElse(List.of()).stream().map(V::map).toList();
    return steps.stream().reduce(leaf, (v, step) -> step(v, step), (a, b) -> b);
  }

  private static Value step(Value v, Value.Map step) {
    return step.get("copies").isPresent()
      ? ok(Value.array(IntStream.range(0, (int) num(step, "copies")).mapToObj(i -> v).toList()), "copies")
      : IntStream.range(0, (int) num(step, "times")).boxed().reduce(v, (x, i) -> fill(cbor(str(step, "template")), x), (a, b) -> b);
  }

  private static Value fill(Value template, Value v) {
    return switch (template) {
      case Value.Text t when t.equals(HOLE) -> v;
      case Value.Array a -> ok(Value.array(a.items().stream().map(x -> fill(x, v)).toList()), "a template's array");
      case Value.Map m -> ok(
        Value.map(m.entries().entrySet().stream().collect(java.util.stream.Collectors.toMap(
          e -> e.getKey(), e -> fill(e.getValue(), v), (a, b) -> a, () -> new java.util.LinkedHashMap<String, Value>()))),
        "a template's map"
      );
      default -> template;
    };
  }
}
