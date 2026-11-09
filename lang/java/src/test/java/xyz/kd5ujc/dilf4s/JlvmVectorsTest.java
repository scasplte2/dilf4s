package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.kd5ujc.dilf4s.V.*;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import xyz.kd5ujc.dilf4s.error.CryptoCause;
import xyz.kd5ujc.dilf4s.error.JlvmError;

/**
 * {@code shared/vectors/jlvm_v1.json} through the Java API: every case evaluated by {@link Jlvm#evaluate} on the
 * loaded backends, its outcome (the value's CBOR and JSON surface, or the refusal's kind, op and cause), gas and
 * warnings exactly as the vector has them; and every alias and decoding row.
 */
public class JlvmVectorsTest {

  private static Value.Map vectors;

  @BeforeAll
  static void load() throws IOException {
    vectors = file("jlvm_v1.json");
  }

  @Test
  void aliases() {
    final List<Value.Map> rows = rows(vectors, "aliases");
    assertEquals(4, rows.size());
    assertAll(rows.stream().map(r -> () -> {
      final Jlvm.Program alias = ok(Jlvm.parse(str(r, "program")), str(r, "alias"));
      final Jlvm.Program own = ok(Jlvm.parse(str(r, "own")), str(r, "op"));
      assertEquals(own, alias, str(r, "alias") + " parses as " + str(r, "op"));
      assertEquals(str(r, "cbor"), hex(ok(Jlvm.encode(alias), "encode")), str(r, "alias") + ": its canonical CBOR");
      assertEquals(str(r, "id"), ok(Jlvm.id(alias), "id").hex(), str(r, "alias") + ": its id");
      assertEquals(alias, ok(Jlvm.decode(hexAt(r, "cbor")), "decode"), str(r, "alias") + ": its CBOR reads back");
      assertEquals(alias, ok(Jlvm.fromCanonical(ok(Jlvm.toCanonical(alias), "canonical")), "fromCanonical"), "its canonical form reads back");
    }));
  }

  @Test
  void decodings() {
    final List<Value.Map> rows = rows(vectors, "decodings");
    assertEquals(6, rows.size());
    assertAll(rows.stream().map(r -> () -> {
      final Value form = built(get(r, "form"));
      final Value.Map outcome = map(get(r, "outcome"));
      final Result<Jlvm.Program, JlvmError> read = Jlvm.fromCanonical(form);
      if (outcome.get("error").isPresent()) {
        assertEquals(str(outcome, "error"), err(read, str(r, "name")).kind(), str(r, "name"));
      } else {
        assertEquals(str(outcome, "cbor"), hex(ok(Jlvm.encode(ok(read, str(r, "name"))), "encode")), str(r, "name"));
      }
    }));
  }

  /**
   * One case as the reference's harness runs it: the program, the data and the bindings read in that order (a refusal
   * there is the outcome, using no gas), the built bindings built, then the evaluation at the case's gas limit.
   */
  private static Jlvm.Outcome run(Value.Map c) {
    final Result<Jlvm.Program, JlvmError> program = Jlvm.parse(str(c, "program"));
    final Result<Value, JlvmError> data = Jlvm.parseValue(str(c, "data"));
    final Map<String, Result<Value, JlvmError>> env = opt(c, "env").map(e -> map(e).entries().entrySet().stream()
      .collect(Collectors.toMap(Map.Entry::getKey, x -> Jlvm.parseValue(text(x.getValue())), (a, b) -> a, LinkedHashMap<String, Result<Value, JlvmError>>::new)))
      .orElse(new LinkedHashMap<>());
    final Optional<JlvmError> early = Stream.concat(Stream.of(program.failure(), data.failure()), env.values().stream().map(Result::failure))
      .flatMap(Optional::stream)
      .findFirst();
    return early.map(e -> new Jlvm.Outcome(0L, Result.<Value, JlvmError>err(e), List.of())).orElseGet(() -> {
      final Map<String, Value> bindings = new LinkedHashMap<>();
      env.forEach((k, v) -> bindings.put(k, ok(v, k)));
      opt(c, "built").ifPresent(b -> map(b).entries().forEach((k, recipe) -> bindings.put(k, built(recipe))));
      final long limit = opt(c, "gasLimit").map(l -> big(l).longValueExact()).orElse(Jlvm.DEFAULT_GAS_LIMIT);
      return ok(Jlvm.evaluate(V.backends(), ok(program, "program"), ok(data, "data"), bindings, limit), str(c, "name"));
    });
  }

  private static Optional<String> op(JlvmError e) {
    return switch (e) {
      case JlvmError.Type t -> Optional.of(t.op());
      case JlvmError.Arity a -> Optional.of(a.op());
      case JlvmError.Arithmetic a -> Optional.of(a.op());
      case JlvmError.Domain d -> Optional.of(d.op());
      case JlvmError.Crypto c -> Optional.of(c.op());
      default -> Optional.empty();
    };
  }

  /** A Crypto error's cause as the vectors record it: {@code {family: kind}}, the family in lower camel case. */
  private static Optional<Map<String, String>> cause(JlvmError e) {
    return e instanceof JlvmError.Crypto c ? c.cause().map(x -> Map.of(Character.toLowerCase(x.kind().charAt(0)) + x.kind().substring(1), causeKind(x))) : Optional.empty();
  }

  private static String causeKind(CryptoCause c) {
    return switch (c) {
      case CryptoCause.Signing s -> s.error().kind();
      case CryptoCause.Sigma s -> s.error().kind();
      case CryptoCause.Zk z -> z.error().kind();
      case CryptoCause.Groth16 g -> g.error().kind();
      case CryptoCause.Vrf v -> v.error().kind();
      case CryptoCause.Hash h -> h.error().kind();
      case CryptoCause.Merkle m -> m.error().kind();
      case CryptoCause.Trie t -> t.error().kind();
      case CryptoCause.Poseidon p -> p.error().kind();
      case CryptoCause.PoseidonMerkle p -> p.error().kind();
    };
  }

  @Test
  void cases() {
    final List<Value.Map> rows = rows(vectors, "cases");
    assertEquals(3361, rows.size(), "the cases");
    assertAll(rows.stream().map(c -> () -> {
      final String name = str(c, "name");
      final Jlvm.Outcome o = run(c);
      final Value.Map expected = map(get(c, "outcome"));
      assertEquals(num(c, "gasUsed"), o.gasUsed(), name + ": the gas used");
      assertEquals(num(c, "warnings"), (long) o.warnings().size(), name + ": the warnings");
      switch (o.result()) {
        case Result.Ok<Value, JlvmError> v -> {
          assertEquals(opt(expected, "cbor").map(V::text), Optional.of(hex(ok(Cbor.encode(v.value()), "encode"))), name + ": the value's CBOR");
          assertEquals(opt(expected, "json").map(V::text), Jlvm.renderValue(v.value()).success(), name + ": the value's JSON surface");
        }
        case Result.Err<Value, JlvmError> e -> {
          assertEquals(opt(expected, "error").map(V::text), Optional.of(e.error().kind()), name + ": the refusal");
          assertEquals(opt(expected, "op").map(V::text), op(e.error()), name + ": the refusal's op");
          assertEquals(
            opt(expected, "cause").map(x -> map(x).entries().entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, y -> text(y.getValue())))),
            cause(e.error()),
            name + ": the refusal's cause"
          );
        }
      }
    }));
  }

  @Test
  void outcomesCover() {
    final List<Value.Map> rows = rows(vectors, "cases");
    final long values = rows.stream().filter(c -> map(get(c, "outcome")).get("cbor").isPresent()).count();
    final long causes = rows.stream().filter(c -> map(get(c, "outcome")).get("cause").isPresent()).count();
    final long built = rows.stream().filter(c -> c.get("built").isPresent()).count();
    assertAll(
      () -> assertEquals(1129L, values, "the cases with a value"),
      () -> assertEquals(90L, causes, "the cases with a Crypto cause"),
      () -> assertEquals(20L, built, "the cases with built bindings"),
      () -> assertTrue(rows.stream().filter(c -> c.get("built").isPresent()).flatMap(c -> map(get(c, "built")).entries().values().stream().map(V::built))
        .anyMatch(v -> v.weight() > (1L << 20)), "a built binding is far heavier than its description")
    );
  }
}
