package xyz.kd5ujc.dilf4s;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static xyz.kd5ujc.dilf4s.V.*;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import xyz.kd5ujc.dilf4s.error.JlvmError;

/**
 * A gas limit is 0 to {@link Jlvm#MAX_GAS_LIMIT}: at the maximum a program is metered as under any other limit, and one
 * short of its cost runs out; below 0 or above the maximum it is {@code Limit} with no gas used, through both entry
 * points. The reference keeps the limit as given, so a negative one is never read as 0.
 */
public class JlvmGasLimitTest {

  private static final Value DATA = ok(Jlvm.parseValue("null"), "data");
  private static final Jlvm.Program SUM = ok(Jlvm.parse("{\"+\":[1,2]}"), "a program");

  /** The program, then the same program compiled first. */
  private static List<Jlvm.Outcome> both(long limit) {
    return List.of(
      ok(Jlvm.evaluate(backends(), SUM, DATA, Map.of(), limit), "evaluate"),
      ok(Jlvm.evaluate(backends(), ok(Jlvm.compile(SUM), "compile"), DATA, Map.of(), limit), "evaluate compiled")
    );
  }

  @Test
  void theMaximumIsMeteredLikeAnyLimit() {
    assertEquals(9007199254740991L, Jlvm.MAX_GAS_LIMIT, "2^53 - 1");
    for (final Jlvm.Outcome o : both(Jlvm.MAX_GAS_LIMIT)) {
      assertEquals(148L, o.gasUsed(), "the gas used at the maximum");
      assertEquals(Optional.of("3"), o.result().success().flatMap(v -> Jlvm.renderValue(v).success()), "the value at the maximum");
    }
    for (final Jlvm.Outcome o : both(147L)) {
      assertEquals(Optional.of("OutOfGas"), o.result().failure().map(JlvmError::kind), "one short of the cost");
      assertEquals(147L, o.gasUsed(), "out of gas uses the whole limit");
    }
  }

  @Test
  void aLimitOutsideTheRangeIsLimit() {
    for (final long limit : new long[] {-1L, Long.MIN_VALUE, Jlvm.MAX_GAS_LIMIT + 1, Long.MAX_VALUE}) {
      for (final Jlvm.Outcome o : both(limit)) {
        assertEquals(Optional.of("Limit"), o.result().failure().map(JlvmError::kind), limit + ": the refusal");
        assertEquals(0L, o.gasUsed(), limit + ": no gas used");
      }
    }
  }
}
