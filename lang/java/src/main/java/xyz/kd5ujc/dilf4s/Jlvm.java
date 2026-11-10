package xyz.kd5ujc.dilf4s;

import java.util.List;
import java.util.Map;

import xyz.kd5ujc.dilf4s.error.JlvmError;
import xyz.kd5ujc.dilf4s.internal.Programs;

/**
 * The JLVM's programs ({@code docs/specs/jlvm.md}, language version 1): a strict, deterministic expression language in
 * JsonLogic syntax. A program has exactly one canonical form ({@code shared/schemas/jlvm.cddl}), its canonical CBOR,
 * and an identity, the tagged hash of that encoding.
 *
 * <p>Evaluation runs on the reference evaluator, which takes the loaded native backends ({@link Backends}): given them,
 * it is a pure function of the program, the data, the bindings, the gas limit and the schedule, and reports the gas it
 * used whether it succeeds or fails. Everything else here needs no backend.
 */
public final class Jlvm {
  private Jlvm() {}

  /** The language version. */
  public static final int VERSION = Programs.Version();

  /** A parsed program: valid by construction, equal to another exactly when their canonical forms are equal. */
  public static final class Program {
    private final xyz.kd5ujc.jlvm.Program scala;

    private Program(xyz.kd5ujc.jlvm.Program scala) {
      this.scala = scala;
    }

    @Override
    public boolean equals(Object other) {
      return other instanceof Program p && Programs.same(p.scala, scala);
    }

    @Override
    public int hashCode() {
      return Programs.hash(scala);
    }

    @Override
    public String toString() {
      return Programs.show(scala);
    }
  }

  /** A program whose special forms are well-formed ({@link #compile}). */
  public static final class Compiled {
    private final xyz.kd5ujc.jlvm.Compiled scala;

    private Compiled(xyz.kd5ujc.jlvm.Compiled scala) {
      this.scala = scala;
    }

    /**
     * The program.
     *
     * @return the program
     */
    public Program program() {
      return new Program(Programs.programOf(scala));
    }
  }

  /**
   * A program from its JSON surface text, bounded in size and depth. The reference's reader recurses once per nesting
   * level of the text, up to the JSON bound, before the program's own bound refuses it: on a thread stack under the
   * JVM's default 1 MB, text nested near that bound can overflow it (the README's "Thread stacks"), as can {@link
   * #decode} and {@link #fromCanonical} on the deepest program under about 768 KB.
   *
   * @param text the program
   * @return the program, or the refusal ({@code Malformed}, {@code Limit})
   */
  public static Result<Program, JlvmError> parse(String text) {
    return text == null ? Result.err(Wrap.nullArgument("program text")) : Programs.parse(text).map(Program::new);
  }

  /**
   * A program's surface text, which {@link #parse} reads back as the same program; a literal holding {@code 0x}-hex
   * text, or a non-terminating rational, has none.
   *
   * @param program the program
   * @return the text, or {@code Malformed}
   */
  public static Result<String, JlvmError> render(Program program) {
    return program == null ? Result.err(Wrap.nullArgument("a program")) : Programs.render(program.scala);
  }

  /**
   * A program's canonical form.
   *
   * @param program the program
   * @return the value, or the refusal
   */
  public static Result<Value, JlvmError> toCanonical(Program program) {
    return program == null ? Result.err(Wrap.nullArgument("a program")) : Programs.canonical(program.scala).map(Wrap::of);
  }

  /**
   * The program a canonical form denotes: heavy forms are refused from their weight first, then the shape, the
   * {@code var} paths and the size bound, in that order.
   *
   * @param value the canonical form
   * @return the program, or the refusal
   */
  public static Result<Program, JlvmError> fromCanonical(Value value) {
    return value == null ? Result.err(Wrap.nullArgument("a canonical value")) : Programs.fromCanonical(Wrap.scala(value)).map(Program::new);
  }

  /**
   * A program's canonical CBOR.
   *
   * @param program the program
   * @return the encoding, or the refusal
   */
  public static Result<byte[], JlvmError> encode(Program program) {
    return program == null ? Result.err(Wrap.nullArgument("a program")) : Programs.encode(program.scala);
  }

  /**
   * The program a canonical CBOR encoding denotes.
   *
   * @param bytes the encoding
   * @return the program, or the refusal
   */
  public static Result<Program, JlvmError> decode(byte[] bytes) {
    return bytes == null ? Result.err(Wrap.nullArgument("bytes")) : Programs.decode(bytes).map(Program::new);
  }

  /**
   * A program's identity: the SHA-256 tagged hash ({@code dilf4s/jlvm/program/v1}) of its canonical encoding.
   *
   * @param program the program
   * @return the id, or the refusal
   */
  public static Result<Hashing.Digest, JlvmError> id(Program program) {
    return id(program, Hashing.Algorithm.SHA_256);
  }

  /**
   * A program's identity under {@code algorithm}.
   *
   * @param program the program
   * @param algorithm the hash
   * @return the id, or the refusal
   */
  public static Result<Hashing.Digest, JlvmError> id(Program program, Hashing.Algorithm algorithm) {
    return program == null || algorithm == null
      ? Result.err(Wrap.nullArgument("a program and an algorithm"))
      : Programs.id(program.scala, algorithm).map(b -> Hashing.digestOf(algorithm, b));
  }

  /**
   * Checks a program's special forms ({@code if}, {@code let}, the callbacks), its {@code let} names and its spreads.
   *
   * @param program the program
   * @return the compiled program, or {@code Malformed}
   */
  public static Result<Compiled, JlvmError> compile(Program program) {
    return program == null ? Result.err(Wrap.nullArgument("a program")) : Programs.compile(program.scala).map(Compiled::new);
  }

  /** The default gas limit: 2^24. */
  public static final long DEFAULT_GAS_LIMIT = Programs.DefaultGasLimit();

  /**
   * The largest gas limit: 2^53 - 1, which every dilf4s language holds exactly. A limit above it, or below 0, is
   * {@code Limit}, with no gas used.
   */
  public static final long MAX_GAS_LIMIT = Programs.MaxGasLimit();

  /** The most distinct warnings an evaluation keeps. */
  public static final int MAX_WARNINGS = Programs.MaxWarnings();

  /** A lint finding of an evaluation: it never changes a result or its gas. */
  public sealed interface Warning permits Warning.AmbiguousPath {

    /**
     * Inside a callback, a plain path resolved in an element while an outer element or the data holds the same first
     * key: {@code $current} (or an explicit outer path) says which is meant.
     *
     * @param path the path, as a preview
     */
    record AmbiguousPath(xyz.kd5ujc.dilf4s.error.Preview path) implements Warning {}
  }

  /**
   * An evaluation's outcome: the gas it used whether it succeeded or failed (out of gas, the whole limit), the value or
   * the refusal, and the warnings.
   *
   * @param gasUsed the gas used
   * @param result the value, or the refusal
   * @param warnings the warnings, distinct, at most {@link #MAX_WARNINGS}
   */
  public record Outcome(long gasUsed, Result<Value, JlvmError> result, List<Warning> warnings) {
    public Outcome {
      warnings = Bytes.listCopy(warnings);
    }
  }

  private static boolean malformed(Backends backends, Value data, Map<String, ? extends Value> env) {
    return backends == null || data == null || env == null
      || env.entrySet().stream().anyMatch(e -> e == null || e.getKey() == null || e.getValue() == null);
  }

  private static List<Map.Entry<String, xyz.kd5ujc.core.canonical.CanonicalValue>> bindings(Map<String, ? extends Value> env) {
    return env.entrySet().stream().map(e -> Map.entry(e.getKey(), Wrap.scala(e.getValue()))).toList();
  }

  private static Outcome outcome(Programs.Evaluated e) {
    return new Outcome(e.gasUsed(), e.result().map(Wrap::of), e.warnings());
  }

  /**
   * Compiles and evaluates {@code program} against {@code data}, with the host bindings {@code env} (read as {@code
   * $name}; {@code let} and callback bindings shadow them), under the published gas schedule. A program that does not
   * compile uses no gas, and a limit outside 0 to {@link #MAX_GAS_LIMIT} is {@code Limit}, with no gas used, before the
   * program is compiled.
   *
   * @param backends the loaded backends
   * @param program the program
   * @param data the input data
   * @param env the host bindings
   * @param gasLimit the gas limit ({@link #DEFAULT_GAS_LIMIT} by default)
   * @return the outcome, or {@code TypeMismatch} for a null argument
   */
  public static Result<Outcome, JlvmError> evaluate(Backends backends, Program program, Value data, Map<String, ? extends Value> env, long gasLimit) {
    return program == null || malformed(backends, data, env)
      ? Result.err(Wrap.nullArgument("backends, a program, data and bindings"))
      : Result.ok(outcome(Programs.evaluate(backends.jlvm, program.scala, Wrap.scala(data), bindings(env), gasLimit)));
  }

  /**
   * {@link #evaluate(Backends, Program, Value, Map, long)} on a program already compiled: a limit outside 0 to {@link
   * #MAX_GAS_LIMIT} is {@code Limit} too.
   *
   * @param backends the loaded backends
   * @param compiled the compiled program
   * @param data the input data
   * @param env the host bindings
   * @param gasLimit the gas limit
   * @return the outcome, or {@code TypeMismatch} for a null argument
   */
  public static Result<Outcome, JlvmError> evaluate(Backends backends, Compiled compiled, Value data, Map<String, ? extends Value> env, long gasLimit) {
    return compiled == null || malformed(backends, data, env)
      ? Result.err(Wrap.nullArgument("backends, a compiled program, data and bindings"))
      : Result.ok(outcome(Programs.evaluateCompiled(backends.jlvm, compiled.scala, Wrap.scala(data), bindings(env), gasLimit)));
  }

  /**
   * Input data or a host binding from its JSON surface: canonical JSON, with {@code 0x}-hex strings (lowercase, an even
   * number of digits) read as bytes.
   *
   * @param text the JSON
   * @return the value, or {@code Malformed}
   */
  public static Result<Value, JlvmError> parseValue(String text) {
    return text == null ? Result.err(Wrap.nullArgument("value text")) : Programs.parseValue(text).map(Wrap::of);
  }

  /**
   * A value's JSON surface text: bytes as {@code 0x}-hex; text that has that form has none, nor does a non-terminating
   * rational.
   *
   * @param value the value
   * @return the text, or {@code Malformed}
   */
  public static Result<String, JlvmError> renderValue(Value value) {
    return value == null ? Result.err(Wrap.nullArgument("a value")) : Programs.renderValue(Wrap.scala(value));
  }
}
