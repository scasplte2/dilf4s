package xyz.kd5ujc.dilf4s.internal

import scala.jdk.CollectionConverters._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.hash.Hasher
import xyz.kd5ujc.dilf4s.Hashing.Algorithm
import xyz.kd5ujc.dilf4s.Result
import xyz.kd5ujc.dilf4s.error.{JlvmError => JJlvmError}
import xyz.kd5ujc.jlvm._

import scodec.bits.ByteVector

/**
 * The JLVM for the Java API (docs/specs/jlvm.md): the surface parser and renderer, the canonical form, its encoding
 * and the program id, the JSON surface of values, compilation, and evaluation on the evaluator the caller loaded
 * (`Jlvm.load[F]`, which loads the native secp256k1, BLS12-381 and gnark backends): given it, evaluation is pure.
 */
object Programs {

  // qualified: the jlvm wildcard import has a Values of its own
  private def jlvm[A](e: Either[JlvmError, A]): Result[A, JJlvmError] =
    xyz.kd5ujc.dilf4s.internal.Values.result(e.left.map(Errors.jlvmError))

  val Version: Int = Program.Version

  def parse(text: String): Result[Program, JJlvmError] = jlvm(Parser.parse(text))
  def render(p:   Program): Result[String, JJlvmError] = jlvm(Parser.render(p))

  def canonical(p:     Program): Result[CanonicalValue, JJlvmError] = jlvm(Parser.canonical(p))
  def fromCanonical(v: CanonicalValue): Result[Program, JJlvmError] = jlvm(Parser.fromCanonical(v))

  def encode(p:     Program): Result[Array[Byte], JJlvmError] = jlvm(Parser.encode(p).map(_.toArray))
  def decode(bytes: Array[Byte]): Result[Program, JJlvmError] = jlvm(Parser.decode(ByteVector(bytes)))

  /** The program's identity: the tagged hash (`dilf4s/jlvm/program/v1`) of its canonical encoding. */
  def id(p: Program, a: Algorithm): Result[Array[Byte], JJlvmError] = idOf(Hashes.hasher(a), p)

  private def idOf[H](h: Hasher[H], p: Program): Result[Array[Byte], JJlvmError] = jlvm(Parser.id[H](p)(using h).map(_.bytes.toArray))

  def compile(p:   Program): Result[Compiled, JJlvmError] = jlvm(Compiled(p))
  def programOf(c: Compiled): Program = c.program

  def same(a: Program, b: Program): Boolean = a == b
  def hash(p: Program): Int = p.hashCode
  def show(p: Program): String = p.toString

  /** Input data or a host binding from its JSON surface: canonical JSON, with `0x`-hex strings as bytes. */
  def parseValue(text: String): Result[CanonicalValue, JJlvmError] = jlvm(JsonSurface.parse(text))

  /** A value's JSON surface text, which [[parseValue]] reads back as the same value. */
  def renderValue(v: CanonicalValue): Result[String, JJlvmError] = jlvm(JsonSurface.render(v))

  // ---- evaluation (the production driver, `Config.Driver.Direct`)

  /** An evaluation's outcome, its value still the reference's: the Java API wraps it. */
  final class Evaluated private[Programs] (
    val gasUsed:  Long,
    val result:   Result[CanonicalValue, JJlvmError],
    val warnings: java.util.List[xyz.kd5ujc.dilf4s.Jlvm.Warning]
  )

  private def warning(w: Warning): xyz.kd5ujc.dilf4s.Jlvm.Warning =
    w match {
      case Warning.AmbiguousPath(path) => new xyz.kd5ujc.dilf4s.Jlvm.Warning.AmbiguousPath(Errors.preview(path))
    }

  private def evaluated(o: Outcome): Evaluated = new Evaluated(o.gasUsed.value, jlvm(o.result), o.warnings.map(warning).asJava)

  private def bindings(env: java.util.List[java.util.Map.Entry[String, CanonicalValue]]): Map[String, CanonicalValue] =
    env.asScala.iterator.map(e => e.getKey -> e.getValue).toMap

  /** `schedule` 1, the only one published (`GasSchedule.Latest`). A negative limit is the reference's: no gas. */
  private def config(gasLimit: Long): Config = Config(limit = GasLimit(xyz.kd5ujc.jlvm.Gas(gasLimit)), schedule = GasSchedule.V1)

  /** Compiles and evaluates: a program that does not compile uses no gas. */
  def evaluate(
    j:        Jlvm,
    p:        Program,
    data:     CanonicalValue,
    env:      java.util.List[java.util.Map.Entry[String, CanonicalValue]],
    gasLimit: Long
  ): Evaluated =
    evaluated(j.evaluate(p, data, bindings(env), config(gasLimit)))

  def evaluateCompiled(
    j:        Jlvm,
    c:        Compiled,
    data:     CanonicalValue,
    env:      java.util.List[java.util.Map.Entry[String, CanonicalValue]],
    gasLimit: Long
  ): Evaluated =
    evaluated(j.evaluate(c, data, bindings(env), config(gasLimit)))

  val DefaultGasLimit: Long = GasLimit.Default.amount.value
  val MaxWarnings: Int = Warning.Max
}
