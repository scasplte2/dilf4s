package xyz.kd5ujc.jlvm.difffuzz

import java.io.{BufferedReader, InputStreamReader, OutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.{BuiltValues, CanonicalValue, Cbor}
import xyz.kd5ujc.core.circe.CanonicalCirce
import xyz.kd5ujc.jlvm._

/**
 * The reference's side of the differential harness: one case to one [[Record]], as `JlvmVectors.run` evaluates a
 * vector case. The program is parsed (or its form decoded), then the data, then each binding in order; the first that
 * fails is the outcome, with no gas. The bindings built with shared parts are added, and the program is evaluated with
 * the case's gas limit and the latest schedule. The driver calls this in process; [[Runner]] is the same function as
 * a program, for a batch in a file.
 */
object Evaluate {

  private def error(e: JlvmError): Record.Outcome =
    Record.Outcome.Error(
      e.kind,
      e.site.map(_.wire),
      JlvmVectors.cause(e).flatMap(_.asObject).flatMap(_.toList.headOption).map {
        case (family, kind) => family -> kind.asString.getOrElse(kind.noSpaces)
      }
    )

  private def value(v: CanonicalValue): Either[String, Record.Outcome] =
    Cbor.encode(v).bimap(e => s"a result with no encoding: $e", b => Record.Outcome.Value(b.toHex))

  /** A recipe that does not build is a broken case file, never an outcome: `Left`, as a runner fault. */
  private def built(name: String, recipe: io.circe.Json): Either[String, (String, CanonicalValue)] =
    CanonicalCirce.fromCirce(recipe).leftMap(_.toString).flatMap(BuiltValues.built).bimap(e => s"binding $name: $e", name -> _)

  def record(jlvm: Jlvm, c: FuzzCase): Either[String, Record] = {
    val failed = (program: Option[String], e: JlvmError) => Record(c.name, program, error(e), None, 0L, Vector.empty)
    val parsed = c.source match {
      case FuzzCase.Source.Surface(text) => Parser.parse(text)
      case FuzzCase.Source.Form(bytes)   => Parser.decode(bytes)
    }
    parsed.fold(
      e => Right(failed(None, e)),
      program => {
        val form = Parser.encode(program).toOption.map(_.toHex)
        val inputs = (JsonSurface.parse(c.data), c.env.traverse { case (k, v) => JsonSurface.parse(v).map(k -> _) }).tupled
        inputs.fold(
          e => Right(failed(form, e)),
          {
            case (data, env) =>
              c.built.traverse { case (k, b) => built(k, b) }.flatMap { heavy =>
                val limit = c.gasLimit.fold(GasLimit.Default)(l => GasLimit(Gas(l)))
                val o = jlvm.evaluate(program, data, (env ++ heavy).toMap, Config(limit = limit))
                o.result
                  .fold(e => Right(error(e)), value)
                  .map(
                    Record(
                      c.name,
                      form,
                      _,
                      o.result.left.toOption.collect { case JlvmError.OutOfGas(_, available) => available },
                      o.gasUsed.value,
                      o.warnings.map { case Warning.AmbiguousPath(p) => p.head -> p.length }
                    )
                  )
              }
          }
        )
      }
    )
  }

  /** The reference's message for a case's error, if it fails: informative, for inspecting a run (`--dump`). */
  def message(jlvm: Jlvm, c: FuzzCase): Option[String] =
    (c.source match {
      case FuzzCase.Source.Surface(text) => Parser.parse(text)
      case FuzzCase.Source.Form(bytes)   => Parser.decode(bytes)
    }).fold(
      e => Some(e.message),
      p =>
        (JsonSurface.parse(c.data), c.env.traverse { case (k, v) => JsonSurface.parse(v).map(k -> _) }).tupled.fold(
          e => Some(e.message),
          {
            case (d, env) =>
              jlvm
                .evaluate(p, d, env.toMap, Config(limit = c.gasLimit.fold(GasLimit.Default)(l => GasLimit(Gas(l)))))
                .result
                .left
                .toOption
                .map(_.message)
          }
        )
    )

  /**
   * Whether the reference's record `r` of `c` keeps its own metering rules (jlvm.md section 7), judged from the case
   * alone, so it applies to a shrunk case as to the one generated: out of gas, the gas used and `available` are the
   * limit; otherwise the gas used is within it; and below the default limit, the default gives the same record, or
   * runs past the limit if this one ran out. A limit at the exact cost or one below it is where these are sharpest.
   */
  def metered(reference: FuzzCase => Record, c: FuzzCase, r: Record): Boolean = {
    val default = GasLimit.Default.amount.value
    val limit = c.gasLimit.getOrElse(default)
    val starved = r.outcome match {
      case Record.Outcome.Error("OutOfGas", None, None) => true
      case _                                            => false
    }
    lazy val free = reference(c.copy(gasLimit = None))
    if (starved) r.gasUsed == limit && r.available.contains(limit) && (limit >= default || free.gasUsed > limit)
    else r.gasUsed <= limit && r.available.isEmpty && (limit >= default || free.copy(name = r.name) == r)
  }

  /** One transport line to one record line; a line that cannot be read or run is a runner fault, `Left`. */
  def line(jlvm: Jlvm, l: String): Either[String, String] =
    FuzzCase.fromTransport(l).flatMap(record(jlvm, _)).map(r => Record.json(r).noSpaces)
}

/**
 * `tools/sbt "jlvm/Test/runMain xyz.kd5ujc.jlvm.difffuzz.Runner [cases.jsonl]"`: reads transport lines (from the file, or
 * standard input), writes one record line per case to standard output as UTF-8, flushed per record, so a driver can
 * stream to it. A line it cannot read or run ends the run with a non-zero exit.
 */
object Runner extends IOApp {

  private def lines(args: List[String]): Resource[IO, BufferedReader] =
    args.headOption.fold(Resource.eval(IO(new BufferedReader(new InputStreamReader(System.in, UTF_8))))) { f =>
      Resource.fromAutoCloseable(IO.blocking(Files.newBufferedReader(Paths.get(f), UTF_8)))
    }

  private def put(out: OutputStream, s: String): IO[Unit] =
    IO.blocking {
      out.write((s + "\n").getBytes(UTF_8))
      out.flush()
    }

  def run(args: List[String]): IO[ExitCode] =
    (Resource.eval(Jlvm.load[IO]), lines(args)).tupled.use {
      case (jlvm, in) =>
        val out: OutputStream = System.out
        def loop: IO[ExitCode] =
          IO.blocking(Option(in.readLine())).flatMap {
            case None                 => IO.pure(ExitCode.Success)
            case Some(l) if l.isEmpty => loop
            case Some(l) =>
              Evaluate.line(jlvm, l).fold(e => IO.blocking(System.err.println(s"runner: $e")).as(ExitCode.Error), put(out, _) >> loop)
          }
        loop
    }
}
