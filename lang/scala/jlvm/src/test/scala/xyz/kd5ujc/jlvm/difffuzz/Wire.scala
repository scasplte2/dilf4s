package xyz.kd5ujc.jlvm.difffuzz

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.Preview

import io.circe.syntax._
import io.circe.{ACursor, HCursor, Json}
import scodec.bits.ByteVector

enum WireError {
  case Json(cause: io.circe.Error)
  case InvalidHex(input: Preview)
  case InvalidUtf8(cause: Throwable)
  case SourceChoice(name: String, program: Boolean, form: Boolean)
  case Keys(required: Set[String], optional: Set[String], actual: Set[String])
  case CauseCardinality(actual: Int)
}

object WireError {
  final case class Failure(error: WireError) extends RuntimeException(error.toString)

  def bytes(hex: String): Either[WireError, ByteVector] =
    ByteVector.fromHex(hex).toRight(InvalidHex(Preview.of(hex)))
}

/**
 * One case of a differential batch: a program (its JSON surface text, or a canonical form's CBOR read by `decode`), the
 * input data and host bindings as JSON surface text, host bindings built with shared parts (`built-value` recipes,
 * `jlvm-vectors.cddl`), and the gas limit (the default, 2^24, when absent). It is a `jlvm_v1.json` input case, plus the
 * canonical-bytes mode.
 */
final case class FuzzCase(
  name:     String,
  source:   FuzzCase.Source,
  data:     String,
  env:      Vector[(String, String)] = Vector.empty,
  built:    Vector[(String, Json)] = Vector.empty,
  gasLimit: Option[Long] = None
)

object FuzzCase {

  enum Source {

    /** Program text, read by the surface parser. */
    case Surface(text: String)

    /** A canonical form's CBOR, read by `decode` (jlvm.md section 1, "Reading a canonical form"). */
    case Form(bytes: ByteVector)
  }

  /** Text as the hex of its UTF-8, so no transport parser reads it before the JLVM's own parser does. */
  def hexOf(s: String): String = ByteVector(s.getBytes(UTF_8)).toHex

  /** Hex of UTF-8 back to text: ill-formed UTF-8 is refused, never replaced. */
  def textOf(hex: String): Either[WireError, String] =
    WireError
      .bytes(hex)
      .flatMap(b => Either.catchNonFatal(UTF_8.newDecoder().decode(ByteBuffer.wrap(b.toArray)).toString).leftMap(WireError.InvalidUtf8(_)))

  /**
   * The transport line the three runners read (tools/difffuzz/README.md): every text as the hex of its UTF-8, the
   * bindings as `[name, text]` pairs in order, and the recipes as they are in the vectors.
   */
  def transport(c: FuzzCase): String =
    Json
      .fromFields(
        List("name" -> c.name.asJson) ++
        (c.source match {
          case Source.Surface(t) => List("program" -> hexOf(t).asJson)
          case Source.Form(b)    => List("form" -> b.toHex.asJson)
        }) ++
        List("data" -> hexOf(c.data).asJson) ++
        Option.when(c.env.nonEmpty)("env" -> c.env.map { case (k, v) => Json.arr(hexOf(k).asJson, hexOf(v).asJson) }.asJson) ++
        Option.when(c.built.nonEmpty)("built" -> c.built.map { case (k, b) => Json.arr(hexOf(k).asJson, b) }.asJson) ++
        c.gasLimit.map(l => "gasLimit" -> l.asJson)
      )
      .noSpaces

  private def pairs(c: ACursor, f: String => Either[WireError, String]): Either[WireError, Vector[(String, Json)]] =
    c.as[Option[Vector[(String, Json)]]]
      .leftMap(WireError.Json(_))
      .flatMap(_.getOrElse(Vector.empty).traverse { case (k, v) => f(k).map(_ -> v) })

  /** A transport line read back. */
  def fromTransport(line: String): Either[WireError, FuzzCase] =
    io.circe.parser.parse(line).leftMap(WireError.Json(_)).flatMap { j =>
      val c = j.hcursor
      for {
        name <- c.get[String]("name").leftMap(WireError.Json(_))
        source <- (c.get[Option[String]]("program"), c.get[Option[String]]("form")).tupled.leftMap(WireError.Json(_)).flatMap {
          case (Some(p), None) => textOf(p).map(Source.Surface(_))
          case (None, Some(f)) => WireError.bytes(f).map(Source.Form(_))
          case (p, f)          => Left(WireError.SourceChoice(name, p.isDefined, f.isDefined))
        }
        data <- c.get[String]("data").leftMap(WireError.Json(_)).flatMap(textOf)
        env <- pairs(c.downField("env"), textOf).flatMap(_.traverse {
          case (k, v) => v.as[String].leftMap(WireError.Json(_)).flatMap(textOf).map(k -> _)
        })
        built <- pairs(c.downField("built"), textOf)
        gas   <- c.get[Option[Long]]("gasLimit").leftMap(WireError.Json(_))
      } yield FuzzCase(name, source, data, env, built, gas)
    }

  /**
   * The case as a `jlvm_v1.json` input row (`jlvm-vectors.cddl` `jlvm-case`, without its outcome), with `form` (hex)
   * in place of `program` for a canonical form: what a finding is written as, and what `--replay` reads.
   */
  def row(c: FuzzCase): Json =
    Json.fromFields(
      List("name" -> c.name.asJson) ++
      (c.source match {
        case Source.Surface(t) => List("program" -> t.asJson)
        case Source.Form(b)    => List("form" -> b.toHex.asJson)
      }) ++
      List("data" -> c.data.asJson) ++
      Option.when(c.env.nonEmpty)("env" -> Json.fromFields(c.env.map { case (k, v) => k -> v.asJson })) ++
      Option.when(c.built.nonEmpty)("built" -> Json.fromFields(c.built)) ++
      c.gasLimit.map(l => "gasLimit" -> l.asJson)
    )

  /** A `jlvm_v1.json` case, or a finding row, as a case; extra fields (an outcome, gas) are ignored. */
  def fromRow(c: HCursor): Either[WireError, FuzzCase] =
    for {
      name <- c.get[String]("name").leftMap(WireError.Json(_))
      source <- (c.get[Option[String]]("program"), c.get[Option[String]]("form")).tupled.leftMap(WireError.Json(_)).flatMap {
        case (Some(p), None) => Right(Source.Surface(p))
        case (None, Some(f)) => WireError.bytes(f).map(Source.Form(_))
        case (p, f)          => Left(WireError.SourceChoice(name, p.isDefined, f.isDefined))
      }
      data  <- c.get[String]("data").leftMap(WireError.Json(_))
      env   <- c.get[Option[Map[String, String]]]("env").leftMap(WireError.Json(_))
      built <- c.get[Option[Map[String, Json]]]("built").leftMap(WireError.Json(_))
      gas   <- c.get[Option[Long]]("gasLimit").leftMap(WireError.Json(_))
    } yield
      FuzzCase(
        name,
        source,
        data,
        env.getOrElse(Map.empty).toVector.sortBy(_._1),
        built.getOrElse(Map.empty).toVector.sortBy(_._1),
        gas
      )
}

/**
 * What a runner reports for one case: exactly the fields the vectors pin (docs/specs/jlvm.md section 3), and the ones
 * as normative that they leave out. `program` is the canonical CBOR of the program the text parsed to (or the form
 * decoded to), which is its identity (section 1); `outcome` is a value's canonical CBOR, or an error's kind, its `op`
 * for the kinds that carry one and a `Crypto` error's `cause`; `available` is an `OutOfGas` error's limit; `gasUsed`;
 * and `warnings`, each warning's path preview by its two normative fields (conformance.md section 8), in the order
 * recorded. The vectors pin the warnings' count. Messages and `OutOfGas.required` are informative and never reported.
 */
final case class Record(
  name:      String,
  program:   Option[String],
  outcome:   Record.Outcome,
  available: Option[Long],
  gasUsed:   Long,
  warnings:  Vector[(String, Long)]
)

object Record {

  enum Outcome {
    case Value(cbor: String)

    /** `cause` is the family, in lower camel case (`poseidonMerkle`), and that family's kind. */
    case Error(kind: String, op: Option[String], cause: Option[(String, String)])
  }

  def outcomeJson(o: Outcome): Json = o match {
    case Outcome.Value(cbor) => Json.obj("cbor" -> cbor.asJson)
    case Outcome.Error(kind, op, cause) =>
      Json.fromFields(
        List("error" -> kind.asJson) ++ op.map("op" -> _.asJson) ++ cause.map { case (f, k) => "cause" -> Json.obj(f -> k.asJson) }
      )
  }

  def json(r: Record): Json =
    Json.fromFields(
      List("name" -> r.name.asJson) ++ r.program.map("program" -> _.asJson) ++ r.available.map("available" -> _.asJson) ++ List(
        "outcome"  -> outcomeJson(r.outcome),
        "gasUsed"  -> r.gasUsed.asJson,
        "warnings" -> r.warnings.map { case (h, l) => Json.arr(h.asJson, l.asJson) }.asJson
      )
    )

  /** Exactly these keys, so a runner reporting something more (or less) is a fault, not a pass. */
  private def exactly(c: HCursor, required: Set[String], optional: Set[String]): Either[WireError, Unit] = {
    val keys = c.keys.fold(Set.empty[String])(_.toSet)
    Either.cond(
      required.subsetOf(keys) && keys.subsetOf(required ++ optional),
      (),
      WireError.Keys(required, optional, keys)
    )
  }

  private def readOutcome(c: HCursor): Either[WireError, Outcome] =
    c.downField("cbor")
      .success
      .fold {
        for {
          _    <- exactly(c, Set("error"), Set("op", "cause"))
          kind <- c.get[String]("error").leftMap(WireError.Json(_))
          op   <- c.get[Option[String]]("op").leftMap(WireError.Json(_))
          cause <- c.get[Option[Map[String, String]]]("cause").leftMap(WireError.Json(_)).flatMap {
            case None                     => Right(None)
            case Some(m) if m.sizeIs == 1 => Right(m.headOption)
            case Some(m)                  => Left(WireError.CauseCardinality(m.size))
          }
        } yield Outcome.Error(kind, op, cause)
      }(v => exactly(c, Set("cbor"), Set.empty) *> v.as[String].leftMap(WireError.Json(_)).map(Outcome.Value(_)))

  def fromLine(line: String): Either[WireError, Record] =
    io.circe.parser.parse(line).leftMap(WireError.Json(_)).flatMap { j =>
      val c = j.hcursor
      for {
        _           <- exactly(c, Set("name", "outcome", "gasUsed", "warnings"), Set("program", "available"))
        name        <- c.get[String]("name").leftMap(WireError.Json(_))
        program     <- c.get[Option[String]]("program").leftMap(WireError.Json(_))
        outcomeJson <- c.get[Json]("outcome").leftMap(WireError.Json(_))
        outcome     <- readOutcome(outcomeJson.hcursor)
        available   <- c.get[Option[Long]]("available").leftMap(WireError.Json(_))
        gas         <- c.get[Long]("gasUsed").leftMap(WireError.Json(_))
        warnings    <- c.get[Vector[(String, Long)]]("warnings").leftMap(WireError.Json(_))
      } yield Record(name, program, outcome, available, gas, warnings)
    }
}
