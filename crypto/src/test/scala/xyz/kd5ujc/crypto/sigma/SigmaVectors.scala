package xyz.kd5ujc.crypto.sigma

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalJson, CanonicalValue}

import io.circe.Json
import scodec.bits.ByteVector

/**
 * The sigma vectors (docs/specs/sigma.md): `sigma_v1.json` holds Ergo's proofs, `sigma_dilf4s_v1.json` ours; both
 * carry Ergo's verdict on every case, from tools/sigma-vectors.
 */
object SigmaVectors {

  final case class Case(
    name:    String,
    kind:    String,
    prop:    SigmaProp,
    secrets: Vector[BigInt],
    message: ByteVector,
    proof:   ByteVector,
    ergo:    Boolean
  )

  val ergoPath: Path = Paths.get("shared", "vectors", "sigma_v1.json")
  val oursPath: Path = Paths.get("shared", "vectors", "sigma_dilf4s_v1.json")

  /** Our proofs before Ergo has checked them: the input of `tools/sigma-vectors check`. */
  val proofsPath: Path = Paths.get("target", "sigma_dilf4s_proofs.json")

  private def hex(s: String): Either[String, ByteVector] =
    ByteVector.fromHex(s.stripPrefix("0x")).filter(_ => s.startsWith("0x")).toRight(s"not 0x-hex: $s")

  /** The vector files write bytes as 0x-hex text, as JLVM's JSON surface does. */
  private def bytesIn(v: CanonicalValue): Either[String, CanonicalValue] =
    v match {
      case CText(s) if s.startsWith("0x") => hex(s).map(CBytes(_))
      case CArray(vs)                     => vs.traverse(bytesIn).flatMap(CanonicalValue.array(_).leftMap(_.toString))
      case CMap(m) => m.toList.traverse { case (k, x) => bytesIn(x).map(k -> _) }.flatMap(CanonicalValue.map(_).leftMap(_.toString))
      case other   => other.asRight
    }

  def proposition(v: CanonicalValue): Either[String, SigmaProp] = bytesIn(v).flatMap(SigmaProp.fromCanonical)

  private def field(m: Map[String, CanonicalValue], k: String): Either[String, CanonicalValue] = m.get(k).toRight(s"missing $k")
  private def text(v: CanonicalValue): Either[String, String] = v match {
    case CText(s) => s.asRight
    case other    => s"expected text, got $other".asLeft
  }

  private def parseCase(v: CanonicalValue): Either[String, Case] =
    v match {
      case CMap(m) =>
        for {
          name <- field(m, "name").flatMap(text)
          kind <- field(m, "kind").flatMap(text)
          prop <- field(m, "proposition").flatMap(proposition)
          secrets <- field(m, "secrets").flatMap {
            case CArray(ws) => ws.traverse(text(_).flatMap(hex)).map(_.map(b => BigInt(1, b.toArray)))
            case other      => s"secrets: $other".asLeft
          }
          message <- field(m, "message").flatMap(text).flatMap(hex)
          proof   <- field(m, "proof").flatMap(text).flatMap(hex)
          ergo <- field(m, "ergo").flatMap {
            case CBool(b) => b.asRight
            case other    => s"ergo: $other".asLeft
          }
        } yield Case(name, kind, prop, secrets, message, proof, ergo)
      case other => s"a case is an object, got $other".asLeft
    }

  def load(path: Path): IO[List[Case]] =
    IO(new String(Files.readAllBytes(path), UTF_8)).flatMap { text =>
      CanonicalJson
        .parse(text)
        .leftMap(_.toString)
        .flatMap {
          case CMap(m) =>
            field(m, "cases").flatMap {
              case CArray(cs) => cs.toList.traverse(parseCase)
              case other      => s"cases: $other".asLeft
            }
          case other => s"a vector file is an object, got $other".asLeft
        }
        .leftMap(e => new IllegalArgumentException(s"$path: $e"))
        .liftTo[IO]
    }

  /** Our seed for a case: fixed, so our proofs are reproducible and Ergo's verdict on them stays valid. */
  def seed(name: String): ByteVector = ByteVector.view(s"dilf4s sigma vector: $name".getBytes(UTF_8))
}

/** Writes our proofs for every valid case of sigma_v1.json; then run `tools/sigma-vectors check` on them. */
object SigmaProofsGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    (
      IO(new String(Files.readAllBytes(SigmaVectors.ergoPath), UTF_8)).flatMap(t => io.circe.parser.parse(t).liftTo[IO]),
      SigmaVectors.load(SigmaVectors.ergoPath)
    ).flatMapN { (json, cases) =>
      val propositions =
        json.hcursor.downField("cases").as[List[Json]].toOption.getOrElse(Nil).flatMap(_.hcursor.downField("proposition").as[Json].toOption)
      val rows = cases.zip(propositions).filter(_._1.kind == "valid").traverse {
        case (c, prop) =>
          SigmaProver
            .proveWith(c.prop, c.secrets, c.message, SigmaVectors.seed(c.name))
            .map(proof =>
              Json.obj(
                "name"        -> Json.fromString(c.name),
                "proposition" -> prop,
                "secrets"     -> Json.fromValues(c.secrets.map(w => Json.fromString("0x" + Secp256k1Group.scalarBytes(w).toHex))),
                "message"     -> Json.fromString("0x" + c.message.toHex),
                "proof"       -> Json.fromString("0x" + proof.toHex)
              )
            )
      }
      rows.leftMap(new IllegalStateException(_)).liftTo[IO].flatMap { rs =>
        IO(Files.write(SigmaVectors.proofsPath, Json.obj("cases" -> Json.fromValues(rs)).spaces2.getBytes(UTF_8)))
      }
    }
      .as(ExitCode.Success)
}
