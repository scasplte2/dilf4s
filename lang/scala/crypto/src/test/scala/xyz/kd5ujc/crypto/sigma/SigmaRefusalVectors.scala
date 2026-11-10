package xyz.kd5ujc.crypto.sigma

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}

import io.circe.Json
import io.circe.syntax._
import scodec.bits.{ByteVector, hex}
import weaver.SimpleIOSuite

/**
 * `shared/vectors/sigma_refusals_v1.json` (docs/specs/sigma.md §6): calls to `sigma_verify` with the reference's
 * outcome, a refusal by its [[SigmaError]] kind; and our outcome on every case of the two Ergo-checked vector files,
 * whose tamperings record only Ergo's verdict.
 */
object SigmaRefusalVectors {

  val path: Path = Paths.get("shared", "vectors", "sigma_refusals_v1.json")

  /** A proposition's canonical value, then its proof checked against it: `accepted`, `rejected`, or the kind. */
  def outcome(proposition: CanonicalValue, proof: ByteVector, message: ByteVector): String =
    SigmaProp.decode(proposition).flatMap(Sigma.check(_, proof, message)).fold(_.productPrefix, ok => if (ok) "accepted" else "rejected")

  private def get[A](e: Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)

  private def obj(fields: (String, CanonicalValue)*): CanonicalValue = get(CanonicalValue.map(fields))
  private def text(s:     String): CanonicalValue = get(CanonicalValue.text(s))
  private def array(vs:   CanonicalValue*): CanonicalValue = get(CanonicalValue.array(vs.toVector))

  private def dlogValue(pk: ByteVector): CanonicalValue = obj("type" -> text("dlog"), "pk" -> CBytes(pk))

  private def row(name: String, proposition: CanonicalValue, proof: ByteVector, message: ByteVector, expected: String): Json = {
    val actual = outcome(proposition, proof, message)
    require(actual == expected, s"$name: expected $expected, got $actual")
    Json.obj(
      "name"        -> name.asJson,
      "proposition" -> Cbor.encode(proposition).fold(e => sys.error(e.toString), _.toHex).asJson,
      "proof"       -> proof.toHex.asJson,
      "message"     -> message.toHex.asJson,
      "outcome"     -> actual.asJson
    )
  }

  def cases: List[Json] = {
    val w = BigInt(42)
    val pk = Secp256k1Group.base(w).bytes
    val prop = get(SigmaProp.dlog(Secp256k1Group.base(w)))
    val message = hex"7369676d61"
    val proof = get(SigmaProver.proveWith(prop, Vector(w), message, SigmaVectors.seed("refusals")))
    val good = dlogValue(pk)
    // an x with no point on the curve: the first such from 1 up
    val offCurve = LazyList
      .from(1)
      .map(x => ByteVector(0x02) ++ ByteVector.view(BigInt(x).toByteArray).dropWhile(_ == 0).padLeft(32))
      .find(Secp256k1Group.decode(_).isLeft)
      .getOrElse(sys.error("unreachable"))
    val zMax = proof.take(Sigma.ChallengeSize.toLong) ++ ByteVector.fill(32)(0xff)
    List(
      row("valid", good, proof, message, "accepted"),
      row("other-message", good, proof, message.reverse, "rejected"),
      row("not-an-object", CInt(5), proof, message, "InvalidProposition"),
      row("unknown-type", obj("type" -> text("schnorr"), "pk" -> CBytes(pk)), proof, message, "InvalidProposition"),
      row("missing-field", obj("type" -> text("dlog")), proof, message, "InvalidProposition"),
      row(
        "extra-field",
        obj("type" -> text("dlog"), "pk" -> CBytes(pk), "k" -> CInt(1)),
        proof,
        message,
        "InvalidProposition"
      ),
      row("pk-not-bytes", obj("type" -> text("dlog"), "pk" -> text("02")), proof, message, "InvalidProposition"),
      row("identity-pk", dlogValue(ByteVector.low(33)), proof, message, "InvalidProposition"),
      row("empty-and", obj("type" -> text("and"), "children" -> array()), proof, message, "InvalidProposition"),
      row(
        "threshold-k-0",
        obj("type" -> text("threshold"), "k" -> CInt(0), "children" -> array(good)),
        proof,
        message,
        "InvalidProposition"
      ),
      row(
        "threshold-k-over-n",
        obj("type" -> text("threshold"), "k" -> CInt(2), "children" -> array(good)),
        proof,
        message,
        "InvalidProposition"
      ),
      row("pk-32-bytes", dlogValue(pk.drop(1)), proof, message, "InvalidPoint"),
      row("pk-prefix-05", dlogValue(ByteVector(0x05) ++ pk.drop(1)), proof, message, "InvalidPoint"),
      row("pk-off-curve", dlogValue(offCurve), proof, message, "InvalidPoint"),
      row("short-proof", good, proof.dropRight(1), message, "ProofLength"),
      row("long-proof", good, proof :+ 0.toByte, message, "ProofLength"),
      row("empty-proof", good, ByteVector.empty, message, "ProofLength"),
      row("response-out-of-range", good, zMax, message, "ResponseOutOfRange"),
      // the proposition is decoded before the proof is read
      row("bad-proposition-and-short-proof", dlogValue(ByteVector.low(33)), proof.dropRight(1), message, "InvalidProposition"),
      row("bad-point-and-short-proof", dlogValue(offCurve), proof.dropRight(1), message, "InvalidPoint")
    )
  }

  /** Our outcome on every case of a sigma vector file, in its order. */
  private def ours(file: Path): IO[List[String]] =
    SigmaVectors
      .load(file)
      .map(_.map(c => Sigma.check(c.prop, c.proof, c.message).fold(_.productPrefix, ok => if (ok) "accepted" else "rejected")))

  val render: IO[String] =
    (ours(SigmaVectors.ergoPath), ours(SigmaVectors.oursPath)).mapN { (ergo, dilf4s) =>
      Json
        .obj(
          "spec"        -> "docs/specs/sigma.md".asJson,
          "version"     -> 1.asJson,
          "cases"       -> cases.asJson,
          "ergoCases"   -> ergo.asJson,
          "dilf4sCases" -> dilf4s.asJson
        )
        .spaces2 + "\n"
    }
}

object SigmaRefusalVectorGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    SigmaRefusalVectors.render.flatMap(r => IO(Files.write(SigmaRefusalVectors.path, r.getBytes(UTF_8)))).as(ExitCode.Success)
}

object SigmaRefusalVectorsSuite extends SimpleIOSuite {
  test("committed sigma refusal vectors match the reference implementation") {
    (IO(new String(Files.readAllBytes(SigmaRefusalVectors.path), UTF_8)), SigmaRefusalVectors.render).mapN { (committed, rendered) =>
      expect(
        committed == rendered,
        s"${SigmaRefusalVectors.path} is stale; regenerate with: tools/sbt \"crypto/Test/runMain xyz.kd5ujc.crypto.sigma.SigmaRefusalVectorGen\""
      )
    }
  }
}
