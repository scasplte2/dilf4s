import java.math.BigInteger
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import io.circe.{Json, parser}
import scorex.util.encode.Base16
import sigmastate.Values.SigmaBoolean
import sigmastate.crypto.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.crypto.{CryptoConstants, DiffieHellmanTupleProverInput, ProveDHTuple, SigmaProtocolPrivateInput}
import sigmastate.interpreter.{HintsBag, ProverInterpreter}
import sigmastate.serialization.GroupElementSerializer
import sigmastate.{CAND, COR, CTHRESHOLD}
import org.ergoplatform.{ErgoLikeContext, ErgoLikeInterpreter}

/**
 * Ergo's own prover and verifier (sigmastate 5.0.13) over the propositions of docs/specs/sigma.md, in the JSON form
 * JLVM programs use: {"type": "dlog", "pk": "0x.."}, {"type": "dhtuple", "g", "h", "u", "v"},
 * {"type": "and" | "or", "children": [...]}, {"type": "threshold", "k": n, "children": [...]}.
 */
object SigmaVectors {

  private val g = CryptoConstants.dlogGroup.generator
  private def hex(b: Array[Byte]): String = "0x" + Base16.encode(b)
  private def unhex(s: String): Array[Byte] = Base16.decode(s.stripPrefix("0x")).get
  private def point(w: BigInt) = CryptoConstants.dlogGroup.exponentiate(g, w.bigInteger)
  private def pt(p: sigmastate.crypto.CryptoConstants.EcPointType): Json = Json.fromString(hex(GroupElementSerializer.toBytes(p)))

  private class Prover(ws: Seq[SigmaProtocolPrivateInput[_]]) extends ErgoLikeInterpreter with ProverInterpreter {
    override type CTX = ErgoLikeContext
    override lazy val secrets: Seq[SigmaProtocolPrivateInput[_]] = ws
  }
  private object Verifier extends ErgoLikeInterpreter { override type CTX = ErgoLikeContext }

  private def verify(sb: SigmaBoolean, msg: Array[Byte], proof: Array[Byte]): Boolean =
    Verifier.verifySignature(sb, msg, proof)(null)

  // ---- propositions as JSON, both ways

  def toJson(sb: SigmaBoolean): Json = sb match {
    case ProveDlog(pk)            => Json.obj("type" -> Json.fromString("dlog"), "pk" -> pt(pk))
    case ProveDHTuple(gv, h, u, v) => Json.obj("type" -> Json.fromString("dhtuple"), "g" -> pt(gv), "h" -> pt(h), "u" -> pt(u), "v" -> pt(v))
    case CAND(cs)                 => Json.obj("type" -> Json.fromString("and"), "children" -> Json.fromValues(cs.map(toJson)))
    case COR(cs)                  => Json.obj("type" -> Json.fromString("or"), "children" -> Json.fromValues(cs.map(toJson)))
    case CTHRESHOLD(k, cs) =>
      Json.obj("type" -> Json.fromString("threshold"), "k" -> Json.fromInt(k), "children" -> Json.fromValues(cs.map(toJson)))
    case other => sys.error(s"not a proposition of the spec: $other")
  }

  def fromJson(j: Json): SigmaBoolean = {
    val c = j.hcursor
    def p(k: String) = GroupElementSerializer.parse(unhex(c.downField(k).as[String].toOption.get))
    def kids = c.downField("children").as[List[Json]].toOption.get.map(fromJson)
    c.downField("type").as[String].toOption.get match {
      case "dlog"      => ProveDlog(p("pk"))
      case "dhtuple"   => ProveDHTuple(p("g"), p("h"), p("u"), p("v"))
      case "and"       => CAND(kids)
      case "or"        => COR(kids)
      case "threshold" => CTHRESHOLD(c.downField("k").as[Int].toOption.get, kids)
    }
  }

  // ---- the cases: fixed secrets, so the propositions are stable across runs (Ergo's proofs are randomised)

  private val ws: Vector[BigInt] = Vector(
    BigInt("109749205800194830127901595352600384558037183218698112947062497909408298157746"),
    BigInt("50415569076448343263191022044468203756975150511337537963383000142821297891310"),
    BigInt("34648336872573478681093104997365775365807654884817677358848426648354905397359"),
    BigInt("416167686186183758173232992934554728075978573242452195968805863126437865059"),
    BigInt(7),
    BigInt(11),
    BigInt(13)
  )
  private def dl(i: Int): ProveDlog = ProveDlog(point(ws(i)))
  private def din(i: Int): SigmaProtocolPrivateInput[_] = DLogProverInput(ws(i).bigInteger)
  private val (x, y) = (ws(5), ws(6))
  private val (c1, c2) = (point(y), point(x * y))
  private def dht(gv: sigmastate.crypto.CryptoConstants.EcPointType, h: sigmastate.crypto.CryptoConstants.EcPointType,
                  u: sigmastate.crypto.CryptoConstants.EcPointType, v: sigmastate.crypto.CryptoConstants.EcPointType) =
    ProveDHTuple(gv, h, u, v)
  private def dhin(w: BigInt, t: ProveDHTuple): SigmaProtocolPrivateInput[_] = DiffieHellmanTupleProverInput(w.bigInteger, t)

  private val halfMix0 = dht(g, point(x), c2, c1)
  private val halfMix1 = dht(g, point(x), c1, c2)
  private val fullAlice = dht(g, c1, point(x), c2)

  private val cases: List[(String, SigmaBoolean, Seq[SigmaProtocolPrivateInput[_]])] = List(
    ("dlog", dl(0), Seq(din(0))),
    ("dhtuple", fullAlice, Seq(dhin(x, fullAlice))),
    ("and of two", CAND(Seq(dl(0), dl(1))), Seq(din(0), din(1))),
    ("and of one", CAND(Seq(dl(0))), Seq(din(0))),
    ("ring of two, the first signs", COR(Seq(dl(0), dl(1))), Seq(din(0))),
    ("ring of three, the middle signs", COR(Seq(dl(0), dl(1), dl(2))), Seq(din(1))),
    ("ring of three, the last signs", COR(Seq(dl(0), dl(1), dl(2))), Seq(din(2))),
    ("or of one", COR(Seq(dl(3))), Seq(din(3))),
    ("2-of-3, the first two", CTHRESHOLD(2, Seq(dl(0), dl(1), dl(2))), Seq(din(0), din(1))),
    ("2-of-3, the outer two", CTHRESHOLD(2, Seq(dl(0), dl(1), dl(2))), Seq(din(0), din(2))),
    ("1-of-2 threshold", CTHRESHOLD(1, Seq(dl(0), dl(1))), Seq(din(1))),
    ("2-of-2 threshold", CTHRESHOLD(2, Seq(dl(0), dl(1))), Seq(din(0), din(1))),
    ("3-of-5", CTHRESHOLD(3, Seq(dl(0), dl(1), dl(2), dl(3), dl(4))), Seq(din(1), din(3), din(4))),
    ("and with or", CAND(Seq(dl(0), COR(Seq(dl(1), dl(2))))), Seq(din(0), din(2))),
    ("or with and", COR(Seq(dl(0), CAND(Seq(dl(1), dl(2))))), Seq(din(1), din(2))),
    ("or of a threshold and a key", COR(Seq(CTHRESHOLD(2, Seq(dl(0), dl(1), dl(2))), dl(3))), Seq(din(0), din(2))),
    (
      "threshold over connectives and a DH tuple",
      CTHRESHOLD(2, Seq(CAND(Seq(dl(0), dl(1))), COR(Seq(dl(2), dl(3))), fullAlice)),
      Seq(din(3), dhin(x, fullAlice))
    ),
    ("ZeroJoin half-mix, Bob spends", COR(Seq(halfMix0, halfMix1)), Seq(dhin(y, halfMix1))),
    ("ZeroJoin full-mix, Alice spends", COR(Seq(ProveDlog(c2), fullAlice)), Seq(dhin(x, fullAlice))),
    ("ZeroJoin full-mix, Bob spends", COR(Seq(ProveDlog(c1), dht(g, c2, point(x), c1))), Seq(din(6)))
  )

  private val message = unhex("1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00")
  private val other = "another message".getBytes(UTF_8)

  private def scalar(w: BigInteger): Json = {
    val b = w.toByteArray.takeRight(32)
    Json.fromString(hex(Array.fill[Byte](32 - b.length)(0) ++ b))
  }

  private def witness(s: SigmaProtocolPrivateInput[_]): BigInteger = s match {
    case d: DLogProverInput               => d.w
    case d: DiffieHellmanTupleProverInput => d.w
  }

  private def row(name: String, kind: String, sb: SigmaBoolean, secrets: Seq[SigmaProtocolPrivateInput[_]], msg: Array[Byte], proof: Array[Byte]): Json =
    Json.obj(
      "name" -> Json.fromString(name),
      "kind" -> Json.fromString(kind),
      "proposition" -> toJson(sb),
      "secrets" -> Json.fromValues(secrets.map(s => scalar(witness(s)))),
      "message" -> Json.fromString(hex(msg)),
      "proof" -> Json.fromString(hex(proof)),
      "ergo" -> Json.fromBoolean(verify(sb, msg, proof))
    )

  /** Every proof, and its tamperings, with Ergo's verdict on each. */
  private def variants(name: String, sb: SigmaBoolean, secrets: Seq[SigmaProtocolPrivateInput[_]], proof: Array[Byte]): List[Json] = {
    val flipped = proof.clone(); flipped(proof.length - 1) = (flipped(proof.length - 1) ^ 1).toByte
    val challenge = proof.clone(); challenge(0) = (challenge(0) ^ 1).toByte
    List(
      row(name, "valid", sb, secrets, message, proof),
      row(name, "other-message", sb, secrets, other, proof),
      row(name, "flipped-response", sb, secrets, message, flipped),
      row(name, "flipped-challenge", sb, secrets, message, challenge),
      row(name, "trailing-byte", sb, secrets, message, proof :+ 0.toByte),
      row(name, "truncated", sb, secrets, message, proof.dropRight(1))
    )
  }

  private def write(path: String, rows: List[Json], source: String): Unit =
    Files.write(Paths.get(path), Json.obj("spec" -> Json.fromString("docs/specs/sigma.md"), "version" -> Json.fromInt(1), "source" -> Json.fromString(source), "cases" -> Json.fromValues(rows)).spaces2.getBytes(UTF_8))

  def main(args: Array[String]): Unit = args.toList match {
    case "generate" :: out :: Nil =>
      val rows = cases.flatMap { case (name, sb, secrets) =>
        variants(name, sb, secrets, new Prover(secrets).signMessage(sb, message, HintsBag.empty).get)
      }
      require(rows.forall(r => r.hcursor.downField("kind").as[String].toOption.get != "valid" || r.hcursor.downField("ergo").as[Boolean].toOption.get))
      write(out, rows, "sigmastate 5.0.13 prover and verifier (tools/sigma-vectors generate)")
    case "check" :: in :: out :: Nil =>
      val ours = parser.parse(new String(Files.readAllBytes(Paths.get(in)), UTF_8)).toOption.get.hcursor.downField("cases").as[List[Json]].toOption.get
      val rows = ours.flatMap { j =>
        val c = j.hcursor
        val sb = fromJson(c.downField("proposition").as[Json].toOption.get)
        val secrets = c.downField("secrets").as[List[String]].toOption.get.map(w => DLogProverInput(new BigInteger(1, unhex(w))): SigmaProtocolPrivateInput[_])
        variants(c.downField("name").as[String].toOption.get, sb, secrets, unhex(c.downField("proof").as[String].toOption.get))
      }
      write(out, rows, "dilf4s prover, verified by sigmastate 5.0.13 (tools/sigma-vectors check)")
    case _ => sys.error("usage: generate <out.json> | check <dilf4s-proofs.json> <out.json>")
  }
}
