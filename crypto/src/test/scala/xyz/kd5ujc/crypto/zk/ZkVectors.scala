package xyz.kd5ujc.crypto.zk

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}

import io.circe.syntax._
import io.circe.{Json, parser}
import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/**
 * Conformance vectors for docs/specs/zk.md sections 1 and 2, from the reference engine: the BN254 operations on
 * exact-width bytes and the SP1 Groth16 verifier, each with the outcome a conforming implementation must give.
 */
object ZkVectors {

  val path: Path = Paths.get("shared", "vectors", "zk_v1.json")

  private def get[A](e: Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)

  private def read(name: String): Json =
    get(parser.parse(new String(Files.readAllBytes(Paths.get("docs", "test-vectors", "zk", name)), UTF_8)))

  /** go-ethereum's precompile vectors as (name, input); the precompiles zero-pad short input, which is applied here. */
  private def geth(name: String): List[(String, ByteVector)] =
    read(s"$name.json").asArray.toList.flatten.map { m =>
      (get(m.hcursor.get[String]("Name")), ByteVector.fromValidHex(get(m.hcursor.get[String]("Input"))))
    }

  private val engine = Bn254Engine.reference

  private def word(x: BigInt): ByteVector = Bn254.be32(x)
  private val g1: ByteVector = word(1) ++ word(2)
  private val offCurve: ByteVector = word(1) ++ word(3)
  private val nonCanonical: ByteVector = word(Bn254.P) ++ word(2)
  private val g2: ByteVector = Bn254.encodeG2(Some(Sp1Groth16.VerifyingKey.gamma))
  private val offTwist: ByteVector = g2.update(127, (g2(127) ^ 1).toByte)

  /** On the twist but outside the order-r subgroup (x = 2 + i), as Sp1Groth16Suite plants it. */
  private val nonSubgroup: ByteVector = Bn254.encodeG2(
    Some(
      Bn254.G2(
        Bn254.Fp2(2, 1),
        Bn254.Fp2(
          BigInt("7292567877523311580221095596750716176434782432868683424513645834767876293070"),
          BigInt("19659275751359636165940301690575149581329631496732780143538578556285923319774")
        )
      )
    )
  )

  private def kind(e: Bn254.ZkError): String = e match {
    case Bn254.ZkError.Malformed(_)    => "Malformed"
    case Bn254.ZkError.InvalidPoint(_) => "InvalidPoint"
  }

  private def point(r: Either[Bn254.ZkError, ByteVector]): Json =
    r.fold(e => Json.obj("error" -> kind(e).asJson), p => Json.obj("point" -> p.toHex.asJson))

  private def add(name: String, a: ByteVector, b: ByteVector): Json =
    Json.obj("name" -> name.asJson, "a" -> a.toHex.asJson, "b" -> b.toHex.asJson, "result" -> point(engine.add(a, b)))

  private def mul(name: String, p: ByteVector, k: ByteVector): Json =
    Json.obj("name" -> name.asJson, "point" -> p.toHex.asJson, "scalar" -> k.toHex.asJson, "result" -> point(engine.mul(p, k)))

  private def pairing(name: String, pairs: Vector[(ByteVector, ByteVector)]): Json =
    Json.obj(
      "name"   -> name.asJson,
      "pairs"  -> pairs.map { case (a, b) => Json.obj("g1" -> a.toHex.asJson, "g2" -> b.toHex.asJson) }.asJson,
      "result" -> engine.pairing(pairs).fold(e => Json.obj("error" -> kind(e).asJson), v => Json.obj("value" -> v.asJson))
    )

  val adds: List[Json] =
    geth("bn256Add").map { case (n, in) => val x = in.take(128).padRight(128); add(s"go-ethereum $n", x.take(64), x.drop(64)) } ++
    List(
      add("a 63-byte point", g1.drop(1), g1),
      add("a coordinate equal to p", nonCanonical, g1),
      add("a point off the curve", g1, offCurve),
      add("the identity and G", ByteVector.low(64), g1)
    )

  val muls: List[Json] =
    geth("bn256ScalarMul").map { case (n, in) => val x = in.take(96).padRight(96); mul(s"go-ethereum $n", x.take(64), x.drop(64)) } ++
    List(
      mul("a 31-byte scalar", g1, word(2).drop(1)),
      mul("a point off the curve", offCurve, word(2)),
      mul("r times G is the identity", g1, word(Bn254.R)),
      mul("the largest 256-bit scalar", g1, ByteVector.high(32))
    )

  val pairings: List[Json] =
    geth("bn256Pairing").filter(_._2.size % 192 == 0).map {
      case (n, in) => pairing(s"go-ethereum $n", in.grouped(192).map(c => c.take(64) -> c.drop(64)).toVector)
    } ++
    List(
      pairing("no pairs", Vector.empty),
      pairing("e(G, Q) alone", Vector(g1 -> g2)),
      pairing("e(G, Q) e(-G, Q)", Vector(g1 -> g2, Bn254.encodeG1(Bn254.negG1(Some(Bn254.G1(1, 2)))) -> g2)),
      pairing("a G2 point off the twist", Vector(g1 -> offTwist)),
      pairing("a G2 point outside the subgroup", Vector(g1 -> nonSubgroup)),
      pairing("a G1 point off the curve", Vector(offCurve -> g2)),
      pairing("a 127-byte G2 point", Vector(g1 -> g2.drop(1)))
    )

  // ---- SP1 Groth16

  private final case class Fixture(vkey: ByteVector, publicValues: ByteVector, proofBytes: ByteVector)

  private def fixture(name: String): Fixture = {
    val c = read(name).hcursor
    def hx(k: String) = ByteVector.fromValidHex(get(c.get[String](k)).stripPrefix("0x"))
    Fixture(hx("vkey"), hx("publicValues"), hx("proofBytes"))
  }

  private val premium = fixture("sp1-groth16-premium.json")
  private val shielded = fixture("sp1-groth16-shielded.json")

  private def flip(b:    ByteVector, i: Long): ByteVector = b.update(i, (b(i) ^ 1).toByte)
  private def setWord(b: ByteVector, i: Int, v: BigInt): ByteVector = b.patch(4L + 32L * i, word(v))
  private def wordAt(b:  ByteVector, i: Int): BigInt = BigInt(1, b.slice(4L + 32L * i, 4L + 32L * (i + 1)).toArray)

  private def groth16Kind(r: Either[Groth16Error, Unit]): String = r match {
    case Right(())                             => "verified"
    case Left(Groth16Error.Malformed(_))       => "Malformed"
    case Left(Groth16Error.InvalidProof(_))    => "InvalidProof"
    case Left(Groth16Error.PairingCheckFailed) => "PairingCheckFailed"
    case Left(Groth16Error.Rejected(_))        => "Rejected"
  }

  private def sp1(name: String, vkey: ByteVector, publicValues: ByteVector, proofBytes: ByteVector): Json =
    Json.obj(
      "name"         -> name.asJson,
      "programVKey"  -> vkey.toHex.asJson,
      "publicValues" -> publicValues.toHex.asJson,
      "proofBytes"   -> proofBytes.toHex.asJson,
      "result"       -> groth16Kind(Sp1Groth16.verify(engine)(vkey, publicValues, proofBytes)).asJson
    )

  val sp1Groth16: List[Json] = {
    val f = premium
    val p = f.proofBytes
    // proof words start at word 3 (after exit code, VK root, nonce): A.x, A.y, B (4 words), C.x, C.y
    val nonSub = List(
      5 -> BigInt(1),
      6 -> BigInt(2),
      7 -> BigInt("19659275751359636165940301690575149581329631496732780143538578556285923319774"),
      8 -> BigInt("7292567877523311580221095596750716176434782432868683424513645834767876293070")
    ).foldLeft(p) { case (b, (i, v)) => setWord(b, i, v) }
    List(
      sp1("premium: verifies", f.vkey, f.publicValues, p),
      sp1("shielded: verifies", shielded.vkey, shielded.publicValues, shielded.proofBytes),
      sp1("the last proof byte flipped", f.vkey, f.publicValues, flip(p, p.size - 1)),
      sp1("a public value flipped", f.vkey, flip(f.publicValues, 0), p),
      sp1("another program key", flip(f.vkey, 31), f.publicValues, p),
      sp1("another program's proof", shielded.vkey, shielded.publicValues, p),
      sp1("a wrong selector", f.vkey, f.publicValues, flip(p, 0)),
      sp1("a non-zero exit code", f.vkey, f.publicValues, setWord(p, 0, 1)),
      sp1("a wrong VK root", f.vkey, f.publicValues, setWord(p, 1, wordAt(p, 1) + 1)),
      sp1("A off the curve", f.vkey, f.publicValues, setWord(p, 4, (wordAt(p, 4) + 1).mod(Bn254.P))),
      sp1("A the identity", f.vkey, f.publicValues, setWord(setWord(p, 3, 0), 4, 0)),
      sp1("B outside the subgroup", f.vkey, f.publicValues, nonSub),
      sp1("A.x equal to p", f.vkey, f.publicValues, setWord(p, 3, Bn254.P)),
      sp1("a program key not in Fr", ByteVector.high(32), f.publicValues, p),
      sp1("a 31-byte program key", f.vkey.drop(1), f.publicValues, p),
      sp1("a proof one byte short", f.vkey, f.publicValues, p.dropRight(1))
    )
  }

  def render: String =
    Json
      .obj(
        "spec"    -> "docs/specs/zk.md".asJson,
        "version" -> 1.asJson,
        "source" -> "go-ethereum EIP-196/197 precompile vectors and metakit's SP1 v6.1.0 proofs (docs/test-vectors/zk), with dilf4s's own malformed and invalid inputs (ZkVectors)".asJson,
        "add"        -> adds.asJson,
        "mul"        -> muls.asJson,
        "pairing"    -> pairings.asJson,
        "sp1Groth16" -> sp1Groth16.asJson
      )
      .spaces2 + "\n"
}

object ZkVectorGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    IO.blocking(Files.write(ZkVectors.path, ZkVectors.render.getBytes(UTF_8))).as(ExitCode.Success)
}

object ZkVectorsSuite extends SimpleIOSuite {
  test("committed zk vectors match the reference implementation") {
    IO.blocking(new String(Files.readAllBytes(ZkVectors.path), UTF_8)).map { committed =>
      expect(
        committed == ZkVectors.render,
        s"${ZkVectors.path} is stale; regenerate with: sbt \"crypto/Test/runMain xyz.kd5ujc.crypto.zk.ZkVectorGen\""
      )
    }
  }
}
