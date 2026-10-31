package xyz.kd5ujc.crypto.zk

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.syntax.all._

import io.circe.parser
import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/** Real SP1 v6.1.0 Groth16 proofs (metakit fixtures) and adversarial mutations of them, on one BN254 engine. */
abstract class Sp1Groth16Checks(engine: Bn254Engine) extends SimpleIOSuite {

  private final case class Fixture(vkey: ByteVector, publicValues: ByteVector, proofBytes: ByteVector)

  private def fixture(name: String): Fixture = {
    val m = parser
      .parse(new String(Files.readAllBytes(Paths.get("docs", "test-vectors", "zk", name)), UTF_8))
      .flatMap(_.as[Map[String, io.circe.Json]])
      .toOption
      .get
    def hx(k: String) = ByteVector.fromValidHex(m(k).asString.get.stripPrefix("0x"))
    Fixture(hx("vkey"), hx("publicValues"), hx("proofBytes"))
  }

  private val premium = fixture("sp1-groth16-premium.json")
  private val shielded = fixture("sp1-groth16-shielded.json")

  private def flip(b: ByteVector, i: Long) = b.update(i, (b(i) ^ 1).toByte)
  private val proofBase = 4L + 32L * 3
  private def setWord(b: ByteVector, idx: Int, v: BigInt) = b.patch(proofBase + 32L * idx, Bn254.be32(v))
  private def word(b:    ByteVector, idx: Int) = BigInt(1, b.slice(proofBase + 32L * idx, proofBase + 32L * (idx + 1)).toArray)

  pureTest("the SP1 verifying key is well formed: G1 points on the curve, G2 points in the subgroup") {
    val vk = Sp1Groth16.VerifyingKey
    expect((vk.alpha +: vk.ic).forall(Bn254.onCurveG1))
      .and(expect(List(vk.beta, vk.gamma, vk.delta).forall(q => Bn254.onCurveG2(q) && Bn254.inSubgroupG2(q))))
      .and(expect.same(5, vk.publicInputs))
  }

  pureTest("real SP1 proofs verify (premium JsonLogic program, shielded transfer)") {
    List(premium, shielded).foldMap(f => expect.same(Right(()), Sp1Groth16.verify(engine)(f.vkey, f.publicValues, f.proofBytes)))
  }

  pureTest("tampering with the proof, public values, selector or program key is rejected") {
    val f = premium
    expect(Sp1Groth16.verify(engine)(f.vkey, f.publicValues, flip(f.proofBytes, f.proofBytes.size - 1)).isLeft, "last proof byte")
      .and(expect.same(Left(Groth16Error.PairingCheckFailed), Sp1Groth16.verify(engine)(f.vkey, flip(f.publicValues, 0), f.proofBytes)))
      .and(
        expect
          .same(
            Left(Groth16Error.Rejected("wrong verifier selector")),
            Sp1Groth16.verify(engine)(f.vkey, f.publicValues, flip(f.proofBytes, 0))
          )
      )
      .and(expect.same(Left(Groth16Error.PairingCheckFailed), Sp1Groth16.verify(engine)(flip(f.vkey, 31), f.publicValues, f.proofBytes)))
      .and(
        expect.same(Left(Groth16Error.PairingCheckFailed), Sp1Groth16.verify(engine)(shielded.vkey, shielded.publicValues, f.proofBytes))
      )
  }

  pureTest("adversarial points: off-curve A and non-subgroup B are invalid proofs; a coordinate >= P is malformed") {
    val f = premium
    val offCurve = setWord(f.proofBytes, 1, (word(f.proofBytes, 1) + 1).mod(Bn254.P))
    // on the twist, outside the order-r subgroup (x = 2 + i)
    val nonSub = List(
      2 -> BigInt(1),
      3 -> BigInt(2),
      4 -> BigInt("19659275751359636165940301690575149581329631496732780143538578556285923319774"),
      5 -> BigInt("7292567877523311580221095596750716176434782432868683424513645834767876293070")
    ).foldLeft(f.proofBytes) { case (b, (i, v)) => setWord(b, i, v) }
    val planted = Bn254.G2(
      Bn254.Fp2(2, 1),
      Bn254.Fp2(
        BigInt("7292567877523311580221095596750716176434782432868683424513645834767876293070"),
        BigInt("19659275751359636165940301690575149581329631496732780143538578556285923319774")
      )
    )
    val nonCanonical = setWord(f.proofBytes, 0, Bn254.P)
    def kind(e: Either[Groth16Error, Unit]) = e.leftMap(_.getClass.getSimpleName)
    expect
      .same(Left("InvalidProof"), kind(Sp1Groth16.verify(engine)(f.vkey, f.publicValues, offCurve)))
      .and(expect(Bn254.onCurveG2(planted) && !Bn254.inSubgroupG2(planted), "the planted point isolates the subgroup check"))
      .and(expect.same(Left(Bn254.ZkError.InvalidPoint("G2 point is not in the order-r subgroup")), Bn254.G2.from(planted.x, planted.y)))
      .and(expect(Bn254.G1.from(1, 3).isLeft, "off-curve G1 is unconstructible"))
      .and(expect(Bn254.G1.from(Bn254.P, 2).isLeft, "non-canonical G1 is unconstructible"))
      .and(
        expect.same(
          Left(Groth16Error.InvalidProof("G2 point is not in the order-r subgroup")),
          Sp1Groth16.verify(engine)(f.vkey, f.publicValues, nonSub)
        )
      )
      .and(expect.same(Left("Malformed"), kind(Sp1Groth16.verify(engine)(f.vkey, f.publicValues, nonCanonical))))
      .and(expect.same(Left("Malformed"), kind(Sp1Groth16.verify(engine)(f.vkey, f.publicValues, f.proofBytes.dropRight(1)))))
  }

  pureTest("pairing algebra: e(G1, G2) e(-G1, G2) = 1, bilinearity, and e(G1, G2) alone is not 1") {
    val g1 = Some(Bn254.G1(1, 2))
    val g2 = Some(Sp1Groth16.VerifyingKey.gamma) // any point of the order-r subgroup
    val (a, b) = (BigInt(123456789), BigInt(987654321))
    expect(Bn254.pairingProductIsOne(List(g1 -> g2, Bn254.negG1(g1) -> g2)))
      .and(expect(Bn254.pairingProductIsOne(List(Bn254.mulG1(g1, a) -> Bn254.mulG2(g2, b), Bn254.negG1(Bn254.mulG1(g1, a * b)) -> g2))))
      .and(expect(!Bn254.pairingProductIsOne(List(g1 -> g2))))
  }
}

object Sp1Groth16Suite extends Sp1Groth16Checks(Bn254Engine.reference)

object Sp1Groth16GnarkSuite
    extends Sp1Groth16Checks(Bn254Engine.gnark[cats.effect.IO].unsafeRunSync()(using cats.effect.unsafe.IORuntime.global))
