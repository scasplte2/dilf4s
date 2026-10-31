package xyz.kd5ujc.crypto.zk

import cats.syntax.all._

import xyz.kd5ujc.core.hash.{Hasher, Sha256}
import xyz.kd5ujc.crypto.zk.Bn254.{G1, G2, ZkError}

import scodec.bits.{ByteVector, hex}

sealed trait Groth16Error extends Product with Serializable

object Groth16Error {

  /** The bytes are not a proof or input of the expected shape (lengths, non-canonical field elements). */
  final case class Malformed(reason: String) extends Groth16Error

  /** Well-formed but invalid: off-curve, outside the subgroup, the identity, or an input outside Fr. */
  final case class InvalidProof(reason: String) extends Groth16Error
  case object PairingCheckFailed extends Groth16Error

  /** A framing check of a wrapper (such as SP1's selector, exit code or VK root) failed. */
  final case class Rejected(reason: String) extends Groth16Error

  private[zk] def fromZk(e: ZkError): Groth16Error = e match {
    case ZkError.Malformed(r)    => Malformed(r)
    case ZkError.InvalidPoint(r) => InvalidProof(r)
  }
}

/**
 * A Groth16 verifying key over BN254: `ic(0) + sum_i x_i ic(i + 1)` is the public-input commitment. Its points are
 * valid by construction ([[G1]], [[G2]]), so only the shape is checked here.
 */
final case class Groth16VerifyingKey private[zk] (alpha: G1, beta: G2, gamma: G2, delta: G2, ic: Vector[G1]) {
  def publicInputs: Int = ic.size - 1

  private[zk] lazy val alphaBytes: ByteVector = Bn254.encodeG1(Some(alpha))
  private[zk] lazy val betaBytes: ByteVector = Bn254.encodeG2(Some(beta))
  private[zk] lazy val gammaBytes: ByteVector = Bn254.encodeG2(Some(gamma))
  private[zk] lazy val deltaBytes: ByteVector = Bn254.encodeG2(Some(delta))
  private[zk] def icBytes(i: Int): ByteVector = Bn254.encodeG1(ic.lift(i))
}

object Groth16VerifyingKey {
  def from(alpha: G1, beta: G2, gamma: G2, delta: G2, ic: Vector[G1]): Either[Groth16Error, Groth16VerifyingKey] =
    Either.cond(
      ic.nonEmpty,
      Groth16VerifyingKey(alpha, beta, gamma, delta, ic),
      Groth16Error.Malformed("a verifying key needs at least the constant IC point")
    )
}

/**
 * Groth16 verification over BN254, on a [[Bn254Engine]]: `e(-A, B) * e(alpha, beta) * e(L, gamma) * e(C, delta) == 1`,
 * with `L = ic(0) + sum_i x_i ic(i + 1)`.
 */
object Groth16 {

  /** Proof bytes: EIP-197 word layout `A.x, A.y, B.x.imag, B.x.real, B.y.imag, B.y.real, C.x, C.y`, as gnark and SP1 emit. */
  val ProofSize: Long = 256L

  /** The engine validates every point; here only the identities, and A's coordinates, which negating it needs. */
  def verify(engine: Bn254Engine)(vk: Groth16VerifyingKey, proof: ByteVector, inputs: Vector[BigInt]): Either[Groth16Error, Unit] = {
    val (a, b, c) = (proof.take(64), proof.slice(64, 192), proof.drop(192))
    def nonIdentity(p: ByteVector, role: String): Either[Groth16Error, Unit] =
      Either.cond(p != ByteVector.low(p.size), (), Groth16Error.InvalidProof(s"$role is the identity"))
    def field(w: ByteVector, role: String): Either[Groth16Error, BigInt] = {
      val x = BigInt(1, w.toArray)
      Either.cond(x < Bn254.P, x, Groth16Error.Malformed(s"$role is not a canonical field element"))
    }
    def zk[A](r: Either[ZkError, A]): Either[Groth16Error, A] = r.leftMap(Groth16Error.fromZk)
    for {
      _  <- Either.cond(proof.size == ProofSize, (), Groth16Error.Malformed(s"a proof is $ProofSize bytes, got ${proof.size}"))
      _  <- nonIdentity(a, "A") >> nonIdentity(b, "B") >> nonIdentity(c, "C")
      ax <- field(a.take(32), "A.x")
      ay <- field(a.drop(32), "A.y")
      negA = Bn254.be32(ax) ++ Bn254.be32((Bn254.P - ay).mod(Bn254.P))
      _ <- Either.cond(
        inputs.size == vk.publicInputs,
        (),
        Groth16Error.Malformed(s"expected ${vk.publicInputs} public inputs, got ${inputs.size}")
      )
      _ <- Either.cond(inputs.forall(x => x >= 0 && x < Bn254.R), (), Groth16Error.InvalidProof("a public input is not in Fr"))
      l <- inputs.zip(vk.ic.drop(1)).foldLeftM(vk.icBytes(0)) {
        case (acc, (x, p)) => zk(engine.mul(Bn254.encodeG1(Some(p)), Bn254.be32(x)).flatMap(engine.add(acc, _)))
      }
      ok <- zk(engine.pairing(Vector(negA -> b, vk.alphaBytes -> vk.betaBytes, l -> vk.gammaBytes, c -> vk.deltaBytes)))
      _  <- Either.cond(ok, (), Groth16Error.PairingCheckFailed)
    } yield ()
  }
}

/**
 * SP1's Groth16 wrapper (`SP1VerifierGroth16.sol`, circuit v6.1.0): `proofBytes = selector (4) || exitCode || vkRoot ||
 * nonce || proof (8 words)`, public inputs `[programVKey, sha256(publicValues) mod 2^253, exitCode, vkRoot, nonce]`.
 */
object Sp1Groth16 {

  val Selector: ByteVector = hex"4388a21c"
  val VkRoot: BigInt = BigInt("002f850ee998974d6cc00e50cd0814b098c05bfade466d28573240d057f25352", 16)
  val ProofLength: Long = 4L + 32L * 11

  private def fp2(real: String, imag: String) = Bn254.Fp2(BigInt(real), BigInt(imag))

  /** The negated G2 constants of the Solidity verifier, negated back to `beta`, `gamma`, `delta`. */
  private def unNeg(x: Bn254.Fp2, y: Bn254.Fp2): G2 = G2(x, -y)

  val VerifyingKey: Groth16VerifyingKey = Groth16VerifyingKey(
    alpha = G1(
      BigInt("15279411540481963483749982645131486879260751823620651493692884460296130891713"),
      BigInt("15872895802316430142046488442363778159164596024024981740547841316113839677454")
    ),
    beta = unNeg(
      fp2(
        "6145571844528009385227270901181311049451968424667282936975270874464890915386",
        "12771786691609444002416405093387705070206640282801320788762089789398249455552"
      ),
      fp2(
        "4488883874756188982949192438322346627006627895205628031405236004639323835517",
        "1735169520034591855846686229876971881413094324547255227368057137445726296809"
      )
    ),
    gamma = unNeg(
      fp2(
        "10857046999023057135944570762232829481370756359578518086990519993285655852781",
        "11559732032986387107991004021392285783925812861821192530917403151452391805634"
      ),
      fp2(
        "13392588948715843804641432497768002650278120570034223513918757245338268106653",
        "17805874995975841540914202342111839520379459829704422454583296818431106115052"
      )
    ),
    delta = unNeg(
      fp2(
        "10465707362494635227101096813108413078937487707553051407465224907243675430929",
        "8014260607368773541998918215611927658290278403999176336697043972644519659243"
      ),
      fp2(
        "19389283139277148919245778864125350153699493315071306268776225113374776030523",
        "16335894885742905444968709132584769120387318573561090701871591658625758958113"
      )
    ),
    ic = Vector(
      (
        "20281192269339458123687070687118212311775320590888414619062163734024177320592",
        "4733327396113282720944079206751955104965328647794767422434462962576999295035"
      ),
      (
        "6933777020392885277709527453058337947310422411038083362275568070104688005311",
        "981134475045095331624771061624185350383934842154508663637397442918499383708"
      ),
      (
        "4994703368938944727583784298191985234033403433117347198670233075674015451426",
        "8251219283963080431419977720140972699009004688253176317231536639169726973868"
      ),
      (
        "4290838847096051522936899065591427041691227664160185228987863596451823131267",
        "20588566735491008722164159313316540988426258906449040460220495569364391658476"
      ),
      (
        "10868099250506113890234768256645470833285719586092080686774540776807380789751",
        "481415511937576118656966359026147167555048629225366340770167496559184060449"
      ),
      (
        "248210862999154995000539012177951057105481472135341820587821789934938975214",
        "4435539404843896136682123140600986858809597152596796648926707165831171499457"
      )
    ).map { case (x, y) => G1(BigInt(x), BigInt(y)) }
  )

  /** `sha256(publicValues)` with its top three bits cleared (an element of Fr). */
  def hashPublicValues(publicValues: ByteVector): BigInt =
    BigInt(1, Hasher[Sha256].digest(publicValues).bytes.toArray) & ((BigInt(1) << 253) - 1)

  def verify(engine: Bn254Engine)(programVKey: ByteVector, publicValues: ByteVector, proofBytes: ByteVector): Either[Groth16Error, Unit] = {
    def word(i: Int): BigInt = BigInt(1, proofBytes.slice(4L + 32L * i, 4L + 32L * (i + 1)).toArray)
    for {
      _ <- Either.cond(programVKey.size == 32L, (), Groth16Error.Malformed(s"programVKey must be 32 bytes, got ${programVKey.size}"))
      _ <- Either.cond(
        proofBytes.size == ProofLength,
        (),
        Groth16Error.Malformed(s"proofBytes must be $ProofLength bytes, got ${proofBytes.size}")
      )
      _ <- Either.cond(proofBytes.take(4) == Selector, (), Groth16Error.Rejected("wrong verifier selector"))
      _ <- Either.cond(word(0) == 0, (), Groth16Error.Rejected("non-zero exit code"))
      _ <- Either.cond(word(1) == VkRoot, (), Groth16Error.Rejected("wrong VK root"))
      inputs = Vector(BigInt(1, programVKey.toArray), hashPublicValues(publicValues), word(0), word(1), word(2))
      _ <- Groth16.verify(engine)(VerifyingKey, proofBytes.drop(4L + 32L * 3), inputs)
    } yield ()
  }
}
