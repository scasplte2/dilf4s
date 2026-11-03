package xyz.kd5ujc.crypto.curve25519

import java.nio.charset.StandardCharsets.US_ASCII

import cats.Show
import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.core.hash.{Hasher, Sha512}
import xyz.kd5ujc.crypto.signing.{Ed25519, Signature, VerifyingKey}
import xyz.kd5ujc.crypto.vrf._

import cafe.cryptography.curve25519.{CompressedEdwardsY, Constants, EdwardsPoint, Scalar}
import org.bouncycastle.crypto.digests.SHA512Digest
import org.scalacheck.Gen
import scodec.bits.{ByteVector, hex}
import weaver.SimpleIOSuite
import weaver.scalacheck.{CheckConfig, Checkers}

/**
 * The variable-time edwards25519 core against curve25519-elisabeth, which it replaced for verification: decoding,
 * group arithmetic, multi-scalar multiplication, ZIP-215 and libsodium Ed25519 verification, and ECVRF verification in
 * all three suites (an oracle that is the previous elisabeth implementation), on random, tampered and mixed-order
 * inputs and on every edge encoding.
 */
object Curve25519CrossCheckSuite extends SimpleIOSuite with Checkers {

  override def checkConfig: CheckConfig = super.checkConfig.withMinimumSuccessful(200)

  private given Show[ByteVector] = Show.show(_.toHex)
  private given Show[EdwardsPoint] = Show.show(pt => eBytes(pt).toHex)
  private given Show[Scalar] = Show.show(s => ByteVector.view(s.toByteArray).toHex)

  private val p: BigInt = BigInt(2).pow(255) - 19
  private val l: BigInt = BigInt(2).pow(252) + BigInt("27742317777372353535851937790883648493")

  private def le32(i:    BigInt): ByteVector = ByteVector.view(i.toByteArray).takeRight(32).padLeft(32).reverse
  private def eDecode(b: ByteVector): Option[EdwardsPoint] = Either.catchNonFatal(new CompressedEdwardsY(b.toArray).decompress()).toOption
  private def eBytes(pt: EdwardsPoint): ByteVector = ByteVector.view(pt.compress().toByteArray)
  private def fromE(pt:  EdwardsPoint): Point = Point.decode(eBytes(pt)).getOrElse(Point.Identity)

  private val bytes32: Gen[ByteVector] = Gen.listOfN(32, Gen.choose(0, 255)).map(bs => ByteVector(bs.map(_.toByte)))
  private val scalarGen: Gen[Scalar] = bytes32.map(b => Scalar.fromBytesModOrder(b.toArray))

  /** The order-8 torsion subgroup, from one generator. */
  private val torsion: Vector[EdwardsPoint] = {
    val t = new CompressedEdwardsY(hex"26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05".toArray).decompress()
    Vector.iterate(EdwardsPoint.IDENTITY, 8)(_.add(t))
  }

  /** A point of the prime-order subgroup, or one with a torsion component. */
  private val pointGen: Gen[EdwardsPoint] = for {
    r <- scalarGen
    t <- Gen.choose(0, 7)
  } yield Constants.ED25519_BASEPOINT_TABLE.multiply(r).add(torsion(t))

  /** Every `y` from 0 to 2 and from p - 1 to 2^255 - 1, each with both sign bits, and every small-order point's encodings. */
  private val edges: List[ByteVector] = {
    val ys = (List.tabulate(3)(BigInt(_)) ++ List.tabulate(20)(i => p - 1 + i)).map(le32)
    val small = torsion.toList.map(eBytes).flatMap { e =>
      val y = BigInt(1, e.update(31, (e(31) & 0x7f).toByte).reverse.toArray)
      e :: Option.when(y + p < BigInt(2).pow(255))(le32(y + p).update(31, ((le32(y + p)(31) & 0x7f) | (e(31) & 0x80)).toByte)).toList
    }
    (ys ++ small).flatMap(b => List(b.update(31, (b(31) & 0x7f).toByte), b.update(31, (b(31) | 0x80).toByte))).distinct
  }

  private def agreeOnDecode(b: ByteVector) = {
    val (ours, theirs) = (Point.decode(b), eDecode(b))
    expect(
      ours.map(_.compress) == theirs.map(eBytes),
      s"decode ${b.toHex}: ${ours.map(_.compress.toHex)} vs ${theirs.map(eBytes(_).toHex)}"
    )
      .and(expect(ours.map(_.isIdentity) == theirs.map(_.isIdentity), s"identity ${b.toHex}"))
      .and(expect(ours.map(_.isSmallOrder) == theirs.map(_.isSmallOrder), s"small order ${b.toHex}"))
      .and(expect(ours.map(_.x.isZero) == theirs.map(pt => eBytes(pt.negate()) == eBytes(pt)), s"x = 0 ${b.toHex}"))
  }

  test("decoding agrees on random 32-byte strings") {
    forall(bytes32)(agreeOnDecode)
  }

  pureTest(s"decoding agrees on every edge encoding (${edges.size})") {
    edges.foldMap(agreeOnDecode)
  }

  pureTest("the torsion points are exactly the small-order points, and the basepoint is RFC 8032's") {
    torsion.toList
      .foldMap(t => expect(fromE(t).isSmallOrder))
      .and(expect.same(eBytes(Constants.ED25519_BASEPOINT), Point.Basepoint.compress))
      .and(expect(!Point.Basepoint.isSmallOrder))
      .and(expect(Point.Identity.isIdentity))
  }

  test("addition, subtraction, negation, doubling by 8 and batch compression agree") {
    forall(Gen.zip(pointGen, pointGen)) {
      case (a, b) =>
        val (x, y) = (fromE(a), fromE(b))
        expect
          .same(eBytes(a.add(b)), (x + y).compress)
          .and(expect.same(eBytes(a.subtract(b)), (x - y).compress))
          .and(expect.same(eBytes(a.negate()), x.negate.compress))
          .and(expect.same(eBytes(a.multiplyByCofactor()), x.multiplyByCofactor().compress))
          .and(
            expect.same(
              (eBytes(a), eBytes(b), eBytes(a.add(b)), eBytes(a.add(a))),
              Point.compressBatch(x, y, x + y, x + x)
            )
          )
    }
  }

  private def le(s: Scalar): ByteVector = ByteVector.view(s.toByteArray)

  test("aP + bB and aP + bQ agree with elisabeth, for any scalars below L and mixed-order points") {
    forall(Gen.zip(scalarGen, pointGen, scalarGen, pointGen)) {
      case (a, pt, b, q) =>
        expect
          .same(
            eBytes(EdwardsPoint.vartimeDoubleScalarMultiplyBasepoint(a, pt, b)),
            Point.doubleScalarMultiplyBasepoint(a, fromE(pt), b).compress
          )
          .and(expect.same(eBytes(pt.multiply(a).add(q.multiply(b))), Point.doubleScalarMultiply(a, fromE(pt), b, fromE(q)).compress))
    }
  }

  pureTest("multi-scalar edge scalars: 0, 1, 2, L - 1, 2^128 - 1, 2^252") {
    val scalars = List(BigInt(0), BigInt(1), BigInt(2), l - 1, BigInt(2).pow(128) - 1, BigInt(2).pow(252))
    val pt = Constants.ED25519_BASEPOINT_TABLE.multiply(Scalar.fromBytesModOrder(Array.fill[Byte](32)(3))).add(torsion(3))
    scalars.foldMap { a =>
      scalars.foldMap { b =>
        val (sa, sb) = (Scalar.fromBytesModOrder(le32(a).toArray), Scalar.fromBytesModOrder(le32(b).toArray))
        expect
          .same(
            eBytes(pt.multiply(sa).add(Constants.ED25519_BASEPOINT.multiply(sb))),
            Point.doubleScalarMultiplyBasepoint(sa, fromE(pt), sb).compress
          )
          .and(expect.same(eBytes(pt.multiply(sa).add(pt.multiply(sb))), Point.doubleScalarMultiply(sa, fromE(pt), sb, fromE(pt)).compress))
      }
    }
  }

  // ------------------------------------------------------------------ Ed25519

  private def sha512(b: ByteVector): Array[Byte] = {
    val d = new SHA512Digest(); val out = new Array[Byte](64); d.update(b.toArray, 0, b.size.toInt); d.doFinal(out, 0); out
  }

  /** The previous ZIP-215 verifier, on elisabeth. */
  private def oracle(key: ByteVector, message: ByteVector, signature: ByteVector): Boolean =
    signature.size == 64 && key.size == 32 && (for {
      a <- eDecode(key)
      r <- eDecode(signature.take(32))
      s <- Either.catchNonFatal(Scalar.fromCanonicalBytes(signature.drop(32).toArray)).toOption
      k = Scalar.fromBytesModOrderWide(sha512(signature.take(32) ++ key ++ message))
    } yield EdwardsPoint.vartimeDoubleScalarMultiplyBasepoint(Scalar.ZERO.subtract(k), a, s).subtract(r).multiplyByCofactor().isIdentity)
      .getOrElse(false)

  /** The previous libsodium verifier, on elisabeth. */
  private def libsodiumOracle(key: ByteVector, message: ByteVector, signature: ByteVector): Boolean =
    signature.size == 64 && key.size == 32 && BigInt(1, key.update(31, (key(31) & 0x7f).toByte).reverse.toArray) < p && (for {
      a <- eDecode(key).filterNot(_.multiplyByCofactor().isIdentity)
      s <- Either.catchNonFatal(Scalar.fromCanonicalBytes(signature.drop(32).toArray)).toOption
      k = Scalar.fromBytesModOrderWide(sha512(signature.take(32) ++ key ++ message))
      r = EdwardsPoint.vartimeDoubleScalarMultiplyBasepoint(k, a.negate(), s)
    } yield eBytes(r) == signature.take(32) && !r.multiplyByCofactor().isIdentity).getOrElse(false)

  private def ours(key: ByteVector, message: ByteVector, signature: ByteVector): Boolean =
    Ed25519.verifier.verify(VerifyingKey[Ed25519](key), message, Signature[Ed25519](signature))
  private def oursLibsodium(key: ByteVector, message: ByteVector, signature: ByteVector): Boolean =
    Ed25519.libsodiumVerifier.verify(VerifyingKey[Ed25519](key), message, Signature[Ed25519](signature))

  private def agree(key: ByteVector, message: ByteVector, signature: ByteVector) = {
    val (zip, sodium) = (oracle(key, message, signature), libsodiumOracle(key, message, signature))
    expect(ours(key, message, signature) == zip, s"ZIP-215 disagrees (oracle $zip): ${key.toHex} ${signature.toHex}")
      .and(expect(oursLibsodium(key, message, signature) == sodium, s"libsodium disagrees (oracle $sodium): ${key.toHex}"))
  }

  /** A secret scalar `a`, a key `aB + T`, and `S = r + k a` over `R = rB + T'`: valid only cofactored when T, T' != 0. */
  private val mixedGen: Gen[(ByteVector, ByteVector, ByteVector)] = for {
    a        <- scalarGen
    r        <- scalarGen
    (ta, tr) <- Gen.zip(Gen.choose(0, 7), Gen.choose(0, 7))
    message  <- Gen.listOf(Gen.choose(0, 255)).map(bs => ByteVector(bs.map(_.toByte)))
  } yield {
    val key = eBytes(Constants.ED25519_BASEPOINT_TABLE.multiply(a).add(torsion(ta)))
    val rb = eBytes(Constants.ED25519_BASEPOINT_TABLE.multiply(r).add(torsion(tr)))
    val k = Scalar.fromBytesModOrderWide(sha512(rb ++ key ++ message))
    (key, message, rb ++ le(k.multiplyAndAdd(a, r)))
  }

  test("valid, mixed-order and tampered signatures: the same verdicts as elisabeth") {
    forall(Gen.zip(mixedGen, Gen.choose(0, 64 * 8 + 32 * 8 - 1))) {
      case ((key, message, sig), bit) =>
        val (tamperedSig, tamperedKey) =
          if (bit < 512) (sig.update(bit / 8L, (sig(bit / 8L) ^ (1 << (bit % 8))).toByte), key)
          else (sig, key.update((bit - 512) / 8L, (key((bit - 512) / 8L) ^ (1 << ((bit - 512) % 8))).toByte))
        agree(key, message, sig).and(agree(tamperedKey, message, tamperedSig)).and(agree(key, message ++ hex"00", sig))
    }
  }

  test("random keys and signatures: the same verdicts as elisabeth") {
    forall(Gen.zip(bytes32, bytes32, bytes32)) { case (key, r, s) => agree(key, hex"00", r ++ s) }
  }

  pureTest("honest signatures and edge encodings of A and R: the same verdicts") {
    val kp = Ed25519.seedDerivation.fromSeed(ByteVector.fill(32)(9)).toOption.get
    val msg = ByteVector("cross-check".getBytes(US_ASCII))
    val sig = Ed25519.signer.sign(kp.signing, msg).bytes
    expect(ours(kp.verifying.bytes, msg, sig)).and(
      edges.foldMap(e =>
        agree(e, msg, sig).and(agree(kp.verifying.bytes, msg, e ++ sig.drop(32))).and(agree(e, msg, e ++ ByteVector.low(32)))
      )
    )
  }

  // ------------------------------------------------------------------ ECVRF

  private def hash(parts: ByteVector*): ByteVector = Hasher[Sha512].digest(parts.foldLeft(ByteVector.empty)(_ ++ _)).bytes
  private def strict(b:   ByteVector): Option[EdwardsPoint] = eDecode(b).filter(pt => eBytes(pt) == b)
  private def draft03(b: ByteVector): Option[EdwardsPoint] =
    Option.when(BigInt(1, b.update(31, (b(31) & 0x7f).toByte).reverse.toArray) < p)(b).flatMap(eDecode)

  private enum Suite { case Tai, Ell2, Draft03 }

  /** The previous verifier, on elisabeth (hash-to-curve's `BigInt` maps are shared; their points are re-decoded). */
  private def vrfOracle(suite: Suite, key: ByteVector, alpha: ByteVector, proof: ByteVector): Either[String, ByteVector] = {
    val id = if (suite == Suite.Tai) 3 else 4
    val decode = if (suite == Suite.Draft03) draft03 else strict
    def h2c(pk: ByteVector): Option[EdwardsPoint] = suite match {
      case Suite.Tai =>
        @tailrec def loop(ctr: Int): Option[EdwardsPoint] =
          if (ctr > 255) None
          else
            strict(hash(ByteVector(3, 1), pk, alpha, ByteVector(ctr, 0)).take(32)) match {
              case Some(pt) => Some(pt.multiplyByCofactor())
              case None     => loop(ctr + 1)
            }
        loop(0)
      case Suite.Ell2 =>
        val dst = ByteVector("ECVRF_edwards25519_XMD:SHA-512_ELL2_NU_".getBytes(US_ASCII)) ++ ByteVector(4)
        Curve25519Maps.encodeToCurveEll2(pk ++ alpha, dst).flatMap(pt => eDecode(pt.compress))
      case Suite.Draft03 => Curve25519Maps.hashToCurveDraft03(pk, alpha).flatMap(pt => eDecode(pt.compress))
    }
    def eq(pt: EdwardsPoint, q: EdwardsPoint, c: Scalar, s: Scalar): EdwardsPoint =
      if (suite == Suite.Draft03) pt.multiply(Scalar.ZERO.subtract(c)).add(q.multiply(s)) else q.multiply(s).subtract(pt.multiply(c))
    def challenge(h: ByteVector, g: ByteVector, u: ByteVector, v: ByteVector) =
      if (suite == Suite.Draft03) hash(ByteVector(4, 2), h, g, u, v).take(16)
      else hash(ByteVector(id, 2), key, h, g, u, v, ByteVector(0)).take(16)
    for {
      _     <- Either.cond(proof.size == 80 && key.size == 32, (), "malformed")
      y     <- decode(key).toRight("key")
      _     <- Either.cond(!y.multiplyByCofactor().isIdentity, (), "small")
      gamma <- decode(proof.take(32)).toRight("gamma")
      c = Scalar.fromBytesModOrder(proof.slice(32, 48).padRight(32).toArray)
      _ <- Either.cond(BigInt(1, proof.drop(48).reverse.toArray) < l, (), "scalar")
      s = Scalar.fromBytesModOrder(proof.drop(48).toArray)
      h <- h2c(eBytes(y)).toRight("h2c")
      u = eq(y, Constants.ED25519_BASEPOINT, c, s)
      v = eq(gamma, h, c, s)
      _ <- Either.cond(challenge(eBytes(h), proof.take(32), eBytes(u), eBytes(v)) == proof.slice(32, 48), (), "challenge")
    } yield hash(ByteVector(id, 3), eBytes(gamma.multiplyByCofactor()), if (suite == Suite.Draft03) ByteVector.empty else ByteVector(0))
  }

  private def vrfOurs(suite: Suite, key: ByteVector, alpha: ByteVector, proof: ByteVector): Either[String, ByteVector] = {
    val r = suite match {
      case Suite.Tai     => Vrf.tai.verifyBytes(key, alpha, proof).map(_.bytes)
      case Suite.Ell2    => Vrf.ell2.verifyBytes(key, alpha, proof).map(_.bytes)
      case Suite.Draft03 => Vrf.draft03.verifyBytes(key, alpha, proof).map(_.bytes)
    }
    r.leftMap {
      case VrfError.Malformed(_, _)      => "malformed"
      case VrfError.InvalidPublicKey     => "key"
      case VrfError.SmallOrderPublicKey  => "small"
      case VrfError.InvalidGamma         => "gamma"
      case VrfError.NonCanonicalScalar   => "scalar"
      case VrfError.HashToCurveFailed(_) => "h2c"
      case VrfError.ChallengeMismatch    => "challenge"
      case VrfError.KeyMismatch          => "key mismatch"
    }
  }

  private def prove(suite: Suite, seed: ByteVector, alpha: ByteVector): (ByteVector, ByteVector) = {
    def run[S](implicit V: Vrf[S]) = {
      val kp = Vrf.fromSeed[S](seed).toOption.get
      (kp.verifying.bytes, V.prove(kp.signing, alpha).toOption.get._1.bytes)
    }
    suite match {
      case Suite.Tai     => run[EcVrfTai]
      case Suite.Ell2    => run[EcVrfEll2]
      case Suite.Draft03 => run[EcVrfDraft03]
    }
  }

  private val alphaGen: Gen[ByteVector] = Gen.listOf(Gen.choose(0, 255)).map(bs => ByteVector(bs.map(_.toByte)))

  Suite.values.toList.foreach { suite =>
    test(s"ECVRF $suite: honest, tampered, mixed-order-Gamma and random-key proofs give the old verdicts and outputs") {
      forall(Gen.zip(bytes32, alphaGen, Gen.choose(0, 80 * 8 - 1), Gen.choose(1, 7), bytes32)) {
        case (seed, alpha, bit, t, randomKey) =>
          val (key, proof) = prove(suite, seed, alpha)
          val tampered = proof.update(bit / 8L, (proof(bit / 8L) ^ (1 << (bit % 8))).toByte)
          val mixedGamma = eDecode(proof.take(32)).fold(proof)(g => eBytes(g.add(torsion(t))) ++ proof.drop(32))
          def same(k: ByteVector, a: ByteVector, pi: ByteVector) = {
            val (o, n) = (vrfOracle(suite, k, a, pi), vrfOurs(suite, k, a, pi))
            expect(o == n, s"$suite: oracle $o, ours $n")
          }
          expect(vrfOurs(suite, key, alpha, proof).isRight, "an honest proof verifies")
            .and(same(key, alpha, proof))
            .and(same(key, alpha, tampered))
            .and(same(key, alpha, mixedGamma))
            .and(same(key, alpha ++ hex"01", proof))
            .and(same(randomKey, alpha, proof))
      }
    }
  }

  pureTest("ECVRF: edge encodings as keys and as Gamma give the old verdicts") {
    val alpha = hex"72"
    Suite.values.toList.foldMap { suite =>
      val (key, proof) = prove(suite, ByteVector.fill(32)(5), alpha)
      edges.foldMap { e =>
        expect
          .same(vrfOracle(suite, e, alpha, proof), vrfOurs(suite, e, alpha, proof))
          .and(expect.same(vrfOracle(suite, key, alpha, e ++ proof.drop(32)), vrfOurs(suite, key, alpha, e ++ proof.drop(32))))
      }
    }
  }
}
