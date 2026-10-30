package xyz.kd5ujc.crypto.vrf

import java.nio.charset.StandardCharsets.{US_ASCII, UTF_8}
import java.nio.file.{Files, Paths}

import cats.syntax.all._

import io.circe.{Json, parser}
import org.scalacheck.Gen
import scodec.bits.{ByteVector, hex}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object EcVrfSuite extends SimpleIOSuite with Checkers {

  private def load(name: String): Json =
    parser.parse(new String(Files.readAllBytes(Paths.get("docs", "test-vectors", "vrf", name)), UTF_8)).toOption.get

  private def rows(j: Json, path: String*): List[Map[String, Json]] =
    path.foldLeft(j.hcursor: io.circe.ACursor)(_.downField(_)).as[List[Map[String, Json]]].toOption.get

  private def hx(r: Map[String, Json], k: String): ByteVector = ByteVector.fromValidHex(r(k).asString.get)

  private def affine(x: BigInt, y: BigInt): ByteVector = {
    val b = Curve25519Maps.le32(y)
    b.update(31, (b(31) | (if (x.testBit(0)) 0x80 else 0)).toByte)
  }

  // ------------------------------------------------------------------ RFC 9380 edwards25519_XMD:SHA-512_ELL2_NU_

  pureTest("RFC 9380 J.5.2: hash_to_field, map_to_curve and encode_to_curve") {
    val section = load("rfc9380.json").hcursor.downField("edwards25519_XMD_SHA512_ELL2_NU")
    val dst = ByteVector(section.downField("header").get[String]("dst").toOption.get.getBytes(US_ASCII))
    val vs = rows(load("rfc9380.json"), "edwards25519_XMD_SHA512_ELL2_NU", "vectors")
    expect
      .same(5, vs.size)
      .and(vs.foldMap { r =>
        def int(k: String) = BigInt(r(k).asString.get, 16)
        val msg = ByteVector(r("msg").asString.get.getBytes(US_ASCII))
        expect
          .same(Some(int("u[0]")), Curve25519Maps.hashToField(msg, dst))
          .and(expect.same(Some((int("Q.x"), int("Q.y"))), Curve25519Maps.mapToCurveEll2(int("u[0]"))))
          .and(expect.same(Some(affine(int("P.x"), int("P.y"))), Curve25519Maps.encodeToCurveEll2(msg, dst).map(Curve25519Maps.bytes)))
      })
  }

  // ------------------------------------------------------------------ RFC 9381 and draft-03 examples

  private def examples[S](rs: List[Map[String, Json]], extra: Map[String, Json] => weaver.Expectations)(implicit V: Vrf[S]) =
    expect
      .same(3, rs.size)
      .and(rs.foldMap { r =>
        val sk = V match {
          case _ if V eq Vrf.draft03 => V.signingKey(hx(r, "SK") ++ hx(r, "PK")).toOption.get
          case _                     => V.signingKey(hx(r, "SK")).toOption.get
        }
        val alpha = hx(r, "alpha")
        val proved = V.prove(sk, alpha)
        val vk = V.verifyingKey(hx(r, "PK")).toOption.get
        val pi = V.proof(hx(r, "pi")).toOption.get
        val tampered = V.proof(pi.bytes.update(40, (pi.bytes(40) ^ 1).toByte)).toOption.get
        expect
          .same(hx(r, "PK"), V.publicKey(sk).bytes)
          .and(expect.same(Right(hx(r, "pi")), proved.map(_._1.bytes)))
          .and(expect.same(Right(hx(r, "beta")), proved.map(_._2.bytes)))
          .and(expect.same(Right(hx(r, "beta")), V.verify(vk, alpha, pi).map(_.bytes)))
          .and(expect.same(Left(VrfError.ChallengeMismatch), V.verify(vk, alpha ++ hex"00", pi)))
          .and(expect(V.verify(vk, alpha, tampered).isLeft, "tampered proof"))
          .and(extra(r))
      })

  pureTest("RFC 9381 B.3: ECVRF-EDWARDS25519-SHA512-TAI examples 16-18") {
    examples[EcVrfTai](rows(load("rfc9381.json"), "tai"), _ => success)
  }

  pureTest("RFC 9381 B.4: ECVRF-EDWARDS25519-SHA512-ELL2 examples 19-21 (with intermediate u and H)") {
    val dst = ByteVector("ECVRF_edwards25519_XMD:SHA-512_ELL2_NU_".getBytes(US_ASCII)) ++ hex"04"
    examples[EcVrfEll2](
      rows(load("rfc9381.json"), "ell2"),
      r =>
        expect
          .same(Some(BigInt(1, hx(r, "u").reverse.toArray)), Curve25519Maps.hashToField(hx(r, "PK") ++ hx(r, "alpha"), dst))
          .and(
            expect.same(Some(hx(r, "H")), Curve25519Maps.encodeToCurveEll2(hx(r, "PK") ++ hx(r, "alpha"), dst).map(Curve25519Maps.bytes))
          )
    )
  }

  pureTest("draft-irtf-cfrg-vrf-03 A.4: Elligator2 examples (with intermediate H)") {
    examples[EcVrfDraft03](
      rows(load("draft03.json"), "examples"),
      r => expect.same(Some(hx(r, "H")), Curve25519Maps.hashToCurveDraft03(hx(r, "PK"), hx(r, "alpha")).map(Curve25519Maps.bytes))
    )
  }

  // ------------------------------------------------------------------ properties

  private val seed: Gen[ByteVector] = Gen.listOfN(32, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))
  private val alpha: Gen[ByteVector] = Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))
  implicit private val showPair: cats.Show[(ByteVector, ByteVector)] = cats.Show.show { case (a, b) => s"${a.toHex}/${b.toHex}" }

  private def roundTrip[S](implicit V: Vrf[S]) =
    forall(Gen.zip(seed, alpha)) {
      case (s, a) =>
        val kp = Vrf.fromSeed[S](s).toOption.get
        val (pi, beta) = V.prove(kp.signing, a).toOption.get
        expect
          .same(Right(beta), V.verify(kp.verifying, a, pi))
          .and(expect.same(Vrf.ProofLength, pi.bytes.size))
          .and(expect.same(Vrf.OutputLength, beta.bytes.size))
    }

  test("TAI: prove/verify round-trips")(roundTrip[EcVrfTai])
  test("ELL2: prove/verify round-trips")(roundTrip[EcVrfEll2])
  test("draft-03: prove/verify round-trips")(roundTrip[EcVrfDraft03])

  pureTest("suites sharing suite byte 0x04 never accept each other's proofs") {
    val s = ByteVector.fill(32)(5)
    val ell2 = Vrf.fromSeed[EcVrfEll2](s).toOption.get
    val (pi, _) = Vrf.ell2.prove(ell2.signing, hex"01").toOption.get
    expect(Vrf.draft03.verifyBytes(ell2.verifying.bytes, hex"01", pi.bytes).isLeft)
  }

  pureTest("malformed envelopes, small-order keys, non-canonical encodings and mismatched keys are typed errors") {
    val kp = Vrf.fromSeed[EcVrfTai](ByteVector.fill(32)(9)).toOption.get
    val (pi, _) = Vrf.tai.prove(kp.signing, hex"").toOption.get
    val l = Curve25519Maps.le32(Curve25519Maps.L)
    def malformed(e: Either[VrfError, ?]) = e match {
      case Left(VrfError.Malformed(_, _)) => true
      case _                              => false
    }
    val identity = hex"0100000000000000000000000000000000000000000000000000000000000000"
    val negZero = hex"0100000000000000000000000000000000000000000000000000000000000080"
    val nonCanonical = hex"edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f"
    expect(malformed(Vrf.tai.verifyBytes(kp.verifying.bytes.drop(1), hex"", pi.bytes)), "31-byte key")
      .and(expect(malformed(Vrf.tai.verifyBytes(kp.verifying.bytes, hex"", pi.bytes.drop(1))), "79-byte proof"))
      .and(expect.same(Left(VrfError.SmallOrderPublicKey), Vrf.tai.verifyBytes(identity, hex"", pi.bytes)))
      .and(expect.same(Left(VrfError.InvalidPublicKey), Vrf.tai.verifyBytes(negZero, hex"", pi.bytes)))
      .and(expect.same(Left(VrfError.InvalidPublicKey), Vrf.tai.verifyBytes(nonCanonical, hex"", pi.bytes)))
      .and(expect.same(Left(VrfError.NonCanonicalScalar), Vrf.tai.verifyBytes(kp.verifying.bytes, hex"", pi.bytes.take(48) ++ l)))
      .and(expect.same(Left(VrfError.KeyMismatch), Vrf.draft03.signingKey(ByteVector.fill(32)(1) ++ ByteVector.fill(32)(2))))
      .and(expect(Vrf.draft03.verifyingKey(negZero).isLeft, "draft-03 decodes -0 to the identity, which is small order"))
  }
}
