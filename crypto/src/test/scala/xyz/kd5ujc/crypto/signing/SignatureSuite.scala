package xyz.kd5ujc.crypto.signing

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.effect.std.SecureRandom
import cats.mtl.Handle

import io.circe.parser
import org.scalacheck.Gen
import scodec.bits.{ByteVector, HexStringSyntax}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object SignatureSuite extends SimpleIOSuite with Checkers {

  private val message: Gen[ByteVector] = Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))
  private val seed32: Gen[ByteVector] = Gen.listOfN(32, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))
  implicit private val showPair: cats.Show[(ByteVector, ByteVector)] = cats.Show.show { case (a, b) => s"${a.toHex}/${b.toHex}" }

  private def flip(b: ByteVector, i: Long): ByteVector = b.update(i, (b(i) ^ 0x01).toByte)

  /** Laws every deterministic scheme satisfies; `seedOk` filters seeds a scheme rejects (secp256k1 scalars >= n). */
  private def laws[S](name: String)(implicit C: SchemeCodec[S], D: SeedDerivation[S], Si: Signer[S], V: Verifier[S]): Unit = {
    test(s"$name: signatures verify, are deterministic, and reject any tampering") {
      forall(Gen.zip(seed32, message)) {
        case (seed, msg) =>
          D.fromSeed(seed) match {
            case Left(_) => success // out-of-range seed for this scheme
            case Right(KeyPair(sk, vk)) =>
              val sig = Si.sign(sk, msg)
              val other = D.fromSeed(flip(seed, 0)).toOption
              expect(V.verify(vk, msg, sig), "a fresh signature verifies")
                .and(expect.same(sig, Si.sign(sk, msg)))
                .and(expect(!V.verify(vk, msg ++ hex"00", sig), "an extended message is rejected"))
                .and(expect(C.signature(flip(sig.bytes, 5)).forall(s => !V.verify(vk, msg, s)), "a tampered signature is rejected"))
                .and(expect(other.forall(kp => kp.verifying == vk || !V.verify(kp.verifying, msg, sig)), "another key is rejected"))
                .and(expect.same(Right(vk), C.verifyingKey(vk.bytes)))
                .and(expect.same(Right(sig), C.signature(sig.bytes)))
          }
      }
    }
    pureTest(s"$name: encodings of the wrong length are typed errors") {
      expect(C.signingKey(hex"00").isLeft).and(expect(C.verifyingKey(hex"00").isLeft)).and(expect(C.signature(hex"00").isLeft))
    }
  }

  // ------------------------------------------------------------------ Ed25519

  laws[Ed25519]("Ed25519")

  pureTest("Ed25519: RFC 8032 known answers (BramblSc vector file)") {
    val raw = new String(Files.readAllBytes(Paths.get("docs", "test-vectors", "Ed25519.json")), UTF_8)
    val vectors = parser.parse(raw).flatMap(_.as[List[io.circe.Json]]).toOption.get
    vectors.map { v =>
      val c = v.hcursor
      def hx(path: String*) = ByteVector.fromValidHex(path.foldLeft(c: io.circe.ACursor)(_.downField(_)).as[String].toOption.get)
      val kp = Ed25519.seedDerivation.fromSeed(hx("inputs", "secretKey")).toOption.get
      val sig = Ed25519.signer.sign(kp.signing, hx("inputs", "message"))
      expect.same(hx("outputs", "verificationKey"), kp.verifying.bytes).and(expect.same(hx("outputs", "signature"), sig.bytes))
    }.reduce(_ and _)
  }

  pureTest("Ed25519: off-curve verifying keys are rejected by the codec") {
    // y = 2 does not decode to a point on edwards25519
    expect(Ed25519.codec.verifyingKey(hex"0200000000000000000000000000000000000000000000000000000000000000").isLeft)
  }

  test("Ed25519: key generation from a secure random source") {
    SecureRandom.javaSecuritySecureRandom[IO].flatMap { implicit r =>
      Handle
        .allowF[IO, CryptoError] { implicit h =>
          KeyGen[IO, Ed25519].generate
            .map(kp => expect(Ed25519.verifier.verify(kp.verifying, hex"01", Ed25519.signer.sign(kp.signing, hex"01"))))
        }
        .rescue(e => IO.pure(failure(s"unexpected $e")))
    }
  }

  pureTest("signing keys never print their bytes and compare in constant time") {
    val kp = Ed25519.seedDerivation.fromSeed(ByteVector.fill(32)(7)).toOption.get
    val same = Ed25519.seedDerivation.fromSeed(ByteVector.fill(32)(7)).toOption.get
    expect(!kp.signing.toString.contains("07")).and(expect(kp.signing == same.signing)).and(expect(!kp.toString.contains("0707")))
  }

  // ------------------------------------------------------------------ P-256

  laws[P256Ecdsa]("P-256 ECDSA")

  pureTest("P-256 ECDSA: RFC 6979 A.2.5 known answers (SHA-256), with s normalised low") {
    val n = BigInt("ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551", 16)
    val sk = P256Ecdsa.codec.signingKey(hex"c9afa9d845ba75166b5c215767b1d6934e50c3db36e89b127b8a622b120f6721").toOption.get
    def low(sig: ByteVector): ByteVector = {
      val s = BigInt(1, sig.drop(32).toArray)
      sig.take(32) ++ ByteVector.view((if (s > n / 2) n - s else s).toByteArray).takeRight(32).padLeft(32)
    }
    val sample = P256Ecdsa.signer.sign(sk, ByteVector("sample".getBytes(UTF_8))).bytes
    val testMsg = P256Ecdsa.signer.sign(sk, ByteVector("test".getBytes(UTF_8))).bytes
    expect
      .same(hex"0360fed4ba255a9d31c961eb74c6356d68c049b8923b61fa6ce669622e60f29fb6", P256Ecdsa.publicKeyOf.verifyingKey(sk).bytes)
      .and(
        expect.same(
          low(
            hex"efd48b2aacb6a8fd1140dd9cd45e81d69d2c877b56aaf991c34d0ea84eaf3716f7cb1c942d657c41d436c7a1b6e29f65f3e900dbb9aff4064dc4ab2f843acda8"
          ),
          sample
        )
      )
      .and(
        expect.same(
          low(
            hex"f1abb023518351cd71d881567b1ea663ed3efcf6c5132b354f28d3b0b7d38367019f4113742a2b14bd25926b49c649155f267e60d3814b4c0cc84250e46f0083"
          ),
          testMsg
        )
      )
  }

  // ------------------------------------------------------------------ secp256k1

  private val backend = Secp256k1.load[IO].unsafeRunSync()(cats.effect.unsafe.IORuntime.global)
  private val k1 = new Secp256k1.Instances(backend)

  {
    import k1._
    laws[Secp256k1Ecdsa]("secp256k1 ECDSA")
  }

  pureTest("secp256k1 ECDSA: RFC 6979 known answer (key 1, \"Satoshi Nakamoto\")") {
    val sk = backend.ecdsaCodec.signingKey(ByteVector.fromLong(1).padLeft(32)).toOption.get
    val sig = backend.ecdsaSigner.sign(sk, ByteVector("Satoshi Nakamoto".getBytes(UTF_8)))
    expect.same(
      hex"934b1ea10a4b3c1757e2b0c017d0b6143ce3c9a7e6a4a49860d7a6ab210ee3d82442ce9d2b916064108014783e923ec36b49743e2ffa1c4496f01a512aafd9e5",
      sig.bytes
    )
  }

  pureTest("secp256k1 ECDSA: high-S signatures are rejected as non-canonical") {
    val n = BigInt("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)
    val sk = backend.ecdsaCodec.signingKey(ByteVector.fromLong(1).padLeft(32)).toOption.get
    val sig = backend.ecdsaSigner.sign(sk, hex"0102").bytes
    val highS = sig.take(32) ++ ByteVector.view((n - BigInt(1, sig.drop(32).toArray)).toByteArray).takeRight(32).padLeft(32)
    val vk = backend.ecdsaPublicKeyOf.verifyingKey(sk)
    val forged = Signature[Secp256k1Ecdsa](highS)
    expect(backend.ecdsaCodec.signature(highS).isLeft, "the codec rejects the malleated twin")
      .and(expect(!backend.ecdsaVerifier.verify(vk, hex"0102", forged), "libsecp256k1 also rejects high-S"))
      .and(
        expect(backend.ecdsaCodec.signingKey(ByteVector.view(n.toByteArray).takeRight(32)).isLeft, "a secret key equal to n is rejected")
      )
      .and(expect(backend.ecdsaCodec.signingKey(ByteVector.fill(32)(0)).isLeft, "the zero key is rejected"))
  }

  pureTest("BIP-340: official test vector 0") {
    val sk = backend.schnorrCodec.signingKey(ByteVector.fromLong(3).padLeft(32)).toOption.get
    val vk = backend.schnorrPublicKeyOf.verifyingKey(sk)
    val sig = backend.schnorrSignRaw(sk, ByteVector.fill(32)(0), ByteVector.fill(32)(0)).toOption.get
    expect
      .same(hex"f9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9", vk.bytes)
      .and(
        expect.same(
          hex"e907831f80848d1069a5371b402410364bdf1c5f8307b0084c55f1ce2dca821525f66a4a85ea8b71e482a74f382d2ce5ebeee8fdb2172f477df4900d310536c0",
          sig.bytes
        )
      )
      .and(expect(backend.schnorrVerifyRaw(vk, ByteVector.fill(32)(0), sig)))
      .and(expect(backend.schnorrSignRaw(sk, ByteVector.fill(31)(0), ByteVector.fill(32)(0)).isLeft, "raw input must be exactly 32 bytes"))
  }

  test("BIP-340: randomised signatures verify and reject tampering") {
    SecureRandom.javaSecuritySecureRandom[IO].flatMap { implicit r =>
      forall(Gen.zip(seed32, message)) {
        case (seed, msg) =>
          backend.schnorrSeedDerivation.fromSeed(seed) match {
            case Left(_) => IO.pure(success)
            case Right(KeyPair(sk, vk)) =>
              backend.schnorrSigner[IO].sign(sk, msg).map { sig =>
                expect(backend.schnorrVerifier.verify(vk, msg, sig)).and(expect(!backend.schnorrVerifier.verify(vk, msg ++ hex"00", sig)))
              }
          }
      }
    }
  }

  pureTest("crypto domain tags satisfy the tag grammar") {
    Secp256k1.Tags.map(t => expect(xyz.kd5ujc.core.hash.DomainTag(t).isRight, t)).reduce(_ and _)
  }
}
