package xyz.kd5ujc.crypto.signing

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

import cats.Show
import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.crypto.{CryptoVectors, SigningRejections}

import cafe.cryptography.curve25519.{CompressedEdwardsY, Constants, Scalar}
import io.circe.{ACursor, Decoder, parser}
import org.bouncycastle.crypto.digests.SHA512Digest
import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.scalacheck.{CheckConfig, Checkers}
import weaver.{Expectations, IOSuite}

/**
 * The native Ed25519 backend (dilf4s-signing through sdk/rust/jvm-ffi) against the conformance vectors and against the
 * pure-Scala backend: the same verdict, and the same typed error, on every input.
 */
object Ed25519NativeSuite extends IOSuite with Checkers {

  override def checkConfig: CheckConfig = CheckConfig.default.withMinimumSuccessful(500)

  final case class Row(scheme: String, name: String, key: ByteVector, message: ByteVector, signature: ByteVector, result: String)
  final case class KeyRow(name: String, bytes: ByteVector, result: String)
  final case class Signed(key: ByteVector, message: ByteVector, signature: ByteVector)
  final case class Vectors(rows: List[Row], keys: List[KeyRow], signed: List[Signed])

  type Res = (Ed25519Backend, Vectors)
  def sharedResource: Resource[IO, Res] = Resource.eval((Ed25519Backend.native[IO], vectors).tupled)

  private val reference = Ed25519Backend.reference

  private def hex(c: ACursor, field: String): Decoder.Result[ByteVector] =
    c.get[String](field).flatMap(ByteVector.fromHex(_).toRight(io.circe.DecodingFailure(s"$field is not hex", Nil)))

  private val vectors: IO[Vectors] =
    IO.blocking(new String(Files.readAllBytes(CryptoVectors.path), UTF_8)).flatMap(parser.parse(_).liftTo[IO]).flatMap { json =>
      val c = json.hcursor
      val rows = c
        .downField("verification")
        .values
        .toList
        .flatten
        .traverse { j =>
          val r = j.hcursor
          (
            r.get[String]("scheme"),
            r.get[String]("name"),
            hex(r, "verifyingKey"),
            hex(r, "message"),
            hex(r, "signature"),
            r.get[String]("result")
          )
            .mapN(Row.apply)
        }
        .map(_.filter(r => r.scheme == "Ed25519" || r.scheme == "Ed25519-libsodium"))
      val keys = c
        .downField("keyDecoding")
        .values
        .toList
        .flatten
        .traverse { j =>
          val r = j.hcursor
          (r.get[String]("scheme"), r.get[String]("part"), r.get[String]("name"), hex(r, "bytes"), r.get[String]("result")).tupled
        }
        .map(_.collect { case ("Ed25519", "verifyingKey", name, bytes, result) => KeyRow(name, bytes, result) })
      val signed = c
        .downField("signing")
        .values
        .toList
        .flatten
        .find(_.hcursor.get[String]("scheme").contains("Ed25519"))
        .toList
        .flatMap(_.hcursor.downField("keys").values.toList.flatten)
        .traverse { k =>
          hex(k.hcursor, "verifyingKey").flatMap { vk =>
            k.hcursor.downField("signatures").values.toList.flatten.traverse { s =>
              (hex(s.hcursor, "message"), hex(s.hcursor, "signature")).mapN(Signed(vk, _, _))
            }
          }
        }
        .map(_.flatten)
      (rows, keys, signed).mapN(Vectors.apply).liftTo[IO]
    }

  /** The vectors' verdict: the key is decoded first, then the signature. */
  private def verdict(b: Ed25519Backend, libsodium: Boolean)(key: ByteVector, message: ByteVector, signature: ByteVector): String =
    b.codec
      .verifyingKey(key)
      .fold(
        _ => "malformedKey",
        k =>
          b.codec
            .signature(signature)
            .fold(
              _ => "malformedSignature",
              s => if ((if (libsodium) b.libsodiumVerifier else b.verifier).verify(k, message, s)) "valid" else "invalid"
            )
      )

  private def agree[A](what: String, pure: A, native: A): Expectations =
    expect(pure == native, s"$what: pure Scala gives $pure, native gives $native")

  /** Every result the two backends can be compared on, for one input. */
  private def same(native: Ed25519Backend)(key: ByteVector, message: ByteVector, signature: ByteVector): Expectations = {
    val (vk, sig) = (VerifyingKey[Ed25519](key), Signature[Ed25519](signature))
    val label = s"key ${key.toHex}, message ${message.size} bytes, signature ${signature.toHex}"
    agree(s"verifying key ($label)", reference.codec.verifyingKey(key), native.codec.verifyingKey(key))
      .and(agree(s"signature ($label)", reference.codec.signature(signature), native.codec.signature(signature)))
      .and(agree(s"verifyBytes ($label)", reference.verifyBytes(key, message, signature), native.verifyBytes(key, message, signature)))
      .and(agree(s"ZIP-215 ($label)", reference.verifier.verify(vk, message, sig), native.verifier.verify(vk, message, sig)))
      .and(
        agree(
          s"libsodium ($label)",
          reference.libsodiumVerifier.verify(vk, message, sig),
          native.libsodiumVerifier.verify(vk, message, sig)
        )
      )
  }

  test("every Ed25519 and Ed25519-libsodium verification verdict in signing_v1.json") {
    case (native, v) =>
      IO {
        val wrong = v.rows.flatMap { r =>
          val got = verdict(native, r.scheme == "Ed25519-libsodium")(r.key, r.message, r.signature)
          Option.when(got != r.result)(s"${r.scheme} ${r.name}: expected ${r.result}, got $got")
        }
        expect.same(406, v.rows.size).and(expect(wrong.isEmpty, wrong.mkString("\n")))
      }
  }

  test("verifyBytes gives the codec's typed errors on every verification row") {
    case (native, v) =>
      IO(v.rows.filter(_.scheme == "Ed25519").foldMap { r =>
        agree(r.name, reference.verifyBytes(r.key, r.message, r.signature), native.verifyBytes(r.key, r.message, r.signature))
      })
  }

  test("every Ed25519 verifying-key decoding verdict in signing_v1.json") {
    case (native, v) =>
      IO {
        expect(v.keys.nonEmpty, "the vectors have Ed25519 verifying-key rows").and(v.keys.foldMap { k =>
          val got = native.codec.verifyingKey(k.bytes).fold(_.productPrefix, _ => "accepted")
          expect(got == k.result, s"${k.name}: expected ${k.result}, got $got")
        })
      }
  }

  test("every committed Ed25519 signature verifies under both rules") {
    case (native, v) =>
      IO {
        expect
          .same(9, v.signed.size)
          .and(v.signed.foldMap { s =>
            expect
              .same("valid", verdict(native, libsodium = false)(s.key, s.message, s.signature))
              .and(expect.same("valid", verdict(native, libsodium = true)(s.key, s.message, s.signature)))
          })
      }
  }

  private val L: BigInt = SigningRejections.L
  private def le32(i:  BigInt): ByteVector = ByteVector.view(i.toByteArray).takeRight(32).padLeft(32).reverse
  private def leInt(b: ByteVector): BigInt = BigInt(1, b.reverse.toArray)

  /** `p + d` for `d < 19`, with and without the sign bit: every non-canonical `y`, on the curve or not. */
  private val nonCanonical: List[ByteVector] =
    for {
      d    <- (0 until 19).toList
      sign <- List(0x00, 0x80)
    } yield le32(SigningRejections.p25519 + d).update(31, (0x7f | sign).toByte)

  /** Encodings where implementations have differed: small-order points, canonical or not, and every `y >= p`. */
  private val edges: List[ByteVector] = (SigningRejections.smallOrder ++ nonCanonical).distinct

  private val honest: List[(KeyPair[Ed25519], ByteVector, ByteVector)] =
    (1 to 3).toList.map { i =>
      val kp = Ed25519.seedDerivation.fromSeed(ByteVector.fill(32)(i.toByte)).toOption.get
      val m = ByteVector(s"message $i".getBytes(UTF_8))
      (kp, m, Ed25519.signer.sign(kp.signing, m).bytes)
    }

  private def sha512(b: ByteVector): Array[Byte] = {
    val d = new SHA512Digest(); val out = new Array[Byte](64); d.update(b.toArray, 0, b.size.toInt); d.doFinal(out, 0); out
  }

  /**
   * Signatures valid only under the cofactored equation: `R' = [r]B + T` for each small-order `T` but the identity, and
   * `S` computed for `R'`. ZIP-215 accepts them, libsodium's rule does not.
   */
  private val mixedOrder: List[(ByteVector, ByteVector, ByteVector)] = {
    val seed = ByteVector.fill(32)(0x42)
    val kp = Ed25519.seedDerivation.fromSeed(seed).toOption.get
    val h = sha512(seed).take(32)
    val clamped = h.updated(0, (h(0) & 248).toByte).updated(31, ((h(31) & 127) | 64).toByte)
    val a = Scalar.fromBytesModOrder(clamped)
    val m = ByteVector("mixed order".getBytes(UTF_8))
    val r = Scalar.fromBytesModOrderWide(sha512(ByteVector("nonce".getBytes(UTF_8))))
    // the seven small-order points but the identity, in canonical form
    SigningRejections.smallOrder.slice(1, 8).map { t =>
      val rPrime =
        ByteVector(Constants.ED25519_BASEPOINT.multiply(r).add(new CompressedEdwardsY(t.toArray).decompress()).compress().toByteArray)
      val k = Scalar.fromBytesModOrderWide(sha512(rPrime ++ kp.verifying.bytes ++ m))
      (kp.verifying.bytes, m, rPrime ++ ByteVector(k.multiplyAndAdd(a, r).toByteArray))
    }
  }

  test("the same verdicts on mixed-order signatures, which only ZIP-215 accepts") {
    case (native, _) =>
      IO {
        mixedOrder.foldMap {
          case (k, m, s) =>
            same(native)(k, m, s)
              .and(expect(native.verifier.verify(VerifyingKey(k), m, Signature(s)), "ZIP-215 accepts"))
              .and(expect(!native.libsodiumVerifier.verify(VerifyingKey(k), m, Signature(s)), "libsodium's rule rejects"))
        }
      }
  }

  test("the same verdicts for every edge encoding as A and as R, with S = 0 and S = L - 1") {
    case (native, _) =>
      IO {
        val m = ByteVector("Zcash".getBytes(UTF_8))
        (for {
          a <- edges
          r <- edges
          s <- List(le32(0), le32(L - 1))
        } yield same(native)(a, m, r ++ s)).combineAll
      }
  }

  private def bytes(n: Int): Gen[ByteVector] = Gen.listOfN(n, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))
  private def flipBit(b: ByteVector): Gen[ByteVector] =
    if (b.isEmpty) Gen.const(b) else Gen.choose(0L, b.size * 8 - 1).map(i => b.update(i / 8, (b(i / 8) ^ (1 << (i % 8).toInt)).toByte))

  /** Mostly honest signatures, with their parts replaced or perturbed in the ways that matter to the decoders. */
  private val inputs: Gen[(ByteVector, ByteVector, ByteVector)] =
    for {
      (kp, m0, sig) <- Gen.oneOf(honest)
      key <- Gen.frequency(5 -> Gen.const(kp.verifying.bytes), 2 -> Gen.oneOf(edges), 1 -> bytes(32), 1 -> Gen.choose(0, 40).flatMap(bytes))
      message <- Gen.frequency(5 -> Gen.const(m0), 1 -> flipBit(m0), 1 -> Gen.choose(0, 300).flatMap(bytes))
      r       <- Gen.frequency(5 -> Gen.const(sig.take(32)), 2 -> Gen.oneOf(edges), 1 -> bytes(32), 1 -> flipBit(sig.take(32)))
      s <- Gen.frequency(
        5 -> Gen.const(sig.drop(32)),
        1 -> Gen.const(le32(leInt(sig.drop(32)) + L)),
        1 -> Gen.oneOf(List(le32(0), le32(L - 1), le32(L), ByteVector.fill(32)(0xff))),
        1 -> bytes(32),
        1 -> flipBit(sig.drop(32))
      )
      signature <- Gen.frequency(8 -> Gen.const(r ++ s), 1 -> Gen.choose(0, 70).map(n => (r ++ s ++ r).take(n.toLong)))
    } yield (key, message, signature)

  private given Show[(ByteVector, ByteVector, ByteVector)] = Show.show { case (k, m, s) => s"(${k.toHex}, ${m.toHex}, ${s.toHex})" }

  test("differential: the same verdicts and errors as the pure-Scala backend on perturbed inputs") {
    case (native, _) => forall(inputs) { case (k, m, s) => same(native)(k, m, s) }
  }

  test("loading again, in sequence and concurrently, leaves every loaded backend working") {
    case (first, v) =>
      val valid = v.signed.head
      def works(b: Ed25519Backend): Expectations =
        expect
          .same(Right(true), b.verifyBytes(valid.key, valid.message, valid.signature))
          .and(expect.same(Right(false), b.verifyBytes(valid.key, valid.message ++ ByteVector(0), valid.signature)))
      for {
        again      <- Ed25519Backend.native[IO]
        concurrent <- (1 to 8).toList.parTraverse(_ => Ed25519Backend.native[IO])
        _          <- IO(System.gc())
        calls      <- (first :: again :: concurrent).parTraverse(b => IO(works(b)))
      } yield calls.combineAll
  }

  pureTest("platform names follow os.name and os.arch") {
    import Ed25519Native.{LoadError, Platform}
    expect
      .same(Right("native/linux-x86_64/libdilf4s_jvm_ffi.so"), Platform.of("Linux", "amd64").map(_.resource))
      .and(expect.same(Right("native/linux-aarch64/libdilf4s_jvm_ffi.so"), Platform.of("Linux", "aarch64").map(_.resource)))
      .and(expect.same(Right("native/darwin-aarch64/libdilf4s_jvm_ffi.dylib"), Platform.of("Mac OS X", "aarch64").map(_.resource)))
      .and(expect.same(Right("native/darwin-x86_64/libdilf4s_jvm_ffi.dylib"), Platform.of("Mac OS X", "x86_64").map(_.resource)))
      .and(expect.same(Left(LoadError.UnsupportedPlatform("Windows 11", "amd64")), Platform.of("Windows 11", "amd64")))
      .and(expect.same(Left(LoadError.UnsupportedPlatform("Linux", "riscv64")), Platform.of("Linux", "riscv64")))
  }

  test("a library that does not load is a typed failure, not a fatal error") {
    val missing = java.nio.file.Paths.get("target", "no-such-dir", "libdilf4s_jvm_ffi.so")
    Resource
      .make(IO.blocking(Files.createTempFile("not-a-library", ".so")))(f => IO.blocking(Files.deleteIfExists(f)).void)
      .use { text =>
        IO.blocking(Files.write(text, "not ELF".getBytes(UTF_8))) >>
        (Ed25519Native.loadFrom[IO](missing).attempt, Ed25519Native.loadFrom[IO](text).attempt).mapN { (a, b) =>
          def linkFailed(r: Either[Throwable, ?]): Boolean = r.left.exists(_.isInstanceOf[Ed25519Native.LoadError.LinkFailed])
          expect(linkFailed(a), s"a missing file: $a").and(expect(linkFailed(b), s"a file that is not a library: $b"))
        }
      }
  }
}
