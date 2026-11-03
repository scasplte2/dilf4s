package xyz.kd5ujc.crypto.vrf

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.Show
import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.crypto.SigningRejections
import xyz.kd5ujc.crypto.signing.Edwards25519Backend

import io.circe.{ACursor, Decoder, Json, parser}
import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.scalacheck.{CheckConfig, Checkers}
import weaver.{Expectations, IOSuite}

/**
 * The native ECVRF (dilf4s-signing through sdk/rust/jvm-ffi) against vrf_v1.json, the RFC 9381 and draft-03 examples,
 * the libsodium (Cardano) oracle corpora, and the pure-Scala suites: the same output or the same typed error on every
 * input.
 */
object VrfNativeSuite extends IOSuite with Checkers {

  override def checkConfig: CheckConfig = CheckConfig.default.withMinimumSuccessful(300)

  type Res = Edwards25519Backend
  def sharedResource: Resource[IO, Res] = Resource.eval(Edwards25519Backend.native[IO])

  /** A suite on both backends. */
  final case class Pair[S](pure: Vrf[S], native: Vrf[S])

  private def pairs(b: Edwards25519Backend): List[Pair[?]] =
    List(Pair(Vrf.tai, b.vrfTai), Pair(Vrf.ell2, b.vrfEll2), Pair(Vrf.draft03, b.vrfDraft03))

  private def read(path: Path): IO[Json] = IO.blocking(new String(Files.readAllBytes(path), UTF_8)).flatMap(parser.parse(_).liftTo[IO])
  private def hex(c: ACursor, field: String): Decoder.Result[ByteVector] =
    c.get[String](field).flatMap(ByteVector.fromHex(_).toRight(io.circe.DecodingFailure(s"$field is not hex", Nil)))

  /** The kind a result has in vrf_v1.json. */
  private def kind(r: Either[VrfError, ?]): String = r.fold(_.productPrefix, _ => "valid")

  private def agree[A](what: String, pure: A, native: A): Expectations =
    expect(pure == native, s"$what: pure Scala gives $pure, native gives $native")

  /** Every result the two backends can be compared on, for one input. */
  private def same[S](p: Pair[S])(key: ByteVector, alpha: ByteVector, proof: ByteVector): Expectations = {
    val label = s"${p.pure.name}: key ${key.toHex}, alpha ${alpha.size} bytes, proof ${proof.toHex}"
    agree(s"verifyingKey ($label)", p.pure.verifyingKey(key), p.native.verifyingKey(key))
      .and(agree(s"verifyBytes ($label)", p.pure.verifyBytes(key, alpha, proof), p.native.verifyBytes(key, alpha, proof)))
  }

  test("every vrf_v1.json proof verifies natively to its output, and every rejection is the same typed error") { b =>
    read(Paths.get("shared", "vectors", "vrf_v1.json")).map { json =>
      val c = json.hcursor
      val byName = pairs(b).map(p => p.pure.name -> p).toMap
      val proofs = c.downField("suites").values.toList.flatten.flatMap { s =>
        val p = byName(s.hcursor.get[String]("suite").toOption.get)
        s.hcursor.downField("keys").values.toList.flatten.flatMap { k =>
          val vk = hex(k.hcursor, "verifyingKey").toOption.get
          k.hcursor.downField("proofs").values.toList.flatten.map { pr =>
            val (alpha, pi, beta) = (hex(pr.hcursor, "alpha"), hex(pr.hcursor, "pi"), hex(pr.hcursor, "beta")).tupled.toOption.get
            expect.same(Right(beta), p.native.verifyBytes(vk, alpha, pi).map(_.bytes)).and(same(p)(vk, alpha, pi))
          }
        }
      }
      val rejections = c.downField("rejections").values.toList.flatten.map { r =>
        val h = r.hcursor
        val p = byName(h.get[String]("suite").toOption.get)
        val (vk, alpha, pi) = (hex(h, "verifyingKey"), hex(h, "alpha"), hex(h, "pi")).tupled.toOption.get
        val expected = h.get[String]("result").toOption.get
        val got = kind(p.native.verifyBytes(vk, alpha, pi))
        expect(got == expected, s"${p.pure.name} ${h.get[String]("name")}: expected $expected, got $got").and(same(p)(vk, alpha, pi))
      }
      expect.same(27, proofs.size).and(expect.same(33, rejections.size)).and(proofs.combineAll).and(rejections.combineAll)
    }
  }

  test("RFC 9381 B.3 and B.4, and draft-03 A.4: every example verifies natively to its output") { b =>
    val dir = Paths.get("docs", "test-vectors", "vrf")
    (read(dir.resolve("rfc9381.json")), read(dir.resolve("draft03.json"))).mapN { (rfc, draft) =>
      def examples[S](p: Pair[S], j: Json, field: String): List[Expectations] =
        j.hcursor.downField(field).values.toList.flatten.map { r =>
          val (pk, alpha, pi, beta) =
            (hex(r.hcursor, "PK"), hex(r.hcursor, "alpha"), hex(r.hcursor, "pi"), hex(r.hcursor, "beta")).tupled.toOption.get
          expect.same(Right(beta), p.native.verifyBytes(pk, alpha, pi).map(_.bytes)).and(same(p)(pk, alpha, pi))
        }
      val all = examples(Pair(Vrf.tai, b.vrfTai), rfc, "tai") ++ examples(Pair(Vrf.ell2, b.vrfEll2), rfc, "ell2") ++
        examples(Pair(Vrf.draft03, b.vrfDraft03), draft, "examples")
      expect.same(9, all.size).and(all.combineAll)
    }
  }

  private val corpus = Paths.get("docs", "test-vectors", "vrf", "draft03-corpus")
  private def lines(name: String): IO[Vector[String]] =
    IO.blocking(new String(Files.readAllBytes(corpus.resolve(name)), UTF_8).linesIterator.toVector)

  /** As Draft03CorpusSuite: `NULL` cells are null-pointer envelopes, which a typed API cannot receive. */
  private def outcome(v: Vrf[EcVrfDraft03])(row: String): String = {
    val c = row.split("\t", -1).toVector
    if (c.contains("NULL")) "MALFORMED"
    else
      v.verifyBytes(ByteVector.fromValidHex(c(1)), ByteVector.fromValidHex(c(3)), ByteVector.fromValidHex(c(2))) match {
        case Right(beta)                    => s"VALID:${beta.bytes.toHex}"
        case Left(VrfError.Malformed(_, _)) => "MALFORMED"
        case Left(_)                        => "INVALID"
      }
  }

  private def replay(name: String, rowsFile: String, expectedFile: String, count: Int) =
    test(s"libsodium $name: $count oracle verdicts through the native draft-03, each the pure-Scala one's too") { b =>
      (lines(rowsFile), lines(expectedFile)).mapN { (rows, expected) =>
        val native = rows.map(r => s"${r.split("\t", -1).head}\t${outcome(b.vrfDraft03)(r)}")
        val mismatches = native.zip(expected).filter { case (a, e) => a != e }
        val disagreements = rows.filter(r => outcome(b.vrfDraft03)(r) != outcome(Vrf.draft03)(r))
        expect
          .same(count, rows.size)
          .and(expect(mismatches.isEmpty, s"${mismatches.size} mismatches, first: ${mismatches.take(3).mkString("; ")}"))
          .and(expect(disagreements.isEmpty, s"${disagreements.size} disagreements with pure Scala"))
      }
    }

  replay("original corpus", "vectors.tsv", "expected.tsv", 2048)
  replay("additional corpus", "additional.tsv", "additional-results.tsv", 2338)

  private val L: BigInt = SigningRejections.L
  private def le32(i: BigInt): ByteVector = ByteVector.view(i.toByteArray).takeRight(32).padLeft(32).reverse

  /** Encodings where decoders differ: small-order points, canonical or not, and every `y >= p`, with both sign bits. */
  private val edges: List[ByteVector] =
    (SigningRejections.smallOrder ++ (0 until 19).toList.flatMap { d =>
      List(0x00, 0x80).map(sign => le32(SigningRejections.p25519 + d).update(31, (0x7f | sign).toByte))
    }).distinct

  private def bytes(n: Int): Gen[ByteVector] = Gen.listOfN(n, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))
  private def flipBit(b: ByteVector): Gen[ByteVector] =
    if (b.isEmpty) Gen.const(b) else Gen.choose(0L, b.size * 8 - 1).map(i => b.update(i / 8, (b(i / 8) ^ (1 << (i % 8).toInt)).toByte))

  /** Honest proofs with their key, alpha, `Gamma`, `c` or `s` replaced or perturbed, and some wrong lengths. */
  private def inputs[S](v: Vrf[S]): Gen[(ByteVector, ByteVector, ByteVector)] = {
    val honest = (1 to 3).toList.map { i =>
      val kp = Vrf.fromSeed[S](ByteVector.fill(32)(i.toByte))(using v).toOption.get
      val alpha = ByteVector(s"alpha $i".getBytes(UTF_8))
      (kp.verifying.bytes, alpha, v.prove(kp.signing, alpha).toOption.get._1.bytes)
    }
    for {
      (k0, a0, pi) <- Gen.oneOf(honest)
      key          <- Gen.frequency(5 -> Gen.const(k0), 2 -> Gen.oneOf(edges), 1 -> bytes(32), 1 -> Gen.choose(0, 40).flatMap(bytes))
      alpha        <- Gen.frequency(5 -> Gen.const(a0), 1 -> flipBit(a0), 1 -> Gen.choose(0, 100).flatMap(bytes))
      gamma        <- Gen.frequency(5 -> Gen.const(pi.take(32)), 2 -> Gen.oneOf(edges), 1 -> flipBit(pi.take(32)))
      c            <- Gen.frequency(5 -> Gen.const(pi.slice(32, 48)), 1 -> flipBit(pi.slice(32, 48)))
      s <- Gen.frequency(
        5 -> Gen.const(pi.drop(48)),
        1 -> Gen.oneOf(List(le32(L - 1), le32(L), le32(L + 1), ByteVector.fill(32)(0xff))),
        1 -> flipBit(pi.drop(48))
      )
      proof <- Gen.frequency(8 -> Gen.const(gamma ++ c ++ s), 1 -> Gen.choose(0, 90).map(n => (gamma ++ c ++ s ++ c).take(n.toLong)))
    } yield (key, alpha, proof)
  }

  private given Show[(ByteVector, ByteVector, ByteVector)] = Show.show { case (k, a, p) => s"(${k.toHex}, ${a.toHex}, ${p.toHex})" }

  test("differential, TAI: the same output or typed error as pure Scala on perturbed inputs") { b =>
    forall(inputs(Vrf.tai)) { case (k, a, p) => same(Pair(Vrf.tai, b.vrfTai))(k, a, p) }
  }

  test("differential, ELL2: the same output or typed error as pure Scala on perturbed inputs") { b =>
    forall(inputs(Vrf.ell2)) { case (k, a, p) => same(Pair(Vrf.ell2, b.vrfEll2))(k, a, p) }
  }

  test("differential, draft-03: the same output or typed error as pure Scala on perturbed inputs") { b =>
    forall(inputs(Vrf.draft03)) { case (k, a, p) => same(Pair(Vrf.draft03, b.vrfDraft03))(k, a, p) }
  }

  test("every edge encoding as the key and as Gamma, in every suite: the same result on both backends") { b =>
    IO {
      pairs(b).foldMap { p =>
        def run[S](q: Pair[S]): Expectations = {
          val kp = Vrf.fromSeed[S](ByteVector.fill(32)(9))(using q.pure).toOption.get
          val alpha = ByteVector("edges".getBytes(UTF_8))
          val pi = q.pure.prove(kp.signing, alpha).toOption.get._1.bytes
          edges.foldMap(e => same(q)(e, alpha, pi).and(same(q)(kp.verifying.bytes, alpha, e ++ pi.drop(32))))
        }
        run(p)
      }
    }
  }

  test("signing keys and proving stay pure Scala's; proofs prove and verify across backends") { b =>
    IO {
      pairs(b).foldMap { p =>
        def run[S](q: Pair[S]): Expectations = {
          val kp = Vrf.fromSeed[S](ByteVector.fill(32)(3))(using q.pure).toOption.get
          val alpha = ByteVector("round trip".getBytes(UTF_8))
          val proved = q.native.prove(kp.signing, alpha)
          expect
            .same(q.pure.prove(kp.signing, alpha), proved)
            .and(expect.same(proved.map(_._2), proved.flatMap { case (pi, _) => q.native.verify(kp.verifying, alpha, pi) }))
            .and(expect.same(q.pure.publicKey(kp.signing), q.native.publicKey(kp.signing)))
        }
        run(p)
      }
    }
  }
}
