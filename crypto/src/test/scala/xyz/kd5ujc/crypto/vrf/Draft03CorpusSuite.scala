package xyz.kd5ujc.crypto.vrf

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.syntax.all._

import xyz.kd5ujc.core.hash.{Hasher, Sha256}

import cafe.cryptography.curve25519.{Constants, Scalar}
import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/** Replays the libsodium (Cardano) oracle corpus against [[Vrf.draft03]]; see the corpus README for provenance. */
object Draft03CorpusSuite extends SimpleIOSuite {

  private val dir = Paths.get("docs", "test-vectors", "vrf", "draft03-corpus")
  private def raw(name:   String): Array[Byte] = Files.readAllBytes(dir.resolve(name))
  private def lines(name: String): Vector[String] = new String(raw(name), UTF_8).linesIterator.toVector
  private def cols(line:  String): Vector[String] = line.split("\t", -1).toVector
  private def hex(s:      String): ByteVector = ByteVector.fromValidHex(s)

  /** `NULL` cells are the oracle's null-pointer envelopes; a typed API cannot receive null, so they are malformed. */
  private def outcome(row: String): String = {
    val c = cols(row)
    if (c.contains("NULL")) "MALFORMED"
    else
      Vrf.draft03.verifyBytes(hex(c(1)), hex(c(3)), hex(c(2))) match {
        case Right(beta)                    => s"VALID:${beta.bytes.toHex}"
        case Left(VrfError.Malformed(_, _)) => "MALFORMED"
        case Left(_)                        => "INVALID"
      }
  }

  test("corpus files match their recorded SHA-256 sums") {
    cats.effect.IO(lines("SHA256SUMS")).map { sums =>
      expect
        .same(6, sums.size)
        .and(sums.foldMap { l =>
          val Array(sum, name) = l.split("  ", 2)
          expect.same(sum, Hasher[Sha256].digest(ByteVector.view(raw(name))).hex)
        })
    }
  }

  private def replay(name: String, rowsFile: String, expectedFile: String, count: Int, valid: Int, malformed: Int) =
    test(s"$name: $count oracle verdicts") {
      cats.effect.IO((lines(rowsFile), lines(expectedFile))).map {
        case (rows, expected) =>
          val actual = rows.map(r => s"${cols(r).head}\t${outcome(r)}")
          val mismatches = actual.zip(expected).filter { case (a, e) => a != e }
          expect
            .same(count, rows.size)
            .and(expect.same(count, expected.size))
            .and(expect.same(count, rows.map(cols(_).head).distinct.size))
            .and(expect.same(valid, expected.count(_.contains("\tVALID:"))))
            .and(expect.same(malformed, expected.count(_.endsWith("\tMALFORMED"))))
            .and(expect(mismatches.isEmpty, s"${mismatches.size} mismatches, first: ${mismatches.take(3).mkString("; ")}"))
      }
    }

  replay("original corpus", "vectors.tsv", "expected.tsv", 2048, 3, 27)
  replay("additional corpus", "additional.tsv", "additional-results.tsv", 2338, 3, 0)

  private val profile = Vrf.Draft03Profile
  private def b(pt: cafe.cryptography.curve25519.EdwardsPoint) = Curve25519Maps.bytes(pt)

  /** The four helper kinds, evaluated with the production functions (same output formats as the oracle helper). */
  private def helper(row: String): String = {
    val c = cols(row)
    val in = hex(c(2))
    c(1) match {
      case "d" =>
        Curve25519Maps.decodeDraft03(in).fold("INVALID")(p => (if (Curve25519Maps.isSmallOrder(p)) "SMALL:" else "POINT:") + b(p).toHex)
      case "u" => Curve25519Maps.fromUniformDraft03(in).fold("INVALID")(p => b(p).toHex)
      case "e" =>
        (Curve25519Maps.decodeDraft03(in), Curve25519Maps.decodeDraft03(hex(c(3))))
          .mapN((p, q) => b(profile.equation(p, q, Vrf.scalar16(hex(c(4))), Scalar.fromBytesModOrder(hex(c(5)).toArray))).toHex)
          .getOrElse("INVALID")
      case _ =>
        val (pi, alpha) = (hex(c(3)), hex(c(4)))
        val (gb, cb, sb) = (pi.take(32), pi.slice(32, 48), pi.drop(48))
        (for {
          y     <- Curve25519Maps.decodeDraft03(in).filterNot(Curve25519Maps.isSmallOrder)
          gamma <- Curve25519Maps.decodeDraft03(gb)
          s     <- Option.when(Curve25519Maps.le(sb) < Curve25519Maps.L)(Scalar.fromBytesModOrder(sb.toArray))
          h     <- Curve25519Maps.hashToCurveDraft03(b(y), alpha)
          u = profile.equation(y, Constants.ED25519_BASEPOINT, Vrf.scalar16(cb), s)
          v = profile.equation(gamma, h, Vrf.scalar16(cb), s)
        } yield
          (b(h) ++ b(u) ++ b(v) ++ profile.challenge(in, b(h), gb, b(u), b(v)) ++
          Hasher[xyz.kd5ujc.core.hash.Sha512].digest(ByteVector(0x04, 0x03) ++ b(gamma.multiplyByCofactor())).bytes).toHex)
          .getOrElse("INVALID")
    }
  }

  List("d" -> 612, "u" -> 519, "e" -> 240, "t" -> 2055).foreach {
    case (op, count) =>
      test(s"helper '$op': $count intermediate-step comparisons") {
        cats.effect.IO((lines("helpers.tsv"), lines("helper-results.tsv"))).map {
          case (all, expected) =>
            val rows = all.zip(expected).filter { case (r, _) => cols(r)(1) == op }
            val mismatches = rows.filter { case (r, e) => s"${cols(r).head}\t${helper(r)}" != e }
            expect
              .same(3426, all.size)
              .and(expect.same(count, rows.size))
              .and(expect(mismatches.isEmpty, s"${mismatches.size} mismatches, first: ${mismatches.take(2).mkString("; ")}"))
        }
      }
  }
}
