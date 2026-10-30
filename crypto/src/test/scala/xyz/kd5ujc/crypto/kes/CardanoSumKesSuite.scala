package xyz.kd5ujc.crypto.kes

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.core.hash.{Blake2b256, Hasher, Sha256}

import org.scalacheck.Gen
import scodec.bits.{ByteVector, HexStringSyntax}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object CardanoSumKesSuite extends SimpleIOSuite with Checkers {

  private val dir = Paths.get("docs", "test-vectors", "kes", "cardano")
  private def raw(name: String) = Files.readAllBytes(dir.resolve(name))
  private def rows(name: String): Vector[Vector[String]] =
    new String(raw(name), UTF_8).linesIterator.map(_.split("\t", -1).toVector).toVector
  private def hex(s: String) = ByteVector.fromValidHex(s)
  private val sum6 = CardanoSumKes.Sum6

  test("fixture files match their recorded SHA-256 sums") {
    IO(new String(raw("SHA256SUMS"), UTF_8).linesIterator.toVector).map { sums =>
      expect
        .same(3, sums.size)
        .and(sums.foldMap { l =>
          val Array(sum, name) = l.split("  ", 2)
          expect.same(sum, Hasher[Sha256].digest(ByteVector.view(raw(name))).hex)
        })
    }
  }

  pureTest("cardano-cli key-gen-KES: the depth-5 tree regenerated from r1 has root vk1, and vk = H(vk0 || vk1)") {
    val depth5 = CardanoSumKes(5).toOption.get
    val vs = rows("keygen.tsv")
    expect
      .same(3, vs.size)
      .and(vs.foldMap {
        case Vector(_, r1, vk1, vk0, vk) =>
          expect
            .same(Right(hex(vk1)), depth5.generate(hex(r1)).map(depth5.verifyingKey))
            .and(expect.same(hex(vk), Hasher[Blake2b256].digest(hex(vk0) ++ hex(vk1)).bytes))
        case other => failure(s"bad row $other")
      })
  }

  private val originals = rows("originals.tsv").map(r => r(0) -> r).toMap

  private def flip(b: ByteVector, at: Long, bit: Int) = b.update(at, (b(at) ^ (1 << bit)).toByte)

  /** Rebuilds a mutated input from its case name, as the corpus describes. */
  private def mutate(r: Vector[String], recipe: String): (ByteVector, Long, ByteVector, ByteVector) = {
    val (root, period, msg, sig) = (hex(r(1)), r(2).toLong, hex(r(3)), hex(r(4)))
    recipe.split("/").toList match {
      case List("period", p)              => (root, p.toLong, msg, sig)
      case List("root", "xor", o, b)      => (flip(root, o.toLong, b.toInt), period, msg, sig)
      case List("signature", "xor", o, b) => (root, period, msg, flip(sig, o.toLong, b.toInt))
      case List("message", "xor", o, b)   => (root, period, flip(msg, o.toLong, b.toInt), sig)
      case List("signature", "swap-pair", d) =>
        val off = 64L + 64L * (d.toLong - 1)
        (root, period, msg, sig.take(off) ++ sig.slice(off + 32, off + 64) ++ sig.slice(off, off + 32) ++ sig.drop(off + 64))
      case List("message", "empty")         => (root, period, ByteVector.empty, sig)
      case List("message", "truncate-last") => (root, period, msg.dropRight(1), sig)
      case List("message", "append-zero")   => (root, period, msg :+ 0.toByte, sig)
      case _                                => (root, period, msg, sig)
    }
  }

  test("17,336 verdicts on four preprod headers and their mutations") {
    IO(rows("observations.tsv")).map { obs =>
      val mismatches = obs.filter { o =>
        val (root, period, msg, sig) = mutate(originals(o(0)), o(1))
        val verdict = if (sum6.verify(root, period, msg, sig) == Right(true)) "VERIFIED" else "REJECTED"
        verdict != o(2)
      }
      expect
        .same(17336, obs.size)
        .and(expect.same(4, obs.count(_(2) == "VERIFIED")))
        .and(
          expect(
            mismatches.isEmpty,
            s"${mismatches.size} mismatches, first: ${mismatches.take(3).map(_.take(3).mkString(" ")).mkString("; ")}"
          )
        )
    }
  }

  // ------------------------------------------------------------------ properties (Sum3: 8 periods keeps them fast)

  private val sum3 = CardanoSumKes(3).toOption.get
  implicit private val showBytes: cats.Show[ByteVector] = cats.Show.show(_.toHex)
  private val seed: Gen[ByteVector] = Gen.listOfN(32, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))

  private def periods(k: CardanoSumKes)(key: k.Key): List[k.Key] =
    List.unfold(Option(key))(_.map(cur => (cur, k.evolve(cur).toOption)))

  test("every period signs, verifies only at that period, and the last period cannot evolve") {
    forall(seed) { s =>
      val k0 = sum3.generate(s).toOption.get
      val vk = sum3.verifyingKey(k0)
      val keys = periods(sum3)(k0)
      expect
        .same(8, keys.size)
        .and(expect(keys.forall(k => sum3.verifyingKey(k) == vk), "the root key never changes"))
        .and(keys.foldMap { k =>
          val sig = sum3.sign(k, hex"cafe")
          expect
            .same(sum3.signatureLength, sig.size)
            .and(expect.same(Right(true), sum3.verify(vk, k.period, hex"cafe", sig)))
            .and(
              expect(
                (0L until 8L).filterNot(_ == k.period).forall(p => sum3.verify(vk, p, hex"cafe", sig) == Right(false)),
                "other periods"
              )
            )
            .and(expect.same(Right(false), sum3.verify(vk, k.period, hex"caff", sig)))
        })
        .and(expect.same(Left(KesError.Exhausted(7L)), sum3.evolve(keys.last)))
    }
  }

  pureTest("a raw key loads at its period and continues; layout sizes and erased seeds match cardano-base") {
    val k0 = sum6.generate(ByteVector.fill(32)(1)).toOption.get
    val k40 = (1 to 40).foldLeft(k0)((k, _) => sum6.evolve(k).toOption.get)
    val reloaded = sum6.load(k40.bytes, 40).toOption.get
    expect
      .same(608L, k0.bytes.size)
      .and(expect.same(448L, sum6.signatureLength))
      .and(expect.same(sum6.sign(k40, hex"01"), sum6.sign(reloaded, hex"01")))
      .and(expect.same(sum6.evolve(k40).map(_.bytes), sum6.evolve(reloaded).map(_.bytes)))
      .and(expect.same(ByteVector.low(32), k40.bytes.slice(512, 544)))
      .and(expect(!k0.toString.contains(k0.bytes.toHex.take(16)), "redacted"))
  }

  pureTest("a key loaded at the wrong period, or with a corrupted r1, is refused") {
    val k0 = sum6.generate(ByteVector.fill(32)(5)).toOption.get
    val k31 = sum6.evolveTo(k0, 31).toOption.get
    val badR1 = k31.bytes.update(512L, (k31.bytes(512L) ^ 1).toByte)
    expect(sum6.load(k0.bytes, 32).isLeft, "period-0 key as period 32")
      .and(expect(sum6.load(k0.bytes, 1).isLeft, "period-0 key as period 1"))
      .and(expect(sum6.load(sum6.evolve(k31).toOption.get.bytes, 31).isLeft, "period-32 key as period 31"))
      .and(expect(sum6.load(badR1, 31).flatMap(sum6.evolve).isLeft, "corrupted r1 caught on activation"))
      .and(
        expect((0L until 64L).forall(p => sum6.evolveTo(k0, p).flatMap(k => sum6.load(k.bytes, p)).isRight), "every honest period loads")
      )
  }

  pureTest("malformed inputs and out-of-range periods are typed errors") {
    val k = sum6.generate(ByteVector.fill(32)(2)).toOption.get
    val vk = sum6.verifyingKey(k)
    val sig = sum6.sign(k, hex"")
    expect(sum6.verify(vk.drop(1), 0, hex"", sig).isLeft)
      .and(expect(sum6.verify(vk, 0, hex"", sig.drop(1)).isLeft))
      .and(expect.same(Left(KesError.PeriodOutOfRange(64, 64)), sum6.verify(vk, 64, hex"", sig)))
      .and(expect(sum6.generate(ByteVector.fill(31)(0)).isLeft))
      .and(expect(sum6.load(k.bytes, 64).isLeft))
      .and(expect(CardanoSumKes(8).isLeft))
  }
}
