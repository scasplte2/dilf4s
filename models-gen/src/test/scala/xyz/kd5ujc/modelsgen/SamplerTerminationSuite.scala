package xyz.kd5ujc.modelsgen

import java.nio.charset.StandardCharsets.UTF_8

import cats.effect.IO
import cats.syntax.all._

import scala.util.Random

import xyz.kd5ujc.core.hash.{Hasher, Sha256}

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

object SamplerTerminationSuite extends SimpleIOSuite {

  private def sampler(cddl: String, maxDepth: Int = 7): Either[String, CddlSampler] =
    CddlParser.parse(cddl).bimap(_.message, file => new CddlSampler(Schema.build(Vector("t" -> Vector(file))), maxDepth))

  private val r = QName("t", "r")

  test("direct, indirect and group cycles return a typed refusal in bounded time") {
    Vector("r = r\n", "r = s\ns = r\n", "r = [g]\ng = (g, uint)\n")
      .traverse(cddl =>
        IO(sampler(cddl).map(_.sample(r, new Random(1)))).timeout(scala.concurrent.duration.DurationInt(5).seconds).map(cddl -> _)
      )
      .map(_.foldMap {
        case (_, Right(Left(GenError.SamplingCycle(q)))) => expect(Set("r", "s", "g")(q.name))
        case (cddl, other)                               => failure(s"expected a sampling cycle for $cddl, got $other")
      })
  }

  pureTest("a productive choice with a recursive first alternative refuses its cutoff cycle") {
    expect
      .same(Right(Left(GenError.SamplingCycle(r))), sampler("r = r / uint\n", maxDepth = 0).map(_.sample(r, new Random(1))))
      .and(expect(sampler("r = uint / [r]\n", maxDepth = 0).exists(_.sample(r, new Random(1)).isRight)))
  }

  pureTest("greedy optional samples retain required suffixes through enclosing groups") {
    Vector(
      "r = [? before: uint, after: uint]\n"                      -> 2,
      "r = [g, tail: uint]\ng = (? before: uint, after: uint)\n" -> 3,
      "r = [g, tail: uint]\ng = (? before: uint)\n"              -> 2
    ).foldMap {
      case (cddl, size) =>
        (1L to 64L).toList.foldMap { seed =>
          val head = sampler(cddl).flatMap(_.sample(r, new Random(seed)).leftMap(_.message)).map(_.headOption.map(_ & 0xff))
          expect.same(Right(Some(0x80 + size)), head)
        }
    }
  }

  /** Captured in scarab (fac5554c) before the cycle-refusal change: 384 samples, seeds 20261018..20. */
  pureTest("fixed-seed recursive and compound samples preserve their original bytes") {
    val schemas = Vector("r = uint / [r]\n", "r = [0, bytes .size 4, ? text, * uint]\n", "r = { * uint => bytes }\n")
    val samples = schemas.zipWithIndex.traverse {
      case (cddl, i) =>
        sampler(cddl).flatMap { s =>
          val rnd = new Random(20261018L + i)
          Vector.fill(128)(s.sample(r, rnd)).traverse(_.leftMap(_.message))
        }
    }
    val transcript = samples.map(_.flatten.foldLeft(ByteVector.empty) { (acc, bytes) =>
      acc ++ ByteVector(bytes.length.toString.getBytes(UTF_8)) ++ ByteVector(0) ++ ByteVector(bytes)
    })
    expect.same(
      Right("14a0d6242eaf59048296048ccd9f751d4edcef1e2d073623370ef50a861f01c1"),
      transcript.map(t => Hasher[Sha256].digest(t).hex)
    )
  }
}
