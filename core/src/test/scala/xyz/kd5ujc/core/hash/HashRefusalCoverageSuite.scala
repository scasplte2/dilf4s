package xyz.kd5ujc.core.hash

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

import xyz.kd5ujc.core.canonical.{CanonicalJson, Preview}
import xyz.kd5ujc.models.canonical_vectors.CanonicalVectors

import weaver.SimpleIOSuite

/**
 * The coverage gate of docs/specs/conformance.md §6 for the hash family: every case of [[HashError]] has a row in
 * `canonical_encoding_v1.json` (its domain tags, algorithm names, digest hex and expansions) that the reference refuses
 * with it. The cases come from the compiler (`Mirror.SumOf`), so a kind added to `hash-errors.cddl` fails this suite
 * until a row reaches it; the rows are read through their generated models, so a row naming a kind the schema does not
 * list does not load.
 */
object HashRefusalCoverageSuite extends SimpleIOSuite {

  inline def kinds[T](using m: Mirror.SumOf[T]): List[String] =
    constValueTuple[m.MirroredElemLabels].productIterator.map(_.toString).toList

  private val vectors: IO[CanonicalVectors] =
    IO.blocking(new String(Files.readAllBytes(Paths.get("shared", "vectors", "canonical_encoding_v1.json")), UTF_8)).flatMap { text =>
      CanonicalJson
        .parse(text)
        .leftMap(_.toString)
        .flatMap(CanonicalVectors.codec.decoder.decode(_).leftMap(_.message))
        .leftMap(new IllegalStateException(_))
        .liftTo[IO]
    }

  /** The kind each section's rows record, as the vectors write it. */
  val pinned: IO[Set[String]] = vectors.map { v =>
    (v.domainTags.map(_.outcome) ++ v.algorithms.map(_.outcome) ++ v.digestHex.map(_.outcome) ++ v.expansions.map(_.outcome))
      .map(identity[String])
      .toSet - "accepted"
  }

  test("every HashError kind has a vector row, and every row names one of its kinds") {
    pinned.map { rows =>
      val all = kinds[HashError]
      expect(all.filterNot(rows).isEmpty, s"no vector row pins ${all.filterNot(rows).mkString(", ")}")
        .and(expect((rows -- all).isEmpty, s"rows name kinds HashError does not have: ${rows -- all}"))
    }
  }

  test("every expansion refusal has a row, and only an InvalidExpansion row names one") {
    vectors.map { v =>
      val refused = v.expansions.flatMap(_.refusal).map(identity[String]).toSet
      val all = ExpansionRefusal.values.toList.map(identity[String])
      expect(all.filterNot(refused).isEmpty, s"no row pins the expansion refusals ${all.filterNot(refused).mkString(", ")}")
        .and(
          expect(
            v.expansions.forall(e => e.refusal.isDefined == ((e.outcome: String) == "InvalidExpansion")),
            "refusal exactly with InvalidExpansion"
          )
        )
    }
  }

  pureTest("each case's kind is its name, and the schema lists the kinds in the family's order") {
    val samples: List[HashError] = List(
      HashError.InvalidLength("SHA-256", 32, 31),
      HashError.InvalidHex(0),
      HashError.InvalidDomainTag(Preview.of("x")),
      HashError.InvalidExpansion("emptyDst", "SHA-256", 8160),
      HashError.UnknownAlgorithm(Preview.of("x"))
    )
    expect
      .same(kinds[HashError], samples.map(_.productPrefix))
      .and(samples.foldMap(e => expect.same(e.productPrefix, e.kind: String)))
      .and(expect.same(kinds[HashError], HashErrorKind.values.toList.map(identity[String])))
  }
}
