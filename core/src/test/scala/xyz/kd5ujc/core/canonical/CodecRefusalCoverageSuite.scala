package xyz.kd5ujc.core.canonical

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

import xyz.kd5ujc.models.canonical_vectors.{CanonicalErrorKind, CanonicalJsonRejected, CanonicalVectors}
import xyz.kd5ujc.models.versioned_trie_vectors.VersionedTrieVectors

import weaver.SimpleIOSuite

/**
 * The coverage gate of docs/specs/conformance.md §6 for the canonical codecs: every case of [[CanonicalError]] has a
 * vector row pinning it by kind. The cases come from the compiler (`Mirror.SumOf`), so a new case fails this suite
 * until it is pinned; the pinned kinds come from the committed vector files, read through their generated models, so
 * a row naming a kind the schema's `canonical-error-kind` does not list does not load.
 */
object CodecRefusalCoverageSuite extends SimpleIOSuite {

  /** The case names of a sealed type, in declaration order. */
  inline def kinds[T](using m: Mirror.SumOf[T]): List[String] =
    constValueTuple[m.MirroredElemLabels].productIterator.map(_.toString).toList

  /** The schema's kind of each case: exhaustive, so a new case does not compile until it is mapped. */
  def schemaKind(e: CanonicalError): CanonicalErrorKind = e match {
    case CanonicalError.UnpairedSurrogate       => "UnpairedSurrogate"
    case _: CanonicalError.DuplicateKey         => "DuplicateKey"
    case _: CanonicalError.RationalTooWide      => "RationalTooWide"
    case _: CanonicalError.IntegerTooWide       => "IntegerTooWide"
    case _: CanonicalError.JsonSyntax           => "JsonSyntax"
    case _: CanonicalError.InvalidNumber        => "InvalidNumber"
    case _: CanonicalError.NotJsonRepresentable => "NotJsonRepresentable"
    case _: CanonicalError.TypeMismatch         => "TypeMismatch"
    case _: CanonicalError.UnexpectedEnd        => "UnexpectedEnd"
    case _: CanonicalError.TrailingBytes        => "TrailingBytes"
    case _: CanonicalError.DepthExceeded        => "DepthExceeded"
    case _: CanonicalError.NonCanonical         => "NonCanonical"
    case _: CanonicalError.Unsupported          => "Unsupported"
    case _: CanonicalError.InvalidUtf8          => "InvalidUtf8"
  }

  private def load[A](file: String, codec: Codec[A]): IO[A] =
    IO.blocking(new String(Files.readAllBytes(Paths.get("shared", "vectors", file)), UTF_8)).flatMap { text =>
      CanonicalJson
        .parse(text)
        .leftMap(_.toString)
        .flatMap(codec.decoder.decode(_).leftMap(_.message))
        .leftMap(new IllegalStateException(_))
        .liftTo[IO]
    }

  /** The kinds the rows pin: JSON, render and CBOR refusals, and the typed encoder's refusal of a state key. */
  val pinned: IO[Set[String]] =
    (load("canonical_encoding_v1.json", CanonicalVectors.codec), load("versioned_trie_v1.json", VersionedTrieVectors.codec)).mapN {
      (c, vt) =>
        val json = c.json.collect { case r: CanonicalJsonRejected => r.error }
        (json ++ c.renderRejections.map(_.error) ++ c.cborRejections.map(_.error) ++ vt.stateKeys.map(_.schema))
          .map(identity[String])
          .toSet - "accepted"
    }

  test("every CanonicalError kind has a vector row, and every row names one of its kinds") {
    pinned.map { rows =>
      val all = kinds[CanonicalError]
      expect(all.filterNot(rows).isEmpty, s"no vector row pins ${all.filterNot(rows).mkString(", ")}")
        .and(expect((rows -- all).isEmpty, s"rows name kinds CanonicalError does not have: ${rows -- all}"))
    }
  }

  pureTest("the schema's kind of each case is its name, as the vectors record it") {
    val samples: List[CanonicalError] = List(
      CanonicalError.UnpairedSurrogate,
      CanonicalError.DuplicateKey(Preview.of("k")),
      CanonicalError.RationalTooWide(1),
      CanonicalError.IntegerTooWide(1),
      CanonicalError.JsonSyntax(0, ""),
      CanonicalError.InvalidNumber(NumberSource.NoDecimal, None),
      CanonicalError.NotJsonRepresentable(""),
      CanonicalError.TypeMismatch(""),
      CanonicalError.UnexpectedEnd(0),
      CanonicalError.TrailingBytes(0),
      CanonicalError.DepthExceeded(1),
      CanonicalError.NonCanonical(0, ""),
      CanonicalError.Unsupported(0, ""),
      CanonicalError.InvalidUtf8(0)
    )
    expect
      .same(kinds[CanonicalError], samples.map(_.productPrefix))
      .and(samples.foldMap(e => expect.same(e.productPrefix, schemaKind(e): String)))
  }

  sealed trait Probe
  object Probe {
    case object Pinned extends Probe
    case object Unpinned extends Probe
  }

  pureTest("the gate finds a case with no row: the cases come from the compiler, not from a list") {
    expect.same(List("Unpinned"), kinds[Probe].filterNot(Set("Pinned")))
  }
}
