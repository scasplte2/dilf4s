package xyz.kd5ujc.core.canonical

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

import xyz.kd5ujc.models.canonical_vectors.{CanonicalJsonRejected, CanonicalVectors}
import xyz.kd5ujc.models.versioned_trie_vectors.{VersionedTrieVectors, VtSchemaOutcome}

import weaver.SimpleIOSuite

/**
 * The coverage gate of docs/specs/conformance.md §6 for the canonical codecs: every kind of [[CanonicalError]] (the
 * generated `CanonicalErrorKind.values`, and the family's cases through the compiler, `Mirror.SumOf`) has a vector row
 * pinning it. The pinned kinds come from the committed vector files, read through their generated models, so a row
 * naming a kind the schema's `canonical-error-kind` does not list does not load.
 */
object CodecRefusalCoverageSuite extends SimpleIOSuite {

  final case class ReadFailure(cause: CanonicalError | DecodeFailure) extends RuntimeException(cause.toString)

  /** The case names of a sealed type, in declaration order. */
  inline def kinds[T](using m: Mirror.SumOf[T]): List[String] =
    constValueTuple[m.MirroredElemLabels].productIterator.map(_.toString).toList

  private def load[A](file: String, codec: Codec[A]): IO[A] =
    IO.blocking(new String(Files.readAllBytes(Paths.get("shared", "vectors", file)), UTF_8)).flatMap { text =>
      CanonicalJson
        .parse(text)
        .flatMap(codec.decoder.decode(_))
        .leftMap(ReadFailure(_))
        .liftTo[IO]
    }

  /** The kinds the rows pin: JSON, render and CBOR refusals, and the typed encoder's refusal of a state key. */
  val pinned: IO[Set[CanonicalErrorKind]] =
    (load("canonical_encoding_v1.json", CanonicalVectors.codec), load("versioned_trie_v1.json", VersionedTrieVectors.codec)).mapN {
      (c, vt) =>
        val json = c.json.collect { case r: CanonicalJsonRejected => r.error }
        val stateKeys = vt.stateKeys.flatMap(_.schema match {
          case VtSchemaOutcome.Accepted     => None
          case VtSchemaOutcome.TypeMismatch => Some(CanonicalErrorKind.TypeMismatch)
        })
        (json ++ c.renderRejections.map(_.error) ++ c.cborRejections.map(_.error) ++ stateKeys).toSet
    }

  test("every CanonicalError kind has a vector row, and every row names one of its kinds") {
    pinned.map { rows =>
      val all = CanonicalErrorKind.values.toList
      expect(all.filterNot(rows).isEmpty, s"no vector row pins ${all.filterNot(rows).mkString(", ")}")
        .and(expect((rows -- all).isEmpty, s"rows name kinds CanonicalError does not have: ${rows -- all}"))
        .and(expect.same(all.map(_.wire), kinds[CanonicalError]))
    }
  }

  pureTest("the schema's kind of each case is its name, as the vectors record it") {
    val samples: List[CanonicalError] = List(
      CanonicalError.UnpairedSurrogate,
      CanonicalError.DuplicateKey(Preview.of("k")),
      CanonicalError.RationalTooWide(1),
      CanonicalError.IntegerTooWide(1),
      CanonicalError.JsonSyntax(0, JsonSyntaxReason.ExpectedValue),
      CanonicalError.InvalidNumber(None, None, None),
      CanonicalError.NotJsonRepresentable(NotJson.Bytes),
      CanonicalError.TypeMismatch(""),
      CanonicalError.UnexpectedEnd(0),
      CanonicalError.TrailingBytes(0),
      CanonicalError.DepthExceeded(1),
      CanonicalError.NonCanonical(0, NonCanonicalReason.KeyOrder),
      CanonicalError.Unsupported(0, UnsupportedItem.NonTextKey),
      CanonicalError.InvalidUtf8(0)
    )
    expect
      .same(kinds[CanonicalError], samples.map(_.productPrefix))
      .and(samples.foldMap(e => expect.same(e.productPrefix, e.kind.wire)))
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
