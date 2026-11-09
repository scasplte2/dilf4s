package xyz.kd5ujc.core.canonical

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.hash.{Digest, DomainTag, HashError, Hasher, Sha256}
import xyz.kd5ujc.models.diagnostic_vectors.DiagnosticVectors

import weaver.SimpleIOSuite

/**
 * Bounded diagnostics (docs/specs/conformance.md §8): a refusal holds caller text only as a [[Preview]], so however
 * long the input, what an error holds and renders stays within a fixed bound.
 */
object DiagnosticsSuite extends SimpleIOSuite {

  // a preview renders as at most 32 code points, each escaped to at most 6 characters, quotes, a mark and a length
  private val PreviewBound = 32 * 6 + 64
  private val MessageBound = 2 * PreviewBound + 256
  private val Huge = 1 << 20

  private def txt(s: String): CanonicalValue = CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)

  private def bounded(what: String, rendered: String, bound: Int = MessageBound) =
    expect(rendered.length <= bound, s"$what renders ${rendered.length} characters, more than $bound")

  private def kept(p: Preview) =
    expect(p.head.codePointCount(0, p.head.length) <= Preview.MaxCodePoints, s"a preview keeps ${p.head.length} code units")

  /**
   * shared/vectors/diagnostics_v1.json (conformance.md §8), the same in every SDK: each object repeats one member name,
   * refused as `DuplicateKey` with the preview given.
   */
  private val vectors: Either[String, DiagnosticVectors] =
    CanonicalJson
      .parse(new String(Files.readAllBytes(Paths.get("shared", "vectors", "diagnostics_v1.json")), UTF_8))
      .leftMap(_.toString)
      .flatMap(DiagnosticVectors.codec.decoder.decode(_).leftMap(_.message))

  pureTest("a preview keeps the first 32 code points and the UTF-8 length, the same in every SDK") {
    vectors.fold(
      e => failure(s"diagnostics_v1.json: $e"),
      v =>
        v.previews.toList.foldMap { c =>
          CanonicalJson.parse(c.json) match {
            case Left(CanonicalError.DuplicateKey(p)) =>
              expect
                .same(c.head, p.head)
                .and(expect.same(c.length, BigInt(p.length)))
                .and(expect.same(c.truncated, p.truncated))
            case other => failure(s"${c.json}: expected a duplicate key, got ${other.map(_ => "a value")}")
          }
        }.and(expect(v.previews.size >= 11, "every row of the spec's table"))
    )
  }

  pureTest("a preview renders the head escaped, a mark when truncated, and the length") {
    expect
      .same("\"abc\" (UTF-8 length 3)", Preview.of("abc").toString)
      .and(expect.same(s"\"${"a" * 32}\"… (UTF-8 length 40)", Preview.of("a" * 40).toString))
      .and(expect.same("\"\\\"\\\\\\u0000\" (UTF-8 length 3)", Preview.of("\"\\\u0000").toString))
      .and(expect.same(s"a.${"b" * 32}…: r", DecodeFailure(List(Preview.of("a"), Preview.of("b" * 33)), "r").message))
  }

  pureTest("a repeated JSON member name of 1 MiB is held and rendered as a preview") {
    val key = "k" * Huge
    CanonicalJson.parse(s"""{"$key":1,"$key":2}""") match {
      case Left(e @ CanonicalError.DuplicateKey(p)) =>
        kept(p).and(expect.same(Huge.toLong, p.length)).and(bounded("DuplicateKey", e.toString))
      case other => failure(s"expected a duplicate key, got ${other.map(_ => "a value")}")
    }
  }

  pureTest("a map built with a repeated 1 MiB key is refused with a preview") {
    val key = "k" * Huge
    CanonicalValue.map(List(key -> CInt(1), key -> CInt(2))) match {
      case Left(e @ CanonicalError.DuplicateKey(p)) => kept(p).and(bounded("DuplicateKey", e.toString))
      case other                                    => failure(s"expected a duplicate key, got ${other.map(_ => "a map")}")
    }
  }

  pureTest("an unpaired surrogate is refused without the text") {
    expect.same(Left(CanonicalError.UnpairedSurrogate), CanonicalValue.text("x" * Huge + "\ud800"))
  }

  pureTest("literal, pattern and text-alternative refusals preview a 1 MiB text") {
    val huge = txt("x" * Huge)
    val literal = Codec.literalText("a").decoder.decode(huge)
    val pattern = Codec.matching("[a-z]".r).decoder.decode(txt("X" * Huge))
    val alternative = Codec.selectAlternative[Int](huge, Nil, List("a" -> 1))
    List("literal" -> literal, "pattern" -> pattern, "alternative" -> alternative).map {
      case (what, r) =>
        expect(r.isLeft, s"$what refused").and(bounded(what, r.left.toOption.fold("")(f => f.message + f.toString)))
    }.reduce(_ and _)
  }

  pureTest("unknown keys are counted and the first, in canonical order, previewed; never listed") {
    val record = CanonicalDecoder.record[Unit](Set("a"))(_ => ().asRight)
    val small = CanonicalValue.map(List("a" -> CInt(1), "zz" -> CInt(1), "é" -> CInt(1), "aaa" -> CInt(1), "b" -> CInt(1)))
    val many = CanonicalValue.map((0 until 4096).map(i => s"${"u" * 256}$i" -> CInt(i)))
    expect
      .same(
        Left("<root>: 4 unknown keys, the first \"b\" (UTF-8 length 1)"),
        small.leftMap(_.toString).flatMap(record.decode(_).leftMap(_.message))
      )
      .and(many.toOption.map(record.decode).fold(failure("a map")) {
        case Left(f)  => bounded("unknown keys", f.message).and(expect(f.reason.startsWith("4096 unknown keys"), f.reason))
        case Right(_) => failure("4096 unknown keys accepted")
      })
  }

  pureTest("no alternative fitting a map of many keys is a count, never the keys") {
    val many = CanonicalValue.map((0 until 4096).map(i => s"${"u" * 256}$i" -> CInt(i)))
    val one: CanonicalDecoder[Int] = _ => Right(1)
    val r = many.toOption.map(Codec.selectAlternative[Int](_, List((Set("a"), Set("a"), one))))
    expect.same(Some(Left("<root>: no alternative fits a map of 4096 keys")), r.map(_.leftMap(_.message)))
  }

  pureTest("a 1 MiB caller key on a failure's path is a preview") {
    val r = CanonicalValue.map(List("k" * Huge -> txt("not an int"))).toOption.map(Codec.textMap(Codec.int).decoder.decode)
    r.fold(failure("a map")) {
      case Left(f)  => f.path.foldMap(kept).and(bounded("path", f.message)).and(bounded("failure", f.toString))
      case Right(_) => failure("text accepted as an int")
    }
  }

  pureTest("an invalid domain tag is previewed") {
    DomainTag("X" * Huge) match {
      case Left(e @ HashError.InvalidDomainTag(p)) => kept(p).and(bounded("InvalidDomainTag", e.toString))
      case other                                   => failure(s"expected an invalid tag, got ${other.isRight}")
    }
  }

  pureTest("an unknown hash function's name is previewed") {
    Hasher.named("X" * Huge) match {
      case Left(e @ HashError.UnknownAlgorithm(p)) => kept(p).and(bounded("UnknownAlgorithm", e.toString))
      case other                                   => failure(s"expected an unknown algorithm, got ${other.isRight}")
    }
  }

  pureTest("a 1 MiB digest hex is refused by its width, holding sizes only") {
    val refusals =
      List("a" * Huge, "0x" + "a" * Huge, "a" * 64 + "é" * Huge, "é" * Huge, "a" * 65 + "é" * Huge).map(Digest.fromHex[Sha256](_))
    expect
      .same(
        List(
          Left(HashError.InvalidLength("SHA-256", 32, Huge / 2L)),
          Left(HashError.InvalidLength("SHA-256", 32, Huge / 2L)),
          Left(HashError.InvalidHex(64)),
          Left(HashError.InvalidHex(0))
        ),
        refusals.take(4)
      )
      // past the one digit it reads beyond the width, the text is not read: its length refuses it, whatever follows
      .and(expect.same(List("InvalidLength"), refusals.drop(4).map(_.fold(_.kind: String, _ => "accepted"))))
      .and(refusals.foldMap(r => bounded("digest hex", r.fold(_.toString, _.toString))))
  }
}
