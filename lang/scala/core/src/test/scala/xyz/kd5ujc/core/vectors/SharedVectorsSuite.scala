package xyz.kd5ujc.core.vectors

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalJson, Cbor, DecodeFailure}
import xyz.kd5ujc.models.canonical_vectors.{
  CanonicalLimitEncoding,
  CanonicalLimitKind,
  CanonicalLimitOutcome,
  CanonicalLimitRefusal,
  CanonicalVectors => WireVectors
}

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

object SharedVectorsSuite extends SimpleIOSuite {

  test("committed canonical-encoding vectors match the reference implementation") {
    IO(new String(Files.readAllBytes(CanonicalVectors.path), UTF_8)).map { committed =>
      expect(
        committed == CanonicalVectors.render,
        s"${CanonicalVectors.path} is stale; regenerate with: tools/sbt \"core/Test/runMain xyz.kd5ujc.core.vectors.CanonicalVectorGen\""
      )
    }
  }

  final case class ReadFailure(cause: CanonicalError | DecodeFailure) extends RuntimeException(cause.toString)

  test("all CBOR byte-limit rows pin the exact measured length or refusal and limit") {
    IO.blocking(new String(Files.readAllBytes(CanonicalVectors.path), UTF_8))
      .flatMap(text => CanonicalJson.parse(text).flatMap(WireVectors.codec.decoder.decode(_)).leftMap(ReadFailure(_)).liftTo[IO])
      .flatMap { vectors =>
        val rows = vectors.limits.filter(_.encoding == CanonicalLimitEncoding.Cbor)
        rows.traverse { row =>
          Cbor.decode(ByteVector.fromValidHex(row.value)).leftMap(ReadFailure(_)).liftTo[IO].map { value =>
            val measured = Cbor.encodedLengthWithin(value, row.maxBytes.bigInteger.longValueExact)
            val outcome: CanonicalLimitOutcome = measured.fold(
              e => CanonicalLimitRefusal(CanonicalLimitKind.TooLarge, Some(BigInt(e.limit))),
              _ => CanonicalLimitOutcome.Accepted
            )
            expect
              .same(row.name -> row.outcome, row.name -> outcome)
              .and(expect.same(row.name -> row.output.map(_.length.toLong / 2), row.name -> measured.toOption))
          }
        }.map(_.foldLeft(expect.same(19, vectors.limits.size).and(expect.same(9, rows.size)))(_ and _))
      }
  }

  pureTest("no rejection vector is accidentally accepted") {
    expect(!CanonicalVectors.render.contains("ACCEPTED"), "every cborRejections entry must fail to decode")
      .and(expect(!CanonicalVectors.render.contains("RENDERED"), "every renderRejections entry must fail to render"))
      .and(expect(!CanonicalVectors.render.contains("UNRENDERABLE"), "every accepted JSON case must render"))
  }
}
