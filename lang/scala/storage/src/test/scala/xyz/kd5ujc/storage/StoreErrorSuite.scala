package xyz.kd5ujc.storage

import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalValue, Codec, DecodeFailure, DecodeReason, Preview, ValueKind}

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

object StoreErrorSuite extends SimpleIOSuite {

  pureTest("fixed-width and escaped keys return distinct exact encoding failures") {
    expect
      .same(Left(StoreError.Encoding(StoreContext.Key, EncodingReason.FixedWidth(8, 7))), KeyCodec.longKey.decode(ByteVector.fill(7)(0)))
      .and(
        expect.same(
          Left(StoreError.Encoding(StoreContext.Key, EncodingReason.UnterminatedComponent)),
          KeyCodec.Terminated.decode(ByteVector(1))
        )
      )
      .and(
        expect.same(Left(StoreError.Encoding(StoreContext.Key, EncodingReason.InvalidEscape)), KeyCodec.Terminated.decode(ByteVector(0)))
      )
      .and(
        expect.same(Left(StoreError.Encoding(StoreContext.Key, EncodingReason.InvalidEscape)), KeyCodec.Terminated.decode(ByteVector(0, 1)))
      )
  }

  pureTest("invalid UTF-8 retains the key context and refusal without replacement decoding") {
    val refused = Left(StoreError.Encoding(StoreContext.Key, EncodingReason.InvalidUtf8))
    expect
      .same(refused, KeyCodec.textKey.decode(ByteVector(0xc0, 0x80)))
      .and(expect.same(refused, KeyCodec.textKey.decode(ByteVector(0xed, 0xa0, 0x80))))
  }

  pureTest("namespace failures distinguish the character cap from the encoded byte bounds") {
    expect
      .same(Left(StoreError.Encoding(StoreContext.Namespace, EncodingReason.NamespaceCharacters(256, 255))), Namespace("a" * 256))
      .and(
        expect.same(Left(StoreError.Encoding(StoreContext.Namespace, EncodingReason.NamespaceBytes(Preview.of(""), 1, 255))), Namespace(""))
      )
      .and(
        expect.same(
          Left(StoreError.Encoding(StoreContext.Namespace, EncodingReason.NamespaceBytes(Preview.of("é" * 128), 1, 255))),
          Namespace("é" * 128)
        )
      )
  }

  pureTest("canonical and schema value decoding preserve different structured causes") {
    val integer = ValueCodec.fromCodec(Codec.intIn(0, 12))
    expect
      .same(
        Left(StoreError.Encoding(StoreContext.Value, EncodingReason.Canonical(CanonicalError.UnexpectedEnd(0)))),
        ValueCodec.canonicalValue.decode(ByteVector.empty)
      )
      .and(
        expect.same(
          Left(
            StoreError.Encoding(
              StoreContext.Value,
              EncodingReason.Decode(DecodeFailure(Nil, DecodeReason.ExpectedType(Vector(ValueKind.Int), ValueKind.Bool)))
            )
          ),
          integer.decode(ByteVector(0xf5))
        )
      )
      .and(
        expect.same(
          Left(
            StoreError.Encoding(
              StoreContext.Value,
              EncodingReason.Decode(DecodeFailure(Nil, DecodeReason.IntegerRange(4, Some(BigInt(0)), Some(BigInt(12)))))
            )
          ),
          integer.decode(ByteVector(13))
        )
      )
      .and(expect.same(Right(CanonicalValue.CInt(1)), ValueCodec.canonicalValue.decode(ByteVector(1))))
  }
}
