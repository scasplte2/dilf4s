package xyz.kd5ujc.core.canonical

import xyz.kd5ujc.models.refusal_vectors.{BuiltCopies, BuiltValue, BuiltWrap}

import weaver.SimpleIOSuite

object BuiltValuesSuite extends SimpleIOSuite {

  pureTest("a malformed leaf retains its bounded hex input") {
    val input = "z" * 100
    expect.same(Left(BuiltValues.ReadError.InvalidHex(Preview.of(input))), BuiltValues.fromModel(BuiltValue(input, None)))
  }

  pureTest("recipe counts refuse negative and overflowing values before constructing a value") {
    val negative = BuiltValue("f6", Some(Vector(BuiltCopies(-1))))
    val overflow = BuiltValue("f6", Some(Vector(BuiltWrap("f6", BigInt(Int.MaxValue) + 1))))
    expect
      .same(Left(BuiltValues.ReadError.CountOutOfRange(0, true)), BuiltValues.fromModel(negative))
      .and(expect.same(Left(BuiltValues.ReadError.CountOutOfRange(32, false)), BuiltValues.fromModel(overflow)))
  }

  pureTest("a non-record recipe retains the schema decoder's exact refusal") {
    expect.same(
      Left(BuiltValues.ReadError.Decode(DecodeFailure(Nil, DecodeReason.ExpectedType(Vector(ValueKind.Map), ValueKind.Null)))),
      BuiltValues.fromValue(CanonicalValue.CNull)
    )
  }

  pureTest("zero copies produce an empty array") {
    expect.same(
      CanonicalValue.array(Vector.empty),
      BuiltValues.fromModel(BuiltValue("f6", Some(Vector(BuiltCopies(0))))).flatMap(BuiltValues.build)
    )
  }
}
