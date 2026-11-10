package xyz.kd5ujc.crypto.sigma

import scala.compiletime.testing.typeCheckErrors

import xyz.kd5ujc.core.canonical.{CanonicalValue, ValueKind}
import xyz.kd5ujc.crypto.sigma.SigmaError.{PointRole, PropositionReason}

import scodec.bits.ByteVector
import weaver.FunSuite

object SigmaErrorSuite extends FunSuite {
  test("point decoding preserves its exact cause and role") {
    val inputs = Vector(
      ByteVector.empty                          -> Secp256k1Group.DecodeError.WrongSize(0, 33),
      (ByteVector(4) ++ ByteVector.fill(32)(0)) -> Secp256k1Group.DecodeError.InvalidPrefix(4)
    )
    inputs.foldLeft(success) {
      case (acc, (bytes, cause)) =>
        val result = for {
          name  <- CanonicalValue.text("dlog")
          value <- CanonicalValue.map(Vector("type" -> name, "pk" -> CanonicalValue.CBytes(bytes)))
        } yield SigmaProp.decode(value)
        acc.and(expect.same(Right(Left(SigmaError.InvalidPoint(PointRole.PublicKey, cause))), result))
    }
  }

  test("proposition refusals distinguish identity, wrong field type and threshold bounds") {
    val wrongType = for {
      name  <- CanonicalValue.text("dlog")
      value <- CanonicalValue.map(Vector("type" -> name, "pk" -> CanonicalValue.CInt(7)))
    } yield SigmaProp.decode(value)
    expect
      .same(Left(SigmaError.InvalidProposition(PropositionReason.Identity(PointRole.PublicKey))), SigmaProp.dlog(Secp256k1Group.Identity))
      .and(
        expect.same(
          Right(Left(SigmaError.InvalidProposition(PropositionReason.PointBytes(PointRole.PublicKey, Some(ValueKind.Int))))),
          wrongType
        )
      )
      .and(
        expect.same(
          Left(SigmaError.InvalidProposition(PropositionReason.Threshold(0, 1))),
          SigmaProp.dlog(Secp256k1Group.Generator).flatMap(p => SigmaProp.threshold(0, Vector(p)))
        )
      )
  }

  test("string-taking sigma constructors and the generated escape hatch are unavailable") {
    expect
      .same(1, typeCheckErrors("xyz.kd5ujc.crypto.sigma.SigmaError.InvalidProposition(\"bad\")").size)
      .and(expect.same(2, typeCheckErrors("xyz.kd5ujc.crypto.sigma.SigmaError.InvalidPoint(\"pk\", \"bad\")").size))
      .and(expect.same(1, typeCheckErrors("xyz.kd5ujc.models.crypto_errors.SigmaError").size))
  }
}
