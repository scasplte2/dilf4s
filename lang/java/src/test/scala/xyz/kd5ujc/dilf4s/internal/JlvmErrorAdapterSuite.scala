package xyz.kd5ujc.dilf4s.internal

import java.util.Optional

import xyz.kd5ujc.crypto.sigma.{Secp256k1Group, SigmaError}
import xyz.kd5ujc.dilf4s.error.{CryptoCause => JCause, JlvmError => JError, SigmaError => JSigma}
import xyz.kd5ujc.jlvm.JlvmError._
import xyz.kd5ujc.jlvm.{CryptoCause, JlvmError, JlvmErrorKind, Op, Site}

import weaver.FunSuite

object JlvmErrorAdapterSuite extends FunSuite {
  test("all native JLVM cases preserve Java records, sites and absent causes") {
    val cases: Vector[(JlvmError, JError)] = Vector(
      JlvmError.Malformed(MalformedReason.NotJson)                  -> new JError.Malformed("not JSON"),
      JlvmError.Type(Op.Add, TypeReason.Entry)                      -> new JError.Type("+", "an entry is [key, value], with a value"),
      JlvmError.Arity(Op.Add, ArgumentCount.Exactly(2), 0)          -> new JError.Arity("+", "takes 2 arguments, got 0"),
      JlvmError.Arithmetic(Op.Div, ArithmeticReason.DivisionByZero) -> new JError.Arithmetic("/", "division by zero"),
      JlvmError.Domain(Site.Var, DomainReason.PathSegmentWidth) -> new JError.Domain("var", "an integer path segment must fit in 64 bits"),
      JlvmError.Crypto(Op.SchnorrVerify, CryptoReason.Unavailable) -> new JError.Crypto(
        "schnorr_verify",
        "not available yet",
        Optional.empty[JCause]()
      ),
      JlvmError.Limit(LimitReason.NegativeGas(-1)) -> new JError.Limit("a gas limit is negative"),
      JlvmError.OutOfGas(2, 1)                     -> new JError.OutOfGas(2, 1)
    )
    expect
      .same(JlvmErrorKind.values.toVector, cases.map(_._1.kind))
      .and(expect.same(cases.map(_._2), cases.map(c => Errors.jlvmError(c._1))))
  }

  test("a retained Sigma point cause renders its exact role at the Java boundary") {
    val refusal = SigmaError.InvalidPoint(SigmaError.PointRole.H, Secp256k1Group.DecodeError.WrongSize(32, 33))
    val javaRefusal = new JSigma.InvalidPoint("h", "a group element is 33 bytes, got 32")
    val error = JlvmError.Crypto(Op.SigmaVerify, CryptoReason.Refused(CryptoCause.Sigma(refusal)))
    expect
      .same(javaRefusal, Errors.sigmaError(refusal))
      .and(
        expect.same(
          new JError.Crypto(
            "sigma_verify",
            "argument: h: a group element is 33 bytes, got 32",
            Optional.of[JCause](new JCause.Sigma(javaRefusal))
          ),
          Errors.jlvmError(error)
        )
      )
  }
}
