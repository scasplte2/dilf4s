package xyz.kd5ujc.jlvm

import scala.compiletime.testing.typeCheckErrors
import scala.compiletime.{erasedValue, summonFrom}
import scala.deriving.Mirror

import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalValue, Preview, ValueKind}
import xyz.kd5ujc.crypto.sigma.{ChildPoint, Secp256k1Group, SigmaError}
import xyz.kd5ujc.jlvm.JlvmError._

import weaver.FunSuite

object TypedRefusalSuite extends FunSuite {
  // Native sums are checked individually below, avoiding cycles through computed-path errors and nested roles.
  // Domain causes retain their owners' types; migrating those families is separate from this JLVM boundary.
  private inline def typedField[T]: Boolean = inline erasedValue[T] match {
    case _: String                                                                                                               => false
    case _: Int | _: Long | _: Boolean | _: BigInt | _: Preview                                                                  => true
    case _: ValueKind | _: Op | _: Site | _: OperationForm | _: RemovedOp                                                        => true
    case _: SigmaError | _: SigmaError.PropositionReason | _: SigmaError.PointRole | _: SigmaError.PropositionKind               => true
    case _: Secp256k1Group.DecodeError | _: ChildPoint.Error                                                                     => true
    case _: JlvmError | _: MalformedReason | _: TypeReason | _: ArgumentCount | _: ArithmeticReason                              => true
    case _: DomainReason | _: CryptoReason | _: LimitReason | _: Role | _: Construct | _: CryptoCause                            => true
    case _: CanonicalError | _: xyz.kd5ujc.core.canonical.CanonicalJson.RenderError | _: xyz.kd5ujc.core.canonical.DecodeFailure => true
    case _: xyz.kd5ujc.crypto.signing.CryptoError                                                                                => true
    case _: xyz.kd5ujc.crypto.zk.ZkError | _: xyz.kd5ujc.crypto.zk.Groth16Error | _: xyz.kd5ujc.crypto.vrf.VrfError              => true
    case _: xyz.kd5ujc.core.hash.HashError | _: xyz.kd5ujc.core.hash.PoseidonError                                               => true
    case _: xyz.kd5ujc.accumulators.merkle.MerkleError | _: xyz.kd5ujc.accumulators.trie.TrieError                               => true
    case _: xyz.kd5ujc.accumulators.poseidon.PoseidonMerkleError                                                                 => true
    case _: Option[t] => typedField[t]
    case _: Vector[t] => typedField[t]
    case _: List[t]   => typedField[t]
    case _: Map[k, v] => typedField[k] && typedField[v]
    case _ =>
      summonFrom {
        case m: Mirror.ProductOf[T] => typedFields[m.MirroredElemTypes]
        case m: Mirror.SumOf[T]     => typedCases[m.MirroredElemTypes]
        case _                      => false
      }
  }

  private inline def typedFields[T <: Tuple]: Boolean = inline erasedValue[T] match {
    case _: EmptyTuple => true
    case _: (h *: t)   => typedField[h] && typedFields[t]
  }

  private inline def typedProduct[T]: Boolean = summonFrom {
    case m: Mirror.ProductOf[T] => typedFields[m.MirroredElemTypes]
  }

  private inline def typedCases[T <: Tuple]: Boolean = inline erasedValue[T] match {
    case _: EmptyTuple              => true
    case _: (a *: b *: c *: d *: t) => typedProduct[a] && typedProduct[b] && typedProduct[c] && typedProduct[d] && typedCases[t]
    case _: (h *: t)                => typedProduct[h] && typedCases[t]
  }

  private inline def typedPayloads[T](using m: Mirror.SumOf[T]): Boolean = typedCases[m.MirroredElemTypes]

  test("payload checks reject strings, including strings inside containers") {
    expect
      .same(false, typedFields[String *: EmptyTuple])
      .and(expect.same(false, typedFields[Option[String] *: EmptyTuple]))
      .and(expect.same(false, typedFields[List[String] *: EmptyTuple]))
      .and(expect.same(false, typedFields[Vector[String] *: EmptyTuple]))
      .and(expect.same(false, typedFields[Map[Int, String] *: EmptyTuple]))
      .and(expect.same(false, typedFields[(Int, String) *: EmptyTuple]))
      .and(expect.same(false, typedFields[Any *: EmptyTuple]))
      .and(expect.same(true, typedFields[Long *: Preview *: EmptyTuple]))
  }

  test("native JLVM refusal cases cannot gain raw string payloads") {
    expect(typedPayloads[JlvmError], "JlvmError fields must be typed")
      .and(expect(typedPayloads[MalformedReason], "malformed reasons must be typed"))
      .and(expect(typedPayloads[TypeReason], "type reasons must be typed"))
      .and(expect(typedPayloads[ArgumentCount], "argument counts must be typed"))
      .and(expect(typedPayloads[ArithmeticReason], "arithmetic reasons must be typed"))
      .and(expect(typedPayloads[DomainReason], "domain reasons must be typed"))
      .and(expect(typedPayloads[CryptoReason], "crypto reasons must be typed"))
      .and(expect(typedPayloads[LimitReason], "limit reasons must be typed"))
      .and(expect(typedPayloads[Role], "argument roles must be typed"))
      .and(expect(typedPayloads[Construct], "construct contexts must be typed"))
      .and(expect(typedPayloads[OperationForm], "operation forms must be typed"))
      .and(expect(typedPayloads[CallName], "surface call names must be typed"))
      .and(expect(typedPayloads[SigmaError], "sigma errors must retain typed causes"))
      .and(expect(typedPayloads[SigmaError.PropositionReason], "sigma proposition reasons must be typed"))
      .and(expect(typedPayloads[Secp256k1Group.DecodeError], "point decoding reasons must be typed"))
      .and(expect(typedPayloads[ChildPoint.Error], "child index errors must be typed"))
      .and(expect(typedPayloads[CryptoCause], "crypto causes must retain their domain types"))
  }

  test("the compiler rejects every former string-taking error constructor") {
    val rejected = List(
      ("Malformed", 1)  -> typeCheckErrors("xyz.kd5ujc.jlvm.JlvmError.Malformed(\"bad\")"),
      ("Type", 1)       -> typeCheckErrors("xyz.kd5ujc.jlvm.JlvmError.Type(xyz.kd5ujc.jlvm.Op.Add, \"bad\")"),
      ("Arity", 2)      -> typeCheckErrors("xyz.kd5ujc.jlvm.JlvmError.Arity(xyz.kd5ujc.jlvm.Op.Add, \"bad\")"),
      ("Arithmetic", 1) -> typeCheckErrors("xyz.kd5ujc.jlvm.JlvmError.Arithmetic(xyz.kd5ujc.jlvm.Op.Add, \"bad\")"),
      ("Domain", 1)     -> typeCheckErrors("xyz.kd5ujc.jlvm.JlvmError.Domain(xyz.kd5ujc.jlvm.Op.Add, \"bad\")"),
      ("Crypto", 2)     -> typeCheckErrors("xyz.kd5ujc.jlvm.JlvmError.Crypto(xyz.kd5ujc.jlvm.Op.Add, \"bad\", None)"),
      ("Limit", 1)      -> typeCheckErrors("xyz.kd5ujc.jlvm.JlvmError.Limit(\"bad\")")
    )
    rejected.foldLeft(expect.same(7, rejected.size)) {
      case (acc, ((name, expected), errors)) =>
        acc.and(
          expect(
            errors.size == expected,
            s"$name must reject its former string constructor with exactly $expected compiler errors; got $errors"
          )
        )
    }
  }

  test("legacy generated error families cannot bypass native refusal types") {
    val errors = List(
      typeCheckErrors("xyz.kd5ujc.models.jlvm_errors.JlvmError.Malformed(\"bad\")"),
      typeCheckErrors("xyz.kd5ujc.models.jlvm_errors.JlvmCryptoCause")
    )
    errors.foldLeft(expect.same(2, errors.size))((acc, e) => acc.and(expect.same(1, e.size)))
  }

  test("type refusals preserve expected and actual kinds, and arity preserves exact counts") {
    val typed = PureOps(Op.Add, Vector(CanonicalValue.CNull), Kernels.Default)
    val arity = PureOps(Op.Sub, Vector.empty, Kernels.Default)
    expect
      .same(Some(Left(JlvmError.Type(Op.Add, TypeReason.Expected(Vector(ValueKind.Int, ValueKind.Rational), ValueKind.Null)))), typed)
      .and(expect.same(Some(Left(JlvmError.Arity(Op.Sub, ArgumentCount.Between(1, 2), 0))), arity))
  }

  test("path parsing returns an exact structured refusal") {
    expect
      .same(Left(JlvmError.Malformed(MalformedReason.EmptyPathSegment(Preview.of("a..b")))), VarRef.parse("a..b"))
      .and(expect.same(Left(JlvmError.Malformed(MalformedReason.EmptyBindingName(Preview.of("$")))), VarRef.parse("$")))
  }

  test("canonical input failures retain their cause") {
    val bad = JsonSurface.parse("{\"a\":1,\"a\":2}")
    expect.same(Left(JlvmError.Malformed(MalformedReason.Canonical(CanonicalError.DuplicateKey(Preview.of("a"))))), bad)
  }

  test("crypto cause presence is derived from its refusal, not supplied independently") {
    val cause = CryptoCause.Poseidon(xyz.kd5ujc.core.hash.PoseidonError.NonCanonical(255))
    val e = JlvmError.Crypto(Op.Poseidon, CryptoReason.Refused(cause, Role.Input(2)))
    expect
      .same(Some(cause), e.cause)
      .and(expect.same(None, JlvmError.Crypto(Op.Poseidon, CryptoReason.ByteWidth(Role.Input(2), 32, 31)).cause))
      .and(expect.same(Some(Op.Poseidon: Site), e.site))
      .and(expect.same(JlvmErrorKind.Crypto, e.kind))
  }
  test("parser call refusals retain the classified name and exact input") {
    val cases = Vector(
      "[\"+\",1]"    -> MalformedReason.ArrayCall(CallName.Operation(Op.Add, Preview.of("+"))),
      "[\"var\",1]"  -> MalformedReason.ArrayCall(CallName.Variable),
      "[\"noop\",1]" -> MalformedReason.ArrayCall(CallName.Removed(RemovedOp.Noop)),
      "{\"noop\":1}" -> MalformedReason.RemovedOperation(RemovedOp.Noop),
      "{\"typo\":1}" -> MalformedReason.UnknownOperation(Preview.of("typo"))
    )
    cases.foldLeft(success) {
      case (acc, (json, reason)) =>
        acc.and(expect.same(Left(JlvmError.Malformed(reason)), Parser.parse(json)))
    }
  }

  test("compilation preserves exact malformed-form refusals") {
    val cases = Vector(
      "{\"map\":[]}"                              -> MalformedReason.FormArguments(Op.Map, ArgumentCount.Exactly(2), 0),
      "{\"+\":1}"                                 -> MalformedReason.SpreadKind(Op.Add, ValueKind.Int),
      "{\"let\":[[[\"x\",1]],{\"var\":\"x.a\"}]}" -> MalformedReason.BindingShadow(Preview.of("x.a"), Preview.of("x"))
    )
    cases
      .foldLeft(success) {
        case (acc, (json, reason)) =>
          acc.and(expect.same(Left(JlvmError.Malformed(reason)), Parser.parse(json).flatMap(Node.compile(_, Kernels.Default))))
      }
      .and(
        expect.same(
          Left(JlvmError.Malformed(MalformedReason.UnspellablePath(Preview.of("a.b")))),
          Program.from(Expr.Var(VarRef.Data(Vector("a.b")), None))
        )
      )
  }

  test("gas and program limits retain exact bounds and rejected values") {
    val deep = (0 to Program.MaxDepth).foldLeft[Expr](Expr.Lit(CanonicalValue.CNull))((e, _) => Expr.Arr(Vector(e)))
    expect
      .same(Some(JlvmError.Limit(LimitReason.NegativeGas(-1))), GasLimit.refusal(GasLimit.of(-1)))
      .and(
        expect.same(
          Some(JlvmError.Limit(LimitReason.GasRange(GasLimit.Max.requested + 1, GasLimit.Max.requested))),
          GasLimit.refusal(GasLimit.of(GasLimit.Max.requested + 1))
        )
      )
      .and(expect.same(Left(JlvmError.Limit(LimitReason.ProgramDepth(Program.MaxDepth))), Program.from(deep)))
  }

}
