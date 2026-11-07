package xyz.kd5ujc.jlvm

import cats.effect.IO

import weaver.SimpleIOSuite

/**
 * The generated operations (`Op.values`, from `shared/schemas/jlvm.cddl`) are exactly the ones the evaluator dispatches:
 * each is a form the compiler builds itself, a strict operation ([[PureOps]]) or a cryptographic one ([[CryptoOps]]),
 * never two, so no operation lacks a handler and no handler names anything but an operation.
 */
object OpDispatchSuite extends SimpleIOSuite {

  /** The forms `Node.compile` builds itself: the lazy and binding forms, the callbacks and the lookups. */
  private val Forms: Set[Op] =
    Set(
      Op.If,
      Op.And,
      Op.Or,
      Op.Default,
      Op.Let,
      Op.Map,
      Op.Filter,
      Op.Reduce,
      Op.All,
      Op.Some,
      Op.None,
      Op.Find,
      Op.Missing,
      Op.MissingSome
    )

  test("every operation has exactly one handler") {
    CryptoOps.load[IO].map { crypto =>
      val handlers = Op.values.toList.map { op =>
        op -> List(Forms(op), PureOps(op, Vector.empty, Kernels.Default).isDefined, crypto(op, Vector.empty).isDefined).count(identity)
      }
      val wrong = handlers.collect { case (op, n) if n != 1 => s"${op.wire}: $n handlers" }
      expect(wrong.isEmpty, wrong.mkString(", "))
        .and(expect(Forms.subsetOf(Op.values.toSet), "every form is an operation"))
        .and(expect.same(Op.values.toList, Op.values.toList.flatMap(op => Op.fromWire(op.wire))))
    }
  }
}
