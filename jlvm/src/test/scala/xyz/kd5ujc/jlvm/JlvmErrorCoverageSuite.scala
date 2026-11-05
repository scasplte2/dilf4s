package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

import cats.effect.IO
import cats.syntax.all._

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

import io.circe.Json
import weaver.SimpleIOSuite

/**
 * The coverage gate of docs/specs/conformance.md §6 for the JLVM's errors: every case of `JlvmError` has a row of
 * `jlvm_v1.json` pinning it by kind, and both kinds of `Site` (an operation, and the `var` read of a computed path)
 * have a row pinning an error's `op` (jlvm.md §3). The cases come from the compiler (`Mirror.SumOf`), so a new kind or
 * site fails this suite until a generator gives it a row; the pinned kinds come from the committed vector file.
 */
object JlvmErrorCoverageSuite extends SimpleIOSuite {

  /** The case names of a sealed type, in declaration order. */
  inline def kinds[T](using m: Mirror.SumOf[T]): List[String] =
    constValueTuple[m.MirroredElemLabels].productIterator.map(_.toString).toList

  def uncovered(kinds: List[String], pinned: Set[String]): List[String] = kinds.filterNot(pinned)

  /** The error of every failing case: its kind and `op`, if it has one. */
  private val errors: IO[List[(String, Option[String])]] =
    IO.blocking(new String(Files.readAllBytes(JlvmVectors.path), UTF_8))
      .flatMap(io.circe.parser.parse(_).liftTo[IO])
      .map { v =>
        v.hcursor.downField("cases").as[List[Json]].getOrElse(Nil).flatMap { c =>
          val o = c.hcursor.downField("outcome")
          o.get[String]("error").toOption.map(_ -> o.get[String]("op").toOption)
        }
      }

  /** The `Site` case a recorded `op` names: `Op` for an operation's own wire name, `Var` for `var`. */
  def site(op: String): Option[String] =
    if (op == Site.Var.wire) Some("Var") else Op.values.find(_.wire == op).as("Op")

  test("every JLVM error kind has a vector row") {
    errors.map { es =>
      val pinned = es.map(_._1).toSet
      val all = kinds[JlvmError]
      expect(uncovered(all, pinned).isEmpty, s"no jlvm_v1.json row pins ${uncovered(all, pinned).mkString(", ")}")
        .and(expect(pinned.subsetOf(all.toSet), s"rows name kinds JlvmError does not have: ${pinned -- all}"))
    }
  }

  test("every site has a row, and every recorded op names one") {
    errors.map { es =>
      val ops = es.flatMap(_._2)
      val unknown = ops.filter(site(_).isEmpty).distinct
      val pinned = ops.flatMap(site).toSet
      expect(unknown.isEmpty, s"ops that name no site: ${unknown.mkString(", ")}")
        .and(expect(uncovered(kinds[Site], pinned).isEmpty, s"no row pins the site ${uncovered(kinds[Site], pinned).mkString(", ")}"))
    }
  }

  test("an op is recorded exactly for the kinds whose errors carry a site") {
    val withSite = Set("Type", "Arity", "Arithmetic", "Domain", "Crypto")
    val sample: List[JlvmError] = List(
      JlvmError.Malformed(""),
      JlvmError.Type(Op.Add, ""),
      JlvmError.Arity(Op.Add, ""),
      JlvmError.Arithmetic(Op.Add, ""),
      JlvmError.Domain(Site.Var, ""),
      JlvmError.Crypto(Op.Add, ""),
      JlvmError.Limit(""),
      JlvmError.OutOfGas(1, 0)
    )
    errors.map { es =>
      val wrong = es.filter { case (k, op) => withSite(k) != op.isDefined }
      expect(wrong.isEmpty, s"rows whose op does not match their kind: ${wrong.take(5)}")
        .and(expect.same(kinds[JlvmError], sample.map(JlvmVectors.kind(_): String)))
        .and(expect.same(sample.map(e => withSite(JlvmVectors.kind(e))), sample.map(JlvmError.site(_).isDefined)))
    }
  }

  sealed trait Probe
  object Probe {
    case object Pinned extends Probe
    case object Unpinned extends Probe
  }

  pureTest("the gate finds a case with no row: the cases come from the compiler, not from a list") {
    expect.same(List("Unpinned"), uncovered(kinds[Probe], Set("Pinned")))
  }
}
