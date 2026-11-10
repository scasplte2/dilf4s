package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

import cats.effect.IO
import cats.syntax.all._

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalJson, DecodeFailure}
import xyz.kd5ujc.models.{jlvm_vectors => model}

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

  def uncovered[A](kinds: List[A], pinned: Set[A]): List[A] = kinds.filterNot(pinned)

  final case class ReadFailure(cause: CanonicalError | DecodeFailure) extends RuntimeException(cause.toString)

  private val errors: IO[List[model.JlvmError]] =
    IO.blocking(new String(Files.readAllBytes(JlvmVectors.path), UTF_8))
      .flatMap { text =>
        CanonicalJson.parse(text).flatMap(model.JlvmVectors.codec.decoder.decode(_)).leftMap(ReadFailure(_)).liftTo[IO]
      }
      .map(_.cases.toList.flatMap(_.outcome match {
        case e: model.JlvmError => Some(e)
        case _: model.JlvmValue => None
      }))

  private def family(cause: model.JlvmCause): CryptoCauseKind = cause match {
    case _: model.JlvmSigningCause        => CryptoCauseKind.Signing
    case _: model.JlvmSigmaCause          => CryptoCauseKind.Sigma
    case _: model.JlvmZkCause             => CryptoCauseKind.Zk
    case _: model.JlvmGroth16Cause        => CryptoCauseKind.Groth16
    case _: model.JlvmVrfCause            => CryptoCauseKind.Vrf
    case _: model.JlvmHashCause           => CryptoCauseKind.Hash
    case _: model.JlvmMerkleCause         => CryptoCauseKind.Merkle
    case _: model.JlvmTrieCause           => CryptoCauseKind.Trie
    case _: model.JlvmPoseidonCause       => CryptoCauseKind.Poseidon
    case _: model.JlvmPoseidonMerkleCause => CryptoCauseKind.PoseidonMerkle
  }

  test("exactly 90 Crypto refusals record the expected cause families") {
    errors.map { es =>
      val caused = es.filter(_.cause.isDefined)
      val expected = Set(
        CryptoCauseKind.Signing,
        CryptoCauseKind.Sigma,
        CryptoCauseKind.Zk,
        CryptoCauseKind.Groth16,
        CryptoCauseKind.Merkle,
        CryptoCauseKind.Trie,
        CryptoCauseKind.Poseidon,
        CryptoCauseKind.PoseidonMerkle
      )
      expect
        .same(Set(JlvmErrorKind.Crypto), caused.map(_.error).toSet)
        .and(expect.same(90, caused.size))
        .and(expect.same(expected, caused.flatMap(_.cause).map(family).toSet))
    }
  }

  enum SiteKind { case Operation, Variable }

  def category(s: Site): SiteKind = s match {
    case _: Op                                      => SiteKind.Operation
    case xyz.kd5ujc.models.jlvm_errors.JlvmSite.Var => SiteKind.Variable
  }

  test("every JLVM error kind has a vector row") {
    errors.map { es =>
      val all = JlvmErrorKind.values.toList
      expect
        .same(all.toSet, es.map(_.error).toSet)
        .and(expect.same(kinds[JlvmError], all.map(_.wire)))
    }
  }

  test("both site categories have rows, and every recorded op decodes as a site") {
    errors.map(es => expect.same(SiteKind.values.toSet, es.flatMap(_.op).map(category).toSet))
  }

  test("an op is recorded exactly for the kinds whose errors carry a site") {
    val withSite = Set(JlvmErrorKind.Type, JlvmErrorKind.Arity, JlvmErrorKind.Arithmetic, JlvmErrorKind.Domain, JlvmErrorKind.Crypto)
    val sample: List[JlvmError] = List(
      JlvmError.Malformed(""),
      JlvmError.Type(Op.Add, ""),
      JlvmError.Arity(Op.Add, ""),
      JlvmError.Arithmetic(Op.Add, ""),
      JlvmError.Domain(Site.Var, ""),
      JlvmError.Crypto(Op.Add, "", None),
      JlvmError.Limit(""),
      JlvmError.OutOfGas(1, 0)
    )
    errors.map { es =>
      val wrong = es.filter(e => withSite(e.error) != e.op.isDefined)
      expect(wrong.isEmpty, s"rows whose op does not match their kind: ${wrong.take(5)}")
        .and(expect.same(kinds[JlvmError], sample.map(_.kind.wire)))
        .and(expect.same(sample.map(e => withSite(e.kind)), sample.map(_.site.isDefined)))
    }
  }

  sealed trait Probe
  object Probe {
    case object Pinned extends Probe
    case object Unpinned extends Probe
  }

  pureTest("the gate finds a case with no row: the cases come from the compiler, not from a list") {
    expect.same(List(Probe.Unpinned), uncovered[Probe](List(Probe.Pinned, Probe.Unpinned), Set(Probe.Pinned)))
  }
}
