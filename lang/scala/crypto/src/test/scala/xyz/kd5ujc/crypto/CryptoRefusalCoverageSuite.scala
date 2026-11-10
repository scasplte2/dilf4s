package xyz.kd5ujc.crypto

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalJson, Codec, DecodeFailure}
import xyz.kd5ujc.crypto.kes.KesError
import xyz.kd5ujc.crypto.keys.{KeyError, PathRefusal}
import xyz.kd5ujc.crypto.sigma.SigmaError
import xyz.kd5ujc.crypto.signing.CryptoError
import xyz.kd5ujc.crypto.vrf.VrfError
import xyz.kd5ujc.crypto.zk.{Groth16Error, ZkError}
import xyz.kd5ujc.models.crypto_errors.{
  CryptoErrorKind,
  Groth16ErrorKind,
  KesErrorKind,
  KeyErrorKind,
  SigmaErrorKind,
  VrfErrorKind,
  ZkErrorKind
}
import xyz.kd5ujc.models.{kes_vectors => kv, sigma_refusal_vectors => sv, signing_vectors => signing, vrf_vectors => vv, zk_vectors => zv}

import weaver.{Expectations, SimpleIOSuite}

/**
 * The coverage gate of docs/specs/conformance.md §6 for the signing, key, VRF, KES, sigma and zk families: every case
 * of their error types has a vector row pinning it by kind, or an exemption with its reason. The error types are
 * generated from `shared/schemas/crypto-errors.cddl`; their cases come from the compiler (`Mirror.SumOf`), and the
 * path refusals (a choice of texts) from the generated `values`, so a new kind fails this suite until it is pinned or
 * exempted; the pinned kinds come from the committed vector files.
 */
object CryptoRefusalCoverageSuite extends SimpleIOSuite {

  /** The case names of a sealed type, in declaration order. */
  inline def kinds[T](using m: Mirror.SumOf[T]): List[String] =
    constValueTuple[m.MirroredElemLabels].productIterator.map(_.toString).toList

  final case class ReadFailure(cause: CanonicalError | DecodeFailure) extends RuntimeException(cause.toString)

  private def read[A](file: String, codec: Codec[A]): IO[A] =
    IO.blocking(new String(Files.readAllBytes(Paths.get("shared", "vectors", file)), UTF_8)).flatMap { text =>
      CanonicalJson.parse(text).flatMap(codec.decoder.decode(_)).leftMap(ReadFailure(_)).liftTo[IO]
    }

  private val signingRows = read("signing_v1.json", signing.SigningVectors.codec)
  private val vrfRows = read("vrf_v1.json", vv.VrfVectors.codec)
  private val kesRows = read("kes_v1.json", kv.KesVectors.codec)
  private val sigmaRows = read("sigma_refusals_v1.json", sv.SigmaRefusalVectors.codec)
  private val zkRows = read("zk_v1.json", zv.ZkVectors.codec)

  def uncovered[K](all: List[K], pinned: Set[K], exempt: Set[K]): List[K] = all.filterNot(k => pinned(k) || exempt(k))

  trait Gate {
    def check: IO[Expectations]
    def exemptions: List[String]
  }

  final case class Family[K](
    name:      String,
    all:       List[K],
    labels:    K => String,
    caseNames: List[String],
    exempt:    Map[K, String],
    rows:      IO[Set[K]]
  ) extends Gate {
    def exemptions: List[String] = exempt.keys.toList.map(labels)
    def check: IO[Expectations] = rows.map { pinned =>
      expect
        .same(Set.empty[K], uncovered(all, pinned, exempt.keySet).toSet)
        .and(expect.same(Set.empty[K], pinned -- all))
        .and(expect.same(Set.empty[K], exempt.keySet -- all))
        .and(expect.same(Set.empty[K], exempt.keySet.intersect(pinned)))
        .and(expect.same(caseNames, all.map(labels)))
    }
  }

  private val pathRows = signingRows.map(
    _.paths
      .flatMap(_.result match {
        case r: PathRefusal         => Some(r)
        case _: signing.PathIndices => None
      })
      .toSet
  )

  val families: List[Gate] = List(
    Family[CryptoErrorKind](
      "signing",
      CryptoErrorKind.values.toList,
      _.wire,
      kinds[CryptoError],
      Map(CryptoErrorKind.BackendFailure -> "a JVM provider's own failure"),
      signingRows.map { s =>
        (s.keyDecoding.flatMap(_.result match {
          case k: CryptoErrorKind                 => Some(k)
          case signing.KeyDecodingResult.Accepted => None
        }) ++ s.verification.flatMap(_.result match {
          case signing.VerificationRefusal(_, k)                                     => Some(k)
          case signing.VerificationResult.Valid | signing.VerificationResult.Invalid => None
        })).toSet
      }
    ),
    Family[KeyErrorKind](
      "keys",
      KeyErrorKind.values.toList,
      _.wire,
      kinds[KeyError],
      Map.empty,
      (signingRows, pathRows).mapN((s, paths) => s.keyRefusals.map(_.kind).toSet ++ Option.when(paths.nonEmpty)(KeyErrorKind.InvalidPath))
    ),
    Family[PathRefusal]("paths", PathRefusal.values.toList, _.wire, PathRefusal.values.toList.map(_.wire), Map.empty, pathRows),
    Family[VrfErrorKind](
      "vrf",
      VrfErrorKind.values.toList,
      _.wire,
      kinds[VrfError],
      Map(VrfErrorKind.HashToCurveFailed -> "no input is known that reaches it"),
      vrfRows.map { v =>
        (v.rejections.flatMap(_.result match {
          case k: VrfErrorKind    => Some(k)
          case vv.VrfResult.Valid => None
        }) ++ v.keyDecoding.flatMap(_.result match {
          case k: VrfErrorKind          => Some(k)
          case vv.VrfKeyResult.Accepted => None
        })).toSet
      }
    ),
    Family[KesErrorKind](
      "kes",
      KesErrorKind.values.toList,
      _.wire,
      kinds[KesError],
      Map.empty,
      kesRows.map(
        _.refusals
          .flatMap(_.outcome match {
            case k: KesErrorKind                                 => Some(k)
            case kv.KesOutcome.Accepted | kv.KesOutcome.Rejected => None
          })
          .toSet
      )
    ),
    Family[SigmaErrorKind](
      "sigma",
      SigmaErrorKind.values.toList,
      _.wire,
      kinds[SigmaError],
      Map.empty,
      sigmaRows.map(
        _.cases
          .flatMap(_.outcome match {
            case k: SigmaErrorKind                                   => Some(k)
            case sv.SigmaOutcome.Accepted | sv.SigmaOutcome.Rejected => None
          })
          .toSet
      )
    ),
    Family[ZkErrorKind](
      "zk",
      ZkErrorKind.values.toList,
      _.wire,
      kinds[ZkError],
      Map.empty,
      zkRows.map { z =>
        (z.add.map(_.result) ++ z.mul.map(_.result) ++ z.pairing.map(_.result)).flatMap {
          case zv.ZkFailure(k)              => Some(k)
          case _: zv.ZkPoint | _: zv.ZkBool => None
        }.toSet
      }
    ),
    Family[Groth16ErrorKind](
      "groth16",
      Groth16ErrorKind.values.toList,
      _.wire,
      kinds[Groth16Error],
      Map.empty,
      zkRows.map(
        _.sp1Groth16
          .flatMap(_.result match {
            case k: Groth16ErrorKind         => Some(k)
            case zv.ZkGroth16Result.Verified => None
          })
          .toSet
      )
    )
  )

  test("every refusal kind has a vector row, with exactly the declared exemptions") {
    families.traverse(_.check).map(_.combineAll)
  }

  test("every exemption is the spec's: conformance.md §6 names each exempted kind") {
    IO.blocking(new String(Files.readAllBytes(Paths.get("docs", "specs", "conformance.md")), UTF_8)).map { spec =>
      val table =
        spec.linesIterator.dropWhile(!_.startsWith("| Family | Kind | Why it has no row |")).takeWhile(_.startsWith("|")).mkString("\n")
      families.foldMap { f =>
        f.exemptions.foldMap(k => expect(table.contains(s"`$k`"), s"$k is exempt here but not in conformance.md §6"))
      }
    }
  }

  sealed trait Probe
  object Probe {
    case object Pinned extends Probe
    case object Unpinned extends Probe
  }

  pureTest("the gate finds a case with no row: the cases come from the compiler, not from a list") {
    expect.same(List(Probe.Unpinned), uncovered[Probe](List(Probe.Pinned, Probe.Unpinned), Set(Probe.Pinned), Set.empty))
  }
}
