package xyz.kd5ujc.crypto

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

import xyz.kd5ujc.crypto.kes.KesError
import xyz.kd5ujc.crypto.keys.{KeyError, PathRefusal}
import xyz.kd5ujc.crypto.sigma.SigmaError
import xyz.kd5ujc.crypto.signing.CryptoError
import xyz.kd5ujc.crypto.vrf.VrfError
import xyz.kd5ujc.crypto.zk.{Groth16Error, ZkError}

import io.circe.{ACursor, Json}
import weaver.SimpleIOSuite

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

  final case class Family(name: String, kinds: List[String], exempt: Map[String, String])

  val families: List[Family] = List(
    Family("signing", kinds[CryptoError], Map("BackendFailure" -> "a JVM provider's own failure")),
    Family("keys", kinds[KeyError], Map.empty),
    Family("paths", PathRefusal.values.toList, Map.empty),
    Family("vrf", kinds[VrfError], Map("HashToCurveFailed" -> "no input is known that reaches it")),
    Family("kes", kinds[KesError], Map.empty),
    Family("sigma", kinds[SigmaError], Map.empty),
    Family("zk", kinds[ZkError], Map.empty),
    Family("groth16", kinds[Groth16Error], Map.empty)
  )

  def uncovered(f: Family, pinned: Set[String]): List[String] = f.kinds.filterNot(k => pinned(k) || f.exempt.contains(k))

  private def read(file: String): IO[Json] =
    IO.blocking(new String(Files.readAllBytes(Paths.get("shared", "vectors", file)), UTF_8)).flatMap(io.circe.parser.parse(_).liftTo[IO])

  private def list(c:     ACursor, field:    String): List[Json] = c.downField(field).as[List[Json]].getOrElse(Nil)
  private def texts(rows: List[Json], field: String): Set[String] = rows.flatMap(_.hcursor.get[String](field).toOption).toSet

  /** Outcomes that are results rather than refusals. */
  private val results = Set("accepted", "valid", "invalid", "rejected", "verified")

  /** The kinds each family's rows pin. */
  val pinned: IO[Map[String, Set[String]]] =
    (read("signing_v1.json"), read("vrf_v1.json"), read("kes_v1.json"), read("sigma_refusals_v1.json"), read("zk_v1.json")).mapN {
      (signing, vrf, kes, sigma, zk) =>
        val s = signing.hcursor
        val verification = list(s, "verification").flatMap(_.hcursor.downField("result").get[String]("kind").toOption)
        val paths = list(s, "paths").flatMap(_.hcursor.get[String]("result").toOption)
        val zkPoints = (list(zk.hcursor, "add") ++ list(zk.hcursor, "mul") ++ list(zk.hcursor, "pairing"))
          .flatMap(_.hcursor.downField("result").get[String]("error").toOption)
        Map(
          "signing" -> (texts(list(s, "keyDecoding"), "result") ++ verification),
          "keys"    -> (texts(list(s, "keyRefusals"), "kind") ++ Option.when(paths.nonEmpty)("InvalidPath")),
          "paths"   -> paths.toSet,
          "vrf"     -> (texts(list(vrf.hcursor, "rejections"), "result") ++ texts(list(vrf.hcursor, "keyDecoding"), "result")),
          "kes"     -> texts(list(kes.hcursor, "refusals"), "outcome"),
          "sigma"   -> texts(list(sigma.hcursor, "cases"), "outcome"),
          "zk"      -> zkPoints.toSet,
          "groth16" -> texts(list(zk.hcursor, "sp1Groth16"), "result")
        ).view.mapValues(_ -- results).toMap
    }

  test("every refusal kind of the signing, key, VRF, KES, sigma and zk families has a vector row, or is exempt with a reason") {
    pinned.map { p =>
      families.foldMap { f =>
        val rows = p.getOrElse(f.name, Set.empty)
        expect(uncovered(f, rows).isEmpty, s"${f.name}: no vector row pins ${uncovered(f, rows).mkString(", ")}")
          .and(expect(f.exempt.keySet.subsetOf(f.kinds.toSet), s"${f.name}: exempts kinds it does not have: ${f.exempt.keySet -- f.kinds}"))
          .and(expect(f.exempt.keySet.intersect(rows).isEmpty, s"${f.name}: exempts pinned kinds: ${f.exempt.keySet.intersect(rows)}"))
          .and(expect(rows.subsetOf(f.kinds.toSet), s"${f.name}: rows name kinds it does not have: ${rows -- f.kinds}"))
      }
    }
  }

  test("every exemption is the spec's: conformance.md §6 names each exempted kind") {
    IO.blocking(new String(Files.readAllBytes(Paths.get("docs", "specs", "conformance.md")), UTF_8)).map { spec =>
      val table =
        spec.linesIterator.dropWhile(!_.startsWith("| Family | Kind | Why it has no row |")).takeWhile(_.startsWith("|")).mkString("\n")
      families.foldMap { f =>
        f.exempt.keySet.toList.foldMap(k => expect(table.contains(s"`$k`"), s"${f.name}: $k is exempt here but not in conformance.md §6"))
      }
    }
  }

  sealed trait Probe
  object Probe {
    case object Pinned extends Probe
    case object Unpinned extends Probe
  }

  pureTest("the gate finds a case with no row: the cases come from the compiler, not from a list") {
    val probe = Family("probe", kinds[Probe], Map.empty)
    expect.same(List("Unpinned"), uncovered(probe, Set("Pinned")))
  }
}
