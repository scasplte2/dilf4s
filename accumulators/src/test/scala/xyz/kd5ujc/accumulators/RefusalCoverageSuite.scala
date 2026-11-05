package xyz.kd5ujc.accumulators

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

import xyz.kd5ujc.accumulators.merkle.MerkleError
import xyz.kd5ujc.accumulators.poseidon.PoseidonMerkleError
import xyz.kd5ujc.accumulators.trie.TrieError
import xyz.kd5ujc.accumulators.versioned.{StateKeyError, VersionedTrieError}

import io.circe.Json
import weaver.SimpleIOSuite

/**
 * The coverage gate of docs/specs/conformance.md §6: every case of the verifiers' and the versioned trie's error types
 * has a vector row pinning it, or an exemption with its reason. The cases come from the compiler (`Mirror.SumOf`), so a
 * new case fails this suite until it is pinned or exempted; the pinned kinds come from the committed vector files.
 */
object RefusalCoverageSuite extends SimpleIOSuite {

  /** The case names of a sealed type, in declaration order. */
  inline def kinds[T](using m: Mirror.SumOf[T]): List[String] =
    constValueTuple[m.MirroredElemLabels].productIterator.map(_.toString).toList

  final case class Family(name: String, kinds: List[String], exempt: Map[String, String])

  val families: List[Family] = List(
    Family(
      "versioned trie",
      kinds[VersionedTrieError],
      List("Empty", "VersionConflict", "CommitRewrite", "NotRetained", "Store").map(_ -> "the store's own refusal").toMap
    ),
    Family("merkle", kinds[MerkleError], Map.empty),
    Family("trie", kinds[TrieError], List("MissingNode", "InvalidNode").map(_ -> "a store read").toMap),
    Family("poseidon", kinds[PoseidonMerkleError], List("Depth", "OccupiedPosition").map(_ -> "building a tree").toMap),
    Family("state keys", kinds[StateKeyError], Map.empty)
  )

  /** A family's kinds with neither a row nor an exemption. */
  def uncovered(f: Family, pinned: Set[String]): List[String] = f.kinds.filterNot(k => pinned(k) || f.exempt.contains(k))

  private def read(file: String): IO[Json] =
    IO.blocking(new String(Files.readAllBytes(Paths.get("shared", "vectors", file)), UTF_8)).flatMap(io.circe.parser.parse(_).liftTo[IO])

  private def family(check: String): String =
    if (check.startsWith("vt-")) "versioned trie"
    else if (check.startsWith("merkle-")) "merkle"
    else if (check.startsWith("trie-")) "trie"
    else if (check.startsWith("poseidon-")) "poseidon"
    else check

  /**
   * The kinds each family's rows pin; a nested kind (`Trie.Encoding`) pins its outer kind, and `Key.InvalidSegment` a
   * state-key kind too. State keys are pinned by the outcomes of `versioned_trie_v1.json`'s keys, bytes and namespaces.
   */
  val pinned: IO[Map[String, Set[String]]] =
    (read("refusals_v1.json"), read("merkle_v1.json"), read("trie_v1.json"), read("versioned_trie_v1.json")).mapN {
      (refusals, merkle, trie, vt) =>
        val kinds = refusals.hcursor.downField("cases").as[List[Json]].getOrElse(Nil).flatMap { c =>
          (c.hcursor.get[String]("check").toOption, c.hcursor.downField("outcome").get[String]("kind").toOption).tupled
        }
        val rows = kinds.map { case (check, kind) => family(check) -> kind.takeWhile(_ != '.') }
        val nestedKeys = kinds.collect { case (_, kind) if kind.startsWith("Key.") => "state keys" -> kind.stripPrefix("Key.") }
        def malformed(name: String, j: Json) =
          j.hcursor.downField("malformed").as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.get[String]("error").toOption).map(name -> _)
        val keyRows = List("stateKeys", "stateKeyBytes", "namespaces").flatMap { section =>
          vt.hcursor.downField(section).as[List[Json]].getOrElse(Nil).flatMap(_.hcursor.downField("outcome").get[String]("kind").toOption)
        }
        (rows ++ nestedKeys ++ keyRows.map("state keys" -> _) ++ malformed("merkle", merkle) ++ malformed("trie", trie))
          .groupMap(_._1)(_._2)
          .view
          .mapValues(_.toSet)
          .toMap
    }

  test("every refusal kind of the verifiers and the versioned trie has a vector row, or is exempt with a reason") {
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
    final case class Pinned(reason: String) extends Probe
    case object Unpinned extends Probe
    case object Exempted extends Probe
  }

  pureTest("the gate finds a case with no row: the cases come from the compiler, not from a list") {
    val probe = Family("probe", kinds[Probe], Map("Exempted" -> "a reason"))
    expect
      .same(List("Pinned", "Unpinned", "Exempted"), probe.kinds)
      .and(expect.same(List("Unpinned"), uncovered(probe, Set("Pinned"))))
      .and(expect.same(Nil, uncovered(probe, Set("Pinned", "Unpinned"))))
  }
}
