package xyz.kd5ujc.accumulators

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

import xyz.kd5ujc.accumulators.merkle.{MerkleError, MerkleErrorKind}
import xyz.kd5ujc.accumulators.poseidon.{PoseidonMerkleError, PoseidonMerkleErrorKind}
import xyz.kd5ujc.accumulators.trie.{TrieError, TrieErrorKind}
import xyz.kd5ujc.accumulators.versioned.{StateKeyError, StateKeyErrorKind, VersionedTrieError, VersionedTrieErrorKind}
import xyz.kd5ujc.core.canonical.{CanonicalJson, Codec}
import xyz.kd5ujc.models.merkle_vectors.MerkleVectors
import xyz.kd5ujc.models.refusal_vectors.{MerkleRefusal, PoseidonRefusal, RefusalVectors, TrieRefusal, VtKeyCause, VtRefusal, VtTrieCause}
import xyz.kd5ujc.models.trie_vectors.TrieVectors
import xyz.kd5ujc.models.versioned_trie_vectors.{VersionedTrieVectors, VtStateKeyRefusal, VtUnknownVersion, VtVersionNotIncreasing}

import weaver.SimpleIOSuite

/**
 * The coverage gate of docs/specs/conformance.md §6: every kind of the verifiers' and the versioned trie's error types
 * (their generated `values`, which the compiler's `Mirror.SumOf` of each family must equal) has a vector row pinning it,
 * or an exemption with its reason. The pinned kinds come from the committed vector files, read through their generated
 * models, so a row naming a kind its family does not have does not load, and a check whose family is another's fails.
 */
object RefusalCoverageSuite extends SimpleIOSuite {

  /** The case names of a sealed type, in declaration order. */
  inline def kinds[T](using m: Mirror.SumOf[T]): List[String] =
    constValueTuple[m.MirroredElemLabels].productIterator.map(_.toString).toList

  final case class Family(name: String, kinds: List[String], cases: List[String], exempt: Map[String, String])

  val families: List[Family] = List(
    Family(
      "versioned trie",
      VersionedTrieErrorKind.values.toList,
      kinds[VersionedTrieError],
      List("Empty", "VersionConflict", "CommitRewrite", "NotRetained", "Store").map(_ -> "the store's own refusal").toMap
    ),
    Family("merkle", MerkleErrorKind.values.toList, kinds[MerkleError], Map.empty),
    Family("trie", TrieErrorKind.values.toList, kinds[TrieError], List("MissingNode", "InvalidNode").map(_ -> "a store read").toMap),
    Family(
      "poseidon",
      PoseidonMerkleErrorKind.values.toList,
      kinds[PoseidonMerkleError],
      List("Depth", "OccupiedPosition").map(_ -> "building a tree").toMap
    ),
    Family("state keys", StateKeyErrorKind.values.toList, kinds[StateKeyError], Map.empty)
  )

  /** A family's kinds with neither a row nor an exemption. */
  def uncovered(f: Family, pinned: Set[String]): List[String] = f.kinds.filterNot(k => pinned(k) || f.exempt.contains(k))

  private def load[A](file: String, codec: Codec[A]): IO[A] =
    IO.blocking(new String(Files.readAllBytes(Paths.get("shared", "vectors", file)), UTF_8)).flatMap { text =>
      CanonicalJson
        .parse(text)
        .leftMap(_.toString)
        .flatMap(codec.decoder.decode(_).leftMap(_.message))
        .leftMap(e => new IllegalStateException(s"$file: $e"))
        .liftTo[IO]
    }

  /** The family a check refuses as (conformance.md §6). */
  private def family(check: String): String =
    if (check.startsWith("vt-")) "versioned trie"
    else if (check.startsWith("merkle-")) "merkle"
    else if (check.startsWith("trie-")) "trie"
    else if (check.startsWith("poseidon-")) "poseidon"
    else check

  /** Each refusal row's family (by its key) and kind, and the check's family: the two must agree. */
  val refusals: IO[List[(String, String, String)]] =
    load("refusals_v1.json", RefusalVectors.codec).map(_.cases.toList.flatMap { c =>
      val f = family(c.check)
      c.outcome match {
        case _: String          => Nil
        case r: VtRefusal       => List((f, "versioned trie", r.versionedTrie: String))
        case r: MerkleRefusal   => List((f, "merkle", r.merkle: String))
        case r: TrieRefusal     => List((f, "trie", r.trie: String))
        case r: PoseidonRefusal => List((f, "poseidon", r.poseidonMerkle: String))
      }
    })

  /**
   * The kinds each family's rows pin: the refusal rows, a versioned trie's cause (`Key`'s state-key kind, `Trie`'s trie
   * kind), the `malformed` rows of merkle_v1.json and trie_v1.json, and versioned_trie_v1.json's state keys, bytes,
   * namespaces and version errors.
   */
  val pinned: IO[Map[String, Set[String]]] =
    (
      load("refusals_v1.json", RefusalVectors.codec),
      load("merkle_v1.json", MerkleVectors.codec),
      load("trie_v1.json", TrieVectors.codec),
      load("versioned_trie_v1.json", VersionedTrieVectors.codec),
      refusals
    ).mapN { (r, merkle, trie, vt, rows) =>
      val causes = r.cases.toList.flatMap(_.outcome match {
        case VtRefusal(_, Some(VtTrieCause(t)), _, _, _)   => List("trie" -> (t: String))
        case VtRefusal(_, Some(VtKeyCause(k, _)), _, _, _) => List("state keys" -> (k: String))
        case _                                             => Nil
      })
      val keyRows = (vt.stateKeys.map(_.outcome) ++ vt.stateKeyBytes.map(_.outcome) ++ vt.namespaces.map(_.outcome)).toList.collect {
        case k: VtStateKeyRefusal => "state keys" -> (k.kind: String)
      }
      val versionRows = vt.versionErrors.toList.map(_.outcome match {
        case _: VtVersionNotIncreasing => "versioned trie" -> "VersionNotIncreasing"
        case _: VtUnknownVersion       => "versioned trie" -> "UnknownVersion"
      })
      (rows.map((_, f, k) => f -> k) ++ causes ++ keyRows ++ versionRows ++ merkle.malformed.toList.map("merkle" -> _.error) ++
      trie.malformed.toList.map("trie" -> _.error))
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.toSet)
        .toMap
    }

  test("every refusal row is of its check's family") {
    refusals.map(_.foldMap {
      case (byCheck, byKey, kind) =>
        expect(byCheck == byKey, s"a $byCheck check's row is a $byKey refusal ($kind)")
    })
  }

  test("a state-key cause records its normative segment exactly for the kinds that name one (conformance.md §3)") {
    load("refusals_v1.json", RefusalVectors.codec).map(_.cases.toList.foldMap { c =>
      c.outcome match {
        case VtRefusal(_, Some(VtKeyCause(k, segment)), _, _, _) =>
          val names = k == "InvalidSegment" || k == "SegmentTooLong"
          expect(segment.isDefined == names, s"${c.name}: $k ${if (names) "without" else "with"} a segment")
        case _ => success
      }
    })
  }

  test("every family's generated kinds are its cases, in order") {
    IO.pure(families.foldMap(f => expect.same(f.kinds, f.cases)))
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
    val probe = Family("probe", kinds[Probe], kinds[Probe], Map("Exempted" -> "a reason"))
    expect
      .same(List("Pinned", "Unpinned", "Exempted"), probe.kinds)
      .and(expect.same(List("Unpinned"), uncovered(probe, Set("Pinned"))))
      .and(expect.same(Nil, uncovered(probe, Set("Pinned", "Unpinned"))))
  }
}
