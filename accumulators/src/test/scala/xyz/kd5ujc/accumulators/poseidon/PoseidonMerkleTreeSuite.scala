package xyz.kd5ujc.accumulators.poseidon

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import xyz.kd5ujc.core.hash.{Fr, Poseidon}

import io.circe.Json
import io.circe.syntax._
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object PoseidonMerkleTreeSuite extends SimpleIOSuite with Checkers {

  // ------------------------------------------------------------------ Merkle tree

  private val depth = 8
  private val tree0 = PoseidonMerkleTree.empty(depth).toOption.get
  private def fr(i: BigInt): Fr = Fr.from(i).toOption.get
  private val entries: Gen[List[(BigInt, Fr)]] =
    Gen.listOfN(6, Gen.zip(Gen.choose(0, 255).map(BigInt(_)), Gen.choose(1L, Long.MaxValue).map(l => fr(BigInt(l)))))
  implicit private val showEntries: cats.Show[List[(BigInt, Fr)]] = cats.Show.fromToString

  pureTest("refused positions and field values are shown by their size, never their digits") {
    val huge = BigInt(1) << 1000000
    expect
      .same(Left("Position(a 1000001-bit position, capacity 2^0)"), PoseidonMerkleProof.from(huge, Vector.empty).left.map(_.toString))
      .and(expect.same(Left("NonCanonical(a 1000001-bit value)"), Fr.from(huge).left.map(_.toString)))
  }

  pureTest("zero hashes chain, and the empty root is zero(depth)") {
    val z = PoseidonMerkleTree.zeroHashes(depth)
    expect
      .same(Some(Fr.Zero), z.headOption)
      .and(expect(z.sliding(2).forall { case Vector(a, b) => Poseidon.compress(a, a) == b; case _ => false }))
      .and(expect.same(z.last, tree0.root))
      .and(expect(PoseidonMerkleTree.empty(0).isLeft))
  }

  test("inclusion proofs verify, the root is order-independent, and tampering is rejected") {
    forall(entries) { es =>
      val t = tree0.insertAll(es).toOption.get
      val last = es.toMap
      val other = tree0.insertAll(es.reverse.distinctBy(_._1).reverse.reverse).toOption.get
      expect
        .same(t.root, other.root)
        .and(last.toList.foldMap {
          case (p, l) =>
            val pr = t.proof(p).toOption.get
            expect(PoseidonMerkleTree.verifyInclusion(l, pr, t.root))
              .and(expect(!PoseidonMerkleTree.verifyInclusion(Fr.reduce(l.value + 1), pr, t.root), "tampered leaf"))
              .and(
                expect(
                  PoseidonMerkleProof
                    .from(pr.position, pr.siblings.updated(3, Fr.reduce(pr.siblings(3).value + 1)))
                    .forall(bad => !PoseidonMerkleTree.verifyInclusion(l, bad, t.root)),
                  "tampered sibling"
                )
              )
              .and(expect.same(Right(pr), PoseidonMerkleProof.toCanonical(pr).flatMap(PoseidonMerkleProof.fromCanonical)))
              .and(expect(!PoseidonMerkleTree.verifyAbsence(pr, t.root), "a set position has no absence proof"))
        })
    }
  }

  pureTest("absence proofs: valid for unset positions, refused for set ones, restored by clearing") {
    val t = tree0.insert(5, fr(42)).toOption.get
    val absent = t.absenceProof(6).toOption.get
    val cleared = t.insert(5, Fr.Zero).toOption.get
    expect(PoseidonMerkleTree.verifyAbsence(absent, t.root))
      .and(expect.same(Left(PoseidonMerkleError.OccupiedPosition(5)), t.absenceProof(5)))
      .and(expect(!PoseidonMerkleTree.verifyAbsence(tree0.absenceProof(5).toOption.get, t.root), "stale absence proof"))
      .and(expect.same(tree0.root, cleared.root))
      .and(expect(t.insert(256, fr(1)).isLeft, "position out of range"))
      .and(expect(PoseidonMerkleProof.from(4, Vector(Fr.Zero, Fr.Zero)).isLeft, "a position beyond 2^depth is unconstructible"))
  }

  test("committed zk vectors match the reference implementation") {
    IO(new String(Files.readAllBytes(PoseidonVectors.path), UTF_8)).map(c =>
      expect(c == PoseidonVectors.render, s"${PoseidonVectors.path} is stale")
    )
  }
}

/** Conformance vectors for Poseidon and the Poseidon Merkle tree (docs/specs/zk.md). */
object PoseidonVectors {
  val path: Path = Paths.get("shared", "vectors", "poseidon_v1.json")

  def render: String = {
    def fr(i: Int): Fr = Fr.reduce(BigInt(i))
    val hashes = List(List(0), List(1), List(1, 2), List(3, 4, 5), List(1, 2, 3, 4)).map(_.map(fr))
    val tree = PoseidonMerkleTree
      .empty(16)
      .flatMap(_.insertAll(List(BigInt(0) -> fr(1), BigInt(1) -> fr(2), BigInt(4660) -> fr(3))))
      .toOption
      .get
    Json
      .obj(
        "spec"    -> "docs/specs/zk.md".asJson,
        "version" -> 1.asJson,
        "poseidon" -> hashes
          .map(in =>
            Json.obj("inputs" -> in.map(_.value.toString).asJson, "output" -> Poseidon.hash(in).toOption.get.value.toString.asJson)
          )
          .asJson,
        "merkle" -> Json.obj(
          "depth"  -> 16.asJson,
          "leaves" -> Json.obj("0" -> "1".asJson, "1" -> "2".asJson, "4660" -> "3".asJson),
          "root"   -> tree.root.value.toString.asJson,
          "proofs" -> List(BigInt(1), BigInt(4660), BigInt(7)).map { p =>
            Json.obj("position" -> p.toString.asJson, "siblings" -> tree.proof(p).toOption.get.siblings.map(_.value.toString).asJson)
          }.asJson
        )
      )
      .spaces2 + "\n"
  }
}

object PoseidonVectorGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    IO(Files.write(PoseidonVectors.path, PoseidonVectors.render.getBytes(UTF_8))).as(ExitCode.Success)
}
