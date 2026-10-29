package xyz.kd5ujc.accumulators.merkle

import xyz.kd5ujc.core.hash.{Digest, Hasher, Sha256}

import org.scalacheck.Gen
import scodec.bits.{ByteVector, HexStringSyntax}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerkleTreeSuite extends SimpleIOSuite with Checkers {

  private val tagged: MerkleHashing[Sha256] = MerkleHashing.tagged[Sha256]
  private val ct: MerkleHashing[Sha256] = MerkleHashing.rfc6962[Sha256]

  /** RFC 9162 MTH written straight from the definition: the reference the level-based tree must match. */
  private def referenceRoot(leaves: Vector[ByteVector])(implicit M: MerkleHashing[Sha256]): Digest[Sha256] =
    leaves.size match {
      case 0 => M.empty
      case 1 => M.leaf(leaves(0))
      case n =>
        val k = java.lang.Integer.highestOneBit(n - 1)
        M.node(referenceRoot(leaves.take(k)), referenceRoot(leaves.drop(k)))
    }

  private def tree(leaves: Vector[ByteVector])(implicit M: MerkleHashing[Sha256]): MerkleTree[Sha256] =
    MerkleTree.fromLeafHashes[Sha256](leaves.map(M.leaf))

  private val leafData: Gen[ByteVector] = Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))
  private val leaves: Gen[Vector[ByteVector]] = Gen.choose(0, 70).flatMap(n => Gen.listOfN(n, leafData)).map(_.toVector)
  private def leavesOfSize(min: Int): Gen[Vector[ByteVector]] = Gen.choose(min, 70).flatMap(n => Gen.listOfN(n, leafData)).map(_.toVector)
  private val nonEmptyLeaves: Gen[Vector[ByteVector]] = leavesOfSize(1)
  implicit private val showLeaves: cats.Show[Vector[ByteVector]] = cats.Show.show(_.map(_.toHex).mkString("[", ",", "]"))
  implicit private val showEdit: cats.Show[(Vector[ByteVector], Int, ByteVector)] = cats.Show.fromToString

  /** The Certificate Transparency test leaves (certificate-transparency-go merkle tests). */
  private val ctLeaves: Vector[ByteVector] = Vector(
    hex"",
    hex"00",
    hex"10",
    hex"2021",
    hex"3031",
    hex"40414243",
    hex"5051525354555657",
    hex"606162636465666768696a6b6c6d6e6f"
  )

  private val ctRoots: Vector[String] = Vector(
    "6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d",
    "fac54203e7cc696cf0dfcb42c92a1d9dbaf70ad9e621f4bd8d98662f00e3c125",
    "aeb6bcfe274b70a14fb067a5e5578264db0fa9b51af5e0ba159158f329e06e77",
    "d37ee418976dd95753c1c73862b9398fa2a2cf9b4ff0fdfe8b30cd95209614b7",
    "4e3bbb1f7b478dcfe71fb631631519a3bca12c9aefca1612bfce4c13a86264d4",
    "76e67dadbcdf1e10e1b74ddc608abd2f98dfb16fbce75277b5232a127f2087ef",
    "ddb89be403809e325750d3d263cd78929c2942b7942a34b77e122c9594a74c8c",
    "5dc9da79a70659a9ad559cb701ded9a2ab9d823aad2f4960cfe370eff4604328"
  )

  pureTest("RFC 6962 profile reproduces the Certificate Transparency roots") {
    (1 to 8)
      .map(n => expect.same(ctRoots(n - 1), tree(ctLeaves.take(n))(ct).root.hex))
      .reduce(_ and _)
      .and(expect.same(Hasher[Sha256].digest(ByteVector.empty), MerkleTree.empty[Sha256](ct).root))
  }

  test("the level-based tree equals the reference MTH at every size") {
    forall(leaves)(ls => expect.same(referenceRoot(ls)(tagged), tree(ls)(tagged).root))
  }

  test("repeating the last leaf always changes the root (no CVE-2012-2459 malleability)") {
    forall(nonEmptyLeaves)(ls => expect(tree(ls)(tagged).root != tree(ls :+ ls.last)(tagged).root, s"size ${ls.size}"))
  }

  test("every inclusion proof verifies, including for repeated leaves") {
    forall(nonEmptyLeaves.map(ls => ls ++ ls.take(3))) { ls =>
      val t = tree(ls)(tagged)
      ls.indices.toList.map { i =>
        val proof = t.inclusionProof(i.toLong)
        expect(proof.flatMap(p => MerkleVerifier.inclusion(t.root, tagged.leaf(ls(i)), p)(tagged)).isRight, s"index $i of ${ls.size}")
      }.reduce(_ and _)
    }
  }

  test("tampered inclusion proofs fail") {
    forall(leavesOfSize(2).flatMap(ls => Gen.choose(0, ls.size - 1).map(ls -> _))) {
      case (ls, i) =>
        val t = tree(ls)(tagged)
        val leaf = tagged.leaf(ls(i))
        val proof = t.inclusionProof(i.toLong).toOption.get
        val other = tagged.leaf(ls(i) ++ hex"ff")
        val flipped = proof.copy(path = proof.path.updated(0, tagged.leaf(hex"deadbeef")))
        expect(MerkleVerifier.inclusion(t.root, other, proof)(tagged).isLeft, "wrong leaf")
          .and(
            expect(
              MerkleVerifier.inclusion(t.root, leaf, proof.copy(index = (i + 1L) % ls.size))(tagged).isLeft || ls(i) == ls(
                (i + 1) % ls.size
              ),
              "wrong index"
            )
          )
          .and(
            expect(
              MerkleVerifier.inclusion(t.append(other).root, leaf, proof.copy(size = proof.size + 1))(tagged).isLeft,
              "stale proof against the grown tree"
            )
          )
          .and(expect(MerkleVerifier.inclusion(t.root, leaf, flipped)(tagged).isLeft, "altered path"))
          .and(expect(MerkleVerifier.inclusion(t.root, leaf, proof.copy(path = proof.path :+ leaf))(tagged).isLeft, "extended path"))
    }
  }

  test("consistency proofs verify between every pair of sizes") {
    forall(Gen.choose(1, 40).flatMap(n => Gen.listOfN(n, leafData)).map(_.toVector)) { ls =>
      val full = tree(ls)(tagged)
      (0 to ls.size).toList.map { m =>
        val old = tree(ls.take(m))(tagged)
        val proof = full.consistencyProof(m.toLong)
        expect(proof.flatMap(p => MerkleVerifier.consistency(old.root, full.root, p)(tagged)).isRight, s"$m -> ${ls.size}")
      }.reduce(_ and _)
    }
  }

  test("consistency fails for a tree that is not an extension") {
    forall(
      Gen.choose(2, 40).flatMap(n => Gen.listOfN(n, leafData)).map(_.toVector).flatMap(ls => Gen.choose(1, ls.size - 1).map(ls -> _))
    ) {
      case (ls, m) =>
        val full = tree(ls)(tagged)
        val forked = tree(ls.take(m - 1) :+ (ls(m - 1) ++ hex"01"))(tagged)
        val proof = full.consistencyProof(m.toLong).toOption.get
        expect(MerkleVerifier.consistency(forked.root, full.root, proof)(tagged).isLeft, s"forked prefix of size $m")
    }
  }

  pureTest("out-of-range requests are typed errors") {
    val t = tree(ctLeaves)(tagged)
    expect
      .same(Left(MerkleError.IndexOutOfRange(8, 8)), t.inclusionProof(8))
      .and(expect.same(Left(MerkleError.IndexOutOfRange(-1, 8)), t.inclusionProof(-1)))
      .and(expect.same(Left(MerkleError.InvalidSizes(9, 8)), t.consistencyProof(9)))
      .and(expect.same(Left(MerkleError.IndexOutOfRange(0, 0)), MerkleTree.empty[Sha256](tagged).inclusionProof(0)))
  }

  pureTest("profiles are domain separated from each other and from raw hashing") {
    expect(tagged.leaf(hex"00") != ct.leaf(hex"00"), "tagged and RFC 6962 leaves differ")
      .and(expect(tagged.leaf(hex"") != tagged.empty, "an empty leaf is not the empty tree"))
      .and(
        expect(
          tagged.node(tagged.leaf(hex"01"), tagged.leaf(hex"02")) != tagged.leaf(tagged.leaf(hex"01").bytes ++ tagged.leaf(hex"02").bytes),
          "a node is never a leaf"
        )
      )
  }

  test("update, prepend and remove agree with rebuilding from the edited leaf list") {
    forall(nonEmptyLeaves.flatMap(ls => Gen.zip(Gen.choose(0, ls.size - 1), leafData).map { case (i, d) => (ls, i, d) })) {
      case (ls, i, d) =>
        implicit val m: MerkleHashing[Sha256] = tagged
        val t = tree(ls)
        expect
          .same(Right(tree(ls.updated(i, d)).root), t.update(i.toLong, m.leaf(d)).map(_.root))
          .and(expect.same(tree(d +: ls).root, t.prependAll(List(m.leaf(d))).root))
          .and(expect.same(Right(tree(ls.patch(i, Nil, 1)).root), t.removeAt(i.toLong).map(_.root)))
          .and(expect.same(Left(MerkleError.IndexOutOfRange(ls.size.toLong, ls.size.toLong)), t.update(ls.size.toLong, m.leaf(d))))
    }
  }

  pureTest("indicesOf finds every position of a repeated leaf") {
    implicit val m: MerkleHashing[Sha256] = tagged
    val t = tree(Vector(hex"01", hex"02", hex"01", hex"03", hex"01"))
    expect.same(List(0L, 2L, 4L), t.indicesOf(m.leaf(hex"01"))).and(expect.same(Nil, t.indicesOf(m.leaf(hex"09"))))
  }
}
