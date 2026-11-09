package xyz.kd5ujc.accumulators.trie

import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.accumulators.trie.ProofNode.{PBranch, PLeaf, Stub}
import xyz.kd5ujc.core.hash.{Digest, Sha256}

import org.scalacheck.Gen
import scodec.bits.{BitVector, ByteVector}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/**
 * The listing algorithms as they were before docs/specs/trie.md §4 bounded their work: every node rebuilds its path and
 * compares it with the bounds from bit 0, and a check builds the whole listing before comparing it with the claim. The
 * differential reference for the walks, and, counting its bit comparisons, the quadratic work they must stay below.
 */
private[trie] object QuadraticListing {

  type Entries = List[(BitVector, Digest[Sha256])]

  /** The common prefix's length, and the bits compared. */
  def commonPrefix(a: BitVector, b: BitVector): (Long, Long) = {
    val c = Bits.commonPrefix(a, b)
    (c, c + 1)
  }

  def lt(a: BitVector, b: BitVector): (Boolean, Long) = {
    val (c, n) = commonPrefix(a, b)
    (if (c == a.size) a.size < b.size else if (c == b.size) false else !a(c), n)
  }

  def lteq(a: BitVector, b: BitVector): (Boolean, Long) = {
    val (c, n) = commonPrefix(a, b)
    if (c == a.size && a.size == b.size) (true, n) else lt(a, b).map(_ + n)
  }

  def startsWith(s: BitVector, prefix: BitVector): (Boolean, Long) =
    if (prefix.size > s.size) (false, 1L) else commonPrefix(s, prefix).leftMap(_ == prefix.size)

  def intersects(prefix: BitVector, from: BitVector, until: Option[BitVector]): (Boolean, Long) = {
    val (extends_, n) = startsWith(from, prefix)
    val (below, m) = if (extends_) (false, 0L) else lt(prefix, from)
    val (above, k) = until.fold((false, 0L))(lteq(_, prefix))
    (!below && !above, n + m + k)
  }

  def within(key: BitVector, from: BitVector, until: Option[BitVector]): (Boolean, Long) = {
    val (atLeast, n) = lteq(from, key)
    val (under, m) = until.fold((true, 0L))(lt(key, _))
    (atLeast && under, n + m)
  }

  def range(top: Option[ProofNode[Sha256]], from: BitVector, until: Option[BitVector]): (Option[Entries], Long) = {
    @tailrec
    def go(todo: List[(ProofNode[Sha256], BitVector)], acc: Entries, steps: Long): (Option[Entries], Long) =
      todo match {
        case Nil => (Some(acc.reverse), steps)
        case (node, at) :: rest =>
          node match {
            case Stub(_) => (None, steps)
            case PLeaf(segment, v) =>
              val key = at ++ segment
              val (in, n) = within(key, from, until)
              go(rest, if (in) (key, v) :: acc else acc, steps + n)
            case PBranch(segment, value, left, right) =>
              val here = at ++ segment
              val (own, n) = value.fold((acc, 0L))(v => within(here, from, until).leftMap(in => if (in) (here, v) :: acc else acc))
              val kids = List(left.map(_ -> (here :+ false)), right.map(_ -> (here :+ true))).flatten.map {
                case (c, p) => (c, p, intersects(p, from, until))
              }
              go(kids.collect { case (c, p, (true, _)) => c -> p } ++ rest, own, steps + n + kids.map(_._3._2).sum)
          }
      }
    val (meets, n) = intersects(BitVector.empty, from, until)
    top.fold[(Option[Entries], Long)]((Some(Nil), 0L))(t => if (meets) go(List(t -> BitVector.empty), Nil, n) else (Some(Nil), n))
  }

  def prefix(top: Option[ProofNode[Sha256]], prefix: BitVector): Option[Entries] = {
    @tailrec
    def locate(node: ProofNode[Sha256], consumed: BitVector, remaining: BitVector): Option[Option[(ProofNode[Sha256], BitVector)]] =
      node match {
        case Stub(_)           => None
        case PLeaf(segment, _) => Some(Option.when(segment.startsWith(remaining))((node, consumed)))
        case PBranch(segment, _, left, right) =>
          if (segment.startsWith(remaining)) Some(Some((node, consumed)))
          else if (!remaining.startsWith(segment)) Some(None)
          else {
            val rest = remaining.drop(segment.size)
            ((if (rest.head) right else left): Option[ProofNode[Sha256]]) match {
              case None        => Some(None)
              case Some(child) => locate(child, consumed ++ segment ++ BitVector.bit(rest.head), rest.tail)
            }
          }
      }
    top.fold[Option[Entries]](Some(Nil)) { t =>
      locate(t, BitVector.empty, prefix).flatMap(_.fold[Option[Entries]](Some(Nil)) {
        case (n, at) => range(Some(n), BitVector.empty, None)._1.map(_.map { case (k, v) => (at ++ k, v) })
      })
    }
  }

  def listsPrefix(top: Option[ProofNode[Sha256]], p: BitVector, claimed: Entries): Either[TrieError, Boolean] =
    prefix(top, p).toRight(TrieError.NotCovered("the prefix")).map(_ == claimed)

  def listsRange(top: Option[ProofNode[Sha256]], from: BitVector, until: Option[BitVector], claimed: Entries): Either[TrieError, Boolean] =
    range(top, from, until)._1.toRight(TrieError.NotCovered("the range")).map(_ == claimed)

  /** The paths above the nodes a range proof expands (looks up), as the prover chose them before. */
  def expanded(full: ProofNode[Sha256], from: BitVector, until: Option[BitVector]): Set[BitVector] = {
    @tailrec
    def go(todo: List[(ProofNode[Sha256], BitVector)], acc: Set[BitVector]): Set[BitVector] =
      todo match {
        case Nil => acc
        case (PBranch(segment, _, left, right), at) :: rest =>
          val here = at ++ segment
          val kids = List(left.map(_ -> (here :+ false)), right.map(_ -> (here :+ true))).flatten.filter {
            case (_, p) => intersects(p, from, until)._1
          }
          go(kids ++ rest, acc + at)
        case (_, at) :: rest => go(rest, acc + at)
      }
    if (intersects(BitVector.empty, from, until)._1) go(List(full -> BitVector.empty), Set.empty) else Set.empty
  }
}

object TrieListingSuite extends SimpleIOSuite with Checkers {

  private val T = TrieHashing[Sha256]
  private val fixed2 = KeyMode.fixed(2).toOption.get
  private val variable4 = KeyMode.variable(4).toOption.get
  private val variableMax = KeyMode.variable(KeyMode.MaxVariableBytes).toOption.get

  private type Entries = QuadraticListing.Entries
  private type Result[A] = Either[TrieError, A]

  private def value(i: Int): Digest[Sha256] = T.value(ByteVector.fromInt(i))

  private val byte: Gen[Byte] = Gen.oneOf(0x00.toByte, 0x01.toByte, 0x7f.toByte, 0x80.toByte, 0xff.toByte)
  private def keyGen(mode: KeyMode): Gen[ByteVector] = mode match {
    case f: KeyMode.Fixed => Gen.listOfN(f.bytes, byte).map(ByteVector(_))
    case _                => Gen.choose(0, 4).flatMap(Gen.listOfN(_, byte)).map(ByteVector(_))
  }

  /** A bound: often a key, or a key cut to any bit length (a prefix of keys, off a byte boundary), or a key extended. */
  private def boundGen(mode: KeyMode): Gen[BitVector] =
    Gen.oneOf(
      keyGen(mode).map(_.bits),
      Gen.zip(keyGen(mode), Gen.choose(0, 40)).map { case (k, n) => k.bits.take(n.toLong) },
      Gen.zip(keyGen(mode), Gen.oneOf(BitVector.zero, BitVector.one, BitVector.low(9))).map { case (k, e) => k.bits ++ e }
    )

  /** The proof's queries: keys, prefixes and ranges, so the proofs have stubs in every position. */
  private def proofGen(mode: KeyMode): Gen[(Trie[Sha256], TrieProof[Sha256])] =
    for {
      keys     <- Gen.listOf(keyGen(mode)).map(_.take(24))
      queried  <- Gen.listOf(keyGen(mode)).map(_.take(3))
      prefixes <- Gen.listOf(Gen.choose(0, 2).flatMap(Gen.listOfN(_, byte)).map(ByteVector(_))).map(_.take(2))
      ranges   <- Gen.listOf(Gen.zip(keyGen(mode), Gen.option(keyGen(mode)))).map(_.take(2))
    } yield {
      val t = Trie.fromEntries[Sha256](mode, keys.zipWithIndex.map { case (k, i) => k -> value(i) }).toOption.get
      (t, t.prove(queried, prefixes, ranges).toOption.get)
    }

  /** Claims to check: the true listing, and near misses of it (one entry short or extra, out of order, a wrong value). */
  private def claims(truth: Entries, other: Entries): List[Entries] =
    List(
      truth,
      Nil,
      truth.drop(1),
      truth.dropRight(1),
      truth.reverse,
      truth ++ other.take(1),
      other.take(1) ++ truth,
      truth.map { case (k, _) => (k, value(-1)) },
      truth.take(1).map { case (k, v) => (k ++ BitVector.zero, v) } ++ truth.drop(1),
      other
    ).distinct

  implicit private val showCase: cats.Show[((Trie[Sha256], TrieProof[Sha256]), BitVector, Option[BitVector])] = cats.Show.show {
    case ((t, _), f, u) => s"$t from=${f.toBin} until=${u.map(_.toBin)}"
  }

  implicit private val showKeys: cats.Show[(List[ByteVector], BitVector, Option[BitVector])] = cats.Show.show {
    case (ks, f, u) => s"${ks.map(_.toHex)} from=${f.toBin} until=${u.map(_.toBin)}"
  }

  private def verified(mode: KeyMode, t: Trie[Sha256], p: TrieProof[Sha256]): VerifiedProof[Sha256] =
    TrieProofs.verify(mode, t.root, p).toOption.get

  private def sameAsBefore(mode: KeyMode) =
    forall(Gen.zip(proofGen(mode), boundGen(mode), Gen.option(boundGen(mode)))) {
      case ((t, p), from, until) =>
        val v = verified(mode, t, p)
        val whole = t.entries.toOption.get.map { case (k, h) => (k.bits, h) }
        val before = QuadraticListing.range(p.top, from, until)._1
        val listing = before.getOrElse(Nil)
        val ranges = claims(listing, whole).map(c => (v.listsRange(from, until, c), QuadraticListing.listsRange(p.top, from, until, c)))
        val prefixes = claims(QuadraticListing.prefix(p.top, from).getOrElse(Nil), whole).map(c =>
          (v.listsPrefix(from, c), QuadraticListing.listsPrefix(p.top, from, c))
        )
        expect
          .same(before, v.range(from, until))
          .and(expect.same(QuadraticListing.prefix(p.top, from), v.prefix(from)))
          .and(expect.same(ranges.map(_._2), ranges.map(_._1)))
          .and(expect.same(prefixes.map(_._2), prefixes.map(_._1)))
    }

  test("range and prefix listings and checks answer as before, for every bound and claim, in variable mode") {
    sameAsBefore(variable4)
  }

  test("range and prefix listings and checks answer as before, for every bound and claim, in fixed mode") {
    sameAsBefore(fixed2)
  }

  pureTest("boundary cases: from == until, until at the empty key, the empty proof, a stub out of range") {
    val keys = List(ByteVector.empty, ByteVector(0x61), ByteVector(0x61, 0x62), ByteVector(0x62))
    val t = Trie.fromEntries[Sha256](variable4, keys.zipWithIndex.map { case (k, i) => k -> value(i) }).toOption.get
    val keyOnly = t.prove(List(ByteVector(0x62))).toOption.get
    val v = verified(variable4, t, keyOnly)
    val b = (k: ByteVector) => k.bits
    val empty = TrieProofs.verify(variable4, T.empty, TrieProof[Sha256](None)).toOption.get
    expect
      .same(Right(true), v.listsRange(b(ByteVector(0x62)), Some(b(ByteVector(0x62))), Nil))
      .and(expect.same(Right(true), v.listsRange(BitVector.empty, Some(BitVector.empty), Nil)))
      .and(expect.same(Right(true), empty.listsRange(BitVector.empty, None, Nil)))
      .and(expect.same(Right(false), empty.listsRange(BitVector.empty, None, List(BitVector.empty -> value(0)))))
      .and(expect.same(Right(true), v.listsRange(b(ByteVector(0x62)), None, List(b(ByteVector(0x62)) -> value(3)))))
      .and(expect.same(Left(TrieError.NotCovered("the range")), v.listsRange(b(ByteVector(0x61)), Some(b(ByteVector(0x61))), Nil)))
      .and(expect.same(Left(TrieError.NotCovered("the range")), v.listsRange(BitVector.empty, None, Nil)))
      .and(expect.same(Right(true), v.listsPrefix(b(ByteVector(0x62)), List(b(ByteVector(0x62)) -> value(3)))))
      .and(expect.same(Left(TrieError.NotCovered("the prefix")), v.listsPrefix(b(ByteVector(0x61)), Nil)))
  }

  test("a range proof expands the same nodes as before") {
    forall(Gen.zip(Gen.listOf(keyGen(variable4)).map(_.take(24)), boundGen(variable4), Gen.option(boundGen(variable4)))) {
      case (keys, from, until) =>
        val t = Trie.fromEntries[Sha256](variable4, keys.zipWithIndex.map { case (k, i) => k -> value(i) }).toOption.get
        val full = t.prove(Nil, List(ByteVector.empty)).toOption.get.top
        val lookup = (h: Digest[Sha256]) => t.storedNodes.get(h).map(_._1).toRight[TrieError](TrieError.MissingNode(h.hex))
        val ranged = TrieProofs.prove[Result, Sha256](lookup, t.rootOption, List(TrieProofs.Range(from, until)))
        expect.same(
          Right(full.fold(Set.empty[BitVector])(QuadraticListing.expanded(_, from, until))),
          ranged.map(_.top.fold(Set.empty[BitVector])(expandedPaths))
        )
    }
  }

  /** The paths above a proof's non-stub nodes. */
  private def expandedPaths(top: ProofNode[Sha256]): Set[BitVector] = {
    @tailrec
    def go(todo: List[(ProofNode[Sha256], BitVector)], acc: Set[BitVector]): Set[BitVector] =
      todo match {
        case Nil                       => acc
        case (Stub(_), _) :: rest      => go(rest, acc)
        case (PLeaf(_, _), at) :: rest => go(rest, acc + at)
        case (PBranch(s, _, l, r), at) :: rest =>
          go(l.map(_ -> (at ++ s :+ false)).toList ++ r.map(_ -> (at ++ s :+ true)).toList ++ rest, acc + at)
      }
    go(List(top -> BitVector.empty), Set.empty)
  }

  // ------------------------------------------------------------------ the deep chain (issue #90): work, not time

  /**
   * The keys `00^k` for `k = 0..n` in `variable(65535)`: a pre-order chain of `n` branches, each a 7-bit zero segment
   * (the first an empty one) with a value and a left child, ending in a leaf. Built bottom up, as its root.
   */
  private def chain(n: Int): (Digest[Sha256], TrieProof[Sha256]) = {
    val seven = BitVector.low(7)
    @tailrec
    def go(k: Int, below: ProofNode[Sha256], hash: Digest[Sha256]): (Digest[Sha256], TrieProof[Sha256]) =
      if (k < 0) (hash, TrieProof(Some(below)))
      else {
        val segment = if (k == 0) BitVector.empty else seven
        go(k - 1, PBranch(segment, Some(value(k)), Some(below), None), T.branch(segment, Some(value(k)), Some(hash), None))
      }
    go(n - 1, PLeaf(seven, value(n)), T.leaf(seven, value(n)))
  }

  private def zeros(k: Int): BitVector = ByteVector.low(k.toLong).bits

  /** The chain's listing of `[from, until)`. */
  private def chainListing(n: Int, from: Int, until: Option[Int]): Entries =
    (from to until.fold(n)(_ - 1)).toList.map(k => zeros(k) -> value(k))

  final private case class Deep(
    name:      String,
    run:       VerifiedProof[Sha256] => Counted[Either[TrieError, Boolean]],
    expected:  Boolean,
    boundBits: Long,
    claimBits: Long
  )

  /**
   * The constructions of issue #90 on a chain of `n`: long bounds (`from = 00^(n-1)`, `until = 00^(n-1)‖01`, both, a
   * long prefix) with an empty claim (false), and the full listing claimed (true), whose keys the check compares.
   */
  private def deepCases(n: Int): List[Deep] = {
    val long = zeros(n - 1)
    val past = long ++ ByteVector(0x01).bits
    val tail = chainListing(n, n - 1, None)
    val all = chainListing(n, 0, None)
    val bits = (es: Entries) => es.map(_._1.size).sum
    List(
      Deep("long from", _.checkRange(long, None, Nil), expected = false, long.size, 0L),
      Deep("long until", _.checkRange(BitVector.empty, Some(past), Nil), expected = false, past.size, 0L),
      Deep("long from and until", _.checkRange(long, Some(past), Nil), expected = false, long.size + past.size, 0L),
      Deep("long prefix", _.checkPrefix(long, Nil), expected = false, long.size, 0L),
      Deep("long from, the tail claimed", _.checkRange(long, None, tail), expected = true, long.size, bits(tail)),
      Deep("from 0x, everything claimed", _.checkRange(BitVector.empty, None, all), expected = true, 0L, bits(all)),
      Deep("prefix 0x, everything claimed", _.checkPrefix(BitVector.empty, all), expected = true, 0L, bits(all))
    )
  }

  /** Bits of weight: the proof as JLVM gas meters it (30 per unit of weight for trie_range_verify). */
  private def proofBits(p: TrieProof[Sha256]): Long = 8L * TrieProofs.encode(p).toOption.get.weight

  pureTest("listing checks on a deep chain do work linear in the proof plus the bounds and the claim; before, quadratic") {
    val n = 600
    val (root, proof) = chain(n)
    val v = TrieProofs.verify(variableMax, root, proof).toOption.get
    val bits = proofBits(proof)
    val long = zeros(n - 1)
    val (_, before) = QuadraticListing.range(proof.top, long, None)
    deepCases(n).map { c =>
      val Counted(result, steps) = c.run(v)
      val bound = 2L * (bits + c.boundBits + c.claimBits)
      expect
        .same(Right(c.expected), result)
        .and(expect(steps <= bound, s"${c.name}: $steps steps over 2 * (proof + bounds + claim) = $bound"))
    }
      .reduce(_ and _)
      .and(expect(before > 2L * (bits + long.size), s"the quadratic walk of a long from: $before steps, under ${2L * (bits + long.size)}"))
  }

  pureTest("a chain twice as deep takes twice the work, and stays stack-safe at 8000 levels") {
    val counts = List(4000, 8000).map { n =>
      val (root, proof) = chain(n)
      val v = TrieProofs.verify(variableMax, root, proof).toOption.get
      // claiming every key claims Θ(n^2) bits, which the check compares: the doubling holds for linear claims
      deepCases(n).filter(_.claimBits <= 16L * n).map(c => c.name -> c.run(v)).toMap
    }
    val (small, large) = (counts.head, counts(1))
    small.keys.toList.map { name =>
      val (a, b) = (small(name), large(name))
      expect.same(a.value, b.value).and(expect(b.steps <= 2 * a.steps + 64, s"$name: ${a.steps} steps at 4000, ${b.steps} at 8000"))
    }
      .reduce(_ and _)
  }

  // ------------------------------------------------------------------ operands built as ropes

  /** `n` zero bytes as a rope of `n` one-byte prepends, which scodec indexes recursively (time and stack its depth). */
  private def rope(n: Int): ByteVector = (1 to n).foldLeft(ByteVector.empty)((acc, _) => ByteVector.low(1) ++ acc)

  pureTest("rope-built bounds and claims are read flat: the same answers and work as flat ones") {
    val n = 600
    val (root, proof) = chain(n)
    val v = TrieProofs.verify(variableMax, root, proof).toOption.get
    val tail = chainListing(n, n - 1, None)
    val ropeTail = tail.map { case (k, h) => (rope((k.size / 8).toInt).bits, h) }
    val flat = v.checkRange(zeros(n - 1), None, tail)
    val roped = v.checkRange(rope(n - 1).bits, None, ropeTail)
    val prefixFlat = v.checkPrefix(zeros(n - 1), tail)
    val prefixRoped = v.checkPrefix(rope(n - 1).bits, ropeTail)
    expect
      .same(Right(true), roped.value)
      .and(expect.same(flat, roped))
      .and(expect.same(Right(true), prefixRoped.value))
      .and(expect.same(prefixFlat, prefixRoped))
  }

  pureTest("a 60000-byte rope key, bound, prefix or segment is stack-safe") {
    val k = 60000
    val deep = rope(k)
    val leaf = T.leaf(deep.bits, value(1))
    val proof = TrieProof[Sha256](Some(PLeaf(deep.bits, value(1))))
    val v = TrieProofs.verify(variableMax, leaf, proof).toOption.get
    val claim = List(rope(k).bits -> value(1))
    expect
      .same(Right(true), v.listsRange(rope(k).bits, None, claim))
      .and(expect.same(Right(true), v.listsRange(BitVector.empty, Some(rope(k + 1).bits), claim)))
      .and(expect.same(Right(true), v.listsPrefix(rope(k - 1).bits, claim)))
      .and(expect.same(Some(List(deep.bits -> value(1))), v.range(rope(k).bits, None)))
  }

  pureTest("a bound longer than any path compares as its first PathCap bits do") {
    val (root, proof) = chain(8)
    val v = TrieProofs.verify(variableMax, root, proof).toOption.get
    val huge = BitVector.low(Bits.PathCap + 64)
    val all = chainListing(8, 0, None)
    expect
      .same(Right(true), v.listsRange(BitVector.empty, Some(huge), all))
      .and(expect.same(Right(true), v.listsRange(huge, None, Nil)))
      .and(expect.same(QuadraticListing.range(proof.top, huge, None)._1, v.range(huge, None)))
      .and(expect.same(Right(true), v.listsPrefix(huge, Nil)))
  }
}
