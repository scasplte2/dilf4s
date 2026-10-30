package xyz.kd5ujc.accumulators.trie

import cats.syntax.all._

import scala.util.Random

import xyz.kd5ujc.accumulators.trie.Node.{Branch, Leaf}
import xyz.kd5ujc.core.canonical.Cbor
import xyz.kd5ujc.core.hash.{Digest, Sha256}
import xyz.kd5ujc.storage.ByteOrder.unsigned

import org.scalacheck.Gen
import scodec.bits.{BitVector, ByteVector, hex}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object TrieSuite extends SimpleIOSuite with Checkers {

  private val T = TrieHashing[Sha256]
  private val fixed2 = KeyMode.fixed(2).toOption.get
  private val variable4 = KeyMode.variable(4).toOption.get

  private def value(i: Int): Digest[Sha256] = T.value(ByteVector.fromInt(i))

  /** Small alphabets make shared prefixes, and in variable mode prefix keys, common. */
  private val byte: Gen[Byte] = Gen.oneOf(0x00.toByte, 0x01.toByte, 0x7f.toByte, 0x80.toByte, 0xff.toByte)
  private def keyGen(mode: KeyMode): Gen[ByteVector] = mode match {
    case f: KeyMode.Fixed => Gen.listOfN(f.bytes, byte).map(ByteVector(_))
    case _                => Gen.choose(0, 4).flatMap(Gen.listOfN(_, byte)).map(ByteVector(_))
  }

  private type Model = Map[ByteVector, Digest[Sha256]]
  private def opsGen(mode: KeyMode): Gen[List[(ByteVector, Option[Digest[Sha256]])]] =
    Gen.listOf(Gen.zip(keyGen(mode), Gen.option(Gen.choose(0, 9).map(value)))).map(_.take(40))

  private def model(ops: List[(ByteVector, Option[Digest[Sha256]])]): Model =
    ops.foldLeft(Map.empty: Model) { case (m, (k, v)) => v.fold(m - k)(m.updated(k, _)) }

  private def build(mode: KeyMode, ops: List[(ByteVector, Option[Digest[Sha256]])]): Trie[Sha256] =
    Trie.empty[Sha256](mode).update(ops).fold(e => throw new AssertionError(e.toString), identity)

  /** Applies the ops in batches of the given sizes, so every commit after the first releases an old root. */
  private def buildIncrementally(mode: KeyMode, ops: List[(ByteVector, Option[Digest[Sha256]])], sizes: LazyList[Int]): Trie[Sha256] = {
    @scala.annotation.tailrec
    def go(t: Trie[Sha256], rest: List[(ByteVector, Option[Digest[Sha256]])], sizes: LazyList[Int]): Trie[Sha256] =
      rest match {
        case Nil => t
        case _ =>
          val n = sizes.headOption.getOrElse(1).max(1)
          go(t.update(rest.take(n)).fold(e => throw new AssertionError(e.toString), identity), rest.drop(n), sizes.drop(1))
      }
    go(Trie.empty[Sha256](mode), ops, sizes)
  }

  implicit private val showOps: cats.Show[List[(ByteVector, Option[Digest[Sha256]])]] = cats.Show.show(_.map {
    case (k, v) => s"${k.toHex}->${v.map(_.hex.take(6))}"
  }.mkString(","))
  implicit private val showOpsKey: cats.Show[(List[(ByteVector, Option[Digest[Sha256]])], ByteVector)] = cats.Show.fromToString
  implicit private val showOpsSeed: cats.Show[(List[(ByteVector, Option[Digest[Sha256]])], Long)] = cats.Show.fromToString
  implicit private val showOpsKeys: cats.Show[(List[(ByteVector, Option[Digest[Sha256]])], List[ByteVector])] = cats.Show.fromToString

  private def bothModes[A](f: KeyMode => A): List[A] = List(fixed2, variable4).map(f)

  // ------------------------------------------------------------------ dictionary semantics

  test("lookups and listings match the reference model, in both key modes") {
    bothModes { mode =>
      forall(opsGen(mode)) { ops =>
        val m = model(ops)
        val t = build(mode, ops)
        val probes = (m.keySet ++ ops.map(_._1)).toList
        expect
          .same(probes.map(k => m.get(k)), probes.traverse(t.get).toOption.get)
          .and(expect.same(m.toList.sortBy(_._1), t.entries.toOption.get))
          .and(expect.same(m.isEmpty, t.isEmpty))
      }
    }.sequence.map(_.reduce(_ and _))
  }

  test("the root is a function of the contents: insertion order and history never matter") {
    bothModes { mode =>
      forall(Gen.zip(opsGen(mode), Gen.long)) {
        case (ops, seed) =>
          val m = model(ops)
          val incremental = buildIncrementally(mode, ops, LazyList.continually(new Random(seed).nextInt(4) + 1))
          val fresh = build(mode, m.toList.map { case (k, v) => k -> Some(v) })
          val shuffled = build(mode, new Random(seed).shuffle(m.toList).map { case (k, v) => k -> Some(v) })
          expect.same(fresh.root, incremental.root).and(expect.same(fresh.root, shuffled.root))
      }
    }.sequence.map(_.reduce(_ and _))
  }

  test("garbage collection keeps exactly the reachable nodes with exact reference counts") {
    bothModes { mode =>
      forall(Gen.zip(opsGen(mode), Gen.long)) {
        case (ops, seed) =>
          val t = buildIncrementally(mode, ops, LazyList.continually(new Random(seed).nextInt(3) + 1))
          val nodes = t.nodesForTest
          val reachable = TrieEngine
            .collect[Either[TrieError, *], Sha256](h => nodes.get(h).map(_._1).toRight(TrieError.MissingNode(h.hex)), t.rootForTest.toList)
            .toOption
            .get
          val parents = nodes.values.toList.flatMap {
            case (node, _) =>
              node match {
                case b: Branch[Sha256] => b.left.toList ++ b.right.toList
                case _: Leaf[Sha256]   => Nil
              }
          } ++ t.rootForTest.toList
          val expected = parents.groupBy(identity).view.mapValues(_.size).toMap
          expect.same(reachable.keySet, nodes.keySet).and(expect.same(expected, nodes.view.mapValues(_._2).toMap))
      }
    }.sequence.map(_.reduce(_ and _))
  }

  test("deleting everything, one key at a time, frees every node") {
    bothModes { mode =>
      forall(opsGen(mode)) { ops =>
        val full = build(mode, ops)
        val emptied = model(ops).keys.foldLeft(full)((t, k) => t.remove(k).fold(e => throw new AssertionError(e.toString), identity))
        expect.same(0, emptied.nodeCount).and(expect.same(T.empty, emptied.root))
      }
    }.sequence.map(_.reduce(_ and _))
  }

  test("bulk builds, sequential and parallel, equal incremental insertion with exact counts") {
    bothModes { mode =>
      forall(opsGen(mode)) { ops =>
        val entries = ops.collect { case (k, Some(v)) => k -> v }
        val incremental = build(mode, entries.map { case (k, v) => k -> Some(v) })
        val bulk = Trie.fromEntries[Sha256](mode, entries).toOption.get
        Trie.fromEntriesPar[cats.effect.IO, Sha256](mode, entries, parallelDepth = 3).map(_.toOption.get).map { par =>
          val counts = (t: Trie[Sha256]) => t.nodesForTest.view.mapValues(_._2).toMap
          expect
            .same(incremental.root, bulk.root)
            .and(expect.same(incremental.root, par.root))
            .and(expect.same(counts(incremental), counts(bulk)))
            .and(expect.same(counts(incremental), counts(par)))
            .and(expect.same(incremental.entries, par.entries))
        }
      }
    }.sequence.map(_.reduce(_ and _))
  }

  pureTest("keys are validated by mode") {
    expect(Trie.empty[Sha256](fixed2).put(hex"01", value(1)).isLeft, "fixed(2) rejects 1-byte keys")
      .and(expect(Trie.empty[Sha256](variable4).put(hex"0102030405", value(1)).isLeft, "variable(4) rejects 5-byte keys"))
      .and(expect(Trie.empty[Sha256](variable4).put(hex"", value(1)).isRight, "variable mode accepts the empty key"))
      .and(expect(KeyMode.fixed(0).isLeft && KeyMode.fixed(1025).isLeft && KeyMode.variable(65536).isLeft, "limits are enforced"))
  }

  pureTest("prefix keys coexist in variable mode") {
    val t =
      build(variable4, List(hex"61" -> Some(value(1)), hex"6162" -> Some(value(2)), hex"" -> Some(value(0)), hex"616263" -> Some(value(3))))
    expect
      .same(Right(Some(value(1))), t.get(hex"61"))
      .and(expect.same(Right(Some(value(0))), t.get(hex"")))
      .and(expect.same(Right(None), t.get(hex"6100")))
      .and(expect.same(Right(List(hex"" -> value(0), hex"61" -> value(1), hex"6162" -> value(2), hex"616263" -> value(3))), t.entries))
  }

  pureTest("the empty trie has the empty root and an empty proof") {
    val t = Trie.empty[Sha256](fixed2)
    val proof = t.prove(List(hex"0102")).toOption.get
    expect
      .same(T.empty, t.root)
      .and(expect.same(TrieProof[Sha256](None), proof))
      .and(expect.same(Right(Lookup.Absent), TrieProofs.verify(fixed2, t.root, proof).map(_.lookup(hex"0102".bits))))
  }

  // ------------------------------------------------------------------ proofs

  test("key proofs verify and answer presence and absence exactly") {
    bothModes { mode =>
      forall(Gen.zip(opsGen(mode), Gen.listOf(keyGen(mode)).map(_.take(6)))) {
        case (ops, queries) =>
          val m = model(ops)
          val t = build(mode, ops)
          val asked = queries ++ m.keys.take(3)
          val verified = t.prove(asked).flatMap(p => TrieProofs.verify(mode, t.root, p))
          verified match {
            case Left(e) => failure(s"proof failed to verify: $e")
            case Right(v) =>
              asked.map { k =>
                val expected = m.get(k).fold[Lookup[Sha256]](Lookup.Absent)(Lookup.Present(_))
                expect.same(expected, v.lookup(k.bits))
              }.foldLeft(success)(_ and _)
          }
      }
    }.sequence.map(_.reduce(_ and _))
  }

  /** Ops whose final model is guaranteed to hold two distinct keys `a` and `b` (built, never filtered). */
  private val twoKeyOps: Gen[(List[(ByteVector, Option[Digest[Sha256]])], ByteVector, ByteVector)] =
    Gen.zip(opsGen(fixed2), keyGen(fixed2)).map {
      case (ops, a) =>
        val b = a.update(a.size - 1, (a(a.size - 1) ^ 0x01).toByte)
        (ops ++ List(a -> Some(value(1)), b -> Some(value(2))), a, b)
    }
  implicit private val showTwoKeys: cats.Show[(List[(ByteVector, Option[Digest[Sha256]])], ByteVector, ByteVector)] = cats.Show.fromToString

  test("a stub is never mistaken for absence") {
    forall(twoKeyOps) {
      case (ops, asked, other) =>
        val m = model(ops)
        val t = build(fixed2, ops)
        val v = t.prove(List(asked)).flatMap(p => TrieProofs.verify(fixed2, t.root, p)).toOption.get
        val answer = v.lookup(other.bits)
        expect(answer == Lookup.NotCovered || answer == Lookup.Present(m(other)), s"unproven key answered $answer")
    }
  }

  test("prefix proofs list exactly the entries under the prefix") {
    forall(Gen.zip(opsGen(variable4), Gen.choose(0, 2).flatMap(Gen.listOfN(_, byte)).map(ByteVector(_)))) {
      case (ops, prefix) =>
        val m = model(ops)
        val t = build(variable4, ops)
        val listed = t.prove(Nil, List(prefix)).flatMap(p => TrieProofs.verify(variable4, t.root, p)).map(_.prefix(prefix.bits))
        val expected = m.toList.filter(_._1.startsWith(prefix)).sortBy(_._1).map { case (k, v) => (k.bits, v) }
        expect.same(Right(Some(expected)), listed)
    }
  }

  test("tampered proofs fail to verify") {
    forall(twoKeyOps) {
      case (ops, key, _) =>
        val t = build(fixed2, ops)
        val proof = t.prove(List(key)).toOption.get
        val bumped = mapLeaves(proof)(v => T.value(v.bytes ++ hex"00"))
        expect(TrieProofs.verify(fixed2, t.root, bumped).isLeft, "an altered value hash changes the root")
          .and(expect(TrieProofs.verify(fixed2, T.value(hex"00"), proof).isLeft, "a proof verifies only against its own root"))
    }
  }

  private def mapLeaves(p: TrieProof[Sha256])(f: Digest[Sha256] => Digest[Sha256]): TrieProof[Sha256] = {
    def go(n: ProofNode[Sha256]): ProofNode[Sha256] = n match {
      case ProofNode.PLeaf(s, v)         => ProofNode.PLeaf(s, f(v))
      case ProofNode.PBranch(s, v, l, r) => ProofNode.PBranch(s, v, l.map(go), r.map(go))
      case stub                          => stub
    }
    TrieProof(p.top.map(go))
  }

  pureTest("verification enforces the branch invariant and the key mode") {
    val leafA = ProofNode.PLeaf[Sha256](BitVector.low(15), value(1))
    val lonely = TrieProof[Sha256](Some(ProofNode.PBranch(BitVector.empty, None, Some(leafA), None)))
    val lonelyRoot = T.branch(BitVector.empty, None, Some(T.leaf(BitVector.low(15), value(1))), None)
    val valued =
      TrieProof[Sha256](Some(ProofNode.PBranch(BitVector.low(8), Some(value(9)), Some(ProofNode.PLeaf(BitVector.low(7), value(1))), None)))
    val valuedRoot = T.branch(BitVector.low(8), Some(value(9)), Some(T.leaf(BitVector.low(7), value(1))), None)
    val shortLeaf = TrieProof[Sha256](Some(ProofNode.PLeaf(BitVector.low(8), value(1))))
    expect(TrieProofs.verify(fixed2, lonelyRoot, lonely).isLeft, "one child and no value is not a valid branch")
      .and(expect(TrieProofs.verify(fixed2, valuedRoot, valued).isLeft, "fixed mode forbids branch values"))
      .and(expect(TrieProofs.verify(variable4, valuedRoot, valued).isRight, "variable mode allows them"))
      .and(expect(TrieProofs.verify(fixed2, T.leaf(BitVector.low(8), value(1)), shortLeaf).isLeft, "fixed(2) leaves must end at bit 16"))
  }

  test("proofs round-trip through the flat canonical encoding") {
    forall(Gen.zip(opsGen(variable4), Gen.listOf(keyGen(variable4)).map(_.take(5)))) {
      case (ops, queries) =>
        val t = build(variable4, ops)
        val proof = t.prove(queries, List(hex"01")).toOption.get
        val decoded = TrieProofs
          .encode(proof)
          .map(Cbor.encode)
          .flatMap(b => Cbor.decode(b).leftMap(e => TrieError.Encoding(e.toString)))
          .flatMap(TrieProofs.decode[Sha256])
        expect.same(Right(proof), decoded)
    }
  }

  pureTest("an adversarially deep trie stays stack-safe end to end") {
    val mode = KeyMode.variable(2000).toOption.get
    val keys = (1 to 2000).map(n => ByteVector.fill(n.toLong)(0x01)).toList
    val t = Trie.empty[Sha256](mode).update(keys.map(k => k -> Some(T.value(k)))).toOption.get
    val deepest = keys.last
    val result = for {
      proof    <- t.prove(List(deepest))
      encoded  <- TrieProofs.encode(proof)
      decoded  <- TrieProofs.decode[Sha256](encoded)
      verified <- TrieProofs.verify(mode, t.root, decoded)
    } yield verified.lookup(deepest.bits)
    expect
      .same(Right(Lookup.Present(T.value(deepest))), result)
      .and(expect.same(Right(Some(T.value(keys(999)))), t.get(keys(999))))
      .and(expect(t.remove(deepest).map(_.get(deepest)) == Right(Right(None)), "deleting the deepest key works"))
  }

  // ------------------------------------------------------------------ ranges, batches, forgery, shapes, scale

  implicit private val showRange: cats.Show[(List[(ByteVector, Option[Digest[Sha256]])], ByteVector, Option[ByteVector])] =
    cats.Show.fromToString

  private val bitOrder: Ordering[ByteVector] = (a, b) => if (KeyOrder.lt(a.bits, b.bits)) -1 else if (a == b) 0 else 1

  test("range proofs list exactly the entries in [from, until), and are complete") {
    forall(Gen.zip(opsGen(variable4), keyGen(variable4), Gen.option(keyGen(variable4)))) {
      case (ops, from, until) =>
        val m = model(ops)
        val t = build(variable4, ops)
        val expected = m.toList.filter { case (k, _) => KeyOrder.lteq(from.bits, k.bits) && until.forall(u => KeyOrder.lt(k.bits, u.bits)) }
          .sortBy(_._1)(using bitOrder)
          .map { case (k, v) => (k.bits, v) }
        val listed = t
          .prove(Nil, Nil, List(from -> until))
          .flatMap(p => TrieProofs.verify(variable4, t.root, p))
          .map(_.range(from.bits, until.map(_.bits)))
        expect.same(Right(Some(expected)), listed)
    }
  }

  test("a range proof with a stub inside the range is reported as not covered") {
    forall(twoKeyOps) {
      case (ops, a, _) =>
        val t = build(fixed2, ops)
        val keyOnly = t.prove(List(a)).toOption.get
        val v = TrieProofs.verify(fixed2, t.root, keyOnly).toOption.get
        val whole = v.range(BitVector.empty, None)
        expect(whole.isEmpty || model(ops).size == 1, s"a single-key proof cannot certify the whole range: $whole")
    }
  }

  test("a batch proof is no larger than the separate proofs combined") {
    forall(Gen.zip(opsGen(fixed2), Gen.listOfN(4, keyGen(fixed2)))) {
      case (ops, keys) =>
        val t = build(fixed2, ops)
        val size = (p: TrieProof[Sha256]) => TrieProofs.encode(p).map(v => Cbor.encode(v).size).toOption.get
        val batch = size(t.prove(keys).toOption.get)
        val separate = keys.map(k => size(t.prove(List(k)).toOption.get)).sum
        expect(batch <= separate, s"batch $batch > separate $separate")
    }
  }

  test("absence cannot be forged for a present key") {
    forall(twoKeyOps) {
      case (ops, a, _) =>
        val t = build(fixed2, ops)
        val proof = t.prove(List(a)).toOption.get
        // replace a's leaf by a stub of a different hash, or flip a bit of its segment
        def edit(f: ProofNode.PLeaf[Sha256] => ProofNode[Sha256]): TrieProof[Sha256] = {
          def go(n: ProofNode[Sha256], at: BitVector): ProofNode[Sha256] = n match {
            case l @ ProofNode.PLeaf(seg, _) if (at ++ seg) == a.bits => f(l)
            case ProofNode.PBranch(seg, v, l, r) =>
              ProofNode.PBranch(seg, v, l.map(go(_, at ++ seg :+ false)), r.map(go(_, at ++ seg :+ true)))
            case other => other
          }
          TrieProof(proof.top.map(go(_, BitVector.empty)))
        }
        val stubbed = edit(_ => ProofNode.Stub(T.value(ByteVector(7))))
        val flipped = edit(l =>
          if (l.segment.isEmpty) l.copy(segment = BitVector.one)
          else l.copy(segment = l.segment.update(l.segment.size - 1, !l.segment(l.segment.size - 1)))
        )
        val answers = List(stubbed, flipped).map(p => TrieProofs.verify(fixed2, t.root, p).map(_.lookup(a.bits)))
        expect(answers.forall(_.isLeft), s"forged proofs must not verify: $answers")
    }
  }

  pureTest("small tries have the expected shapes") {
    val v = value(1)
    def top(entries: List[(ByteVector, Digest[Sha256])], mode: KeyMode) =
      Trie.fromEntries[Sha256](mode, entries).flatMap(_.prove(Nil, List(ByteVector.empty))).map(_.top)
    val fixed1 = KeyMode.fixed(1).toOption.get
    expect
      .same(Right(Some(ProofNode.PLeaf[Sha256](hex"a5".bits, v))), top(List(hex"a5" -> v), fixed1))
      .and(
        expect.same(
          Right(
            Some(
              ProofNode.PBranch[Sha256](
                BitVector.empty,
                None,
                Some(ProofNode.PLeaf(BitVector.low(7), v)),
                Some(ProofNode.PLeaf(BitVector.low(7), v))
              )
            )
          ),
          top(List(hex"00" -> v, hex"80" -> v), fixed1)
        )
      )
      .and(
        expect.same(
          Right(
            Some(
              ProofNode.PBranch[Sha256](
                BitVector.low(7),
                None,
                Some(ProofNode.PLeaf(BitVector.empty, v)),
                Some(ProofNode.PLeaf(BitVector.empty, v))
              )
            )
          ),
          top(List(hex"00" -> v, hex"01" -> v), fixed1)
        )
      )
      .and(
        expect.same(
          Right(Some(ProofNode.PBranch[Sha256](hex"61".bits, Some(v), Some(ProofNode.PLeaf(hex"62".bits.tail, v)), None))),
          top(List(hex"61" -> v, hex"6162" -> v), variable4)
        )
      )
  }

  pureTest("a 5000-key trie builds, proves and deletes consistently") {
    val mode = KeyMode.fixed(4).toOption.get
    val rnd = new Random(7L)
    val keys = List.fill(5000)(ByteVector(rnd.nextBytes(4))).distinct
    val entries = keys.zipWithIndex.map { case (k, i) => k -> value(i) }
    val bulk = Trie.fromEntries[Sha256](mode, entries).toOption.get
    val incremental = Trie.empty[Sha256](mode).update(entries.map { case (k, v) => k -> Some(v) }).toOption.get
    val sample = keys.take(50)
    val proved = bulk
      .prove(sample)
      .flatMap(p => TrieProofs.verify(mode, bulk.root, p))
      .map(v => sample.forall(k => v.lookup(k.bits).isInstanceOf[Lookup.Present[?]]))
    val halved = keys.take(2500).foldLeft(bulk)((t, k) => t.remove(k).toOption.get)
    expect
      .same(bulk.root, incremental.root)
      .and(expect.same(Right(true), proved))
      .and(expect.same(Trie.fromEntries[Sha256](mode, entries.drop(2500)).map(_.root), Right(halved.root)))
  }
}
