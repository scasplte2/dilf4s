package xyz.kd5ujc.accumulators.versioned

import scala.collection.immutable.TreeMap

import xyz.kd5ujc.accumulators.ProofInput
import xyz.kd5ujc.accumulators.versioned.Harness._
import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.storage.Batch

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/**
 * A value that shares structure is small in memory and can be exponentially long as CBOR: 60 levels of `[v, v]` over
 * an integer is 2^61 nodes. Every versioned entry point that sizes or hashes a caller's value refuses it by its weight
 * first (docs/specs/versioned-trie.md §4), so each of these returns at once instead of encoding it.
 */
object SharedValueSuite extends SimpleIOSuite {

  private val doubled: CanonicalValue = (1 to 60).foldLeft(int(1))((v, _) => get(CanonicalValue.array(Vector(v, v))))

  /** `TooLarge` with the limit alone: nothing past the limit is measured. */
  private def tooLarge(what: String, limit: Long)(r: Either[VersionedTrieError, ?]): Boolean =
    r == Left(VersionedTrieError.TooLarge(what, limit))

  private val k = key("a/b")
  private val ofK = "the value of a/b"
  private val root = xyz.kd5ujc.core.hash.Hasher[H].digest(ByteVector.empty)

  pureTest("the doubled value weighs 2^61 - 1 (2^60 integers, 2^60 - 1 arrays) and is 60 levels deep, within the limit") {
    expect
      .same((BigInt(1) << 61) - 1, BigInt(doubled.weight))
      .and(expect.same(60, doubled.depth))
      .and(expect(doubled.depth <= VersionedTrie.MaxValueDepth))
  }

  pureTest("checkValue: refused by its weight, with the limit alone") {
    expect(tooLarge(ofK, VersionedTrie.MaxValueBytes)(VersionedTrie.checkValue(k, doubled)), s"${VersionedTrie.checkValue(k, doubled)}")
      .and(expect.same(doubled.weight, VersionedTrie.valueBytes(doubled)))
  }

  pureTest("checkValue: within the weight, the length is measured, and refused past the limit") {
    // 2^17 integers of 2^40 weigh 1 each but take 9 bytes each
    val wide = get(CanonicalValue.array(Vector.fill(1 << 17)(CanonicalValue.CInt(1L << 40))))
    val exact = Cbor.encode(wide).size.toLong
    val atLimit = CanonicalValue.bytes(ByteVector.fill(VersionedTrie.MaxValueBytes - 5)(1))
    expect(wide.weight <= VersionedTrie.MaxValueBytes, s"weight ${wide.weight}")
      .and(expect(exact > VersionedTrie.MaxValueBytes, s"length $exact"))
      .and(expect.same(Left(VersionedTrieError.TooLarge(ofK, VersionedTrie.MaxValueBytes)), VersionedTrie.checkValue(k, wide)))
      .and(expect.same(VersionedTrie.MaxValueBytes, Cbor.encode(atLimit).size))
      .and(expect.same(Right(()), VersionedTrie.checkValue(k, atLimit)))
  }

  test("a commit refuses it") {
    attempt(implicit h => fresh.flatMap(s => h.attempt(s._2.commit(None, 1L, Batch.put(k, doubled))))).map { r =>
      expect(r.flatten.left.exists(_ == VersionedTrieError.TooLarge(ofK, VersionedTrie.MaxValueBytes)), s"$r")
    }
  }

  test("a verifier answers a claim no commit accepts false without hashing it; others as before") {
    attempt { implicit h =>
      for {
        s     <- fresh
        head  <- s._2.commit(None, 1L, Batch.put(k, int(7)))
        proof <- s._2.proveState(StateQuery.keys(k, key("a/c")))
      } yield (head, proof)
    }.map {
      case Left(e) => failure(s"unexpected $e")
      case Right((head, proof)) =>
        val state = get(VersionedTrieVerifier.verifyState(get(VersionedTrie.root(head)), proof))
        val deep = (0 to VersionedTrie.MaxValueDepth).foldLeft(int(7))((v, _) => get(CanonicalValue.array(Vector(v))))
        expect
          .same(Right(false), state.holds(k, Some(doubled)))
          .and(expect.same(Right(false), state.holds(key("a/c"), Some(doubled))))
          .and(expect.same(Right(true), state.holds(k, Some(int(7)))))
          .and(expect.same(Right(false), state.holds(k, Some(int(8)))))
          .and(expect.same(Right(false), state.holds(k, Some(deep))))
          .and(expect.same(Right(true), state.holds(key("a/c"), None)))
    }
  }

  pureTest("Projection.stateRoot and verifyAgainst refuse state no commit accepts: too large before hashing, too deep") {
    val deep = (0 to VersionedTrie.MaxValueDepth).foldLeft(int(1))((v, _) => get(CanonicalValue.array(Vector(v))))
    val committable = TreeMap(key("a/a") -> int(1))
    val r = get(Projection.stateRoot[H](committable))
    expect(tooLarge(ofK, VersionedTrie.MaxValueBytes)(Projection.stateRoot[H](committable + (k -> doubled))))
      .and(expect(tooLarge(ofK, VersionedTrie.MaxValueBytes)(Projection.verifyAgainst[H](committable + (k -> doubled), r))))
      .and(
        expect.same(
          Left(VersionedTrieError.TooDeep(ofK, VersionedTrie.MaxValueDepth + 1, VersionedTrie.MaxValueDepth)),
          Projection.verifyAgainst[H](committable + (k -> deep), r)
        )
      )
      .and(expect.same(Right(()), Projection.verifyAgainst[H](committable, r)))
  }

  pureTest("a snapshot carrying it is chunked without encoding it, then refused") {
    val head = Head[Long, H](1L, root, root)
    val snapshot = Snapshot[Long, H](head, List(k -> doubled), List(1L -> root))
    val chunks = Snapshot.chunks(snapshot)
    expect
      .same(k.bytes.size + VersionedTrie.MaxValueBytes + 1 + 16, Snapshot.entryBytes(k, doubled))
      .and(expect.same(3, chunks.size))
      .and(expect(tooLarge(ofK, VersionedTrie.MaxValueBytes)(Snapshot.read(head, chunks))))
      .and(expect(tooLarge(ofK, VersionedTrie.MaxValueBytes)(Replica.fromSnapshot(head, snapshot))))
  }

  pureTest("a delta upserting it is refused") {
    val delta = Delta[Long, H](None, Head(1L, root, root), List(k -> doubled), Nil)
    expect(tooLarge(ofK, VersionedTrie.MaxValueBytes)(Replica.empty[Long, H].flatMap(Replica.applyDelta(_, delta))))
  }

  pureTest("a snapshot chunk holding it is refused by its weight before it is encoded") {
    val chunk: SnapshotChunk[Long, H] = SnapshotChunk.State(Vector(k -> doubled))
    expect.same(Left(VersionedTrieError.TooLarge("a snapshot chunk", VersionedTrie.MaxChunkBytes)), VersionedTrieWire.encodeChunk(chunk))
  }

  /** One 1 MiB byte string (2^20 bytes of CBOR, the largest value), shared by every key that holds it. */
  private val mib = CanonicalValue.bytes(ByteVector.fill(VersionedTrie.MaxValueBytes - 5)(7))

  private def wide(n: Int): Batch[StateKey, CanonicalValue] =
    (1 to n).map(i => Batch.put(key(s"wide/$i"), mib)).reduce(_ ++ _)

  test("a commit whose delta no replica would decode (over MaxProofWeight) is refused, and nothing is written") {
    attempt(implicit h => fresh.flatMap(s => h.attempt(s._2.commit(None, 1L, wide(17))).flatMap(r => s._2.head.map(r -> _)))).map { r =>
      expect.same(Right((Left(VersionedTrieError.TooLarge("the delta", ProofInput.MaxProofWeight)), None)), r)
    }
  }

  test("a draft staged on a draft counts its parent: a delta one over MaxProofWeight is refused before it is written") {
    attempt { implicit h =>
      for {
        s  <- fresh
        d1 <- s._2.preview(1L, Batch.put(key("a"), int(1)))
        // the pad that brings version 2's delta, parent included, to one over the cap
        upserts = (1 to 15).map(i => key(s"wide/$i") -> mib).toList
        w0 = get(
          VersionedTrieWire.encodeDelta(
            Delta(Some(d1.head), d1.head, (key("pad") -> CanonicalValue.bytes(ByteVector.empty)) :: upserts, Nil)
          )
        ).weight
        pad = CanonicalValue.bytes(ByteVector.fill(ProofInput.MaxProofWeight + 1 - w0)(1))
        d2   <- h.attempt(s._2.previewOn(d1, 2L, wide(15) ++ Batch.put(key("pad"), pad)))
        head <- s._2.head
      } yield (pad, d2.map(_.head), head)
    }.map {
      case Left(e) => failure(s"unexpected $e")
      case Right((pad, d2, head)) =>
        expect(VersionedTrie.checkValue(key("pad"), pad).isRight, "the pad is a value a commit accepts")
          .and(expect.same(Left(VersionedTrieError.TooLarge("the delta", ProofInput.MaxProofWeight)), d2))
          .and(expect.same(None, head))
    }
  }

  pureTest("a proof no verifier would decode is TooLarge, whether one embedded trie proof or the whole record is over") {
    def leafOf(bytes: Long) =
      xyz.kd5ujc.accumulators.trie
        .TrieProof[H](Some(xyz.kd5ujc.accumulators.trie.ProofNode.PLeaf(scodec.bits.BitVector.low(bytes * 8), root)))
    val head = Head[Long, H](1L, root, root)
    val third = ProofInput.MaxProofWeight / 3 + 1024
    val refused = Left(VersionedTrieError.TooLarge("the proof", ProofInput.MaxProofWeight))
    expect
      .same(refused, VersionedTrieWire.encodeProof(VersionedTrieProof[Long, H](head, leafOf(ProofInput.MaxProofWeight), None, None, None)))
      .and(
        expect.same(
          refused,
          VersionedTrieWire.encodeProof(VersionedTrieProof[Long, H](head, leafOf(third), None, Some(leafOf(third)), Some(leafOf(third))))
        )
      )
      .and(expect(VersionedTrieWire.encodeProof(VersionedTrieProof[Long, H](head, leafOf(third), None, None, None)).isRight))
  }

  test("a batch of many keys sharing one long value is refused by the delta's weight before any value is measured") {
    // 2^16 integers of 2^40 weigh 1 each and take 9 bytes: a value of weight 65,537 and 589,827 bytes of CBOR that a
    // commit accepts; 20,000 keys hold it, so measuring each would cost 2^34 steps; their weights alone pass 2^24
    val long: CanonicalValue = get(CanonicalValue.array(Vector.fill(1 << 16)(CanonicalValue.CInt(1L << 40))))
    val batch = (1 to 20000).map(i => Batch.put(key(s"shared/$i"), long)).reduce(_ ++ _)
    attempt(implicit h => fresh.flatMap(s => h.attempt(s._2.commit(None, 1L, batch)))).map { r =>
      expect.same(Right(Left(VersionedTrieError.TooLarge("the delta", ProofInput.MaxProofWeight))), r)
    }
  }

  test("a commit just within it serves a delta that decodes and replays to the same head") {
    attempt { implicit h =>
      for {
        s    <- fresh
        head <- s._2.commit(None, 1L, wide(15))
        d    <- s._2.delta(1L)
      } yield head -> d
    }.map {
      case Left(e) => failure(s"unexpected $e")
      case Right((head, d)) =>
        val replayed = for {
          wire    <- VersionedTrieWire.encodeDelta(d)
          decoded <- VersionedTrieWire.decodeDelta[Long, H](wire)
          r0      <- Replica.empty[Long, H]
          r       <- Replica.applyDelta(r0, decoded)
        } yield (wire.weight, r.head)
        expect(replayed.exists(_._1 <= ProofInput.MaxProofWeight), s"$replayed").and(expect.same(Right(Some(head)), replayed.map(_._2)))
    }
  }
}
