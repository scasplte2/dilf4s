package xyz.kd5ujc.accumulators.versioned

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

  /** `TooLarge` for the doubled value, its size the value's weight: a lower bound on its length. */
  private def tooLarge(what: String, max: Long)(r: Either[VersionedTrieError, ?]): Boolean =
    r == Left(VersionedTrieError.TooLarge(what, doubled.weight, max))

  private val k = key("a/b")
  private val ofK = "the value of a/b"
  private val root = xyz.kd5ujc.core.hash.Hasher[H].digest(ByteVector.empty)

  pureTest("the doubled value weighs 2^61 - 1 (2^60 integers, 2^60 - 1 arrays) and is 60 levels deep, within the limit") {
    expect
      .same((BigInt(1) << 61) - 1, BigInt(doubled.weight))
      .and(expect.same(60, doubled.depth))
      .and(expect(doubled.depth <= VersionedTrie.MaxValueDepth))
  }

  pureTest("checkValue: refused by its weight, the size reported as that lower bound") {
    expect(tooLarge(ofK, VersionedTrie.MaxValueBytes)(VersionedTrie.checkValue(k, doubled)), s"${VersionedTrie.checkValue(k, doubled)}")
      .and(expect.same(doubled.weight, VersionedTrie.valueBytes(doubled)))
  }

  pureTest("checkValue: within the weight, the exact length is measured, and refused past the limit") {
    // 2^17 integers of 2^40 weigh 1 each but take 9 bytes each
    val wide = get(CanonicalValue.array(Vector.fill(1 << 17)(CanonicalValue.int(BigInt(1) << 40))))
    val exact = Cbor.encode(wide).size
    val atLimit = CanonicalValue.bytes(ByteVector.fill(VersionedTrie.MaxValueBytes - 5)(1))
    expect(wide.weight <= VersionedTrie.MaxValueBytes, s"weight ${wide.weight}")
      .and(expect.same(Left(VersionedTrieError.TooLarge(ofK, exact, VersionedTrie.MaxValueBytes)), VersionedTrie.checkValue(k, wide)))
      .and(expect.same(VersionedTrie.MaxValueBytes, Cbor.encode(atLimit).size))
      .and(expect.same(Right(()), VersionedTrie.checkValue(k, atLimit)))
  }

  test("a commit refuses it") {
    attempt(implicit h => fresh.flatMap(s => h.attempt(s._2.commit(None, 1L, Batch.put(k, doubled))))).map { r =>
      expect(r.flatten.left.exists(_ == VersionedTrieError.TooLarge(ofK, doubled.weight, VersionedTrie.MaxValueBytes)), s"$r")
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
    val committable = Map(key("a/a") -> int(1))
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
    val refused = VersionedTrieWire.encodeChunk(chunk)
    expect(
      refused.left.exists {
        case VersionedTrieError.TooLarge("a snapshot chunk", size, VersionedTrie.MaxChunkBytes) => size > doubled.weight
        case _                                                                                  => false
      },
      s"$refused"
    )
  }
}
