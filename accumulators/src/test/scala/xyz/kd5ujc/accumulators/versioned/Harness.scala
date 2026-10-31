package xyz.kd5ujc.accumulators.versioned

import cats.effect.IO
import cats.mtl.Handle
import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.{KeyMode, Trie, TrieHashing}
import xyz.kd5ujc.accumulators.versioned.VersionedTrie.Head
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue.CNull
import xyz.kd5ujc.core.hash.{Digest, Sha256}
import xyz.kd5ujc.storage.{Batch, RawStore}

import org.scalacheck.Gen
import scodec.bits.ByteVector

/** Test helpers, and the reference model: tries built from scratch over plain maps, independent of the store. */
object Harness {

  type H = Sha256
  val T: TrieHashing[H] = TrieHashing[H]

  def get[A](e: Either[?, A]): A = e.fold(err => throw new AssertionError(err.toString), identity)

  def key(s:  String): StateKey = get(StateKey.parse(s))
  def ns(s:   String): StateNamespace = get(StateNamespace.parse(s))
  def int(i:  Int): CanonicalValue = CanonicalValue.int(BigInt(i))
  def text(s: String): CanonicalValue = get(CanonicalValue.text(s))
  def vk(v:   Long): ByteVector = get(VersionKey.long.encode(v))

  def attempt[A](f: Handle[IO, VersionedTrieError] => IO[A]): IO[Either[VersionedTrieError, A]] =
    Handle.allowF[IO, VersionedTrieError](h => f(h).map(_.asRight[VersionedTrieError])).rescue(e => IO.pure(e.asLeft[A]))

  def fresh(implicit h: Handle[IO, VersionedTrieError]): IO[(RawStore[IO], VersionedTrieStore[IO, Long, H])] =
    RawStore.inMemory[IO].flatMap(raw => VersionedTrieStore.make[IO, Long, H](raw, "c").map(raw -> _))

  def open(raw: RawStore[IO])(implicit h: Handle[IO, VersionedTrieError]): IO[VersionedTrieStore[IO, Long, H]] =
    VersionedTrieStore.make[IO, Long, H](raw, "c")

  /** Commits batches at the given versions, each against the previous head. */
  def commitAll(
    store: VersionedTrieStore[IO, Long, H],
    steps: List[(Long, Batch[StateKey, CanonicalValue])],
    from:  Option[Long] = None
  ): IO[List[Head[Long, H]]] =
    steps
      .foldLeftM((from, List.empty[Head[Long, H]])) {
        case ((prev, acc), (v, b)) => store.commit(prev, v, b).map(h => (Some(v), acc :+ h))
      }
      .map(_._2)

  // ---- reference model

  type State = Map[StateKey, CanonicalValue]

  def applyBatch(s: State, b: Batch[StateKey, CanonicalValue]): State =
    b.last.foldLeft(s) {
      case (acc, (k, Some(v))) if v != CNull => acc.updated(k, v)
      case (acc, (k, _))                     => acc - k
    }

  def stateRoot(s: State): Digest[H] = get(Projection.stateRoot[H](s))

  def historyRoot(h: List[(Long, Digest[H])]): Digest[H] =
    get(Trie.fromEntries[H](get(KeyMode.fixed(8)), h.map { case (v, s) => vk(v) -> VersionedTrie.bytesHash[H](s.bytes) })).root

  /** The heads and states a sequence of commits must produce, computed from scratch at every step. */
  def model(steps: List[(Long, Batch[StateKey, CanonicalValue])]): List[(Head[Long, H], State)] =
    steps
      .foldLeft((Map.empty: State, List.empty[(Long, Digest[H])], List.empty[(Head[Long, H], State)])) {
        case ((s, hist, acc), (v, b)) =>
          val next = applyBatch(s, b)
          val h2 = hist :+ (v                                          -> stateRoot(next))
          (next, h2, acc :+ (Head(v, stateRoot(next), historyRoot(h2)) -> next))
      }
      ._3

  // ---- generators

  private val segment = Gen.oneOf("a", "b", "ab", "a.b", "x-1", "z_9")
  val keyGen: Gen[StateKey] = Gen.choose(1, 3).flatMap(Gen.listOfN(_, segment)).map(s => key(s.mkString("/")))

  val valueGen: Gen[CanonicalValue] = Gen.oneOf(
    Gen.choose(0, 3).map(int),
    Gen.oneOf("x", "y").map(text),
    Gen.oneOf(ByteVector.empty, ByteVector(0x61, 0x61)).map(CanonicalValue.bytes),
    Gen.const(get(CanonicalValue.array(Vector(int(1), CNull)))),
    Gen.const(get(CanonicalValue.map(List("k" -> int(1)))))
  )

  val batchGen: Gen[Batch[StateKey, CanonicalValue]] =
    Gen
      .listOf(Gen.zip(keyGen, Gen.option(valueGen)))
      .map(_.take(6))
      .map(ops =>
        Batch(ops.toVector.map {
          case (k, v) =>
            v.fold[xyz.kd5ujc.storage.Op[StateKey, CanonicalValue]](xyz.kd5ujc.storage.Op.Delete(k))(xyz.kd5ujc.storage.Op.Put(k, _))
        })
      )

  /** Strictly increasing versions with gaps. */
  val stepsGen: Gen[List[(Long, Batch[StateKey, CanonicalValue])]] =
    Gen.choose(1, 6).flatMap(n => Gen.listOfN(n, Gen.zip(Gen.choose(1L, 300L), batchGen))).map { gs =>
      gs.scanLeft((-1L, Batch.empty[StateKey, CanonicalValue])) { case ((v, _), (gap, b)) => (v + gap, b) }.tail
    }

  implicit val showBatch: cats.Show[Batch[StateKey, CanonicalValue]] = cats.Show.fromToString
  implicit val showValue: cats.Show[CanonicalValue] = cats.Show.fromToString
}
