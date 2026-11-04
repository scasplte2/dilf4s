package xyz.kd5ujc.accumulators.trie

import java.util.concurrent.atomic.AtomicLong

import cats.effect.IO
import cats.mtl.{Handle, Raise}
import cats.syntax.all._

import scala.util.hashing.MurmurHash3

import xyz.kd5ujc.core.canonical.{ByteSource, CanonicalValue}
import xyz.kd5ujc.core.hash.{Digest, Sha256}
import xyz.kd5ujc.storage.{RawStore, StoreError, Version}

import scodec.bits.{BitVector, ByteOrdering, ByteVector}
import weaver.{Expectations, SimpleIOSuite}

/**
 * Hash-flooding regressions for maps of trie nodes. A node's digest is content a peer may choose (a snapshot's), and a
 * `Digest`'s JVM hash code is a seedless MurmurHash3 of its bytes, so a hashed map of them degrades to scanning one
 * bucket. A real attacker must find node content whose digest has the chosen hash code, about 2^32 node hashes per
 * colliding node (done once, then reusable). A test can skip that work: its `TrieHashing` chooses the node digests
 * directly, solving MurmurHash3's last block so that every one has the same hash code. Nothing here measures time:
 *   - every map is built in the order passed, and its comparisons are counted: O(n log n) for a build, O(depth log n)
 *     per update;
 *   - every node digest counts the reads of its bytes, and outside the order's comparisons and node hashing there are
 *     none. Any `hashCode` reads all of a digest's bytes and any `equals` between two instances reads some, so no node
 *     digest is ever hashed or tested for equality.
 */
object CollidingDigestsSuite extends SimpleIOSuite {

  private val Base = TrieHashing[Sha256]
  private val fixed = KeyMode.fixed(32).fold(e => sys.error(e.toString), identity)
  private val N = 1 << 12

  private def get[A](e: Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)

  /** `ceil(log2 (n + 1))`: a red-black tree of `n` entries is at most twice this high. */
  private def log2(n: Long): Long = 64L - java.lang.Long.numberOfLeadingZeros(n)

  /** The most comparisons one descent of a red-black tree of at most `n` entries makes. */
  private def descent(n: Long): Long = 2L * log2(n) + 1L

  /**
   * `ByteVector`'s hash code is MurmurHash3 over little-endian 4-byte blocks, and one block's mixing is a bijection, so
   * the last block can be solved for any state: every digest forced here hashes as `Target` does after it. Only a test
   * can do this, because it chooses the digest bytes itself; an attacker controls only the content they hash.
   */
  private object Murmur {
    private val (c1, c2, c3) = (0xcc9e2d51, 0x1b873593, 0xe6546b64)
    private val Target = 0x2545f491

    /** The inverse of an odd number mod 2^32: Newton's iteration doubles the correct low bits from three. */
    private def inverse(odd: Int): Int = (1 to 5).foldLeft(odd)((x, _) => x * (2 - odd * x))

    private def block(b: ByteVector, i: Int): Int = b.slice(4L * i, 4L * i + 4L).toInt(ordering = ByteOrdering.LittleEndian)

    /** `b` with its last block replaced so that MurmurHash3's state after it is `Target`. */
    def collide(b: ByteVector): ByteVector = {
      val h = (0 until 7).foldLeft(MurmurHash3.arraySeed)((h, i) => MurmurHash3.mix(h, block(b, i)))
      // mix(h, k) = rotl(h ^ rotl(k * c1, 15) * c2, 13) * 5 + c3, solved for k
      val mixed = Integer.rotateRight((Target - c3) * inverse(5), 13) ^ h
      val k = Integer.rotateRight(mixed * inverse(c2), 15) * inverse(c1)
      b.take(28) ++ ByteVector.fromInt(k, ordering = ByteOrdering.LittleEndian)
    }
  }

  /** Reads of the bytes of every node digest made under it, and of those made while hashing nodes. */
  final private class Reads {
    val total: AtomicLong = new AtomicLong
    val hashing: AtomicLong = new AtomicLong
    def during[A](into: AtomicLong)(f: => A): A = {
      val before = total.get
      val a = f
      into.addAndGet(total.get - before)
      a
    }
  }

  /**
   * The trie's node hashing with every node digest forced onto one hash code, and counting the reads of its bytes. Value
   * digests are the caller's and plain; whatever a value's streamed commitment reads counts as hashing, like a node's.
   */
  final private class Colliding(reads: Reads) extends TrieHashing[Sha256] {
    def hasher = Base.hasher
    def empty: Digest[Sha256] = Base.empty
    val streamed: AtomicLong = new AtomicLong
    def valueOf(bytes: ByteSource): Digest[Sha256] = {
      streamed.incrementAndGet()
      reads.during(reads.hashing)(Base.valueOf(bytes))
    }
    def leaf(segment: BitVector, value: Digest[Sha256]): Digest[Sha256] = forced(reads.during(reads.hashing)(Base.leaf(segment, value)))
    def branch(
      segment: BitVector,
      value:   Option[Digest[Sha256]],
      left:    Option[Digest[Sha256]],
      right:   Option[Digest[Sha256]]
    ): Digest[Sha256] =
      forced(reads.during(reads.hashing)(Base.branch(segment, value, left, right)))

    private def forced(d: Digest[Sha256]): Digest[Sha256] = {
      val bytes = Murmur.collide(d.bytes)
      get(Digest.fromBytes[Sha256](ByteVector.viewAt(i => { reads.total.incrementAndGet(); bytes(i) }, bytes.size)))
    }
  }

  /** The digests' byte order, counting its comparisons and the bytes they read. */
  final private class Counting(reads: Reads) extends Ordering[Digest[Sha256]] {
    val comparisons: AtomicLong = new AtomicLong
    val bytes: AtomicLong = new AtomicLong
    def compare(a: Digest[Sha256], b: Digest[Sha256]): Int = {
      comparisons.incrementAndGet()
      reads.during(bytes)(Digest.digestOrdering[Sha256].compare(a, b))
    }
  }

  /** A fresh world: reads, the colliding hashing over them, and a counting order. */
  private def world: IO[(Reads, Colliding, Counting)] = IO {
    val reads = new Reads
    (reads, new Colliding(reads), new Counting(reads))
  }

  private def key(i:   Int): ByteVector = Base.value(ByteVector.fromInt(i)).bytes
  private def value(i: Int): Digest[Sha256] = Base.value(ByteVector.fromInt(-i))
  private val entries: List[(ByteVector, Digest[Sha256])] = (0 until N).toList.map(i => key(i) -> value(i))

  /** The reads that neither a comparison nor node hashing made: any is a `hashCode` or an `equals` on a node digest. */
  private def stray(reads: Reads, order: Counting): Long = reads.total.get - reads.hashing.get - order.bytes.get

  /** One hash code across all of `ds`, computed on copies so that no counted digest is read. */
  private def oneHashCode(what: String, ds: Iterable[Digest[Sha256]]): Expectations = {
    val copies = ds.toList.map(d => get(Digest.fromBytes[Sha256](ByteVector(d.bytes.toArray))))
    expect(copies.map(_.hashCode).distinct.size == 1, s"$what: ${copies.map(_.hashCode).distinct.size} hash codes")
      .and(expect(copies.map(_.bytes).distinct.size == copies.size, s"$what: the digests are distinct"))
  }

  /** The branches on a key's path: a key proof expands exactly them. */
  private def depth(trie: Trie[Sha256], k: ByteVector): Long = {
    def go(n: Option[ProofNode[Sha256]]): Long = n match {
      case Some(b: ProofNode.PBranch[Sha256]) => 1L + go(b.left) + go(b.right)
      case _                                  => 0L
    }
    go(get(trie.prove(List(k))).top)
  }

  test("forced digests collide: one ByteVector and one Digest hash code for distinct bytes") {
    IO {
      val forced = (0 until 512).toList.map(i => Murmur.collide(key(i)))
      expect
        .same(1, forced.map(_.hashCode).distinct.size)
        .and(expect.same(1, forced.map(b => get(Digest.fromBytes[Sha256](b)).hashCode).distinct.size))
        .and(expect.same(512, forced.distinct.size))
    }
  }

  test("fromEntries: O(n log n) comparisons, in the order passed; no node digest hashed or compared for equality") {
    world.map {
      case (reads, hashing, order) =>
        val trie = get(Trie.fromEntriesIn(fixed, entries, order)(using hashing))
        val (comparisons, strays) = (order.comparisons.get, stray(reads, order))
        val m = trie.nodeCount.toLong
        expect(comparisons >= m - 1L, s"$comparisons comparisons for $m nodes; a hashed map makes none")
          .and(expect(comparisons <= 3L * m * descent(m), s"$comparisons comparisons for $m nodes, above ${3L * m * descent(m)}"))
          .and(expect(strays == 0L, s"$strays reads of node digests outside comparisons and hashing"))
          .and(expect(trie.storedNodes.ordering eq order, "the nodes are keyed in the order passed"))
          .and(expect.same(2L * N - 1L, m))
          .and(oneHashCode("built", trie.storedNodes.keys))
          .and(expect.same(get(Trie.fromEntries(fixed, entries)(using hashing)).root, trie.root))
    }
  }

  test("updates: O(depth log n) comparisons each, the trie's order kept; no node digest hashed or compared for equality") {
    world.map {
      case (reads, hashing, order) =>
        val built = get(Trie.fromEntriesIn(fixed, entries, order)(using hashing))
        // puts, removes, and puts of canonical values, whose commitments are streamed through `valueOf`
        type Step = Trie[Sha256] => Either[TrieError, Trie[Sha256]]
        val ops: List[(ByteVector, Step)] = (0 until 64).toList.map { i =>
          val (removed, added) = (key(i * 17), key(N + i))
          i % 3 match {
            case 2 => removed -> ((t: Trie[Sha256]) => t.remove(removed))
            case 1 => added   -> ((t: Trie[Sha256]) => t.putCanonical(added, CanonicalValue.CInt(i)))
            case _ => added   -> ((t: Trie[Sha256]) => t.put(added, value(N + i)))
          }
        }
        val (after, costs) = ops.foldLeft((built, Vector.empty[(Long, Long)])) {
          case ((trie, cs), (_, op)) =>
            val before = order.comparisons.get
            val next = get(op(trie))
            (next, cs :+ (order.comparisons.get - before -> trie.nodeCount.toLong))
        }
        val (strays, streamed) = (stray(reads, order), hashing.streamed.get)
        val reference = ops.foldLeft(get(Trie.fromEntries(fixed, entries)(using hashing)))((t, op) => get(op._2(t)))
        // an update rebuilds the frames on its key's path and releases the old path, so the deeper of the two bounds it
        val bounds = ops.zip(costs).map {
          case ((k, _), (_, m)) =>
            val d = math.max(depth(built, k), depth(after, k))
            16L * (d + 3L) * descent(m)
        }
        costs
          .zip(bounds)
          .foldLeft(success) {
            case (acc, ((comparisons, m), bound)) =>
              acc
                .and(expect(comparisons >= 1L, s"an update of a trie of $m nodes made no comparison; a hashed map makes none"))
                .and(expect(comparisons <= bound, s"an update of a trie of $m nodes made $comparisons comparisons, above $bound"))
          }
          .and(expect(strays == 0L, s"$strays reads of node digests outside comparisons and hashing"))
          .and(expect(streamed >= 21L, s"$streamed canonical values streamed through valueOf, of 21 put"))
          .and(expect(after.storedNodes.ordering eq order, "an updated trie keeps the order its nodes were keyed in"))
          .and(expect.same(reference.root, after.root))
          .and(expect.same(reference.nodeCount, after.nodeCount))
          .and(oneHashCode("updated", after.storedNodes.keys))
    }
  }

  test("proofs: O(depth log n) comparisons per key, in the trie's order") {
    world.map {
      case (_, hashing, order) =>
        val trie = get(Trie.fromEntriesIn(fixed, entries, order)(using hashing))
        val m = trie.nodeCount.toLong
        (0 until 16).toList.map(i => key(i * 101)).foldLeft(success) { (acc, k) =>
          val d = depth(trie, k)
          val before = order.comparisons.get
          val proof = trie.prove(List(k))
          val comparisons = order.comparisons.get - before
          val bound = 4L * (d + 1L) * descent(m)
          acc
            .and(expect(comparisons >= d, s"a proof of $d branches made $comparisons comparisons; a hashed map makes none"))
            .and(expect(comparisons <= bound, s"a proof of $d branches made $comparisons comparisons, above $bound"))
            .and(expect(proof.flatMap(p => TrieProofs.verify(fixed, trie.root, p)(using hashing)).isRight, "the proof verifies"))
        }
    }
  }

  sealed private trait Failure
  final private case class Store(e: StoreError) extends Failure
  final private case class Tree(e: TrieError) extends Failure

  private def run[A](f: (Raise[IO, StoreError], Raise[IO, TrieError]) => IO[A]): IO[Either[Failure, A]] =
    Handle
      .allowF[IO, StoreError](rs =>
        Handle.allowF[IO, TrieError](rt => f(rs, rt).map(_.asRight[Failure])).rescue(e => IO.pure(Tree(e).asLeft[A]))
      )
      .rescue(e => IO.pure(Store(e).asLeft[A]))

  test("store: a restored colliding snapshot and staged commits on it: O(n log n), then O(depth log n) per commit") {
    world.flatMap {
      case (_, hashing, order) =>
        val trie = get(Trie.fromEntriesIn(fixed, entries, order)(using hashing))
        val m = trie.nodeCount.toLong
        val ops = (0 until 32).toList.map(i => key(N + i) -> Option(value(N + i)))
        run { (rs, rt) =>
          given Raise[IO, StoreError] = rs
          given Raise[IO, TrieError] = rt
          given TrieHashing[Sha256] = hashing
          for {
            raw      <- RawStore.inMemory[IO]
            store    <- TrieStore.makeIn[IO, Sha256](raw, "trie", fixed, order)
            before   <- IO(order.comparisons.get)
            restored <- store.stageRestore(trie, Version.unsafe(1L))
            restore  <- IO(order.comparisons.get - before)
            chain <- ops.foldLeftM((restored, Vector.empty[Long])) {
              case ((base, cs), op) =>
                IO(order.comparisons.get).flatMap(c0 =>
                  store.stageOn(base, List(op)).map(next => (next, cs :+ (order.comparisons.get - c0)))
                )
            }
            (last, costs) = chain
            _    <- store.write(last)
            root <- store.root(Version.unsafe(1L + ops.size))
          } yield {
            val updated = get(trie.update(ops))
            val bound = 16L * (ops.map((k, _) => depth(updated, k)).max + 3L) * descent(m + 64L * ops.size)
            expect(restore >= m - 1L, s"restoring $m nodes made $restore comparisons; a hashed map makes none")
              .and(expect(restore <= m * descent(m), s"restoring $m nodes made $restore comparisons, above ${m * descent(m)}"))
              .and(costs.foldLeft(success) { (acc, c) =>
                acc
                  .and(expect(c >= 1L, "a staged commit made no comparison; a hashed map makes none"))
                  .and(expect(c <= bound, s"a staged commit over $m nodes made $c comparisons, above $bound"))
              })
              .and(expect(restored.nodes.ordering eq order, "a restore's plan is keyed in the store's order"))
              .and(expect(last.nodes.ordering eq order, "a staged commit's overlay is keyed in the store's order"))
              .and(expect.same(updated.root, root))
          }
        }.map(_.fold(e => failure(s"unexpected $e"), identity))
    }
  }

  test("a trie keyed in another order restores into a store in its own") {
    world.flatMap {
      case (_, hashing, order) =>
        val trie = get(Trie.fromEntries(fixed, entries.take(64))(using hashing))
        run { (rs, rt) =>
          given Raise[IO, StoreError] = rs
          given Raise[IO, TrieError] = rt
          given TrieHashing[Sha256] = hashing
          for {
            raw      <- RawStore.inMemory[IO]
            store    <- TrieStore.makeIn[IO, Sha256](raw, "trie", fixed, order)
            restored <- store.stageRestore(trie, Version.unsafe(1L))
            _        <- store.write(restored)
            root     <- store.root(Version.unsafe(1L))
          } yield
            expect(restored.nodes.ordering eq order, "the plan is rebuilt in the store's order")
              .and(expect(order.comparisons.get >= trie.nodeCount - 1L, "restoring compared the nodes in the store's order"))
              .and(expect.same(trie.root, root))
        }.map(_.fold(e => failure(s"unexpected $e"), identity))
    }
  }
}
