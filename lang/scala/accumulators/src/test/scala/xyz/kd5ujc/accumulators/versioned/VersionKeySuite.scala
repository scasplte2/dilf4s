package xyz.kd5ujc.accumulators.versioned

import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.KeyMode
import xyz.kd5ujc.accumulators.versioned.Harness._

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/**
 * The bound on a version a refusal names (docs/specs/versioned-trie.md §2): every key reaching a refusal is checked at
 * its width, at most `KeyMode.MaxFixedBytes`, so its hex is at most twice that long, whatever version key a caller
 * brings; and that hex is the key in full, lowercase, with no `0x`.
 */
object VersionKeySuite extends SimpleIOSuite {

  /** A caller's version key whose encoding ignores its width. */
  private def lying(w: Int, out: ByteVector): VersionKey[Long] = new VersionKey[Long] {
    val width: Int = w
    def encode(v:     Long): Either[VersionedTrieError, ByteVector] = out.asRight
    def decode(bytes: ByteVector): Either[VersionedTrieError, Long] = 0L.asRight
  }

  pureTest("keyOf refuses an encoding of another width, and a width the history trie cannot key, as InvalidVersion") {
    val long = ByteVector.fill(1L << 20)(0xab)
    expect
      .same(Left(VersionedTrieError.InvalidVersion(VersionRefusal.EncodedWidth(8, long.size))), lying(8, long).keyOf(1L))
      .and(
        expect.same(
          Left(
            VersionedTrieError.InvalidVersion(
              VersionRefusal.Width(VersionKeyFormat.Configured, KeyMode.MaxFixedBytes + 1, KeyMode.MaxFixedBytes)
            )
          ),
          lying(KeyMode.MaxFixedBytes + 1, ByteVector.fill(KeyMode.MaxFixedBytes + 1L)(1)).keyOf(1L)
        )
      )
      .and(
        expect.same(
          Left(VersionedTrieError.InvalidVersion(VersionRefusal.Width(VersionKeyFormat.Configured, 0, KeyMode.MaxFixedBytes))),
          lying(0, ByteVector.empty).keyOf(1L)
        )
      )
      .and(expect.same(Right(ByteVector.fill(8)(1)), lying(8, ByteVector.fill(8)(1)).keyOf(1L)))
  }

  pureTest("a refusal names the key it is given in full: lowercase hex, no 0x, at the widest width") {
    val k = ByteVector((0 until KeyMode.MaxFixedBytes).map(i => (i * 7).toByte)*)
    val hex = versionHex(k)
    expect
      .same(2 * KeyMode.MaxFixedBytes, hex.length)
      .and(expect(hex.matches("([0-9a-f]{2})*"), hex.take(16)))
      .and(expect.same(Right(k), ByteVector.fromHexDescriptive(hex)))
  }

  pureTest("a snapshot's out-of-order versions are named by their full keys, at 32 and 1024 bytes") {
    List(32, KeyMode.MaxFixedBytes).foldMap { w =>
      implicit val key: VersionKey[ByteVector] = get(VersionKey.bytes(w))
      // keys that differ only in their last byte: a truncated rendering could not tell them apart
      val (k1, k2) = (ByteVector.fill(w.toLong)(0) :+ 1.toByte, ByteVector.fill(w.toLong)(0) :+ 2.toByte)
      val (a, b) = (k1.drop(1), k2.drop(1))
      val root = T.empty
      val head = VersionedTrie.Head[ByteVector, H](b, root, root)
      val read = Snapshot.read(head, List(SnapshotChunk.Top(head), SnapshotChunk.History(Vector(b -> root, a -> root))))
      expect.same(Left(VersionedTrieError.Malformed(MalformedReason.Snapshot(SnapshotRefusal.VersionsOutOfOrder(b, a)))), read)
    }
  }

  pureTest("the bytes key is its own version at every width from 1 to 1024, and refuses another length") {
    List(1, 8, 32, KeyMode.MaxFixedBytes).foldMap { w =>
      val key = get(VersionKey.bytes(w))
      val v = ByteVector.fill(w.toLong)(0x5a)
      expect
        .same(Right(v), key.keyOf(v))
        .and(expect.same(Right(v), key.decode(v)))
        .and(expect.same(Left(VersionedTrieError.InvalidVersion(VersionRefusal.EncodedWidth(w, w + 1L))), key.keyOf(v ++ ByteVector(0))))
    }.and(List(0, KeyMode.MaxFixedBytes + 1).foldMap { w =>
      expect.same(
        Left(VersionedTrieError.InvalidVersion(VersionRefusal.Width(VersionKeyFormat.Bytes, w, KeyMode.MaxFixedBytes))),
        VersionKey.bytes(w)
      )
    })
  }

  pureTest("unsigned Long versions retain exact width and range refusals") {
    val one = get(VersionKey.unsignedLong(1))
    val high = ByteVector(0x80.toByte) ++ ByteVector.fill(7)(0)
    expect
      .same(Left(VersionedTrieError.InvalidVersion(VersionRefusal.ValueRange(-1, 255))), one.encode(-1))
      .and(expect.same(Left(VersionedTrieError.InvalidVersion(VersionRefusal.ValueRange(256, 255))), one.encode(256)))
      .and(expect.same(Left(VersionedTrieError.InvalidVersion(VersionRefusal.EncodedWidth(1, 2))), one.decode(ByteVector(0, 1))))
      .and(
        expect.same(Left(VersionedTrieError.InvalidVersion(VersionRefusal.EncodedRange(high, Long.MaxValue))), VersionKey.long.decode(high))
      )
      .and(List(0, 9).foldMap { w =>
        expect.same(
          Left(VersionedTrieError.InvalidVersion(VersionRefusal.Width(VersionKeyFormat.UnsignedLong, w, 8))),
          VersionKey.unsignedLong(w)
        )
      })
      .and(expect.same(Right(ByteVector(0xff.toByte)), one.encode(255)))
  }
}
