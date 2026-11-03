package xyz.kd5ujc.core.hash

import java.util.concurrent.atomic.AtomicLong

import cats.effect.IO

import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/**
 * The digests' byte order, which keys every map of digests (never their 32-bit hash codes): unsigned bytewise
 * lexicographic, consistent with equality, and read in place up to the first difference.
 */
object DigestOrderSuite extends SimpleIOSuite with Checkers {

  private val order = Digest.digestOrdering[Sha256]
  private def digest(b: ByteVector): Digest[Sha256] = Digest.fromBytes[Sha256](b).fold(e => sys.error(e.toString), identity)

  // digests sharing prefixes often, so the comparison reaches past the first byte
  private val bytes: Gen[ByteVector] = Gen.listOfN(32, Gen.oneOf(0x00, 0x01, 0x7f, 0x80, 0xff)).map(bs => ByteVector(bs.map(_.toByte)))
  implicit private val show: cats.Show[ByteVector] = cats.Show.show(_.toHex)

  test("the order is the unsigned order of the bytes (their hex), and 0 exactly when the digests are equal") {
    // b is a itself, another digest, or a with one byte's top bit flipped (where signed and unsigned orders disagree)
    val pairs =
      bytes.flatMap(a => Gen.oneOf(Gen.const(a), bytes, Gen.choose(0L, 31L).map(i => a.update(i, (a(i) ^ 0x80).toByte))).map(a -> _))
    forall(pairs) {
      case (a, b) =>
        val c = order.compare(digest(a), digest(b))
        expect
          .same(Integer.signum(a.toHex.compareTo(b.toHex)), Integer.signum(c))
          .and(expect.same(digest(a) == digest(b), c == 0))
          .and(expect.same(-Integer.signum(c), Integer.signum(order.compare(digest(b), digest(a)))))
    }
  }

  test("a comparison reads each digest in place, only up to the first difference") {
    forall(Gen.zip(bytes, Gen.choose(0, 31))) {
      case (a, at) =>
        val b = a.update(at.toLong, (a(at.toLong) ^ 0x01).toByte)
        IO {
          val reads = new AtomicLong
          def counted(v: ByteVector) = digest(ByteVector.viewAt(i => { reads.incrementAndGet(); v(i) }, v.size))
          val c = order.compare(counted(a), counted(b))
          val same = order.compare(counted(a), counted(a))
          expect
            .same(Integer.signum(a.toHex.compareTo(b.toHex)), Integer.signum(c))
            .and(expect.same(0, same))
            .and(expect.same(2L * (at + 1L) + 64L, reads.get))
        }
    }
  }
}
