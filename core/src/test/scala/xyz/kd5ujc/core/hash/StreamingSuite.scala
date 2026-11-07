package xyz.kd5ujc.core.hash

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.atomic.AtomicLong

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical._
import xyz.kd5ujc.core.vectors.CanonicalVectors

import org.bouncycastle.crypto.ExtendedDigest
import org.bouncycastle.crypto.digests.{Blake2bDigest, KeccakDigest, SHA256Digest, SHA3Digest, SHA512Digest}
import scodec.bits.ByteVector
import weaver.SimpleIOSuite
import weaver.scalacheck.{CheckConfig, Checkers}

/**
 * Encodings streamed into the hash equal the old path, which builds the whole encoding and hashes it in one call
 * ([[BufferedCbor]], the oracle, hashed straight through BouncyCastle), for vectors, edge cases and generated values
 * under every hash; and the hashing path holds a bounded window of the encoding, whatever the value's size.
 */
object StreamingSuite extends SimpleIOSuite with Checkers {

  override def checkConfig: CheckConfig = CheckConfig.default.withMinimumSuccessful(300)

  final private case class Algo[H](hasher: Hasher[H], make: () => ExtendedDigest)

  private val algos: List[Algo[?]] = List(
    Algo(Hasher.sha256, () => new SHA256Digest()),
    Algo(Hasher.sha512, () => new SHA512Digest()),
    Algo(Hasher.sha3_256, () => new SHA3Digest(256)),
    Algo(Hasher.sha3_512, () => new SHA3Digest(512)),
    Algo(Hasher.keccak256, () => new KeccakDigest(256)),
    Algo(Hasher.blake2b256, () => new Blake2bDigest(256)),
    Algo(Hasher.blake2b512, () => new Blake2bDigest(512))
  )

  private val tag = DomainTag("dilf4s/test/streaming/v1").fold(e => sys.error(e.toString), identity)

  /** The whole message hashed in one `update`, as `Hasher.digest` did before streaming. */
  private def oneShot(make: () => ExtendedDigest, message: ByteVector): ByteVector = {
    val d = make()
    val in = message.toArray
    val out = new Array[Byte](d.getDigestSize)
    d.update(in, 0, in.length)
    d.doFinal(out, 0)
    ByteVector.view(out)
  }

  private def oracleTagged(make: () => ExtendedDigest, message: ByteVector): ByteVector = {
    val t = oneShot(make, ByteVector(tag.value.getBytes(UTF_8)))
    oneShot(make, t ++ t ++ message)
  }

  /** A test value's hash or source: every one is far within `Cbor.MaxEncodedBytes`, so a refusal fails the test. */
  private def ok[A](e: Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)

  private def same(name: String, v: CanonicalValue): weaver.Expectations = {
    val buffered = BufferedCbor.encode(v)
    algos.foldLeft(expect(xyz.kd5ujc.core.canonical.CanonicalGens.encoded(v) == buffered, s"$name: the encoding")) { (acc, a) =>
      def check[H](a: Algo[H]): weaver.Expectations = {
        implicit val H: Hasher[H] = a.hasher
        val t = TaggedHash[H](tag)
        expect(ok(t.value(v)).bytes == oracleTagged(a.make, buffered), s"$name: content hash under ${H.name}")
          .and(expect(H.digestOf(ok(Cbor.source(v))).bytes == oneShot(a.make, buffered), s"$name: plain hash under ${H.name}"))
          .and(expect(H.digest(buffered).bytes == oneShot(a.make, buffered), s"$name: digest of bytes under ${H.name}"))
      }
      acc.and(check(a))
    }
  }

  private def txt(s:   String): CanonicalValue = CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)
  private def arr(vs:  Vector[CanonicalValue]): CanonicalValue = CanonicalValue.array(vs).fold(e => sys.error(e.toString), identity)
  private def obj(kvs: List[(String, CanonicalValue)]): CanonicalValue = CanonicalValue.map(kvs).fold(e => sys.error(e.toString), identity)

  private def nested(depth: Int, leaf: CanonicalValue, maps: Boolean): CanonicalValue =
    (1 until depth).foldLeft(leaf)((v, i) => if (maps && i % 2 == 0) obj(List("k" -> v)) else arr(Vector(v)))

  private val edgeCases: List[(String, CanonicalValue)] = {
    val boundaries: List[BigInt] = List(
      BigInt(0),
      BigInt(23),
      BigInt(24),
      BigInt(255),
      BigInt(256),
      BigInt(65535),
      BigInt(65536),
      (BigInt(1) << 32) - 1,
      BigInt(1) << 32,
      (BigInt(1) << 63) - 1,
      BigInt(1) << 63,
      (BigInt(1) << 64) - 1
    )
    val lengths = List(
      23,
      24,
      255,
      256,
      Hasher.Window - 1,
      Hasher.Window,
      Hasher.Window + 1,
      Cbor.TextWindow - 1,
      Cbor.TextWindow,
      Cbor.TextWindow + 1,
      65535,
      65536
    )
    val bignums = List(64, 65, 127, 128, 129, 256, 2051).flatMap { bits =>
      val p = BigInt(1) << bits
      List(p - 1, p, p + 1, -p - 1, -p, -p + 1)
    }
    List(
      "null"           -> CNull,
      "false"          -> False,
      "true"           -> True,
      "empty text"     -> txt(""),
      "empty bytes"    -> CBytes(ByteVector.empty),
      "empty array"    -> EmptyArray,
      "empty map"      -> EmptyMap,
      "non-ascii text" -> txt("é😀￿"),
      "rational" -> CanonicalValue
        .number(xyz.kd5ujc.numerics.Ratio.of(-7, 3).fold(e => sys.error(e.toString), identity))
        .fold(e => sys.error(e.toString), identity),
      "deep arrays"          -> nested(CanonicalValue.MaxDepth, CInt(1), maps = false),
      "deep maps and arrays" -> nested(CanonicalValue.MaxDepth, txt("leaf"), maps = true),
      "keys of every length order" -> obj(List("", "a", "b", "aa", "z", "zz", "é", "😀", "￿", "").zipWithIndex.map {
        case (k, i) => k -> CInt(i)
      }),
      // a surrogate pair straddling the text window's edge: the window must not split it
      "pair at the window edge" -> txt("a" * (Cbor.TextWindow - 1) + "😀" + "b"),
      "pairs throughout"        -> txt("😀" * (Cbor.TextWindow + 3))
    ) ++
    boundaries.flatMap(n => List(s"int $n" -> CanonicalGens.int(n), s"int -1-$n" -> CanonicalGens.int(-1 - n))) ++
    lengths.flatMap(n =>
      List(
        s"text of $n"  -> txt("t" * n),
        s"bytes of $n" -> CBytes(ByteVector.fill(n.toLong)(7)),
        s"array of $n" -> arr(Vector.fill(n)(CNull))
      )
    ) ++
    bignums.map(n => s"bignum $n" -> CanonicalGens.int(n))
  }

  edgeCases.foreach { case (name, v) => pureTest(s"streamed equals buffered: $name")(same(name, v)) }

  pureTest("streamed equals buffered for every canonical vector") {
    val json = CanonicalVectors.jsonInputs.flatMap(in => CanonicalJson.parse(in).toOption.map(in.take(40) -> _))
    val all = json ++ CanonicalVectors.modelInputs
    all.foldLeft(expect(all.nonEmpty))((acc, c) => acc.and(same(c._1, c._2)))
  }

  test("streamed equals buffered for generated values") {
    forall(CanonicalGens.value(5))(v => same(v.toString.take(60), v))
  }

  // ---- the bound

  /** A BouncyCastle digest that counts what reaches it: the bytes, and the longest single `update`. */
  final private class Counting(inner: ExtendedDigest) extends ExtendedDigest {
    val total = new AtomicLong(0L)
    val longest = new AtomicLong(0L)
    val calls = new AtomicLong(0L)
    def getAlgorithmName: String = inner.getAlgorithmName
    def getDigestSize: Int = inner.getDigestSize
    def getByteLength: Int = inner.getByteLength
    def update(in: Byte): Unit = {
      total.incrementAndGet()
      inner.update(in)
    }
    def update(in: Array[Byte], off: Int, len: Int): Unit = {
      total.addAndGet(len.toLong)
      longest.accumulateAndGet(len.toLong, math.max)
      calls.incrementAndGet()
      inner.update(in, off, len)
    }
    def doFinal(out: Array[Byte], off: Int): Int = inner.doFinal(out, off)
    def reset(): Unit = inner.reset()
  }

  /**
   * What the walk itself hands a sink: the total, and the longest piece other than a byte string of the value, which
   * goes to the sink as it is. Pure: the state is the counts.
   */
  final private case class Pieces(total: Long, longestMade: Long)

  private def piecesSink(lent: List[ByteVector]): ByteSink[Pieces] = new ByteSink[Pieces] {
    def byte(s: Pieces, b: Int): Pieces = Pieces(s.total + 1, math.max(s.longestMade, 1L))
    def bytes(s: Pieces, b: FlatBytes): Pieces =
      Pieces(s.total + b.size, if (lent.exists(_ eq b)) s.longestMade else math.max(s.longestMade, b.size))
  }

  private def structures(n: Int): List[(String, CanonicalValue, List[ByteVector])] =
    List(
      ("wide", arr(Vector.tabulate(n / 10)(i => CanonicalGens.int(BigInt(i) << 40))), Nil),
      ("keyed", obj(List.tabulate(n / 20)(i => s"k$i" -> CBool(i % 2 == 0))), Nil),
      ("deep", nested(CanonicalValue.MaxDepth, arr(Vector.fill(n / 10)(txt("x"))), maps = true), Nil)
    )

  private def strings(n: Int): List[(String, CanonicalValue, List[ByteVector])] = {
    val bytes = ByteVector.fill(n.toLong)(0x5a)
    val text = txt("é😀" * (n / 6))
    List(
      ("text", text, Nil),
      ("bytes", CBytes(bytes), List(bytes)),
      ("both in an array", arr(Vector(text, CBytes(bytes))), List(bytes))
    )
  }

  private val bounded: List[(String, List[(String, CanonicalValue, List[ByteVector])])] = List(
    "structures of about 1000 bytes"   -> structures(1000),
    "structures of about 100000 bytes" -> structures(100000),
    "strings of about 1000 bytes"      -> strings(1000),
    "strings of about 10000000 bytes"  -> strings(10000000)
  )

  bounded.foreach {
    case (what, cases) =>
      test(s"the hashing path holds a bounded window of $what") {
        cases.traverse {
          case (name, v, lent) =>
            IO(new Counting(new SHA256Digest())).map { counting =>
              val H = Hasher.bouncyCastle[Sha256]("SHA-256", () => counting)
              val streamed = H.digestOf(ok(Cbor.source(v)))
              val length = Cbor.encodedLength(v)
              val pieces = ok(Cbor.source(v)).writeTo(Pieces(0L, 0L))(using piecesSink(lent))
              expect(counting.total.get == length, s"$name: every byte absorbed once")
                .and(
                  expect(counting.longest.get <= Hasher.Window.toLong, s"$name: updates of at most the window, got ${counting.longest.get}")
                )
                .and(expect(pieces.total == length, s"$name: the walk writes every byte"))
                .and(
                  expect(
                    pieces.longestMade <= 3L * Cbor.TextWindow,
                    s"$name: the walk makes pieces of at most a text window, got ${pieces.longestMade}"
                  )
                )
                .and(expect(streamed.bytes == oneShot(() => new SHA256Digest(), BufferedCbor.encode(v)), s"$name: the digest"))
            }
        }.map(_.combineAll)
      }
  }

  test("a bignum's magnitude is the one piece the walk copies whole, and the hash still takes it in windows") {
    // the widest integer: 4153 bytes of magnitude, past the hash's window
    val v = CanonicalGens.int((BigInt(1) << CanonicalValue.MaxIntegerBits) - 1)
    IO(new Counting(new SHA256Digest())).map { counting =>
      val streamed = Hasher.bouncyCastle[Sha256]("SHA-256", () => counting).digestOf(ok(Cbor.source(v)))
      val pieces = ok(Cbor.source(v)).writeTo(Pieces(0L, 0L))(using piecesSink(Nil))
      expect
        .same(4153L, pieces.longestMade)
        .and(expect(counting.longest.get <= Hasher.Window.toLong))
        .and(expect(streamed.bytes == oneShot(() => new SHA256Digest(), BufferedCbor.encode(v))))
    }
  }

  /** A byte string of `chunks` separate chunks of `size` bytes, as `cat` in a JLVM program builds one. */
  private def fragmented(chunks: Int, size: Int): ByteVector =
    (0 until chunks).foldLeft(ByteVector.empty)((acc, i) => acc ++ ByteVector.fill(size.toLong)(i.toByte))

  /** Generated chunks (`ByteVector.fill`), one per `sizes` entry, between array-backed ones. */
  private def generated(sizes: List[Long]): ByteVector =
    sizes.zipWithIndex.foldLeft(ByteVector(0x05)) { case (acc, (n, i)) => acc ++ ByteVector.fill(n)(i + 1) ++ ByteVector(0x06) }

  test("a byte string of many chunks is visited once, absorbed a full window at a time") {
    val cases = List((8192, 512), (4096, 100), (100000, 1), (20000, 37), (64, 70000), (3, 1)).map {
      case (c, n) => (s"$c x $n", fragmented(c, n))
    } ++
      List(
        "generated"                 -> generated(List(1L, 511L, 512L, 513L, 1L << 20, 3L)),
        "a populated nested buffer" -> (ByteVector(3) ++ (ByteVector(3).bufferBy(1 << 20) ++ ByteVector.view(Array.fill[Byte](300000)(4))))
      )
    cases.traverse {
      case (name, chunked) =>
        val b = ByteVector(0x01, 0x02) ++ chunked
        val source = ByteSource.byte(0x7f) ++ ByteSource.bytes(b) ++ ByteSource.byte(0x7e)
        IO(new Counting(new SHA256Digest())).map { counting =>
          val streamed = Hasher.bouncyCastle[Sha256]("SHA-256", () => counting).digestOf(source)
          val total = b.size + 2
          val windows = (total + Hasher.Window - 1) / Hasher.Window
          expect(streamed.bytes == oneShot(() => new SHA256Digest(), ByteVector(0x7f) ++ b ++ ByteVector(0x7e)), s"$name: the digest")
            .and(expect(counting.total.get == total, s"$name: every byte once"))
            .and(expect(counting.longest.get <= Hasher.Window.toLong, s"$name: windows at most"))
            .and(expect(counting.calls.get <= windows, s"$name: ${counting.calls.get} updates for $windows windows"))
        }
    }.map(_.combineAll)
  }

  /** The digest of `n` bytes of `b`, fed to BouncyCastle a mebibyte at a time: independent of the code under test. */
  private def oracleFill(n: Long, b: Byte): ByteVector = {
    val d = new SHA256Digest()
    val block = Array.fill[Byte](1 << 20)(b)
    @scala.annotation.tailrec
    def go(left: Long): Unit =
      if (left > 0) {
        val k = math.min(left, block.length.toLong).toInt
        d.update(block, 0, k)
        go(left - k)
      }
    go(n)
    val out = new Array[Byte](32)
    d.doFinal(out, 0)
    ByteVector.view(out)
  }

  test("one generated chunk longer than Int.MaxValue hashes in windows, without materialising it") {
    val n = (1L << 31) + 3
    IO(new Counting(new SHA256Digest())).map { counting =>
      val streamed = Hasher.bouncyCastle[Sha256]("SHA-256", () => counting).digestOf(ByteSource.bytes(ByteVector.fill(n)(7)))
      expect(streamed.bytes == oracleFill(n, 7), "the digest of 2^31 + 3 sevens")
        .and(expect(counting.total.get == n, "every byte once"))
        .and(expect(counting.longest.get <= Hasher.Window.toLong, "windows at most"))
    }
  }

  /** The bytes this thread allocates running `f` (the JDK's supported management API), with its result. */
  private def allocated[A](f: => A): (A, Long) =
    java.lang.management.ManagementFactory.getThreadMXBean match {
      case mx: com.sun.management.ThreadMXBean =>
        val before = mx.getCurrentThreadAllocatedBytes
        val a = f
        (a, mx.getCurrentThreadAllocatedBytes - before)
      case _ => (f, -1L)
    }

  /** A rope of `n` one-byte chunks built by prepending, as `x ++ acc` in a loop builds one: as deep as it is long. */
  private def prepended(n: Int): ByteVector = (0 until n).foldLeft(ByteVector.empty)((acc, i) => ByteVector(i.toByte) ++ acc)

  /**
   * Byte strings built every way a host, a decoder or a program can build them, each about `n` bytes. As canonical
   * values they are flat ([[FlatBytes]]), whatever they were built from.
   */
  private def builtEveryWay(n: Int): List[(String, CanonicalValue)] = {
    val encoded = BufferedCbor.encode(CBytes(ByteVector.fill(n.toLong)(4)))
    val decodedFromRope = Cbor.decode(encoded.grouped(1000L).foldLeft(ByteVector.empty)(_ ++ _))
    List(
      "an array"                     -> CBytes(ByteVector.view(Array.fill[Byte](n)(1))),
      "a slice of an array"          -> CBytes(ByteVector.view(Array.fill[Byte](n + 64)(1)).drop(32).take(n.toLong)),
      "a lazy fill"                  -> CBytes(ByteVector.fill(n.toLong)(2)),
      "a rope built by prepending"   -> CBytes(prepended(math.min(n, 1 << 18))),
      "a rope built by appending"    -> CBytes(fragmented(n / 512, 512)),
      "a populated buffer"           -> CBytes(ByteVector(3).bufferBy(n) ++ ByteVector.view(Array.fill[Byte](n - 1)(3))),
      "a buffer inside an append"    -> CBytes(ByteVector(9) ++ (ByteVector(3).bufferBy(n) ++ ByteVector.view(Array.fill[Byte](n - 2)(3)))),
      "concatenated once (JLVM cat)" -> CBytes(FlatBytes.concat(Vector.fill(64)(ByteVector.fill((n / 64).toLong)(5)))),
      "decoded from a rope buffer"   -> decodedFromRope.fold(e => sys.error(e.toString), identity)
    )
  }

  List(1 << 12, 1 << 22).foreach { n =>
    test(s"a value's content hash copies none of its bytes, however they were built (about $n bytes)") {
      builtEveryWay(n).traverse {
        case (name, v) =>
          IO {
            val t = TaggedHash[Sha256](tag)
            val warm = ok(t.value(v))
            val (d, bytes) = allocated(ok(t.value(v)))
            val wrapped = arr(Vector(v, v))
            val (_, inArray) = allocated(ok(t.value(wrapped)))
            // no copy of the bytes: the window, and a few small objects per window copied (scodec-bits slices the chunk)
            val size = BufferedCbor.encode(v).size
            val limit = (64L << 10) + size / 4
            expect(d == warm, s"$name: the same digest twice")
              .and(expect(d.bytes == oracleTagged(() => new SHA256Digest(), BufferedCbor.encode(v)), s"$name: the digest"))
              .and(expect(bytes >= 0 && bytes <= limit, s"$name: allocated $bytes bytes, at most $limit"))
              .and(
                expect(inArray >= 0 && inArray <= 2 * limit, s"$name, twice in an array: allocated $inArray bytes, at most ${2 * limit}")
              )
          }
      }.map(_.combineAll)
    }
  }

  pureTest("a canonical byte string is flat however it was built, and an already-flat one is kept, not copied") {
    val array = ByteVector.view(Array.fill[Byte](1000)(1))
    builtEveryWay(1 << 12).foldLeft(expect(CBytes(array).value eq array, "an array is kept as it is")) {
      case (acc, (name, CBytes(b))) => acc.and(expect(FlatBytes.isFlat(b), s"$name: flat"))
      case (acc, (name, other))     => acc.and(failure(s"$name: not bytes: $other"))
    }
  }

  test("ordering a map's keys encodes none of them: a map hashes with what its keys, as plain texts, would take") {
    // A text is converted a window at a time (TextWindow characters), so what hashing one allocates is a few windows'
    // worth of short-lived pieces, never the text whole; a map's keys must add nothing to that to be ordered.
    val key = "\u00e9\uD83D\uDE00k" * (1 << 20)
    val keys = List(key, "a", key + "x")
    val asMap = obj(keys.zipWithIndex.map { case (k, i) => k -> CInt(i) })
    val asTexts = arr(keys.zipWithIndex.flatMap { case (k, i) => List(txt(k), CInt(i)) }.toVector)
    IO {
      val t = TaggedHash[Sha256](tag)
      val (_, _) = (ok(t.value(asMap)), ok(t.value(asTexts)))
      val (d, map) = allocated(ok(t.value(asMap)))
      val (_, texts) = allocated(ok(t.value(asTexts)))
      val limit = texts + texts / 10 + (64L << 10)
      expect(d.bytes == oracleTagged(() => new SHA256Digest(), BufferedCbor.encode(asMap)), "the digest")
        .and(expect(map >= 0 && map <= limit, s"the map allocated $map bytes, its keys as texts $texts"))
    }
  }

  test("a content hash past its limit is refused before the hash absorbs a byte: from the weight, or measured") {
    // [x, x] nested 70 deep weighs 2^63 - 1 (saturated); a host's lazy byte string of Long.MaxValue bytes too
    val heavy = (1 to 70).foldLeft(txt("abc"))((v, _) => arr(Vector(v, v)))
    val lazyBytes = CBytes(ByteVector.low(Long.MaxValue))
    val long = txt("a" * 1000) // weight 1001, encoding 1003 bytes
    val ceiling = Left(Cbor.TooLarge(Cbor.MaxEncodedBytes))
    IO(new Counting(new SHA256Digest())).map { counting =>
      val H = Hasher.bouncyCastle[Sha256]("SHA-256", () => counting)
      val t = TaggedHash[Sha256](tag)(using H)
      def absorbed[A](f: => A): (A, Long) = {
        val before = counting.total.get
        val a = f
        (a, counting.total.get - before)
      }
      val prefix = 2L * H.outputSize
      expect
        .same((ceiling, 0L), absorbed(t.value(heavy)))
        .and(expect.same((ceiling, 0L), absorbed(t.value(lazyBytes))))
        .and(expect.same((Left(Cbor.TooLarge(1000L)), 0L), absorbed(t.valueWithin(long, 1000L))))
        .and(expect.same((Left(Cbor.TooLarge(1002L)), 0L), absorbed(t.valueWithin(long, 1002L))))
        .and(expect.same(1003L + prefix, absorbed(t.valueWithin(long, 1003L))._2))
        .and(expect.same(Right(oracleTagged(() => new SHA256Digest(), BufferedCbor.encode(long))), t.valueWithin(long, 1003L).map(_.bytes)))
        .and(expect.same(Left(Cbor.TooLarge(0L)), Cbor.sourceWithin(CNull, -1L).map(_ => ())))
        .and(expect.same(ceiling, Cbor.source(heavy).map(_ => ())))
    }
  }
}
