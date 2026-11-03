package xyz.kd5ujc.jlvm

import java.util.concurrent.atomic.AtomicLong

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}

import scodec.bits.ByteVector
import weaver.IOSuite

/**
 * Byte strings are flat (issue #96, docs/specs/jlvm.md §5.5). scodec's `ByteVector` is a rope, and a program that
 * prepends in a loop used to build one about `sqrt(2 × gas)` levels deep: every later read of a byte then walked, and
 * recursed through, three frames a level, work no size charge priced (an absent-needle `in` on a deep rope, doubled
 * nine times, took minutes on 2^24 gas). Nothing here measures time: the work an access does is read off the host stack
 * at a counting leaf, and stack safety off a thread with a small stack.
 */
object FlatBytesSuite extends IOSuite {

  type Res = Jlvm
  def sharedResource: Resource[IO, Jlvm] = Resource.eval(Jlvm.load[IO])

  private def get[A](e: Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)
  private def arr(vs:   Vector[CanonicalValue]): CanonicalValue = get(CanonicalValue.array(vs))
  private def obj(kvs:  (String, CanonicalValue)*): CanonicalValue = get(CanonicalValue.map(kvs.toVector))

  private def eval(jlvm: Jlvm, json: String, data: CanonicalValue): Outcome =
    jlvm.evaluate(get(Parser.parse(json)), data, Map.empty, Config())

  private val one: CanonicalValue = CBytes(ByteVector(1))

  /** Prepends each element of `xs` to `init`: the deepest rope a program can build per unit of gas. */
  private val prepend = """{"reduce": [{"var": "xs"}, {"cat": [{"var": "current"}, {"var": "accumulator"}]}, {"var": "init"}]}"""

  /** Reads every byte of the prepended bytes, looking for one that is not there. */
  private val search = s"""{"in": ["0x02", $prepend]}"""

  /** Compares two equal byte strings of the same depth, built apart, byte by byte. */
  private val compare = s"""{"==": [{"cat": ["0x01", $prepend]}, {"cat": [$prepend, "0x01"]}]}"""

  private def prepending(d: Int, leaf: => CanonicalValue = one): CanonicalValue =
    obj("xs" -> arr(Vector.fill(d)(leaf)), "init" -> leaf)

  /** The most elements `program` prepends within the default limit: the deepest it can reach. */
  private def deepest(jlvm: Jlvm, program: String): Int = {
    @tailrec
    def go(lo: Int, hi: Int): Int =
      if (lo >= hi) lo
      else {
        val mid = (lo + hi + 1) / 2
        if (eval(jlvm, program, prepending(mid)).result.isRight) go(mid, hi) else go(lo, mid - 1)
      }
    go(1, 1 << 15)
  }

  private def isFlat(v: CanonicalValue): Boolean = v match {
    case CBytes(b) => FlatBytes.isFlat(b)
    case _         => false
  }

  /** A one-byte leaf that counts its reads and records the deepest host stack (in frames) it was read from. */
  private def counting(reads: AtomicLong, frames: AtomicLong): CanonicalValue =
    CBytes(
      ByteVector.viewAt(
        _ => {
          reads.incrementAndGet()
          frames.accumulateAndGet(StackWalker.getInstance().walk(_.count()), math.max)
          1.toByte
        },
        1
      )
    )

  /** Runs `thunk` on a fresh thread with a `kb` KB stack: its value, or what it threw (a stack overflow). */
  private def onStack[A](kb: Long)(thunk: => A): IO[Either[Throwable, A]] =
    IO.async_ { cb =>
      val t = new Thread(null, () => cb(Right(Right(thunk))), "small-stack", kb << 10)
      t.setUncaughtExceptionHandler((_, e) => cb(Right(Left(e))))
      t.start()
    }

  /** Before the fix, reading the deepest rope recursed ~16,800 frames: a 128 KB stack overflowed, 256 KB did not. */
  private val SmallStackKb = 128L

  test("a prepend loop at the deepest the default limit allows builds one flat chunk, every cat paid for") { jlvm =>
    IO {
      val d = deepest(jlvm, search)
      val o = eval(jlvm, prepend, prepending(d))
      // the k-th cat copies the element and the k bytes before it
      val copied = (1L to d.toLong).map(_ + 1).sum
      // 5,604 on schedule v1: a loose bound, so repricing cat does not fail it
      expect(d >= 1000, s"the default limit reaches $d elements")
        .and(expect(o.result.exists(isFlat), s"the result is one chunk: ${o.result.map(_.weight)}"))
        .and(expect(o.result.exists(_.weight == d + 2L), s"the result holds every byte: ${o.result.map(_.weight)}"))
        .and(expect(copied <= o.gasUsed.value, s"the cats copied $copied bytes, within the ${o.gasUsed.value} gas paid"))
    }
  }

  test("the work of reading a byte does not grow with how many times a program prepended") { jlvm =>
    IO {
      val deep = deepest(jlvm, search)
      def probe(d: Int): (Outcome, Long, Long) = {
        val (reads, frames) = (new AtomicLong, new AtomicLong)
        val o = eval(jlvm, search, prepending(d, counting(reads, frames)))
        (o, reads.get, frames.get)
      }
      val (shallow, shallowReads, shallowFrames) = probe(16)
      val (deepOutcome, deepReads, deepFrames) = probe(deep)
      // before the fix, a leaf under d prepends was read from 3 more frames per level: ~16,800 at the deepest
      expect
        .same(Right(False), shallow.result)
        .and(expect.same(Right(False), deepOutcome.result))
        .and(expect.same(17L, shallowReads))
        .and(expect.same(deep + 1L, deepReads))
        .and(expect(deepFrames == shallowFrames, s"$deep prepends read a leaf from $deepFrames frames, 16 from $shallowFrames"))
    }
  }

  test(s"every byte-reading operation reads the deepest bytes a program can build on a $SmallStackKb KB stack") { jlvm =>
    IO((deepest(jlvm, search), deepest(jlvm, compare))).flatMap {
      case (d, e) =>
        // one element fewer than `in` affords leaves room for an operation's larger base charge
        val reads =
          List("in" -> search) ++ List("hex", "sha256", "keccak256", "bytes_to_int", "length").map(op => op -> s"""{"$op": $prepend}""")
        val runs = reads.map { case (name, p) => (name, p, d - 1) } :+ (("==", compare, e))
        runs.traverse {
          case (name, p, n) =>
            onStack(SmallStackKb)(eval(jlvm, p, prepending(n)).result).map { r =>
              val finished = r.exists(
                _.fold(
                  {
                    case JlvmError.OutOfGas(_, _) => false
                    case _                        => true
                  },
                  _ => true
                )
              )
              expect(finished, s"$name over $n prepends: ${r.leftMap(_.getClass.getSimpleName)}")
            }
        }.map(_.combineAll)
    }
  }

  test("host bytes that are a deep rope are flattened, under the reading operation's size charge, before they are read") { jlvm =>
    // the depth a prepend loop of 5,000 gives, built by the host
    val rope = CBytes((1 to 5000).foldLeft(ByteVector(1))((acc, _) => ByteVector(1) ++ acc))
    val programs = List(
      """{"in": ["0x02", {"var": "r"}]}""",
      """{"hex": {"var": "r"}}""",
      """{"sha256": {"var": "r"}}""",
      """{"unhex": {"var": "r"}}""",
      """{"cat": [{"var": "r"}, "0x01"]}"""
    )
    programs
      .traverse(p => onStack(SmallStackKb)(eval(jlvm, p, obj("r" -> rope)).result).map(p -> _))
      .map { results =>
        val all = results.foldMap {
          case (p, r) => expect(r.exists(_.isRight), s"$p: ${r.leftMap(_.getClass.getSimpleName)}")
        }
        val made = results.drop(3).foldMap { case (p, r) => expect(r.exists(_.exists(isFlat)), s"$p gives flat bytes") }
        all.and(made)
      }
  }

  test("every operation that makes bytes makes them flat, from flat and from rope operands") { _ =>
    val rope = CBytes(ByteVector(1, 2) ++ ByteVector(3) ++ ByteVector(4, 5))
    val flat = CBytes(ByteVector(1, 2, 3))
    def run(op: Op, args: CanonicalValue*) = PureOps(op, args.toVector, Kernels.Default).getOrElse(Left(JlvmError.Malformed(op.wire)))
    val made = List(
      "cat of one"          -> run(Op.Cat, rope),
      "cat appending"       -> run(Op.Cat, flat, rope, flat),
      "cat prepending"      -> run(Op.Cat, flat, rope),
      "unhex of bytes"      -> run(Op.Unhex, rope),
      "unhex of text"       -> run(Op.Unhex, get(CanonicalValue.text("0x0102"))),
      "int_to_bytes"        -> run(Op.IntToBytes, CInt(BigInt(258))),
      "int_to_bytes [n, w]" -> run(Op.IntToBytes, CInt(BigInt(258)), CInt(BigInt(32))),
      "sha256"              -> run(Op.Sha256, rope),
      "keccak256 of text"   -> run(Op.Keccak256, get(CanonicalValue.text("a")))
    )
    // a copy is one whole array, read in place, never a lazy view over its operands
    def wholeArray(v: CanonicalValue): Boolean = v match {
      case CBytes(b) => b.toArrayUnsafe eq b.toArrayUnsafe
      case _         => false
    }
    val copies = made.filter { case (name, _) => name.startsWith("cat") || name == "unhex of bytes" || name == "sha256" }
    IO.pure(
      made.foldMap { case (name, r) => expect(r.exists(isFlat), s"$name: $r") }
        .and(copies.foldMap { case (name, r) => expect(r.exists(wholeArray), s"$name is one whole array") })
    )
  }

  test("a byte string's sort key is one whole array, so comparing two never copies either") { _ =>
    // a slice of a decoded buffer, and a rope: both used to be copied on every comparison of a sort
    val sources = List("slice" -> ByteVector(0, 1, 2, 3).slice(1, 3), "rope" -> (ByteVector(1) ++ ByteVector(2)))
    IO.pure(sources.foldMap {
      case (name, b) =>
        val key = Values.sortKey(CBytes(b))
        expect(key.encoding.toArrayUnsafe eq key.encoding.toArrayUnsafe, s"$name: the key's bytes are read in place")
          .and(expect.same(b, key.encoding))
    })
  }

  test("bytes decoded from a flat buffer are flat: CBOR data, and 0x literals and data") { jlvm =>
    val encoded = Cbor.encode(arr(Vector(CBytes(ByteVector(1, 2, 3)), obj("k" -> CBytes(ByteVector(4)))))).compact
    val decoded = Cbor.decode(encoded).map {
      case CArray(Vector(a, CMap(m))) => isFlat(a) && m.get("k").exists(isFlat)
      case _                          => false
    }
    val literal = eval(jlvm, "\"0x0102\"", CNull).result
    val data = JsonSurface.parse("\"0x0304\"")
    IO.pure(
      expect(decoded == Right(true), s"CBOR: $decoded")
        .and(expect(literal.exists(isFlat), s"a literal: $literal"))
        .and(expect(data.exists(isFlat), s"surface data: $data"))
    )
  }
}
