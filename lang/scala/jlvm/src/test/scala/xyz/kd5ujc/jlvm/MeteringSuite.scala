package xyz.kd5ujc.jlvm

import java.util.concurrent.atomic.AtomicLong

import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.accumulators.trie.{KeyMode, ProofNode, TrieHashing, TrieProof, TrieProofs}
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.hash.{Digest, Sha256}
import xyz.kd5ujc.numerics.Ratio

import scodec.bits.{BitVector, ByteVector}
import weaver.{Expectations, IOSuite}

/**
 * Denial-of-service regressions: every unit of work is charged before it is done and bounded by the gas limit. Nothing
 * here measures time, so a loaded machine cannot fail it. A test either reads the bound off the gas (the charge a piece
 * of work pays grows with its size), or counts the comparisons the evaluator makes through counting [[Kernels]] and
 * bounds them by the input's size. Names use comparison orders; value membership counts radix byte reads and branch
 * visits against key length. Neither hashes attacker-controlled input. How fast a unit of work runs is calibration, measured by the `benchmarks` GasRatios job.
 */
object MeteringSuite extends IOSuite {

  type Res = Jlvm
  def sharedResource: Resource[IO, Jlvm] = Resource.eval(Jlvm.load[IO])

  private val S = GasSchedule.V1

  private def get[A](e:     Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)
  private def t(s:          String): CanonicalValue = get(CanonicalValue.text(s))
  private def arr(vs:       Vector[CanonicalValue]): CanonicalValue = get(CanonicalValue.array(vs))
  private def obj(kvs:      (String, CanonicalValue)*): CanonicalValue = get(CanonicalValue.map(kvs.toVector))
  private def program(json: String): Program = get(Parser.parse(json))

  private def eval(jlvm: Jlvm, p: Program, data: CanonicalValue, limit: Long = GasLimit.Default.amount.value): Outcome =
    jlvm.evaluate(p, data, Map.empty, Config(limit = GasLimit(Gas(limit))))

  private def isLimit(r: Either[JlvmError, ?]): Boolean = r match {
    case Left(JlvmError.Limit(_)) => true
    case _                        => false
  }

  private def isOutOfGas(r: Either[JlvmError, ?]): Boolean = r match {
    case Left(JlvmError.OutOfGas(_, _)) => true
    case _                              => false
  }

  /** `2^k` distinct strings of length `2k` with one Java hash code: every combination of "Aa" and "BB". */
  private def colliding(k: Int): Vector[String] = combinations(k, "Aa", "BB")

  /** The control: as many strings, as long, from "Aa" and "Ac", whose hash codes differ. */
  private def control(k: Int): Vector[String] = combinations(k, "Aa", "Ac")

  private def combinations(k: Int, a: String, b: String): Vector[String] =
    (1 to k).foldLeft(Vector(""))((acc, _) => acc.flatMap(s => Vector(s + a, s + b)))

  /** `order`, counting its comparisons in `count`. */
  private def counted[A](order: Ordering[A], count: AtomicLong): Ordering[A] = (a, b) => {
    count.incrementAndGet()
    order.compare(a, b)
  }

  private def counting(names: AtomicLong): Kernels = Kernels.Default.copy(names = counted(Ordering.String, names))

  /** `ceil(log2 n)`. */
  private def log2(n: Int): Long = 64L - java.lang.Long.numberOfLeadingZeros(n.toLong - 1)

  private def comparing(jlvm: Jlvm, p: Program, data: CanonicalValue): IO[(Outcome, Long)] =
    IO(new AtomicLong).flatMap { names =>
      IO {
        val kernels = counting(names)
        val compiled = get(Compiled(p, kernels))
        val before = names.get
        val result = jlvm.evaluate(compiled, data, Map.empty, Config(), kernels)
        (result, names.get - before)
      }
    }

  private def compiling(json: String): IO[(Boolean, Long)] =
    IO(new AtomicLong).flatMap { names =>
      IO((Parser.parse(json).flatMap(Compiled(_, counting(names))).isRight, names.get))
    }

  // ---- 1. join

  test("join charges its output, separators included, before building it") { jlvm =>
    val p = program("""{"join": [{"var": "xs"}, {"var": "sep"}]}""")
    val fits = eval(jlvm, p, obj("xs" -> arr(Vector.fill(1000)(CNull)), "sep" -> t("x" * 1000)))
    val outputWeight = fits.result.map(_.weight).getOrElse(0L)
    val oversized = eval(jlvm, p, obj("xs" -> arr(Vector.fill(5000)(CNull)), "sep" -> t("x" * 5000)))
    IO.pure(
      expect(fits.result.isRight, s"a join of weight about 10^6 fits: ${fits.result.leftMap(_.toString)}")
        .and(
          expect(
            fits.gasUsed.value >= S.cost(Op.Join).perUnit.value * outputWeight,
            s"gas ${fits.gasUsed.value} pays for the output's weight $outputWeight"
          )
        )
        .and(expect(isLimit(oversized.result), s"25 MB of separators is Limit: ${oversized.result}"))
        .and(
          expect(
            oversized.gasUsed.value < S.cost(Op.Join).perUnit.value * 4999L * 5000L,
            s"rejected before its output is charged: ${oversized.gasUsed.value}"
          )
        )
    )
  }

  test("cat and join charge for rendering numbers, which can be far longer than their weight") { jlvm =>
    val fifth = CanonicalValue.number(get(Ratio.of(BigInt(1), BigInt(5).pow(1764)))).fold(e => sys.error(e.toString), identity)
    val half = CanonicalValue.number(get(Ratio.of(BigInt(1), BigInt(1) << 4095))).fold(e => sys.error(e.toString), identity)
    val cat = program("""{"cat": {"var": "xs"}}""")
    val join = program("""{"join": [{"var": "xs"}, ""]}""")
    // the rendering charge grows with each number's decimal expansion ((6 + bits/64)^2 units), and is taken before it
    def rendersPaid(p: Program, v: CanonicalValue): Expectations = {
      val numbers = Vector.fill(20)(v)
      val o = eval(jlvm, p, obj("xs" -> arr(numbers)))
      val out = o.result.map(_.weight).getOrElse(0L)
      expect(o.result.isRight, s"renders: ${o.result.leftMap(_.toString)}")
        .and(expect(o.gasUsed.value >= out, s"at least 1 gas per output character: ${o.gasUsed.value} for $out"))
        .and(expect(S.rendered(numbers) <= o.gasUsed, s"pays the rendering charge ${S.rendered(numbers)}: ${o.gasUsed}"))
    }
    val ints = Vector.tabulate(1000)(i => CInt(i * 1000003L))
    val words = arr(Vector.fill(1000)(t("w")))
    val gasInts = eval(jlvm, cat, obj("xs" -> arr(ints))).gasUsed
    val gasWords = eval(jlvm, cat, obj("xs" -> words)).gasUsed
    IO(
      rendersPaid(cat, fifth)
        .and(rendersPaid(cat, half))
        .and(rendersPaid(join, fifth))
        .and(expect(S.rendered(ints) <= gasInts, s"1000 small integers pay their rendering ${S.rendered(ints)}: $gasInts"))
        .and(expect(gasInts.value > gasWords.value, s"1000 rendered integers ($gasInts) cost more than 1000 one-letter texts ($gasWords)"))
    )
  }

  // ---- 2. variadic arithmetic

  test("+ and * bound every intermediate, in the strict and the spread form") { jlvm =>
    val spread = program("""{"*": {"var": "xs"}}""")
    val twos = obj("xs" -> arr(Vector.fill(10000)(CInt(2)) :+ CInt(0)))
    val strict = program(s"""{"*": [${List.fill(5000)("2").mkString(", ")}, 0]}""")
    val sum = program(s"""{"+": [${(List.fill(5000)("1") :+ "0").mkString(", ")}]}""")
    IO.pure(
      expect(isLimit(eval(jlvm, spread, twos).result), s"spread: ${eval(jlvm, spread, twos).result}")
        .and(expect(isLimit(eval(jlvm, strict, CNull).result), s"strict: ${eval(jlvm, strict, CNull).result}"))
        .and(expect.same(Right(CInt(5000)), eval(jlvm, sum, CNull).result))
    )
  }

  test("each step of + and * pays for its accumulator: a running product is priced by its size") { jlvm =>
    // 4000 twos build a 4000-bit product: step i multiplies an i-bit accumulator
    val p = program("""{"*": {"var": "xs"}}""")
    val data = obj("xs" -> arr(Vector.fill(4000)(CInt(2))))
    val o = eval(jlvm, p, data)
    val accumulators = (1 until 4000).map(i => 1L + i / 64).sum
    IO(
      expect
        .same(Right(xyz.kd5ujc.core.canonical.CanonicalGens.int(BigInt(1) << 4000)), o.result)
        .and(expect(o.gasUsed.value >= S.cost(Op.Mul).perUnit.value * accumulators, s"gas ${o.gasUsed.value}"))
    )
  }

  // ---- 3. unique and intersect

  /**
   * An element that can only be ordered: hashing it or comparing it for equality fails the test. Hashing is what
   * attacker-chosen values can be made to collide in (every "Aa"/"BB" string has one Java hash code), and a hash set of
   * colliding values compares each insertion for equality against all the others: quadratic work for linear gas.
   */
  private final class Opaque(val n: Int) {
    override def hashCode: Int = sys.error(s"element $n was hashed")
    override def equals(that: Any): Boolean = sys.error(s"element $n was compared for equality")
  }

  test("production dedup keys preserve unsigned bytes, Unicode and first occurrences") { jlvm =>
    val values = Vector(
      CBytes(ByteVector(0x00)),
      CBytes(ByteVector(0x7f)),
      CBytes(ByteVector(0x80.toByte)),
      CBytes(ByteVector(0xff.toByte)),
      t("\u0000"),
      t("\u00ff"),
      t("\u0100"),
      t("\ud800\udc00")
    )
    val data = obj("xs" -> arr(values ++ values.reverse), "ys" -> arr(values))
    IO.pure(
      expect
        .same(Right(arr(values)), eval(jlvm, program("""{"unique":{"var":"xs"}}"""), data).result)
        .and(expect.same(Right(True), eval(jlvm, program("""{"intersect":[{"var":"xs"},{"var":"ys"}]}"""), data).result))
        .and(expect.same(Right(False), eval(jlvm, program("""{"intersect":[["0x00"],["0x80"]]}"""), CNull).result))
        .and(expect.same(Right(False), eval(jlvm, program("""{"intersect":[["ÿ"],["Ā"]]}"""), CNull).result))
    )
  }

  test("radix deduplication reads explicit keys without hashing or comparing payload objects") { _ =>
    val n = 8192
    val xs = Vector.tabulate(n)(i => new Opaque(i % (n / 2)))
    def key(x: Opaque): RadixIndex.Key = RadixIndex.Key(2, 4L)(i => (x.n >>> ((3 - i.toInt) * 8)) & 255)
    val (index, reversed, work) = xs.foldLeft((RadixIndex.empty, List.empty[Int], RadixIndex.Work.Zero)) {
      case ((index, kept, work), x) =>
        val next = index.added(key(x))
        (next.index, if (next.added) x.n :: kept else kept, work + next.work)
    }
    val lookups = xs.map(x => index.contains(key(x)))
    IO.pure(
      expect
        .same(Vector.range(0, n / 2), reversed.reverse.toVector)
        .and(expect(lookups.forall(_._1), "every payload key is present"))
        .and(expect(work.bytes <= n.toLong * (18L * 13L + 1L), s"insertion byte reads ${work.bytes}"))
        .and(expect(lookups.forall(_._2.bytes <= 10L * 13L), "every lookup is bounded by its key length"))
    )
  }

  test("intersect stops at the first element missing from the second array, before keying the rest") { jlvm =>
    // 8191 arrays after a missing 0: keying them would encode every one; only the 0 and the second array's [1] are keyed
    val left = arr(CInt(0) +: Vector.fill(8191)(arr(Vector.tabulate(100)(i => CInt(i)))))
    val data = obj("a" -> left, "b" -> arr(Vector(CInt(1))))
    val intersect = program("""{"intersect": [{"var": "a"}, {"var": "b"}]}""")
    (IO(new AtomicLong), IO(new AtomicLong)).flatMapN { (consumed, keyed) =>
      IO {
        val generic = Kernels.Default.dedup
          .subset(
            Vector.tabulate(8192)(identity),
            (i: Int) => { consumed.incrementAndGet(); Values.sortKey(CInt(if (i == 0) -1 else i)) },
            Vector(get(Values.sortKey(CInt(1))))
          )
          .map(_._1)
        val kernels = Kernels.Default.copy(key = v => { keyed.incrementAndGet(); Values.sortKey(v) })
        val o = jlvm.evaluate(get(Compiled(intersect, kernels)), data, Map.empty, Config(), kernels)
        expect
          .same(Right(false), generic)
          .and(expect.same(1L, consumed.get))
          .and(expect.same(Right(False), o.result))
          .and(expect.same(2L, keyed.get))
      }
    }
  }

  test("a sort key past the encoding ceiling is a Limit in unique and intersect; intersect keys its first array lazily") { jlvm =>
    // an array whose encoding is refused stands in for one past the JVM's largest array, which no test can build
    val tooLarge = xyz.kd5ujc.core.canonical.Cbor.TooLarge(xyz.kd5ujc.core.canonical.Cbor.MaxEncodedBytes)
    val bigText = get(CanonicalValue.text("big"))
    val kernels = Kernels.Default.copy(key = {
      case CArray(vs) if vs.contains(bigText) => Left(tooLarge)
      case v                                  => Values.sortKey(v)
    })
    val big = arr(Vector(bigText))
    def run(p: String, data: CanonicalValue) = jlvm.evaluate(get(Compiled(program(p), kernels)), data, Map.empty, Config(), kernels).result
    def limit(o: Either[JlvmError, CanonicalValue]) = o match {
      case Left(JlvmError.Limit(_)) => success
      case other                    => failure(s"expected a Limit, got $other")
    }
    val unique = """{"unique": {"var": "a"}}"""
    val intersect = """{"intersect": [{"var": "a"}, {"var": "b"}]}"""
    IO(
      limit(run(unique, obj("a" -> arr(Vector(CInt(1), big)))))
        .and(limit(run(intersect, obj("a" -> arr(Vector(CInt(1))), "b" -> arr(Vector(big))))))
        .and(limit(run(intersect, obj("a" -> arr(Vector(CInt(0))), "b" -> arr(Vector(CInt(1), CInt(1), big))))))
        .and(limit(run(intersect, obj("a" -> arr(Vector(big, CInt(0))), "b" -> arr(Vector(CInt(1)))))))
        .and(expect.same(Right(False), run(intersect, obj("a" -> arr(Vector(CInt(0), big)), "b" -> arr(Vector(CInt(1)))))))
        .and(expect.same(Right(arr(Vector(CInt(1)))), run(unique, obj("a" -> arr(Vector(CInt(1), CInt(1)))))))
    )
  }

  test("the production deduplicator scans colliding strings within linear byte and branch bounds") { jlvm =>
    val n = 8192
    val values = colliding(13).map(t)
    val keys = values.map(v => get(Values.sortKey(v)))
    val bytes = keys.map(_.radix.length).sum
    val (unique, uw) = Kernels.Default.dedup.distinct(keys)
    val (subset, iw) = get(Kernels.Default.dedup.subset(keys, (k: Values.SortKey) => Right(k), keys))
    val data = obj("xs" -> arr(values))
    IO.pure(
      expect
        .same(values, unique.map(_.value))
        .and(expect.same(true, subset))
        .and(expect(uw.bytes > 0 && uw.bytes <= 18L * bytes + n, s"unique byte reads ${uw.bytes} for $bytes key bytes"))
        .and(expect(iw.bytes > uw.bytes && iw.bytes <= 28L * bytes + n, s"intersect byte reads ${iw.bytes} for $bytes key bytes"))
        .and(expect(uw.branches <= 16L * bytes + n, s"unique branch visits ${uw.branches}"))
        .and(expect(iw.branches <= 24L * bytes + 2L * n, s"intersect branch visits ${iw.branches}"))
        .and(expect.same(Right(arr(values)), eval(jlvm, program("""{"unique": {"var": "xs"}}"""), data).result))
        .and(expect.same(Right(True), eval(jlvm, program("""{"intersect": [{"var": "xs"}, {"var": "xs"}]}"""), data).result))
    )
  }

  test("unique and intersect of colliding strings: the right answers, for the same gas as strings that do not collide") { jlvm =>
    val same = obj("xs" -> arr(colliding(13).map(t)))
    val other = obj("xs" -> arr(control(13).map(t)))
    val unique = program("""{"unique": {"var": "xs"}}""")
    val intersect = program("""{"intersect": [{"var": "xs"}, {"var": "xs"}]}""")
    IO(
      expect
        .same(Right(arr(colliding(13).map(t))), eval(jlvm, unique, same).result)
        .and(expect.same(Right(True), eval(jlvm, intersect, same).result))
        .and(expect.same(eval(jlvm, unique, other).gasUsed, eval(jlvm, unique, same).gasUsed))
        .and(expect.same(eval(jlvm, intersect, other).gasUsed, eval(jlvm, intersect, same).gasUsed))
    )
  }

  test("unique keeps the first occurrence of each value, in order; intersect is a subset test") { jlvm =>
    def run(json: String) = eval(jlvm, program(json), CNull).result
    def ok(json:  String) = JsonSurface.parse(json)
    IO.pure(
      expect
        .same(
          ok("""[3, "a", 1, [1], {"k": 1}, 2, null]"""),
          run("""{"unique": [[3, "a", 1, 3, [1], {"literal": {"k": 1}}, "a", 2, [1], {"literal": {"k": 1}}, null, 2, null]]}""")
        )
        .and(expect.same(ok("[1, 2.5]"), run("""{"unique": [[1, 2.5, 1, 2.5]]}""")))
        .and(expect.same(ok("true"), run("""{"intersect": [[2, [1], "x"], [[1], 1, 2, "x"]]}""")))
        .and(expect.same(ok("false"), run("""{"intersect": [[2, [1, 2]], [[1], 1, 2]]}""")))
    )
  }

  // ---- 4. missing and missing_some

  test("missing and missing_some pay for the path text before resolving it") { jlvm =>
    val long = t(Vector.fill(50001)("a").mkString("."))
    val some = program("""{"missing_some": [0, [{"var": "p"}]]}""")
    val missing = program("""{"missing": [{"var": "p"}]}""")
    def gap(p: Program): Long = eval(jlvm, p, obj("p" -> long)).gasUsed.value - eval(jlvm, p, obj("p" -> t("a"))).gasUsed.value
    IO(
      expect(gap(some) >= long.weight, s"missing_some: ${gap(some)} more gas for weight ${long.weight}")
        .and(expect(gap(missing) >= long.weight, s"missing: ${gap(missing)} more gas for weight ${long.weight}"))
    )
  }

  test("inside callbacks, missing pays for every frame each key may search, as var does") { jlvm =>
    val keys = arr(Vector.tabulate(50)(i => t(s"k$i")))
    val one = program("""{"map": [[1], {"missing": {"var": [["ks"]]}}]}""")
    val two = program("""{"map": [[1], {"map": [[1], {"missing": {"var": [["ks"]]}}]}]}""")
    val oneVar = program("""{"map": [[1], {"var": "k0"}]}""")
    val twoVar = program("""{"map": [[1], {"map": [[1], {"var": "k0"}]}]}""")
    val data = obj("ks" -> keys)
    val frames = eval(jlvm, two, data).gasUsed.value - eval(jlvm, one, data).gasUsed.value
    val varFrame = eval(jlvm, twoVar, data).gasUsed.value - eval(jlvm, oneVar, data).gasUsed.value
    // the extra frame costs one more map (its base, element and run) plus one segment per key; var pays one segment
    IO.pure(
      expect(frames - varFrame >= 50L * S.perPathSegment.value - S.perPathSegment.value, s"$frames vs $varFrame for one more frame")
    )
  }

  test("each frame a read searches pays for its first key's length, which the frame compares") { jlvm =>
    // every frame's element holds a 96-character key the query shares all but its last character with
    val held = "k" * 95 + "b"
    val data = obj("xs" -> arr(Vector(obj(held -> t("v")))))
    def nested(frames: Int, key: String): Program =
      program((1 to frames).foldLeft(s"""{"var": "$key"}""")((inner, _) => s"""{"map": [{"var": "xs"}, $inner]}"""))
    def gas(frames: Int, key: String): Long = eval(jlvm, nested(frames, key), data).gasUsed.value
    // a 96-character key counts 4 segments where a 1-character key counts 1: 3 more, once itself and once per frame
    val extra = 3L * S.perPathSegment.value
    IO.pure((1 to 3).toList.foldMap { frames =>
      val delta = gas(frames, "k" * 95 + "a") - gas(frames, "a")
      expect.same(extra * (1 + frames), delta)
    })
  }

  // ---- 5. computed paths

  test("a computed path is charged by its size before it is parsed") { jlvm =>
    val p = program("""{"var": {"var": "p"}}""")
    // a trailing '.' makes the path malformed: parsing it is the only way to find out
    val malformed = t(Vector.fill(50001)("a").mkString(".") + ".")
    val read = S.varAccess.value + S.perPathSegment.value
    val starved = eval(jlvm, p, obj("p" -> malformed), limit = read + 15)
    // a well-formed path pays for its text each time it is read, by its size
    val loop = program("""{"count": [{"var": "xs"}, {"var": {"var": "p"}}]}""")
    val wellFormed = t(Vector.fill(50001)("a").mkString("."))
    def gas(path: CanonicalValue): Long = eval(jlvm, loop, obj("xs" -> arr(Vector.fill(20)(CInt(1))), "p" -> path)).gasUsed.value
    val gap = gas(wellFormed) - gas(t("a"))
    val text = 20L * (S.pathText(wellFormed).value - S.pathText(t("a")).value)
    IO(
      expect(isOutOfGas(starved.result), s"out of gas before parsing: ${starved.result}")
        .and(expect(gap >= text, s"20 reads of a 100,001-character path pay $gap more gas, for $text of path text"))
    )
  }

  test("integer path segments are bounded before they are rendered as keys") { jlvm =>
    val p = program("""{"var": [["m", {"var": "i"}]]}""")
    val data = obj("m" -> obj("-1" -> CInt(7), "12" -> CInt(8)), "i" -> CInt(-1))
    val huge = obj("m" -> obj("-1" -> CInt(7)), "i" -> xyz.kd5ujc.core.canonical.CanonicalGens.int((BigInt(1) << 4095) + 1))
    IO.pure(
      expect
        .same(Right(CInt(7)), eval(jlvm, p, data).result)
        .and(expect.same(Right(CInt(8)), eval(jlvm, p, obj("m" -> obj("12" -> CInt(8)), "i" -> CInt(12))).result))
        .and(
          expect(
            eval(jlvm, p, huge).result match {
              case Left(JlvmError.Domain(Site.Var, _)) => true
              case _                                   => false
            },
            s"a 4096-bit segment is out of domain: ${eval(jlvm, p, huge).result}"
          )
        )
    )
  }

  // ---- 6. object keys

  test("object literals hold their keys as checked text, and an evaluation pays nothing for a key's length") { jlvm =>
    // the key is checked once, when the program compiles: an evaluation builds the map from the cached CText
    val key = "k" * 50000
    def p(k: String) = program(s"""{"count": [{"var": "xs"}, {"object": {"$k": 1}}]}""")
    val data = obj("xs" -> arr(Vector.fill(1000)(CInt(1))))
    val long = eval(jlvm, p(key), data)
    val literal = get(Compiled(p(key))).root match {
      case Node.Each(_, _, Node.ObjectOf(Vector((k, _)))) => scala.Some(k)
      case _                                              => scala.None
    }
    IO(
      expect
        .same(Right(CInt(1000)), long.result)
        .and(expect.same(eval(jlvm, p("k"), data).gasUsed, long.gasUsed))
        .and(expect(literal.exists(_.value == key), s"the compiled literal holds its key as CText: $literal"))
    )
  }

  // ---- 7. let

  private def lets(n: Int, names: Vector[String]): String =
    s"""{"let": [[${names.take(n).map(x => s"""["$x", 1]""").mkString(", ")}], {"var": "$$${names(n - 1)}"}]}"""

  test("let compiles with O(n log n) name comparisons: the scope is carried forward, not rebuilt per binding") { _ =>
    // rebuilding the preceding names for each binding would compare about n^2 log n / 2 times
    val names = Vector.tabulate(4000)(i => s"n$i")
    (compiling(lets(1000, names)), compiling(lets(4000, names))).mapN {
      case ((small, cs), (large, cl)) =>
        expect(small && large, "both compile")
          .and(expect(cs >= 999 && cs <= 3 * 1000 * log2(1000), s"1000 bindings compared names $cs times"))
          .and(expect(cl >= 3999 && cl <= 3 * 4000 * log2(4000), s"4000 bindings compared names $cl times"))
    }
  }

  test("let and $ reads compare colliding names through the evaluator's name order, O(n log n) times") { jlvm =>
    // 1024 bindings and a read of the last: the two runs differ only in the names, so they cost the same gas
    val n = 1024
    (comparing(jlvm, program(lets(n, colliding(10))), CNull), comparing(jlvm, program(lets(n, control(10))), CNull)).mapN {
      case ((o, names), (c, _)) =>
        expect
          .same(Right(CInt(1)), o.result)
          .and(expect.same(o.gasUsed, c.gasUsed))
          .and(expect(names >= n - 1 && names <= 2 * n * log2(n), s"$n colliding bindings compared names $names times"))
    }
  }

  test("a let pays for the length of its names, which every insertion compares") { jlvm =>
    // two names sharing a long prefix: each insertion compares them in full
    val prefix = "n" * 100000
    def twoNames(p: String) = program(s"""{"let": [[["${p}a", 1], ["${p}b", 2]], 3]}""")
    val gap = eval(jlvm, twoNames(prefix), CNull).gasUsed.value - eval(jlvm, twoNames(""), CNull).gasUsed.value
    val blocks = 2L * ((prefix.length + 1) / Values.KeyBlock)
    IO.pure(expect(gap >= S.element.value * blocks, s"$gap more gas for two names of ${prefix.length + 1} characters"))
  }

  // ---- keys and path segments compared on every lookup

  test("get and set pay for a long key at every level of their search, read off the gas") { jlvm =>
    val long = "k" * (1 << 20)
    val short = "k"
    val build =
      program("""{"reduce": [{"var": "xs"}, {"set": [{"var": "$accumulator"}, {"var": "k"}, {"var": "$index"}]}, {"object": {}}]}""")
    val lookup = program("""{"count": [{"var": "xs"}, {"get": [{"var": "m"}, {"var": "k"}]}]}""")
    def data(k: String, n: Int) =
      // the map's key and the looked-up key are equal but distinct strings, so a hit compares them
      obj("xs" -> arr(Vector.fill(n)(CInt(1))), "k" -> t(k), "m" -> obj(new String(k.toCharArray) -> CInt(1)))
    // each search pays per SearchBlock of the key at each level: the first set's map is empty, the others hold the key
    val blocks = (long.length / Values.SearchBlock).toLong
    def perBlock(op: Op) = S.cost(op).search.fold(0L)(_.perBlock.value)
    val sets = perBlock(Op.Set) * blocks * (GasSchedule.levels(0L) + 199L * GasSchedule.levels(1L))
    val gets = perBlock(Op.Get) * blocks * 200L * GasSchedule.levels(1L)
    val (b, bs) = (eval(jlvm, build, data(long, 200)), eval(jlvm, build, data(short, 200)))
    val (l, ls) = (eval(jlvm, lookup, data(long, 200)), eval(jlvm, lookup, data(short, 200)))
    IO(
      expect(b.result.isRight, s"builds: ${b.result.leftMap(_.toString)}")
        .and(expect.same(Right(CInt(200)), l.result))
        .and(expect(sets > 0 && gets > 0, "both search a key per block"))
        .and(expect.same(sets, b.gasUsed.value - bs.gasUsed.value))
        .and(expect.same(gets, l.gasUsed.value - ls.gasUsed.value))
    )
  }

  test("a computed array path pays for the length of its segments before converting them") { jlvm =>
    val p = program("""{"var": [[{"var": "k"}]]}""")
    val long = "k" * 50000
    val gap = eval(jlvm, p, obj("k" -> t(long))).gasUsed.value - eval(jlvm, p, obj("k" -> t("k"))).gasUsed.value
    IO.pure(expect(gap >= long.length / Values.KeyBlock, s"$gap more gas for a 50,000-character segment"))
  }

  test("programs are bounded in size before compilation") { _ =>
    val bound = Program.MaxSize.toInt
    val huge = Expr.Arr(Vector.fill(bound + 1)(Expr.Lit(CNull)))
    val fits = Expr.Arr(Vector.fill(bound / 4)(Expr.Lit(CNull)))
    val longPath = Expr.Var(VarRef.Data(Vector("x" * (bound + 1))), scala.None)
    val longName = Expr.Var(VarRef.Bound("x" * (bound + 1), Vector.empty), scala.None)
    // one 1 MiB segment shared 1024 times: a small tree whose path renders to 1 GiB
    val sharedPath = Expr.Var(VarRef.Data(Vector.fill(1024)("x" * (1 << 20))), scala.None)
    IO.pure(
      expect(isLimit(Program.from(huge)), "one past the bound")
        .and(expect(Program.from(fits).isRight, "at the bound"))
        .and(expect(isLimit(Program.from(longPath)), "a path segment counts its length"))
        .and(expect(isLimit(Program.from(longName)), "a binding name counts its length"))
        .and(expect(isLimit(Program.from(sharedPath)), "a path of shared segments is bounded before it is rendered"))
    )
  }

  // ---- trie listing checks: work linear in the proof plus the bounds (docs/specs/trie.md §4)

  /**
   * The chain of issue #90 in `variable(65535)`: keys `00^k` for `k = 0..n`, a pre-order chain of `n` branches (a
   * 7-bit zero segment, the first empty, with a value and a left child) ending in a leaf. Its root and encoded proof.
   */
  private def trieChain(n: Int): (Digest[Sha256], CanonicalValue) = {
    val T = TrieHashing[Sha256]
    val v = T.value(ByteVector(0x01))
    val seven = BitVector.low(7)
    @scala.annotation.tailrec
    def go(k: Int, below: ProofNode[Sha256], hash: Digest[Sha256]): (Digest[Sha256], ProofNode[Sha256]) =
      if (k < 0) (hash, below)
      else {
        val segment = if (k == 0) BitVector.empty else seven
        go(k - 1, ProofNode.PBranch(segment, Some(v), Some(below), scala.None), T.branch(segment, Some(v), Some(hash), scala.None))
      }
    val (root, top) = go(n - 1, ProofNode.PLeaf(seven, v), T.leaf(seven, v))
    (root, get(TrieProofs.encode(TrieProof(Some(top)))))
  }

  /** `trie_range_verify` of `[00^(n-1), ∞)` with an empty claim on the chain: the outcome, and the check's steps. */
  private def longFrom(jlvm: Jlvm, n: Int): (Outcome, Long, Long) = {
    val (root, proof) = trieChain(n)
    val from = ByteVector.low((n - 1).toLong)
    val p = program(
      """{"trie_range_verify": [{"literal": {"variable": 65535}}, {"var": "root"}, {"var": "from"}, null, {"literal": []}, {"var": "p"}]}"""
    )
    val o = eval(jlvm, p, obj("root" -> CBytes(root.bytes), "from" -> CBytes(from), "p" -> proof))
    val mode = get(KeyMode.variable(KeyMode.MaxVariableBytes))
    val steps = get(TrieProofs.decode[Sha256](proof).flatMap(TrieProofs.verify(mode, root, _))).checkRange(from.bits, scala.None, Nil).steps
    (o, steps, proof.weight)
  }

  test("trie_range_verify with a long from on a deep chain: work linear in the proof and bound, which its gas pays") { jlvm =>
    IO {
      val runs = List(2000, 4000).map(n => n -> longFrom(jlvm, n))
      val perWeight = S.cost(Op.TrieRangeVerify).perUnit.value
      runs.map {
        case (n, (o, steps, weight)) =>
          val bits = 8L * weight + 8L * (n - 1)
          expect
            .same(Right(False), o.result)
            .and(expect(steps <= 2L * bits, s"n = $n: $steps steps, over 2 * (proof + bound bits) = ${2L * bits}"))
            .and(expect(o.gasUsed.value >= perWeight * weight, s"n = $n: gas ${o.gasUsed.value} pays the proof's weight $weight"))
            .and(expect(steps <= o.gasUsed.value, s"n = $n: $steps steps for ${o.gasUsed.value} gas"))
      }
        .reduce(_ and _)
        .and(runs.map(_._2._2) match {
          case List(small, large) => expect(large <= 2L * small + 64L, s"twice as deep, twice the work: $small steps, then $large")
          case other              => failure(s"two runs, got $other")
        })
    }
  }

  // ---- parsing and compiling: work linear in the program, n log n where keys or names are sorted

  /**
   * The work of parsing and compiling a program: expressions visited, and object-key and name comparisons, with the key
   * comparisons parsing made apart from compiling's (each sorts the keys it holds: one could hash while the other sorts).
   */
  private final case class Work(ok: Boolean, steps: Long, keys: Long, parsedKeys: Long, names: Long, spanned: Long) {
    def compiledKeys: Long = keys - parsedKeys
  }

  /** Parses (with [[Program.from]]'s passes) and compiles `json`, counting steps, comparisons and text read back. */
  private def work(json: String): IO[Work] =
    (IO(new AtomicLong), IO(new AtomicLong), IO(new AtomicLong), IO(new AtomicLong)).flatMapN { (steps, keys, names, spans) =>
      IO {
        val kernels = Kernels.Default.copy(
          names = counted(Ordering.String, names),
          keys = counted(Kernels.CodePoints, keys),
          steps = new Steps {
            def step(): Unit = { steps.incrementAndGet(); () }
            override def spanned(length: Int): Unit = { spans.addAndGet(length.toLong); () }
          }
        )
        val parsed = Parser.parse(json, kernels)
        val parsedKeys = keys.get
        val ok = parsed.flatMap(Compiled(_, kernels)).isRight
        Work(ok, steps.get, keys.get, parsedKeys, names.get, spans.get)
      }
    }

  /** Five passes visit each expression once: reading the JSON, measuring, checking, the surface depth, compiling. */
  private val Passes = 5L

  private val N = 4096

  test("an array of n expressions is parsed and compiled in O(n) steps") { _ =>
    work(s"[${List.fill(N)("1").mkString(", ")}]").map { w =>
      expect(w.ok, "compiles").and(expect(w.steps <= Passes * (N + 1), s"$N items took ${w.steps} steps"))
    }
  }

  test("an operation's n arguments are parsed and compiled in O(n) steps") { _ =>
    work(s"""{"+": [${List.fill(N)("1").mkString(", ")}]}""").map { w =>
      expect(w.ok, "compiles").and(expect(w.steps <= Passes * (N + 1), s"$N arguments took ${w.steps} steps"))
    }
  }

  test("a map expression's n fields: O(n) steps and O(n log n) key comparisons, never a hash") { _ =>
    work(s"""{${List.tabulate(N)(i => s""""k$i": 1""").mkString(", ")}}""").map { w =>
      expect(w.ok, "compiles")
        .and(expect(w.steps <= Passes * (N + 1), s"$N fields took ${w.steps} steps"))
        .and(expect(w.parsedKeys >= N - 1 && w.parsedKeys <= 2 * N * log2(N), s"parsing $N fields compared keys ${w.parsedKeys} times"))
        .and(expect(w.compiledKeys >= N - 1 && w.compiledKeys <= 2 * N * log2(N), s"compiling them compared keys ${w.compiledKeys} times"))
    }
  }

  test("a let's n bindings: O(n) steps and O(n log n) name comparisons") { _ =>
    // each binding is three expressions: the pair, its name and its value
    work(lets(N, Vector.tabulate(N)(i => s"n$i"))).map { w =>
      expect(w.ok, "compiles")
        .and(expect(w.steps <= Passes * (3 * N + 3), s"$N bindings took ${w.steps} steps"))
        .and(expect(w.names >= N - 1 && w.names <= 3 * N * log2(N), s"$N bindings compared names ${w.names} times"))
    }
  }

  test("a let's n bindings written as an object: O(n log n) key and name comparisons") { _ =>
    work(s"""{"let": [{${List.tabulate(N)(i => s""""n$i": 1""").mkString(", ")}}, 1]}""").map { w =>
      expect(w.ok, "compiles")
        .and(expect(w.steps <= Passes * (N + 3), s"$N bindings took ${w.steps} steps"))
        .and(expect(w.parsedKeys >= N - 1 && w.parsedKeys <= 2 * N * log2(N), s"parsing $N bindings compared keys ${w.parsedKeys} times"))
        .and(expect(w.compiledKeys >= N - 1 && w.compiledKeys <= 2 * N * log2(N), s"compiling them compared keys ${w.compiledKeys} times"))
        .and(expect(w.names >= N - 1 && w.names <= 3 * N * log2(N), s"$N bindings compared names ${w.names} times"))
    }
  }

  /**
   * The strict parser reads the whole text once, then each scalar, member name and literal from its own span: at most
   * twice the text (an array's leading string is read to check it names no call, then as its value), however deeply
   * the containers around the spans nest. Reading a container's span where a string or an integer is looked for would
   * read it once per level instead.
   */
  test("the text read back from spans is at most twice the program, however deeply it nests") { _ =>
    val fields = List.tabulate(N)(i => s""""k$i": {"literal": [$i, "t", {"a": null}]}""").mkString(", ")
    val texts = List(
      "nested arrays"                 -> ("[" * 100 + "1" + "]" * 100),
      "arrays headed by text"         -> ("[\"x\", " * 100 + "\"a\"" + "]" * 100),
      "computed paths"                -> ("{\"var\": " * 120 + "\"a\"" + "}" * 120),
      "computed paths with a default" -> ("{\"var\": [" * 40 + "\"a\"" + ", 1]}" * 40),
      "literals and fields"           -> s"{$fields}"
    )
    texts.traverse {
      case (name, text) =>
        work(text).map { w =>
          expect(w.ok, s"$name compiles").and(expect(w.spanned <= 2L * text.length, s"$name: ${w.spanned} read back of ${text.length}"))
        }
    }
      .map(_.reduce(_ and _))
  }

  test("a program nested d deep is parsed and compiled in O(d) steps") { _ =>
    val d = 120
    work((1 to d).foldLeft("1")((inner, _) => s"""{"!": $inner}""")).map { w =>
      expect(w.ok, "compiles").and(expect(w.steps <= Passes * (d + 1), s"$d levels took ${w.steps} steps"))
    }
  }

  // ---- map searches: comparisons per level of the depth bound, whatever order the map was built in

  test("get, has, set and unset compare at most 2 keys per level they pay for (4 for set and unset), each read in full") { jlvm =>
    val n = 1000
    val prefix = "k" * 300
    val keys = Vector.tabulate(n)(i => prefix + f"$i%04d")
    val order = new xyz.kd5ujc.core.canonical.CountingKeys
    def built(ks: Vector[String]): CanonicalValue =
      ks.foldLeft[CanonicalValue](get(order.map(Vector.empty))) {
        case (m: CMap, k) =>
          t(k) match {
            case key: CText => get(CanonicalValue.updated(m, key, CInt(1)))
            case _          => m
          }
        case (m, _) => m
      }
    // decoded maps are built from their entries; programs build them one key at a time, here in the worst orders
    val maps = List(
      "from entries" -> get(order.map(keys.map(_ -> CInt(1)))),
      "ascending"    -> built(keys),
      "descending"   -> built(keys.reverse)
    )
    val levels = GasSchedule.levels(n.toLong)
    val ops = List("get" -> 2L, "has" -> 2L, "set" -> 4L, "unset" -> 4L)
    val searches = for {
      (shape, mp) <- maps
      (op, per)   <- ops
      k           <- keys.take(64) ++ keys.takeRight(64) :+ (prefix + "zzzz")
    } yield {
      val p = program(if (op == "set") s"""{"set": [{"var": "m"}, {"var": "k"}, 2]}""" else s"""{"$op": [{"var": "m"}, {"var": "k"}]}""")
      val (c0, u0) = (order.comparisons.get, order.units.get)
      val o = eval(jlvm, p, obj("m" -> mp, "k" -> t(k)))
      (shape, op, per, o.result.isRight, order.comparisons.get - c0, order.units.get - u0)
    }
    val worst = searches.groupBy(s => (s._1, s._2)).view.mapValues(_.map(_._5).max).toMap
    IO(
      searches
        .foldLeft(expect(searches.forall(_._4), "every search succeeds")) {
          case (acc, (shape, op, per, _, comparisons, units)) =>
            acc
              .and(expect(comparisons <= per * levels, s"$op on a map built $shape: $comparisons comparisons, $levels levels"))
              .and(expect(units <= comparisons * (prefix.length + 5), s"$op on a map built $shape: $units units read"))
        }
        .and(expect(worst.values.max >= levels, s"the bound is reached: ${worst.toList.sortBy(_._1.toString).mkString(", ")}"))
    )
  }

  test("a limit of 2^53 - 1 is metered like any other; a larger or negative one is Limit with no gas used, before compiling") { jlvm =>
    val sum = program("""{"+": [1, 2]}""")
    val twice = program("""{"let": [[["x", 1], ["x", 2]], 1]}""")
    def run(p: Program, limit: Long, d: Config.Driver) =
      jlvm.evaluate(p, CNull, Map.empty, Config(limit = GasLimit.of(limit), driver = d))
    val max = GasLimit.Max.amount.value
    IO(
      expect
        .same(9007199254740991L, max)
        .and(Config.Driver.values.toList.foldMap { d =>
          val atDefault = run(sum, GasLimit.Default.amount.value, d)
          val atMax = run(sum, max, d)
          val over = List(max + 1, Long.MaxValue, -1L, Long.MinValue).map(l => (l, run(sum, l, d), run(twice, l, d)))
          expect
            .same(Right(CInt(3)), atMax.result)
            .and(
              expect(atDefault.gasUsed.value == atMax.gasUsed.value, s"$d: the gas used at the maximum is exact: ${atMax.gasUsed.value}")
            )
            .and(expect(isOutOfGas(run(sum, atMax.gasUsed.value - 1, d).result), s"$d: one gas short is out of gas"))
            .and(over.foldMap {
              case (l, o, t) =>
                expect(isLimit(o.result), s"$d: a limit of $l is Limit, got ${o.result}")
                  .and(expect(o.gasUsed.value == 0L, s"$d: a limit of $l uses no gas, used ${o.gasUsed.value}"))
                  .and(expect(o.warnings.isEmpty, s"$d: a limit of $l warns nothing"))
                  .and(expect(isLimit(t.result), s"$d: a limit of $l is Limit before the program is compiled, got ${t.result}"))
                  .and(expect(t.gasUsed.value == 0L, s"$d: a program that does not compile, at a limit of $l, uses no gas"))
            })
        })
    )
  }
}
