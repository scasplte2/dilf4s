package xyz.kd5ujc.jlvm

import java.util.concurrent.atomic.AtomicLong

import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import weaver.{Expectations, IOSuite}

/**
 * Denial-of-service regressions: every unit of work is charged before it is done and bounded by the gas limit. Nothing
 * here measures time, so a loaded machine cannot fail it. A test either reads the bound off the gas (the charge a piece
 * of work pays grows with its size), or counts the comparisons the evaluator makes through counting [[Kernels]] and
 * bounds them by the input's size: a program-chosen name or value is only ever compared, never hashed, and in
 * O(n log n) comparisons. How fast a unit of work runs is calibration, measured by the `benchmarks` GasCalibration job.
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

  /** The production kernels, with the name order counting its comparisons in `names` and the value order in `values`. */
  private def counting(names: AtomicLong, values: AtomicLong): Kernels =
    Kernels.Default.copy(names = counted(Ordering.String, names), values = counted(Values.sortKeyOrder, values))

  /** `ceil(log2 n)`. */
  private def log2(n: Int): Long = 64L - java.lang.Long.numberOfLeadingZeros(n.toLong - 1)

  /**
   * Compiles and evaluates `p` with counting kernels: the outcome, and the name and value comparisons the evaluation
   * made (compilation's are counted apart, in [[compiling]]).
   */
  private def comparing(jlvm: Jlvm, p: Program, data: CanonicalValue): IO[(Outcome, Long, Long)] =
    (IO(new AtomicLong), IO(new AtomicLong)).flatMapN { (names, values) =>
      IO {
        val kernels = counting(names, values)
        val compiled = get(Compiled(p, kernels))
        val (n0, v0) = (names.get, values.get)
        val o = jlvm.evaluate(compiled, data, Map.empty, Config(), kernels)
        (o, names.get - n0, values.get - v0)
      }
    }

  /** Parses and compiles `json` with counting kernels: whether it compiled, and the name comparisons it made. */
  private def compiling(json: String): IO[(Boolean, Long)] =
    (IO(new AtomicLong), IO(new AtomicLong)).flatMapN { (names, values) =>
      IO((Parser.parse(json).flatMap(Compiled(_, counting(names, values))).isRight, names.get))
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
    val fifth = CanonicalValue.number(get(Ratio.of(BigInt(1), BigInt(5).pow(1764))))
    val half = CanonicalValue.number(get(Ratio.of(BigInt(1), BigInt(1) << 4095)))
    val cat = program("""{"cat": {"var": "xs"}}""")
    val join = program("""{"join": [{"var": "xs"}, ""]}""")
    // the rendering charge grows with each number's decimal expansion ((4 + bits/64)^2 units), and is taken before it
    def rendersPaid(p: Program, v: CanonicalValue): Expectations = {
      val numbers = Vector.fill(20)(v)
      val o = eval(jlvm, p, obj("xs" -> arr(numbers)))
      val out = o.result.map(_.weight).getOrElse(0L)
      expect(o.result.isRight, s"renders: ${o.result.leftMap(_.toString)}")
        .and(expect(o.gasUsed.value >= out, s"at least 1 gas per output character: ${o.gasUsed.value} for $out"))
        .and(expect(S.rendered(numbers) <= o.gasUsed, s"pays the rendering charge ${S.rendered(numbers)}: ${o.gasUsed}"))
    }
    val ints = Vector.tabulate(1000)(i => CInt(BigInt(i) * 1000003))
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
        .and(expect.same(Right(CInt(BigInt(5000))), eval(jlvm, sum, CNull).result))
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
        .same(Right(CInt(BigInt(1) << 4000)), o.result)
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

  private val byNumber: Ordering[Opaque] = Ordering.by(_.n)

  test("deduplication never hashes or compares values for equality, and compares them O(n log n) times") { _ =>
    // 8192 elements, each value twice; a quadratic algorithm would compare about n^2 / 2 = 33 million times
    val n = 8192
    val log2n = log2(n)
    val xs = Vector.tabulate(n)(i => new Opaque(i % (n / 2)))
    (IO(new AtomicLong), IO(new AtomicLong)).flatMapN { (uniqueCount, subsetCount) =>
      IO {
        val unique = Either.catchNonFatal(Dedup.Sorting.distinct(xs)(using counted(byNumber, uniqueCount)).map(_.n)).leftMap(_.getMessage)
        val subset = Either.catchNonFatal(Dedup.Sorting.subset(xs.iterator, xs)(using counted(byNumber, subsetCount))).leftMap(_.getMessage)
        expect
          .same(Right(Vector.range(0, n / 2)), unique)
          .and(expect.same(Right(true), subset))
          .and(expect(uniqueCount.get <= 2 * n * log2n, s"unique compared ${uniqueCount.get} times for $n elements"))
          .and(expect(subsetCount.get <= 3 * n * log2n, s"intersect compared ${subsetCount.get} times for $n elements"))
      }
    }
  }

  test("intersect stops at the first element missing from the second array, before keying the rest") { jlvm =>
    // 8191 arrays after a missing 0: keying them would encode every one; only the 0 and the second array's [1] are keyed
    val left = arr(CInt(0) +: Vector.fill(8191)(arr(Vector.tabulate(100)(i => CInt(BigInt(i))))))
    val data = obj("a" -> left, "b" -> arr(Vector(CInt(1))))
    val intersect = program("""{"intersect": [{"var": "a"}, {"var": "b"}]}""")
    (IO(new AtomicLong), IO(new AtomicLong)).flatMapN { (consumed, keyed) =>
      IO {
        val generic = Dedup.Sorting.subset(
          Iterator.tabulate(8192) { i =>
            consumed.incrementAndGet(); new Opaque(if (i == 0) -1 else i)
          },
          Vector(new Opaque(1))
        )(using byNumber)
        val kernels = Kernels.Default.copy(key = v => { keyed.incrementAndGet(); Values.sortKey(v) })
        val o = jlvm.evaluate(get(Compiled(intersect, kernels)), data, Map.empty, Config(), kernels)
        expect(!generic, "the first element is missing")
          .and(expect.same(1L, consumed.get))
          .and(expect.same(Right(False), o.result))
          .and(expect.same(2L, keyed.get))
      }
    }
  }

  test("unique and intersect compare colliding strings through the evaluator's value order, O(n log n) times") { jlvm =>
    // at least n - 1 comparisons: the values went through the order, not a hash set
    val n = 8192
    val same = obj("xs" -> arr(colliding(13).map(t)))
    (
      comparing(jlvm, program("""{"unique": {"var": "xs"}}"""), same),
      comparing(jlvm, program("""{"intersect": [{"var": "xs"}, {"var": "xs"}]}"""), same)
    ).mapN {
      case ((u, _, uc), (i, _, ic)) =>
        expect
          .same(Right(arr(colliding(13).map(t))), u.result)
          .and(expect.same(Right(True), i.result))
          .and(expect(uc >= n - 1 && uc <= 2 * n * log2(n), s"unique compared $uc times for $n values"))
          .and(expect(ic >= n - 1 && ic <= 3 * n * log2(n), s"intersect compared $ic times for $n values"))
    }
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
    val huge = obj("m" -> obj("-1" -> CInt(7)), "i" -> CInt((BigInt(1) << 4095) + 1))
    IO.pure(
      expect
        .same(Right(CInt(7)), eval(jlvm, p, data).result)
        .and(expect.same(Right(CInt(8)), eval(jlvm, p, obj("m" -> obj("12" -> CInt(8)), "i" -> CInt(12))).result))
        .and(
          expect(
            eval(jlvm, p, huge).result match {
              case Left(JlvmError.Domain("var", _)) => true
              case _                                => false
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
        .same(Right(CInt(BigInt(1000))), long.result)
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
      case ((o, names, _), (c, _, _)) =>
        expect
          .same(Right(CInt(BigInt(1))), o.result)
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

  test("set does not rescan its key, and get, has, set and unset pay for long keys") { jlvm =>
    val long = "k" * (1 << 20)
    val short = "k"
    val build =
      program("""{"reduce": [{"var": "xs"}, {"set": [{"var": "$accumulator"}, {"var": "k"}, {"var": "$index"}]}, {"object": {}}]}""")
    val lookup = program("""{"count": [{"var": "xs"}, {"get": [{"var": "m"}, {"var": "k"}]}]}""")
    def data(k: String, n: Int) =
      // the map's key and the looked-up key are equal but distinct strings, so a hit compares them
      obj("xs" -> arr(Vector.fill(n)(CInt(1))), "k" -> t(k), "m" -> obj(new String(k.toCharArray) -> CInt(1)))
    // each of the 200 sets and gets pays the key's length (the Key unit: one per KeyBlock), which a hit compares
    val blocks = 200L * (long.length / Values.KeyBlock)
    val (b, bs) = (eval(jlvm, build, data(long, 200)), eval(jlvm, build, data(short, 200)))
    val (l, ls) = (eval(jlvm, lookup, data(long, 200)), eval(jlvm, lookup, data(short, 200)))
    IO(
      expect(b.result.isRight, s"builds: ${b.result.leftMap(_.toString)}")
        .and(expect.same(Right(CInt(BigInt(200))), l.result))
        .and(expect(b.gasUsed.value - bs.gasUsed.value >= blocks, s"set with a 1 MB key pays ${b.gasUsed.value - bs.gasUsed.value} more"))
        .and(expect(l.gasUsed.value - ls.gasUsed.value >= blocks, s"get with a 1 MB key pays ${l.gasUsed.value - ls.gasUsed.value} more"))
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
}
