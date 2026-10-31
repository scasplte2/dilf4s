package xyz.kd5ujc.jlvm

import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.numerics.Ratio

import weaver.{Expectations, IOSuite}

/**
 * Denial-of-service regressions: every unit of work is charged before it is done and bounded by the gas limit. Where a
 * bound cannot be read off the gas alone, a test times the work and compares its gas per microsecond with a reference
 * timed alongside it (a `map`, whose node charges are calibrated): at least [[MinShare]] of the reference rate, so only
 * work priced an order of magnitude too low fails, however loaded the machine is.
 */
object MeteringSuite extends IOSuite {

  type Res = Jlvm
  def sharedResource: Resource[IO, Jlvm] = Resource.eval(Jlvm.load[IO])

  private val S = GasSchedule.V1
  private val MinShare = 0.1

  // timed tests run one at a time
  override def maxParallelism: Int = 1

  private def get[A](e:     Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)
  private def t(s:          String): CanonicalValue = get(CanonicalValue.text(s))
  private def arr(vs:       Vector[CanonicalValue]): CanonicalValue = get(CanonicalValue.array(vs))
  private def obj(kvs:      (String, CanonicalValue)*): CanonicalValue = get(CanonicalValue.map(kvs.toVector))
  private def program(json: String): Program = get(Parser.parse(json))

  private def eval(jlvm: Jlvm, p: Program, data: CanonicalValue, limit: Long = GasLimit.Default.amount.value): Outcome =
    jlvm.evaluate(p, data, Map.empty, Config(limit = GasLimit(Gas(limit))))

  /**
   * The fastest of five runs, in microseconds, after warming up (at least 20 runs or 300 ms, at most 200 runs), so the
   * JIT has compiled what is timed.
   */
  private def micros[A](f: => A): IO[(A, Double)] = {
    val once = IO.monotonic.flatMap(t0 => IO(f).flatMap(a => IO.monotonic.map(t1 => (a, (t1 - t0).toNanos / 1000.0))))
    def warm(runs: Int, spent: Double): IO[Unit] =
      if (runs >= 200 || (runs >= 20 && spent >= 300000.0)) IO.unit else once.flatMap { case (_, us) => warm(runs + 1, spent + us) }
    warm(0, 0.0) >> List.fill(5)(once).sequence.map(rs => rs.minBy(_._2))
  }

  /** Gas per microsecond of an evaluation (compiled beforehand), timed. */
  private def rate(jlvm: Jlvm, p: Program, data: CanonicalValue): IO[(Outcome, Double)] = {
    val compiled = get(Compiled(p))
    micros(jlvm.evaluate(compiled, data, Map.empty, Config(limit = GasLimit(Gas(Long.MaxValue / 4))))).map {
      case (o, us) => (o, o.gasUsed.value / math.max(us, 1.0))
    }
  }

  private val reference = program("""{"map": [{"var": "xs"}, {"+": [{"var": "$current"}, 1]}]}""")
  private val referenceData = obj("xs" -> arr(Vector.tabulate(2000)(i => CInt(BigInt(i)))))

  /** An evaluation's gas per microsecond, as a share of the reference's, timed right after it. */
  private def priced(jlvm: Jlvm, p: Program, data: CanonicalValue): IO[(Outcome, Double)] =
    (rate(jlvm, p, data), rate(jlvm, reference, referenceData)).mapN { case ((o, r), (_, ref)) => (o, r / ref) }

  private def isLimit(r: Either[JlvmError, ?]): Boolean = r match {
    case Left(JlvmError.Limit(_)) => true
    case _                        => false
  }

  private def isOutOfGas(r: Either[JlvmError, ?]): Boolean = r match {
    case Left(JlvmError.OutOfGas(_, _)) => true
    case _                              => false
  }

  /** `2^k` distinct strings of length `2k` with one Java hash code: every combination of "Aa" and "BB". */
  private def colliding(k: Int): Vector[String] =
    (1 to k).foldLeft(Vector(""))((acc, _) => acc.flatMap(s => Vector(s + "Aa", s + "BB")))

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
    def rendersPaid(p: Program, v: CanonicalValue): IO[Expectations] =
      priced(jlvm, p, obj("xs" -> arr(Vector.fill(20)(v)))).map {
        case (o, rate) =>
          val out = o.result.map(_.weight).getOrElse(0L)
          expect(o.result.isRight, s"renders: ${o.result.leftMap(_.toString)}")
            .and(expect(o.gasUsed.value >= out, s"at least 1 gas per output character: ${o.gasUsed.value} for $out"))
            .and(expect(rate >= MinShare, s"priced at $rate of the reference rate"))
      }
    val ints = arr(Vector.tabulate(1000)(i => CInt(BigInt(i) * 1000003)))
    val words = arr(Vector.fill(1000)(t("w")))
    (rendersPaid(cat, fifth), rendersPaid(cat, half), rendersPaid(join, fifth), priced(jlvm, cat, obj("xs" -> ints))).mapN {
      (a, b, c, small) =>
        val gasInts = small._1.gasUsed.value
        val gasWords = eval(jlvm, cat, obj("xs" -> words)).gasUsed.value
        a.and(b)
          .and(c)
          .and(expect(small._2 >= MinShare, s"rendering small integers priced at ${small._2} of the reference rate"))
          .and(expect(gasInts > gasWords, s"1000 rendered integers ($gasInts) cost more than 1000 one-letter texts ($gasWords)"))
    }
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
    priced(jlvm, p, data).map {
      case (o, rate) =>
        val accumulators = (1 until 4000).map(i => 1L + i / 64).sum
        expect
          .same(Right(CInt(BigInt(1) << 4000)), o.result)
          .and(expect(o.gasUsed.value >= S.cost(Op.Mul).perUnit.value * accumulators, s"gas ${o.gasUsed.value}"))
          .and(expect(rate >= MinShare, s"priced at $rate of the reference rate"))
    }
  }

  // ---- 3. unique and intersect

  test("unique and intersect do not hash attacker-chosen values: colliding strings are as cheap as any others") { jlvm =>
    val same = arr(colliding(13).map(t))
    val unique = program("""{"unique": {"var": "xs"}}""")
    val intersect = program("""{"intersect": [{"var": "xs"}, {"var": "xs"}]}""")
    (priced(jlvm, unique, obj("xs" -> same)), priced(jlvm, intersect, obj("xs" -> same))).mapN {
      case ((u, ru), (i, ri)) =>
        expect
          .same(Right(same), u.result)
          .and(expect.same(Right(True), i.result))
          .and(expect(ru >= MinShare, s"unique of 8192 colliding strings priced at $ru of the reference rate"))
          .and(expect(ri >= MinShare, s"intersect of 8192 colliding strings priced at $ri of the reference rate"))
    }
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
    val loop = program("""{"count": [{"var": "xs"}, {"missing_some": [0, [{"var": "p"}]]}]}""")
    priced(jlvm, loop, obj("xs" -> arr(Vector.fill(20)(CInt(1))), "p" -> long)).map {
      case (_, rate) =>
        expect(gap(some) >= long.weight, s"missing_some: ${gap(some)} more gas for weight ${long.weight}")
          .and(expect(gap(missing) >= long.weight, s"missing: ${gap(missing)} more gas for weight ${long.weight}"))
          .and(expect(rate >= MinShare, s"priced at $rate of the reference rate"))
    }
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

  // ---- 5. computed paths

  test("a computed path is charged by its size before it is parsed") { jlvm =>
    val p = program("""{"var": {"var": "p"}}""")
    // a trailing '.' makes the path malformed: parsing it is the only way to find out
    val malformed = t(Vector.fill(50001)("a").mkString(".") + ".")
    val read = S.varAccess.value + S.perPathSegment.value
    val starved = eval(jlvm, p, obj("p" -> malformed), limit = read + 15)
    val loop = program("""{"count": [{"var": "xs"}, {"var": {"var": "p"}}]}""")
    val wellFormed = t(Vector.fill(50001)("a").mkString("."))
    priced(jlvm, loop, obj("xs" -> arr(Vector.fill(20)(CInt(1))), "p" -> wellFormed)).map {
      case (_, rate) =>
        expect(isOutOfGas(starved.result), s"out of gas before parsing: ${starved.result}")
          .and(expect(rate >= MinShare, s"priced at $rate of the reference rate"))
    }
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

  test("object literals do not rescan their keys on every evaluation") { jlvm =>
    val key = "k" * 50000
    val p = program(s"""{"count": [{"var": "xs"}, {"object": {"$key": 1}}]}""")
    priced(jlvm, p, obj("xs" -> arr(Vector.fill(1000)(CInt(1))))).map {
      case (o, rate) =>
        expect.same(Right(CInt(BigInt(1000))), o.result).and(expect(rate >= MinShare, s"priced at $rate of the reference rate"))
    }
  }

  // ---- 7. let

  private def lets(n: Int, names: Vector[String]): String =
    s"""{"let": [[${names.take(n).map(x => s"""["$x", 1]""").mkString(", ")}], {"var": "$$${names(n - 1)}"}]}"""

  test("let compiles in linear time") { _ =>
    val names = Vector.tabulate(8000)(i => s"n$i")
    def compile(n: Int) = micros(Parser.parse(lets(n, names)).flatMap(Compiled(_)))
    (compile(1000), compile(4000)).mapN {
      case ((small, ts), (large, tl)) =>
        expect(small.isRight && large.isRight, "both compile")
          .and(expect(tl < 400000.0, s"4000 bindings compile in ${tl / 1000} ms"))
          .and(expect(tl < 10 * math.max(ts, 1000.0), s"4000 bindings take ${tl / ts} times as long as 1000"))
    }
  }

  test("let and $ reads do not hash attacker-chosen names") { jlvm =>
    val names = colliding(10)
    val p = program(lets(names.size, names))
    priced(jlvm, p, CNull).map {
      case (o, rate) =>
        expect.same(Right(CInt(BigInt(1))), o.result).and(expect(rate >= MinShare, s"priced at $rate of the reference rate"))
    }
  }

  test("programs are bounded in size before compilation") { _ =>
    val bound = Program.MaxSize.toInt
    val huge = Expr.Arr(Vector.fill(bound + 1)(Expr.Lit(CNull)))
    val fits = Expr.Arr(Vector.fill(bound / 4)(Expr.Lit(CNull)))
    IO.pure(expect(isLimit(Program.from(huge)), "one past the bound").and(expect(Program.from(fits).isRight, "at the bound")))
  }
}
