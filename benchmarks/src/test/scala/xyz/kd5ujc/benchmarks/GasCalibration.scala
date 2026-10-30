package xyz.kd5ujc.benchmarks

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.concurrent.duration._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.hash.{Hasher, Sha256}
import xyz.kd5ujc.jlvm._
import xyz.kd5ujc.numerics.Ratio

import scodec.bits.ByteVector

/**
 * Calibrates JLVM gas schedule v1 against wall-clock time, at a reference rate of [[Rate]] gas per microsecond: the rate
 * the cryptographic operations are priced at. Every operation is timed through `Jlvm.evaluate` at a small and a large
 * argument size, less a baseline that evaluates the same argument reads, and a base and per-unit charge are fitted to the
 * two points. The node charges are fitted the same way, from programs of growing size.
 *
 * Results are machine-specific; run on the reference hardware, then apply the suggestions to `GasSchedule`:
 * `sbt "benchmarks/Test/runMain xyz.kd5ujc.benchmarks.GasCalibration"`.
 */
object GasCalibration extends IOApp {

  val Rate: Double = 150.0

  private val schedule = GasSchedule.V1
  private val unlimited = Config(limit = GasLimit(Gas(Long.MaxValue / 4)))

  private def get[A](e: Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)
  private def t(s:      String): CanonicalValue = get(CanonicalValue.text(s))
  private def a(vs:     Vector[CanonicalValue]): CanonicalValue = get(CanonicalValue.array(vs))
  private def m(kvs:    Vector[(String, CanonicalValue)]): CanonicalValue = get(CanonicalValue.map(kvs))

  /** A `bits`-bit number with no structure (gcds take their typical time), derived from `seed` by hashing. */
  private def word(bits: Int, seed: Int): BigInt = {
    val stream = (0 to bits / 256).foldLeft(ByteVector.empty)((b, i) =>
      b ++ Hasher[Sha256].digest(ByteVector.fromInt(seed) ++ ByteVector.fromInt(i)).bytes
    )
    (BigInt(1, stream.toArray) >> (stream.size.toInt * 8 - bits)).setBit(bits - 1)
  }
  private def int(bits: Int, seed: Int): CanonicalValue = CInt(word(bits, seed))
  private def rat(bits: Int, seed: Int): CanonicalValue =
    CanonicalValue.number(get(Ratio.of(word(bits, seed), word(bits, seed + 7).setBit(0))))
  private def ints(n:  Int): CanonicalValue = a(Vector.tabulate(n)(i => CInt(BigInt(i))))
  private def texts(n: Int): CanonicalValue = a(Vector.tabulate(n)(i => t(s"t$i")))
  private def map(n:   Int): CanonicalValue = m(Vector.tabulate(n)(i => s"k$i" -> CInt(BigInt(i))))
  private def text(n:  Int): CanonicalValue = t(("ab c" * (n / 4 + 1)).take(n))
  private def bytes(n: Int): CanonicalValue = CBytes(ByteVector.fill(n.toLong)(0x5a))

  /** Microseconds per call, from batches of at least 200ms, after a warm-up batch. */
  private def micros(f: () => Long): IO[Double] = {
    def batch(k: Int): IO[(Int, FiniteDuration, Long)] =
      IO.monotonic.flatMap(t0 => IO((1 to k).foldLeft(0L)((s, _) => s + f())).flatMap(sum => IO.monotonic.map(t1 => (k, t1 - t0, sum))))
    def go(k: Int): IO[Double] =
      batch(k).flatMap {
        case (n, d, _) => if (d < 200.millis && k < (1 << 26)) go(k * 2) else IO.pure(d.toNanos.toDouble / 1000.0 / n.toDouble)
      }
    batch(4).flatMap(_ => go(1))
  }

  private def reads(n:   Int): Vector[Expr] = Vector.tabulate(n)(i => Expr.Var(VarRef.Data(Vector(s"a$i")), scala.None))
  private def data(args: Vector[CanonicalValue]): CanonicalValue = m(args.zipWithIndex.map { case (v, i) => s"a$i" -> v })

  private def run(jlvm: Jlvm, e: Expr, d: CanonicalValue, config: Config = unlimited): () => Long = {
    val p = get(Program.from(e))
    () => jlvm.evaluate(p, d, Map.empty, config).gasUsed.value
  }

  /** The op's own time: the call, less reading its arguments. */
  private def opTime(jlvm: Jlvm, op: Op, args: Vector[CanonicalValue]): IO[Double] = {
    val d = data(args)
    (micros(run(jlvm, Expr.Apply(op, reads(args.size)), d)), micros(run(jlvm, Expr.Arr(reads(args.size)), d))).mapN((x, b) =>
      math.max(x - b, 0.0)
    )
  }

  private def units(op: Op, args: Vector[CanonicalValue]): Long =
    args.zipWithIndex.foldLeft(0L) { case (n, (v, i)) => n + GasSchedule.units(schedule.cost(op).rate(i).unit, v) }

  private def charged(op: Op, args: Vector[CanonicalValue]): Long = (schedule.cost(op).base + schedule.sized(op, args)).value

  final case class Case(op: Op, small: Vector[CanonicalValue], large: Vector[CanonicalValue], note: String = "")

  private def two(f:  Int => CanonicalValue, s:        Int, l: Int) = (Vector(f(s), f(s)), Vector(f(l), f(l)))
  private def num2(f: (Int, Int) => CanonicalValue, s: Int, l: Int) = (Vector(f(s, 1), f(s, 2)), Vector(f(l, 1), f(l, 2)))
  private def one(f:  Int => CanonicalValue, s:        Int, l: Int) = (Vector(f(s)), Vector(f(l)))
  private def mk(op: Op, p: (Vector[CanonicalValue], Vector[CanonicalValue]), note: String = "") = Case(op, p._1, p._2, note)

  private val cases: List[Case] = List(
    mk(Op.Add, num2(int, 64, 4000), "integers"),
    mk(Op.Sub, num2(int, 64, 4000), "integers"),
    mk(Op.Mul, num2(int, 64, 2000), "integers"),
    mk(Op.Div, num2(int, 64, 4000), "integers: the quotient is normalised"),
    mk(Op.Mod, num2(int, 64, 4000), "integers"),
    mk(Op.Min, num2(int, 64, 4000)),
    mk(Op.Max, num2(int, 64, 4000)),
    mk(Op.Lt, num2(int, 64, 4000)),
    mk(Op.Eq, two(ints, 4, 8000), "equal arrays, distinct copies"),
    mk(Op.Ne, two(ints, 4, 8000)),
    mk(Op.Abs, (Vector(rat(64, 1)), Vector(rat(2000, 1)))),
    mk(Op.Round, (Vector(rat(64, 1)), Vector(rat(2000, 1)))),
    mk(Op.Floor, (Vector(rat(64, 1)), Vector(rat(2000, 1)))),
    Case(Op.Pow, Vector(int(64, 1), CInt(2)), Vector(int(64, 1), CInt(60))),
    mk(Op.Not, one(ints, 1, 1000)),
    mk(Op.TypeOf, one(ints, 1, 1000)),
    mk(Op.Exists, one(ints, 1, 1000)),
    mk(Op.Length, one(text, 16, 64000), "text"),
    mk(Op.Lower, one(text, 16, 64000)),
    mk(Op.Upper, one(text, 16, 64000)),
    mk(Op.Trim, one(text, 16, 64000)),
    Case(Op.Substr, Vector(text(16), CInt(2), CInt(8)), Vector(text(64000), CInt(2), CInt(32000))),
    Case(Op.StartsWith, Vector(text(16), text(4)), Vector(text(64000), text(32000))),
    Case(Op.Split, Vector(text(16), t(" ")), Vector(text(64000), t(" "))),
    Case(Op.In, Vector(t("zz"), text(16)), Vector(t("zz"), text(64000)), "substring"),
    mk(Op.Cat, one(texts, 4, 4000), "a spread-free array is one argument: texts as elements"),
    Case(Op.Join, Vector(texts(4), t(",")), Vector(texts(8000), t(","))),
    Case(Op.In, Vector(CInt(-1), ints(4)), Vector(CInt(-1), ints(8000)), "array membership"),
    mk(Op.Unique, one(ints, 4, 8000)),
    mk(Op.Reverse, one(ints, 4, 8000)),
    Case(Op.Slice, Vector(ints(4), CInt(1), CInt(3)), Vector(ints(8000), CInt(1), CInt(7000))),
    mk(Op.Flatten, one(n => a(Vector.fill(n)(ints(4))), 2, 2000)),
    mk(Op.Count, one(ints, 4, 8000)),
    mk(Op.Intersect, two(ints, 4, 4000)),
    mk(Op.Merge, two(map, 4, 8000), "maps"),
    mk(Op.Keys, one(map, 4, 8000)),
    mk(Op.Values, one(map, 4, 8000)),
    mk(Op.Entries, one(map, 4, 8000)),
    Case(Op.Get, Vector(map(4), t("k1")), Vector(map(8000), t("k1"))),
    Case(Op.Has, Vector(map(4), t("k1")), Vector(map(8000), t("k1"))),
    Case(Op.Set, Vector(map(4), t("x"), CInt(1)), Vector(map(8000), t("x"), CInt(1))),
    Case(Op.Unset, Vector(map(4), t("k1")), Vector(map(8000), t("k1"))),
    mk(Op.Hex, one(bytes, 32, 64000)),
    mk(Op.Unhex, one(n => t("0x" + "ab" * n), 32, 32000)),
    mk(Op.BytesToInt, one(bytes, 8, 512)),
    Case(Op.IntToBytes, Vector(int(64, 1), CInt(8)), Vector(int(4000, 1), CInt(512))),
    mk(Op.Sha256, one(bytes, 32, 64000)),
    mk(Op.Sha3_256, one(bytes, 32, 64000)),
    mk(Op.Blake2b256, one(bytes, 32, 64000))
  )

  /** Rationals: the gcd that normalises them is quadratic in their size. */
  private val rationals: List[Case] = List(
    mk(Op.Add, num2(rat, 256, 2000), "rationals"),
    mk(Op.Mul, num2(rat, 256, 1000), "rationals"),
    mk(Op.Div, num2(rat, 256, 2000), "rationals"),
    mk(Op.Div, num2(int, 256, 4000), "integers"),
    mk(Op.Mod, num2(int, 256, 4000), "integers"),
    mk(Op.Lt, num2(rat, 256, 2000), "rationals")
  )

  private def limbs2(args: Vector[CanonicalValue]): Long =
    args.foldLeft(0L) { (n, v) =>
      val l = v match {
        case CInt(i)      => (i.bitLength / 64).toLong
        case CRational(r) => (r.numerator.bitLength / 64 + r.denominator.bitLength / 64).toLong
        case _            => 0L
      }
      n + l * l
    }

  private def fit(jlvm: Jlvm, c: Case): IO[String] =
    (opTime(jlvm, c.op, c.small), opTime(jlvm, c.op, c.large)).mapN { (ts, tl) =>
      val (us, ul) = (units(c.op, c.small), units(c.op, c.large))
      val per = if (ul > us) math.max((tl - ts) * Rate / (ul - us), 0.0) else 0.0
      val base = math.max(ts * Rate - per * us, 0.0)
      val cost = schedule.cost(c.op)
      f"${c.op.wire}%-12s suggest c(${math.round(base)}%7d, ${math.round(per)}%5d, ${cost.unit}%-8s) now c(${cost.base.value}%6d, ${cost.perUnit.value}%4d)" +
      f"  ${ts}%9.2f us / ${tl}%10.2f us  charged ${charged(c.op, c.small)}%7d / ${charged(c.op, c.large)}%9d  ${c.note}"
    }

  private def quadratic(jlvm: Jlvm, c: Case): IO[String] =
    (opTime(jlvm, c.op, c.small), opTime(jlvm, c.op, c.large)).mapN { (ts, tl) =>
      val (qs, ql) = (limbs2(c.small), limbs2(c.large))
      f"${c.op.wire}%-12s ${c.note}%-10s ${ts}%9.2f / ${tl}%10.2f us, limbs^2 $qs%6d / $ql%7d -> ${(tl - ts) * Rate / math.max(ql - qs, 1L)}%6.2f gas per limb^2"
    }

  /** Node charges: the time per item of growing programs. */
  private def nodes(jlvm: Jlvm): IO[List[String]] = {
    val d = m(
      Vector("x" -> CInt(1), "xs" -> ints(1000), "deep" -> (1 to 32).foldLeft[CanonicalValue](CInt(1))((v, _) => m(Vector("d" -> v))))
    )
    def per(label: String, small: Int, large: Int)(e: Int => Expr): IO[String] =
      Config.Driver.values.toList.traverse { driver =>
        val c = unlimited.copy(driver = driver)
        (micros(run(jlvm, e(small), d, c)), micros(run(jlvm, e(large), d, c))).mapN { (ts, tl) =>
          f"node $label%-28s ${driver.toString}%-12s ${(tl - ts) / (large - small)}%8.4f us each = ${(tl - ts) * Rate / (large - small)}%7.1f gas"
        }
      }.map(_.mkString("\n"))
    List(
      per("literal in an array", 10, 1000)(n => Expr.Arr(Vector.fill(n)(Expr.Lit(CInt(1))))),
      per("var read in an array", 10, 1000)(n => Expr.Arr(Vector.fill(n)(Expr.Var(VarRef.Data(Vector("x")), scala.None)))),
      per("path segment", 2, 32)(n => Expr.Var(VarRef.Data(Vector.fill(n)("d")), scala.None)),
      per("callback run (map, body 1)", 10, 1000)(n =>
        Expr.Apply(
          Op.Map,
          Vector(
            Expr.Apply(Op.Slice, Vector(Expr.Var(VarRef.Data(Vector("xs")), scala.None), Expr.Lit(CInt(0)), Expr.Lit(CInt(BigInt(n))))),
            Expr.Lit(CInt(1))
          )
        )
      ),
      per("if, untaken branches", 3, 201)(n =>
        Expr.Apply(Op.If, Vector.fill(n / 2)(Vector(Expr.Lit(False), Expr.Lit(CInt(1)))).flatten :+ Expr.Lit(CInt(2)))
      )
    ).sequence
  }

  def run(args: List[String]): IO[ExitCode] =
    Jlvm.load[IO].flatMap { jlvm =>
      for {
        _  <- IO.println(f"JLVM gas calibration at $Rate%.0f gas/us")
        ns <- nodes(jlvm)
        _  <- ns.traverse_(IO.println)
        os <- cases.traverse(fit(jlvm, _))
        _  <- os.traverse_(IO.println)
        qs <- rationals.traverse(quadratic(jlvm, _))
        _  <- qs.traverse_(IO.println)
      } yield ExitCode.Success
    }
}
