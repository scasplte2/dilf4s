package xyz.kd5ujc.benchmarks

import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import java.util.concurrent.Executors

import cats.effect.std.SecureRandom
import cats.effect.unsafe.IORuntimeConfig
import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all._

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.crypto.bls.Bls12381
import xyz.kd5ujc.crypto.signing.Secp256k1
import xyz.kd5ujc.jlvm._

import GasCases.GasCase

/**
 * Prices the JLVM's operations against the anchor, `secp256k1_verify`, by **ratio**, with common-mode rejection
 * (docs/specs/jlvm.md §7, "Calibration"). Every case is timed in short rounds in one thread. A round is
 *
 * `probe, s1, s2, reference, s2, s1, probe`, with `(s1, s2)` either (anchor, case) or (case, anchor), chosen at random,
 *
 * so it is a palindrome: drift that is linear across the round cancels in every ratio (case/anchor, case/reference,
 * reference/anchor). Each of anchor, case and reference is timed net of its baseline (the same argument reads) in
 * batches of about [[Target]] ns, so a round lasts a few milliseconds. The probe is a fixed compute-bound kernel: it
 * shows the core's speed during the round, so rounds on a throttled or migrated core can be rejected, and rounds on
 * performance and efficiency cores told apart.
 *
 * The harness prints raw rounds; `benchmarks/ratios.py` does the statistics (probe rejection, clustering by core speed,
 * medians with bootstrap intervals, the cross-check against the reference, agreement across processes). The Rust
 * harness (`sdk/rust/jlvm/examples/gas_ratios.rs`) reads the same cases (`export`) and runs the same rounds.
 *
 * `sbt "benchmarks/Test/runMain xyz.kd5ujc.benchmarks.GasRatios export <file>"`;
 * `... GasRatios measure [rounds] [regex]`, in the reference configuration ([[Reference]]) and several processes:
 * `benchmarks/calibrate.sh` runs both harnesses as the schedule's prices are derived.
 */
object GasRatios extends IOApp {

  /** No starvation checker: it is a fiber of its own, and nothing but the timed rounds may run while they do. */
  override def runtimeConfig: IORuntimeConfig = super.runtimeConfig.copy(cpuStarvationCheckInitialDelay = Duration.Inf)

  /** The thread every timed round runs on, alone: no other fiber is ever scheduled there. */
  private val dedicated: Resource[IO, ExecutionContext] =
    Resource.make(IO(Executors.newSingleThreadExecutor()))(e => IO(e.shutdown())).map(ExecutionContext.fromExecutor)

  private val unlimited = Config(limit = GasLimit(Gas(Long.MaxValue / 4)))

  /** Each batch of a unit takes at least this long. */
  val Target: Double = 2.5e5

  /**
   * The JVM configuration prices are derived under (docs/specs/jlvm.md §7): one CPU, a fixed 4 GiB heap touched in
   * full at start, and the serial collector, so every collection runs on the measuring core, inside the round of the
   * allocation that caused it. `measure` refuses to run in any other.
   */
  object Reference {
    val Heap: Long = 4L << 30
    val Flags: List[String] = List("-Xms4g", "-Xmx4g", "-XX:+UseSerialGC", "-XX:+AlwaysPreTouch")
    val Collectors: Set[String] = Set("Copy", "MarkSweepCompact")

    /** The configuration this JVM runs in, as the header names it, or what differs from the reference. */
    val check: IO[Either[String, String]] = IO {
      val heap = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage
      val flags = ManagementFactory.getRuntimeMXBean.getInputArguments.asScala.toList
      val collectors = ManagementFactory.getGarbageCollectorMXBeans.asScala.map(_.getName).toSet
      val cpus = Runtime.getRuntime.availableProcessors
      val wrong = List(
        // the reported maximum leaves out a survivor space; -Xmx4g is among the flags checked below
        Option.when(heap.getInit != Heap)(s"initial heap ${heap.getInit} bytes, not $Heap"),
        Option.when(!Flags.forall(flags.contains))(
          s"flags ${flags.mkString(" ")}, missing ${Flags.filterNot(flags.contains).mkString(" ")}"
        ),
        Option.when(collectors != Collectors)(s"collectors ${collectors.mkString(", ")}, not serial"),
        Option.when(cpus != 1)(s"$cpus CPUs, not pinned to one")
      ).flatten
      Either.cond(
        wrong.isEmpty,
        s"${Flags.mkString(" ")}; collectors ${Collectors.toList.sorted.mkString(", ")}; 1 CPU",
        wrong.mkString("; ")
      )
    }
  }

  /** Iterations of the probe: xorshift64, a dependent chain of shifts and xors, so its time is the core's speed. */
  val ProbeSteps: Int = 1 << 17

  final case class Prepared(c: GasCase, program: Compiled, baseline: Compiled, data: CanonicalValue) {
    def gas(jlvm:     Jlvm, p: Compiled): Long = jlvm.evaluate(p, data, Map.empty, unlimited).gasUsed.value
    def charged(jlvm: Jlvm): Long = gas(jlvm, program) - gas(jlvm, baseline) + c.adjust
  }

  private def prepare(c: GasCase): Either[JlvmError, Prepared] =
    (
      Parser.parse(c.program).flatMap(Compiled(_)),
      Parser.parse(c.baseline).flatMap(Compiled(_)),
      JsonSurface.parse(c.data)
    ).mapN(Prepared(c, _, _, _))

  @tailrec private def xorshift(k: Int, x: Long): Long =
    if (k == 0) x
    else {
      val a = x ^ (x << 13)
      val b = a ^ (a >>> 7)
      xorshift(k - 1, b ^ (b << 17))
    }

  private def loop(jlvm: Jlvm, c: Prepared, p: Compiled, n: Int): Long = {
    @tailrec def go(i: Int, sink: Long): Long = if (i >= n) sink else go(i + 1, sink + c.gas(jlvm, p))
    go(0, 0L)
  }

  private def timed(work: => Long): IO[(Long, Long)] =
    (IO.monotonic, IO(work), IO.monotonic).mapN((t0, sink, t1) => ((t1 - t0).toNanos, sink))

  /** Nanoseconds per evaluation over a batch of `n`. */
  private def batch(jlvm: Jlvm, c: Prepared, p: Compiled, n: Int): IO[Double] = timed(loop(jlvm, c, p, n)).map(_._1.toDouble / n)

  private def probe: IO[Double] = timed(xorshift(ProbeSteps, 0x9e3779b97f4a7c15L)).map(_._1.toDouble)

  /**
   * The batch size that takes at least `target` ns, doubling from 1, judged by the fastest of three batches at each
   * size (`tries`): one batch slowed by an interruption cannot stop the doubling early, and leave a case timed one
   * evaluation at a time (its timer and cold caches with it). The warm-up only needs the work done, once per size.
   */
  private def size(jlvm: Jlvm, c: Prepared, p: Compiled, target: Double, tries: Int = 3): IO[Int] = {
    def go(n: Int): IO[Int] =
      List.fill(tries)(batch(jlvm, c, p, n)).sequence.map(_.min).flatMap { ns =>
        if (ns * n >= target || n >= (1 << 24)) IO.pure(n) else go(n * 2)
      }
    go(1)
  }

  /** A case and its baseline, each with its batch size: timed as one net unit. */
  final case class Timed(c: Prepared, n: Int, nBase: Int) {
    def net(jlvm: Jlvm): IO[Double] = (batch(jlvm, c, c.program, n), batch(jlvm, c, c.baseline, nBase)).mapN(_ - _)
  }

  private def unit(jlvm: Jlvm, c: Prepared): IO[Timed] =
    (size(jlvm, c, c.program, Target), size(jlvm, c, c.baseline, Target)).mapN(Timed(c, _, _))

  /** The next order bit and state of a xorshift64 generator (fixed seed: runs are repeatable in their orders). */
  private def coin(state: Long): (Boolean, Long) = { val s = xorshift(1, state); ((s >>> 33 & 1L) == 1L, s) }

  /** One round: `S, index, round, order, probe1, probe2, anchor1, anchor2, case1, case2, reference` (ns). */
  private def round(jlvm: Jlvm, a: Timed, x: Timed, r: Timed, anchorFirst: Boolean): IO[Vector[Double]] = {
    val (s1, s2) = if (anchorFirst) (a, x) else (x, a)
    for {
      p1 <- probe
      u1 <- s1.net(jlvm)
      u2 <- s2.net(jlvm)
      rf <- r.net(jlvm)
      v2 <- s2.net(jlvm)
      v1 <- s1.net(jlvm)
      p2 <- probe
    } yield {
      val (aa, xx) = if (anchorFirst) ((u1, v1), (u2, v2)) else ((u2, v2), (u1, v1))
      Vector(p1, p2, aa._1, aa._2, xx._1, xx._2, rf)
    }
  }

  private def measure(jlvm: Jlvm, a: Timed, r: Timed, index: Int, c: Prepared, rounds: Int, seed: Long): IO[Long] =
    unit(jlvm, c).flatMap { x =>
      IO.println(List("C", index.toString, c.c.name, c.c.group, c.charged(jlvm).toString, x.n.toString, x.nBase.toString).mkString("\t")) *>
      (0 until rounds).toList.foldLeftM(seed) { (state, i) =>
        val (anchorFirst, next) = coin(state)
        round(jlvm, a, x, r, anchorFirst).flatMap { v =>
          IO.println((List("S", index.toString, i.toString, if (anchorFirst) "AXA" else "XAX") ++ v.map(d => f"$d%.1f")).mkString("\t"))
            .as(next)
        }
      }
    }

  /** Every case and baseline run for at least `nanos`: the JIT sees every operation before anything is timed. */
  private def warm(jlvm: Jlvm, cs: List[Prepared], nanos: Double): IO[Unit] =
    cs.traverse_(c => size(jlvm, c, c.program, nanos, 1) *> size(jlvm, c, c.baseline, nanos, 1).void)

  private def load: IO[(Jlvm, List[GasCase])] =
    for {
      jlvm  <- Jlvm.load[IO]
      secp  <- Secp256k1.load[IO]
      bls   <- Bls12381.load[IO]
      rnd   <- SecureRandom.javaSecuritySecureRandom[IO]
      cases <- GasCases.all(secp, bls, rnd)
    } yield (jlvm, cases)

  def run(args: List[String]): IO[ExitCode] =
    args match {
      // what each case is charged, and what its program evaluates to: a case that fails early, or a check that comes
      // out false, may not time the work it names
      case "check" :: Nil =>
        load.flatMap {
          case (jlvm, cases) =>
            IO.fromEither(cases.traverse(prepare).leftMap(e => new IllegalArgumentException(e.toString)))
              .flatMap(_.traverse_ { c =>
                val o = jlvm.evaluate(c.program, c.data, Map.empty, unlimited)
                val shown = o.result.fold(
                  e => s"error ${e.getClass.getSimpleName}",
                  v => if (v == CanonicalValue.CBool(false)) "false" else "ok"
                )
                IO.println(s"${c.c.name}\t${c.charged(jlvm)}\t$shown")
              })
        }.as(ExitCode.Success)
      case "export" :: path :: Nil =>
        load.flatMap {
          case (_, cases) => IO.blocking(Files.write(Paths.get(path), cases.map(_.line).mkString("", "\n", "\n").getBytes(UTF_8)))
        }.as(ExitCode.Success)
      case "measure" :: rest =>
        val rounds = rest.headOption.flatMap(_.toIntOption).getOrElse(101)
        val filter = rest.drop(1).headOption.filter(_.nonEmpty).map(_.r)
        (
          Reference.check.flatMap(_.leftMap(w => new IllegalStateException(s"not the reference configuration: $w")).liftTo[IO]),
          load
        ).flatMapN {
          case (configuration, (jlvm, cases)) =>
            for {
              prepared <- IO.fromEither(cases.traverse(prepare).leftMap(e => new IllegalArgumentException(e.toString)))
              (anchor, reference) = (prepared(0), prepared(1))
              chosen = prepared.zipWithIndex.drop(2).filter { case (c, _) => filter.forall(_.findFirstIn(c.c.name).isDefined) }
              _ <- IO.println(
                s"# backend JVM ${System.getProperty("java.version")}; ${chosen.size} cases; $rounds rounds; probe $ProbeSteps steps"
              )
              _ <- IO.println(s"# configuration $configuration")
              _ <- IO.println(s"# anchor ${anchor.charged(jlvm)}; reference ${reference.charged(jlvm)}")
              _ <- dedicated.use { ec =>
                (for {
                  _ <- warm(jlvm, anchor :: reference :: chosen.map(_._1), 2e8)
                  _ <- size(jlvm, anchor, anchor.program, 3e9, 1)
                  a <- unit(jlvm, anchor)
                  r <- unit(jlvm, reference)
                  _ <- chosen.foldLeftM(0x2545f4914f6cdd1dL) { case (seed, (c, i)) => measure(jlvm, a, r, i, c, rounds, seed) }
                } yield ()).evalOn(ec)
              }
            } yield ExitCode.Success
        }
      case _ => IO.println("usage: GasRatios export <file> | measure [rounds] [regex]").as(ExitCode.Error)
    }
}
