package xyz.kd5ujc.benchmarks

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.concurrent.duration._

import xyz.kd5ujc.crypto.signing._

import scodec.bits.ByteVector

/**
 * Ed25519 ZIP-215 verification time on the JVM, through the pure-Scala backend and through dilf4s-signing's native
 * library, at two message sizes: `sbt "benchmarks/Test/runMain xyz.kd5ujc.benchmarks.Ed25519Timing"`.
 */
object Ed25519Timing extends IOApp {

  /** Microseconds per call, from batches of at least 500ms, after a warm-up of the same length. */
  private def micros(f: () => Boolean): IO[Double] = {
    def batch(k: Int): IO[(Int, FiniteDuration)] =
      IO.monotonic.flatMap(t0 => IO((1 to k).count(_ => f())).flatMap(_ => IO.monotonic.map(t1 => (k, t1 - t0))))
    def go(k: Int): IO[Double] =
      batch(k).flatMap { case (n, d) => if (d < 500.millis) go(k * 2) else IO.pure(d.toNanos.toDouble / 1000.0 / n.toDouble) }
    go(1) >> go(1)
  }

  def run(args: List[String]): IO[ExitCode] =
    Ed25519Backend
      .native[IO]
      .flatMap { native =>
        val kp = Ed25519.seedDerivation.fromSeed(ByteVector.fill(32)(7)).toOption.get
        List(32L, 1024L).traverse_ { size =>
          val m = ByteVector.fill(size)(0x5a)
          val sig = Ed25519.signer.sign(kp.signing, m)
          def time(b: Ed25519Backend) = micros(() => b.verifyBytes(kp.verifying.bytes, m, sig.bytes) == Right(true))
          (time(Ed25519Backend.reference), time(native)).flatMapN { (pure, rust) =>
            IO.println(f"$size%5d-byte message: pure Scala $pure%7.1f us, native $rust%6.1f us (${pure / rust}%.1fx)")
          }
        }
      }
      .as(ExitCode.Success)
}
