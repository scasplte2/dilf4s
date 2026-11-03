package xyz.kd5ujc.benchmarks

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.concurrent.duration._

import xyz.kd5ujc.crypto.signing._
import xyz.kd5ujc.crypto.vrf._

import scodec.bits.ByteVector

/**
 * Verification time on the JVM through the pure-Scala backend and through dilf4s-signing's native library: Ed25519
 * ZIP-215 and each ECVRF suite, at two message sizes: `sbt "benchmarks/Test/runMain xyz.kd5ujc.benchmarks.Edwards25519Timing"`.
 */
object Edwards25519Timing extends IOApp {

  /** Microseconds per call, from batches of at least 500ms, after a warm-up of the same length. */
  private def micros(f: () => Boolean): IO[Double] = {
    def batch(k: Int): IO[(Int, FiniteDuration)] =
      IO.monotonic.flatMap(t0 => IO((1 to k).count(_ => f())).flatMap(_ => IO.monotonic.map(t1 => (k, t1 - t0))))
    def go(k: Int): IO[Double] =
      batch(k).flatMap { case (n, d) => if (d < 500.millis) go(k * 2) else IO.pure(d.toNanos.toDouble / 1000.0 / n.toDouble) }
    go(1) >> go(1)
  }

  private def line(what: String, pure: Double, native: Double): IO[Unit] =
    IO.println(f"$what%-42s pure Scala $pure%7.1f us, native $native%6.1f us (${pure / native}%.1fx)")

  private def vrf[S](pure: Vrf[S], native: Vrf[S], size: Long): IO[Unit] = {
    val kp = Vrf.fromSeed[S](ByteVector.fill(32)(7))(using pure).toOption.get
    val alpha = ByteVector.fill(size)(0x5a)
    val pi = pure.prove(kp.signing, alpha).toOption.get._1.bytes
    def time(v: Vrf[S]) = micros(() => v.verifyBytes(kp.verifying.bytes, alpha, pi).isRight)
    (time(pure), time(native)).flatMapN((p, n) => line(s"${pure.name}, $size-byte alpha:", p, n))
  }

  def run(args: List[String]): IO[ExitCode] =
    Edwards25519Backend
      .native[IO]
      .flatMap { native =>
        val reference = Edwards25519Backend.reference
        val kp = Ed25519.seedDerivation.fromSeed(ByteVector.fill(32)(7)).toOption.get
        List(32L, 1024L).traverse_ { size =>
          val m = ByteVector.fill(size)(0x5a)
          val sig = Ed25519.signer.sign(kp.signing, m)
          def time(b: Edwards25519Backend) = micros(() => b.verifyBytes(kp.verifying.bytes, m, sig.bytes) == Right(true))
          (time(reference), time(native)).flatMapN((p, n) => line(s"Ed25519, $size-byte message:", p, n)) >>
          vrf(reference.vrfTai, native.vrfTai, size) >>
          vrf(reference.vrfEll2, native.vrfEll2, size) >>
          vrf(reference.vrfDraft03, native.vrfDraft03, size)
        }
      }
      .as(ExitCode.Success)
}
