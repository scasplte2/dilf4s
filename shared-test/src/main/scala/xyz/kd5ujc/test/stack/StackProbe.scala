package xyz.kd5ujc.test.stack

import cats.effect.IO

import weaver.Expectations

/**
 * Runs a read on a thread with a stack of its own size, so that a `StackOverflowError` is a failed expectation, not
 * a crashed test JVM. Suites named `*StackSuite` use it, alone in a cold JVM (build.sbt), so the code is read before
 * the JIT has compiled it, when its frames are largest.
 */
object StackProbe {

  /** Half of a normal JVM stack (1 MiB): what each of the deepest reads must fit in. */
  val Bytes: Long = 512L * 1024

  /** `f` on a new thread of [[Bytes]] stack: its result, or what it threw. */
  def run[A](f: => A): IO[Either[Throwable, A]] =
    IO.async_[Either[Throwable, A]] { cb =>
      Thread
        .ofPlatform()
        .name("stack-probe")
        .stackSize(Bytes)
        .uncaughtExceptionHandler((_, e) => cb(Right(Left(e))))
        .start(() => cb(Right(Right(f))))
      ()
    }

  /** [[run]], then `check` of the result; what was thrown fails, named. */
  def fits[A](what: String)(f: => A)(check: A => Expectations): IO[Expectations] =
    run(f).map(_.fold(e => Expectations.Helpers.failure(s"$what: ${e.getClass.getName} on a ${Bytes / 1024} KiB stack"), check))
}
