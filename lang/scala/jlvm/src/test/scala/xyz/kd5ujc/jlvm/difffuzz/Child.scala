package xyz.kd5ujc.jlvm.difffuzz

import java.io.{BufferedReader, File, InputStreamReader, OutputStream}
import java.lang.ProcessBuilder.Redirect
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all._

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** A started runner: its process, the pipe to its standard input and the reader of its standard output. */
final case class Proc(process: Process, in: OutputStream, out: BufferedReader)

/**
 * A runner in a child process (the Rust binary, or node on the TypeScript script), streamed to: a batch's lines are
 * written while its records are read, so neither pipe fills. A runner that dies, writes something that is not a
 * record, or takes longer than the batch's timeout is a fault of the case it was on: that case gets the fault, the
 * runner is restarted, and the rest of the batch runs again, so one crash never blames the cases after it.
 */
final class Child private (val lang: String, command: List[String], cwd: Path, log: File, state: Ref[IO, Option[Proc]]) {

  private val start: IO[Proc] =
    IO.blocking {
      val p = new ProcessBuilder(command.asJava).directory(cwd.toFile).redirectError(Redirect.appendTo(log)).start()
      Proc(p, p.getOutputStream, new BufferedReader(new InputStreamReader(p.getInputStream, UTF_8)))
    }

  private val proc: IO[Proc] = state.get.flatMap(_.fold(start.flatTap(p => state.set(Some(p))))(IO.pure))

  /** Stops the runner, if it runs; the next batch starts a fresh one. */
  val stop: IO[Unit] =
    state
      .getAndSet(None)
      .flatMap(_.traverse_ { p =>
        IO.blocking {
          p.in.close()
          if (!p.process.waitFor(2, java.util.concurrent.TimeUnit.SECONDS)) Child.kill(p.process)
        }.attempt.void
      })

  private def why(p: Proc): IO[String] =
    IO.blocking(
      if (p.process.waitFor(2, java.util.concurrent.TimeUnit.SECONDS)) s"no record, and it exited with ${p.process.exitValue()}"
      else "no record before the timeout: hung"
    )

  /**
   * Kills the runner once `stall` passes with no record (a case that takes that long has hung), or once `deadline`
   * passes. Checked every 200 ms against the time of the last record.
   */
  private def watch(p: Proc, last: Ref[IO, FiniteDuration], stall: FiniteDuration, deadline: FiniteDuration): IO[Unit] =
    (IO.sleep(200.millis) *> (IO.monotonic, last.get).tupled).flatMap {
      case (now, at) if now - at > stall || now >= deadline => IO.blocking(Child.kill(p.process))
      case _                                                => watch(p, last, stall, deadline)
    }

  /** The records the runner gave, in order, up to the first line it did not answer. */
  private def once(lines: Vector[String], stall: FiniteDuration, deadline: FiniteDuration): IO[(Vector[String], Option[String])] =
    proc.flatMap { p =>
      val write = IO.blocking {
        lines.foreach(l => p.in.write((l + "\n").getBytes(UTF_8)))
        p.in.flush()
      }.attempt
      def read(acc: Vector[String], last: Ref[IO, FiniteDuration]): IO[Vector[String]] =
        if (acc.size == lines.size) IO.pure(acc)
        else
          IO.blocking(Option(p.out.readLine())).attempt.flatMap {
            _.toOption.flatten.fold(IO.pure(acc))(r => IO.monotonic.flatMap(last.set) *> read(acc :+ r, last))
          }
      for {
        last   <- IO.monotonic.flatMap(Ref.of[IO, FiniteDuration])
        dog    <- watch(p, last, stall, deadline).start
        writer <- write.start
        // the watchdog stays until the writer is done too: a runner that answers without reading its input blocks the
        // writer on a full pipe, and only the watchdog's kill frees it
        both <- (read(Vector.empty, last), writer.joinWithNever).tupled.guarantee(dog.cancel)
        (got, written) = both
        // a runner whose input could not all be written is not reused
        _     <- stop.whenA(written.isLeft)
        fault <- if (got.size == lines.size) IO.pure(None) else why(p).flatTap(_ => stop).map(Some(_))
      } yield got -> fault
    }

  /**
   * One reply per line: the record's line, or the runner's fault on it. A case taking over `stall` has hung. Once
   * `deadline` passes, the runner is stopped and every line it has not answered is [[Child.Late]]: neither run nor
   * blamed, however many hang, so a run never outlives its `--minutes`.
   */
  def run(
    lines:    Vector[String],
    stall:    FiniteDuration = 60.seconds,
    deadline: FiniteDuration = Child.Never
  ): IO[Vector[Either[String, String]]] =
    if (lines.isEmpty) IO.pure(Vector.empty)
    else
      IO.monotonic.flatMap { now =>
        if (now >= deadline) IO.pure(lines.map(_ => Child.Late.asLeft))
        else
          once(lines, stall, deadline).flatMap {
            case (got, None) => IO.pure(got.map(_.asRight))
            case (got, Some(fault)) =>
              IO.monotonic.flatMap { after =>
                if (after >= deadline) IO.pure(got.map(_.asRight) ++ lines.drop(got.size).map(_ => Child.Late.asLeft))
                else
                  run(lines.drop(got.size + 1), stall, deadline).map(rest => (got.map(_.asRight) :+ s"$lang runner: $fault".asLeft) ++ rest)
              }
          }
      }
}

object Child {

  /** The reply to a line the run's deadline cut off: not a record, and not a fault of the case. */
  val Late: String = "cut off by the run's deadline"

  /** No deadline. */
  val Never: FiniteDuration = Long.MaxValue.nanos

  /**
   * Kills a runner and every process it started. A shell that forks its command (dash, unlike bash, does not exec the
   * last one) leaves the command holding the runner's stdout, so killing the shell alone never ends the read.
   */
  private def kill(p: Process): Unit = {
    p.descendants().forEach { h =>
      h.destroyForcibly(); ()
    }
    p.destroyForcibly()
    ()
  }

  def apply(lang: String, command: List[String], cwd: Path, log: File): Resource[IO, Child] =
    Resource.make(Ref.of[IO, Option[Proc]](None).map(new Child(lang, command, cwd, log, _)))(_.stop)
}
