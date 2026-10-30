package xyz.kd5ujc.modelsgen

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.jdk.CollectionConverters._

/**
 * `Main <schemaDir> <outDir> <packagePrefix>`: every `<module>.cddl` in `schemaDir` becomes `<outDir>/<module>.scala`
 * in package `<packagePrefix>.<module>`. Any parse error or profile violation fails the build.
 */
object Main extends IOApp {

  private def cddlFiles(dir: Path): IO[Vector[Path]] =
    IO.blocking(Files.list(dir))
      .bracket(s => IO(s.iterator().asScala.toVector))(s => IO(s.close()))
      .map(_.filter(_.toString.endsWith(".cddl")).sortBy(_.toString))

  /** Parses `files` (module = file name without `.cddl`) and emits each module's source. */
  def generate(files: Vector[(String, String)], packagePrefix: String): Either[GenError, Vector[(String, String)]] =
    files.traverse { case (module, text) => CddlParser.parse(text).leftMap(GenError.Parse(module, _)).map(module -> Vector(_)) }.flatMap {
      parsed =>
        val emitter = new CanonicalEmitter(Schema.build(parsed), packagePrefix)
        parsed.traverse { case (module, _) => emitter.module(module).map(module -> _) }
    }

  def run(args: List[String]): IO[ExitCode] =
    args match {
      case schemaDir :: outDir :: packagePrefix :: Nil =>
        for {
          paths <- cddlFiles(Paths.get(schemaDir))
          texts <- paths.traverse(p => IO.blocking(Files.readString(p, UTF_8)).map(p.getFileName.toString.stripSuffix(".cddl") -> _))
          out   <- IO.fromEither(generate(texts, packagePrefix).leftMap(e => new IllegalArgumentException(e.message)))
          _     <- IO.blocking(Files.createDirectories(Paths.get(outDir)))
          _     <- out.traverse_ { case (m, src) => IO.blocking(Files.writeString(Paths.get(outDir, s"$m.scala"), src, UTF_8)) }
        } yield ExitCode.Success
      case _ => IO.println("usage: Main <schemaDir> <outDir> <packagePrefix>").as(ExitCode.Error)
    }
}
