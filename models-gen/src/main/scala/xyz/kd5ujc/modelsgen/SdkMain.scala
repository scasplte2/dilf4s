package xyz.kd5ujc.modelsgen

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.jdk.CollectionConverters._

/** The generated SDK sources: file name to content, for each language's `generated` directory. */
final case class SdkSources(typescript: Vector[(String, String)], rust: Vector[(String, String)])

object SdkSources {

  /** Where the committed output lives, relative to the repository root. */
  val TypeScriptDir: Path = Paths.get("sdk", "ts", "packages", "core", "src", "generated")
  val RustDir: Path = Paths.get("sdk", "rust", "core", "src", "generated")

  /** The command that rewrites the committed output. */
  val Regenerate: String =
    "sbt \"modelsGen/runMain xyz.kd5ujc.modelsgen.SdkMain shared/schemas sdk/ts/packages/core/src/generated sdk/rust/core/src/generated\""

  /** Parses `files` (module = file name without `.cddl`) and emits every module in TypeScript and Rust. */
  def generate(files: Vector[(String, String)]): Either[GenError, SdkSources] =
    files.traverse { case (module, text) => CddlParser.parse(text).leftMap(GenError.Parse(module, _)).map(module -> Vector(_)) }.flatMap {
      parsed =>
        val profile = new Profile(Schema.build(parsed))
        val modules = parsed.map(_._1)
        modules.flatTraverse(m => profile.module(m)).flatMap { rules =>
          val models = rules.toMap
          val ts = new TsEmitter(models)
          val rs = new RustEmitter(models)
          (
            modules.traverse(m => ts.module(m).map(s"${CanonicalEmitter.packageSegment(m)}.ts" -> _)),
            modules.traverse(m => rs.module(m).map(s"${CanonicalEmitter.packageSegment(m)}.rs" -> _))
          ).mapN((t, r) =>
            SdkSources((t :+ ("index.ts" -> ts.index(modules))).sortBy(_._1), (r :+ ("mod.rs" -> rs.index(modules))).sortBy(_._1))
          )
        }
    }

  def readSchemas(dir: Path): IO[Vector[(String, String)]] =
    listFiles(dir, ".cddl").flatMap(
      _.traverse(p => IO.blocking(Files.readString(p, UTF_8)).map(p.getFileName.toString.stripSuffix(".cddl") -> _))
    )

  def listFiles(dir: Path, suffix: String): IO[Vector[Path]] =
    IO.blocking(Files.isDirectory(dir))
      .ifM(
        ifFalse = IO.pure(Vector.empty),
        ifTrue = IO
          .blocking(Files.list(dir))
          .bracket(s => IO(s.iterator().asScala.toVector))(s => IO(s.close()))
          .map(_.filter(_.toString.endsWith(suffix)).sortBy(_.toString))
      )
}

/**
 * `SdkMain <schemaDir> <tsOutDir> <rustOutDir>`: writes the TypeScript and Rust models of every `<module>.cddl` in
 * `schemaDir`, and removes generated files of modules that no longer exist. Any parse error or profile violation fails.
 */
object SdkMain extends IOApp {

  private def write(dir: Path, suffix: String, files: Vector[(String, String)]): IO[Unit] =
    for {
      _       <- IO.blocking(Files.createDirectories(dir))
      present <- SdkSources.listFiles(dir, suffix)
      keep = files.map(_._1).toSet
      _ <- present.filterNot(p => keep(p.getFileName.toString)).traverse_(p => IO.blocking(Files.delete(p)))
      _ <- files.traverse_ { case (name, src) => IO.blocking(Files.writeString(dir.resolve(name), src, UTF_8)) }
    } yield ()

  def run(args: List[String]): IO[ExitCode] =
    args match {
      case schemaDir :: tsDir :: rustDir :: Nil =>
        for {
          schemas <- SdkSources.readSchemas(Paths.get(schemaDir))
          out     <- IO.fromEither(SdkSources.generate(schemas).leftMap(e => new IllegalArgumentException(e.message)))
          _       <- write(Paths.get(tsDir), ".ts", out.typescript)
          _       <- write(Paths.get(rustDir), ".rs", out.rust)
        } yield ExitCode.Success
      case _ => IO.println("usage: SdkMain <schemaDir> <tsOutDir> <rustOutDir>").as(ExitCode.Error)
    }
}
