package xyz.kd5ujc.modelsgen

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.jdk.CollectionConverters._

/**
 * The generated SDK sources: file name to content, for each language's `generated` directory; for Go, path under
 * `sdk/go` to content (a package per module, [[GoEmitter.pathOf]]).
 */
final case class SdkSources(
  typescript: Vector[(String, String)],
  rust:       Vector[(String, String)],
  go:         Vector[(String, String)] = Vector.empty
)

object SdkSources {

  /** Where the committed output lives, relative to the repository root. */
  val TypeScriptDir: Path = Paths.get("sdk", "ts", "packages", "core", "src", "generated")
  val RustDir: Path = Paths.get("sdk", "rust", "core", "src", "generated")
  val GoDir: Path = Paths.get("sdk", "go")

  /** Fixture schemas for the Rust emitter, and the crate that compiles their models (`sdk/rust/fixtures`). */
  val RustFixtureSchemas: Path = Paths.get("sdk", "rust", "fixtures", "schemas")
  val RustFixtureDir: Path = Paths.get("sdk", "rust", "fixtures", "src", "generated")
  val RegenerateFixtures: String =
    "sbt \"modelsGen/runMain xyz.kd5ujc.modelsgen.SdkMain sdk/rust/fixtures/schemas - sdk/rust/fixtures/src/generated\""

  /**
   * Fixture schemas for the Go emitter, which exercise what the shared schemas do not (a record holding itself, a
   * choice of another package's record and texts, a wide enumeration, an error wrapping another), and the package root
   * their models are written to, under `sdk/go`, where `go test` compiles and runs them.
   */
  val GoFixtureSchemas: Path = Paths.get("sdk", "go", "internal", "fixtures", "schemas")
  val GoFixtureRoot: String = "internal/fixtures"
  val RegenerateGoFixtures: String =
    "sbt \"modelsGen/runMain xyz.kd5ujc.modelsgen.SdkMain sdk/go/internal/fixtures/schemas - - sdk/go internal/fixtures\""

  /** The command that rewrites the committed output. */
  val Regenerate: String =
    "sbt \"modelsGen/runMain xyz.kd5ujc.modelsgen.SdkMain shared/schemas sdk/ts/packages/core/src/generated sdk/rust/core/src/generated sdk/go\""

  /** The file each language's generated directory keeps for its module list, which no module may be written to. */
  private val IndexFiles: Map[String, Language] = Map("index.ts" -> Language.TypeScript, "mod.rs" -> Language.Rust)

  /** Parses `files` (module = file name without `.cddl`) and emits every module in TypeScript, Rust and Go. */
  def generate(files: Vector[(String, String)]): Either[GenError, SdkSources] =
    files.traverse { case (module, text) => CddlParser.parse(text).leftMap(GenError.Parse(module, _)).map(module -> Vector(_)) }.flatMap {
      parsed =>
        val profile = new Profile(Schema.build(parsed))
        val modules = parsed.map(_._1)
        modules.flatTraverse(m => profile.module(m)).flatMap { rules =>
          val models = rules.toMap
          val ts = new TsEmitter(models, profile.schema.doc)
          val rs = new RustEmitter(models, profile.schema.doc)
          (
            modules.traverse(m => ts.module(m).map(s"${CanonicalEmitter.packageSegment(m)}.ts" -> _)),
            modules.traverse(m => rs.module(m).map(s"${CanonicalEmitter.packageSegment(m)}.rs" -> _))
          ).mapN((t, r) =>
            SdkSources((t :+ ("index.ts" -> ts.index(modules))).sortBy(_._1), (r :+ ("mod.rs" -> rs.index(modules))).sortBy(_._1))
          ).flatMap { out =>
            // two modules (or a module and an index) writing the same file would silently overwrite one another
            val files = out.typescript.map(_._1) ++ out.rust.map(_._1)
            files.groupBy(identity).toVector.sortBy(_._1).collectFirst { case (f, fs) if fs.size > 1 => f }.fold(out.asRight[GenError]) {
              f =>
                val owners = modules.filter(m => f.startsWith(CanonicalEmitter.packageSegment(m) + "."))
                IndexFiles
                  .get(f)
                  .fold(GenError.NameClash("generated files", f, owners))(l => GenError.ReservedName(owners.mkString(", "), l, f))
                  .asLeft
            }
          }.flatMap { out =>
            // Go last, so the TypeScript and Rust refusals of a schema stay what they were
            go(models, profile.schema.doc, modules, GoEmitter.DefaultRoot).map(g => out.copy(go = g))
          }
        }
    }

  /** The Go sources of `modules`, under `root`: each module's package, and the test listing every model. */
  private def go(
    models:  Map[QName, ModelRule],
    docs:    QName => Vector[String],
    modules: Vector[String],
    root:    String,
    schemas: String = "shared/schemas"
  ): Either[GenError, Vector[(String, String)]] = {
    val gos = new GoEmitter(models, docs, root, schemas)
    gos.check(modules).toLeft(()).flatMap(_ => modules.traverse(gos.module)).map(g => (g :+ gos.registry(modules)).sortBy(_._1))
  }

  /** The Go sources alone of `files`, their packages under `root` (the emitter fixtures'). */
  def generateGo(files: Vector[(String, String)], root: String, schemas: String): Either[GenError, Vector[(String, String)]] =
    files.traverse { case (module, text) => CddlParser.parse(text).leftMap(GenError.Parse(module, _)).map(module -> Vector(_)) }.flatMap {
      parsed =>
        val profile = new Profile(Schema.build(parsed))
        val modules = parsed.map(_._1)
        modules.flatTraverse(m => profile.module(m)).flatMap(rules => go(rules.toMap, profile.schema.doc, modules, root, schemas))
    }

  /** Every schema in `dir`; a missing directory, or one with no schemas, is an error (SdkMain would empty its output). */
  def readSchemas(dir: Path): IO[Vector[(String, String)]] =
    listFiles(dir, ".cddl")
      .flatMap(ps =>
        IO.raiseWhen(ps.isEmpty)(new java.io.FileNotFoundException(s"no .cddl schemas in $dir"))
          .as(ps)
      )
      .flatMap(
        _.traverse(p => IO.blocking(Files.readString(p, UTF_8)).map(p.getFileName.toString.stripSuffix(".cddl") -> _))
      )

  /**
   * Every file under `dir/scope` (recursively) that the Go emitter writes, as its path relative to `dir`, `/`-separated:
   * a root's run owns its scope ([[GoEmitter.scope]]) alone.
   */
  def listGenerated(dir: Path, scope: String): IO[Vector[String]] =
    IO.blocking(Files.isDirectory(dir.resolve(scope)))
      .ifM(
        ifFalse = IO.pure(Vector.empty),
        ifTrue = IO
          .blocking(Files.walk(dir.resolve(scope)))
          .bracket(s => IO(s.iterator().asScala.toVector))(s => IO(s.close()))
          .map(
            _.filter(p => Files.isRegularFile(p))
              .map(p => dir.relativize(p).iterator().asScala.map(_.toString).mkString("/"))
              .filter(GoEmitter.generated)
              .sorted
          )
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
 * `SdkMain <schemaDir> <tsOutDir> <rustOutDir> [<goDir>]`: writes the TypeScript and Rust models of every
 * `<module>.cddl` in `schemaDir` and, given `goDir` (`sdk/go`), the Go models under it; and removes generated files of
 * modules that no longer exist. A `tsOutDir` of `-` writes Rust only (the Rust emitter fixtures).
 * `SdkMain <schemaDir> - - <goDir> <goRoot>` writes Go only, its packages under `goDir/goRoot` (the Go emitter
 * fixtures). Any parse error or profile violation fails.
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

  /** Whether `dir` holds no entries. */
  private def empty(dir: Path): IO[Boolean] =
    IO.blocking(Files.list(dir)).bracket(s => IO(s.findAny().isEmpty))(s => IO(s.close()))

  /** Writes the Go files under `dir`, removing the generated files no module writes, then directories they emptied. */
  private def writeGo(dir: Path, root: String, files: Vector[(String, String)]): IO[Unit] =
    for {
      present <- SdkSources.listGenerated(dir, GoEmitter.scope(root))
      keep = files.map(_._1).toSet
      stale = present.filterNot(keep).map(f => dir.resolve(f))
      _ <- stale.traverse_(p => IO.blocking(Files.delete(p)))
      _ <- stale.map(_.getParent).distinct.traverse_(d => empty(d).ifM(ifTrue = IO.blocking(Files.delete(d)), ifFalse = IO.unit))
      _ <- files.traverse_ {
        case (name, src) =>
          val p = dir.resolve(name)
          IO.blocking(Files.createDirectories(p.getParent)) >> IO.blocking(Files.writeString(p, src, UTF_8)).void
      }
    } yield ()

  private def failed(e: GenError): Throwable = new IllegalArgumentException(e.message)

  def run(args: List[String]): IO[ExitCode] =
    args match {
      case schemaDir :: "-" :: "-" :: goDir :: goRoot :: Nil =>
        for {
          schemas <- SdkSources.readSchemas(Paths.get(schemaDir))
          out     <- IO.fromEither(SdkSources.generateGo(schemas, goRoot, schemaDir.stripSuffix("/")).leftMap(failed))
          _       <- writeGo(Paths.get(goDir), goRoot, out)
        } yield ExitCode.Success
      case schemaDir :: tsDir :: rustDir :: goDir if goDir.size <= 1 =>
        for {
          schemas <- SdkSources.readSchemas(Paths.get(schemaDir))
          out     <- IO.fromEither(SdkSources.generate(schemas).leftMap(failed))
          _       <- write(Paths.get(tsDir), ".ts", out.typescript).unlessA(tsDir == "-")
          _       <- write(Paths.get(rustDir), ".rs", out.rust)
          _       <- goDir.traverse_(d => writeGo(Paths.get(d), GoEmitter.DefaultRoot, out.go))
        } yield ExitCode.Success
      case _ =>
        IO.println("usage: SdkMain <schemaDir> <tsOutDir | -> <rustOutDir> [<goDir>], or SdkMain <schemaDir> - - <goDir> <goRoot>")
          .as(ExitCode.Error)
    }
}
