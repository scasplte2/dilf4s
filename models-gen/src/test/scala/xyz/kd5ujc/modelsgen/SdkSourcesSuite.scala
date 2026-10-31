package xyz.kd5ujc.modelsgen

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.syntax.all._

import weaver.SimpleIOSuite

/**
 * The TypeScript and Rust models committed under `sdk/` are exactly what the generator makes of `shared/schemas`
 * today: same files, same bytes, nothing left over from a removed schema. CI runs this, so stale SDK models fail it.
 */
object SdkSourcesSuite extends SimpleIOSuite {

  private def committed(dir: Path, suffix: String): IO[Map[String, String]] =
    SdkSources
      .listFiles(dir, suffix)
      .flatMap(_.traverse(p => IO.blocking(Files.readString(p, UTF_8)).map(p.getFileName.toString -> _)))
      .map(_.toMap)

  private val generated: IO[SdkSources] =
    SdkSources
      .readSchemas(java.nio.file.Paths.get("shared", "schemas"))
      .flatMap(s => IO.fromEither(SdkSources.generate(s).leftMap(e => new IllegalStateException(e.message))))

  private def compare(language: String, dir: Path, suffix: String, fresh: Vector[(String, String)]): IO[weaver.Expectations] =
    committed(dir, suffix).map { files =>
      val expected = fresh.toMap
      val stale = expected.collect { case (name, src) if !files.get(name).contains(src) => name }.toList.sorted
      val extra = files.keySet.diff(expected.keySet).toList.sorted
      expect(
        stale.isEmpty && extra.isEmpty,
        s"$language models in $dir are stale (${(stale ++ extra).mkString(", ")}); regenerate with: ${SdkSources.Regenerate}"
      )
    }

  test("committed TypeScript models match shared/schemas") {
    generated.flatMap(g => compare("TypeScript", SdkSources.TypeScriptDir, ".ts", g.typescript))
  }

  test("committed Rust models match shared/schemas") {
    generated.flatMap(g => compare("Rust", SdkSources.RustDir, ".rs", g.rust))
  }

  private def gen(cddl: String): Either[GenError, SdkSources] = SdkSources.generate(Vector("t" -> cddl))

  private def outside(e: Either[GenError, ?]): Boolean =
    e.swap.exists { case GenError.Unsupported(_, UnsupportedFeature.OutsideProfile(_)) => true; case _ => false }

  pureTest("a .regexp outside the shared patterns is a generator error in every language") {
    val cddl = "r = text .regexp \"[a-z]+\"\n"
    expect(outside(gen(cddl)), "SDKs").and(expect(outside(Main.generate(Vector("t" -> cddl), "p")), "Scala"))
  }

  pureTest("Rust boxes a field that holds its own type, and nothing else") {
    val out =
      gen("e = a / b\na = { \"x\": e, \"y\": f }\nb = { \"z\": uint }\nf = { \"w\": uint }\n").map(_.rust.toMap.getOrElse("t.rs", ""))
    expect(out.exists(_.contains("pub x: Box<E>,")), out.fold(_.message, identity))
      .and(expect(out.exists(_.contains("pub y: F,")), "an acyclic field is held by value"))
  }

  pureTest("names that collide in one language are errors there") {
    val clash = gen("r = { \"aB\": uint, \"a_b\": uint }\n")
    expect(clash.swap.exists { case GenError.NameClash(_, _, _) => true; case _ => false }, "aB and a_b are both a_b in Rust")
  }

  pureTest("a union whose alternatives differ only in wire spelling is ambiguous once names are normalised") {
    val cddl = "r = a / b\na = { \"x-y\": uint }\nb = { \"x_y\": uint }\n"
    def ambiguous(e: Either[GenError, ?]) =
      e.swap.exists { case GenError.Unsupported(_, UnsupportedFeature.AmbiguousAlternatives(_)) => true; case _ => false }
    expect(ambiguous(gen(cddl)), "TypeScript tells alternatives apart by camelCase field names")
      .and(expect(ambiguous(Main.generate(Vector("t" -> cddl), "p")), "the profile is the same for Scala"))
  }

  pureTest("record fields that collide in any language's naming are rejected in every language") {
    // "AB" and "ab" stay apart in camelCase but are both `ab` in Rust
    val cddl = "r = { \"AB\": uint, \"ab\": uint }\n"
    def clash(e: Either[GenError, ?]) = e.swap.exists { case GenError.NameClash(_, _, _) => true; case _ => false }
    expect(clash(gen(cddl)), "SDKs").and(expect(clash(Main.generate(Vector("t" -> cddl), "p")), "Scala"))
  }

  pureTest("keywords become raw identifiers in Rust") {
    val out = gen("r = { \"type\": uint, \"self\": uint }\n").map(_.rust.toMap.getOrElse("t.rs", ""))
    expect(out.exists(s => s.contains("pub r#type: c::BigInt,") && s.contains("pub self_: c::BigInt,")), out.fold(_.message, identity))
  }
}
