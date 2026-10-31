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

  private def compare(
    language: String,
    dir:      Path,
    suffix:   String,
    fresh:    Vector[(String, String)],
    command:  String = SdkSources.Regenerate
  ): IO[weaver.Expectations] =
    committed(dir, suffix).map { files =>
      val expected = fresh.toMap
      val stale = expected.collect { case (name, src) if !files.get(name).contains(src) => name }.toList.sorted
      val extra = files.keySet.diff(expected.keySet).toList.sorted
      expect(
        stale.isEmpty && extra.isEmpty,
        s"$language models in $dir are stale (${(stale ++ extra).mkString(", ")}); regenerate with: $command"
      )
    }

  test("committed TypeScript models match shared/schemas") {
    generated.flatMap(g => compare("TypeScript", SdkSources.TypeScriptDir, ".ts", g.typescript))
  }

  test("committed Rust models match shared/schemas") {
    generated.flatMap(g => compare("Rust", SdkSources.RustDir, ".rs", g.rust))
  }

  test("committed Rust emitter fixtures match their schemas") {
    SdkSources
      .readSchemas(SdkSources.RustFixtureSchemas)
      .flatMap(s => IO.fromEither(SdkSources.generate(s).leftMap(e => new IllegalStateException(e.message))))
      .flatMap(g => compare("Rust fixture", SdkSources.RustFixtureDir, ".rs", g.rust, SdkSources.RegenerateFixtures))
  }

  pureTest("an alias of another module's alias names that module's types from the consuming module") {
    val a = "item = { \"x\": uint }\nitems = [* item]\n"
    val b = ";# import a as a\nagain = a.items\n"
    val out = SdkSources.generate(Vector("a" -> a, "b" -> b)).map(_.rust.toMap.getOrElse("b.rs", ""))
    expect(
      out.exists(_.contains("pub const AGAIN: c::VecOf<c::ModelOf<super::a::Item>> = super::a::ITEMS;")),
      out.fold(_.message, identity)
    )
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

  test("a missing schema directory is an error, not an empty schema set (SdkMain would delete every model)") {
    SdkSources.readSchemas(java.nio.file.Paths.get("shared", "no-such-schemas")).attempt.map(r => expect(r.isLeft, r.toString))
  }

  pureTest("enumeration wire values beyond CDDL int are rejected in every language") {
    val cddl = "e = &(small: 1, large: 18446744073709551616)\n"
    expect(outside(gen(cddl)), "SDKs")
      .and(expect(outside(Main.generate(Vector("t" -> cddl), "p")), "Scala"))
      .and(expect(gen("e = &(low: -18446744073709551616, high: 18446744073709551615)\n").isRight, "the int range itself is fine"))
  }

  pureTest("generated names that collide or are reserved in any namespace are generator errors") {
    def clash(e: Either[GenError, ?]) = e.swap.exists { case GenError.NameClash(_, _, _) => true; case _ => false }
    expect(clash(gen("ab = uint\nAB = text\n")), "both codec constants would be AB in Rust")
      .and(expect(clash(gen("string = uint\nr = { \"s\": text }\n")), "a type String would shadow alloc's String"))
      .and(expect(clash(gen("self = { \"x\": uint }\n")), "Self is reserved in Rust"))
      .and(expect(clash(gen("result = { \"x\": uint }\n")), "a type Result would shadow the prelude's"))
      .and(expect(clash(SdkSources.generate(Vector("type" -> "r = uint\n"))), "a module named type"))
      .and(expect(clash(gen("readonly-map = { \"x\": { * text => uint } }\n")), "a type ReadonlyMap would shadow TypeScript's"))
  }

  pureTest("names with no valid identifier form are generator errors, not uncompilable output") {
    def failed(e: Either[GenError, ?]) = e.isLeft
    expect(failed(gen("r = \"self\" / \"other\"\n")), "a text alternative would be the variant Self")
      .and(expect(failed(gen("e = &(self: 1, other: 2)\n")), "an enumeration label would be the variant Self"))
      .and(expect(failed(gen("r = \"1x\" / \"other\"\n")), "a variant cannot start with a digit"))
      .and(expect(failed(gen("r = { \"1x\": uint }\n")), "nor a field"))
      .and(expect(failed(Main.generate(Vector("t" -> "r = { \"1x\": uint }\n"), "p")), "in any language"))
  }

  pureTest("keywords become raw identifiers in Rust") {
    val out = gen("r = { \"type\": uint, \"self\": uint }\n").map(_.rust.toMap.getOrElse("t.rs", ""))
    expect(out.exists(s => s.contains("pub r#type: u64,") && s.contains("pub self_: u64,")), out.fold(_.message, identity))
  }
}
