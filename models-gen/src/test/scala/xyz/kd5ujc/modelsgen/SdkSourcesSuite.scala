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
    expect
      .same(Left(GenError.NameClash("t", "AB", Vector("AB", "ab"))), gen("ab = uint\nAB = text\n"))
      .and(expect.same(Left(GenError.ReservedName("t", Language.Rust, "String")), gen("string = uint\nr = { \"s\": text }\n")))
      .and(expect.same(Left(GenError.ReservedName("t", Language.Rust, "Self")), gen("self = { \"x\": uint }\n")))
      .and(expect.same(Left(GenError.ReservedName("t", Language.Rust, "Result")), gen("result = { \"x\": uint }\n")))
      .and(expect.same(Left(GenError.ReservedName("type", Language.Rust, "type")), SdkSources.generate(Vector("type" -> "r = uint\n"))))
      .and(
        expect.same(
          Left(GenError.ReservedName("t", Language.TypeScript, "ReadonlyMap")),
          gen("readonly-map = { \"x\": { * text => uint } }\n")
        )
      )
  }

  private def models(module: String, cddl: String): Either[GenError, Map[QName, ModelRule]] =
    CddlParser
      .parse(cddl)
      .leftMap(GenError.Parse(module, _))
      .flatMap(f => new Profile(Schema.build(Vector(module -> Vector(f)))).module(module))
      .map(_.toMap)

  private def rust(cddl: String, module: String = "t"): Either[GenError, String] =
    models(module, cddl).flatMap(new RustEmitter(_).module(module))

  private def typescript(cddl: String, module: String = "t"): Either[GenError, String] =
    models(module, cddl).flatMap(new TsEmitter(_).module(module))

  pureTest("Rust: a name Rust keeps for itself is a ReservedName in Rust, not a clash with the other variants") {
    val okVariant = Left(GenError.ReservedName("t:r", Language.Rust, "Ok"))
    val okCase = Left(GenError.ReservedName("t:e", Language.Rust, "Ok"))
    expect
      .same(okVariant, rust("r = \"ok\" / \"malformed\" / \"rejected\"\n"))
      .and(expect.same(okCase, rust("e = &(ok: 1, rejected: 2)\n")))
      .and(expect.same(Left(GenError.ReservedName("t", Language.Rust, "Result")), rust("result = { \"x\": uint }\n")))
      .and(expect.same(Left(GenError.ReservedName("t", Language.Rust, "Vec")), rust("vec = { \"x\": uint }\n")))
      .and(expect.same(Left(GenError.ReservedName("type", Language.Rust, "type")), rust("r = uint\n", "type")))
      .and(expect.same(Left(GenError.ReservedName("c", Language.Rust, "c")), rust("r = uint\n", "c")))
      .and(expect(typescript("r = \"ok\" / \"malformed\" / \"rejected\"\n").isRight, "TypeScript reserves no text variant"))
      .and(expect(typescript("e = &(ok: 1, rejected: 2)\n").isRight, "nor an enumeration label"))
      .and(
        expect(
          okVariant.value.message == "in t:r the generated name `Ok` is reserved in Rust",
          s"the message names the language and only the reserved name: ${okVariant.value.message}"
        )
      )
  }

  pureTest("Rust: a clash lists only the rules that collide") {
    val byHand = Map(
      QName("t", "r") -> ModelRule.Record(
        Vector(
          ProfileField("aB", false, FieldType.Uint),
          ProfileField("a_b", false, FieldType.Uint),
          ProfileField("c", false, FieldType.Uint)
        )
      )
    )
    expect
      .same(Left(GenError.NameClash("t", "AB", Vector("AB", "ab"))), rust("ab = uint\nAB = text\nzz = uint\n"))
      .and(expect.same(Left(GenError.NameClash("t", "AB", Vector("a-b", "a_b"))), rust("a-b = uint\na_b = text\nzz = uint\n")))
      .and(
        expect.same(
          Left(GenError.NameClash("t:r", "A", Vector("a", "\"a\""))),
          rust("r = a / \"a\" / \"other\"\na = { \"x\": uint }\n")
        )
      )
      .and(expect.same(Left(GenError.NameClash("t:r", "a_b", Vector("aB", "a_b"))), new RustEmitter(byHand).module("t")))
  }

  pureTest("TypeScript: a name TypeScript keeps for itself is a ReservedName in TypeScript") {
    expect
      .same(
        Left(GenError.ReservedName("t", Language.TypeScript, "ReadonlyMap")),
        typescript("readonly-map = { \"x\": { * text => uint } }\n")
      )
      .and(expect.same(Left(GenError.ReservedName("class", Language.TypeScript, "class")), typescript("r = uint\n", "class")))
      .and(expect.same(Left(GenError.ReservedName("c", Language.TypeScript, "c")), typescript("r = uint\n", "c")))
      .and(expect(rust("readonly-map = { \"x\": { * text => uint } }\n").isRight, "Rust does not reserve ReadonlyMap"))
      .and(expect(rust("r = uint\n", "class").isRight, "nor a module named class"))
  }

  pureTest("TypeScript: a clash lists only the rules that collide") {
    val byHand = Map(
      QName("t", "r") -> ModelRule.Record(
        Vector(
          ProfileField("aB", false, FieldType.Uint),
          ProfileField("a_b", false, FieldType.Uint),
          ProfileField("c", false, FieldType.Uint)
        )
      )
    )
    expect
      .same(Left(GenError.NameClash("t", "AB", Vector("a-b", "a_b"))), typescript("a-b = uint\na_b = text\nzz = uint\n"))
      .and(expect.same(Left(GenError.NameClash("t:r", "aB", Vector("aB", "a_b"))), new TsEmitter(byHand).module("t")))
  }

  pureTest("a module written over a language's module list is a ReservedName; two modules on one file are a NameClash") {
    expect
      .same(Left(GenError.ReservedName("index", Language.TypeScript, "index.ts")), SdkSources.generate(Vector("index" -> "r = uint\n")))
      .and(
        expect.same(
          Left(GenError.NameClash("generated files", "a_b.rs", Vector("a-b", "a_b"))),
          SdkSources.generate(Vector("a-b" -> "r = uint\n", "a_b" -> "s = uint\n"))
        )
      )
  }

  pureTest("Scala and the shared profile: a clash lists only the rules or keys that collide") {
    expect
      .same(
        Left(GenError.NameClash("t", "AB", Vector("a-b", "a_b"))),
        Main.generate(Vector("t" -> "a-b = uint\na_b = text\nzz = uint\n"), "p")
      )
      .and(
        expect.same(
          Left(GenError.NameClash("t:r", "aB", Vector("aB", "a_b"))),
          Main.generate(Vector("t" -> "r = { \"aB\": uint, \"a_b\": uint, \"c\": uint }\n"), "p")
        )
      )
  }

  pureTest("names with no valid identifier form are generator errors, not uncompilable output") {
    def failed(e: Either[GenError, ?]) = e.isLeft
    expect(failed(gen("r = \"self\" / \"other\"\n")), "a text alternative would be the variant Self")
      .and(expect(failed(gen("e = &(self: 1, other: 2)\n")), "an enumeration label would be the variant Self"))
      .and(expect(failed(gen("r = \"1x\" / \"other\"\n")), "a variant cannot start with a digit"))
      .and(expect(failed(gen("r = { \"1x\": uint }\n")), "nor a field"))
      .and(expect(failed(Main.generate(Vector("t" -> "r = { \"1x\": uint }\n"), "p")), "in any language"))
  }

  pureTest("modules whose output files would collide are generator errors, not overwritten files") {
    expect(SdkSources.generate(Vector("index" -> "r = uint\n")).isLeft, "index.ts is the barrel")
      .and(expect(SdkSources.generate(Vector("mod" -> "r = uint\n")).isLeft, "mod.rs is the module list"))
      .and(expect(SdkSources.generate(Vector("a-b" -> "r = uint\n", "a_b" -> "s = uint\n")).isLeft, "a-b and a_b are both a_b"))
  }

  pureTest("field names that are not usable in every language are generator errors") {
    expect(gen("r = { \"_\": uint }\n").isLeft, "_ is not a field name in Rust or Scala")
      .and(expect(gen("empty = {}\nnamed = { \"toString\": text }\nu = empty / named\n").isLeft, "toString is every object's"))
  }

  pureTest("Rust imports what the types use, not what names contain") {
    val out = gen("byte-string = bytes\n").map(_.rust.toMap.getOrElse("t.rs", ""))
    expect(out.exists(s => !s.contains("use alloc::string::String;") && s.contains("use alloc::vec::Vec;")), out.fold(_.message, identity))
  }

  pureTest("keywords become raw identifiers in Rust") {
    val out = gen("r = { \"type\": uint, \"self\": uint }\n").map(_.rust.toMap.getOrElse("t.rs", ""))
    expect(out.exists(s => s.contains("pub r#type: u64,") && s.contains("pub self_: u64,")), out.fold(_.message, identity))
  }

  private val constants = "max-depth = 256\nmax-weight = 9223372036854775807\nmax-uint = 18446744073709551615\n"

  pureTest("a uint rule is a constant in every language, with nothing imported") {
    val out = gen(constants)
    val rust = out.map(_.rust.toMap.getOrElse("t.rs", ""))
    val ts = out.map(_.typescript.toMap.getOrElse("t.ts", ""))
    val scala = Main.generate(Vector("t" -> constants), "p").map(_.foldMap(_._2))
    expect(rust.exists(_.contains("pub const MAX_DEPTH: u64 = 256;")), rust.fold(_.message, identity))
      .and(expect(rust.exists(_.contains("pub const MAX_UINT: u64 = 18_446_744_073_709_551_615;")), "a uint fits u64"))
      .and(
        expect(
          rust.exists(_.contains("pub mod as_usize {\n    /// [`super::MAX_DEPTH`] as a `usize`.\n    pub const MAX_DEPTH: usize = 256;")),
          "narrowed"
        )
      )
      .and(expect(rust.exists(_.contains("    pub const MAX_DEPTH: u16 = 256;")), "a u16"))
      .and(expect(rust.exists(s => !s.contains("MAX_WEIGHT: u32") && !s.contains("MAX_WEIGHT: usize")), "only where it fits"))
      .and(expect(rust.exists(s => !s.contains("use ")), "a constants module imports nothing"))
      .and(expect(ts.exists(_.contains("export const MAX_DEPTH = 256;")), ts.fold(_.message, identity)))
      .and(expect(ts.exists(_.contains("export const MAX_WEIGHT = 9223372036854775807n;")), "past 2^53 - 1, a bigint"))
      .and(expect(ts.exists(s => !s.contains("import ")), "a constants module imports nothing"))
      .and(expect(scala.exists(_.contains("final val MaxDepth = 256\n")), scala.fold(_.message, identity)))
      .and(expect(scala.exists(_.contains("final val MaxWeight = 9223372036854775807L\n")), "a Long"))
      .and(expect(scala.exists(_.contains("val MaxUint: BigInt = BigInt(\"18446744073709551615\")")), "past Long, a BigInt"))
      .and(expect(scala.exists(s => !s.contains("import ")), "a constants module imports nothing"))
  }

  pureTest("Rust: a choice of text literals lists every alternative in ALL; a choice with records lists its texts") {
    val out = gen("kind = \"a\" / \"b-c\"\nr = { \"x\": uint }\nmixed = r / \"d\"\nrecords = r\n").map(_.rust.toMap.getOrElse("t.rs", ""))
    expect(out.exists(_.contains("pub const ALL: [Self; 2] = [Self::A, Self::BC];")), out.fold(_.message, identity))
      .and(expect(out.exists(_.contains("pub const TEXTS: [Self; 1] = [Self::D];")), "the texts beside a record"))
      .and(expect(out.exists(s => !s.contains("pub const ALL: [Self; 1]")), "no ALL for a choice with a record"))
  }

  pureTest("a constant bounds a range or a size, and is never a field's type") {
    val bounded = gen("max-n = 1024\nr = { \"n\": 0..max-n, \"b\": bytes .size (0..max-n) }\n").map(_.rust.toMap.getOrElse("t.rs", ""))
    expect(bounded.exists(_.contains("c::U32In { lo: 0, hi: 1024 }")), bounded.fold(_.message, identity))
      .and(expect(bounded.exists(_.contains("c::SizedBytes { lo: 0, hi: Some(1024) }")), "a size bound"))
      .and(expect(outside(gen("max-n = 1024\nr = { \"n\": max-n }\n")), "a constant as a field type"))
      .and(expect(outside(gen("max-n = 1024\nr = [* max-n]\n")), "a constant as an element type"))
      .and(expect(outside(Main.generate(Vector("t" -> "max-n = 1024\nr = { \"n\": max-n }\n"), "p")), "and in Scala"))
      .and(expect(outside(gen("min-n = -1\n")), "a negative constant"))
  }
}
