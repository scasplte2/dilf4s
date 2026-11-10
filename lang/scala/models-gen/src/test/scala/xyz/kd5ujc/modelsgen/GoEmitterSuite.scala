package xyz.kd5ujc.modelsgen

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import weaver.SimpleIOSuite

/**
 * The Go emitter: the committed Go models under `lang/go` are exactly what the generator makes of `shared/schemas`
 * (same files, same bytes, nothing left over from a removed schema; CI runs this, so stale Go models fail it), the
 * reviewed golden snapshot of the modules the Scala golden covers, and the shapes and refusals the Go output relies on.
 */
object GoEmitterSuite extends SimpleIOSuite {

  private val generated: IO[SdkSources] =
    SdkSources
      .readSchemas(Paths.get("shared", "schemas"))
      .flatMap(s => IO.fromEither(SdkSources.generate(s).leftMap(e => new IllegalStateException(e.message))))

  /** The committed generated Go files of `root`'s scope are exactly `fresh`. */
  private def compare(fresh: Vector[(String, String)], root: String, command: String): IO[weaver.Expectations] =
    for {
      committed <- SdkSources.listGenerated(SdkSources.GoDir, GoEmitter.scope(root))
      files     <- committed.traverse(f => IO.blocking(Files.readString(SdkSources.GoDir.resolve(f), UTF_8)).map(f -> _))
    } yield {
      val expected = fresh.toMap
      val present = files.toMap
      val stale = expected.collect { case (name, src) if !present.get(name).contains(src) => name }.toList.sorted
      val extra = present.keySet.diff(expected.keySet).toList.sorted
      expect(
        stale.isEmpty && extra.isEmpty,
        s"Go models under ${SdkSources.GoDir}/${GoEmitter.scope(root)} are stale (${(stale ++ extra).mkString(", ")}); regenerate with: $command"
      )
    }

  test("committed Go models match shared/schemas") {
    generated.flatMap { g =>
      compare(g.go, GoEmitter.DefaultRoot, SdkSources.Regenerate).map(
        _.and(expect(g.go.size > 30, s"a package for every schema: ${g.go.size}"))
      )
    }
  }

  test("committed Go emitter fixtures match their schemas") {
    SdkSources
      .readSchemas(SdkSources.GoFixtureSchemas)
      .flatMap(s =>
        IO.fromEither(
          SdkSources
            .generateGo(s, SdkSources.GoFixtureRoot, "lang/go/internal/fixtures/schemas")
            .leftMap(e => new IllegalStateException(e.message))
        )
      )
      .flatMap(g => compare(g, SdkSources.GoFixtureRoot, SdkSources.RegenerateGoFixtures))
  }

  /** The modules of the Scala golden snapshot (CanonicalEmitterSuite). */
  private val goldenModules = List(
    "merkle",
    "trie",
    "poseidon",
    "storage",
    "bloom",
    "crypto",
    "crypto-errors",
    "hash-errors",
    "codec-errors",
    "jlvm",
    "jlvm-errors",
    "store-errors",
    "proofs-errors",
    "limits"
  )

  /** Regenerate with SdkMain (lang/go), then copy each module's file to `golden/go/<module>.go`. */
  test("generated Go for shared/schemas matches the reviewed golden snapshot") {
    generated.flatMap { g =>
      val out = g.go.toMap
      goldenModules.traverse { m =>
        IO.blocking(Files.readString(Paths.get(s"lang/scala/models-gen/src/test/resources/golden/go/$m.go"), UTF_8))
          .map(golden => expect(out.get(GoEmitter.pathOf(m)).contains(golden), s"$m differs from its golden snapshot"))
      }.map(_.combineAll)
    }
  }

  private def gen(modules: (String, String)*): Either[GenError, Map[String, String]] =
    SdkSources.generate(modules.toVector).map(_.go.toMap)

  private def go(cddl: String, module: String = "t"): Either[GenError, String] =
    gen(module -> cddl).map(_.getOrElse(GoEmitter.pathOf(module), ""))

  pureTest("a record is a struct of exported fields; a literal field is left out, an optional one is an Optional") {
    val out = go("r = { \"version\": 1, \"content-hash\": bytes, ? \"note\": text, \"n\": uint .le 255 }\n")
    expect(
      out.exists(_.contains("type R struct {\n\t// ContentHash is the `content-hash` entry.\n\tContentHash []byte\n")),
      out.fold(_.message, identity)
    )
      .and(expect(out.exists(_.contains("\tNote core.Optional[string]\n")), "optional"))
      .and(expect(out.exists(_.contains("\tN uint32\n")), "the smallest native integer"))
      .and(expect(out.exists(s => !s.contains("Version ")), "a literal carries no data"))
      .and(expect(out.exists(_.contains("core.Required(f, \"version\", core.LiteralInt(1))")), "but is checked"))
      .and(expect(out.exists(_.contains("func EncodeR(x R, at core.Encoding) (core.Value, error) {")), "the encoder"))
      .and(expect(out.exists(_.contains("func DecodeR(v core.Value) (R, error) {")), "the decoder"))
  }

  pureTest("a choice with records is a sealed interface; a record of another package is wrapped, a text is a struct") {
    val a = "item = { \"x\": uint }\n"
    val b = ";# import a as a\nchoice = a.item / local / \"empty\"\nlocal = { \"y\": uint }\n"
    val out = gen("a" -> a, "b" -> b).map(_.getOrElse(GoEmitter.pathOf("b"), ""))
    expect(out.exists(_.contains("type Choice interface {\n\tisChoice()\n}")), out.fold(_.message, identity))
      .and(expect(out.exists(_.contains("type ChoiceItem struct {\n\t// Value is the alternative.\n\tValue a.Item\n}")), "wrapped"))
      .and(expect(out.exists(_.contains("func (Local) isChoice() {}")), "a record of the package is its own alternative"))
      .and(expect(out.exists(_.contains("type ChoiceEmpty struct{}")), "a text"))
      .and(
        expect(
          out.exists(
            _.contains(
              "import (\n\t\"github.com/scasplte2/dilf4s/lang/go/core\"\n\t\"github.com/scasplte2/dilf4s/lang/go/core/generated/a\"\n)"
            )
          ),
          "imports"
        )
      )
  }

  pureTest("a text-keyed map is a core.TextMap, whose keys are compared, never hashed") {
    val out = go("r = { \"m\": { * text => uint } }\nm = { * text => [* uint] }\n")
    expect(out.exists(_.contains("\tM core.TextMap[uint64]\n")), out.fold(_.message, identity))
      .and(expect(out.exists(_.contains("type M = core.TextMap[[]uint64]")), "an alias"))
      .and(expect(out.exists(s => !s.contains("map[string]")), "no Go map"))
  }

  pureTest("a choice of texts alone, and named texts, are a string type with a constant per member") {
    val out = go("kind = \"a\" / \"b-c\"\nop = op-not / op-eq\nop-not = \"!\"\nop-eq = \"==\"\n")
    expect(out.exists(_.contains("type Kind string")), out.fold(_.message, identity))
      .and(expect(out.exists(_.contains("const KindBC Kind = \"b-c\"")), "a member"))
      .and(expect(out.exists(_.contains("const OpNot Op = \"!\"")), "a named text"))
      .and(expect(out.exists(_.contains("func AllOp() []Op {\n\treturn []Op{OpNot, OpEq}\n}")), "every member"))
  }

  pureTest("an enumeration is numbered from 1, written as its integers, however wide") {
    val out = go("e = &(low: -18446744073709551616, high: 18446744073709551615, one: 1)\n")
    expect(out.exists(_.contains("const ELow E = 1")), out.fold(_.message, identity))
      .and(expect(out.exists(_.contains("\tcase EOne:\n\t\treturn core.IntOf(1)")), "a narrow wire"))
      .and(
        expect(
          out.exists(
            _.contains(
              "\tcase ELow:\n\t\treturn core.BigBound{Negative: true, Magnitude: \"\\x01\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\"}.Int()"
            )
          ),
          "a wide one"
        )
      )
  }

  pureTest("an error family is a sealed interface of kind structs; an error's error is Err, which it unwraps to") {
    val cddl =
      "inner-kind = \"A\" / \"B\"\ninner = { \"kind\": \"A\" // \"kind\": \"B\" }\n" +
      "outer-kind = \"Wrap\" / \"Bare\"\nouter = { \"kind\": \"Wrap\", \"error\": inner // \"kind\": \"Bare\", \"n\": uint }\n"
    val out = go(cddl)
    expect(
      out.exists(_.contains("type Outer interface {\n\terror\n\t// Kind is the error's kind.\n\tKind() OuterKind\n\tisOuter()\n}")),
      out.fold(_.message, identity)
    )
      .and(expect(out.exists(_.contains("\tErr Inner\n")), "Err"))
      .and(expect(out.exists(_.contains("func (e OuterWrap) Unwrap() error {\n\treturn e.Err\n}")), "Unwrap"))
      .and(expect(out.exists(_.contains("return fmt.Sprintf(\"Bare (n: %v)\", e.N)")), "Error"))
      .and(expect(out.exists(s => !s.contains("func EncodeOuter(")), "no codec"))
      .and(
        expect(
          go("k-kind = \"A\" / \"B\"\nk = { \"kind\": \"A\", \"Kind\": text // \"kind\": \"B\" }\n").swap.exists {
            case GenError.ReservedName(_, Language.Go, "Kind") => true
            case _                                             => false
          },
          "a field named like a method"
        )
      )
  }

  pureTest("a field holding its own record by value is a pointer; one held through an interface or slice is not") {
    val out = go("a = { \"b\": b, \"u\": u, \"l\": [* a] }\nb = { ? \"a\": a }\nu = a / c\nc = { \"z\": uint }\n")
    expect(out.exists(_.contains("\tB *B\n")), out.fold(_.message, identity))
      .and(expect(out.exists(_.contains("\tA core.Optional[*A]\n")), "an optional pointer"))
      .and(expect(out.exists(_.contains("\tU U\n")), "a choice is an interface"))
      .and(expect(out.exists(_.contains("\tL []A\n")), "a slice"))
  }

  pureTest("names Go would declare twice, packages it could not import, and cycles between packages are generator errors") {
    expect(
      gen("t" -> "x = uint\nencode-x = { \"y\": uint }\n").swap.exists {
        case GenError.NameClash(_, "EncodeX", _) => true
        case _                                   => false
      },
      "EncodeX twice"
    )
      .and(
        expect(
          gen("x-y" -> "r = uint\n", "xy" -> "s = uint\n").swap.exists {
            case GenError.NameClash("Go packages", "xy", _) => true
            case _                                          => false
          },
          "two modules, one package"
        )
      )
      .and(
        expect(
          gen("map" -> "r = uint\n").swap.exists {
            case GenError.ReservedName("map", Language.Go, "map") => true
            case _                                                => false
          },
          "a keyword"
        )
      )
      .and(
        expect.same(
          Left(
            GenError.Unsupported(
              "a:*",
              UnsupportedFeature.OutsideProfile(ProfileRefusal.ForLanguage(Language.Go, ProfileRefusal.ImportCycle(Vector("a", "b", "a"))))
            )
          ),
          gen("a" -> ";# import b as b\nr = { \"x\": b.s }\n", "b" -> ";# import a as a\ns = { ? \"y\": a.r }\n")
        )
      )
      .and(
        expect.same(
          Left(
            GenError.Unsupported(
              "limits:*",
              UnsupportedFeature.OutsideProfile(ProfileRefusal.ForLanguage(Language.Go, ProfileRefusal.CoreReferences(Vector("m"))))
            )
          ),
          gen("limits" -> ";# import m as m\nr = m.s\n", "m" -> "s = uint\n")
        )
      )
      .and(
        expect(
          gen("codec-errors" -> "value = uint\n").swap.exists {
            case GenError.ReservedName(_, Language.Go, "Value") => true
            case _                                              => false
          },
          "a core module declaring what core declares"
        )
      )
  }

  pureTest("keys whose exported Go names collide are refused in every language") {
    def clash(e: Either[GenError, ?]) = e.swap.exists { case GenError.NameClash(_, _, _) => true; case _ => false }
    val cddl = "r = { \"xY\": uint, \"XY\": uint }\n"
    expect(clash(SdkSources.generate(Vector("t" -> cddl))), "SDKs").and(expect(clash(Main.generate(Vector("t" -> cddl), "p")), "Scala"))
  }

  pureTest("the generated test lists every model with a codec, and no error family") {
    val out = gen("t" -> "r = { \"x\": uint }\nk = \"a\" / \"b\"\ne-kind = \"A\" / \"B\"\ne = { \"kind\": \"A\" // \"kind\": \"B\" }\n")
      .map(_.getOrElse(GoEmitter.RegistryPath, ""))
    expect(out.exists(_.contains("\t\tmodel(\"t:r\", t.DecodeR, t.EncodeR),")), out.fold(_.message, identity))
      .and(expect(out.exists(_.contains("\t\tmodel(\"t:k\", t.DecodeK, t.EncodeK),")), "a text type"))
      .and(expect(out.exists(s => !s.contains("t:e\"")), "no family"))
  }
}
