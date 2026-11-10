package xyz.kd5ujc.modelsgen

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.syntax.all._

import weaver.FunSuite

object CanonicalEmitterSuite extends FunSuite {

  private def read(path:      String): String = new String(Files.readAllBytes(Paths.get(path)), UTF_8)
  private def gen(cddl:       String): Either[GenError, String] = Main.generate(Vector("t" -> cddl), "p").map(_.foldMap(_._2))
  private def violation(cddl: String): Option[UnsupportedFeature] = gen(cddl).swap.toOption.collect { case GenError.Unsupported(_, f) => f }

  /** Regenerate with: tools/sbt "modelsGen/runMain xyz.kd5ujc.modelsgen.Main shared/schemas <dir> xyz.kd5ujc.models". */
  test("generated sources for shared/schemas match the reviewed golden snapshot") {
    val modules =
      List(
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
    val out = Main.generate(modules.toVector.map(m => m -> read(s"shared/schemas/$m.cddl")), "xyz.kd5ujc.models").map(_.toMap)
    modules.foldMap(m => expect.same(Right(Some(read(s"lang/scala/models-gen/src/test/resources/golden/$m.scala"))), out.map(_.get(m))))
  }

  test("native error ownership is limited to the exact registered families") {
    import CanonicalEmitter.NativeErrorFamily
    val rules = Vector(QName("jlvm-errors", "jlvm-error"), QName("jlvm-errors", "jlvm-crypto-cause"), QName("crypto-errors", "sigma-error"))
    val cddl = """extra-kind = "Failed" / "Missing"
                  |extra = { "kind": "Failed" // "kind": "Missing" }
                  |""".stripMargin
    val generated = Main.generate(Vector("jlvm-errors" -> cddl), "p").map(_.map(_._2).mkString)
    expect
      .same(rules, NativeErrorFamily.values.toVector.map(_.rule))
      .and(expect.same(NativeErrorFamily.values.toVector.map(Some(_)), rules.map(NativeErrorFamily.fromRule)))
      .and(expect.same(None, NativeErrorFamily.fromRule(QName("jlvm-errors", "extra"))))
      .and(expect(generated.exists(_.contains("sealed trait Extra")), "an additional error family in the same module is generated"))
  }

  test("profile violations are typed generator errors") {
    def outside(cddl: String) = violation(cddl).exists { case UnsupportedFeature.OutsideProfile(_) => true; case _ => false }
    expect(outside("r = { 1: uint }\n"), "integer map key")
      .and(expect(outside("r = { \"a\": uint / text }\n"), "inline choice in a field"))
      .and(expect(outside("r = 1 / 2\n"), "unlabelled literal choice"))
      .and(expect(outside("r = { \"a\": { \"b\": uint } }\n"), "inline nested map"))
      .and(expect(outside("r = { * \"a\": uint }\n"), "repeated literal key"))
      .and(expect(gen("r = #6.30([uint, uint])\n").isLeft, "tags"))
      .and(expect(gen("r = float\n").isLeft, "floats"))
  }

  test("profile refusals preserve exact reasons and input fields") {
    val cases = Vector(
      "r = { 1: uint }"                    -> ProfileRefusal.MapEntry(Ast.Occ.Once, Some(RKey.IntKey(1))),
      "r = { * \"a\": uint }"              -> ProfileRefusal.MapEntry(Ast.Occ.Star, Some(RKey.TextKey("a"))),
      "r = text .regexp \"[a-z]+\""        -> ProfileRefusal.UnsupportedPattern("[a-z]+"),
      "max-n = 1024\nr = { \"n\": max-n }" -> ProfileRefusal.ConstantInField(QName("t", "max-n")),
      "min-n = -1"                         -> ProfileRefusal.ConstantOutOfRange(-1),
      "k = \"a\" / \"b\"\nr = \"a\" / k"   -> ProfileRefusal.RepeatedTexts(Vector("a"))
    )
    cases.foldLeft(success) {
      case (acc, (cddl, reason)) =>
        acc.and(expect.same(Some(UnsupportedFeature.OutsideProfile(reason)), violation(cddl)))
    }
  }

  test("profile diagnostics render typed reasons at the presentation boundary") {
    expect
      .same(
        "outside the dilf4s canonical profile: constant -1 (a constant is a uint, 0..2^64-1)",
        UnsupportedFeature.OutsideProfile(ProfileRefusal.ConstantOutOfRange(-1)).message
      )
      .and(
        expect.same(
          "outside the dilf4s canonical profile: alias cycle through item (Rust)",
          UnsupportedFeature
            .OutsideProfile(ProfileRefusal.ForLanguage(Language.Rust, ProfileRefusal.AliasCycle(QName("t", "item"))))
            .message
        )
      )
      .and(
        expect.same(
          "outside the dilf4s canonical profile: optional literal field version (Go)",
          UnsupportedFeature.OutsideProfile(ProfileRefusal.ForLanguage(Language.Go, ProfileRefusal.OptionalLiteral("version"))).message
        )
      )
  }

  test("diagnostics distinguish field, member and declaration names") {
    val cases = Vector(
      ProfileRefusal.InvalidKeyIdentifier("x-y")                -> "key \"x-y\" has no identifier form",
      ProfileRefusal.InvalidMemberIdentifier("X")               -> "member X has no identifier form",
      ProfileRefusal.NoExportedFieldName("x")                   -> "key \"x\" has no exported Go name",
      ProfileRefusal.NoExportedTypeName("D")                    -> "name D has no exported Go form",
      ProfileRefusal.ErrorField(FieldType.Ref(QName("m", "r"))) -> "error field of type r (text, an integer, a boolean, a kind or an error)"
    )
    cases.foldLeft(success) { case (acc, (reason, expected)) => acc.and(expect.same(expected, reason.message)) }
  }

  test("ambiguous choices are rejected; a key-disjoint choice is accepted") {
    expect
      .same(
        Some(UnsupportedFeature.AmbiguousAlternatives(Vector(QName("t", "a") -> QName("t", "b")))),
        violation("r = a / b\na = { \"x\": uint, ? \"y\": uint }\nb = { \"x\": uint }\n")
      )
      .and(expect(gen("r = a / b\na = { \"x\": uint }\nb = { \"y\": uint }\n").isRight))
  }

  test("names map deterministically and collide as errors; keywords are escaped") {
    expect(gen("a-b = uint\na_b = text\n").swap.exists { case GenError.NameClash(_, "AB", _) => true; case _ => false })
      .and(expect(gen("r = { \"new\": uint }\n").exists(_.contains("`new`: BigInt"))))
  }

  test("with several clashes, the first type name in order is the one reported") {
    expect.same(Left(GenError.NameClash("t", "AB", Vector("a-b", "a_b"))), gen("c-d = uint\nc_d = text\na-b = uint\na_b = text\n"))
  }

  test("text alternatives have exact naming refusals before emitting Scala") {
    expect
      .same(Left(GenError.NameClash("t:r", "AB", Vector("a-b", "a_b"))), gen("r = \"a-b\" / \"a_b\""))
      .and(
        expect.same(
          Left(GenError.Unsupported("t:r", UnsupportedFeature.OutsideProfile(ProfileRefusal.InvalidAlternativeName("!")))),
          gen("r = \"!\" / \"ok\"")
        )
      )
      .and(List("List", "R").foldMap { name =>
        expect.same(Left(GenError.ReservedName("t:r", Language.Scala, name)), gen(s"r = \"$name\" / \"ok\""))
      })
      .and(List("Set", "Vector", "Literal").foldMap { name =>
        expect
          .same(Left(GenError.ReservedName("t:r", Language.Scala, name)), gen(s"r = \"$name\" / rec\nrec = { \"x\": uint }"))
          .and(expect(gen(s"r = \"$name\" / \"ok\"").isRight, s"$name shadows no pure text enum dependency"))
      })
  }

  test("plain text alternatives and error kinds are enums with original wire spellings") {
    val result =
      gen("r = \"not-ready\" / \"ready\"\ne-kind = \"Failed\" / \"Missing\"\ne = { \"kind\": \"Failed\" // \"kind\": \"Missing\" }")
    expect(result.exists(_.contains("case NotReady extends R(\"not-ready\")")), result.toString)
      .and(expect(result.exists(_.contains("def kind: EKind = EKind.Failed")), result.toString))
      .and(expect(result.exists(_.contains("def fromWire(wire: String): Option[R]")), result.toString))
  }

  test("a rule named like a type the generated Scala refers to is a ReservedName in Scala") {
    val names = List(
      "big-int"           -> "BigInt",
      "boolean"           -> "Boolean",
      "byte-vector"       -> "ByteVector",
      "canonical"         -> "Canonical",
      "canonical-decoder" -> "CanonicalDecoder",
      "canonical-entries" -> "CanonicalEntries",
      "canonical-value"   -> "CanonicalValue",
      "codec"             -> "Codec",
      "decode-reason"     -> "DecodeReason",
      "int"               -> "Int",
      "list"              -> "List",
      "long"              -> "Long",
      "map"               -> "Map",
      "none"              -> "None",
      "option"            -> "Option",
      "set"               -> "Set",
      "some"              -> "Some",
      "string"            -> "String",
      "vector"            -> "Vector"
    )
    names.foldMap {
      case (rule, scala) => expect.same(Left(GenError.ReservedName("t", Language.Scala, scala)), gen(s"$rule = { \"x\": uint }\n"))
    }.and(expect(gen("either = { \"x\": uint }\nseq = uint\n").isRight, "names the generated code never uses stay available"))
  }

  test("an enumeration case named like a term its body refers to is a ReservedName in Scala") {
    expect
      .same(Left(GenError.ReservedName("t:e", Language.Scala, "Codec")), gen("e = &(codec: 1, other: 2)\n"))
      .and(expect.same(Left(GenError.ReservedName("t:e", Language.Scala, "BigInt")), gen("e = &(big-int: 1, other: 2)\n")))
      .and(
        expect.same(Left(GenError.ReservedName("t:e", Language.Scala, "CanonicalDecoder")), gen("e = &(canonical-decoder: 1, other: 2)\n"))
      )
      .and(expect.same(Left(GenError.ReservedName("t:e", Language.Scala, "DecodeReason")), gen("e = &(decode-reason: 1, other: 2)\n")))
      .and(expect(gen("e = &(canonical: 1, string: 2)\n").isRight, "a case is a term, so it shadows no type"))
  }

  test("a field named like a member every case class has is a ReservedName in Scala") {
    expect
      .same(Left(GenError.ReservedName("t:r", Language.Scala, "hashCode")), gen("r = { \"hash-code\": uint }\n"))
      .and(expect.same(Left(GenError.ReservedName("t:r", Language.Scala, "productArity")), gen("r = { \"productArity\": uint }\n")))
      .and(expect.same(Left(GenError.ReservedName("t:r", Language.Scala, "wait")), gen("r = { \"wait\": uint }\n")))
      .and(expect(gen("r = { \"copy\": uint, \"equals\": uint }\n").isRight, "members a field may overload stay available"))
  }
}
