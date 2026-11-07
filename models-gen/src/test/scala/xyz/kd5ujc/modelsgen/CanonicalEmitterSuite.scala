package xyz.kd5ujc.modelsgen

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.syntax.all._

import weaver.FunSuite

object CanonicalEmitterSuite extends FunSuite {

  private def read(path:      String): String = new String(Files.readAllBytes(Paths.get(path)), UTF_8)
  private def gen(cddl:       String): Either[GenError, String] = Main.generate(Vector("t" -> cddl), "p").map(_.foldMap(_._2))
  private def violation(cddl: String): Option[UnsupportedFeature] = gen(cddl).swap.toOption.collect { case GenError.Unsupported(_, f) => f }

  /** Regenerate with: sbt "modelsGen/runMain xyz.kd5ujc.modelsgen.Main shared/schemas <dir> xyz.kd5ujc.models". */
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
        "limits"
      )
    val out = Main.generate(modules.toVector.map(m => m -> read(s"shared/schemas/$m.cddl")), "xyz.kd5ujc.models").map(_.toMap)
    modules.foldMap(m => expect.same(Right(Some(read(s"models-gen/src/test/resources/golden/$m.scala"))), out.map(_.get(m))))
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

  test("ambiguous choices are rejected; a key-disjoint choice is accepted") {
    expect
      .same(
        Some(UnsupportedFeature.AmbiguousAlternatives(Vector("a/b"))),
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
