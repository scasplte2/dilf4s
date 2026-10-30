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
    val modules = List("merkle", "trie", "poseidon", "storage", "bloom", "crypto")
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
}
