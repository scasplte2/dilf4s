package xyz.kd5ujc.jlvm

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.{CanonicalValue, Cbor}
import xyz.kd5ujc.core.hash.Sha256
import xyz.kd5ujc.test.stack.StackProbe

import weaver.SimpleIOSuite

/**
 * The deepest programs read and written at a normal stack: the surface parser, the canonical form both ways, the CBOR
 * and a program's identity. `{"var": ...}` nested `n` deep is a computed path in each level but the last, which nests
 * the JSON and the canonical form one level a node, the most any expression can: 255 is the deepest whose canonical
 * form fits (the program's record is a level too), 256 the deepest text the JSON parser accepts.
 */
object ProgramStackSuite extends SimpleIOSuite {

  private def nested(n: Int): String = "{\"var\": " * n + "\"a\"" + "}" * n

  private def kind(r: Either[JlvmError, ?]): String = r.fold(e => e.kind: String, _ => "program")

  test("the deepest program parses, and reads back from its canonical form and CBOR as itself") {
    StackProbe.fits("the deepest program") {
      Parser.parse(nested(255)).flatMap { p =>
        (Parser.canonical(p), Parser.encode(p), Parser.id[Sha256](p)).tupled.flatMap {
          case (form, bytes, id) =>
            (Parser.fromCanonical(form), Parser.decode(bytes)).tupled.map {
              case (fromForm, fromBytes) => (p, form.depth, fromForm, fromBytes, id.bytes.size)
            }
        }
      }
    } {
      case Left(e) => failure(s"refused: ${e.message}")
      case Right((p, depth, fromForm, fromBytes, idBytes)) =>
        expect
          .same(CanonicalValue.MaxDepth, depth)
          .and(expect.same(p, fromForm))
          .and(expect.same(p, fromBytes))
          .and(expect.same(32, idBytes))
    }
  }

  test("text one level deeper is refused by the bound on its canonical form, not by the stack") {
    StackProbe.fits("one level deeper")(kind(Parser.parse(nested(256))))(k => expect.same("Limit", k))
  }

  test("an array nested as deep as JSON allows is read, then refused by the bound") {
    StackProbe.fits("the deepest array")(kind(Parser.parse("[" * 256 + "1" + "]" * 256)))(k => expect.same("Limit", k))
  }

  test("a canonical form as deep as a value nests, refused at its deepest node, names the whole path") {
    val form = Parser.parse(nested(255)).flatMap(Parser.canonical).map { f =>
      // the innermost `{"var": "a"}` made `{"var": 1}`, which no alternative reads as a path
      CanonicalValue.mapScalars(f) {
        case CanonicalValue.CText("a") => CanonicalValue.CInt(1)
        case other                     => other
      }
    }
    StackProbe.fits("the refused form")(form.flatMap(Parser.fromCanonical).leftMap(_.message)) { r =>
      expect(r.left.exists(_.startsWith("expr.path.path.")), s"a refusal with the path from the root, got ${r.left.map(_.take(80))}")
        .and(expect(r.left.exists(_.endsWith(": expected text, got int")), s"the leaf's refusal, got ${r.left.map(_.takeRight(80))}"))
    }
  }

  test("the CBOR of the deepest form decodes to it") {
    StackProbe.fits("the deepest CBOR")(Parser.parse(nested(255)).flatMap(Parser.encode).map(b => Cbor.decode(b).map(_.depth))) { r =>
      expect.same(Right(Right(CanonicalValue.MaxDepth)), r)
    }
  }
}
