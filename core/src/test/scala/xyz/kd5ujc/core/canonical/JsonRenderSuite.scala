package xyz.kd5ujc.core.canonical

import cats.Eval
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalError._
import xyz.kd5ujc.core.canonical.CanonicalJson.{RenderError, TextSink}
import xyz.kd5ujc.core.canonical.CanonicalValue._

import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

/**
 * The JSON renderer's cost, counted rather than timed: a sink that keeps only how many characters it was given, and in
 * how many appends, shows that every character of a rendering is written once, whatever the value's depth or width.
 */
object JsonRenderSuite extends SimpleIOSuite with Checkers {

  private def get[A](e: Either[CanonicalError, A]): A = e.fold(err => sys.error(err.toString), identity)

  /** Characters appended, and appends. */
  final private case class Count(chars: Long, appends: Long)

  private given TextSink[Count] = (c, _, from, until) => Count(c.chars + (until - from), c.appends + 1)

  private def counted(v: CanonicalValue, maxBytes: Long = CanonicalJson.MaxRenderBytes): Either[RenderError, Count] =
    CanonicalJson.renderInto(v, maxBytes, Count(0L, 0L))

  /**
   * The renderer before it was one pass: each level built its children's strings and joined them, so a character was
   * copied once per level above it. Kept as the oracle for what the rendering is.
   */
  private def byLevels(value: CanonicalValue): Either[CanonicalError, String] = {
    def quote(s: String): String =
      s.flatMap {
        case '"'          => "\\\""
        case '\\'         => "\\\\"
        case '\n'         => "\\n"
        case '\r'         => "\\r"
        case '\t'         => "\\t"
        case '\b'         => "\\b"
        case '\f'         => "\\f"
        case c if c < ' ' => f"\\u${c.toInt}%04x"
        case c            => c.toString
      }.prepended('"').appended('"')
    def go(v: CanonicalValue): Eval[Either[CanonicalError, String]] =
      v match {
        case CNull        => Eval.now("null".asRight)
        case CBool(b)     => Eval.now(b.toString.asRight)
        case i: CInt      => Eval.now(CanonicalJson.integerText(i).asRight)
        case r: CRational => Eval.now(CanonicalJson.render(r))
        case t: CText     => Eval.now(quote(t.value).asRight)
        case CBytes(_)    => Eval.now(NotJsonRepresentable("bytes").asLeft)
        case CArray(vs)   => Eval.defer(vs.traverse(go)).map(_.sequence.map(_.mkString("[", ",", "]")))
        case CMap(m) =>
          Eval
            .defer(m.toVector.sortBy(_._1)(using CanonicalJson.CodePointOrder).traverse {
              case (k, x) => go(x).map(_.map(quote(k) + ":" + _))
            })
            .map(_.sequence.map(_.mkString("{", ",", "}")))
      }
    go(value).value
  }

  private val chain: CanonicalValue =
    (1 to CanonicalValue.MaxDepth).foldLeft(CInt(7): CanonicalValue)((v, _) => get(CanonicalValue.array(Vector(v))))

  private val mapChain: CanonicalValue =
    (1 to CanonicalValue.MaxDepth).foldLeft(get(CanonicalValue.text("a\"b\n\u0001é😀")): CanonicalValue) { (v, i) =>
      get(CanonicalValue.map(List(s"k$i" -> v, "z" -> CInt(i.toLong))))
    }

  private val wide: CanonicalValue =
    get(
      CanonicalValue.map(
        (0 until 20000).map(i => f"key$i%05d" -> (if (i % 2 == 0) CInt(i.toLong) else get(CanonicalValue.text(s"v\t$i")))).toList
      )
    )

  test("the one-pass renderer writes what rendering level by level wrote, refusals included") {
    forall(CanonicalGens.value(4)) { v =>
      expect.same(byLevels(v), CanonicalJson.render(v))
    }
  }

  pureTest("every character of a depth-256 chain, a deep map chain and a wide map is appended once") {
    List("array chain" -> chain, "map chain" -> mapChain, "wide map" -> wide).foldMap {
      case (name, v) =>
        val text = get(CanonicalJson.render(v))
        counted(v) match {
          case Right(Count(chars, appends)) =>
            // the rendering level by level copied each character once per level above it: about 33,000 for the chain
            expect(chars == text.length.toLong, s"$name: $chars characters appended for a rendering of ${text.length}")
              .and(expect(appends <= 4L * v.weight, s"$name: $appends appends for a value of weight ${v.weight}"))
              .and(expect.same(byLevels(v), Right(text)))
          case Left(e) => failure(s"$name: $e")
        }
    }
  }

  pureTest("the chain's rendering is its 513 characters, written in 513 appends") {
    expect
      .same(Right(Count(513L, 513L)), counted(chain))
      .and(expect.same(Right("[" * 256 + "7" + "]" * 256), CanonicalJson.render(chain)))
  }

  pureTest("a limit counts UTF-8 bytes: a rendering of exactly the limit is written, one byte over it is TooLarge") {
    val samples =
      List(chain, mapChain, wide, get(CanonicalValue.text("é😀\u0000")), get(CanonicalValue.array(Vector(CNull, True, CInt(-5)))))
    samples.foldMap { v =>
      val text = get(CanonicalJson.render(v))
      val bytes = text.getBytes(java.nio.charset.StandardCharsets.UTF_8).length.toLong
      expect
        .same(Right(text), CanonicalJson.renderWithin(v, bytes))
        .and(expect.same(Left(RenderError.TooLarge(bytes - 1)), CanonicalJson.renderWithin(v, bytes - 1)))
    }
  }

  pureTest("a value heavier than the limit is refused before anything is written, however heavy it is") {
    // [$, $] applied 70 times: a weight that saturates at 2^63 - 1, built in 70 steps
    val dag = get(BuiltValues.build(BuiltValues.of(CInt(1)).wrap(BuiltValues.Pair, 70)))
    expect
      .same(Weight.Max, dag.weight)
      .and(expect.same(Left(RenderError.TooLarge(1000L)), counted(dag, 1000L)))
      .and(expect.same(Left(RenderError.TooLarge(1000L)), CanonicalJson.renderWithin(dag, 1000L)))
      .and(
        expect.same(
          Left(NotJsonRepresentable(s"a rendering of more than ${CanonicalJson.MaxRenderBytes} bytes")),
          CanonicalJson.render(dag)
        )
      )
      .and(expect.same(Left(RenderError.TooLarge(10L)), counted(wide, 10L)))
      .and(expect.same(Left(RenderError.TooLarge(0L)), CanonicalJson.renderWithin(CNull, -1L)))
      .and(expect.same(Right("null"), CanonicalJson.renderWithin(CNull, Long.MaxValue)))
  }

  pureTest("what JSON cannot represent is refused at the first such part, unless the limit is met first") {
    val withBytes = get(CanonicalValue.array(Vector(get(CanonicalValue.text("x" * 50)), CBytes(scodec.bits.ByteVector(1)))))
    expect
      .same(Left(RenderError.Canonical(NotJsonRepresentable("bytes"))), CanonicalJson.renderWithin(withBytes, 1000L))
      .and(expect.same(Left(RenderError.TooLarge(20L)), CanonicalJson.renderWithin(withBytes, 20L)))
  }
}
