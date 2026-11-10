package xyz.kd5ujc.core.vectors

import scala.annotation.tailrec

/**
 * JSON documents in the order they are written: the conformance vector files, whose members keep the order a
 * generator lists them in (canonical maps sort theirs). Reading is [[xyz.kd5ujc.core.canonical.CanonicalJson.parse]]'s
 * job; this only writes, laid out as the committed files are (circe's `spaces2`, which first wrote them): two-space
 * indents, `"name" : value`, and strings that escape only `"`, `\` and control characters.
 */
sealed trait VectorJson extends Product with Serializable

object VectorJson {

  final case class Raw(text: String) extends VectorJson
  final case class Arr(items: Vector[VectorJson]) extends VectorJson
  final case class Obj(fields: Vector[(String, VectorJson)]) extends VectorJson

  def str(s:      String): VectorJson = Raw(quote(s))
  def num(n:      BigInt): VectorJson = Raw(n.toString)
  def bool(b:     Boolean): VectorJson = Raw(b.toString)
  def arr(items:  Iterable[VectorJson]): VectorJson = Arr(items.toVector)
  def obj(fields: (String, VectorJson)*): VectorJson = Obj(fields.toVector)

  private enum Token {
    case Text(value: String)
    case Value(value: VectorJson, depth: Int)
  }

  private enum Container(val open: String, val close: String) {
    case Array extends Container("[", "]")
    case Object extends Container("{", "}")
  }

  /** The layout of the committed files, from a worklist: no recursion. */
  def pretty(j: VectorJson): String = {
    @tailrec
    def go(todo: List[Token], acc: Vector[String]): String =
      todo match {
        case Nil                            => acc.mkString
        case Token.Text(text) :: rest       => go(rest, acc :+ text)
        case Token.Value(Raw(t), _) :: rest => go(rest, acc :+ t)
        case Token.Value(Arr(xs), d) :: rest =>
          go(block(Container.Array, xs.map(x => List(Token.Value(x, d + 1))), d) ::: rest, acc)
        case Token.Value(Obj(fs), d) :: rest =>
          go(block(Container.Object, fs.map { case (k, v) => List(Token.Text(quote(k) + " : "), Token.Value(v, d + 1)) }, d) ::: rest, acc)
      }
    go(List(Token.Value(j, 0)), Vector.empty)
  }

  private def block(container: Container, parts: Vector[List[Token]], depth: Int): List[Token] =
    // as spaces2 wrote them: an empty array opens a line, an empty object a blank one
    if (parts.isEmpty) {
      val blank = container match {
        case Container.Array  => ""
        case Container.Object => "\n" + "  " * (depth + 1)
      }
      List(Token.Text(container.open + blank + "\n" + "  " * depth + container.close))
    } else {
      val inner = "\n" + "  " * (depth + 1)
      Token.Text(container.open) :: parts.toList.zipWithIndex.flatMap {
        case (p, i) => Token.Text(if (i == 0) inner else "," + inner) :: p
      } :::
      List(Token.Text("\n" + "  " * depth + container.close))
    }

  /** A JSON string as the committed files write one: control characters escaped, nothing else. */
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
}
