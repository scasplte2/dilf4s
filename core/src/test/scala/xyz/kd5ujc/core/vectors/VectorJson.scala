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

  /** The layout of the committed files, from a worklist: no recursion. */
  def pretty(j: VectorJson): String = {
    @tailrec
    def go(todo: List[Either[String, (VectorJson, Int)]], acc: Vector[String]): String =
      todo match {
        case Nil                         => acc.mkString
        case Left(text) :: rest          => go(rest, acc :+ text)
        case Right((Raw(t), _)) :: rest  => go(rest, acc :+ t)
        case Right((Arr(xs), d)) :: rest => go(block("[", xs.map(x => List(Right((x, d + 1)))), "]", d) ::: rest, acc)
        case Right((Obj(fs), d)) :: rest =>
          go(block("{", fs.map { case (k, v) => List(Left(quote(k) + " : "), Right((v, d + 1))) }, "}", d) ::: rest, acc)
      }
    go(List(Right((j, 0))), Vector.empty)
  }

  private def block(
    open:  String,
    parts: Vector[List[Either[String, (VectorJson, Int)]]],
    close: String,
    depth: Int
  ): List[Either[String, (VectorJson, Int)]] =
    // as spaces2 wrote them: an empty array opens a line, an empty object a blank one
    if (parts.isEmpty) List(Left(open + (if (open == "{") "\n" + "  " * (depth + 1) else "") + "\n" + "  " * depth + close))
    else {
      val inner = "\n" + "  " * (depth + 1)
      Left(open) :: parts.toList.zipWithIndex.flatMap { case (p, i) => Left(if (i == 0) inner else "," + inner) :: p } :::
      List(Left("\n" + "  " * depth + close))
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
