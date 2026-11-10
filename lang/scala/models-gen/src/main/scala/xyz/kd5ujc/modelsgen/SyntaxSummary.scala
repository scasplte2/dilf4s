// SPDX-License-Identifier: Apache-2.0
package xyz.kd5ujc.modelsgen

/**
 * Error payloads keep the complete syntax tree. Diagnostics inspect only a bounded prefix, using
 * an explicit work list rather than the recursive toString generated for AST products.
 */
private[modelsgen] object SyntaxSummary:
  private enum Part:
    case Text(value: String)
    case Inspect(value: Any)

  def apply(value: Ast.Ty): String = render(value)
  def apply(value: RT): String = render(value)

  private def render(value: Any): String =
    import Part.*
    val maxNodes = 64
    val maxChars = 512
    def children(prefix: String, values: List[Any]): List[Part] =
      if values.isEmpty then List(Text(prefix))
      else
        Text(prefix + "(") ::
        (values.zipWithIndex.flatMap { (v, i) =>
          if i == 0 then List(Inspect(v)) else List(Text(", "), Inspect(v))
        } :+ Text(")"))

    @scala.annotation.tailrec
    def loop(todo: List[Part], nodes: Int, size: Int, out: Vector[String]): String =
      if todo.isEmpty then out.mkString
      else if nodes >= maxNodes || size >= maxChars then out.mkString + "…"
      else
        todo match
          case Text(text) :: tail =>
            val taken = text.take(maxChars - size)
            if taken.size < text.size then (out :+ taken).mkString + "…"
            else loop(tail, nodes, size + taken.size, out :+ taken)
          case Inspect(current) :: tail =>
            val parts = current match
              case s: String           => List(Text(s))
              case q: QName            => List(Text(q.toString))
              case n: BigInt           => List(Text(n.toString))
              case n: java.lang.Number => List(Text(n.toString))
              case b: Boolean          => List(Text(b.toString))
              case xs: Vector[?]       => children("Vector", xs.take(maxNodes - nodes).toList)
              case p: Product =>
                children(p.productPrefix, p.productIterator.take(maxNodes - nodes).toList)
              case other => List(Text(other.getClass.getSimpleName))
            loop(parts ::: tail, nodes + 1, size, out)
          case Nil => out.mkString
    loop(List(Inspect(value)), 0, 0, Vector.empty)
