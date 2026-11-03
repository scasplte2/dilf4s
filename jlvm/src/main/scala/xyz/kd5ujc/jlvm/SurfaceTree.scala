package xyz.kd5ujc.jlvm

import scala.annotation.tailrec

import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalJson, CanonicalValue}

/**
 * The structure of JSON text the strict canonical parser has accepted: each node with its source span, nulls and member
 * order still visible, so a null field cannot turn a map expression into an operation. Nothing here decides what JSON
 * is. [[SurfaceTree.read]] runs [[CanonicalJson.parse]] over the whole text first, and only then splits it; a
 * scalar, a member name or a literal value is read by [[CanonicalJson.parse]] from its own span ([[Source.value]]).
 * The Rust reader (sdk/rust/jlvm/src/surface.rs) builds the same tree.
 */
final private[jlvm] case class SurfaceTree private (start: Int, end: Int, kind: SurfaceTree.Kind)

private[jlvm] object SurfaceTree {

  enum Kind {
    case Null

    /** A number, `true` or `false`. */
    case Scalar
    case Str
    case Arr(items: Vector[SurfaceTree])
    case Obj(fields: Vector[(String, SurfaceTree)])
  }

  /**
   * Text the strict parser accepted, and its tree. Spans are offsets into the text, so no node copies it; `spanned` is
   * told the length of each span read back.
   */
  final class Source private[SurfaceTree] (text: String, val root: SurfaceTree, spanned: Int => Unit) {

    /** The canonical value of a node's span, by the strict parser. */
    def value(node: SurfaceTree): Either[CanonicalError, CanonicalValue] = read(text, node.start, node.end, spanned)

    /** The text of a string node; nothing is read for any other node. */
    def string(node: SurfaceTree): Option[String] =
      node.kind match {
        case Kind.Str =>
          value(node).toOption.collect { case CanonicalValue.CText(s) => s }
        case _ => None
      }

    /** The integer a number node denotes; nothing is read for any other node. */
    def integer(node: SurfaceTree): Option[BigInt] =
      node.kind match {
        case Kind.Scalar =>
          value(node).toOption.collect { case CanonicalValue.CInt(i) => i }
        case _ => None
      }
  }

  /**
   * `text` through the strict parser, then its tree. The text is accepted before it is split, so the split meets only
   * well-formed JSON; it reads each member name from its span, and nothing else. An error is the strict parser's;
   * `None`, a split the accepted text does not allow, cannot happen.
   */
  def read(text: String, spanned: Int => Unit): Either[CanonicalError, Option[Source]] =
    CanonicalJson.parse(text).map(_ => scan(text, 0, Nil, spanned).map(new Source(text, _, spanned)))

  private def read(text: String, start: Int, end: Int, spanned: Int => Unit): Either[CanonicalError, CanonicalValue] = {
    spanned(end - start)
    // slice, not substring: it never throws, and a node is only ever made inside its own text
    CanonicalJson.parse(text.slice(start, end))
  }

  private enum Open {
    case InArr(start: Int, items: Vector[SurfaceTree])
    case InObj(start: Int, fields: Vector[(String, SurfaceTree)], key: Option[String])
  }

  /** One node per call, with an explicit stack: no recursion. */
  @tailrec
  private def scan(text: String, from: Int, stack: List[Open], spanned: Int => Unit): Option[SurfaceTree] = {
    val p = skip(text, from)
    charAt(text, p) match {
      case None      => None
      case Some('{') => scan(text, p + 1, Open.InObj(p, Vector.empty, None) :: stack, spanned)
      case Some('[') => scan(text, p + 1, Open.InArr(p, Vector.empty) :: stack, spanned)
      case Some('"') if awaitingKey(stack) =>
        val end = stringEnd(text, p + 1)
        (stack, read(text, p, end, spanned)) match {
          case (Open.InObj(s, fields, _) :: rest, Right(CanonicalValue.CText(k))) =>
            scan(text, end, Open.InObj(s, fields, Some(k)) :: rest, spanned)
          case _ => None
        }
      case Some(c) =>
        completed(text, p, c, stack) match {
          case None                                           => None
          case Some((node, _, Nil))                           => Some(node)
          case Some((node, next, Open.InArr(s, items) :: up)) => scan(text, next, Open.InArr(s, items :+ node) :: up, spanned)
          case Some((node, next, Open.InObj(s, fs, Some(k)) :: up)) =>
            scan(text, next, Open.InObj(s, fs :+ (k -> node), None) :: up, spanned)
          case Some(_) => None
        }
    }
  }

  /** The node that ends or stands at `p`, where scanning resumes, and the stack it belongs to. */
  private def completed(text: String, p: Int, c: Char, stack: List[Open]): Option[(SurfaceTree, Int, List[Open])] =
    c match {
      case '}' | ']' =>
        stack match {
          case Open.InArr(s, items) :: rest if c == ']'        => Some((SurfaceTree(s, p + 1, Kind.Arr(items)), p + 1, rest))
          case Open.InObj(s, fields, None) :: rest if c == '}' => Some((SurfaceTree(s, p + 1, Kind.Obj(fields)), p + 1, rest))
          case _                                               => None
        }
      case '"' =>
        val end = stringEnd(text, p + 1)
        Some((SurfaceTree(p, end, Kind.Str), end, stack))
      case _ =>
        val end = scalarEnd(text, p)
        Some((SurfaceTree(p, end, if (c == 'n') Kind.Null else Kind.Scalar), end, stack))
    }

  private def awaitingKey(stack: List[Open]): Boolean =
    stack match {
      case Open.InObj(_, _, None) :: _ => true
      case _                           => false
    }

  /** Past whitespace and the separators, which the strict parser has already placed. */
  @tailrec
  private def skip(s: String, i: Int): Int =
    charAt(s, i) match {
      case Some(' ' | '\t' | '\n' | '\r' | ',' | ':') => skip(s, i + 1)
      case _                                          => i
    }

  /** Just past the quote closing a string whose contents start at `i`; an escape skips the character it escapes. */
  @tailrec
  private def stringEnd(s: String, i: Int): Int =
    charAt(s, i) match {
      case None       => s.length
      case Some('"')  => i + 1
      case Some('\\') => stringEnd(s, i + 2)
      case Some(_)    => stringEnd(s, i + 1)
    }

  @tailrec
  private def scalarEnd(s: String, i: Int): Int =
    charAt(s, i) match {
      case None | Some(' ' | '\t' | '\n' | '\r' | ',' | ']' | '}') => i
      case Some(_)                                                 => scalarEnd(s, i + 1)
    }

  private def charAt(s: String, i: Int): Option[Char] = Option.when(i >= 0 && i < s.length)(s.charAt(i))
}
