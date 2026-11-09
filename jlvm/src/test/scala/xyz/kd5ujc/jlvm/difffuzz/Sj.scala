package xyz.kd5ujc.jlvm.difffuzz

import cats.Eval
import cats.syntax.all._

import io.circe.Json

/**
 * JSON as the generator writes it, so it can write what no JSON library would: a number's exact spelling (`1E+2`,
 * `-0`, `0.10`, `01`), a string's exact escapes (a lone `\ud800`), repeated member names and member order. Printing is
 * total and stack-safe at any depth (`Eval`), and writes only well-formed UTF-8: a lone surrogate in a [[Sj.Str]] is
 * written as its `\u` escape.
 */
enum Sj {
  case Num(text: String)
  case Str(value: String)

  /** A string literal as written, quotes and escapes included. */
  case Esc(json: String)
  case Bool(value: Boolean)
  case Null
  case Arr(items: Vector[Sj])
  case Obj(fields: Vector[(String, Sj)])
}

object Sj {

  def obj(key: String, value: Sj): Sj = Obj(Vector(key -> value))

  /** `{"op": [args...]}` */
  def call(op: String, args: Sj*): Sj = obj(op, Arr(args.toVector))

  def int(n: BigInt): Sj = Num(n.toString)

  def lit(v: Sj): Sj = obj("literal", v)

  // never the locale's digits: the text is the same on every machine
  private def hex4(c: Int): String = "\\u" + Integer.toHexString(c).reverse.padTo(4, '0').reverse

  /** A string's JSON: `ascii` escapes every code point past U+007E (a pair of escapes past U+FFFF). */
  def quote(s: String, ascii: Boolean): String =
    s.indices.map { i =>
      val c = s.charAt(i)
      val lone =
        (Character.isHighSurrogate(c) && !(i + 1 < s.length && Character.isLowSurrogate(s.charAt(i + 1)))) ||
        (Character.isLowSurrogate(c) && !(i > 0 && Character.isHighSurrogate(s.charAt(i - 1))))
      c match {
        case '"'                              => "\\\""
        case '\\'                             => "\\\\"
        case '\n'                             => "\\n"
        case '\t'                             => "\\t"
        case x if x < 0x20                    => hex4(x.toInt)
        case x if lone || (ascii && x > 0x7e) => hex4(x.toInt)
        case x                                => x.toString
      }
    }
      .mkString("\"", "", "\"")

  def print(j: Sj, ascii: Boolean = false): String = {
    def go(x: Sj): Eval[String] = x match {
      case Num(t)   => Eval.now(t)
      case Str(s)   => Eval.now(quote(s, ascii))
      case Esc(t)   => Eval.now(t)
      case Bool(b)  => Eval.now(b.toString)
      case Null     => Eval.now("null")
      case Arr(xs)  => xs.traverse(e => Eval.defer(go(e))).map(_.mkString("[", ",", "]"))
      case Obj(kvs) => kvs.traverse { case (k, v) => Eval.defer(go(v)).map(quote(k, ascii) + ":" + _) }.map(_.mkString("{", ",", "}"))
    }
    go(j).value
  }

  /** circe's tree as [[Sj]]: numbers keep their text, member order is kept. */
  def fromCirce(j: Json): Sj = {
    def go(x: Json): Eval[Sj] =
      x.fold(
        Eval.now(Null),
        b => Eval.now(Bool(b)),
        n => Eval.now(Num(n.toString)),
        s => Eval.now(Str(s)),
        a => a.traverse(e => Eval.defer(go(e))).map(Arr(_)),
        o => o.toVector.traverse { case (k, v) => Eval.defer(go(v)).map(k -> _) }.map(Obj(_))
      )
    go(j).value
  }

  /** The number of nodes. */
  def size(j: Sj): Int = {
    @scala.annotation.tailrec
    def go(todo: List[Sj], n: Int): Int = todo match {
      case Nil              => n
      case Arr(xs) :: rest  => go(xs.toList ::: rest, n + 1)
      case Obj(kvs) :: rest => go(kvs.map(_._2).toList ::: rest, n + 1)
      case _ :: rest        => go(rest, n + 1)
    }
    go(List(j), 0)
  }
}
