package xyz.kd5ujc.jlvm.difffuzz

import scodec.bits.ByteVector

import Sj._

/**
 * Candidate reductions of a case, most aggressive first: delta debugging over the program's tree (each node replaced by
 * one of its children or by something trivial, chunks of an array and members of a map removed, long text and numbers
 * shortened), over the data and the bindings the same way, and over the gas limit; and over the text itself, or the
 * form's bytes, by removing chunks, where the tree cannot be read or re-printing it loses the mismatch. The driver
 * keeps a candidate only when it is strictly smaller ([[Shrink.measure]]) and still has the same mismatch, so
 * shrinking ends.
 */
object Shrink {

  /** Chunks of `n` items to remove: halves, quarters, ..., single items. */
  private def chunks(n: Int): LazyList[(Int, Int)] =
    LazyList.iterate(n / 2)(_ / 2).takeWhile(_ >= 1).flatMap(size => LazyList.range(0, n, size).map(from => from -> size))

  private def smaller(x: Sj): LazyList[Sj] = x match {
    case Arr(xs)                        => chunks(xs.size).map { case (from, size) => Arr(xs.patch(from, Nil, size)) } :+ Null
    case Obj(kvs)                       => chunks(kvs.size).map { case (from, size) => Obj(kvs.patch(from, Nil, size)) } :+ Null
    case Str(s) if s.length > 1         => LazyList(Str(""), Str(s.take(s.length / 2)), Str(s.take(1)))
    case Num(t) if t != "0" && t != "1" => LazyList(Num("0"), Num("1"), Num(t.take(t.length / 2).filter(_.isDigit).take(1).padTo(1, '0')))
    case Esc(_)                         => LazyList(Str(""), Str("a"))
    case Null                           => LazyList.empty
    case _                              => LazyList(Null)
  }

  /** Every node of `j` replaced by one of its children or by something smaller. */
  def tree(j: Sj): LazyList[Sj] =
    LazyList.from(Tree.nodes(j)).flatMap {
      case (p, x) => (LazyList.from(Tree.children(x)) ++ smaller(x)).filter(_ != x).map(r => Tree.update(j, p, _ => r))
    }

  /** Code points removed in chunks. */
  def text(t: String): LazyList[String] = {
    val cps = t.codePoints().toArray.toVector
    chunks(cps.size).map { case (from, size) => cps.patch(from, Nil, size) }.map(xs => new String(xs.toArray, 0, xs.size))
  }

  def bytes(b: ByteVector): LazyList[ByteVector] =
    chunks(b.size.toInt).map { case (from, size) => b.take(from.toLong) ++ b.drop((from + size).toLong) }

  private def parsed(t: String): Option[Sj] = io.circe.parser.parse(t).toOption.map(Sj.fromCirce)

  /** A case's size: what shrinking makes strictly smaller. */
  def measure(c: FuzzCase): Long =
    (c.source match {
      case FuzzCase.Source.Surface(t) => t.length.toLong
      case FuzzCase.Source.Form(b)    => b.size
    }) + c.data.length + c.env.map { case (k, v) => k.length + v.length }.sum + c.gasLimit.fold(0)(_ => 1)

  /** The candidates, the program's first. `asTree` is whether the program's re-printed tree keeps the mismatch. */
  def candidates(c: FuzzCase, asTree: Boolean): LazyList[FuzzCase] = {
    val program: LazyList[FuzzCase] = c.source match {
      case FuzzCase.Source.Surface(t) =>
        val viaTree = parsed(t)
          .filter(_ => asTree)
          .fold(LazyList.empty[FuzzCase])(j => tree(j).map(x => c.copy(source = FuzzCase.Source.Surface(Sj.print(x)))))
        viaTree ++ text(t).map(x => c.copy(source = FuzzCase.Source.Surface(x)))
      case FuzzCase.Source.Form(b) => bytes(b).map(x => c.copy(source = FuzzCase.Source.Form(x)))
    }
    // a value's text: its tree reduced and re-printed, then the text itself cut, in case re-printing loses the mismatch
    val value = (t: String) => parsed(t).fold(LazyList.empty[String])(j => tree(j).map(Sj.print(_))) ++ text(t)
    val data: LazyList[FuzzCase] =
      LazyList("null", "{}").filter(_ != c.data).map(d => c.copy(data = d)) ++ value(c.data).map(d => c.copy(data = d))
    val env: LazyList[FuzzCase] =
      LazyList.from(c.env.indices).map(i => c.copy(env = c.env.patch(i, Nil, 1))) ++
      LazyList.from(c.env.indices).flatMap { i =>
        val (k, v) = c.env(i)
        value(v).map(x => c.copy(env = c.env.updated(i, k -> x)))
      }
    val gas: LazyList[FuzzCase] = LazyList.from(c.gasLimit).map(_ => c.copy(gasLimit = None))
    gas ++ interleave(List(program, data, env))
  }

  /** One of each in turn, so a round's candidates reach the data and the bindings however large the program is. */
  private def interleave[A](xs: List[LazyList[A]]): LazyList[A] = {
    val live = xs.filter(_.nonEmpty)
    if (live.isEmpty) LazyList.empty else LazyList.from(live.flatMap(_.headOption)) #::: interleave(live.map(_.tail))
  }
}
