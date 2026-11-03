package xyz.kd5ujc.core.canonical

import cats.Eval
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._

import io.circe.syntax._
import io.circe.{ACursor, Json}
import scodec.bits.ByteVector

/**
 * The `built-value` recipes of the vector schemas (shared/schemas/refusal-vectors.cddl, jlvm-vectors.cddl): a value with
 * shared parts, far heavier than its description, written as a leaf and steps. A step `Wrap(t, n)` replaces, `n` times
 * over, every text `"$"` in `t` by the value so far; `Copies(n)` makes an array of `n` of it. Every occurrence is the
 * one value, so building costs the size of the recipe (and `n` for copies), never the weight. Nothing here encodes or
 * renders a built value: only its leaf and templates, which are small.
 */
object BuiltValues {

  sealed trait Step extends Product with Serializable
  final case class Wrap(template: CanonicalValue, times: Int) extends Step
  final case class Copies(n: Int) extends Step

  final case class Built(leaf: CanonicalValue, steps: List[Step]) {
    def wrap(template: CanonicalValue, times: Int = 1): Built = Built(leaf, steps :+ Wrap(template, times))
    def copies(n:      Int): Built = Built(leaf, steps :+ Copies(n))
  }

  /** The text a template marks the value so far with. */
  val Hole: CanonicalValue = get(CanonicalValue.text("$"))

  def of(leaf: CanonicalValue): Built = Built(leaf, Nil)

  /** A template of `[$, $]`: each application doubles the value. */
  val Pair: CanonicalValue = get(CanonicalValue.array(Vector(Hole, Hole)))

  /** `t` with every `"$"` replaced by `v` (shared), stack-safely; templates are small. */
  def fill(t: CanonicalValue, v: CanonicalValue): Either[CanonicalError, CanonicalValue] = {
    def go(x: CanonicalValue): Eval[Either[CanonicalError, CanonicalValue]] =
      x match {
        case h if h == Hole => Eval.now(v.asRight)
        case CArray(vs) =>
          vs.traverse(e => Eval.defer(go(e))).map(_.sequence.flatMap(CanonicalValue.array))
        case CMap(m) =>
          m.toList.traverse { case (k, e) => Eval.defer(go(e)).map(_.map(k -> _)) }.map(_.sequence.flatMap(CanonicalValue.map))
        case other => Eval.now(other.asRight)
      }
    go(t).value
  }

  def build(b: Built): Either[CanonicalError, CanonicalValue] =
    b.steps.foldLeftM(b.leaf) {
      case (v, Wrap(t, n)) => (1 to n).toList.foldLeftM(v)((x, _) => fill(t, x))
      case (v, Copies(n))  => CanonicalValue.array(Vector.fill(n)(v))
    }

  /** The recipe as the schemas' `built-value`. */
  def json(b: Built): Json =
    Json.fromFields(
      List("leaf" -> Cbor.encode(b.leaf).toHex.asJson) ++
      Option.when(b.steps.nonEmpty)("steps" -> b.steps.map {
        case Wrap(t, n) => Json.obj("template" -> Cbor.encode(t).toHex.asJson, "times" -> n.asJson)
        case Copies(n)  => Json.obj("copies" -> n.asJson)
      }.asJson)
    )

  /** A recipe from the schemas' `built-value`. */
  def fromJson(j: ACursor): Either[String, Built] = {
    def value(c: ACursor, name: String): Either[String, CanonicalValue] =
      c.get[String](name).leftMap(_.toString).flatMap(ByteVector.fromHexDescriptive(_)).flatMap(Cbor.decode(_).leftMap(_.toString))
    val steps = j.downField("steps").success.fold(List.empty[Step].asRight[String]) { s =>
      s.as[List[Json]]
        .leftMap(_.toString)
        .flatMap(_.traverse { step =>
          val c = step.hcursor
          c.get[Int]("copies")
            .toOption
            .fold((value(c, "template"), c.get[Int]("times").leftMap(_.toString)).mapN(Wrap(_, _): Step))(n =>
              (Copies(n): Step).asRight[String]
            )
        })
    }
    (value(j, "leaf"), steps).mapN(Built(_, _))
  }

  /** The value a `built-value` describes. */
  def fromJsonValue(j: ACursor): Either[String, CanonicalValue] = fromJson(j).flatMap(build(_).leftMap(_.toString))

  /**
   * A recipe whose value weighs exactly `target` (at least 1): from a leaf of weight 1 or 2, steps `[$, $]` (`w` to
   * `2w + 1`) and `[$, $, null]` (`w` to `2w + 2`), one per bit, so the recipe stays O(log target).
   */
  def ofWeight(target: Long): Built = {
    val pairNull = get(CanonicalValue.array(Vector(Hole, Hole, CNull)))
    @scala.annotation.tailrec
    def back(w: Long, steps: List[Step]): Built =
      if (w <= 2) Built(if (w == 1) CNull else CanonicalValue.bytes(ByteVector(0)), steps)
      else if (w % 2 == 1) back((w - 1) / 2, Wrap(Pair, 1) :: steps)
      else back((w - 2) / 2, Wrap(pairNull, 1) :: steps)
    back(target, Nil)
  }

  private def get[A](e: Either[CanonicalError, A]): A = e.fold(err => throw new IllegalStateException(err.toString), identity)
}
