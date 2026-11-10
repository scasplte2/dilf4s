package xyz.kd5ujc.core.canonical

import cats.Eval
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.models.refusal_vectors.{BuiltCopies, BuiltStep, BuiltValue, BuiltWrap}

import scodec.bits.ByteVector

/**
 * The `built-value` recipes of the vector schemas (shared/schemas/refusal-vectors.cddl, jlvm-vectors.cddl): a value with
 * shared parts, far heavier than its description, written as a leaf and steps. A step `Wrap(t, n)` replaces, `n` times
 * over, every text `"$"` in `t` by the value so far; `Copies(n)` makes an array of `n` of it. Every occurrence is the
 * one value, so building costs the size of the recipe (and `n` for copies), never the weight. Nothing here encodes or
 * renders a built value: only its leaf and templates, which are small.
 */
object BuiltValues {

  enum ReadError {
    case InvalidHex(input: Preview)
    case Canonical(cause: CanonicalError)
    case Decode(cause: DecodeFailure)
    case CountOutOfRange(bits: Int, negative: Boolean)
  }

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
  def model(b: Built): BuiltValue =
    BuiltValue(
      CanonicalGens.encoded(b.leaf).toHex,
      Option.when(b.steps.nonEmpty)(b.steps.toVector.map {
        case Wrap(t, n) => BuiltWrap(CanonicalGens.encoded(t).toHex, BigInt(n))
        case Copies(n)  => BuiltCopies(BigInt(n))
      })
    )

  /** The recipe as a canonical value: the schemas' `built-value`, for a vector file. */
  def value(b: Built): CanonicalValue = get(BuiltValue.codec.encode(model(b)))

  /** A recipe from the schemas' `built-value`. */
  def fromModel(m: BuiltValue): Either[ReadError, Built] = {
    def decoded(hex: String): Either[ReadError, CanonicalValue] =
      ByteVector.fromHex(hex).toRight(ReadError.InvalidHex(Preview.of(hex))).flatMap(Cbor.decode(_).leftMap(ReadError.Canonical(_)))
    def count(n: BigInt): Either[ReadError, Int] =
      Either.cond(n.isValidInt && n >= 0, n.toInt, ReadError.CountOutOfRange(n.bitLength, n < 0))
    def step(s: BuiltStep): Either[ReadError, Step] =
      s match {
        case BuiltWrap(t, n) => (decoded(t), count(n)).mapN(Wrap(_, _))
        case BuiltCopies(n)  => count(n).map(Copies(_))
      }
    val steps = m.steps.getOrElse(Vector.empty).toList.traverse(step)
    (decoded(m.leaf), steps).mapN(Built(_, _))
  }

  /** A recipe from a vector file's `built-value`, read by the canonical parser. */
  def fromValue(v: CanonicalValue): Either[ReadError, Built] =
    BuiltValue.codec.decoder.decode(v).leftMap(ReadError.Decode(_)).flatMap(fromModel)

  /** The value a `built-value` describes. */
  def built(v: CanonicalValue): Either[ReadError, CanonicalValue] = fromValue(v).flatMap(build(_).leftMap(ReadError.Canonical(_)))

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
