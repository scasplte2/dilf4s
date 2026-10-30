package xyz.kd5ujc.jlvm

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._

/**
 * `missing` and `missing_some`: which data paths are absent (null). Paths resolve as plain `var` paths do (inside a
 * callback, the element first), never as bindings.
 */
object Lookups {

  def apply(op: Op, args: Vector[CanonicalValue], data: Vector[String] => CanonicalValue): Either[JlvmError, CanonicalValue] =
    op match {
      case Op.Missing     => keys(op, args).flatMap(ks => absent(op, ks, data))
      case Op.MissingSome => some(op, args, data)
      case other          => JlvmError.Malformed(s"${other.wire} is not a lookup").asLeft
    }

  /** The keys: the arguments themselves, or one array argument holding them (a defined form, as `count([array])`). */
  private def keys(op: Op, args: Vector[CanonicalValue]): Either[JlvmError, Vector[CText]] =
    (args match {
      case Vector(CArray(ks)) => ks
      case ks                 => ks
    }).traverse {
      case t: CText => t.asRight
      case other    => JlvmError.Type(op.wire, s"a key is a string path, got ${Values.kind(other)}").asLeft
    }

  private def missing(ks: Vector[CText], data: Vector[String] => CanonicalValue): Vector[CText] =
    ks.filter(k => data(if (k.value.isEmpty) Vector.empty else k.value.split("\\.", -1).toVector) == CNull)

  private def absent(op: Op, ks: Vector[CText], data: Vector[String] => CanonicalValue): Either[JlvmError, CanonicalValue] =
    Values.array(op, missing(ks, data))

  /** `[minimum, [keys]]`: empty when at least `minimum` keys are present, else the missing keys. */
  private def some(op: Op, args: Vector[CanonicalValue], data: Vector[String] => CanonicalValue): Either[JlvmError, CanonicalValue] =
    args match {
      case Vector(min, ks @ CArray(_)) =>
        (Values.integer(op, min), keys(op, Vector(ks))).tupled.flatMap {
          case (n, all) =>
            val gone = missing(all, data)
            if (BigInt(all.size - gone.size) >= n) Values.array(op, Vector.empty) else Values.array(op, gone)
        }
      case _ => JlvmError.Arity(op.wire, "takes [minimum, [keys]]").asLeft
    }
}
