package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8

import cats.syntax.all._

import scala.annotation.tailrec
import scala.collection.immutable.{Map => SMap}

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.hash.{Blake2b256 => B2b, Hasher, Sha256 => S256, Sha3_256 => S3}
import xyz.kd5ujc.numerics.Ratio

import scodec.bits.ByteVector

/**
 * Operations on already-evaluated values: everything except the lazy and binding forms (`if`, `and`, `or`, `default`,
 * `let`), the callback forms (`map`, `filter`, `reduce`, `all`, `some`, `none`, `find`, and `count` with a callback),
 * the data lookups (`missing`, `missing_some`) and the cryptographic verifiers, which the evaluator owns.
 *
 * Typing is strict (docs/specs/jlvm.md): a value of the wrong kind is a [[JlvmError.Type]], never coerced. The one
 * deliberate exception is metakit's handling of an **absent** value (null) in `in`, `intersect`, `startsWith`,
 * `endsWith`, `keys`, `values`, `entries`, `cat` and `join`, kept because programs read optional fields through them.
 */
object PureOps {

  import Values._

  type Result = Either[JlvmError, CanonicalValue]

  /** The largest exponent magnitude `pow` accepts (metakit's bound). */
  val MaxExponent: Int = 999

  /** The largest `int_to_bytes` width. */
  val MaxIntBytes: Int = 1024

  /** `None` for operations the evaluator owns; otherwise the result of applying `op` to `args`. */
  def apply(op: Op, args: Vector[CanonicalValue]): Option[Result] = {
    import Op.{Map => MapOp, None => NoneOp, Set => SetOp, Some => SomeOp, _}
    op match {
      case If | And | Or | Default | Let | MapOp | Filter | Reduce | All | SomeOp | NoneOp | Find | Missing | MissingSome | Ed25519Verify |
          Secp256k1Verify | Bip340Verify | BlsVerify | BlsAggregateVerify | Poseidon | PmtVerify | Groth16Verify | EcvrfVerify | Bn254Add |
          Bn254Mul | Bn254Pairing | SchnorrVerify | ProveDlogVerify | ProveDhTupleVerify | SigmaVerify | MerkleVerify |
          MerkleConsistencyVerify | TrieVerify | TriePrefixVerify | TrieRangeVerify =>
        Option.empty

      case Not    => Option(unary(op, args)(v => bool(!truthy(v)).asRight))
      case Truthy => Option(unary(op, args)(v => bool(truthy(v)).asRight))

      case Eq | StrictEq => Option(binary(op, args)((a, b) => bool(equal(a, b)).asRight))
      case Ne | StrictNe => Option(binary(op, args)((a, b) => bool(!equal(a, b)).asRight))
      case Lt            => Option(ordering(op, args, _ < 0, between = true))
      case Le            => Option(ordering(op, args, _ <= 0, between = true))
      case Gt            => Option(ordering(op, args, _ > 0, between = false))
      case Ge            => Option(ordering(op, args, _ >= 0, between = false))

      case Add         => Option(total(op, args, Ratio.Zero)(_ + _))
      case Mul         => Option(total(op, args, Ratio.One)(_ * _))
      case Min         => Option(fold(op, args)(_.min(_)))
      case Max         => Option(fold(op, args)(_.max(_)))
      case Sub         => Option(subtract(op, args))
      case Div         => Option(numBinary(op, args)((a, b) => (a / b).leftMap(_ => divisionByZero(op))))
      case Mod         => Option(numBinary(op, args)((a, b) => a.rem(b).leftMap(_ => divisionByZero(op))))
      case Pow         => Option(numBinary(op, args)(power(op)))
      case Abs         => Option(numUnary(op, args)(_.abs))
      case Round       => Option(numUnary(op, args)(r => Ratio.integer(r.roundHalfEven)))
      case RoundHalfUp => Option(numUnary(op, args)(r => Ratio.integer(r.roundHalfUp)))
      case Floor       => Option(numUnary(op, args)(r => Ratio.integer(r.floor)))
      case Ceil        => Option(numUnary(op, args)(r => Ratio.integer(r.ceil)))

      case Cat                   => Option(cat(op, args))
      case Substr                => Option(substr(op, args))
      case Lower                 => Option(unary(op, args)(v => str(op, v).flatMap(s => text(op, lower(s)))))
      case Upper                 => Option(unary(op, args)(v => str(op, v).flatMap(s => text(op, upper(s)))))
      case Trim                  => Option(unary(op, args)(v => str(op, v).flatMap(s => text(op, trim(s)))))
      case StartsWith | EndsWith => Option(affix(op, args))
      case Join                  => Option(join(op, args))
      case Split                 => Option(split(op, args))
      case Length                => Option(unary(op, args)(length(op)))

      case Merge     => Option(merge(op, args))
      case In        => Option(binary(op, args)(contains(op)))
      case Intersect => Option(binary(op, args)(subset(op)))
      case Unique    => Option(unary(op, args)(v => arr(op, v).flatMap(vs => array(op, vs.distinct))))
      case Reverse   => Option(unary(op, args)(v => arr(op, v).flatMap(vs => array(op, vs.reverse))))
      case Flatten   => Option(unary(op, args)(v => arr(op, v).flatMap(vs => array(op, vs.flatMap(spread)))))
      case Slice     => Option(slice(op, args))
      case Count     => Option(unary(op, args)(v => arr(op, v).map(vs => count(vs.size.toLong))))

      case Keys | Values | Entries => Option(listing(op, args))
      case Get                     => Option(get(op, args))
      case Has                     => Option(has(op, args))
      case SetOp                   => Option(set(op, args))
      case Unset                   => Option(unset(op, args))

      case Exists => Option(bool(!args.contains(CNull)).asRight)
      case TypeOf => Option(unary(op, args)(v => text(op, kind(v))))

      case Hex        => Option(unary(op, args)(v => rawBytes(op, v).flatMap(b => text(op, "0x" + b.toHex))))
      case Unhex      => Option(unary(op, args)(v => hexBytes(op, v).flatMap(bytesValue(op, _))))
      case HexToInt   => Option(unary(op, args)(v => hexBytes(op, v).flatMap(unsignedInt(op))))
      case BytesToInt => Option(unary(op, args)(v => rawBytes(op, v).flatMap(unsignedInt(op))))
      case IntToBytes => Option(intToBytes(op, args))
      case Sha256     => Option(unary(op, args)(digest[S256](op)))
      case Sha3_256   => Option(unary(op, args)(digest[S3](op)))
      case Blake2b256 => Option(unary(op, args)(digest[B2b](op)))
    }
  }

  // ---- argument shapes

  private def unary(op: Op, args: Vector[CanonicalValue])(f: CanonicalValue => Result): Result =
    args match {
      case Vector(a) => f(a)
      case _         => arity(op, s"takes 1 argument, got ${args.size}")
    }

  private def binary(op: Op, args: Vector[CanonicalValue])(f: (CanonicalValue, CanonicalValue) => Result): Result =
    args match {
      case Vector(a, b) => f(a, b)
      case _            => arity(op, s"takes 2 arguments, got ${args.size}")
    }

  private def numUnary(op: Op, args: Vector[CanonicalValue])(f: Ratio => Ratio): Result =
    unary(op, args)(v => numeric(op, v).flatMap(r => num(op, f(r))))

  private def numBinary(op: Op, args: Vector[CanonicalValue])(f: (Ratio, Ratio) => Either[JlvmError, Ratio]): Result =
    binary(op, args)((a, b) => (numeric(op, a), numeric(op, b)).tupled.flatMap(f.tupled).flatMap(num(op, _)))

  // ---- kinds

  private def tpe[A](op:         Op, msg: String): Either[JlvmError, A] = JlvmError.Type(op.wire, msg).asLeft
  private def arity[A](op:       Op, msg: String): Either[JlvmError, A] = JlvmError.Arity(op.wire, msg).asLeft
  private def domain[A](op:      Op, msg: String): Either[JlvmError, A] = JlvmError.Domain(op.wire, msg).asLeft
  private def divisionByZero(op: Op): JlvmError = JlvmError.Arithmetic(op.wire, "division by zero")

  private def str(op: Op, v: CanonicalValue): Either[JlvmError, String] =
    v match {
      case CText(s) => s.asRight
      case other    => tpe(op, s"expected a string, got ${kind(other)}")
    }

  private def arr(op: Op, v: CanonicalValue): Either[JlvmError, Vector[CanonicalValue]] =
    v match {
      case CArray(vs) => vs.asRight
      case other      => tpe(op, s"expected an array, got ${kind(other)}")
    }

  private def obj(op: Op, v: CanonicalValue): Either[JlvmError, SMap[String, CanonicalValue]] =
    record(op, v).map(_.entries)

  private def record(op: Op, v: CanonicalValue): Either[JlvmError, CMap] =
    v match {
      case m: CMap => m.asRight
      case other   => tpe(op, s"expected an object, got ${kind(other)}")
    }

  private def key(op: Op, v: CanonicalValue): Either[JlvmError, CText] =
    v match {
      case t: CText => t.asRight
      case other    => tpe(op, s"expected a string key, got ${kind(other)}")
    }

  private def rawBytes(op: Op, v: CanonicalValue): Either[JlvmError, ByteVector] =
    v match {
      case CBytes(b) => b.asRight
      case other     => tpe(op, s"expected bytes, got ${kind(other)}")
    }

  /** An integer index; out-of-range values saturate, since every use clamps to a collection size anyway. */
  private def index(op: Op, v: CanonicalValue): Either[JlvmError, Long] =
    v match {
      case CInt(i) => (if (i.isValidLong) i.toLong else if (i.signum < 0) Long.MinValue else Long.MaxValue).asRight
      case other   => tpe(op, s"expected an integer, got ${kind(other)}")
    }

  private def spread(v: CanonicalValue): Vector[CanonicalValue] =
    v match {
      case CArray(vs) => vs
      case other      => Vector(other)
    }

  private def isArray(v: CanonicalValue): Boolean = v match {
    case CArray(_) => true
    case _         => false
  }

  private def isObject(v: CanonicalValue): Boolean = v match {
    case CMap(_) => true
    case _       => false
  }

  /** JavaScript `slice` bounds: a negative index counts from the end; the result is clamped to `[0, n]`. */
  private def bound(i: Long, n: Int): Int =
    if (i < 0) math.max(0L, n.toLong + i).toInt else math.min(i, n.toLong).toInt

  // ---- comparison and arithmetic

  /** `<` and `<=` also take three arguments, meaning "between" (`a < b < c`). */
  private def ordering(op: Op, args: Vector[CanonicalValue], holds: Int => Boolean, between: Boolean): Result =
    args match {
      case Vector(a, b)               => compare(op, a, b).map(c => bool(holds(c)))
      case Vector(a, b, c) if between => (compare(op, a, b), compare(op, b, c)).mapN((x, y) => bool(holds(x) && holds(y)))
      case _                          => arity(op, if (between) "takes 2 or 3 arguments" else "takes 2 arguments")
    }

  private def fold(op: Op, args: Vector[CanonicalValue])(f: (Ratio, Ratio) => Ratio): Result =
    args
      .traverse(numeric(op, _))
      .flatMap(_.reduceLeftOption(f).toRight(JlvmError.Arity(op.wire, "takes at least 1 argument")))
      .flatMap(num(op, _))

  /** `+` and `*` of no arguments are their identities, so a spread over an empty array is 0 or 1. */
  private def total(op: Op, args: Vector[CanonicalValue], identity: Ratio)(f: (Ratio, Ratio) => Ratio): Result =
    args.traverse(numeric(op, _)).map(_.foldLeft(identity)(f)).flatMap(num(op, _))

  private def subtract(op: Op, args: Vector[CanonicalValue]): Result =
    args.traverse(numeric(op, _)).flatMap {
      case Vector(a)    => num(op, -a)
      case Vector(a, b) => num(op, a - b)
      case rs           => arity(op, s"takes 1 or 2 arguments, got ${rs.size}")
    }

  /**
   * An integer exponent in `-999..999`. The result's size is checked **before** computing it: a few chained `pow` calls
   * could otherwise build a number of gigabytes while each step's input still looks small to the gas schedule.
   */
  private def power(op: Op)(base: Ratio, exponent: Ratio): Either[JlvmError, Ratio] =
    exponent.toBigIntExact.filter(_.abs <= MaxExponent) match {
      case scala.None =>
        JlvmError.Arithmetic(op.wire, s"the exponent must be an integer in -$MaxExponent..$MaxExponent").asLeft
      case scala.Some(e) =>
        val bits = math.max(base.numerator.bitLength, base.denominator.bitLength).toLong * e.abs.toLong
        if (bits > MaxNumberBits) JlvmError.Limit(s"${op.wire}: the result would exceed $MaxNumberBits bits").asLeft
        else base.pow(e.toInt).leftMap(_ => divisionByZero(op))
    }

  // ---- text

  /** Exact decimal text of a number; a non-terminating rational has none. */
  private def render(op: Op, r: Ratio): Either[JlvmError, String] =
    r.toDecimalString.toRight(JlvmError.Arithmetic(op.wire, s"$r has no finite decimal form"))

  /** A piece of `cat` or `join`: a string, a number (exact decimal) or a boolean; null (absent) is empty. */
  private def piece(op: Op, v: CanonicalValue): Either[JlvmError, String] =
    v match {
      case CText(s) => s.asRight
      case CNull    => "".asRight
      case CBool(b) => b.toString.asRight
      case other =>
        number(other).fold(tpe[String](op, s"cannot render ${kind(other)} as text"))(_ => numeric(op, other).flatMap(render(op, _)))
    }

  /** Concatenates text; when every argument is an array, concatenates the arrays instead. */
  private def cat(op: Op, args: Vector[CanonicalValue]): Result =
    if (args.nonEmpty && args.forall(isArray)) array(op, args.flatMap(spread))
    else args.traverse(piece(op, _)).flatMap(ps => text(op, ps.mkString))

  /** `[text, start]` or `[text, start, length]`, counted in code points; a negative length drops from the end. */
  private def substr(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(s, start, rest*) if rest.sizeIs <= 1 =>
        (str(op, s), index(op, start), rest.headOption.traverse(index(op, _))).tupled.flatMap {
          case (t, i, len) =>
            val cps = codePoints(t)
            val from = bound(i, cps.size)
            val to = len.fold(cps.size) { l =>
              if (l < 0) bound(l, cps.size) else from + math.min(l, (cps.size - from).toLong).toInt
            }
            text(op, fromCodePoints(cps.slice(from, math.max(from, to))))
        }
      case _ => arity(op, "takes [text, start] or [text, start, length]")
    }

  /** Null on either side means "not present" and gives `false`, as in metakit. */
  private def affix(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(CNull, _) | Vector(CText(_), CNull) => bool(false).asRight
      case Vector(s, p) =>
        (str(op, s), str(op, p)).mapN((a, b) => bool(if (op == Op.StartsWith) a.startsWith(b) else a.endsWith(b)))
      case _ => arity(op, s"takes 2 arguments, got ${args.size}")
    }

  private def join(op: Op, args: Vector[CanonicalValue]): Result =
    binary(op, args) { (a, sep) =>
      (arr(op, a), str(op, sep)).tupled.flatMap {
        case (vs, s) =>
          vs.traverse(piece(op, _)).flatMap(ps => text(op, ps.mkString(s)))
      }
    }

  private def split(op: Op, args: Vector[CanonicalValue]): Result =
    binary(op, args) { (a, b) =>
      (str(op, a), str(op, b)).tupled.flatMap {
        case (_, "") => domain(op, "the separator must not be empty")
        case (t, s)  => pieces(t, s, 0, Vector.empty).traverse(text(op, _)).flatMap(array(op, _))
      }
    }

  /** Every piece between separators, empty ones included (JavaScript `split`). */
  @tailrec
  private def pieces(t: String, sep: String, from: Int, acc: Vector[String]): Vector[String] =
    indexOf(t, sep, from) match {
      case -1 => acc :+ t.substring(from)
      case at => pieces(t, sep, at + sep.length, acc :+ t.substring(from, at))
    }

  /** Code points of a string, elements of an array, bytes of a byte string, entries of an object. */
  private def length(op: Op)(v: CanonicalValue): Result =
    v match {
      case CText(s)   => count(s.codePointCount(0, s.length).toLong).asRight
      case CArray(vs) => count(vs.size.toLong).asRight
      case CBytes(b)  => count(b.size).asRight
      case CMap(m)    => count(m.size.toLong).asRight
      case other      => tpe(op, s"cannot take the length of ${kind(other)}")
    }

  // ---- collections

  /** Objects merge left to right (last wins); otherwise arrays are concatenated and other values appended. */
  private def merge(op: Op, args: Vector[CanonicalValue]): Result =
    if (args.forall(isObject))
      args.traverse(record(op, _)).flatMap(ms => bounded(op, CanonicalValue.merged(ms)))
    else array(op, args.flatMap(spread))

  /** Substring, byte substring, or array membership; a null needle is never present (metakit). */
  private def contains(op: Op)(needle: CanonicalValue, haystack: CanonicalValue): Result =
    (needle, haystack) match {
      case (CNull, _)             => bool(false).asRight
      case (CText(n), CText(h))   => bool(indexOf(h, n, 0) >= 0).asRight
      case (CBytes(n), CBytes(h)) => bool(indexOf(h, n) >= 0).asRight
      case (n, CArray(vs))        => bool(vs.contains(n)).asRight
      case (n, h)                 => tpe(op, s"cannot look for ${kind(n)} in ${kind(h)}")
    }

  /** Whether every element of the first array is in the second; null is the empty set (metakit). */
  private def subset(op: Op)(sub: CanonicalValue, sup: CanonicalValue): Result =
    (sub, sup) match {
      case (CNull, _)         => bool(true).asRight
      case (CArray(_), CNull) => bool(false).asRight
      case (a, b) =>
        (arr(op, a), arr(op, b)).mapN { (xs, ys) =>
          val present = ys.toSet
          bool(xs.forall(present.contains))
        }
    }

  /** `[array, start]` or `[array, start, end]`, with JavaScript bounds. */
  private def slice(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(a, s, rest*) if rest.sizeIs <= 1 =>
        (arr(op, a), index(op, s), rest.headOption.traverse(index(op, _))).tupled.flatMap {
          case (vs, from, to) =>
            val i = bound(from, vs.size)
            array(op, vs.slice(i, math.max(i, to.fold(vs.size)(bound(_, vs.size)))))
        }
      case _ => arity(op, "takes [array, start] or [array, start, end]")
    }

  // ---- objects

  /** Keys, values or `[key, value]` pairs in code-point key order; null (absent) gives null, as in metakit. */
  private def listing(op: Op, args: Vector[CanonicalValue]): Result =
    unary(op, args) {
      case CNull => CNull.asRight
      case v =>
        obj(op, v).flatMap { m =>
          val ks = sortedKeys(m).toVector
          op match {
            case Op.Keys   => ks.traverse(text(op, _)).flatMap(array(op, _))
            case Op.Values => array(op, ks.map(m))
            case _         => ks.traverse(k => text(op, k).flatMap(t => array(op, Vector(t, m(k))))).flatMap(array(op, _))
          }
        }
    }

  private def get(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(m, k, rest*) if rest.sizeIs <= 1 =>
        (obj(op, m), str(op, k)).mapN((o, key) => o.getOrElse(key, rest.headOption.getOrElse(CNull)))
      case _ => arity(op, "takes [object, key] or [object, key, default]")
    }

  private def has(op: Op, args: Vector[CanonicalValue]): Result =
    binary(op, args)((m, k) => (obj(op, m), str(op, k)).mapN((o, key) => bool(o.contains(key))))

  /** Setting a key to null removes it: canonical maps never hold nulls. */
  private def set(op: Op, args: Vector[CanonicalValue]): Result =
    args match {
      case Vector(m, k, v) =>
        (record(op, m), key(op, k)).tupled.flatMap {
          case (o, kt) =>
            CanonicalValue.updated(o, kt, v).leftMap(e => JlvmError.Limit(s"${op.wire}: $e")).flatMap(bounded(op, _))
        }
      case _ => arity(op, s"takes [object, key, value], got ${args.size} arguments")
    }

  private def unset(op: Op, args: Vector[CanonicalValue]): Result =
    binary(op, args)((m, k) => (record(op, m), key(op, k)).mapN(CanonicalValue.removed))

  // ---- bytes

  /** Bytes, or text of the form `0x` + lowercase hex (metakit's hex syntax), e.g. text built at runtime by `cat`. */
  private def hexBytes(op: Op, v: CanonicalValue): Either[JlvmError, ByteVector] =
    v match {
      case CBytes(b) => b.asRight
      case CText(s) =>
        JsonSurface
          .hexBytes(s)
          .toRight(JlvmError.Domain(op.wire, "expected 0x followed by an even number of lowercase hex digits"))
      case other => tpe(op, s"expected bytes or hex text, got ${kind(other)}")
    }

  /** Hashes bytes, or the UTF-8 encoding of a string. */
  private def digest[H: Hasher](op: Op)(v: CanonicalValue): Result =
    v match {
      case CBytes(b) => CBytes(Hasher[H].digest(b).bytes).asRight
      case CText(s)  => CBytes(Hasher[H].digest(ByteVector(s.getBytes(UTF_8))).bytes).asRight
      case other     => tpe(op, s"expected bytes or a string, got ${kind(other)}")
    }

  /** A big-endian unsigned integer, within the number bound. */
  private def unsignedInt(op: Op)(b: ByteVector): Result =
    Either
      .cond(b.dropWhile(_ == 0).size * 8 <= MaxNumberBits.toLong, b, JlvmError.Limit(s"${op.wire}: a number exceeds $MaxNumberBits bits"))
      .flatMap(bs => int(op, BigInt(1, bs.toArray)))

  /** Big-endian and unsigned: minimal (`0` is empty), or left-padded to a fixed width. */
  private def intToBytes(op: Op, args: Vector[CanonicalValue]): Result = {
    def unsigned(v: CanonicalValue): Either[JlvmError, ByteVector] =
      integer(op, v).flatMap(i =>
        Either.cond(i.signum >= 0, ByteVector(i.toByteArray).dropWhile(_ == 0), JlvmError.Domain(op.wire, "negative integer"))
      )
    def width(v: CanonicalValue): Either[JlvmError, Long] =
      integer(op, v).flatMap(n =>
        Either.cond(n >= 0 && n <= MaxIntBytes, n.toLong, JlvmError.Domain(op.wire, s"the width must be in 0..$MaxIntBytes"))
      )
    args match {
      case Vector(i) => unsigned(i).map(CBytes(_))
      case Vector(i, n) =>
        (unsigned(i), width(n)).tupled.flatMap {
          case (raw, w) =>
            Either.cond(raw.size <= w, CBytes(raw.padLeft(w)), JlvmError.Domain(op.wire, s"the integer does not fit in $w bytes"))
        }
      case _ => arity(op, "takes [integer] or [integer, width]")
    }
  }
}
