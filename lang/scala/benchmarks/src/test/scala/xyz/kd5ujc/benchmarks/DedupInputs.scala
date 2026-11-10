package xyz.kd5ujc.benchmarks

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.core.canonical.{CanonicalError, CanonicalValue, Preview}
import xyz.kd5ujc.jlvm.{JlvmError, JsonSurface}

/** Compact host-input recipes keep parsing and duplicate copies out of the largest deduplication benchmarks. */
object DedupInputs {
  enum Kind { case Integer, ShortText, CommonPrefix }
  enum Order { case Ascending, Descending, Permuted, Repeated }
  enum Error {
    case Count(value: Preview)
    case InputKind(value: Preview)
    case InputOrder(value: Preview)
    case NonBijective(count: Int)
    case Canonical(cause: CanonicalError)
    case Surface(cause: JlvmError)
  }

  private def kind(s: String): Either[Error, Kind] = s match {
    case "int"    => Kind.Integer.asRight
    case "short"  => Kind.ShortText.asRight
    case "prefix" => Kind.CommonPrefix.asRight
    case other    => Error.InputKind(Preview.of(other)).asLeft
  }

  private def order(s: String): Either[Error, Order] = s match {
    case "ascending"  => Order.Ascending.asRight
    case "descending" => Order.Descending.asRight
    case "permuted"   => Order.Permuted.asRight
    case "repeated"   => Order.Repeated.asRight
    case other        => Error.InputOrder(Preview.of(other)).asLeft
  }

  private def index(i: Int, n: Int, order: Order): Int = order match {
    case Order.Ascending  => i
    case Order.Descending => n - 1 - i
    case Order.Repeated   => i                     % 32
    case Order.Permuted   => ((i.toLong * 104729L) % n).toInt
  }

  private def textValues(n: Int, o: Order, prefix: String): Either[Error, Vector[CanonicalValue]] =
    Vector.tabulate(n)(i => CanonicalValue.text(prefix + f"${index(i, n, o)}%08d")).sequence.leftMap(Error.Canonical(_))

  def read(text: String): Either[Error, CanonicalValue] =
    text.split(':').toList match {
      case "dedup" :: kindText :: count :: orderText :: Nil =>
        for {
          n <- count.toIntOption.filter(n => n > 0 && n <= (1 << 24)).toRight(Error.Count(Preview.of(count)))
          k <- kind(kindText)
          o <- order(orderText)
          _ <- Either.cond(BigInt(n).gcd(BigInt(104729)) == 1, (), Error.NonBijective(n))
          values <- k match {
            case Kind.Integer      => Right(Vector.tabulate(n)(i => CInt(index(i, n, o).toLong)))
            case Kind.ShortText    => textValues(n, o, "")
            case Kind.CommonPrefix => textValues(n, o, "k" * 1024)
          }
          array    <- CanonicalValue.array(values).leftMap(Error.Canonical(_))
          expected <- CanonicalValue.array(values.take(32)).leftMap(Error.Canonical(_))
          fields = Vector("a0" -> array, "a1" -> array) ++ Option.when(o == Order.Repeated)("a2" -> expected)
          data <- CanonicalValue.map(fields).leftMap(Error.Canonical(_))
        } yield data
      case _ => JsonSurface.parse(text).leftMap(Error.Surface(_))
    }
}
