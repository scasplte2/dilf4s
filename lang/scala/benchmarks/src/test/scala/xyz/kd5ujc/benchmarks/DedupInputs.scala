package xyz.kd5ujc.benchmarks

import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.jlvm.{JlvmError, JsonSurface}

/** Compact host-input recipes keep parsing and duplicate copies out of the largest deduplication benchmarks. */
object DedupInputs {
  private def index(i: Int, n: Int, order: String): Int = order match {
    case "ascending"  => i
    case "descending" => n - 1 - i
    case _            => ((i.toLong * 104729L) % n).toInt
  }

  def read(text: String): Either[JlvmError, CanonicalValue] =
    text.split(':').toList match {
      case "dedup" :: kind :: count :: order :: Nil =>
        for {
          n <- count.toIntOption.filter(n => n > 0 && n <= (1 << 24)).toRight(JlvmError.Malformed("invalid dedup input count"))
          _ <- Either.cond(Set("int", "short", "prefix")(kind), (), JlvmError.Malformed("invalid dedup input kind"))
          _ <- Either.cond(Set("ascending", "descending", "permuted")(order), (), JlvmError.Malformed("invalid dedup input order"))
          _ <- Either.cond(BigInt(n).gcd(BigInt(104729)) == 1, (), JlvmError.Malformed("dedup permutation must be bijective"))
          values <-
            if (kind == "int") Right(Vector.tabulate(n)(i => CInt(index(i, n, order).toLong)))
            else
              Vector
                .tabulate(n) { i =>
                  CanonicalValue.text((if (kind == "prefix") "k" * 1024 else "") + f"${index(i, n, order)}%08d")
                }
                .sequence
                .leftMap(e => JlvmError.Malformed(e.toString))
          array <- CanonicalValue.array(values).leftMap(e => JlvmError.Malformed(e.toString))
          data  <- CanonicalValue.map(Vector("a0" -> array, "a1" -> array)).leftMap(e => JlvmError.Malformed(e.toString))
        } yield data
      case _ => JsonSurface.parse(text)
    }
}
