package xyz.kd5ujc.jlvm.difffuzz

import cats.data.{NonEmptyVector, State}
import cats.syntax.all._

/**
 * The generator's randomness: SplitMix64 (Steele, Lea and Flood, 2014), a pure function of a 64-bit state, so a seed
 * gives the same cases on every JVM. Case `i` of seed `s` starts from [[Rng.seedOf]]`(s, i)`, so one case is
 * regenerated alone (`--seed s --case i`).
 */
object Rng {

  private val Golden: Long = 0x9e3779b97f4a7c15L

  def mix(z0: Long): Long = {
    val z1 = (z0 ^ (z0 >>> 30)) * 0xbf58476d1ce4e5b9L
    val z2 = (z1 ^ (z1 >>> 27)) * 0x94d049bb133111ebL
    z2 ^ (z2 >>> 31)
  }

  def seedOf(seed: Long, index: Long): Long = mix(mix(seed) ^ mix(index * Golden + 1))
}

/**
 * What a generator threads: the random state, and the input data and host bindings the program being built reads (its
 * values are placed there, as `{"var": "a3"}` or `{"var": "$h1"}`, as often as they are written inline).
 */
final case class GenState(rng: Long, data: Vector[(String, Sj)], env: Vector[(String, Sj)])

object Gen {

  type G[A] = State[GenState, A]

  def pure[A](a: A): G[A] = State.pure(a)

  /** `g`, built only when run: for a generator that recurses. */
  def defer[A](g: => G[A]): G[A] = State.pure[GenState, Unit](()).flatMap(_ => g)

  val long: G[Long] = State { s =>
    val t = s.rng + 0x9e3779b97f4a7c15L
    (s.copy(rng = t), Rng.mix(t))
  }

  /** Uniform in `[0, n)`, for `n >= 1`. */
  def below(n: Int): G[Int] = long.map(x => java.lang.Long.remainderUnsigned(x, n.max(1).toLong).toInt)

  def between(lo: Int, hi: Int): G[Int] = below(hi - lo + 1).map(_ + lo)

  def chance(p: Double): G[Boolean] = long.map(x => (x >>> 11).toDouble / (1L << 53).toDouble < p)

  def pick[A](xs: NonEmptyVector[A]): G[A] = below(xs.length).map(i => xs.getUnsafe(i))

  def oneOf[A](first: A, rest: A*): G[A] = pick(NonEmptyVector(first, rest.toVector))

  def choose[A](first: G[A], rest: G[A]*): G[A] = oneOf(first, rest*).flatten

  /** Each generator with its weight. */
  def weighted[A](first: (Int, G[A]), rest: (Int, G[A])*): G[A] = {
    val all = NonEmptyVector(first, rest.toVector)
    below(all.map(_._1).toVector.sum).map { r =>
      all.toVector
        .scanLeft(0)(_ + _._1)
        .drop(1)
        .zip(all.toVector)
        .collectFirst { case (upTo, (_, g)) if r < upTo => g }
        .getOrElse(all.head._2)
    }.flatten
  }

  def vectorOf[A](n: Int, g: G[A]): G[Vector[A]] = Vector.fill(n)(g).sequence

  def upTo[A](max: Int, g: G[A]): G[Vector[A]] = between(0, max).flatMap(vectorOf(_, g))

  /** A data field holding `v`, read by `{"var": key}`: a fresh key per field. */
  def inData(v: Sj): G[Sj] = State { s =>
    val key = s"a${s.data.size}"
    (s.copy(data = s.data :+ (key -> v)), Sj.obj("var", Sj.Str(key)))
  }

  /** A host binding of `v`, read by `{"var": "$key"}`. */
  def inEnv(v: Sj): G[Sj] = State { s =>
    val key = s"h${s.env.size}"
    (s.copy(env = s.env :+ (key -> v)), Sj.obj("var", Sj.Str("$" + key)))
  }

  val dataKeys: G[Vector[String]] = State.inspect(_.data.map(_._1))
}
