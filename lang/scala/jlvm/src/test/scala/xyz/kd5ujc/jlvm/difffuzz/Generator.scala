package xyz.kd5ujc.jlvm.difffuzz

import cats.data.{NonEmptyVector, State}
import cats.syntax.all._

import xyz.kd5ujc.jlvm.{Op, Parser}

import Gen._
import Sj._

/** A generated case, where it came from, and whether its limit was placed at its exact cost or one below it. */
final case class Generated(c: FuzzCase, source: String, boundary: Boolean, referenceFault: Option[Fault] = None) {
  def replies(actual: Replies): Replies = referenceFault.fold(actual)(f => actual.copy(scala = Left(f)))
}

/**
 * The deterministic generator: case `i` of seed `s` is a function of `(s, i)` alone (and of the code), so a run is
 * reproduced by its seed and one case by `--seed s --case i`. It mixes:
 *
 *   - grammar programs over the closed operation list, rooted at each operation alike, with edge values;
 *   - cryptographic calls on the vector rows' arguments, as they are and mutated;
 *   - mutations of the `jlvm_v1.json` programs (operations swapped within their arity group, literals perturbed and
 *     respelled, arguments dropped or duplicated, the data and gas changed), and the rows unchanged;
 *   - programs nested around the depth bound, and surface text edited as text (whitespace, cuts, a BOM);
 *   - canonical forms, edited at the value and the byte level, read by `decode`.
 *
 * A fifth of the cases then take a gas limit at their exact cost or one below it, asked of the reference
 * (`reference`, which is deterministic too), so the `OutOfGas` boundary is hit on every kind of program.
 */
final class Generator(corpus: Corpus, reference: FuzzCase => Either[Fault, Record], only: Option[String] = None) {

  private val grammar = new Grammar(corpus.crypto)
  private val ops = NonEmptyVector.fromVectorUnsafe(Op.values.toVector)
  private val cryptoOps = NonEmptyVector.fromVectorUnsafe(Op.values.toVector.filter(Corpus.crypto.contains))
  private val root = Ctx(0, false, Vector.empty)

  private def printed(program: Sj, ascii: Boolean): G[FuzzCase] =
    (Grammar.world, State.get[GenState]).mapN { (w, s) =>
      FuzzCase(
        "",
        FuzzCase.Source.Surface(Sj.print(program, ascii)),
        Sj.print(Obj(w ++ s.data), ascii),
        s.env.map { case (k, v) => k -> Sj.print(v, ascii) }
      )
    }

  private val depth: G[Int] = weighted(1 -> pure(0), 3 -> pure(1), 3 -> pure(2), 1 -> pure(3))

  private val grammarCase: G[FuzzCase] =
    (pick(ops), depth, chance(0.25)).tupled.flatMap { case (op, d, ascii) => grammar.invoke(op, d, root).flatMap(printed(_, ascii)) }

  private val cryptoCase: G[FuzzCase] = pick(cryptoOps).flatMap(grammar.crypto).flatMap(printed(_, false))

  private val deepCase: G[FuzzCase] = grammar.deep.flatMap(printed(_, false))

  private val bulkCase: G[FuzzCase] = grammar.bulk.flatMap(printed(_, false)).map(_.copy(gasLimit = Some(1L << 28)))

  /**
   * The result bound (2^24) met by shared parts: a host binding of `n` copies of one text, weighing about 2^23, joined
   * with itself, so the result's weight, computed and never walked, is on either side of the bound by a few copies.
   */
  private val weightCase: G[FuzzCase] =
    (oneOf(1000, 4093, 255), between(-2, 2), weighted(3 -> pure("cat"), 2 -> pure("merge")), between(0, 3)).mapN { (l, d, op, shape) =>
      val n = ((1L << 23) - 1) / (1L + l) + d
      val join = Sj.call(op, obj("var", Str("$a")), obj("var", Str("$a")))
      val program = shape match {
        case 0 => Sj.call("length", join)
        case 1 => Sj.call("typeof", Sj.call("reverse", join))
        case 2 => Sj.call("count", Sj.call(op, obj("var", Str("$a")), obj("var", Str("$a")), Arr(Vector(Str("x")))))
        case _ => Sj.call("length", Sj.call("keys", Sj.call("set", lit(Obj(Vector.empty)), Str("k"), join)))
      }
      val leaf =
        xyz.kd5ujc.core.canonical.CanonicalValue.text("k" * l).flatMap(xyz.kd5ujc.core.canonical.Cbor.encode(_)).fold(_ => "f6", _.toHex)
      FuzzCase(
        "",
        FuzzCase.Source.Surface(Sj.print(program)),
        "null",
        built = Vector(
          "a" -> io.circe.Json.obj(
            "leaf"  -> io.circe.Json.fromString(leaf),
            "steps" -> io.circe.Json.arr(io.circe.Json.obj("copies" -> io.circe.Json.fromLong(n)))
          )
        ),
        gasLimit = Some(1L << 28)
      )
    }

  private def surface(c: FuzzCase): Option[Sj] = c.source match {
    case FuzzCase.Source.Surface(t) => io.circe.parser.parse(t).toOption.map(Sj.fromCirce)
    case FuzzCase.Source.Form(_)    => None
  }

  private val corpusCase: G[FuzzCase] =
    weighted(40 -> pick(NonEmptyVector.fromVectorUnsafe(corpus.light)), 1 -> pick(NonEmptyVector.fromVectorUnsafe(corpus.all))).flatMap {
      base =>
        val data = io.circe.parser.parse(base.data).toOption.map(Sj.fromCirce)
        for {
          program <- surface(base).fold(pure(base.source)) { p =>
            weighted(
              1 -> pure(base.source),
              6 -> between(1, 3).flatMap(n => Mutate.tree(p, n, grammar)).map(m => FuzzCase.Source.Surface(Sj.print(m)))
            )
          }
          newData <- data.fold(pure[Option[Sj]](None))(d =>
            weighted(3 -> pure(d), 2 -> Mutate.tree(d, 1, grammar), 1 -> Mutate.someHex(d)).map(Some(_))
          )
          extra <- State.get[GenState]
        } yield
          base.copy(
            source = program,
            // the values a mutation placed in the data join a map's members; other data is kept as it is
            data = newData.fold(base.data) {
              case Obj(kvs) => Sj.print(Obj(kvs ++ extra.data.filterNot(e => kvs.exists(_._1 == e._1))))
              case other    => Sj.print(other)
            },
            env = base.env ++ extra.env.filterNot(e => base.env.exists(_._1 == e._1)).map { case (k, v) => k -> Sj.print(v) }
          )
    }

  private val vectorCase: G[FuzzCase] = pick(NonEmptyVector.fromVectorUnsafe(corpus.light))

  /** Edits of the text itself, on code points, so a surrogate pair is never split. */
  private def textual(t: String): G[String] = {
    val cps = t.codePoints().toArray.toVector
    def at(f:   Int => String): G[String] = below(cps.size + 1).map(i => f(i))
    def str(xs: Vector[Int]): String = new String(xs.toArray, 0, xs.size)
    weighted(
      3 -> oneOf(" ", "\n", "\t", "\r\n", " ", "﻿", " ").flatMap(ws => at(i => str(cps.take(i)) + ws + str(cps.drop(i)))),
      2 -> at(i => str(cps.take(i))),
      2 -> at(i => str(cps.take(i)) + str(cps.drop(i + 1))),
      1 -> (below(cps.size + 1), below(8)).mapN((i, n) => str(cps.take(i)) + str(cps.slice(i, i + n)) + str(cps.drop(i))),
      1 -> pure("﻿" + t),
      1 -> oneOf("[" + t + "]", "{\"literal\": " + t + "}", t + " x", t + "\u0000", t + t, " " + t + " ")
    )
  }

  private val textCase: G[FuzzCase] =
    weighted(1 -> grammarCase, 1 -> vectorCase).flatMap { c =>
      c.source match {
        case FuzzCase.Source.Surface(t) => textual(t).map(x => c.copy(source = FuzzCase.Source.Surface(x)))
        case FuzzCase.Source.Form(_)    => pure(c)
      }
    }

  private val formCase: G[FuzzCase] =
    weighted(3 -> grammarCase, 2 -> vectorCase, 1 -> cryptoCase).flatMap { c =>
      c.source match {
        case FuzzCase.Source.Surface(t) =>
          Parser.parse(t).fold(_ => pure(c), p => Mutate.form(p).map(b => c.copy(source = FuzzCase.Source.Form(b))))
        case FuzzCase.Source.Form(_) => pure(c)
      }
    }

  // in parts per thousand; a bulk case builds about 2^24 bytes in every runner
  private val all: Vector[(Int, String, G[FuzzCase])] = Vector(
    (300, "grammar", grammarCase),
    (140, "crypto", cryptoCase),
    (275, "corpus", corpusCase),
    (50, "vector", vectorCase),
    (20, "deep", deepCase),
    (3, "bulk", bulkCase),
    (10, "weight", weightCase),
    (60, "text", textCase),
    (152, "form", formCase)
  )

  /** The sources' names, for `--source`. */
  val names: Vector[String] = all.map(_._2)

  private val sources: G[(String, FuzzCase)] = {
    val chosen = all.filter { case (_, n, _) => only.forall(_ == n) }.map { case (w, n, g) => w -> g.map(n -> _) }
    chosen.headOption.fold(grammarCase.map("grammar" -> _))(first => weighted(first, chosen.drop(1)*))
  }

  /** The gas limit: the case's own, a small one, one at the exact cost or below it, or one inside the cost. */
  private def gas(c: FuzzCase): G[(FuzzCase, Boolean, Option[Fault])] =
    weighted(
      14 -> pure((c, false, None)),
      2  -> between(0, 3000).map(l => (c.copy(gasLimit = Some(l.toLong)), false, None)),
      5 -> defer {
        reference(c).fold(
          f => pure((c, false, Some(f))),
          r => {
            val starved = r.outcome match {
              case Record.Outcome.Error("OutOfGas", _, _) => true
              case _                                      => false
            }
            if (starved || r.gasUsed == 0) pure((c, false, None))
            else
              weighted(
                3 -> pure((c.copy(gasLimit = Some(r.gasUsed)), true, None)),
                3 -> pure((c.copy(gasLimit = Some(r.gasUsed - 1)), true, None)),
                1 -> long.map(x => (c.copy(gasLimit = Some(java.lang.Long.remainderUnsigned(x, r.gasUsed))), false, None))
              )
          }
        )
      }
    )

  def apply(seed: Long, index: Long): Generated = {
    val (_, ((source, _), (cased, boundary, fault))) =
      sources.flatMap { case (s, c) => gas(c).map(g => (s, c) -> g) }
        .run(GenState(Rng.seedOf(seed, index), Vector.empty, Vector.empty))
        .value
    Generated(cased.copy(name = s"$source $seed/$index"), source, boundary, fault)
  }
}
