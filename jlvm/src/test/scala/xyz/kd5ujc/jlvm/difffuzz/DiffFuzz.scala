package xyz.kd5ujc.jlvm.difffuzz

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths, StandardOpenOption}

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all._

import scala.concurrent.duration._

import xyz.kd5ujc.jlvm.{Expr, Jlvm, Op, Parser}

import io.circe.Json
import io.circe.syntax._

/** The options of a run (tools/difffuzz/README.md). */
final case class Options(
  seed:         Option[Long] = None,
  cases:        Long = 10000,
  minutes:      Option[Double] = None,
  only:         Option[Long] = None,
  replay:       Option[Path] = None,
  vectors:      Boolean = false,
  batch:        Int = 200,
  rust:         Option[String] = Some("sdk/rust/target/release/dilf4s-difffuzz"),
  ts:           Option[String] = Some("sdk/ts/packages/jlvm/difffuzz/runner.ts"),
  node:         String = "node",
  tsWorkers:    Int = 2,
  rustWorkers:  Int = 1,
  out:          Path = Paths.get("target", "difffuzz"),
  shrinkBudget: Int = 3000,
  perSignature: Int = 2,
  dump:         Option[Path] = None,
  source:       Option[String] = None
)

object Options {

  private def num[A](name: String, v: String, f: String => Option[A]): Either[String, A] = f(v).toRight(s"$name: not a number: $v")

  /**
   * The file a run writes its findings to, emptied when the run starts: `findings.jsonl`, or for a replay `replayed-`
   * and the replayed file's name, which is never the file the replay reads, so replaying findings keeps them.
   */
  def findings(o: Options): Path =
    o.out.resolve(o.replay.fold("findings.jsonl")(r => s"replayed-${Option(r.getFileName).fold("cases")(_.toString)}"))

  @scala.annotation.tailrec
  def parse(args: List[String], o: Options = Options()): Either[String, Options] = args match {
    case Nil => Right(o)
    case "--seed" :: v :: rest =>
      num("--seed", v, _.toLongOption) match { case Right(s) => parse(rest, o.copy(seed = Some(s))); case Left(e) => Left(e) }
    case "--cases" :: v :: rest =>
      num("--cases", v, _.toLongOption) match { case Right(n) => parse(rest, o.copy(cases = n)); case Left(e) => Left(e) }
    case "--minutes" :: v :: rest =>
      num("--minutes", v, _.toDoubleOption) match { case Right(m) => parse(rest, o.copy(minutes = Some(m))); case Left(e) => Left(e) }
    case "--case" :: v :: rest =>
      num("--case", v, _.toLongOption) match { case Right(i) => parse(rest, o.copy(only = Some(i))); case Left(e) => Left(e) }
    case "--batch" :: v :: rest =>
      num("--batch", v, _.toIntOption) match { case Right(n) => parse(rest, o.copy(batch = n.max(1))); case Left(e) => Left(e) }
    case "--ts-workers" :: v :: rest =>
      num("--ts-workers", v, _.toIntOption) match { case Right(n) => parse(rest, o.copy(tsWorkers = n.max(1))); case Left(e) => Left(e) }
    case "--rust-workers" :: v :: rest =>
      num("--rust-workers", v, _.toIntOption) match {
        case Right(n) => parse(rest, o.copy(rustWorkers = n.max(1))); case Left(e) => Left(e)
      }
    case "--shrink-budget" :: v :: rest =>
      num("--shrink-budget", v, _.toIntOption) match { case Right(n) => parse(rest, o.copy(shrinkBudget = n)); case Left(e) => Left(e) }
    case "--per-signature" :: v :: rest =>
      num("--per-signature", v, _.toIntOption) match { case Right(n) => parse(rest, o.copy(perSignature = n)); case Left(e) => Left(e) }
    case "--replay" :: f :: rest    => parse(rest, o.copy(replay = Some(Paths.get(f))))
    case "--dump" :: f :: rest      => parse(rest, o.copy(dump = Some(Paths.get(f))))
    case "--source" :: s :: rest    => parse(rest, o.copy(source = Some(s)))
    case "--vectors" :: rest        => parse(rest, o.copy(vectors = true))
    case "--rust" :: "none" :: rest => parse(rest, o.copy(rust = None))
    case "--rust" :: f :: rest      => parse(rest, o.copy(rust = Some(f)))
    case "--ts" :: "none" :: rest   => parse(rest, o.copy(ts = None))
    case "--ts" :: f :: rest        => parse(rest, o.copy(ts = Some(f)))
    case "--node" :: f :: rest      => parse(rest, o.copy(node = f))
    case "--out" :: f :: rest       => parse(rest, o.copy(out = Paths.get(f)))
    case other :: _                 => Left(s"unknown option $other")
  }
}

/** Where a runner's record differs from the reference's: one of the record's fields, or the runner's own fault. */
enum Field {
  case Program, Outcome, Available, Gas, Warnings, Fault, Invariant
}

final case class Diff(lang: String, field: Field) {
  override def toString: String = s"$lang:${field.toString.toLowerCase(java.util.Locale.ROOT)}"
}

/**
 * Every runner's reply for one case: the reference's first. `late` when the run's deadline cut a runner off before it
 * answered: such a case is neither judged nor counted, and never keeps a signature while shrinking.
 */
final case class Replies(scala: Either[String, Record], others: Vector[(String, Either[String, Record])]) {
  def late: Boolean = others.exists(_._2 == Left(Child.Late))
}

/** A case's outcome at its root operation, for the coverage table. */
final case class OpStats(reached: Long = 0, values: Long = 0, falses: Long = 0, refused: Long = 0, otherErrors: Long = 0) {
  def +(o: OpStats): OpStats =
    OpStats(reached + o.reached, values + o.values, falses + o.falses, refused + o.refused, otherErrors + o.otherErrors)
}

final case class Finding(signature: Set[Diff], original: FuzzCase, shrunk: FuzzCase, replies: Replies, source: String)

final case class Stats(
  cases:    Long = 0,
  bySource: Map[String, Long] = Map.empty,
  byRoot:   Map[Op, OpStats] = Map.empty,
  bySite:   Map[String, Long] = Map.empty,
  byKind:   Map[String, Long] = Map.empty,
  forms:    Map[String, Long] = Map.empty,
  boundary: Long = 0,
  late:     Long = 0,
  crypto:   Map[(Op, String), Long] = Map.empty,
  findings: Vector[Finding] = Vector.empty
)

/**
 * The differential driver: `sbt "jlvm/Test/runMain xyz.kd5ujc.jlvm.difffuzz.DiffFuzz [options]"`, or
 * `tools/difffuzz/run.sh`. It generates cases from a seed (or replays a file, or the vectors), runs every case on the
 * reference in process and on the Rust and TypeScript runners in child processes, and compares each record exactly.
 * A mismatch is shrunk, keeping the same mismatch, and written to `findings.jsonl`; the run then exits non-zero.
 */
object DiffFuzz extends IOApp {

  private def say(s: String): IO[Unit] = IO.blocking(System.out.println(s))

  private def compare(lang: String, ref: Either[String, Record], other: Either[String, Record]): Set[Diff] =
    (ref, other) match {
      case (Left(_), _) => Set(Diff("scala", Field.Fault))
      case (_, Left(_)) => Set(Diff(lang, Field.Fault))
      case (Right(a), Right(b)) =>
        Set(
          Option.when(a.program != b.program)(Diff(lang, Field.Program)),
          Option.when(a.outcome != b.outcome)(Diff(lang, Field.Outcome)),
          Option.when(a.available != b.available)(Diff(lang, Field.Available)),
          Option.when(a.gasUsed != b.gasUsed)(Diff(lang, Field.Gas)),
          Option.when(a.warnings != b.warnings)(Diff(lang, Field.Warnings))
        ).flatten
    }

  /** Where each runner's record differs from the reference's. */
  def signature(r: Replies): Set[Diff] =
    r.others.flatMap { case (lang, o) => compare(lang, r.scala, o) }.toSet

  /** The runners: the reference in process, the others in child processes, each language's batch split among its workers. */
  final class Runners(jlvm: Jlvm, children: Vector[(String, Vector[Child])]) {

    def message(c: FuzzCase): Option[String] = Evaluate.message(jlvm, c)

    def invariant(c: FuzzCase, ref: Either[String, Record]): Set[Diff] =
      if (ref.forall(Evaluate.metered(reference, c, _))) Set.empty else Set(Diff("scala", Field.Invariant))

    /** Every difference: between the runners, and of the reference from its own rules. */
    def diffs(c: FuzzCase, r: Replies): Set[Diff] = signature(r) ++ invariant(c, r.scala)

    /** The reference's record, or its fault: a case it cannot run, or an exception, which would be a bug of its own. */
    def attempt(c: FuzzCase): Either[String, Record] =
      Either.catchNonFatal(Evaluate.record(jlvm, c)).leftMap(e => s"the reference threw ${e.getClass.getName}").flatten

    /** [[attempt]], a fault as a record that matches no runner's. */
    def reference(c: FuzzCase): Record =
      attempt(c).fold(e => Record(c.name, None, Record.Outcome.Error(s"fault: $e", None, None), None, 0L, Vector.empty), identity)

    private def parsed(c: FuzzCase, reply: Either[String, String]): Either[String, Record] =
      reply.flatMap(Record.fromLine).flatMap(r => Either.cond(r.name == c.name, r, s"a record for ${r.name}, not ${c.name}"))

    private def split[A](xs: Vector[A], n: Int): Vector[Vector[A]] = {
      val size = (xs.size + n - 1) / n.max(1)
      if (size == 0) Vector.empty else xs.grouped(size).toVector
    }

    def batch(cases: Vector[FuzzCase], deadline: FiniteDuration = Child.Never): IO[Vector[Replies]] = {
      val lines = cases.map(FuzzCase.transport)
      val ref = cases.traverse(c => IO(attempt(c)))
      val others = children.parTraverse {
        case (lang, workers) =>
          split(lines, workers.size)
            .zip(workers)
            .parTraverse { case (chunk, w) => w.run(chunk, deadline = deadline) }
            .map(rs => lang -> rs.flatten)
      }
      (ref, others).parMapN { (rs, os) =>
        cases.indices.toVector.map(i =>
          Replies(rs(i), os.map { case (lang, xs) => lang -> parsed(cases(i), xs.lift(i).toRight("no reply").flatten) })
        )
      }
    }
  }

  private def runners(o: Options, jlvm: Jlvm): Resource[IO, Runners] = {
    val cwd = Paths.get("").toAbsolutePath
    def log(name: String) = o.out.resolve(s"$name.stderr").toFile
    val rust =
      o.rust.toVector.map(bin => ("rust", (0 until o.rustWorkers).toVector.map(i => Child(s"rust$i", List(bin), cwd, log("rust")))))
    val ts = o.ts.toVector.map(script =>
      (
        "ts",
        (0 until o.tsWorkers).toVector.map(i =>
          Child(s"ts$i", List(o.node, "--experimental-strip-types", "--disable-warning=ExperimentalWarning", script), cwd, log("ts"))
        )
      )
    )
    (rust ++ ts).traverse { case (lang, ws) => ws.sequence.map(lang -> _) }.map(new Runners(jlvm, _))
  }

  // ─── coverage ───

  private def root(c: FuzzCase): Option[Op] =
    (c.source match {
      case FuzzCase.Source.Surface(t) => Parser.parse(t)
      case FuzzCase.Source.Form(b)    => Parser.decode(b)
    }).toOption.flatMap(_.expr match {
      case Expr.Apply(op, _)  => Some(op)
      case Expr.Spread(op, _) => Some(op)
      case _                  => None
    })

  private def count(s: Stats, g: Generated, r: Either[String, Record]): Stats = {
    val rec = r.toOption
    val kind = rec.map(_.outcome).fold("fault") {
      case Record.Outcome.Value(_)       => "value"
      case Record.Outcome.Error(k, _, _) => k
    }
    val site = rec.map(_.outcome).collect { case Record.Outcome.Error(_, Some(op), _) => op }
    val atRoot = root(g.c).map { op =>
      op -> rec.map(_.outcome).fold(OpStats(reached = 1, otherErrors = 1)) {
        case Record.Outcome.Value("f4")                            => OpStats(reached = 1, falses = 1)
        case Record.Outcome.Value(_)                               => OpStats(reached = 1, values = 1)
        case Record.Outcome.Error(_, Some(at), _) if at == op.wire => OpStats(reached = 1, refused = 1)
        case Record.Outcome.Error(_, _, _)                         => OpStats(reached = 1, otherErrors = 1)
      }
    }
    val form = g.c.source match {
      case FuzzCase.Source.Form(_) => Some(rec.fold("fault")(x => if (x.program.isDefined) "decoded" else s"refused ${kind}"))
      case _                       => None
    }
    // a cryptographic operation at the root: true, false, another value, a Crypto refusal with or without a cause
    val crypto = atRoot.map(_._1).filter(Corpus.crypto.contains).map { op =>
      op -> rec.map(_.outcome).fold("fault") {
        case Record.Outcome.Value("f5")                 => "true"
        case Record.Outcome.Value("f4")                 => "false"
        case Record.Outcome.Value(_)                    => "value"
        case Record.Outcome.Error("Crypto", _, Some(_)) => "cause"
        case Record.Outcome.Error("Crypto", _, None)    => "crypto"
        case Record.Outcome.Error(other, _, _)          => other
      }
    }
    s.copy(
      cases = s.cases + 1,
      bySource = s.bySource.updatedWith(g.source)(n => Some(n.getOrElse(0L) + 1)),
      byRoot = atRoot.fold(s.byRoot) { case (op, st) => s.byRoot.updatedWith(op)(x => Some(x.fold(st)(_ + st))) },
      bySite = site.fold(s.bySite)(x => s.bySite.updatedWith(x)(n => Some(n.getOrElse(0L) + 1))),
      byKind = s.byKind.updatedWith(kind)(n => Some(n.getOrElse(0L) + 1)),
      forms = form.fold(s.forms)(f => s.forms.updatedWith(f)(n => Some(n.getOrElse(0L) + 1))),
      boundary = s.boundary + (if (g.boundary) 1L else 0L),
      crypto = crypto.fold(s.crypto)(k => s.crypto.updatedWith(k)(n => Some(n.getOrElse(0L) + 1)))
    )
  }

  def table(s: Stats): String = {
    val header = f"${"operation"}%-26s ${"root"}%9s ${"value"}%9s ${"false"}%9s ${"refused"}%9s ${"other"}%9s ${"refused anywhere"}%17s"
    val rows = Op.values.toVector.map { op =>
      val st = s.byRoot.getOrElse(op, OpStats())
      f"${op.wire}%-26s ${st.reached}%9d ${st.values}%9d ${st.falses}%9d ${st.refused}%9d ${st.otherErrors}%9d ${s.bySite.getOrElse(op.wire, 0L)}%17d"
    }
    val unreached = Op.values.toVector.filter(op => s.byRoot.get(op).forall(_.reached == 0)).map(_.wire)
    (List(
      s"cases: ${s.cases}; by source: ${s.bySource.toList.sorted.map { case (k, n) => s"$k $n" }.mkString(", ")}",
      s"at the exact cost or one below it: ${s.boundary}",
      s"cut off by the deadline, neither judged nor counted: ${s.late}",
      s"outcomes: ${s.byKind.toList.sorted.map { case (k, n) => s"$k $n" }.mkString(", ")}",
      s"canonical forms: ${s.forms.toList.sorted.map { case (k, n) => s"$k $n" }.mkString(", ")}",
      s"var refusals: ${s.bySite.getOrElse("var", 0L)}",
      s"operations never at the root: ${if (unreached.isEmpty) "none" else unreached.mkString(", ")}",
      header
    ) ++ rows ++ List(
      "",
      "cryptographic operations at the root: true, false, another value, Crypto with a cause, Crypto without one, other",
      f"${"operation"}%-26s ${"true"}%9s ${"false"}%9s ${"value"}%9s ${"cause"}%9s ${"crypto"}%9s ${"other"}%9s"
    ) ++ Op.values.toVector.filter(Corpus.crypto.contains).map { op =>
      val n = (k: String) => s.crypto.getOrElse(op -> k, 0L)
      val other = s.crypto.collect { case ((o, k), v) if o == op && !Set("true", "false", "value", "cause", "crypto").contains(k) => v }.sum
      f"${op.wire}%-26s ${n("true")}%9d ${n("false")}%9d ${n("value")}%9d ${n("cause")}%9d ${n("crypto")}%9d ${other}%9d"
    }).mkString("\n")
  }

  // ─── shrinking ───

  /**
   * Shrinks `c`, keeping its signature, within `budget` candidates in all and before `deadline` (the run's `--minutes`):
   * each round tries candidates in groups of 40 and moves to the first that keeps the signature.
   */
  private def shrink(runners: Runners, budget: Int, deadline: FiniteDuration, c: FuzzCase, sig: Set[Diff]): IO[(FuzzCase, Replies)] = {
    def keeps(cs: Vector[FuzzCase]): IO[Option[(FuzzCase, Replies)]] =
      runners.batch(cs, deadline).map(rs => cs.zip(rs).find { case (x, r) => !r.late && runners.diffs(x, r) == sig })
    val reprinted = c.source match {
      case FuzzCase.Source.Surface(t) =>
        io.circe.parser.parse(t).toOption.map(j => c.copy(source = FuzzCase.Source.Surface(Sj.print(Sj.fromCirce(j)))))
      case FuzzCase.Source.Form(_) => None
    }
    // a round's progress: the candidates it has run, the first that kept the signature, whether the deadline passed
    final case class Round(used: Int, found: Option[FuzzCase], late: Boolean)
    def step(cur: FuzzCase, asTree: Boolean, left: Int): IO[FuzzCase] = {
      val m = Shrink.measure(cur)
      val cands = Shrink.candidates(cur, asTree).filter(x => Shrink.measure(x) < m).take(left.max(0).min(400)).toVector.zipWithIndex.map {
        case (x, i) => x.copy(name = s"${c.name} shrink $left.$i")
      }
      cands
        .grouped(40)
        .toVector
        .foldLeftM(Round(0, None, false)) {
          case (Round(used, None, false), group) =>
            IO.monotonic.flatMap { now =>
              if (now >= deadline) IO.pure(Round(used, None, true))
              else keeps(group).map(found => Round(used + group.size, found.map(_._1), false))
            }
          case (done, _) => IO.pure(done)
        }
        .flatMap {
          case Round(used, Some(next), _) => step(next.copy(name = c.name), asTree, left - used)
          case _                          => IO.pure(cur)
        }
    }
    for {
      asTree <- reprinted.fold(IO.pure(false))(r => keeps(Vector(r)).map(_.isDefined))
      start = reprinted.filter(_ => asTree).getOrElse(c)
      small   <- step(start, asTree, budget)
      replies <- runners.batch(Vector(small), deadline).map(_.headOption)
    } yield small -> replies.getOrElse(Replies(Left("lost"), Vector.empty))
  }

  // ─── output ───

  private def recordJson(r: Either[String, Record]): Json = r.fold(e => Json.obj("fault" -> e.asJson), Record.json)

  /** A finding's line: `stage` is `found` for the case as generated (written before shrinking starts), then `shrunk`. */
  private def findingJson(f: Finding, stage: String): Json =
    Json.obj(
      "stage"     -> stage.asJson,
      "signature" -> f.signature.toList.map(_.toString).sorted.asJson,
      "source"    -> f.source.asJson,
      "shrunk"    -> FuzzCase.row(f.shrunk),
      "records" -> Json.fromFields(
        ("scala" -> recordJson(f.replies.scala)) :: f.replies.others.toList.map { case (l, r) => l -> recordJson(r) }
      ),
      "original" -> FuzzCase.row(f.original)
    )

  private def append(path: Path, line: String): IO[Unit] =
    IO.blocking {
      Files.write(path, (line + "\n").getBytes(UTF_8), StandardOpenOption.CREATE, StandardOpenOption.APPEND)
      ()
    }

  // ─── a batch, its findings, and the run ───

  private def judge(
    o:        Options,
    runners:  Runners,
    deadline: FiniteDuration,
    gs:       Vector[Generated],
    rs:       Vector[Replies],
    stats:    Stats
  ): IO[Stats] = {
    // a case the deadline cut off is neither counted nor judged
    val counted = gs.zip(rs).foldLeft(stats) { case (s, (g, r)) => if (r.late) s.copy(late = s.late + 1) else count(s, g, r.scala) }
    val dumped = o.dump.fold(IO.unit) { path =>
      append(
        path,
        gs.zip(rs)
          .map {
            case (g, r) =>
              Json
                .obj(
                  "case"    -> FuzzCase.row(g.c),
                  "source"  -> g.source.asJson,
                  "scala"   -> recordJson(r.scala),
                  "message" -> runners.message(g.c).asJson
                )
                .noSpaces
          }
          .mkString("\n")
      )
    }
    dumped *> gs.zip(rs).foldLeftM(counted) {
      case (s, (g, r)) =>
        val sig = if (r.late) Set.empty[Diff] else runners.diffs(g.c, r)
        if (sig.isEmpty) IO.pure(s)
        else {
          val seen = s.findings.count(_.signature == sig)
          val found = Finding(sig, g.c, g.c, r, g.source)
          val label = s"MISMATCH ${sig.toList.map(_.toString).sorted.mkString(" ")}: ${g.c.name}"
          // the case as generated is saved first, so a run cut short while shrinking still keeps it
          val saved = append(Options.findings(o), findingJson(found, "found").noSpaces)
          val shrunk =
            if (seen >= o.perSignature || o.shrinkBudget <= 0) say(label).as(found)
            else
              shrink(runners, o.shrinkBudget, deadline, g.c, sig).flatMap {
                case (small, replies) =>
                  val f = Finding(sig, g.c, small, replies, g.source)
                  say(s"$label\n  shrunk: ${FuzzCase.row(small).noSpaces.take(2000)}") *>
                  append(Options.findings(o), findingJson(f, "shrunk").noSpaces).as(f)
              }
          saved *> shrunk.map(f => s.copy(findings = s.findings :+ f))
        }
    }
  }

  /** When `--minutes` runs out, counted from `started`; with no `--minutes`, never. */
  private def deadline(o: Options, started: FiniteDuration): FiniteDuration =
    o.minutes.fold(Child.Never)(m => started + (m * 60000).toLong.millis)

  private def campaign(o: Options, runners: Runners, gen: Generator, seed: Long): IO[Stats] = {
    val total = o.only.fold(o.cases)(_ => 1L)
    def loop(next: Long, stats: Stats, started: FiniteDuration): IO[Stats] =
      IO.monotonic.flatMap { now =>
        val late = now >= deadline(o, started)
        if (next >= total || late) IO.pure(stats)
        else {
          val idx = o.only.fold((next until (next + o.batch).min(total)).toVector)(i => Vector(i))
          idx.traverse(i => IO(gen(seed, i))).flatMap { gs =>
            runners
              .batch(gs.map(_.c), deadline(o, started))
              .flatTap { rs =>
                // one case alone (`--case`): the case and every runner's record
                o.only.fold(IO.unit) { _ =>
                  gs.zip(rs).traverse_ {
                    case (g, r) =>
                      say(FuzzCase.row(g.c).spaces2) *>
                      ((("scala" -> r.scala) +: r.others).traverse_ { case (lang, x) => say(s"$lang: ${recordJson(x).noSpaces}") })
                  }
                }
              }
              .flatMap(rs => judge(o, runners, deadline(o, started), gs, rs, stats))
              .flatTap { s =>
                val step = o.batch * 25L
                say(s"${s.cases} cases, ${s.findings.size} mismatches, ${(now - started).toSeconds}s")
                  .whenA((next + idx.size) / step != next / step)
              }
              .flatMap(loop(next + idx.size, _, started))
          }
        }
      }
    IO.monotonic.flatMap(loop(0, Stats(), _))
  }

  /** The `jlvm_v1.json` cases, each record checked against its row as well as the reference's against the others'. */
  private def vectors(runners: Runners): IO[Stats] =
    IO.blocking(new String(Files.readAllBytes(xyz.kd5ujc.jlvm.JlvmVectors.path), UTF_8))
      .flatMap(io.circe.parser.parse(_).liftTo[IO])
      .flatMap { file =>
        val rows = file.hcursor.downField("cases").values.fold(Vector.empty[Json])(_.toVector)
        val expected: Map[String, (Json, Long, Long)] = rows.flatMap { r =>
          val c = r.hcursor
          (c.get[String]("name"), c.get[Json]("outcome"), c.get[Long]("gasUsed"), c.get[Long]("warnings")).tupled.toOption.map {
            case (n, out, g, w) => n -> (out.mapObject(_.remove("json")), g, w)
          }
        }.toMap
        Corpus.cases.flatMap { cases =>
          cases.grouped(200).toVector.foldLeftM(Stats()) { (stats, group) =>
            runners.batch(group).flatMap { rs =>
              group.zip(rs).foldLeftM(stats) {
                case (s, (c, r)) =>
                  val against = (r.others :+ ("scala" -> r.scala)).flatMap {
                    case (lang, rec) =>
                      val ok = rec.toOption.zip(expected.get(c.name)).exists {
                        case (x, (out, g, w)) => Record.outcomeJson(x.outcome) == out && x.gasUsed == g && x.warnings.size.toLong == w
                      }
                      Option.unless(ok)(Diff(lang, Field.Outcome))
                  }.toSet
                  val sig = runners.diffs(c, r) ++ against
                  val counted = count(s, Generated(c, "vector", false), r.scala)
                  if (sig.isEmpty) IO.pure(counted)
                  else
                    say(s"MISMATCH ${sig.mkString(" ")}: ${c.name}") *>
                    IO.pure(counted.copy(findings = counted.findings :+ Finding(sig, c, c, r, "vector")))
              }
            }
          }
        }
      }

  private def replay(o: Options, runners: Runners, path: Path): IO[Stats] =
    IO.blocking(new String(Files.readAllBytes(path), UTF_8)).flatMap { text =>
      val rows = io.circe.parser
        .parse(text)
        .toOption
        .flatMap(_.hcursor.downField("cases").values)
        .fold {
          text.linesIterator.filter(_.trim.nonEmpty).toVector.traverse(l => io.circe.parser.parse(l).leftMap(_.message))
        }(xs => Right(xs.toVector))
      rows
        .flatMap(_.traverse(j => j.hcursor.downField("shrunk").success.fold(FuzzCase.fromRow(j.hcursor))(FuzzCase.fromRow)))
        .leftMap(new IllegalArgumentException(_))
        .liftTo[IO]
        .flatMap { cases =>
          val gs = cases.map(Generated(_, "replay", false))
          IO.monotonic.flatMap { started =>
            gs.grouped(o.batch)
              .toVector
              .foldLeftM(Stats())((s, g) =>
                runners.batch(g.map(_.c), deadline(o, started)).flatMap(judge(o, runners, deadline(o, started), g, _, s))
              )
          }
        }
    }

  def run(args: List[String]): IO[ExitCode] =
    Options
      .parse(args)
      .fold(
        e => IO.blocking(System.err.println(s"difffuzz: $e")).as(ExitCode(2)),
        o =>
          for {
            seed <- o.seed.fold(IO.realTime.map(_.toMillis))(IO.pure)
            _    <- IO.blocking(Files.createDirectories(o.out))
            _    <- IO.blocking(Files.deleteIfExists(Options.findings(o)))
            _ <- say(
              s"difffuzz: seed $seed (reproduce: --seed $seed), runners: scala${o.rust.fold("")(_ => " rust")}${o.ts.fold("")(_ => " ts")}"
            )
            jlvm <- Jlvm.load[IO]
            stats <- runners(o, jlvm).use { rs =>
              (o.vectors, o.replay) match {
                case (true, _)    => vectors(rs)
                case (_, Some(p)) => replay(o, rs, p)
                case (false, None) =>
                  Corpus.load.flatMap { corpus =>
                    val gen = new Generator(corpus, rs.reference, o.source)
                    o.source.filterNot(gen.names.contains).fold(campaign(o, rs, gen, seed)) { s =>
                      IO.raiseError(new IllegalArgumentException(s"--source $s: one of ${gen.names.mkString(", ")}"))
                    }
                  }
              }
            }
            summary = table(stats) + s"\nmismatches: ${stats.findings.size}" +
              stats.findings
                .groupBy(_.signature)
                .toList
                .map { case (k, fs) => s"\n  ${k.toList.map(_.toString).sorted.mkString(" ")}: ${fs.size}" }
                .mkString
            _ <- say(summary)
            _ <- IO.blocking(Files.write(o.out.resolve("summary.txt"), (s"seed $seed\n" + summary + "\n").getBytes(UTF_8)))
          } yield if (stats.findings.isEmpty) ExitCode.Success else ExitCode.Error
      )
}
