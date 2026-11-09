package xyz.kd5ujc.jlvm.difffuzz

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.concurrent.duration._

import xyz.kd5ujc.jlvm.{Jlvm, JlvmVectors, Op}

import io.circe.Json
import weaver.IOSuite

/**
 * The differential harness's own soundness (tools/difffuzz/README.md): the reference's records are the vectors' rows, a
 * seed gives the same cases every time, cases and records cross the transport unchanged and a record with anything more
 * or less is refused, every operation is generated at the root, and the reference keeps its metering rules at the
 * limits the generator places.
 */
object DiffFuzzSuite extends IOSuite {

  final case class Res(jlvm: Jlvm, corpus: Corpus, rows: Map[String, Json]) {
    def reference(c: FuzzCase): Record =
      Evaluate
        .record(jlvm, c)
        .fold(e => Record(c.name, None, Record.Outcome.Error(s"fault: $e", None, None), None, 0L, Vector.empty), identity)
    def generator: Generator = new Generator(corpus, reference)
  }

  private val rows: IO[Map[String, Json]] =
    IO.blocking(new String(Files.readAllBytes(JlvmVectors.path), UTF_8)).flatMap(io.circe.parser.parse(_).liftTo[IO]).map { j =>
      j.hcursor
        .downField("cases")
        .values
        .fold(Map.empty[String, Json])(_.toList.flatMap(r => r.hcursor.get[String]("name").toOption.map(_ -> r)).toMap)
    }

  def sharedResource: Resource[IO, Res] = Resource.eval((Jlvm.load[IO], Corpus.load, rows).mapN(Res.apply))

  private val seed = 20261109L

  test("the reference's record of every jlvm_v1.json case is the case's row: its outcome, gas and warnings") { res =>
    IO {
      val cases = res.corpus.all
      cases.foldLeft(expect.same(res.rows.size, cases.size)) { (acc, c) =>
        val row = res.rows.get(c.name).map(_.hcursor)
        val record = Evaluate.record(res.jlvm, c)
        val outcome = row.flatMap(_.get[Json]("outcome").toOption).map(_.mapObject(_.remove("json")))
        acc
          .and(expect(record.map(r => Record.outcomeJson(r.outcome)).toOption == outcome, s"${c.name}: outcome"))
          .and(expect(record.map(_.gasUsed).toOption == row.flatMap(_.get[Long]("gasUsed").toOption), s"${c.name}: gas"))
          .and(expect(record.map(_.warnings.size.toLong).toOption == row.flatMap(_.get[Long]("warnings").toOption), s"${c.name}: warnings"))
      }
    }
  }

  test("a seed generates the same cases every time, and each case alone") { res =>
    IO {
      val (a, b) = (res.generator, res.generator)
      val first = (0L until 300L).map(a(seed, _))
      val again = (0L until 300L).map(b(seed, _))
      val alone = List(0L, 17L, 299L).map(i => b(seed, i))
      expect(first == again, "two generators agree")
        .and(expect.same(List(first(0), first(17), first(299)), alone))
        .and(expect(first.map(_.c).distinct.size > 250, s"the cases differ: ${first.map(_.c).distinct.size} distinct"))
    }
  }

  test("every generated case crosses the transport unchanged, as well-formed UTF-8") { res =>
    IO {
      val gen = res.generator
      (0L until 600L).map(gen(seed + 1, _).c).foldLeft(success) { (acc, c) =>
        val texts = (c.source match {
          case FuzzCase.Source.Surface(t) => List(t)
          case FuzzCase.Source.Form(_)    => Nil
        }) ++ List(c.data) ++ c.env.flatMap { case (k, v) => List(k, v) }
        val wellFormed = texts.forall(t => new String(t.getBytes(UTF_8), UTF_8) == t)
        acc
          .and(expect.same(Right(c), FuzzCase.fromTransport(FuzzCase.transport(c))))
          .and(expect(wellFormed, s"${c.name}: a lone surrogate in the text"))
      }
    }
  }

  test("a record reads back as itself, and a record with a key more or less, or a cause of two families, is refused") { res =>
    IO {
      val gen = res.generator
      val records = (0L until 300L).map(i => res.reference(gen(seed + 2, i).c))
      val withExtra = (r: Record) => Record.json(r).mapObject(_.add("json", Json.fromString("1"))).noSpaces
      val withoutGas = (r: Record) => Record.json(r).mapObject(_.remove("gasUsed")).noSpaces
      val twoFamilies = """{"name":"x","outcome":{"error":"Crypto","op":"sha256","cause":{"zk":"A","vrf":"B"}},"gasUsed":1,"warnings":[]}"""
      val extraInOutcome = """{"name":"x","outcome":{"cbor":"01","json":"1"},"gasUsed":1,"warnings":[]}"""
      records
        .foldLeft(success) { (acc, r) =>
          acc
            .and(expect.same(Right(r), Record.fromLine(Record.json(r).noSpaces)))
            .and(expect(Record.fromLine(withExtra(r)).isLeft, "an extra key"))
            .and(expect(Record.fromLine(withoutGas(r)).isLeft, "a missing key"))
        }
        .and(expect(Record.fromLine(twoFamilies).isLeft, "two families"))
        .and(expect(Record.fromLine(extraInOutcome).isLeft, "an extra key in the outcome"))
    }
  }

  pureTest("a replay never writes its findings over the file it replays, wherever that file is") {
    val outs = List("target/difffuzz", ".", "target/difffuzz/x")
    val inputs = List(
      "target/difffuzz/findings.jsonl",
      "findings.jsonl",
      "target/difffuzz/replayed-findings.jsonl",
      "./target/difffuzz/findings.jsonl"
    )
    val pairs = for {
      out <- outs
      in  <- inputs
    } yield (out, in)
    pairs.foldLeft(success) {
      case (acc, (out, in)) =>
        Options
          .parse(List("--replay", in, "--out", out))
          .fold(
            e => acc.and(failure(e)),
            o => {
              val written = Options.findings(o).toAbsolutePath.normalize
              acc.and(expect(written != java.nio.file.Paths.get(in).toAbsolutePath.normalize, s"--replay $in --out $out writes $written"))
            }
          )
    }
  }

  test("a runner that hangs on every line is cut off at the deadline: each line is late, none is blamed, none waits") { _ =>
    val log = Files.createTempFile("difffuzz-hang", ".stderr")
    // `; :` keeps the shell alive with sleep as its child, in every shell: killing the shell alone would not end the read
    Child("hang", List("sh", "-c", "sleep 600; :"), java.nio.file.Paths.get("").toAbsolutePath, log.toFile).use { child =>
      IO.monotonic.flatMap { start =>
        child
          .run(Vector("a", "b", "c"), stall = 60.seconds, deadline = start + 2.seconds)
          .flatMap(replies => IO.monotonic.map(end => (replies, end - start)))
      }
    }
      .guarantee(IO.blocking(Files.deleteIfExists(log)).void)
      .map {
        case (replies, took) =>
          expect
            .same(Vector.fill(3)(Left(Child.Late)), replies)
            .and(expect(took < 30.seconds, s"took $took: a stall of 60 s per line would have taken 180 s"))
      }
  }

  test("a runner that answers without reading its input, then hangs, is still cut off at the deadline") { _ =>
    val log = Files.createTempFile("difffuzz-deaf", ".stderr")
    // three lines of 200 KB: past any pipe's capacity, so the writer blocks while the replies are already read
    val lines = Vector.fill(3)("x" * 200000)
    Child("deaf", List("sh", "-c", "echo a; echo b; echo c; sleep 600; :"), java.nio.file.Paths.get("").toAbsolutePath, log.toFile).use {
      child =>
        IO.monotonic.flatMap { start =>
          child
            .run(lines, stall = 60.seconds, deadline = start + 2.seconds)
            .flatMap(replies => IO.monotonic.map(end => (replies, end - start)))
        }
    }
      .guarantee(IO.blocking(Files.deleteIfExists(log)).void)
      .map {
        case (replies, took) =>
          expect
            .same(Vector("a", "b", "c").map(Right(_)), replies)
            .and(expect(took < 30.seconds, s"took $took: the blocked writer outlived the deadline"))
      }
  }

  test("every operation is the root of a generated case") { res =>
    IO {
      val gen = res.generator
      val roots = (0L until 4000L).flatMap { i =>
        gen(seed + 3, i).c.source match {
          case FuzzCase.Source.Surface(t) => xyz.kd5ujc.jlvm.Parser.parse(t).toOption.map(_.expr)
          case FuzzCase.Source.Form(b)    => xyz.kd5ujc.jlvm.Parser.decode(b).toOption.map(_.expr)
        }
      }.collect {
        case xyz.kd5ujc.jlvm.Expr.Apply(op, _)  => op
        case xyz.kd5ujc.jlvm.Expr.Spread(op, _) => op
      }.toSet
      expect.same(Set.empty[Op], Op.values.toSet -- roots)
    }
  }

  test("the reference keeps its metering rules at the limits the generator places: the exact cost, one below, inside") { res =>
    IO {
      val gen = res.generator
      val limited = (0L until 3000L).map(gen(seed + 4, _)).filter(_.c.gasLimit.isDefined)
      limited.foldLeft(expect(limited.count(_.boundary) > 200, s"${limited.count(_.boundary)} cases at their exact cost or one below")) {
        (acc, g) =>
          acc.and(expect(Evaluate.metered(res.reference, g.c, res.reference(g.c)), s"${g.c.name}: ${FuzzCase.row(g.c).noSpaces.take(300)}"))
      }
    }
  }
}
