package xyz.kd5ujc.jlvm.difffuzz

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.concurrent.duration._

import xyz.kd5ujc.core.canonical.{BuiltValues, CanonicalJson, DecodeFailure, DecodeReason, Preview}
import xyz.kd5ujc.core.circe.CanonicalCirce
import xyz.kd5ujc.jlvm.{Jlvm, JlvmErrorKind, JlvmVectors, Op}
import xyz.kd5ujc.models.crypto_errors.CryptoErrorKind
import xyz.kd5ujc.models.jlvm_vectors.JlvmSigningCause
import xyz.kd5ujc.models.{jlvm_vectors => vectorModel}

import io.circe.Json
import scodec.bits.ByteVector
import weaver.IOSuite

/**
 * The differential harness's own soundness (tools/difffuzz/README.md): the reference's records are the vectors' rows, a
 * seed gives the same cases every time, cases and records cross the transport unchanged and a record with anything more
 * or less is refused, every operation is generated at the root, and the reference keeps its metering rules at the
 * limits the generator places.
 */
object DiffFuzzSuite extends IOSuite {

  final case class Res(jlvm: Jlvm, corpus: Corpus, rows: Map[String, (Json, BigInt, Vector[vectorModel.JlvmWarning])]) {
    def attempt(c:   FuzzCase): Either[Fault, Record] = Evaluate.record(jlvm, c).leftMap(Fault.Evaluation(_))
    def reference(c: FuzzCase): Record = attempt(c).fold(e => throw Fault.Failure(e), identity)
    def generator: Generator = new Generator(corpus, attempt)
  }

  private val rows: IO[Map[String, (Json, BigInt, Vector[vectorModel.JlvmWarning])]] =
    IO.blocking(new String(Files.readAllBytes(JlvmVectors.path), UTF_8))
      .flatMap(text =>
        CanonicalJson
          .parse(text)
          .leftMap(WireError.Canonical(_))
          .flatMap(vectorModel.JlvmVectors.codec.decoder.decode(_).leftMap(WireError.Decode(_)))
          .leftMap(WireError.Failure(_))
          .liftTo[IO]
      )
      .flatMap(_.cases.traverse { row =>
        vectorModel.JlvmOutcome.codec
          .encode(row.outcome)
          .flatMap(CanonicalCirce.toCirce)
          .leftMap(e => WireError.Failure(WireError.Canonical(e)))
          .liftTo[IO]
          .map(out => row.name -> (out.mapObject(_.remove("json")), row.gasUsed, row.warnings))
      }.map(_.toMap))

  def sharedResource: Resource[IO, Res] = Resource.eval((Jlvm.load[IO], Corpus.load, rows).mapN(Res.apply))

  private val seed = 20261109L

  pureTest("a fault message cannot masquerade as the deadline") {
    val fault = Fault.ReferenceThrew(new IllegalStateException("cut off by the run's deadline"))
    val failed = Replies(Left(fault), Vector("rust" -> Left(fault)))
    val late = Replies(Left(fault), Vector("rust" -> Left(Fault.Transport(Child.Error.Deadline))))
    expect(!failed.late, "a reference exception remains a fault")
      .and(expect(late.late, "the typed deadline marks an unanswered case"))
      .and(expect.same(Set(Diff("scala", Field.Fault)), DiffFuzz.signature(failed)))
  }

  pureTest("transport source ambiguity retains which source fields were supplied") {
    val both = """{"name":"x","program":"31","form":"01","data":"6e756c6c"}"""
    val neither = """{"name":"x","data":"6e756c6c"}"""
    expect
      .same(Left(WireError.SourceChoice("x", true, true)), FuzzCase.fromTransport(both))
      .and(expect.same(Left(WireError.SourceChoice("x", false, false)), FuzzCase.fromTransport(neither)))
  }

  pureTest("wire outcomes preserve typed errors and refuse unknown kinds and sites") {
    def row(outcome: String) = s"""{"name":"x","outcome":$outcome,"gasUsed":1,"warnings":[]}"""
    val typed = Record(
      "x",
      None,
      Record.Outcome.Error(JlvmErrorKind.Crypto, Some(Op.Sha256), Some(JlvmSigningCause(CryptoErrorKind.InvalidEncoding))),
      None,
      1L,
      Vector.empty
    )
    expect
      .same(Right(typed), Record.fromLine(row("""{"error":"Crypto","op":"sha256","cause":{"signing":"InvalidEncoding"}}""")))
      .and(expect.same(Left(WireError.UnknownErrorKind(Preview.of("unknown"))), Record.fromLine(row("""{"error":"unknown"}"""))))
      .and(expect.same(Left(WireError.UnknownSite(Preview.of("unknown"))), Record.fromLine(row("""{"error":"Crypto","op":"unknown"}"""))))
  }

  pureTest("typed outcome bytes cannot normalize away a wire discrepancy") {
    def row(hex: String) = s"""{"name":"x","outcome":{"cbor":"$hex"},"gasUsed":1,"warnings":[]}"""
    expect
      .same(Left(WireError.InvalidHex(Preview.of("zz"))), Record.fromLine(row("zz")))
      .and(expect.same(Left(WireError.NonCanonicalHex(Preview.of("F4"))), Record.fromLine(row("F4"))))
      .and(
        expect.same(Right(Record("x", None, Record.Outcome.Value(ByteVector(0xf4)), None, 1L, Vector.empty)), Record.fromLine(row("f4")))
      )
  }

  test("a malformed built binding remains a runner fault with its exact cause") { res =>
    IO {
      val c = FuzzCase(
        "invalid recipe",
        FuzzCase.Source.Surface("1"),
        "null",
        built = Vector("a" -> Json.obj("leaf" -> Json.fromString("zz")))
      )
      expect.same(
        Left(
          Evaluate.Error.Binding(
            "a",
            BuiltValues.ReadError.Decode(
              DecodeFailure(List(Preview.of("leaf")), DecodeReason.PatternMismatch("([0-9a-f]{2})*", Preview.of("zz")))
            )
          )
        ),
        Evaluate.record(res.jlvm, c)
      )
    }
  }

  pureTest("a reference fault during the unlimited metering check is preserved") {
    val fault = Fault.MissingReply
    val c = FuzzCase("metered", FuzzCase.Source.Surface("1"), "null", gasLimit = Some(1L))
    val r = Record(c.name, Some("01"), Record.Outcome.Value(ByteVector(1)), None, 1L, Vector.empty)
    expect.same(Left(fault), Evaluate.metered(_ => Left(fault), c, r))
  }

  test("a fault during gas-case generation survives a later successful reply") { res =>
    IO {
      val fault = Fault.MissingReply
      val gen = new Generator(res.corpus, _ => Left(fault), Some("grammar"))
      val generated = (0L until 30L).iterator.map(gen(seed, _)).find(_.referenceFault.nonEmpty)
      val result = generated.map { g =>
        val successful = Right(Record(g.c.name, None, Record.Outcome.Value(ByteVector(1)), None, 0L, Vector.empty))
        g.replies(Replies(successful, Vector("rust" -> successful))).scala
      }
      expect.same(Some(Left(fault)), result)
    }
  }

  test("a generation-time fault is saved without a contradictory successful shrink replay") { res =>
    Resource
      .make(IO.blocking(Files.createTempDirectory("difffuzz-reference-fault"))) { out =>
        IO.blocking {
          Files.deleteIfExists(out.resolve("findings.jsonl"))
          Files.delete(out)
        }
      }
      .use { out =>
        val fault = Fault.MissingReply
        val c = FuzzCase("generation fault", FuzzCase.Source.Surface("1"), "null")
        val g = Generated(c, "grammar", false, Some(fault))
        val successful = Right(Record(c.name, None, Record.Outcome.Value(ByteVector(1)), None, 0L, Vector.empty))
        val replies = g.replies(Replies(successful, Vector("rust" -> successful)))
        val options = Options(out = out, shrinkBudget = 1)
        for {
          stats <- DiffFuzz.judge(options, new DiffFuzz.Runners(res.jlvm, Vector.empty), Child.Never, Vector(g), Vector(replies), Stats())
          saved <- IO.blocking(new String(Files.readAllBytes(Options.findings(options)), UTF_8))
          rows  <- saved.linesIterator.toList.traverse(io.circe.parser.parse(_)).liftTo[IO]
        } yield
          expect
            .same(Vector(Finding(Set(Diff("scala", Field.Fault)), c, c, replies, "grammar")), stats.findings)
            .and(expect.same(List(Right("found")), rows.map(_.hcursor.get[String]("stage"))))
      }
  }

  test("the reference's record of every jlvm_v1.json case is the case's row: its outcome, gas and warnings") { res =>
    val cases = res.corpus.all
    cases.traverse(c => IO.defer(res.attempt(c).leftMap(Fault.Failure(_)).liftTo[IO]).tupleLeft(c)).map { records =>
      val coverage = expect
        .same(3371, cases.size)
        .and(expect.same(3371, res.rows.size))
        .and(expect.same(res.rows.keySet, cases.map(_.name).toSet))
      records.foldLeft(coverage) {
        case (acc, (c, record)) =>
          val row = res.rows.get(c.name)
          val warnings = record.warnings.map {
            case (head, length) =>
              vectorModel.JlvmWarning(
                "AmbiguousPath",
                vectorModel.JlvmWarningPreview(head, BigInt(length), head.getBytes(UTF_8).length.toLong < length)
              )
          }
          acc
            .and(expect(Some(Record.outcomeJson(record.outcome)) == row.map(_._1), s"${c.name}: outcome"))
            .and(expect(Some(BigInt(record.gasUsed)) == row.map(_._2), s"${c.name}: gas"))
            .and(expect(Some(warnings) == row.map(_._3), s"${c.name}: complete ordered warnings"))
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
      val required = Set("name", "outcome", "gasUsed", "warnings")
      val optional = Set("program", "available")
      def keys(r: Record): Set[String] = Record.json(r).asObject.toList.flatMap(_.keys).toSet
      records
        .foldLeft(success) { (acc, r) =>
          acc
            .and(expect.same(Right(r), Record.fromLine(Record.json(r).noSpaces)))
            .and(expect.same(Left(WireError.Keys(required, optional, keys(r) + "json")), Record.fromLine(withExtra(r))))
            .and(expect.same(Left(WireError.Keys(required, optional, keys(r) - "gasUsed")), Record.fromLine(withoutGas(r))))
        }
        .and(expect.same(Left(WireError.CauseCardinality(2)), Record.fromLine(twoFamilies)))
        .and(expect.same(Left(WireError.Keys(Set("cbor"), Set.empty, Set("cbor", "json"))), Record.fromLine(extraInOutcome)))
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
            e => acc.and(failure(e.toString)),
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
            .same(Vector.fill(3)(Left(Child.Error.Deadline)), replies)
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
          acc.and(
            expect(
              Evaluate.metered(res.attempt, g.c, res.reference(g.c)) == Right(true),
              s"${g.c.name}: ${FuzzCase.row(g.c).noSpaces.take(300)}"
            )
          )
      }
    }
  }
}
