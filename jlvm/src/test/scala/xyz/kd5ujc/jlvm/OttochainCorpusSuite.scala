package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{IO, Resource}
import cats.syntax.all._

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue.CInt

import io.circe.{Decoder, Json}
import weaver.{Expectations, IOSuite}

/**
 * The ottochain family's programs, ported to the strict surface (`resources/programs/ottochain`):
 *   - `sdk/`: ottochain-sdk's fiber-app definitions, each transition's guard and effect as the SDK puts them on the
 *     wire (`toProtoDefinition`);
 *   - `ottochain/`: ottochain's e2e examples and the JSON program text of its test suites;
 *   - `services/`: ottochain-services' program literals not already above.
 *
 * Each corpus file mirrors one source file (`source`: repository, commit, path) and lists its expressions with their
 * JSON path there, the rewrites made (`changes`), and every other place the same program occurs (`alsoAt`).
 * Expressions holding `placeholders` were extracted from interpolated Scala or TS text. There is no fiber engine yet,
 * so the corpus is checked as programs; a few guards and effects are also evaluated on sample events.
 */
object OttochainCorpusSuite extends IOSuite {

  final case class Source(repo: String, commit: String, file: String)

  /** `line` is where the source has it, when the source is text a path alone does not locate (Scala, TS). */
  final case class Entry(path: String, line: Option[Int], program: Json)
  final case class CorpusFile(source: Source, expressions: List[Entry])

  private given Decoder[Source] = Decoder.forProduct3("repo", "commit", "file")(Source.apply)
  private given Decoder[Entry] = Decoder.forProduct3("path", "line", "program")(Entry.apply)
  private given Decoder[CorpusFile] = Decoder.forProduct2("source", "expressions")(CorpusFile.apply)

  /** One located expression: the corpus file it is in, and where the source has it. */
  final case class Located(file: String, entry: Entry) {
    def where: String = s"$file ${entry.path}${entry.line.fold("")(n => s" (line $n)")}"
  }

  final case class Corpus(jlvm: Jlvm, files: List[(String, CorpusFile)]) {
    val entries: List[Located] = files.flatMap { case (name, f) => f.expressions.map(Located(name, _)) }
  }
  type Res = Corpus

  private val root: Path = Paths.get("jlvm", "src", "test", "resources", "programs", "ottochain")

  private def read(p: Path): IO[(String, CorpusFile)] =
    IO.blocking(new String(Files.readAllBytes(p), UTF_8))
      .flatMap(text => io.circe.parser.decode[CorpusFile](text).liftTo[IO])
      .tupleLeft(root.relativize(p).toString)

  private val load: IO[List[(String, CorpusFile)]] =
    Resource
      .fromAutoCloseable(IO.blocking(Files.walk(root)))
      .use(s => IO.blocking(s.iterator.asScala.filter(_.toString.endsWith(".json")).toList.sortBy(_.toString)))
      .flatMap(_.traverse(read))

  def sharedResource: Resource[IO, Corpus] = Resource.eval((Jlvm.load[IO], load).mapN(Corpus.apply))

  /** The corpus as built: a lost file or expression fails here rather than shrinking the checks below. */
  private val CorpusFiles = 114
  private val Expressions = 965

  test("the corpus holds every ported source file, each with its expressions") { c =>
    IO(
      expect
        .same(CorpusFiles, c.files.size)
        .and(expect.same(Expressions, c.entries.size))
        .and(expect(c.files.forall(_._2.expressions.nonEmpty), "a corpus file with no expressions"))
    )
  }

  private def strict(e: Entry): Either[String, Program] =
    for {
      p     <- Parser.parse(e.program.noSpaces).leftMap(err => s"parse: ${err.message}")
      _     <- Compiled(p).leftMap(err => s"compile: ${err.message}")
      text  <- Parser.render(p).leftMap(err => s"render: ${err.message}")
      back  <- Parser.parse(text.noSpaces).leftMap(err => s"rendered text does not parse: ${err.message}")
      _     <- Either.cond(back == p, (), "the rendered text reads back as another program")
      bytes <- Parser.encode(p).leftMap(err => s"encode: ${err.message}")
      dec   <- Parser.decode(bytes).leftMap(err => s"decode: ${err.message}")
      _     <- Either.cond(dec == p, (), "the canonical encoding decodes to another program")
    } yield p

  private def failures(c: Corpus)(f: Located => Option[String]): Expectations = {
    val bad = c.entries.flatMap(l => f(l).map(m => s"${l.where}: $m"))
    expect(bad.isEmpty, s"${bad.size} failing, first: ${bad.take(10).mkString("; ")}")
  }

  test("every expression parses strictly, compiles, renders back to the same program, and round-trips its encoding") { c =>
    IO(failures(c)(l => strict(l.entry).left.toOption))
  }

  /** The host bindings an ottochain fiber engine supplies: consensus context, the same on every node. */
  private val HostBindings = Set("ordinal", "epochProgress", "lastSnapshotHash")
  private val CallbackBindings = Set("current", "index", "accumulator")

  /** Every `$name` a program reads, and every name its `let`s bind. A worklist, so any depth is stack-safe. */
  private def bindings(e: Expr): (Set[String], Set[String]) = {
    @tailrec
    def go(todo: List[Expr], read: Set[String], bound: Set[String]): (Set[String], Set[String]) =
      todo match {
        case Nil                                   => (read, bound)
        case Expr.Var(VarRef.Bound(n, _), _) :: tl => go(tl, read + n, bound)
        case (Expr.Var(_, _) | Expr.Lit(_)) :: tl  => go(tl, read, bound)
        case Expr.VarAt(p, _) :: tl                => go(p :: tl, read, bound)
        case Expr.Spread(_, a) :: tl               => go(a :: tl, read, bound)
        case Expr.Arr(is) :: tl                    => go(is.toList ::: tl, read, bound)
        case Expr.Obj(fs) :: tl                    => go(fs.values.toList ::: tl, read, bound)
        case Expr.Apply(Op.Let, as @ Vector(Expr.Arr(pairs), _)) :: tl =>
          val names = pairs.collect { case Expr.Arr(Vector(Expr.Lit(CanonicalValue.CText(n)), _)) => n }
          go(as.toList ::: tl, read, bound ++ names)
        case Expr.Apply(_, as) :: tl => go(as.toList ::: tl, read, bound)
      }
    go(List(e), Set.empty, Set.empty)
  }

  test("every $-binding read is one the fiber engine supplies, a callback's, or a let's: no $timestamp") { c =>
    IO(failures(c) { l =>
      strict(l.entry).toOption.flatMap { p =>
        val (read, bound) = bindings(p.expr)
        val unknown = read -- HostBindings -- CallbackBindings -- bound
        Option.when(unknown.nonEmpty)(s"reads ${unknown.toList.sorted.map("$" + _).mkString(", ")}")
      }
    })
  }

  // ---- a few guards and effects run on sample events (the engine's context: state, event, proofs; $ordinal bound)

  /** An expression of the corpus: its corpus file, its path in the source, and its source line where needed. */
  final case class At(file: String, path: String, line: Option[Int] = scala.None)

  private def find(c: Corpus, at: At): Either[String, Program] =
    c.files.collectFirst { case (at.file, f) => f.expressions.find(e => e.path == at.path && at.line.forall(e.line.contains)) }.flatten
      .toRight(s"no expression at $at")
      .flatMap(strict)

  private def value(json: String): Either[String, CanonicalValue] = JsonSurface.parse(json).leftMap(_.message)

  private def run(c: Corpus, at: At, data: String, ordinal: Long = 0): Either[String, CanonicalValue] =
    (find(c, at), value(data)).tupled.flatMap { (p, d) =>
      c.jlvm.evaluate(p, d, Map("ordinal" -> CInt(BigInt(ordinal))), Config()).result.leftMap(e => s"$at: $e")
    }

  private def bool(b: Boolean): Either[String, CanonicalValue] = Right(CanonicalValue.CBool(b))

  test("$timestamp deadlines count in $ordinal: the Vickrey reveal opens at the deadline ordinal") { c =>
    val reveal = At("sdk/src/privacy/vickreyAuctionDef.json", "$.transitions[0].guard")
    val state = """{"state": {"deadline": 100}}"""
    IO(
      expect
        .same(bool(false), run(c, reveal, state, ordinal = 99))
        .and(expect.same(bool(true), run(c, reveal, state, ordinal = 100)))
    )
  }

  test("!== is the negation of ===: the clinical-trial resolve guard takes every severity but critical") { c =>
    val resolve = At(
      "ottochain/modules/shared-data/src/test/scala/xyz/kd5ujc/shared_data/examples/ClinicalTrialStateMachineSuite.scala.json",
      "$.transitions[2].guard",
      Some(661)
    )
    def guard(state: String) = run(c, resolve, s"""{"state": $state}""")
    IO(
      expect
        .same(bool(false), guard("""{"severity": "critical"}"""))
        .and(expect.same(bool(true), guard("""{"severity": "mild"}""")))
        .and(expect.same(bool(true), guard("{}")))
    )
  }

  /** 32-byte words of a public-values blob, as an SP1 program commits them. */
  private def word(b: Int): String = f"$b%02x" * 32

  test("hex read as text in metakit is bytes here: the shielded-record effect slices words out of publicValues") { c =>
    val file = "ottochain/e2e-test/examples/riverdale-health/record.definition.json"
    val pv = "0x" + word(1) + word(2) + word(3) + word(4)
    val data =
      s"""{"state": {"nullifiers": {}, "commitments": [], "knownRoots": [], "leafCount": 0, "transitions": 0},
         | "event": {"publicValues": "$pv", "newRoot": "0x${word(9)}"}}""".stripMargin
    val expected =
      s"""{"nullifiers": {"0x${word(2)}": true}, "commitments": ["0x${word(3)}"], "knownRoots": ["0x${word(9)}"],
         | "currentRoot": "0x${word(9)}", "leafCount": 1, "transitions": 1, "lastUpdateAt": 7}""".stripMargin
    IO(expect.same(value(expected), run(c, At(file, "$.transitions[0].effect"), data, ordinal = 7)))
  }

  /**
   * ottochain's zk-eligibility e2e event (`e2e-test/examples/zk-eligibility/event-prove.ts` at 7ada01f): a real SP1
   * Groth16 bundle over the rule `{">=": [{"var": "score"}, 700]}` with data `{"score": 740}`.
   */
  private val EligibilityPublicValues =
    "0xccc6c597899c9e0dc76770e4e288fd9796431140c8cd4facc9f01daeaac0bb62fe47d84fb51a51fb7d89133d93ed9c734670cca1662b8e682e" +
    "3bd923c40ffa1b6273151f959616268004b58dbb21e5c851b7b8d04498b4aabee12291d22fc034000000000000000000000000000000000000" +
    "0000000000000000000000000001"
  private val EligibilityProof =
    "0x4388a21c0000000000000000000000000000000000000000000000000000000000000000002f850ee998974d6cc00e50cd0814b098c05bf" +
    "ade466d28573240d057f2535200000000000000000000000000000000000000000000000000000000000000001cfbc4deb8dce6037d9768f05" +
    "1fcf6c9a9e1d120a9e43307b7faa18292c207542df97b6fc5323b7b306f65661404feb3db64410d22439b5bcda7718a301167121683dde3253" +
    "729acd334808bd711b0acc0ef6f0a18c31f7be8ca7cf983e14069080f58e0c1c0c557379210e05d63d96bdc1fc9786f9134c792e52f2770cdf" +
    "38d1741702ac4ed09ba71bc9c82463d66224ff23bdf849de78b02fe3dd3594911a200e8ce59e7dd87d48ab7516e4af19fddc1253a06d5e038b4" +
    "a653961e14cc0a4e2b7b20638e45e04be44c3e85ef77bc475661ca52a254e0ffa18e049caa5db4b20dd042b6e3e26e89c4b3b96b84c6445346" +
    "43b5f2f0c5e40585ae449632f4fb08"

  test("the zk-eligibility guard, its hex words read as bytes, accepts ottochain's real SP1 bundle and no altered one") { c =>
    val guard = At("ottochain/e2e-test/examples/zk-eligibility/definition.json", "$.transitions[0].guard")
    def event(pv: String) = s"""{"event": {"publicValues": "$pv", "proof": "$EligibilityProof"}}"""
    // the rule-hash word (bytes 0..32) altered: the proof no longer verifies
    val otherRule = "0x" + "dd" + EligibilityPublicValues.drop(4)
    IO(
      expect
        .same(bool(true), run(c, guard, event(EligibilityPublicValues)))
        .and(expect.same(bool(false), run(c, guard, event(otherRule))))
    )
  }
}
