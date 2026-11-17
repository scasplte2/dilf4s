package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.{BuiltValues, CanonicalValue}
import xyz.kd5ujc.core.circe.CanonicalCirce
import xyz.kd5ujc.core.hash.Sha256
import xyz.kd5ujc.jlvm.JlvmError._
import xyz.kd5ujc.models.{jlvm_vectors => vectorModel}

import io.circe.syntax._
import io.circe.{Decoder, Json}
import weaver.IOSuite

/**
 * Conformance vectors for docs/specs/jlvm.md: each input case (`vectors/jlvm_cases.json`, a strict program and its data)
 * evaluated with no host bindings, the default limit and the latest schedule, by the production driver.
 */
object JlvmVectors {

  val path: Path = Paths.get("shared", "vectors", "jlvm_v1.json")
  val parseRefusalsPath: Path = Paths.get("shared", "vectors", "jlvm_parse_refusals_v1.tsv")

  def renderParseRefusals(in: Inputs): String =
    in.cases.flatMap(i => Parser.parse(i.program).left.toOption.map(e => s"${i.name}\t${e.kind.wire}\n")).mkString

  /**
   * `env` holds host bindings as JSON surface text, and `built` host bindings too heavy to write out, as `built-value`
   * recipes (shared/schemas/jlvm-vectors.cddl); `gasLimit` replaces the default limit.
   */
  final case class Input(
    name:     String,
    program:  String,
    data:     String,
    env:      Option[Map[String, String]],
    built:    Option[Map[String, Json]],
    gasLimit: Option[Long]
  )
  final case class Inputs(source: String, cases: List[Input])

  given Decoder[Input] = Decoder.forProduct6("name", "program", "data", "env", "built", "gasLimit")(Input.apply)
  given Decoder[Inputs] = Decoder.forProduct2("source", "cases")(Inputs.apply)

  /**
   * The metakit-derived cases, then the example contracts' (ContractVectorCases), then metakit's out-of-gas cases at
   * their own gas limits, then the per-operation coverage (OpCoverageCases).
   */
  val files: List[String] =
    List("jlvm_cases.json", "jlvm_contract_cases.json", "jlvm_gas_cases.json", "jlvm_op_cases.json", "jlvm_warning_cases.json")

  private def read(name: String): IO[Inputs] =
    Resource
      .fromAutoCloseable(IO.blocking(getClass.getResourceAsStream(s"/vectors/$name")))
      .use(in => IO.blocking(new String(in.readAllBytes(), UTF_8)))
      .flatMap(text => io.circe.parser.decode[Inputs](text).liftTo[IO])

  val inputs: IO[Inputs] =
    files.traverse(read).map(all => Inputs(all.map(_.source).mkString("; "), all.flatMap(_.cases)))

  def run(jlvm: Jlvm, i: Input, driver: Config.Driver = Config.Driver.Direct): Outcome =
    (
      Parser.parse(i.program),
      JsonSurface.parse(i.data),
      i.env.getOrElse(Map.empty).toList.traverse { case (k, v) => JsonSurface.parse(v).map(k -> _) }
    ).tupled.map {
      case (p, d, env) =>
        // a recipe that does not build is a broken vector file, not an outcome
        val built = i.built.getOrElse(Map.empty).toList.map {
          case (k, b) =>
            k -> CanonicalCirce
              .fromCirce(b)
              .leftMap(BuiltValues.ReadError.Canonical(_))
              .flatMap(BuiltValues.built)
              .fold(e => sys.error(s"${i.name}: $k: $e"), identity)
        }
        (p, d, env ++ built)
    }
      .fold(
        e => Outcome(Gas.Zero, Left(e)),
        {
          case (p, d, env) =>
            jlvm.evaluate(p, d, env.toMap, Config(limit = i.gasLimit.fold(GasLimit.Default)(l => GasLimit(Gas(l))), driver = driver))
        }
      )

  private def outcome(o: Outcome): Json =
    o.result.fold(
      e =>
        Json.fromFields(
          List("error" -> e.kind.wire.asJson) ++ e.site.map(s => "op" -> s.wire.asJson) ++ cause(e).map("cause" -> _)
        ),
      v =>
        Json
          .obj("cbor" -> xyz.kd5ujc.core.canonical.CanonicalGens.encoded(v).toHex.asJson)
          .deepMerge(JsonSurface.render(v).toOption.fold(Json.obj())(t => Json.obj("json" -> t.asJson)))
    )

  /** A Crypto error's cause as the vectors record it: `{family: kind}`, the family's name in lower camel case. */
  def cause(e: JlvmError): Option[Json] = causeValue(e).map(causeJson)

  def causeValue(e: JlvmError): Option[vectorModel.JlvmCause] = e.cause.map {
    case CryptoCause.Signing(x)        => vectorModel.JlvmSigningCause(x.kind)
    case CryptoCause.Sigma(x)          => vectorModel.JlvmSigmaCause(x.kind)
    case CryptoCause.Zk(x)             => vectorModel.JlvmZkCause(x.kind)
    case CryptoCause.Groth16(x)        => vectorModel.JlvmGroth16Cause(x.kind)
    case CryptoCause.Vrf(x)            => vectorModel.JlvmVrfCause(x.kind)
    case CryptoCause.Hash(x)           => vectorModel.JlvmHashCause(x.kind)
    case CryptoCause.Merkle(x)         => vectorModel.JlvmMerkleCause(x.kind)
    case CryptoCause.Trie(x)           => vectorModel.JlvmTrieCause(x.kind)
    case CryptoCause.Poseidon(x)       => vectorModel.JlvmPoseidonCause(x.kind)
    case CryptoCause.PoseidonMerkle(x) => vectorModel.JlvmPoseidonMerkleCause(x.kind)
  }

  def causeJson(c: vectorModel.JlvmCause): Json = c match {
    case vectorModel.JlvmSigningCause(kind)        => Json.obj("signing" -> kind.wire.asJson)
    case vectorModel.JlvmSigmaCause(kind)          => Json.obj("sigma" -> kind.wire.asJson)
    case vectorModel.JlvmZkCause(kind)             => Json.obj("zk" -> kind.wire.asJson)
    case vectorModel.JlvmGroth16Cause(kind)        => Json.obj("groth16" -> kind.wire.asJson)
    case vectorModel.JlvmVrfCause(kind)            => Json.obj("vrf" -> kind.wire.asJson)
    case vectorModel.JlvmHashCause(kind)           => Json.obj("hash" -> kind.wire.asJson)
    case vectorModel.JlvmMerkleCause(kind)         => Json.obj("merkle" -> kind.wire.asJson)
    case vectorModel.JlvmTrieCause(kind)           => Json.obj("trie" -> kind.wire.asJson)
    case vectorModel.JlvmPoseidonCause(kind)       => Json.obj("poseidon" -> kind.wire.asJson)
    case vectorModel.JlvmPoseidonMerkleCause(kind) => Json.obj("poseidonMerkle" -> kind.wire.asJson)
  }

  /** The arguments each aliased operation's row applies it to. */
  private def aliasArgs(op: Op): String = op match {
    case Op.Get    => """[{"var": "m"}, "k"]"""
    case Op.Set    => """[{"var": "m"}, "k", 1]"""
    case Op.Unset  => """[{"var": "m"}, "k"]"""
    case Op.Length => """[{"var": "xs"}]"""
    case other     => sys.error(s"no alias row for ${other.wire}")
  }

  /** Each alias of [[Ops.Aliases]]: what it parses to, its canonical form (the own name) and that form's identity. */
  def aliases: List[Json] =
    Ops.Aliases.toList.sortBy(_._1).map {
      case (alias, op) =>
        val program = s"""{"$alias": ${aliasArgs(op)}}"""
        val own = s"""{"${op.wire}": ${aliasArgs(op)}}"""
        val parsed = Parser.parse(program).fold(e => sys.error(s"$alias: ${e.message}"), identity)
        Json.obj(
          "alias"   -> alias.asJson,
          "op"      -> op.wire.asJson,
          "program" -> program.asJson,
          "own"     -> own.asJson,
          "cbor"    -> Parser.encode(parsed).fold(e => sys.error(e.message), _.toHex).asJson,
          "id"      -> Parser.id[Sha256](parsed).fold(e => sys.error(e.message), _.bytes.toHex).asJson
        )
    }

  def surfaceOf(json:       String): CanonicalValue = JsonSurface.parse(json).fold(e => sys.error(e.message), identity)
  private def surface(json: String): CanonicalValue = surfaceOf(json)

  /**
   * Canonical forms read as programs (jlvm.md section 1, "Reading a canonical form"), each a value built with shared
   * parts, and the refusal each gets in the normative order: the weight guard before the shape, the shape before the
   * `var` paths, and the paths before the size bound.
   */
  val decodingForms: List[(String, BuiltValues.Built)] = {
    val doubled = BuiltValues.of(surface("1")).wrap(surface("""["$", "$"]"""), 24)
    val nulls = BuiltValues.of(surface("{}")).copies(140000)
    List(
      "an overweight form that is not a map is Limit, before its shape is read" -> doubled,
      "an overweight map with a wrong version is Limit, before its version is read" -> doubled.wrap(
        surface("""{"jlvm": 2, "expr": "$"}""")
      ),
      "a map with a wrong version is Malformed" -> BuiltValues.of(surface("""{"jlvm": 2, "expr": {}}""")),
      "a program too large with a var path that does not parse is Malformed: paths come before the size bound" ->
      nulls.wrap(surface("""{"jlvm": 1, "expr": {"arr": [{"var": "a..b"}, {"arr": "$"}]}}""")),
      "a program too large by its size, 140,000 shared nulls, is Limit" -> nulls.wrap(surface("""{"jlvm": 1, "expr": {"arr": "$"}}""")),
      "a program of shared parts reads as itself" ->
      BuiltValues.of(surface("""{"op": "+", "args": []}""")).copies(3).wrap(surface("""{"jlvm": 1, "expr": {"arr": "$"}}"""))
    )
  }

  /** Each decoding row: its form, and the program it reads as (its canonical CBOR) or the error kind it is refused with. */
  def decodings: List[Json] =
    decodingForms.map {
      case (name, b) =>
        val form = BuiltValues.build(b).fold(e => sys.error(s"$name: $e"), identity)
        val outcome = Parser
          .fromCanonical(form)
          .fold(
            e => Json.obj("error" -> e.kind.wire.asJson),
            p => Json.obj("cbor" -> Parser.encode(p).fold(e => sys.error(e.message), _.toHex).asJson)
          )
        Json.obj(
          "name"    -> name.asJson,
          "form"    -> CanonicalCirce.toCirce(BuiltValues.value(b)).fold(e => sys.error(e.toString), identity),
          "outcome" -> outcome
        )
    }

  def warningJson(warning: Warning): Json = warning match {
    case Warning.AmbiguousPath(path) =>
      Json.obj(
        "kind" -> "AmbiguousPath".asJson,
        "path" -> Json.obj("head" -> path.head.asJson, "length" -> path.length.asJson, "truncated" -> path.truncated.asJson)
      )
  }

  def render(jlvm: Jlvm, in: Inputs): String =
    Json
      .obj(
        "spec"     -> "docs/specs/jlvm.md".asJson,
        "version"  -> Program.Version.asJson,
        "schedule" -> GasSchedule.Latest.version.asJson,
        "gasLimit" -> GasLimit.Default.amount.value.asJson,
        "source"   -> in.source.asJson,
        "cases" -> in.cases.map { i =>
          val o = run(jlvm, i)
          Json.fromFields(
            List("name" -> i.name.asJson, "program" -> i.program.asJson, "data" -> i.data.asJson) ++
            i.env.map(e => "env" -> Json.fromFields(e.toList.sortBy(_._1).map { case (k, v) => k -> v.asJson })) ++
            i.built.map(b => "built" -> Json.fromFields(b.toList.sortBy(_._1))) ++
            i.gasLimit.map(l => "gasLimit" -> l.asJson) ++
            List("outcome" -> outcome(o), "gasUsed" -> o.gasUsed.value.asJson, "warnings" -> o.warnings.map(warningJson).asJson)
          )
        }.asJson,
        "aliases"   -> aliases.asJson,
        "decodings" -> decodings.asJson
      )
      .spaces2 + "\n"
}

object JlvmVectorGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    (Jlvm.load[IO], JlvmVectors.inputs).flatMapN { (jlvm, in) =>
      IO.blocking(Files.write(JlvmVectors.path, JlvmVectors.render(jlvm, in).getBytes(UTF_8))) >>
      IO.blocking(Files.write(JlvmVectors.parseRefusalsPath, JlvmVectors.renderParseRefusals(in).getBytes(UTF_8)))
    }
      .as(ExitCode.Success)
}

object JlvmVectorsSuite extends IOSuite {

  type Res = (Jlvm, JlvmVectors.Inputs)
  def sharedResource: Resource[IO, Res] = Resource.eval((Jlvm.load[IO], JlvmVectors.inputs).tupled)

  test("warning cases pin exact contents, order, deduplication, truncation and the cap") {
    case (jlvm, in) =>
      IO {
        val x = Vector(("x", 1L, false))
        val expected = Map(
          "warning: a plain path shadows the root"               -> x,
          "warning: explicit current avoids ambiguity"           -> Vector.empty[(String, Long, Boolean)],
          "warning: repeated ambiguity is recorded once"         -> x,
          "warning: warnings follow evaluation order"            -> Vector(("b", 1L, false), ("a", 1L, false)),
          "warning: a long Unicode path has an exact preview"    -> Vector(("😀" * 32, 160L, true)),
          "warning: distinct paths sharing a preview warn once"  -> Vector(("x" * 32, 33L, true)),
          "warning: the cap retains the first 64 distinct paths" -> Vector.tabulate(64)(i => (f"p$i%02d", 3L, false)),
          "warning: warnings survive a later type refusal"       -> x
        )
        val cases = in.cases.map(c => c.name -> c).toMap
        expected.toVector.foldMap {
          case (name, warnings) =>
            val actual = cases
              .get(name)
              .map(c =>
                JlvmVectors.run(jlvm, c).warnings.map {
                  case Warning.AmbiguousPath(p) => (p.head, p.length, p.truncated)
                }
              )
            expect.same(name -> Some(warnings), name -> actual)
        }.and(expect.same(3371, in.cases.size))
      }
  }

  test("committed JLVM vectors match the reference implementation") {
    case (jlvm, in) =>
      IO.blocking(new String(Files.readAllBytes(JlvmVectors.path), UTF_8)).map { committed =>
        expect(
          committed == JlvmVectors.render(jlvm, in),
          s"${JlvmVectors.path} is stale; regenerate with: tools/sbt \"jlvm/Test/runMain xyz.kd5ujc.jlvm.JlvmVectorGen\""
        )
      }
  }

  test("committed program parse refusals name exactly the reference's excluded cases") {
    case (_, in) =>
      IO.blocking(new String(Files.readAllBytes(JlvmVectors.parseRefusalsPath), UTF_8)).map { committed =>
        expect.same(67L, committed.linesIterator.size.toLong).and(expect.same(JlvmVectors.renderParseRefusals(in), committed))
      }
  }

  test("each alias row: the alias parses to the program its own name writes, whose canonical form names the operation") {
    case (_, _) =>
      IO.blocking(new String(Files.readAllBytes(JlvmVectors.path), UTF_8)).flatMap(io.circe.parser.parse(_).liftTo[IO]).map { v =>
        val rows = v.hcursor.downField("aliases").as[List[Json]].getOrElse(Nil)
        rows.foldLeft(expect.same(Ops.Aliases.size, rows.size)) { (acc, row) =>
          val c = row.hcursor
          val field = (k: String) => c.get[String](k).getOrElse("")
          val (alias, own) = (Parser.parse(field("program")), Parser.parse(field("own")))
          // the recorded form decodes to the program the own name writes
          val decoded = scodec.bits.ByteVector.fromHex(field("cbor")).map(Parser.decode)
          acc
            .and(expect(alias.isRight && alias == own, s"${field("alias")} parses as ${field("op")}"))
            .and(expect.same(Ops.named(field("alias")).map(_.wire), Some(field("op"))))
            .and(expect.same(Right(field("cbor")), alias.flatMap(Parser.encode).map(_.toHex)))
            .and(expect(own.isRight, "the canonical operation parses successfully"))
            .and(expect.same(Some(own), decoded))
            .and(expect.same(Right(field("id")), alias.flatMap(Parser.id[Sha256]).map(_.bytes.toHex)))
            .and(
              expect(
                !field("cbor").contains(scodec.bits.ByteVector(field("alias").getBytes(UTF_8)).toHex),
                "the form never names the alias"
              )
            )
        }
      }
  }

  private def sameObservedProduct(expected: Product, actual: Product): weaver.Expectations =
    expected.productIterator
      .zip(actual.productIterator)
      .foldLeft(
        expect
          .same(expected.getClass, actual.getClass)
          .and(expect.same(expected.productArity, actual.productArity))
          .and((expected, actual) match {
            case (left: scala.reflect.Enum, right: scala.reflect.Enum) => expect.same(left.ordinal, right.ordinal)
            case _                                                     => success
          })
      ) { case (acc, (left, right)) => acc.and(sameObservedValue(left, right)) }

  private final case class ObservedFailure(width: Long) extends RuntimeException

  // Independent evaluations allocate independent exceptions. Compare every domain field and foreign cause,
  // including diagnostic text, but not allocation identity or driver-specific stack frames.
  private def sameObservedValue(expected: Any, actual: Any): weaver.Expectations = (expected, actual) match {
    case (e: Throwable, a: Throwable) =>
      expect
        .same(e.getClass, a.getClass)
        .and(expect.same(e.getMessage, a.getMessage))
        .and(sameObservedValue(Option(e.getCause), Option(a.getCause)))
        .and(sameObservedValue(e.getSuppressed.toVector, a.getSuppressed.toVector))
        .and((e, a) match {
          case (left: Product, right: Product) => sameObservedProduct(left, right)
          case _                               => success
        })
    case (e: scodec.bits.ByteVector, a: scodec.bits.ByteVector) => expect.same(e, a)
    case (e: Product, a: Product)                               => sameObservedProduct(e, a)
    case (e: Vector[?], a: Vector[?]) =>
      e.zip(a).foldLeft(expect.same(e.size, a.size)) { case (acc, (left, right)) => acc.and(sameObservedValue(left, right)) }
    case _ => expect.same(expected, actual)
  }

  test("observed-value comparison checks exact domain payloads and foreign diagnostics") { _ =>
    IO {
      import xyz.kd5ujc.crypto.signing.CryptoError
      import CryptoError.{DecodeFailure, EncodingReason, KeyReason, Operation, Scheme, Subject}
      val expected = Left(CryptoError.BackendFailure(Operation.Verify, new IllegalArgumentException("foreign diagnostic")))
      val repeated = Left(CryptoError.BackendFailure(Operation.Verify, new IllegalArgumentException("foreign diagnostic")))
      val changedOperation = Left(CryptoError.BackendFailure(Operation.SignSchnorr, new IllegalArgumentException("foreign diagnostic")))
      val changedClass = Left(CryptoError.BackendFailure(Operation.Verify, new IllegalStateException("foreign diagnostic")))
      val changedDiagnostic = Left(CryptoError.BackendFailure(Operation.Verify, new IllegalArgumentException("different diagnostic")))
      val changedCause =
        Left(CryptoError.BackendFailure(Operation.Verify, new IllegalArgumentException("foreign diagnostic", new IllegalStateException())))
      val suppressed = new IllegalArgumentException("foreign diagnostic")
      suppressed.addSuppressed(new IllegalStateException())
      val changedSuppressed = Left(CryptoError.BackendFailure(Operation.Verify, suppressed))
      val width = CryptoError.InvalidEncoding(Subject.Bip340Message, EncodingReason.Length(32, 31))
      val otherWidth = CryptoError.InvalidEncoding(Subject.Bip340Message, EncodingReason.Length(32, 30))
      expect
        .same(true, sameObservedValue(expected, repeated).run.isValid)
        .and(expect.same(false, sameObservedValue(expected, changedOperation).run.isValid))
        .and(expect.same(false, sameObservedValue(expected, changedClass).run.isValid))
        .and(expect.same(false, sameObservedValue(expected, changedDiagnostic).run.isValid))
        .and(expect.same(false, sameObservedValue(expected, changedCause).run.isValid))
        .and(expect.same(false, sameObservedValue(expected, changedSuppressed).run.isValid))
        .and(expect.same(false, sameObservedValue(ObservedFailure(1), ObservedFailure(2)).run.isValid))
        .and(expect.same(false, sameObservedValue(width, otherWidth).run.isValid))
        .and(expect.same(false, sameObservedValue(EncodingReason.OutsideG2, EncodingReason.EmptyAggregate).run.isValid))
        .and(
          expect.same(
            false,
            sameObservedValue(
              CryptoError.InvalidKey(KeyReason.Decode(Scheme.Ristretto255, DecodeFailure.Identity)),
              CryptoError.InvalidKey(KeyReason.Decode(Scheme.Ristretto255, DecodeFailure.ZeroScalar))
            ).run.isValid
          )
        )
        .and(expect.same(false, sameObservedValue(Vector(width), Vector(width, width)).run.isValid))
        .and(
          expect
            .same(true, sameObservedValue(scodec.bits.ByteVector(1, 2), scodec.bits.ByteVector(1) ++ scodec.bits.ByteVector(2)).run.isValid)
        )
        .and(expect.same(false, sameObservedValue(scodec.bits.ByteVector(1, 2), scodec.bits.ByteVector(1, 3)).run.isValid))
    }
  }

  test("every driver gives every vector case the same outcome and gas") {
    case (jlvm, in) =>
      IO {
        in.cases.foldLeft(success) { (acc, i) =>
          val reference = JlvmVectors.run(jlvm, i)
          Config.Driver.values.toList.foldLeft(acc) { (a, d) =>
            val o = JlvmVectors.run(jlvm, i, d)
            val sameResult = (reference.result, o.result) match {
              case (Left(expected), Left(actual)) => sameObservedValue(expected, actual)
              case _                              => expect.same(reference.result, o.result)
            }
            a.and(sameResult)
              .and(expect.same(reference.gasUsed, o.gasUsed))
              .and(expect.same(reference.warnings, o.warnings))
          }
        }
      }
  }

  test("metakit's out-of-gas cases: every charge is taken before the work, and running out uses the whole limit") {
    case (jlvm, in) =>
      IO {
        val gas = in.cases.filter(i => i.name.startsWith("metakit gas oog: ") || i.name.startsWith("dilf4s gas boundary: "))
        // dilf4s's dlog sigma_verify costs less than metakit's limit: it runs, and metakit's BN254 proof is malformed here
        val finishes = Set(
          "metakit gas oog: sigma_verify dlog at 90004",
          "dilf4s gas boundary: nested + at its cost 286",
          "dilf4s gas boundary: var lookup at its cost 74",
          "dilf4s gas boundary: map callback at its cost 1360"
        )
        val read = GasSchedule.V1.varAccess.value + GasSchedule.V1.perPathSegment.value
        def named(name: String): Option[Outcome] = in.cases.find(_.name == name).map(JlvmVectors.run(jlvm, _))
        gas
          .foldLeft(expect.same(15, gas.size)) { (acc, i) =>
            val limit = i.gasLimit.getOrElse(GasLimit.Default.amount.value)
            val o = JlvmVectors.run(jlvm, i)
            val starved = o.result match {
              case Left(JlvmError.OutOfGas(required, available)) => available == limit && required > limit && o.gasUsed == Gas(limit)
              case _                                             => false
            }
            acc.and(expect(starved != finishes.contains(i.name), s"${i.name}: ${o.result}, gas ${o.gasUsed}"))
          }
          // metakit swallowed this failed charge into null for 0 gas
          .and(
            expect.same(
              Option(Outcome(Gas(2), Left(JlvmError.OutOfGas(read, 2)))),
              named("metakit gas oog: var lookup at 2 (metakit gave null for 0 gas)")
            )
          )
          // one gas short of the cost runs out exactly at the last charge
          .and(
            expect.same(
              Option(Outcome(Gas(285), Left(JlvmError.OutOfGas(286, 285)))),
              named("dilf4s gas boundary: nested + one short, 285")
            )
          )
      }
  }

  test("every byte string in every vector outcome is one flat chunk (docs/specs/jlvm.md §5.5)") {
    case (jlvm, in) =>
      IO {

        /** How many byte strings `todo` holds, and how many of them are not one chunk. */
        @scala.annotation.tailrec
        def scan(todo: List[CanonicalValue], seen: Int, rough: Int): (Int, Int) = todo match {
          case Nil                               => (seen, rough)
          case CanonicalValue.CBytes(b) :: rest  => scan(rest, seen + 1, if (FlatBytes.isFlat(b)) rough else rough + 1)
          case CanonicalValue.CArray(vs) :: rest => scan(vs.toList ++ rest, seen, rough)
          case (m: CanonicalValue.CMap) :: rest  => scan(m.entries.values.toList ++ rest, seen, rough)
          case _ :: rest                         => scan(rest, seen, rough)
        }
        val scanned = in.cases.map(i => i.name -> JlvmVectors.run(jlvm, i).result.fold(_ => (0, 0), v => scan(List(v), 0, 0)))
        val rough = scanned.collect { case (name, (_, r)) if r > 0 => name }
        val seen = scanned.map(_._2._1).sum
        expect
          .same(95, seen)
          .and(expect(rough.isEmpty, s"${rough.size} outcomes hold a rope: ${rough.take(5).mkString("; ")}"))
      }
  }

  test("a canonical form is refused in the normative order: weight, shape, var paths, size (jlvm.md section 1)") {
    case (_, _) =>
      IO {
        val kinds = JlvmVectors.decodingForms.map {
          case (name, b) => name -> BuiltValues.build(b).toOption.map(Parser.fromCanonical).map(_.left.map(_.kind).map(_ => ()))
        }
        expect.same(
          List(
            Some(Left(JlvmErrorKind.Limit)),
            Some(Left(JlvmErrorKind.Limit)),
            Some(Left(JlvmErrorKind.Malformed)),
            Some(Left(JlvmErrorKind.Malformed)),
            Some(Left(JlvmErrorKind.Limit)),
            Some(Right(()))
          ),
          kinds.map(_._2)
        )
      }
  }

  test("an overweight form built with shared parts is refused from its weight, without being expanded") {
    case (_, _) =>
      IO {
        // [x, x] nested 60 deep: 2^60 leaves in 60 arrays, which no decoder could walk; inside a valid envelope
        val deep = BuiltValues.of(CanonicalValue.CNull).wrap(JlvmVectors.surfaceOf("""["$", "$"]"""), 60)
        val form = BuiltValues.build(deep.wrap(JlvmVectors.surfaceOf("""{"jlvm": 1, "expr": {"arr": "$"}}""")))
        expect(form.map(_.weight > Parser.MaxCanonicalWeight) == Right(true), "the form is overweight")
          .and(expect(form.map(Parser.fromCanonical).exists(_.left.exists(e => e.kind == JlvmErrorKind.Limit)), "and Limit at once"))
      }
  }

  test("case names are unique") {
    case (_, in) =>
      IO {
        expect.same(in.cases.size, in.cases.map(_.name).distinct.size)
      }
  }
}
