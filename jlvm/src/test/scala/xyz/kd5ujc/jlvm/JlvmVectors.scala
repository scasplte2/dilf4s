package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.Cbor

import io.circe.syntax._
import io.circe.{Decoder, Json}
import weaver.IOSuite

/**
 * Conformance vectors for docs/specs/jlvm.md: each input case (`vectors/jlvm_cases.json`, a strict program and its data)
 * evaluated with no host bindings, the default limit and the latest schedule, by the production driver.
 */
object JlvmVectors {

  val path: Path = Paths.get("shared", "vectors", "jlvm_v1.json")

  /** `env` holds host bindings as JSON surface text; `gasLimit` replaces the default limit. */
  final case class Input(name: String, program: String, data: String, env: Option[Map[String, String]], gasLimit: Option[Long])
  final case class Inputs(source: String, cases: List[Input])

  given Decoder[Input] = Decoder.forProduct5("name", "program", "data", "env", "gasLimit")(Input.apply)
  given Decoder[Inputs] = Decoder.forProduct2("source", "cases")(Inputs.apply)

  /**
   * The metakit-derived cases, then the example contracts' (ContractVectorCases), then metakit's out-of-gas cases at
   * their own gas limits.
   */
  val files: List[String] = List("jlvm_cases.json", "jlvm_contract_cases.json", "jlvm_gas_cases.json")

  private def read(name: String): IO[Inputs] =
    Resource
      .fromAutoCloseable(IO.blocking(getClass.getResourceAsStream(s"/vectors/$name")))
      .use(in => IO.blocking(new String(in.readAllBytes(), UTF_8)))
      .flatMap(text => io.circe.parser.decode[Inputs](text).liftTo[IO])

  val inputs: IO[Inputs] =
    files.traverse(read).map(all => Inputs(all.map(_.source).mkString("; "), all.flatMap(_.cases)))

  def kind(e: JlvmError): String = e match {
    case _: JlvmError.Malformed  => "Malformed"
    case _: JlvmError.Type       => "Type"
    case _: JlvmError.Arity      => "Arity"
    case _: JlvmError.Arithmetic => "Arithmetic"
    case _: JlvmError.Domain     => "Domain"
    case _: JlvmError.Crypto     => "Crypto"
    case _: JlvmError.Limit      => "Limit"
    case _: JlvmError.OutOfGas   => "OutOfGas"
  }

  def run(jlvm: Jlvm, i: Input, driver: Config.Driver = Config.Driver.Direct): Outcome =
    (
      Parser.parse(i.program),
      JsonSurface.parse(i.data),
      i.env.getOrElse(Map.empty).toList.traverse { case (k, v) => JsonSurface.parse(v).map(k -> _) }
    ).tupled
      .fold(
        e => Outcome(Gas.Zero, Left(e)),
        {
          case (p, d, env) =>
            jlvm.evaluate(p, d, env.toMap, Config(limit = i.gasLimit.fold(GasLimit.Default)(l => GasLimit(Gas(l))), driver = driver))
        }
      )

  private def outcome(o: Outcome): Json =
    o.result.fold(
      e => Json.obj("error" -> kind(e).asJson),
      v =>
        Json
          .obj("cbor" -> Cbor.encode(v).toHex.asJson)
          .deepMerge(JsonSurface.render(v).toOption.fold(Json.obj())(t => Json.obj("json" -> t.asJson)))
    )

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
            i.gasLimit.map(l => "gasLimit" -> l.asJson) ++
            List("outcome" -> outcome(o), "gasUsed" -> o.gasUsed.value.asJson, "warnings" -> o.warnings.size.asJson)
          )
        }.asJson
      )
      .spaces2 + "\n"
}

object JlvmVectorGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    (Jlvm.load[IO], JlvmVectors.inputs)
      .flatMapN((jlvm, in) => IO.blocking(Files.write(JlvmVectors.path, JlvmVectors.render(jlvm, in).getBytes(UTF_8))))
      .as(ExitCode.Success)
}

object JlvmVectorsSuite extends IOSuite {

  type Res = (Jlvm, JlvmVectors.Inputs)
  def sharedResource: Resource[IO, Res] = Resource.eval((Jlvm.load[IO], JlvmVectors.inputs).tupled)

  test("committed JLVM vectors match the reference implementation") {
    case (jlvm, in) =>
      IO.blocking(new String(Files.readAllBytes(JlvmVectors.path), UTF_8)).map { committed =>
        expect(
          committed == JlvmVectors.render(jlvm, in),
          s"${JlvmVectors.path} is stale; regenerate with: sbt \"jlvm/Test/runMain xyz.kd5ujc.jlvm.JlvmVectorGen\""
        )
      }
  }

  test("every driver gives every vector case the same outcome and gas") {
    case (jlvm, in) =>
      IO {
        in.cases.foldLeft(success) { (acc, i) =>
          val reference = JlvmVectors.run(jlvm, i)
          Config.Driver.values.toList.foldLeft(acc) { (a, d) =>
            val o = JlvmVectors.run(jlvm, i, d)
            a.and(expect(o.result == reference.result && o.gasUsed == reference.gasUsed, s"${i.name}: $d disagrees"))
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
          "dilf4s gas boundary: nested + at its cost 361",
          "dilf4s gas boundary: var lookup at its cost 19",
          "dilf4s gas boundary: map callback at its cost 1235"
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
              Option(Outcome(Gas(360), Left(JlvmError.OutOfGas(361, 360)))),
              named("dilf4s gas boundary: nested + one short, 360")
            )
          )
      }
  }

  test("case names are unique") {
    case (_, in) =>
      IO {
        expect.same(in.cases.size, in.cases.map(_.name).distinct.size)
      }
  }
}
