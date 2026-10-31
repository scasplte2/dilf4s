package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8

import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._

/**
 * The calling convention of the example contracts in `resources/programs`: a program reads
 * `{"state": ..., "call": {"method": ..., ...}}`, any read-only inputs the host resolves from chain state (as an Ergo
 * data input: `oracle`, an oracle pool's state), and the host bindings (`$sender`, the authenticated caller; `$ordinal`,
 * the snapshot ordinal, which deadlines count in; `$txHash`, what Ergo-style proofs sign), and returns
 * `{"state": next, "effects": [...]}` or `{"reject": reason}`. Every node sees the same inputs and bindings.
 */
object ContractHarness {

  def load(name: String): IO[Program] =
    Resource
      .fromAutoCloseable(IO.blocking(getClass.getResourceAsStream(s"/programs/$name.json")))
      .use(in => IO.blocking(new String(in.readAllBytes(), UTF_8)))
      .flatMap(text => Parser.parse(text).leftMap(e => new IllegalArgumentException(s"$name: ${e.message}")).liftTo[IO])

  def value(json: String): CanonicalValue = JsonSurface.parse(json).fold(e => sys.error(s"$e in $json"), identity)
  def text(s:     String): CanonicalValue = CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)

  final case class Contract(jlvm: Jlvm, program: Program, inputs: List[(String, CanonicalValue)] = Nil) {

    /** The same contract, reading these read-only inputs beside its state and call. */
    def withInputs(in: (String, CanonicalValue)*): Contract = copy(inputs = in.toList)

    def run(state: CanonicalValue, call: String, env: (String, CanonicalValue)*): Outcome =
      jlvm.evaluate(program, ContractHarness.data(state, call, inputs), env.toMap, Config())

    /** The next state, or the rejection or error as text. */
    def next(state: CanonicalValue, call: String, env: (String, CanonicalValue)*): Either[String, CanonicalValue] =
      run(state, call, env*).result.leftMap(_.toString).flatMap {
        case CMap(m) if m.contains("state") => m.get("state").toRight("no state")
        case CMap(m)                        => m.get("reject").fold("no result".asLeft[CanonicalValue])(r => s"reject: ${render(r)}".asLeft)
        case other                          => s"not a contract result: ${render(other)}".asLeft
      }

    def effects(state: CanonicalValue, call: String, env: (String, CanonicalValue)*): Either[String, CanonicalValue] =
      run(state, call, env*).result.leftMap(_.toString).flatMap {
        case CMap(m) => m.get("effects").toRight(s"no effects: ${m.get("reject").map(render)}")
        case other   => s"not a contract result: ${render(other)}".asLeft
      }
  }

  def data(state: CanonicalValue, call: String, inputs: List[(String, CanonicalValue)]): CanonicalValue =
    CanonicalValue.map(("state" -> state) :: ("call" -> value(call)) :: inputs).toOption.get

  def render(v: CanonicalValue): String = JsonSurface.render(v).getOrElse(v.toString)

  def rejected(r: Either[String, CanonicalValue]): Boolean = r.left.exists(_.startsWith("reject:"))
}
