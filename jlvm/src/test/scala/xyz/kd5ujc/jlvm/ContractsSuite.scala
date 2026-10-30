package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8

import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue

import weaver.{Expectations, IOSuite}

/**
 * Example contracts from the projects JLVM replaces, run end to end. `proofchain-fungible.json` is proofchain's ERC-20
 * (`e2e-test/resources/fungible.json` at 28b2d9d), verbatim; `fungible.json` is its port to strict semantics.
 */
object ContractsSuite extends IOSuite {

  final case class Programs(jlvm: Jlvm, original: Program, ported: Program)
  type Res = Programs

  private def load(name: String): IO[Program] =
    Resource
      .fromAutoCloseable(IO.blocking(getClass.getResourceAsStream(s"/programs/$name")))
      .use(in => IO.blocking(new String(in.readAllBytes(), UTF_8)))
      .flatMap(text => IO.fromEither(Parser.parse(text).leftMap(e => new IllegalArgumentException(e.message))))

  def sharedResource: Resource[IO, Programs] =
    Resource.eval((Jlvm.load[IO], load("proofchain-fungible.json"), load("fungible.json")).mapN(Programs.apply))

  private def value(json: String): CanonicalValue = JsonSurface.parse(json).fold(e => sys.error(e.toString), identity)

  private val content =
    """{"name": "Token", "symbol": "TOK", "totalSupply": 100, "balances": {"alice": 60, "bob": 40},
      | "allowances": {"alice": {"carol": 25}}}""".stripMargin

  /** The host's call: the method's arguments, the signers' proofs and the current content, as one data value. */
  private def call(args: String, signers: List[String], state: String = content): CanonicalValue = {
    val proofs = signers.map(s => s"""{"address": "$s", "signature": "0x00"}""").mkString("[", ", ", "]")
    value(s"""{${args.stripPrefix("{").stripSuffix("}")}, "proofs": $proofs, "content": $state}""")
  }

  private def run(p: Program, data: CanonicalValue)(using jlvm: Jlvm): Outcome = jlvm.evaluate(p, data, Map.empty, Config())

  private def updates(json: String): Either[JlvmError, CanonicalValue] = Right(value(json))

  private def check(name: String)(f: Programs => Jlvm ?=> Expectations) = test(name)(r => IO(f(r)(using r.jlvm)))

  check("the ERC-20 is strict JLVM as written, and its port differs only in reading an absent balance as 0") { r =>
    expect(r.original != r.ported, "two programs")
      .and(
        expect.same(
          Right(r.original.expr),
          Parser.migrate(Parser.render(r.original).map(_.noSpaces).getOrElse("")).flatMap(Parser.parse(_)).map(_.expr)
        )
      )
  }

  check("constructor: the holders signed and the balances sum to the supply") { r =>
    val ok = call(
      """{"method": "constructor", "name": "Token", "symbol": "TOK", "totalSupply": 100, "balances": {"alice": 100}, "allowances": {}}""",
      List("alice")
    )
    val short = call(
      """{"method": "constructor", "name": "Token", "symbol": "TOK", "totalSupply": 99, "balances": {"alice": 100}, "allowances": {}}""",
      List("alice")
    )
    expect
      .same(
        updates("""[["name", "Token"], ["symbol", "TOK"], ["totalSupply", 100], ["balances", {"alice": 100}], ["allowances", {}]]"""),
        run(r.ported, ok).result
      )
      .and(expect.same(updates("\"Failed constructor checks\""), run(r.ported, short).result))
      .and(expect.same(run(r.original, ok).result, run(r.ported, ok).result))
  }

  check("transfer: computed balance paths, and the signer must be the sender") { r =>
    val data = call("""{"method": "transfer", "from": "alice", "to": "bob", "amount": 10}""", List("alice"))
    val unsigned = call("""{"method": "transfer", "from": "alice", "to": "bob", "amount": 10}""", List("bob"))
    val overdraw = call("""{"method": "transfer", "from": "bob", "to": "alice", "amount": 41}""", List("bob"))
    val outcome = run(r.ported, data)
    expect
      .same(updates("""[["content.balances.alice", 50], ["content.balances.bob", 50]]"""), outcome.result)
      .and(expect.same(Vector.empty[Warning], outcome.warnings))
      .and(expect.same(updates("\"Failed transfer checks\""), run(r.ported, unsigned).result))
      .and(expect.same(updates("\"Failed transfer checks\""), run(r.ported, overdraw).result))
      .and(expect.same(outcome.result, run(r.original, data).result))
  }

  check("transfer to a new holder: the original relied on null arithmetic, which strict JLVM rejects") { r =>
    val data = call("""{"method": "transfer", "from": "alice", "to": "dave", "amount": 10}""", List("alice"))
    val original = run(r.original, data).result
    expect
      .same(updates("""[["content.balances.alice", 50], ["content.balances.dave", 10]]"""), run(r.ported, data).result)
      .and(
        expect(
          original match {
            case Left(JlvmError.Type("+", _)) => true
            case _                            => false
          },
          s"null + 10 is a type error, got $original"
        )
      )
  }

  check("approve, then transferFrom within the allowance") { r =>
    val approve = call("""{"method": "approve", "from": "alice", "spender": "carol", "amount": 30}""", List("alice"))
    val spend = call("""{"method": "transferFrom", "from": "alice", "spender": "carol", "to": "bob", "amount": 25}""", List("carol"))
    val over = call("""{"method": "transferFrom", "from": "alice", "spender": "carol", "to": "bob", "amount": 26}""", List("carol"))
    expect
      .same(updates("""[["content.allowances.alice.carol", 30]]"""), run(r.ported, approve).result)
      .and(
        expect.same(
          updates("""[["content.balances.alice", 35], ["content.balances.bob", 65], ["content.allowances.alice.carol", 0]]"""),
          run(r.ported, spend).result
        )
      )
      .and(expect.same(updates("\"Failed transferFrom checks\""), run(r.ported, over).result))
      .and(expect.same(updates("\"Unknown method\""), run(r.ported, call("""{"method": "burn"}""", List("alice"))).result))
  }
}
