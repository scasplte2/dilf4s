package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8

import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue

import weaver.{Expectations, IOSuite}

/**
 * Example contracts from the projects JLVM replaces, run end to end. `proofchain-fungible.json` is proofchain's ERC-20
 * (`e2e-test/resources/fungible.json` at 28b2d9d), verbatim; `fungible.json` is its port to strict semantics. The
 * counter is ottochain's counter script as its author first wrote it, before `count` had to be renamed. The
 * `metakit_*.json` programs are metakit's ComplexContractsSuite contracts (escrow, voting, marketplace) at de2a856,
 * verbatim: they are strict JLVM as written.
 */
object ContractsSuite extends IOSuite {

  final case class Programs(jlvm: Jlvm, original: Program, ported: Program, escrow: Program, voting: Program, marketplace: Program)
  type Res = Programs

  private def load(name: String): IO[Program] =
    Resource
      .fromAutoCloseable(IO.blocking(getClass.getResourceAsStream(s"/programs/$name")))
      .use(in => IO.blocking(new String(in.readAllBytes(), UTF_8)))
      .flatMap(text => IO.fromEither(Parser.parse(text).leftMap(e => new IllegalArgumentException(e.message))))

  def sharedResource: Resource[IO, Programs] =
    Resource.eval(
      (
        Jlvm.load[IO],
        load("proofchain-fungible.json"),
        load("fungible.json"),
        load("metakit_escrow.json"),
        load("metakit_voting.json"),
        load("metakit_marketplace.json")
      ).mapN(Programs.apply)
    )

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

  check("the ERC-20 is strict JLVM as written; its port differs only in reading absent balances and allowances as 0") { r =>
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

  check("absent balances and allowances: the original errors where metakit rejected; the port rejects") { r =>
    val unfunded = call("""{"method": "transfer", "from": "erin", "to": "bob", "amount": 1}""", List("erin"))
    val noAllowance = call("""{"method": "transferFrom", "from": "bob", "spender": "carol", "to": "alice", "amount": 1}""", List("carol"))
    def typeError(o: Outcome) = o.result match {
      case Left(JlvmError.Type(">=", _)) => true
      case _                             => false
    }
    expect
      .same(updates("\"Failed transfer checks\""), run(r.ported, unfunded).result)
      .and(expect.same(updates("\"Failed transferFrom checks\""), run(r.ported, noAllowance).result))
      .and(expect(typeError(run(r.original, unfunded)), "null >= 1 is a type error"))
      .and(expect(typeError(run(r.original, noAllowance)), "null >= 1 is a type error"))
  }

  // ---- ottochain's counter, as first written (decision A1)

  private val counter =
    """{"if": [
      |  {"==": [{"var": "method"}, "increment"]}, {"merge": [{"var": "state"}, %s]},
      |  {"==": [{"var": "method"}, "reset"]}, %s,
      |  {"var": "state"}]}""".stripMargin

  check("{\"count\": ...} as data now fails loudly; {\"object\": ...} says what was meant") { _ =>
    val data = value("""{"method": "increment", "state": {"count": 4}}""")
    val written = Parser.parse(counter.format("""{"count": {"+": [{"var": "state.count"}, 1]}}""", """{"count": 0}"""))
    val fixed =
      Parser.parse(counter.format("""{"object": {"count": {"+": [{"var": "state.count"}, 1]}}}""", """{"object": {"count": 0}}"""))
    val renamed = Parser.migrate(counter.format("""{"value": {"+": [{"var": "state.count"}, 1]}}""", """{"value": 0}"""))
    expect(written.flatMap(Compiled(_)).isLeft || written.map(run(_, data).result.isLeft).getOrElse(true), "count as data is an error")
      .and(expect.same(updates("""{"count": 5}"""), fixed.map(run(_, data)).flatMap(_.result)))
      .and(expect.same(updates("""{"count": 4, "value": 5}"""), renamed.flatMap(Parser.parse(_)).map(run(_, data)).flatMap(_.result)))
  }

  // ---- metakit's ComplexContractsSuite: escrow, voting and marketplace, with metakit's expected results

  private val participants = """"participants": ["Alice", "Bob", "Charlie"]"""

  private def escrowState(status: String, deposited: Int, extra: String = ""): String =
    s"""{"buyer": "Alice", "seller": "Bob", "arbiter": "Charlie", "amount": 1000, "status": "$status", "deposited": $deposited$extra}"""

  check("metakit escrow: create, deposit, release by buyer or arbiter, refund; release before deposit fails") { r =>
    val pending = escrowState("pending", 0)
    val deposited = escrowState("deposited", 1000)
    val released = updates(escrowState("completed", 1000, """, "releasedTo": "Bob""""))
    expect
      .same(
        updates(pending),
        run(
          r.escrow,
          value(s"""{"method": "create", "buyer": "Alice", "seller": "Bob", "arbiter": "Charlie", "amount": 1000, $participants}""")
        ).result
      )
      .and(
        expect.same(
          updates(deposited),
          run(
            r.escrow,
            value(s"""{"method": "deposit", "depositor": "Alice", "depositAmount": 1000, $participants, "content": $pending}""")
          ).result
        )
      )
      .and(
        expect.same(
          released,
          run(r.escrow, value(s"""{"method": "release", "releaser": "Alice", $participants, "content": $deposited}""")).result
        )
      )
      .and(
        expect.same(
          released,
          run(r.escrow, value(s"""{"method": "release", "releaser": "Charlie", $participants, "content": $deposited}""")).result
        )
      )
      .and(
        expect.same(
          updates(escrowState("refunded", 1000, """, "refundedTo": "Alice"""")),
          run(r.escrow, value(s"""{"method": "refund", "refunder": "Bob", $participants, "content": $deposited}""")).result
        )
      )
      .and(
        expect.same(
          updates("\"Failed release checks\""),
          run(r.escrow, value(s"""{"method": "release", "releaser": "Alice", $participants, "content": $pending}""")).result
        )
      )
  }

  check("metakit voting: create a proposal and cast a vote; a vote after the deadline fails") { r =>
    val options = """["Approve", "Reject", "Defer"]"""
    val open = s"""{"title": "Project Funding", "options": $options, "voteCount": 0, "endTime": 2000, "active": true}"""
    def vote(at: Int) =
      value(s"""{"method": "vote", "voter": "Alice", "option": "Approve", "currentTime": $at, $participants, "content": $open}""")
    expect
      .same(
        updates(open),
        run(
          r.voting,
          value(s"""{"method": "create", "title": "Project Funding", "options": $options, "currentTime": 1000, "endTime": 2000}""")
        ).result
      )
      .and(
        expect.same(
          updates(
            s"""{"title": "Project Funding", "options": $options, "voteCount": 1, "lastVoter": "Alice", "lastOption": "Approve",
               | "endTime": 2000, "active": true}""".stripMargin
          ),
          run(r.voting, vote(1500)).result
        )
      )
      .and(expect.same(updates("\"Failed vote checks\""), run(r.voting, vote(2500)).result))
  }

  check("metakit marketplace: list, offer, accept, cancel; an offer on one's own listing fails") { r =>
    val item = """"itemId": "item-001", "seller": "Alice", "price": 500, "description": "Vintage bicycle""""
    val active = s"""{$item, "status": "active", "offerCount": 0}"""
    val offered = s"""{$item, "status": "active", "offerCount": 1, "lastOfferBuyer": "Bob", "lastOfferPrice": 450}"""
    def offer(buyer: String) =
      value(s"""{"method": "makeOffer", "buyer": "$buyer", "offerPrice": 450, $participants, "content": $active}""")
    expect
      .same(updates(active), run(r.marketplace, value(s"""{"method": "createListing", $item, $participants}""")).result)
      .and(expect.same(updates(offered), run(r.marketplace, offer("Bob")).result))
      .and(
        expect.same(
          updates(s"""{$item, "status": "sold", "offerCount": 1, "acceptedBuyer": "Bob", "acceptedPrice": 450}"""),
          run(
            r.marketplace,
            value(s"""{"method": "acceptOffer", "acceptor": "Alice", "offerIndex": 0, $participants, "content": $offered}""")
          ).result
        )
      )
      .and(
        expect.same(
          updates(s"""{$item, "status": "cancelled", "offerCount": 0}"""),
          run(r.marketplace, value(s"""{"method": "cancelListing", "canceller": "Alice", $participants, "content": $active}""")).result
        )
      )
      .and(expect.same(updates("\"Failed makeOffer checks\""), run(r.marketplace, offer("Alice")).result))
  }
}
