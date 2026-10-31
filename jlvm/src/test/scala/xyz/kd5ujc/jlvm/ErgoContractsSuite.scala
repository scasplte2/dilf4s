package xyz.kd5ujc.jlvm

import cats.effect.std.SecureRandom
import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue.{CBytes, CInt, CMap}
import xyz.kd5ujc.core.hash.{Blake2b256, Hasher}
import xyz.kd5ujc.crypto.sigma.{GroupElement, Secp256k1Group, SigmaProp, SigmaProver}

import scodec.bits.ByteVector
import weaver.IOSuite

import ContractHarness._

/**
 * Ergo-style contracts as JLVM state transitions (ContractHarness): spending conditions are sigma propositions built
 * by the program from its own state, proven over the transaction (`$txHash`), as docs/specs/sigma.md section 8 asks.
 */
object ErgoContractsSuite extends IOSuite {

  final case class Env(contracts: Map[String, Contract], random: SecureRandom[IO])
  type Res = Env
  private val names = List("crowdfunding", "oracle_pool", "sigmausd_bank", "stealth_address", "auction_house", "threshold_treasury")
  def sharedResource: Resource[IO, Res] =
    Resource.eval(
      (
        Jlvm.load[IO].flatMap(jlvm => names.traverse(n => load(n).map(n -> Contract(jlvm, _))).map(_.toMap)),
        SecureRandom.javaSecuritySecureRandom[IO]
      )
        .mapN(Env.apply)
    )

  private def pk(w:   BigInt): ByteVector = Secp256k1Group.base(w).bytes
  private def h(b:    ByteVector): String = "\"0x" + b.toHex + "\""
  private def dlog(w: BigInt): SigmaProp = SigmaProp.dlog(Secp256k1Group.base(w)).fold(sys.error, identity)

  private def prove(r: SecureRandom[IO], prop: SigmaProp, secrets: Vector[BigInt], message: ByteVector): IO[ByteVector] =
    SigmaProver.prove[IO](prop, secrets, message)(using summon, r).flatMap(e => IO.fromEither(e.leftMap(new IllegalStateException(_))))

  private val tx = ByteVector.fill(32)(0x51)
  private val otherTx = ByteVector.fill(32)(0x52)
  private def at(t: Long, txHash: ByteVector = tx) = List("ordinal" -> CInt(BigInt(t)), "txHash" -> CBytes(txHash))

  private def field(v: CanonicalValue, k: String): CanonicalValue = v match {
    case CMap(m) => m.getOrElse(k, CanonicalValue.CNull)
    case _       => CanonicalValue.CNull
  }

  // ---- crowdfunding (the Ergo whitepaper's example)

  test("crowdfunding: the project claims once the goal is met; backers refund after a missed deadline") { e =>
    val c = e.contracts("crowdfunding")
    val (project, b1, b2) = (BigInt(11), BigInt(21), BigInt(22))
    val start = value(s"""{"project": ${h(pk(project))}, "goal": 100, "deadline": 50, "pledges": {}, "claimed": false}""")
    def pledge(w: BigInt, amount: Int) = s"""{"method": "pledge", "backer": ${h(pk(w))}, "amount": $amount}"""
    def claim(p:  ByteVector) = s"""{"method": "claim", "proof": ${h(p)}}"""
    def refund(w: BigInt, p:      ByteVector) = s"""{"method": "refund", "backer": ${h(pk(w))}, "proof": ${h(p)}}"""
    val funded = c.next(start, pledge(b1, 60), at(10)*).flatMap(s => c.next(s, pledge(b2, 50), at(20)*))
    val short = c.next(start, pledge(b1, 60), at(10)*)
    (
      prove(e.random, dlog(project), Vector(project), tx),
      prove(e.random, dlog(b1), Vector(b1), tx),
      prove(e.random, dlog(b2), Vector(b2), tx)
    ).mapN { (pp, p1, p2) =>
      val claimed = funded.flatMap(s => c.next(s, claim(pp), at(30)*))
      expect(claimed.map(field(_, "claimed")) == Right(CanonicalValue.True), s"$claimed")
        .and(
          expect.same(
            Right(value(s"""[{"kind": "transfer", "to": ${h(pk(project))}, "amount": 110}]""")),
            funded.flatMap(s => c.effects(s, claim(pp), at(30)*))
          )
        )
        .and(expect(rejected(claimed.flatMap(s => c.next(s, claim(pp), at(31)*))), "claim once"))
        .and(expect(rejected(funded.flatMap(s => c.next(s, claim(p1), at(30)*))), "only the project's key"))
        .and(expect(rejected(funded.flatMap(s => c.next(s, claim(pp), at(30, otherTx)*))), "the proof signs this transaction"))
        .and(expect(rejected(short.flatMap(s => c.next(s, claim(pp), at(60)*))), "not below the goal"))
        .and(
          expect.same(
            Right(value(s"""[{"kind": "transfer", "to": ${h(pk(b1))}, "amount": 60}]""")),
            short.flatMap(s => c.effects(s, refund(b1, p1), at(60)*))
          )
        )
        .and(expect(rejected(short.flatMap(s => c.next(s, refund(b1, p1), at(40)*))), "no refunds before the deadline"))
        .and(expect(rejected(short.flatMap(s => c.next(s, refund(b1, p2), at(60)*))), "only the backer refunds"))
        .and(expect(rejected(funded.flatMap(s => c.next(s, refund(b1, p1), at(60)*))), "no refunds once funded"))
        .and(expect(rejected(c.next(start, pledge(b1, 5), at(50)*)), "pledges close at the deadline"))
    }
  }

  // ---- oracle pool: k of n signed datapoints per epoch, aggregated by the median

  test("oracle pool: the lower median of at least k valid datapoints for the next epoch, one per oracle") { e =>
    val c = e.contracts("oracle_pool")
    val oracles = Vector(BigInt(31), BigInt(32), BigInt(33), BigInt(34))
    val state = value(
      s"""{"oracles": [${oracles.map(w => h(pk(w))).mkString(", ")}], "minDatapoints": 3, "epoch": 0, "rate": 0,
         | "epochLength": 60, "lastUpdate": 0}""".stripMargin
    )
    def msg(epoch: Int, v: Int) =
      Hasher[Blake2b256]
        .digest(
          ByteVector(BigInt(epoch).toByteArray).takeRight(8).padLeft(8) ++ ByteVector(BigInt(v).toByteArray).takeRight(32).padLeft(32)
        )
        .bytes
    def point(w: BigInt, v: Int, epoch: Int = 1): IO[String] =
      prove(e.random, dlog(w), Vector(w), msg(epoch, v)).map(p => s"""{"oracle": ${h(pk(w))}, "value": $v, "proof": ${h(p)}}""")
    def update(ps: List[String]) = s"""{"method": "update", "datapoints": [${ps.mkString(", ")}]}"""
    (
      List(31 -> 100, 32 -> 102, 33 -> 98, 34 -> 105).traverse { case (w, v) => point(BigInt(w), v) },
      point(BigInt(34), 105, epoch = 2),
      point(BigInt(99), 1)
    ).mapN { (four, stale, outsider) =>
      val updated = c.next(state, update(four), at(60)*)
      expect
        .same(Right(CInt(BigInt(100))), updated.map(field(_, "rate")))
        .and(expect.same(Right(CInt(BigInt(1))), updated.map(field(_, "epoch"))))
        .and(expect(rejected(c.next(state, update(four), at(59)*)), "not before the epoch ends"))
        .and(expect(rejected(c.next(state, update(four.take(2) :+ stale), at(60)*)), "a datapoint for another epoch is invalid"))
        .and(expect(rejected(c.next(state, update(four.take(2) :+ outsider), at(60)*)), "a non-oracle's datapoint is invalid"))
        .and(expect(rejected(c.next(state, update(four.take(2) :+ four.head), at(60)*)), "one datapoint per oracle"))
        .and(expect.same(Right(CInt(BigInt(100))), c.next(state, update(four.take(3) :+ outsider), at(60)*).map(field(_, "rate"))))
    }
  }

  // ---- SigmaUSD (AgeUSD): a reserve-ratio band of 400% to 800%, a 2% fee, exact rationals with ceil and floor

  test("SigmaUSD bank: mint and redeem prices, fees, and the reserve-ratio band") { e =>
    IO {
      val c = e.contracts("sigmausd_bank")
      val rate = "rate" -> CInt(BigInt(2000000))
      def bank(r: Long, s: Long, rc: Long) =
        value(s"""{"reserve": $r, "stable": $s, "reserveCoins": $rc, "feePercent": 2, "defaultReservePrice": 1000000}""")
      def op(m: String, n: BigDecimal) = s"""{"method": "$m", "amount": $n}"""
      val s1 = c.next(bank(0, 0, 0), op("mintReserve", 1000), rate)
      val s2 = s1.flatMap(s => c.next(s, op("mintStable", 100), rate))
      val s3 = s2.flatMap(s => c.next(s, op("mintStable", 60), rate))
      expect
        .same(Right(bank(1020000000L, 0, 1000)), s1)
        .and(expect.same(Right(bank(1224000000L, 100, 1000)), s2))
        .and(expect.same(Right(bank(1346400000L, 160, 1000)), s3))
        .and(expect(rejected(s3.flatMap(s => c.next(s, op("mintStable", 20), rate))), "below 400%"))
        .and(expect.same(Right(bank(1537344000L, 100, 1300)), s2.flatMap(s => c.next(s, op("mintReserve", 300), rate))))
        .and(expect(rejected(s2.flatMap(s => c.next(s, op("mintReserve", 400), rate))), "above 800%"))
        .and(expect.same(Right(bank(1126000000L, 50, 1000)), s2.flatMap(s => c.next(s, op("redeemStable", 50), rate))))
        .and(expect(rejected(s3.flatMap(s => c.next(s, op("redeemReserve", 100), rate))), "redeeming reservecoins below 400%"))
        .and(expect.same(Right(bank(1336341280L, 160, 990)), s3.flatMap(s => c.next(s, op("redeemReserve", 10), rate))))
        .and(expect(rejected(c.next(bank(0, 0, 0), op("mintReserve", BigDecimal("0.5")), rate)), "whole amounts"))
    }
  }

  // ---- stealth addresses: the sender re-randomises the receiver's (G, U, Y, yU) by r; the receiver proves the tuple

  test("stealth address: only the receiver can spend, and two payments to them share no public point") { e =>
    val c = e.contracts("stealth_address")
    val y = BigInt(777)
    val g = Secp256k1Group.Generator
    val u = Secp256k1Group.base(BigInt(555))
    def box(r: BigInt): (GroupElement, GroupElement, GroupElement, GroupElement) = (g * r, u * r, g * (y * r), u * (y * r))
    def boxJson(b: (GroupElement, GroupElement, GroupElement, GroupElement)) =
      s"""{"g": ${h(b._1.bytes)}, "h": ${h(b._2.bytes)}, "u": ${h(b._3.bytes)}, "v": ${h(b._4.bytes)}}"""
    def statement(b: (GroupElement, GroupElement, GroupElement, GroupElement)) =
      SigmaProp.dhTuple(b._1, b._2, b._3, b._4).fold(sys.error, identity)
    val (b1, b2) = (box(BigInt(1001)), box(BigInt(2002)))
    def spend(b: (GroupElement, GroupElement, GroupElement, GroupElement), p: ByteVector) =
      s"""{"method": "spend", "box": ${boxJson(b)}, "proof": ${h(p)}}"""
    (prove(e.random, statement(b1), Vector(y), tx), prove(e.random, statement(b1), Vector(BigInt(778)), tx).attempt).mapN {
      (proof, outsider) =>
        val points1 = List(b1._1, b1._2, b1._3, b1._4).map(_.bytes)
        val points2 = List(b2._1, b2._2, b2._3, b2._4).map(_.bytes)
        expect(c.next(value("{}"), spend(b1, proof), at(0)*).isRight, "the receiver spends")
          .and(expect(outsider.isLeft, "nobody else can prove the tuple"))
          .and(expect(rejected(c.next(value("{}"), spend(b1, proof), at(0, otherTx)*)), "the proof signs this transaction"))
          .and(expect(rejected(c.next(value("{}"), spend(b2, proof), at(0)*)), "a proof for one box does not spend another"))
          .and(expect(points1.intersect(points2).isEmpty, "unlinkable: no shared point"))
          .and(expect(b1._3 == b1._1 * y && b2._3 == b2._1 * y, "the receiver recognises their boxes: u = y·g"))
    }
  }

  // ---- auction house: signed bids, a minimum step, extension near the end

  test("auction house: bids rise by the minimum step, late bids extend the end, the outbid are refunded") { e =>
    val c = e.contracts("auction_house")
    val (seller, a, b) = (BigInt(41), BigInt(42), BigInt(43))
    val start = value(
      s"""{"seller": ${h(pk(seller))}, "item": "lot-9", "reserve": 100, "minStepPercent": 5, "end": 1000, "extendWindow": 60,
         | "extendBy": 120, "bid": 0, "bidder": "", "settled": false}""".stripMargin
    )
    def bid(w: BigInt, amount: Int, p: ByteVector) = s"""{"method": "bid", "bidder": ${h(pk(w))}, "amount": $amount, "proof": ${h(p)}}"""
    (prove(e.random, dlog(a), Vector(a), tx), prove(e.random, dlog(b), Vector(b), tx)).mapN { (pa, pb) =>
      val first = c.next(start, bid(a, 100, pa), at(100)*)
      val late = first.flatMap(s => c.next(s, bid(b, 105, pb), at(950)*))
      expect(first.isRight, s"$first")
        .and(expect.same(Right(value("[]")), c.effects(start, bid(a, 100, pa), at(100)*)))
        .and(expect(rejected(first.flatMap(s => c.next(s, bid(b, 104, pb), at(200)*))), "at least 5% more"))
        .and(expect(rejected(first.flatMap(s => c.next(s, bid(b, 105, pa), at(200)*))), "the bidder's own proof"))
        .and(expect.same(Right(CInt(BigInt(1120))), late.map(field(_, "end"))))
        .and(
          expect.same(
            Right(value(s"""[{"kind": "transfer", "to": ${h(pk(a))}, "amount": 100}]""")),
            first.flatMap(s => c.effects(s, bid(b, 105, pb), at(950)*))
          )
        )
        .and(expect(rejected(late.flatMap(s => c.next(s, """{"method": "settle"}""", at(1000)*))), "the end moved"))
        .and(
          expect.same(
            Right(value(s"""[{"kind": "transfer", "to": ${h(pk(seller))}, "amount": 105}, {"kind": "deliver", "to": ${h(
                pk(b)
              )}, "item": "lot-9"}]""")),
            late.flatMap(s => c.effects(s, """{"method": "settle"}""", at(1120)*))
          )
        )
        .and(expect(rejected(c.next(start, bid(a, 99, pa), at(100)*)), "at least the reserve"))
    }
  }

  // ---- a 2-of-3 committee treasury, whose proofs do not reveal which two signed

  test("threshold treasury: any 2 of 3 committee members sign a spend at this nonce; one is not enough") { e =>
    val c = e.contracts("threshold_treasury")
    val ws = Vector(BigInt(51), BigInt(52), BigInt(53))
    val treasury = ByteVector.fill(8)(0x7e)
    val to = ByteVector.fill(20)(0x0c)
    def state(nonce: Int) =
      value(s"""{"treasury": ${h(treasury)}, "signers": [${ws.map(w => h(pk(w))).mkString(", ")}], "k": 2, "nonce": $nonce}""")
    def message(nonce: Int, amount: Int) =
      Hasher[Blake2b256]
        .digest(
          treasury ++ ByteVector(BigInt(nonce).toByteArray).takeRight(8).padLeft(8) ++ to ++ ByteVector(BigInt(amount).toByteArray)
            .takeRight(32)
            .padLeft(32)
        )
        .bytes
    val committee = SigmaProp.threshold(2, ws.map(dlog)).fold(sys.error, identity)
    def spend(amount: Int, p: ByteVector) = s"""{"method": "spend", "to": ${h(to)}, "amount": $amount, "proof": ${h(p)}}"""
    (
      prove(e.random, committee, ws.take(2), message(0, 40)),
      prove(e.random, committee, ws.drop(1), message(0, 40)),
      prove(e.random, committee, ws.take(1), message(0, 40)).attempt
    ).mapN { (p12, p23, alone) =>
      expect
        .same(Right(state(1)), c.next(state(0), spend(40, p12)))
        .and(expect.same(Right(state(1)), c.next(state(0), spend(40, p23))))
        .and(expect(alone.isLeft, "one member cannot prove 2 of 3"))
        .and(expect(rejected(c.next(state(1), spend(40, p12))), "no replay at the next nonce"))
        .and(expect(rejected(c.next(state(0), spend(41, p12))), "another amount"))
        .and(expect.same(p12.size, p23.size))
    }
  }
}
