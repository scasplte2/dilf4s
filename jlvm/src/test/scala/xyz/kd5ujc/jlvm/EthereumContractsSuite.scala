package xyz.kd5ujc.jlvm

import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.accumulators.merkle.{MerkleCodec, MerkleTree}
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue.{CInt, CMap}
import xyz.kd5ujc.core.hash.Sha256

import org.scalacheck.Gen
import scodec.bits.ByteVector
import weaver.scalacheck.Checkers
import weaver.{Expectations, IOSuite}

import ContractHarness._

/** Ethereum-style contracts, written as JLVM state transitions (ContractHarness). */
object EthereumContractsSuite extends IOSuite with Checkers {

  type Res = Map[String, Contract]
  private val names = List("erc721", "multisig_wallet", "uniswap_v2", "vesting", "merkle_airdrop", "sealed_bid_auction", "dao")
  def sharedResource: Resource[IO, Res] =
    Resource.eval(Jlvm.load[IO].flatMap(jlvm => names.traverse(n => load(n).map(n -> Contract(jlvm, _))).map(_.toMap)))

  private def check(name: String)(f: Res => Expectations) = test(name)(r => IO(f(r)))
  private def sender(s:   String) = "sender" -> text(s)

  // ---- ERC-721

  private val nft0 = value("""{"minter": "mint", "owners": {}, "approvals": {}, "operators": {}, "balances": {}}""")

  check("ERC-721: the minter mints once; transfers by the owner, the approved address or an operator") { r =>
    val c = r("erc721")
    val minted = c.next(nft0, """{"method": "mint", "tokenId": "7", "to": "alice"}""", sender("mint"))
    val s1 = minted.fold(e => sys.error(s"mint: $e"), identity)
    val approved = c.next(s1, """{"method": "approve", "tokenId": "7", "to": "bob"}""", sender("alice")).toOption.get
    val byBob = c.next(approved, """{"method": "transferFrom", "from": "alice", "to": "carol", "tokenId": "7"}""", sender("bob"))
    val opAll = c
      .next(s1, """{"method": "setApprovalForAll", "operator": "dave", "approved": true}""", sender("alice"))
      .fold(e => sys.error(s"opAll: $e"), identity)
    val byDave = c.next(opAll, """{"method": "transferFrom", "from": "alice", "to": "erin", "tokenId": "7"}""", sender("dave"))
    val revoked = c.next(opAll, """{"method": "setApprovalForAll", "operator": "dave", "approved": false}""", sender("alice")).toOption.get
    expect
      .same(
        Right(value("""{"minter": "mint", "owners": {"7": "alice"}, "approvals": {}, "operators": {}, "balances": {"alice": 1}}""")),
        minted
      )
      .and(expect(rejected(c.next(s1, """{"method": "mint", "tokenId": "7", "to": "bob"}""", sender("mint"))), "a token mints once"))
      .and(expect(rejected(c.next(nft0, """{"method": "mint", "tokenId": "8", "to": "bob"}""", sender("bob"))), "only the minter mints"))
      .and(
        expect.same(
          Right(
            value(
              """{"minter": "mint", "owners": {"7": "carol"}, "approvals": {}, "operators": {}, "balances": {"alice": 0, "carol": 1}}"""
            )
          ),
          byBob
        )
      )
      .and(
        expect(
          rejected(
            c.next(byBob.toOption.get, """{"method": "transferFrom", "from": "carol", "to": "bob", "tokenId": "7"}""", sender("bob"))
          ),
          "the approval clears on transfer"
        )
      )
      .and(expect(byDave.isRight, s"an operator transfers: $byDave"))
      .and(
        expect(
          rejected(c.next(revoked, """{"method": "transferFrom", "from": "alice", "to": "erin", "tokenId": "7"}""", sender("dave"))),
          "a revoked operator cannot"
        )
      )
      .and(
        expect(
          rejected(c.next(s1, """{"method": "transferFrom", "from": "bob", "to": "erin", "tokenId": "7"}""", sender("bob"))),
          "from must be the owner"
        )
      )
      .and(
        expect(
          rejected(c.next(s1, """{"method": "approve", "tokenId": "9", "to": "bob"}""", sender("alice"))),
          "no approvals for unminted tokens"
        )
      )
      .and(
        expect(rejected(c.next(s1, """{"method": "approve", "tokenId": "7", "to": "eve"}""", sender("eve"))), "a stranger cannot approve")
      )
  }

  // ---- Gnosis-style m-of-n wallet: owners sign keccak256(wallet ‖ nonce ‖ to ‖ value ‖ data) with personal_sign

  private val keys = List(BigInt(101), BigInt(202), BigInt(303), BigInt(404))
  private val owners = keys.take(3).map(EthKeys.address)
  private val wallet = ByteVector.fill(20)(0x77)
  private def walletState(nonce: Int) =
    value(s"""{"wallet": "${EthKeys
        .hex(wallet)}", "owners": [${owners.map(a => s"\"${EthKeys.hex(a)}\"").mkString(", ")}], "threshold": 2, "nonce": $nonce}""")

  private val to = ByteVector.fill(20)(0x0b)
  private def digest(nonce: Int, value: Int): ByteVector =
    EthKeys.personal(
      EthKeys.keccak(
        wallet ++ ByteVector(BigInt(nonce).toByteArray).takeRight(32).padLeft(32) ++ to ++ ByteVector(BigInt(value).toByteArray)
          .takeRight(32)
          .padLeft(32)
      )
    )
  private def execute(value: Int, signers: List[BigInt], nonce: Int = 0) =
    s"""{"method": "execute", "to": "${EthKeys.hex(to)}", "value": $value, "data": "0x",
       | "signatures": [${signers.map(k => s"\"${EthKeys.hex(EthKeys.sign(k, digest(nonce, value)))}\"").mkString(", ")}]}""".stripMargin

  check("multisig wallet: 2 of 3 distinct owners, over this nonce and these parameters") { r =>
    val c = r("multisig_wallet")
    val two = c.next(walletState(0), execute(5, keys.slice(0, 2)))
    expect
      .same(Right(walletState(1)), two)
      .and(
        expect.same(
          Right(value(s"""[{"kind": "call", "to": "${EthKeys.hex(to)}", "value": 5, "data": "0x"}]""")),
          c.effects(walletState(0), execute(5, keys.slice(0, 2)))
        )
      )
      .and(expect(rejected(c.next(walletState(0), execute(5, List(keys(0), keys(0))))), "one owner twice is one signer"))
      .and(expect(rejected(c.next(walletState(0), execute(5, List(keys(0), keys(3))))), "a non-owner does not count"))
      .and(expect(rejected(c.next(walletState(1), execute(5, keys.slice(0, 2)))), "signatures for nonce 0 do not replay at nonce 1"))
      .and(expect(rejected(c.next(walletState(0), execute(5, keys.slice(0, 2)).replace("\"value\": 5", "\"value\": 6"))), "another value"))
      .and(expect.same(Right(walletState(1)), c.next(walletState(0), execute(5, keys.slice(0, 3)))))
      .and(
        expect(
          c.run(walletState(0), execute(5, keys.slice(0, 2)).replaceFirst("\"0x[0-9a-f]{130}\"", s"\"0x${"00" * 32}${"11" * 32}1b\""))
            .result
            .isLeft,
          "an unrecoverable signature fails the call: it is never silently skipped or read as an address"
        )
      )
  }

  private def at(v: CanonicalValue, k: String): CanonicalValue = v match {
    case CMap(m) => m.getOrElse(k, CanonicalValue.CNull)
    case _       => CanonicalValue.CNull
  }
  private def int(v: CanonicalValue): BigInt = v match {
    case CInt(i) => i
    case other   => sys.error(s"not an integer: $other")
  }

  // ---- Uniswap v2: constant product, 0.3% fee

  private val pool0 = value("""{"reserve0": 0, "reserve1": 0, "totalSupply": 0, "lp": {}}""")

  /** Uniswap v2's getAmountOut, in integers. */
  private def amountOut(in: BigInt, rin: BigInt, rout: BigInt): BigInt = in * 997 * rout / (rin * 1000 + in * 997)

  check("Uniswap v2: liquidity, quotes as getAmountOut, slippage limits, proportional withdrawal") { r =>
    val c = r("uniswap_v2")
    val seeded = c.next(pool0, """{"method": "addLiquidity", "amount0": 1000000, "amount1": 4000000}""", sender("lp1")).toOption.get
    val swapped =
      c.next(seeded, """{"method": "swap", "zeroForOne": true, "amountIn": 10000, "minOut": 0}""", sender("trader")).toOption.get
    val out = amountOut(10000, 1000000, 4000000)
    val second = c.next(swapped, """{"method": "addLiquidity", "amount0": 101000, "amount1": 400000}""", sender("lp2")).toOption.get
    val removed = c.next(second, """{"method": "removeLiquidity", "liquidity": 500000}""", sender("lp1"))
    expect
      .same(BigInt(4000000) - out, int(at(swapped, "reserve1")))
      .and(expect.same(BigInt(1010000), int(at(swapped, "reserve0"))))
      .and(
        expect(
          rejected(c.next(seeded, s"""{"method": "swap", "zeroForOne": true, "amountIn": 10000, "minOut": ${out + 1}}""", sender("t"))),
          "slippage"
        )
      )
      .and(
        expect(
          rejected(c.next(seeded, """{"method": "swap", "zeroForOne": true, "amountIn": 0.5, "minOut": 0}""", sender("t"))),
          "whole amounts"
        )
      )
      .and(
        expect.same((BigInt(101000) * 1000000 / 1010000).min(BigInt(400000) * 1000000 / (4000000 - out)), int(at(at(second, "lp"), "lp2")))
      )
      .and(expect(removed.isRight, s"lp1 withdraws: $removed"))
      .and(expect(rejected(c.next(second, """{"method": "removeLiquidity", "liquidity": 1}""", sender("stranger"))), "only LP units held"))
  }

  test("Uniswap v2: any sequence of swaps never decreases k = reserve0 · reserve1") { r =>
    val c = r("uniswap_v2")
    val seeded = c.next(pool0, """{"method": "addLiquidity", "amount0": 5000000, "amount1": 5000000}""", sender("lp")).toOption.get
    forall(Gen.listOfN(12, Gen.zip(Gen.oneOf(true, false), Gen.choose(1L, 2000000L)))) { swaps =>
      val states = swaps.scanLeft(seeded) {
        case (s, (dir, in)) =>
          c.next(s, s"""{"method": "swap", "zeroForOne": $dir, "amountIn": $in, "minOut": 0}""", sender("t")).getOrElse(s)
      }
      val ks = states.map(s => int(at(s, "reserve0")) * int(at(s, "reserve1")))
      expect(ks.zip(ks.drop(1)).forall { case (a, b) => b >= a }, s"k fell: $ks")
    }
  }

  // ---- vesting: a cliff, then linear

  private val grant = value("""{"beneficiary": "ann", "start": 1000, "cliff": 1250, "duration": 1000, "total": 3000, "released": 0}""")
  private def at(t: Long) = "timestamp" -> CInt(BigInt(t))

  check("vesting: nothing before the cliff, linear (floored) after it, everything at the end, never twice") { r =>
    val c = r("vesting")
    val call = """{"method": "release"}"""
    val mid = c.next(grant, call, at(1333))
    val end = mid.flatMap(m => c.next(m, call, at(5000)))
    expect(rejected(c.next(grant, call, at(1249))), "before the cliff")
      .and(expect.same(Right(BigInt(999)), mid.map(m => int(at(m, "released")))))
      .and(expect.same(Right(value("""[{"kind": "transfer", "to": "ann", "amount": 999}]""")), c.effects(grant, call, at(1333))))
      .and(expect.same(Right(BigInt(3000)), end.map(e => int(at(e, "released")))))
      .and(expect(rejected(end.flatMap(e => c.next(e, call, at(6000)))), "all released"))
      .and(expect(rejected(mid.flatMap(m => c.next(m, call, at(1333)))), "nothing new at the same time"))
  }

  // ---- Merkle airdrop

  check("Merkle airdrop: each listed (index, account, amount) claims once; anything else is refused") { r =>
    val c = r("merkle_airdrop")
    val entries = (0 until 5).toList.map(i => value(s"""[$i, "acct$i", ${(i + 1) * 100}]"""))
    val tree = MerkleTree.fromLeaves[Sha256, CanonicalValue](entries).toOption.get
    def proof(i: Int) = ContractHarness.render(tree.inclusionProof(i.toLong).flatMap(MerkleCodec.encodeInclusion(_)).toOption.get)
    val state = value(s"""{"head": {"root": "${EthKeys.hex(tree.root.bytes)}", "size": ${tree.size}}, "claimed": {}}""")
    def claim(i: Int, account: String, amount: Int, p: Int = -1) =
      s"""{"method": "claim", "index": $i, "account": "$account", "amount": $amount, "proof": ${proof(if (p < 0) i else p)}}"""
    val first = c.next(state, claim(3, "acct3", 400))
    expect(first.isRight, s"a listed claim: $first")
      .and(expect(rejected(first.flatMap(s => c.next(s, claim(3, "acct3", 400)))), "claimed once"))
      .and(expect(rejected(c.next(state, claim(3, "acct3", 4000))), "another amount"))
      .and(expect(rejected(c.next(state, claim(3, "mallory", 400))), "another account"))
      .and(expect(rejected(c.next(state, claim(3, "acct3", 400, p = 2))), "another index's proof"))
  }

  // ---- sealed-bid auction: commit keccak256(amount ‖ salt), then reveal

  private val auction0 = value(
    """{"seller": "sam", "item": "lot-1", "commitEnd": 100, "revealEnd": 200, "commits": {}, "highestBid": 0, "highestBidder": "", "settled": false}"""
  )
  private def commitment(amount: Int, salt: ByteVector) =
    EthKeys.keccak(ByteVector(BigInt(amount).toByteArray).takeRight(32).padLeft(32) ++ salt)

  check("sealed-bid auction: commitments hide bids, reveals must match them, the highest revealed bid wins") { r =>
    val c = r("sealed_bid_auction")
    val sa = ByteVector.fill(32)(1)
    val sb = ByteVector.fill(32)(2)
    def commit(a: Int, salt: ByteVector) = s"""{"method": "commit", "commitment": "${EthKeys.hex(commitment(a, salt))}"}"""
    def reveal(a: Int, salt: ByteVector) = s"""{"method": "reveal", "amount": $a, "salt": "${EthKeys.hex(salt)}"}"""
    val committed = for {
      s1 <- c.next(auction0, commit(70, sa), sender("ann"), at(10))
      s2 <- c.next(s1, commit(90, sb), sender("bob"), at(20))
    } yield s2
    val revealed = for {
      s2 <- committed
      s3 <- c.next(s2, reveal(70, sa), sender("ann"), at(150))
      s4 <- c.next(s3, reveal(90, sb), sender("bob"), at(160))
    } yield s4
    expect(revealed.map(s => (int(at(s, "highestBid")), at(s, "highestBidder"))) == Right((BigInt(90), text("bob"))), s"$revealed")
      .and(expect(rejected(committed.flatMap(s => c.next(s, reveal(80, sa), sender("ann"), at(150)))), "a bid that does not match"))
      .and(
        expect(rejected(committed.flatMap(s => c.next(s, reveal(70, sa), sender("ann"), at(50)))), "reveal before the commit phase ends")
      )
      .and(expect(rejected(c.next(auction0, commit(70, sa), sender("ann"), at(100))), "commit after the commit phase"))
      .and(
        expect.same(
          Right(value("""[{"kind": "transfer", "to": "sam", "amount": 90}, {"kind": "deliver", "to": "bob", "item": "lot-1"}]""")),
          revealed.flatMap(s => c.effects(s, """{"method": "settle"}""", sender("anyone"), at(200)))
        )
      )
  }

  // ---- DAO: weighted votes, quorum, execution after the deadline

  private val dao0 = value("""{"weights": {"a": 40, "b": 35, "c": 25}, "quorum": 50, "nextId": 0, "proposals": {}}""")

  check("DAO: members propose and vote once by weight; a passed proposal with a quorum executes once, after its deadline") { r =>
    val c = r("dao")
    val action = """{"kind": "call", "target": "treasury", "amount": 10}"""
    val flow = for {
      p  <- c.next(dao0, s"""{"method": "propose", "action": $action, "duration": 100}""", sender("a"), at(0))
      v1 <- c.next(p, """{"method": "vote", "id": 0, "support": true}""", sender("a"), at(10))
      v2 <- c.next(v1, """{"method": "vote", "id": 0, "support": false}""", sender("c"), at(20))
    } yield (p, v1, v2)
    val p = flow.map(_._1)
    val v1 = flow.map(_._2)
    val v2 = flow.map(_._3)
    expect(flow.isRight, s"$flow")
      .and(
        expect(rejected(v1.flatMap(s => c.next(s, """{"method": "vote", "id": 0, "support": true}""", sender("a"), at(30)))), "vote once")
      )
      .and(
        expect(rejected(p.flatMap(s => c.next(s, """{"method": "vote", "id": 0, "support": true}""", sender("z"), at(30)))), "members only")
      )
      .and(
        expect(rejected(v2.flatMap(s => c.next(s, """{"method": "execute", "id": 0}""", sender("a"), at(99)))), "not before the deadline")
      )
      .and(
        expect(
          rejected(v1.flatMap(s => c.next(s, """{"method": "execute", "id": 0}""", sender("a"), at(100)))),
          "40 votes miss a quorum of 50"
        )
      )
      .and(
        expect.same(Right(value(s"[$action]")), v2.flatMap(s => c.effects(s, """{"method": "execute", "id": 0}""", sender("x"), at(100))))
      )
      .and(
        expect(
          rejected(
            v2.flatMap(s => c.next(s, """{"method": "execute", "id": 0}""", sender("x"), at(100)))
              .flatMap(s => c.next(s, """{"method": "execute", "id": 0}""", sender("x"), at(101)))
          ),
          "execute once"
        )
      )
  }
}
