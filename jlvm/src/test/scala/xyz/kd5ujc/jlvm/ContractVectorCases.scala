package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.std.SecureRandom
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import xyz.kd5ujc.accumulators.merkle.{MerkleCodec, MerkleTree}
import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue.{CBytes, CInt}
import xyz.kd5ujc.core.hash.{Blake2b256, Hasher, Sha256}
import xyz.kd5ujc.crypto.sigma.{Secp256k1Group, SigmaProp, SigmaProver}

import io.circe.Json
import io.circe.syntax._
import scodec.bits.ByteVector

import ContractHarness._

/**
 * Writes `vectors/jlvm_contract_cases.json`: calls on the example contracts (successes, rejections and errors) and gas
 * limit boundaries, as vector inputs. Sigma proofs are randomised, so the file is generated once and committed:
 * `sbt "jlvm/Test/runMain xyz.kd5ujc.jlvm.ContractVectorCases"`, then `JlvmVectorGen`.
 */
object ContractVectorCases extends IOApp {

  final case class Case(
    name:     String,
    program:  String,
    state:    CanonicalValue,
    call:     String,
    env:      List[(String, CanonicalValue)],
    gasLimit: Option[Long] = None
  )

  private def programText(name: String): IO[String] =
    IO.blocking(new String(Files.readAllBytes(Paths.get("jlvm", "src", "test", "resources", "programs", s"$name.json")), UTF_8))
      .flatMap(t => io.circe.parser.parse(t).map(_.noSpaces).liftTo[IO])

  private def h(b:    ByteVector): String = "\"0x" + b.toHex + "\""
  private def pk(w:   BigInt): ByteVector = Secp256k1Group.base(w).bytes
  private def dlog(w: BigInt): SigmaProp = SigmaProp.dlog(Secp256k1Group.base(w)).fold(sys.error, identity)
  private def be(i:   BigInt, n: Int): ByteVector = ByteVector(i.toByteArray).takeRight(n.toLong).padLeft(n.toLong)
  private def s(v:    String) = "sender" -> text(v)
  private def t(n:    Long) = "ordinal"  -> CInt(BigInt(n))
  private val tx = ByteVector.fill(32)(0x51)
  private val txb = "txHash" -> CBytes(tx)

  private def prove(r: SecureRandom[IO], prop: SigmaProp, secrets: Vector[BigInt], message: ByteVector): IO[ByteVector] =
    SigmaProver.prove[IO](prop, secrets, message)(using summon, r).flatMap(e => IO.fromEither(e.leftMap(new IllegalStateException(_))))

  private val pool = value("""{"reserve0": 1000000, "reserve1": 4000000, "totalSupply": 1000000, "lp": {"lp1": 1000000}}""")
  private val swap = """{"method": "swap", "zeroForOne": true, "amountIn": 10000, "minOut": 0}"""

  /** The exact gas a call uses suffices; one less does not. */
  private def boundary(jlvm: Jlvm): IO[List[Case]] =
    load("uniswap_v2").map { p =>
      val used = Contract(jlvm, p).run(pool, swap, s("trader")).gasUsed.value
      List(
        Case("swap with exactly the gas it uses", "uniswap_v2", pool, swap, List(s("trader")), Some(used)),
        Case("swap with one gas too few", "uniswap_v2", pool, swap, List(s("trader")), Some(used - 1))
      )
    }

  private def cases(jlvm: Jlvm, random: SecureRandom[IO]): IO[List[Case]] =
    for {
      // ---- Ethereum
      erc721 <- IO.pure {
        val nft0 = value("""{"minter": "mint", "owners": {}, "approvals": {}, "operators": {}, "balances": {}}""")
        val s1 =
          value("""{"minter": "mint", "owners": {"7": "alice"}, "approvals": {"7": "bob"}, "operators": {}, "balances": {"alice": 1}}""")
        List(
          Case("mint", "erc721", nft0, """{"method": "mint", "tokenId": "7", "to": "alice"}""", List(s("mint"))),
          Case("mint by a stranger is rejected", "erc721", nft0, """{"method": "mint", "tokenId": "7", "to": "eve"}""", List(s("eve"))),
          Case(
            "transfer by the approved address",
            "erc721",
            s1,
            """{"method": "transferFrom", "from": "alice", "to": "carol", "tokenId": "7"}""",
            List(s("bob"))
          ),
          Case(
            "transfer from a non-owner is rejected",
            "erc721",
            s1,
            """{"method": "transferFrom", "from": "bob", "to": "carol", "tokenId": "7"}""",
            List(s("bob"))
          ),
          Case(
            "mint with too little gas",
            "erc721",
            nft0,
            """{"method": "mint", "tokenId": "7", "to": "alice"}""",
            List(s("mint")),
            Some(500L)
          )
        )
      }
      wallet <- IO.pure {
        val keys = List(BigInt(101), BigInt(202), BigInt(303), BigInt(404))
        val wallet = ByteVector.fill(20)(0x77)
        val to = ByteVector.fill(20)(0x0b)
        val state = value(
          s"""{"wallet": ${h(wallet)}, "owners": [${keys
              .take(3)
              .map(k => h(EthKeys.address(k)))
              .mkString(", ")}], "threshold": 2, "nonce": 0}"""
        )
        val digest = EthKeys.personal(EthKeys.keccak(wallet ++ be(0, 32) ++ to ++ be(5, 32)))
        def exec(sigs: List[ByteVector]) =
          s"""{"method": "execute", "to": ${h(to)}, "value": 5, "data": "0x", "signatures": [${sigs.map(h).mkString(", ")}]}"""
        List(
          Case("2 of 3 owners sign", "multisig_wallet", state, exec(keys.take(2).map(EthKeys.sign(_, digest))), Nil),
          Case("one owner twice is rejected", "multisig_wallet", state, exec(List.fill(2)(EthKeys.sign(keys(0), digest))), Nil),
          Case("a non-owner does not count", "multisig_wallet", state, exec(List(keys(0), keys(3)).map(EthKeys.sign(_, digest))), Nil),
          Case(
            "an unrecoverable signature is an error",
            "multisig_wallet",
            state,
            exec(List(ByteVector.fill(32)(0) ++ ByteVector.fill(32)(0x11) :+ 27.toByte)),
            Nil
          )
        )
      }
      amm <- IO.pure {
        List(
          Case("swap 0 for 1", "uniswap_v2", pool, swap, List(s("trader"))),
          Case(
            "swap below minOut is rejected",
            "uniswap_v2",
            pool,
            """{"method": "swap", "zeroForOne": true, "amountIn": 10000, "minOut": 40000}""",
            List(s("trader"))
          ),
          Case("add liquidity", "uniswap_v2", pool, """{"method": "addLiquidity", "amount0": 1000, "amount1": 4000}""", List(s("lp2"))),
          Case("remove liquidity", "uniswap_v2", pool, """{"method": "removeLiquidity", "liquidity": 250000}""", List(s("lp1")))
        )
      }
      vesting <- IO.pure {
        val grant = value("""{"beneficiary": "ann", "start": 1000, "cliff": 1250, "duration": 1000, "total": 3000, "released": 0}""")
        List(1249L, 1333L, 5000L).map(at => Case(s"release at $at", "vesting", grant, """{"method": "release"}""", List(t(at))))
      }
      airdrop <- IO.pure {
        val entries = (0 until 5).toList.map(i => value(s"""[$i, "acct$i", ${(i + 1) * 100}]"""))
        val tree = MerkleTree.fromLeaves[Sha256, CanonicalValue](entries).toOption.get
        val proof = render(tree.inclusionProof(3L).flatMap(MerkleCodec.encodeInclusion(_)).toOption.get)
        val state = value(s"""{"head": {"root": ${h(tree.root.bytes)}, "size": ${tree.size}}, "claimed": {}}""")
        List(
          Case(
            "a listed claim",
            "merkle_airdrop",
            state,
            s"""{"method": "claim", "index": 3, "account": "acct3", "amount": 400, "proof": $proof}""",
            Nil
          ),
          Case(
            "another amount is rejected",
            "merkle_airdrop",
            state,
            s"""{"method": "claim", "index": 3, "account": "acct3", "amount": 4000, "proof": $proof}""",
            Nil
          )
        )
      }
      auction <- IO.pure {
        val salt = ByteVector.fill(32)(1)
        val commitment = EthKeys.keccak(be(70, 32) ++ salt)
        val committed = value(
          s"""{"seller": "sam", "item": "lot-1", "commitEnd": 100, "revealEnd": 200, "commits": {"ann": ${h(commitment)}},
                        | "highestBid": 0, "highestBidder": "", "settled": false}""".stripMargin
        )
        List(
          Case(
            "reveal a committed bid",
            "sealed_bid_auction",
            committed,
            s"""{"method": "reveal", "amount": 70, "salt": ${h(salt)}}""",
            List(s("ann"), t(150))
          ),
          Case(
            "a bid that does not match is rejected",
            "sealed_bid_auction",
            committed,
            s"""{"method": "reveal", "amount": 80, "salt": ${h(salt)}}""",
            List(s("ann"), t(150))
          )
        )
      }
      dao <- IO.pure {
        val voted = value(
          """{"weights": {"a": 40, "b": 35, "c": 25}, "quorum": 50, "nextId": 1, "proposals": {"0": {"action": {"kind": "call", "target": "treasury"},
                   | "deadline": 100, "for": 40, "against": 25, "voters": {"a": true, "c": true}, "executed": false}}}""".stripMargin
        )
        List(
          Case("execute a passed proposal", "dao", voted, """{"method": "execute", "id": 0}""", List(s("x"), t(100))),
          Case("execute before the deadline is rejected", "dao", voted, """{"method": "execute", "id": 0}""", List(s("x"), t(99))),
          Case("vote twice is rejected", "dao", voted, """{"method": "vote", "id": 0, "support": true}""", List(s("a"), t(10)))
        )
      }
      // ---- Ergo
      crowd <- (prove(random, dlog(11), Vector(BigInt(11)), tx), prove(random, dlog(21), Vector(BigInt(21)), tx)).mapN { (pp, p1) =>
        val funded = value(
          s"""{"project": ${h(pk(11))}, "goal": 100, "deadline": 50, "pledges": {${h(pk(21))}: 60, ${h(pk(22))}: 50}, "claimed": false}"""
        )
        val short = value(s"""{"project": ${h(pk(11))}, "goal": 100, "deadline": 50, "pledges": {${h(pk(21))}: 60}, "claimed": false}""")
        List(
          Case(
            "the project claims a funded campaign",
            "crowdfunding",
            funded,
            s"""{"method": "claim", "proof": ${h(pp)}}""",
            List(t(30), txb)
          ),
          Case("a backer cannot claim", "crowdfunding", funded, s"""{"method": "claim", "proof": ${h(p1)}}""", List(t(30), txb)),
          Case(
            "a backer refunds a missed goal",
            "crowdfunding",
            short,
            s"""{"method": "refund", "backer": ${h(pk(21))}, "proof": ${h(p1)}}""",
            List(t(60), txb)
          )
        )
      }
      oracle <- {
        val oracles = Vector(31, 32, 33, 34).map(BigInt(_))
        def msg(v: Int) = Hasher[Blake2b256].digest(be(1, 8) ++ be(v, 32)).bytes
        List(31 -> 100, 32 -> 102, 33 -> 98, 34 -> 105).traverse {
          case (w, v) =>
            prove(random, dlog(w), Vector(BigInt(w)), msg(v)).map(p => s"""{"oracle": ${h(pk(w))}, "value": $v, "proof": ${h(p)}}""")
        }.map { ps =>
          val state = value(
            s"""{"oracles": [${oracles
                .map(w => h(pk(w)))
                .mkString(", ")}], "minDatapoints": 3, "epoch": 0, "rate": 0, "epochLength": 60, "lastUpdate": 0}"""
          )
          List(
            Case(
              "update to the median",
              "oracle_pool",
              state,
              s"""{"method": "update", "datapoints": [${ps.mkString(", ")}]}""",
              List(t(60))
            ),
            Case(
              "one datapoint per oracle",
              "oracle_pool",
              state,
              s"""{"method": "update", "datapoints": [${(ps.take(2) :+ ps.head).mkString(", ")}]}""",
              List(t(60))
            )
          )
        }
      }
      bank <- IO.pure {
        val rate = "rate" -> CInt(BigInt(2000000))
        def b(r: Long, s0: Long, rc: Long) =
          value(s"""{"reserve": $r, "stable": $s0, "reserveCoins": $rc, "feePercent": 2, "defaultReservePrice": 1000000}""")
        List(
          Case("mint reservecoins", "sigmausd_bank", b(0, 0, 0), """{"method": "mintReserve", "amount": 1000}""", List(rate)),
          Case("mint stablecoins", "sigmausd_bank", b(1020000000L, 0, 1000), """{"method": "mintStable", "amount": 100}""", List(rate)),
          Case(
            "mint stablecoins below 400% is rejected",
            "sigmausd_bank",
            b(1346400000L, 160, 1000),
            """{"method": "mintStable", "amount": 20}""",
            List(rate)
          ),
          Case(
            "redeem reservecoins",
            "sigmausd_bank",
            b(1346400000L, 160, 1000),
            """{"method": "redeemReserve", "amount": 10}""",
            List(rate)
          )
        )
      }
      stealth <- {
        val y = BigInt(777)
        val g = Secp256k1Group.Generator
        val u = Secp256k1Group.base(BigInt(555))
        val r = BigInt(1001)
        val bg = g * r
        val bh = u * r
        val bu = g * (y * r)
        val bv = u * (y * r)
        prove(random, SigmaProp.dhTuple(bg, bh, bu, bv).fold(sys.error, identity), Vector(y), tx).map { p =>
          val call = s"""{"method": "spend", "box": {"g": ${h(bg.bytes)}, "h": ${h(bh.bytes)}, "u": ${h(bu.bytes)}, "v": ${h(
              bv.bytes
            )}}, "proof": ${h(p)}}"""
          List(
            Case("the receiver spends", "stealth_address", value("{}"), call, List(txb)),
            Case(
              "the proof does not spend another transaction",
              "stealth_address",
              value("{}"),
              call,
              List("txHash" -> CBytes(ByteVector.fill(32)(0x52)))
            )
          )
        }
      }
      house <- prove(random, dlog(42), Vector(BigInt(42)), tx).map { pa =>
        val start = value(
          s"""{"seller": ${h(
              pk(41)
            )}, "item": "lot-9", "reserve": 100, "minStepPercent": 5, "end": 1000, "extendWindow": 60, "extendBy": 120,
                      | "bid": 0, "bidder": "", "settled": false}""".stripMargin
        )
        List(
          Case(
            "a first bid at the reserve",
            "auction_house",
            start,
            s"""{"method": "bid", "bidder": ${h(pk(42))}, "amount": 100, "proof": ${h(pa)}}""",
            List(t(100), txb)
          ),
          Case(
            "a late bid extends the end",
            "auction_house",
            start,
            s"""{"method": "bid", "bidder": ${h(pk(42))}, "amount": 100, "proof": ${h(pa)}}""",
            List(t(950), txb)
          ),
          Case("settle with no bids returns the item", "auction_house", start, """{"method": "settle"}""", List(t(1000), txb))
        )
      }
      treasury <- {
        val ws = Vector(51, 52, 53).map(BigInt(_))
        val treasury = ByteVector.fill(8)(0x7e)
        val to = ByteVector.fill(20)(0x0c)
        val msg = Hasher[Blake2b256].digest(treasury ++ be(0, 8) ++ to ++ be(40, 32)).bytes
        prove(random, SigmaProp.threshold(2, ws.map(dlog)).fold(sys.error, identity), ws.take(2), msg).map { p =>
          def state(n: Int) =
            value(s"""{"treasury": ${h(treasury)}, "signers": [${ws.map(w => h(pk(w))).mkString(", ")}], "k": 2, "nonce": $n}""")
          List(
            Case(
              "2 of 3 sign",
              "threshold_treasury",
              state(0),
              s"""{"method": "spend", "to": ${h(to)}, "amount": 40, "proof": ${h(p)}}""",
              Nil
            ),
            Case(
              "no replay at the next nonce",
              "threshold_treasury",
              state(1),
              s"""{"method": "spend", "to": ${h(to)}, "amount": 40, "proof": ${h(p)}}""",
              Nil
            )
          )
        }
      }
      edges <- boundary(jlvm)
    } yield
      erc721 ++ wallet ++ amm ++ vesting ++ airdrop ++ auction ++ dao ++ crowd ++ oracle ++ bank ++ stealth ++ house ++ treasury ++ edges

  def run(args: List[String]): IO[ExitCode] =
    (Jlvm.load[IO], SecureRandom.javaSecuritySecureRandom[IO])
      .flatMapN(cases)
      .flatMap { cs =>
        cs.traverse { c =>
          programText(c.program).map { program =>
            val data = render(CanonicalValue.map(List("state" -> c.state, "call" -> value(c.call))).toOption.get)
            val env = c.env.map { case (k, v) => k -> render(v) }
            Json.fromFields(
              List("name" -> s"contract ${c.program}: ${c.name}".asJson, "program" -> program.asJson, "data" -> data.asJson) ++
              Option.when(env.nonEmpty)("env" -> Json.fromFields(env.map { case (k, v) => k -> v.asJson })) ++
              c.gasLimit.map(l => "gasLimit" -> l.asJson)
            )
          }
        }.flatMap { rows =>
          val json = Json.obj(
            "source" -> "dilf4s example contracts (jlvm/src/test/resources/programs), generated by ContractVectorCases".asJson,
            "cases"  -> rows.asJson
          )
          IO.blocking(
            Files.write(
              Paths.get("jlvm", "src", "test", "resources", "vectors", "jlvm_contract_cases.json"),
              (json.spaces2 + "\n").getBytes(UTF_8)
            )
          )
        }
      }
      .as(ExitCode.Success)
}
