package xyz.kd5ujc.jlvm

import java.nio.charset.StandardCharsets.UTF_8

import cats.Show
import cats.effect.std.SecureRandom
import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.crypto.bls.Bls12381
import xyz.kd5ujc.crypto.sigma.{Secp256k1Group, SigmaProp, SigmaProver}
import xyz.kd5ujc.crypto.signing._

import io.circe.{Json, parser}
import org.scalacheck.Gen
import scodec.bits.{ByteVector, hex}
import weaver.scalacheck.Checkers
import weaver.{Expectations, IOSuite}

object CryptoOpsSuite extends IOSuite with Checkers {

  final case class Backends(jlvm: Jlvm, ops: CryptoOps, secp: Secp256k1, bls: Bls12381, random: SecureRandom[IO])
  type Res = Backends
  def sharedResource: Resource[IO, Backends] =
    Resource.eval(
      (Jlvm.load[IO], CryptoOps.load[IO], Secp256k1.load[IO], Bls12381.load[IO], SecureRandom.javaSecuritySecureRandom[IO])
        .mapN(Backends.apply)
    )

  private def seed(n: Int): ByteVector = ByteVector.fill(32)(n.toByte)
  private val message = ByteVector("transfer 10 to bob".getBytes("UTF-8"))
  private val other = ByteVector("transfer 99 to eve".getBytes("UTF-8"))

  private def verify(jlvm: Jlvm, op: Op, pk: CanonicalValue, msg: ByteVector, sig: ByteVector): Either[JlvmError, CanonicalValue] = {
    val program = Program.from(Expr.Apply(op, Vector("pk", "msg", "sig").map(k => Expr.Var(VarRef.Data(Vector(k)), None)))).toOption.get
    val data = CanonicalValue.map(List("pk" -> pk, "msg" -> CBytes(msg), "sig" -> CBytes(sig))).toOption.get
    jlvm.evaluate(program, data, Map.empty, Config()).result
  }

  private def crypto(r: Either[JlvmError, CanonicalValue]): Boolean = r match {
    case Left(JlvmError.Crypto(_, _)) => true
    case _                            => false
  }

  /** A good signature verifies; another message gives false; a truncated signature is malformed input. */
  private def scheme(jlvm: Jlvm, op: Op, pk: ByteVector, sig: ByteVector, msg: ByteVector = message): Expectations =
    expect
      .same(Right(True), verify(jlvm, op, CBytes(pk), msg, sig))
      .and(expect.same(Right(False), verify(jlvm, op, CBytes(pk), other, sig)))
      .and(expect(crypto(verify(jlvm, op, CBytes(pk), msg, sig.dropRight(1))), s"${op.wire}: short signature"))
      .and(expect(verify(jlvm, op, CInt(BigInt(1)), msg, sig).isLeft, "a number is not a key"))

  test("ed25519_verify") { b =>
    IO {
      val kp = Ed25519.seedDerivation.fromSeed(seed(1)).toOption.get
      scheme(b.jlvm, Op.Ed25519Verify, kp.verifying.bytes, Ed25519.signer.sign(kp.signing, message).bytes)
    }
  }

  test("secp256k1_verify (ECDSA over SHA-256 of the message)") { b =>
    IO {
      val kp = b.secp.ecdsaSeedDerivation.fromSeed(seed(2)).toOption.get
      scheme(b.jlvm, Op.Secp256k1Verify, kp.verifying.bytes, b.secp.ecdsaSigner.sign(kp.signing, message).bytes)
    }
  }

  test("bip340_verify takes exactly 32-byte messages") { b =>
    IO {
      val kp = b.secp.bip340SeedDerivation.fromSeed(seed(3)).toOption.get
      val m32 = ByteVector.fill(32)(7)
      val sig = b.secp.bip340SignWithAux(kp.signing, Message32.from(m32).toOption.get, ByteVector.fill(32)(0)).toOption.get
      expect
        .same(Right(True), verify(b.jlvm, Op.Bip340Verify, CBytes(kp.verifying.bytes), m32, sig.bytes))
        .and(expect.same(Right(False), verify(b.jlvm, Op.Bip340Verify, CBytes(kp.verifying.bytes), ByteVector.fill(32)(8), sig.bytes)))
        .and(expect(crypto(verify(b.jlvm, Op.Bip340Verify, CBytes(kp.verifying.bytes), message, sig.bytes)), "not 32 bytes"))
    }
  }

  test("bls_verify and bls_aggregate_verify") { b =>
    IO {
      val kps = List(4, 5, 6).map(n => b.bls.seedDerivation.fromSeed(seed(n)).toOption.get)
      val sigs = kps.map(kp => b.bls.sign(kp.signing, message).toOption.get)
      val agg = b.bls.aggregate(sigs).toOption.get
      val keys = CanonicalValue.array(kps.map(kp => CBytes(kp.verifying.bytes): CanonicalValue).toVector).toOption.get
      scheme(b.jlvm, Op.BlsVerify, kps.head.verifying.bytes, sigs.head.bytes)
        .and(expect.same(Right(True), verify(b.jlvm, Op.BlsAggregateVerify, keys, message, agg.bytes)))
        .and(expect.same(Right(False), verify(b.jlvm, Op.BlsAggregateVerify, keys, other, agg.bytes)))
        .and(expect(crypto(verify(b.jlvm, Op.BlsAggregateVerify, keys, message, agg.bytes.dropRight(1))), "short aggregate"))
    }
  }

  test("a multisig: at least two of three keys signed") { b =>
    IO {
      val kps = List(7, 8, 9).map(n => Ed25519.seedDerivation.fromSeed(seed(n)).toOption.get)
      val sigs = kps.map(kp => Ed25519.signer.sign(kp.signing, message))
      def entry(i: Int, signed: Boolean): CanonicalValue =
        CanonicalValue
          .map(List("pk" -> CBytes(kps(i).verifying.bytes), "sig" -> CBytes(if (signed) sigs(i).bytes else ByteVector.fill(64)(0))))
          .toOption
          .get
      val program = Parser
        .parse(
          """{"some": [{"var": "signers"},
            |  {"ed25519_verify": [{"var": "$current.pk"}, {"var": "msg"}, {"var": "$current.sig"}]}, 2]}""".stripMargin
        )
        .toOption
        .get
      def run(signed: List[Boolean]) = {
        val data = CanonicalValue
          .map(
            List(
              "msg"     -> CBytes(message),
              "signers" -> CanonicalValue.array(signed.zipWithIndex.map { case (s, i) => entry(i, s) }.toVector).toOption.get
            )
          )
          .toOption
          .get
        b.jlvm.evaluate(program, data, Map.empty, Config()).result
      }
      expect
        .same(Right(True), run(List(true, false, true)))
        .and(expect.same(Right(False), run(List(true, false, false))))
    }
  }

  test("the BN254 family is reachable from programs: e(P, Q) * e(-P, Q) = 1, priced the same in either form") { b =>
    IO {
      val g1 = "0x" + "00" * 31 + "01" + "00" * 31 + "02"
      val neg = "0x" + "00" * 31 + "01" + "30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd45"
      val g2 = "0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2" +
        "1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed" +
        "090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b" +
        "12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa"
      def run(json: String) = b.jlvm.evaluate(Parser.parse(json).toOption.get, CNull, Map.empty, Config())
      val separate = run(s"""{"bn254_pairing": [["$g1", "$g2"], ["$neg", "$g2"]]}""")
      val single = run(s"""{"bn254_pairing": [[["$g1", "$g2"], ["$neg", "$g2"]]]}""")
      expect
        .same(Right(True), separate.result)
        .and(expect.same(Right(True), single.result))
        // the forms differ by one array node of two items: a unit of weight at the pairing's rate, and its own charges
        .and(
          expect(
            math.abs(separate.gasUsed.value - single.gasUsed.value) <=
              GasSchedule.V1.cost(Op.Bn254Pairing).perUnit.value + GasSchedule.V1.literal.value + 2 * GasSchedule.V1.element.value,
            s"${separate.gasUsed} vs ${single.gasUsed}"
          )
        )
    }
  }

  // ---- sigma protocols (docs/specs/sigma.md)

  private def run(jlvm: Jlvm, program: String, data: String, env: Map[String, CanonicalValue] = Map.empty): Outcome =
    jlvm.evaluate(
      Parser.parse(program).fold(e => sys.error(e.toString), identity),
      JsonSurface.parse(data).fold(e => sys.error(e.toString), identity),
      env,
      Config()
    )

  private def key(pk: xyz.kd5ujc.crypto.sigma.GroupElement): SigmaProp = SigmaProp.dlog(pk).fold(sys.error, identity)
  private def tuple(
    g: xyz.kd5ujc.crypto.sigma.GroupElement,
    h: xyz.kd5ujc.crypto.sigma.GroupElement,
    u: xyz.kd5ujc.crypto.sigma.GroupElement,
    v: xyz.kd5ujc.crypto.sigma.GroupElement
  ): SigmaProp = SigmaProp.dhTuple(g, h, u, v).fold(sys.error, identity)

  private def h(b:  ByteVector): String = "\"0x" + b.toHex + "\""
  private def pk(w: BigInt): ByteVector = Secp256k1Group.base(w).bytes

  private def prove(r: SecureRandom[IO], prop: Either[String, SigmaProp], secrets: Vector[BigInt], message: ByteVector): IO[ByteVector] =
    IO.fromEither(prop.leftMap(new IllegalArgumentException(_)))
      .flatMap(p => SigmaProver.prove[IO](p, secrets, message)(using summon, r))
      .flatMap(e => IO.fromEither(e.leftMap(new IllegalStateException(_))))

  test("schnorr_verify: Ergo's P2PK known-answer vector, through a program") { b =>
    IO {
      val program = """{"schnorr_verify": [{"var": "pk"}, {"var": "msg"}, {"var": "sig"}]}"""
      val data = (sig: String) => s"""{"pk": "0x03cb0d49e4eae7e57059a3da8ac52626d26fc11330af8fb093fa597d8b93deb7b1",
           | "msg": "0x1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00", "sig": "$sig"}""".stripMargin
      val sig = "0xbcb866ba434d5c77869ddcbc3f09ddd62dd2d2539bf99076674d1ae0c32338ea95581fdc18a3b66789904938ac641eba1a66d234070207a2"
      expect
        .same(Right(True), run(b.jlvm, program, data(sig)).result)
        .and(expect(crypto(run(b.jlvm, program, data(sig.dropRight(2))).result), "a short proof is malformed"))
    }
  }

  test("a ring signature built in the program: any member signs, nobody else can") { b =>
    val ws = Vector(BigInt(101), BigInt(202), BigInt(303))
    val message = ByteVector("rotate the oracle key".getBytes("UTF-8"))
    val ring = SigmaProp.or(ws.map(w => key(Secp256k1Group.base(w))))
    val program =
      """{"sigma_verify": [{"type": "or", "children": {"map": [{"var": "ring"}, {"type": "dlog", "pk": {"var": "$current"}}]}},
        |  {"var": "proof"}, {"var": "msg"}]}""".stripMargin
    def data(proof: ByteVector) = s"""{"ring": [${ws.map(w => h(pk(w))).mkString(", ")}], "proof": ${h(proof)}, "msg": ${h(message)}}"""
    (prove(b.random, ring, Vector(ws(1)), message), prove(b.random, ring, Vector(BigInt(404)), message).attempt).mapN { (proof, outsider) =>
      expect
        .same(Right(True), run(b.jlvm, program, data(proof)).result)
        .and(expect(outsider.isLeft, "a non-member cannot prove"))
        .and(expect.same(Right(False), run(b.jlvm, program, data(proof.update(30, (proof(30) ^ 1).toByte))).result))
        .and(expect(run(b.jlvm, program, data(proof)).gasUsed.value > 100000L, "three leaves are priced"))
    }
  }

  test("Ergo's atomic swap: Bob claims with the preimage before the deadline, Alice refunds after it") { b =>
    // anyOf(HEIGHT > deadline && pkA, pkB && blake2b256(secret) == hx): the program reduces the boolean parts and
    // verifies the sigma statement that remains (docs/specs/sigma.md section 3)
    val (alice, bob) = (BigInt(5551), BigInt(7772))
    val preimage = ByteVector("the swap secret".getBytes("UTF-8"))
    val hx = xyz.kd5ujc.core.hash.Hasher[xyz.kd5ujc.core.hash.Blake2b256].digest(preimage).bytes
    val message = ByteVector.fill(32)(9)
    val program =
      """{"let": [{"refund": {">": [{"var": "$height"}, {"var": "deadline"}]},
        |           "claim": {"==": [{"blake2b256": {"var": "secret"}}, {"var": "hx"}]}},
        |  {"and": [{"or": [{"var": "$refund"}, {"var": "$claim"}]},
        |    {"sigma_verify": [
        |      {"if": [{"and": [{"var": "$refund"}, {"var": "$claim"}]},
        |                {"type": "or", "children": [{"type": "dlog", "pk": {"var": "alice"}}, {"type": "dlog", "pk": {"var": "bob"}}]},
        |              {"var": "$refund"}, {"type": "dlog", "pk": {"var": "alice"}},
        |              {"type": "dlog", "pk": {"var": "bob"}}]},
        |      {"var": "proof"}, {"var": "msg"}]}]}]}""".stripMargin
    def data(secret: ByteVector, proof: ByteVector) =
      s"""{"deadline": 100, "hx": ${h(hx)}, "secret": ${h(secret)}, "alice": ${h(pk(alice))}, "bob": ${h(pk(bob))},
         | "proof": ${h(proof)}, "msg": ${h(message)}}""".stripMargin
    def at(height: Int) = Map("height" -> (CInt(BigInt(height)): CanonicalValue))
    (
      prove(b.random, Right(key(Secp256k1Group.base(bob))), Vector(bob), message),
      prove(b.random, Right(key(Secp256k1Group.base(alice))), Vector(alice), message)
    ).mapN { (bobs, alices) =>
      expect
        .same(Right(True), run(b.jlvm, program, data(preimage, bobs), at(50)).result)
        .and(expect.same(Right(False), run(b.jlvm, program, data(ByteVector.empty, bobs), at(50)).result))
        .and(expect.same(Right(True), run(b.jlvm, program, data(ByteVector.empty, alices), at(150)).result))
        .and(expect.same(Right(False), run(b.jlvm, program, data(ByteVector.empty, bobs), at(150)).result))
    }
  }

  test("ZeroJoin's half-mix contract: Bob mixes into two equal outputs, hiding which one is Alice's") { b =>
    // Ergo's MixExampleSpecification, halfMixScript: two outputs of the half-mix's value, both locked by the
    // full-mix script, holding (c1, c2) and (c2, c1); and proveDHTuple(g, gX, c1, c2) || proveDHTuple(g, gX, c2, c1)
    val (x, y) = (BigInt(424242), BigInt(171717))
    val g = Secp256k1Group.Generator
    val (gy, gxy) = ((g * y).bytes, (g * (x * y)).bytes)
    val script = ByteVector("full-mix program".getBytes("UTF-8"))
    val scriptHash = xyz.kd5ujc.core.hash.Hasher[xyz.kd5ujc.core.hash.Blake2b256].digest(script).bytes
    val message = ByteVector.fill(32)(3)
    val program =
      """{"and": [
        |  {"==": [{"length": {"var": "outputs"}}, 2]},
        |  {"==": [{"var": "outputs.0.value"}, {"var": "self.value"}]},
        |  {"==": [{"var": "outputs.1.value"}, {"var": "self.value"}]},
        |  {"==": [{"blake2b256": {"var": "outputs.0.script"}}, {"var": "fullMixScriptHash"}]},
        |  {"==": [{"blake2b256": {"var": "outputs.1.script"}}, {"var": "fullMixScriptHash"}]},
        |  {"==": [{"var": "outputs.1.r4"}, {"var": "outputs.0.r5"}]},
        |  {"==": [{"var": "outputs.1.r5"}, {"var": "outputs.0.r4"}]},
        |  {"sigma_verify": [
        |    {"type": "or", "children": [
        |      {"type": "dhtuple", "g": {"var": "g"}, "h": {"var": "self.gX"}, "u": {"var": "outputs.0.r4"}, "v": {"var": "outputs.0.r5"}},
        |      {"type": "dhtuple", "g": {"var": "g"}, "h": {"var": "self.gX"}, "u": {"var": "outputs.0.r5"}, "v": {"var": "outputs.0.r4"}}]},
        |    {"var": "proof"}, {"var": "msg"}]}]}""".stripMargin
    def out(r4: ByteVector, r5: ByteVector, value: Int = 10) =
      s"""{"value": $value, "script": ${h(script)}, "r4": ${h(r4)}, "r5": ${h(r5)}}"""
    def data(outputs: List[String], proof: ByteVector) =
      s"""{"g": ${h(g.bytes)}, "self": {"value": 10, "gX": ${h((g * x).bytes)}}, "fullMixScriptHash": ${h(scriptHash)},
         | "outputs": [${outputs.mkString(", ")}], "proof": ${h(proof)}, "msg": ${h(message)}}""".stripMargin
    // g is read from data here for brevity; a real contract fixes it as a literal, since a proposition built from the
    // spender's own arguments lets the spender choose a statement they can prove (docs/specs/sigma.md section 8).
    // Bob puts Alice's output (c2 = g^xy) second; his proof covers whichever order he chose
    val outputs = List(out(gy, gxy), out(gxy, gy))
    val prop = SigmaProp.or(
      Vector(
        tuple(g, g * x, Secp256k1Group.decode(gy).toOption.get, Secp256k1Group.decode(gxy).toOption.get),
        tuple(g, g * x, Secp256k1Group.decode(gxy).toOption.get, Secp256k1Group.decode(gy).toOption.get)
      )
    )
    val identity = ByteVector.fill(33)(0)
    prove(b.random, prop, Vector(y), message).map { proof =>
      expect
        .same(Right(True), run(b.jlvm, program, data(outputs, proof)).result)
        .and(expect.same(Right(False), run(b.jlvm, program, data(List(out(gy, gxy), out(gy, gxy)), proof)).result))
        .and(expect.same(Right(False), run(b.jlvm, program, data(List(out(gy, gxy), out(gxy, gy, value = 9)), proof)).result))
        .and(expect(crypto(run(b.jlvm, program, data(outputs, proof.drop(1))).result), "a short proof is malformed"))
        .and(
          expect(crypto(run(b.jlvm, program, data(List(out(identity, identity), out(identity, identity)), proof)).result), "c1 = c2 = O")
        )
    }
  }

  test("schnorr_verify rejects the identity key: anyone could prove dlog(O), as metakit warned") { b =>
    IO {
      // the forgery that would verify if O were accepted: any z, a = z·G, e = H(transcript(a) || msg)
      val z = BigInt(12345)
      val a = Secp256k1Group.base(z).bytes
      val o = ByteVector.fill(33)(0)
      val statement = ByteVector(0x10, 0x01, 0x08, 0xcd) ++ o ++ ByteVector(0x73, 0x00)
      val transcript = ByteVector(0x01, 0x00, statement.size.toInt) ++ statement ++ ByteVector(0x00, 0x21) ++ a
      val msg = ByteVector.fill(32)(5)
      val e = xyz.kd5ujc.crypto.sigma.Sigma.challenge(transcript, msg)
      val forged = e ++ Secp256k1Group.scalarBytes(z)
      val outcome = run(
        b.jlvm,
        """{"schnorr_verify": [{"var": "pk"}, {"var": "msg"}, {"var": "sig"}]}""",
        s"""{"pk": ${h(o)}, "msg": ${h(msg)}, "sig": ${h(forged)}}"""
      )
      expect(crypto(outcome.result), s"the identity key is malformed, got ${outcome.result}")
    }
  }

  test("sigma_verify: malformed propositions are crypto errors") { b =>
    IO {
      // each proof has the length the proposition would fix if its flaw were ignored (sigma.md §5: 24 + 32 per leaf
      // + 24 (n - k) per threshold), so only the flaw named can make it an error; a well-formed one gives false
      val proof = (n: Int) => "\"0x" + "01" * n + "\""
      val p = (prop: String, n: Int) => run(b.jlvm, s"""{"sigma_verify": [$prop, ${proof(n)}, "0x00"]}""", "{}").result
      val leaf = s"""{"type": "dlog", "pk": "$G"}"""
      val offCurve = "0x02" + "00" * 31 + "05" // 5^3 + 7 is not a square modulo the field prime
      expect
        .same(Right(False), p(leaf, 56))
        .and(expect(crypto(p("""{"type": "or", "children": []}""", 24)), "an empty or"))
        .and(expect(crypto(p("""{"type": "dlog", "pk": "0x02"}""", 56)), "a short key"))
        .and(expect(crypto(p("""{"object": {"type": "dlog"}}""", 56)), "no pk")) // a single-key map needs "object"
        .and(expect(crypto(p(s"""{"type": "dlog", "pk": "$G", "extra": 1}""", 56)), "an extra field"))
        .and(expect(crypto(p(s"""{"type": "dlog", "pk": "$G", "children": [$leaf]}""", 56)), "children on a leaf"))
        .and(expect(crypto(p(s"""{"type": "dlog", "pk": "$offCurve"}""", 56)), "an off-curve key"))
        .and(expect(crypto(p(s"""{"type": "dhtuple", "g": "$offCurve", "h": "$G", "u": "$G", "v": "$G"}""", 56)), "an off-curve base"))
        .and(expect(crypto(p(s"""{"type": "xor", "children": [$leaf]}""", 56)), "an unknown type"))
        .and(expect(crypto(p(s"""{"type": "threshold", "k": 0, "children": [$leaf]}""", 80)), "k = 0"))
        .and(expect(crypto(p(s"""{"type": "threshold", "k": 2, "children": [$leaf]}""", 32)), "k > n"))
        .and(expect(crypto(p(leaf, 1)), "a 1-byte proof has the wrong length"))
    }
  }

  /** The generator, compressed: a well-formed key, so the malformed cases above fail for the reason they name. */
  private val G = "0x0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"

  // ---- Ethereum: keccak256, ecrecover and EIP-191 personal_sign

  test("ecrecover gives the signer of an EIP-191 personal_sign message built in the program") { b =>
    IO {
      val key = BigInt("4c0883a69102937d6231471b5dbb6204fe5129617082792ae468d01a3f362318", 16)
      val hash = EthKeys.keccak("transfer 10 to bob")
      val sig = EthKeys.sign(key, EthKeys.personal(hash))
      val program =
        s"""{"ecrecover": [{"keccak256": {"cat": ["${EthKeys.hex(EthKeys.PersonalPrefix)}", {"var": "hash"}]}}, {"var": "sig"}]}"""
      def recover(h: ByteVector, s: ByteVector) =
        run(b.jlvm, program, s"""{"hash": "${EthKeys.hex(h)}", "sig": "${EthKeys.hex(s)}"}""").result
      val n = BigInt("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)
      val highS =
        sig.take(32) ++ ByteVector((n - BigInt(1, sig.slice(32, 64).toArray)).toByteArray).takeRight(32).padLeft(32) ++ sig.takeRight(1)
      expect
        .same(Right(CBytes(EthKeys.address(key))), recover(hash, sig))
        .and(
          expect(
            recover(EthKeys.keccak("transfer 99 to eve"), sig).exists(_ != CBytes(EthKeys.address(key))),
            "another message, another signer"
          )
        )
        .and(expect(crypto(recover(hash, highS)), "the high-s twin is an error, never a value"))
        .and(expect(crypto(recover(hash, sig.dropRight(1) :+ 29.toByte)), "v = 29 is malformed"))
        .and(expect(crypto(recover(hash, sig.drop(1))), "a 64-byte signature is malformed"))
    }
  }

  test("a failed recovery cannot equal an unset owner: it is an error, never null or the zero address") { b =>
    IO {
      val garbage = ByteVector.fill(32)(0) ++ ByteVector.fill(32)(1) :+ 27.toByte
      val program = """{"==": [{"ecrecover": [{"var": "digest"}, {"var": "sig"}]}, {"var": "owner"}]}"""
      expect(crypto(run(b.jlvm, program, s"""{"digest": "0x${"00" * 32}", "sig": "${EthKeys.hex(garbage)}"}""").result), "no owner set")
    }
  }

  test("keccak256 of text and bytes; cat concatenates bytes only when every argument is bytes") { b =>
    IO {
      def eval(p: String) = run(b.jlvm, p, "{}").result
      expect
        .same(Right(CBytes(EthKeys.keccak("abc"))), eval("""{"keccak256": "abc"}"""))
        .and(expect.same(Right(CBytes(EthKeys.keccak("abc"))), eval("""{"keccak256": "0x616263"}""")))
        .and(expect.same(Right(CBytes(ByteVector(1, 2, 3))), eval("""{"cat": ["0x01", "0x0203"]}""")))
        .and(expect(eval("""{"cat": ["0x01", "a"]}""").isLeft, "bytes and text do not mix"))
    }
  }

  // ---- programs over verified facts (metakit's worked examples)

  private def text(s: String): CanonicalValue = CanonicalValue.text(s).fold(e => sys.error(e.toString), identity)

  /** The arguments of the first metakit zk vector of `op` whose expected result is `true`. */
  private def validArgs(op: String): IO[Vector[Json]] =
    Resource
      .fromAutoCloseable(IO.blocking(getClass.getResourceAsStream("/vectors/jlvm_zk_from_metakit.json")))
      .use(in => IO.blocking(new String(in.readAllBytes(), UTF_8)))
      .flatMap(t => IO.fromEither(parser.parse(t).flatMap(_.hcursor.downField("cases").as[Vector[Json]])))
      .flatMap { cases =>
        val found = cases.find(c => c.hcursor.get[String]("op").contains(op) && c.hcursor.downField("expected").focus.contains(Json.True))
        IO.fromOption(found.flatMap(_.hcursor.get[Vector[Json]]("args").toOption))(new NoSuchElementException(s"no valid $op vector"))
      }

  test("a policy over verified facts: granted only when the Groth16 proof and the Merkle path both verify") { b =>
    (validArgs("groth16_verify"), validArgs("pmt_verify")).mapN { (groth16, pmt) =>
      val program =
        """{"if": [{"and": [
          |  {"groth16_verify": [{"var": "vkey"}, {"var": "publicValues"}, {"var": "proof"}]},
          |  {"pmt_verify": [{"var": "root"}, {"var": "leaf"}, {"var": "index"}, {"var": "siblings"}]}]},
          |  "granted", "denied"]}""".stripMargin
      val proof = groth16(2).asString.getOrElse("")
      // the lowest bit of the last proof byte flipped: well formed, but the pairing check fails
      val badProof = proof.dropRight(1) + Integer.toHexString(Integer.parseInt(proof.takeRight(1), 16) ^ 1)
      val siblings = pmt(3).asArray.getOrElse(Vector.empty)
      val badSiblings = Json.fromValues(Json.fromString("0x" + "00" * 31 + "07") +: siblings.drop(1))
      def data(p: String, sibs: Json) =
        Json
          .obj(
            "vkey"         -> groth16(0),
            "publicValues" -> groth16(1),
            "proof"        -> Json.fromString(p),
            "root"         -> pmt(0),
            "leaf"         -> pmt(1),
            "index"        -> pmt(2),
            "siblings"     -> sibs
          )
          .noSpaces
      expect
        .same(Right(text("granted")), run(b.jlvm, program, data(proof, Json.fromValues(siblings))).result)
        .and(expect.same(Right(text("denied")), run(b.jlvm, program, data(badProof, Json.fromValues(siblings))).result))
        .and(expect.same(Right(text("denied")), run(b.jlvm, program, data(proof, badSiblings)).result))
    }
  }

  // Eth2 fast_aggregate_verify_valid_3d7576f3c0e3570a (ethereum/bls12-381-tests v0.1.2): three signers, one message
  private val quorum = List(
    "0xa491d1b0ecd9bb917989f0e74f0dea0422eac4a873e5e2644f368dffb9a6e20fd6e10c1b77654d067c0618f6e5a7f79a",
    "0xb301803f8b5ac4a1133581fc676dfedc60d891dd5fa99028805e5ea5b08d3491af75d0707adab3b70c6a6a580217bf81",
    "0xb53d21a4cfd562c469cc81514d4ce5a6b577d8403d32a394dc265dd190b47fa9f829fdd7963afdf972e5e77854051f6f"
  )
  private val quorumMessage = "0x" + "ab" * 32
  private val quorumSignature =
    "0x9712c3edd73a209c742b8250759db12549b3eaf43b5ca61376d9f30e2747dbcf842d8b2ac0901d2a093713e20284a767" +
    "0fcf6954e9ab93de991bb9b313e664785a075fc285806fa5224c82bde146561b446ccfc706a64b8579513cfc4ff1d930"

  test("bls_aggregate_verify gates a program on a published quorum; a key left out of the set is false") { b =>
    IO {
      val bare = """{"bls_aggregate_verify": [{"var": "keys"}, {"var": "msg"}, {"var": "sig"}]}"""
      val gate = s"""{"if": [$bare, "authorized", "rejected"]}"""
      def data(keys: List[String]) =
        s"""{"keys": [${keys.map(k => s"\"$k\"").mkString(", ")}], "msg": "$quorumMessage", "sig": "$quorumSignature"}"""
      expect
        .same(Right(True), run(b.jlvm, bare, data(quorum)).result)
        .and(expect.same(Right(False), run(b.jlvm, bare, data(quorum.take(2))).result))
        .and(expect.same(Right(text("authorized")), run(b.jlvm, gate, data(quorum)).result))
        .and(expect.same(Right(text("rejected")), run(b.jlvm, gate, data(quorum.take(2))).result))
    }
  }

  test("bn254_pairing: each extra pair costs the same, its weight at the pairing's rate plus its literal nodes") { b =>
    IO {
      val g1 = "0x" + "00" * 31 + "01" + "00" * 31 + "02"
      val g2 = "0x198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c2" +
        "1800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed" +
        "090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b" +
        "12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa"
      val pair = s"""["$g1", "$g2"]"""
      def gas(n: Int) = run(b.jlvm, s"""{"bn254_pairing": [${List.fill(n)(pair).mkString(", ")}]}""", "{}").gasUsed.value
      val schedule = Config().schedule
      val pairValue = JsonSurface.parse(pair).fold(e => sys.error(e.toString), identity)
      // one more array expression of two byte literals, and the pair's weight at the pairing's per-unit rate
      val perPair = 3 * schedule.literal.value + 2 * schedule.element.value + schedule.sized(Op.Bn254Pairing, Vector(pairValue)).value
      expect
        .same(195L, pairValue.weight)
        .and(expect.same(perPair, gas(2) - gas(1)))
        .and(expect.same(perPair, gas(3) - gas(2)))
    }
  }

  test("schnorr_verify: a tampered response or another message is false; a one-leaf sigma_verify agrees on each") { b =>
    IO {
      val program = """{"schnorr_verify": [{"var": "pk"}, {"var": "msg"}, {"var": "sig"}]}"""
      val oneLeaf = """{"sigma_verify": [{"type": "dlog", "pk": {"var": "pk"}}, {"var": "sig"}, {"var": "msg"}]}"""
      val msg = "0x1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00"
      val sig = "0xbcb866ba434d5c77869ddcbc3f09ddd62dd2d2539bf99076674d1ae0c32338ea95581fdc18a3b66789904938ac641eba1a66d234070207a2"
      val pk = "0x03cb0d49e4eae7e57059a3da8ac52626d26fc11330af8fb093fa597d8b93deb7b1"
      def data(m: String, s: String) = s"""{"pk": "$pk", "msg": "$m", "sig": "$s"}"""
      val cases = List(
        "valid"           -> data(msg, sig),
        "response byte"   -> data(msg, sig.dropRight(2) + "a3"),
        "challenge byte"  -> data(msg, "0xbdb8" + sig.drop(6)),
        "another message" -> data("0x1ec01772" + msg.drop(10), sig)
      )
      expect
        .same(Right(True), run(b.jlvm, program, data(msg, sig)).result)
        .and(expect.same(Right(False), run(b.jlvm, program, data(msg, sig.dropRight(2) + "a3")).result))
        .and(expect.same(Right(False), run(b.jlvm, program, data(msg, "0xbdb8" + sig.drop(6))).result))
        .and(expect.same(Right(False), run(b.jlvm, program, data("0x1ec01772" + msg.drop(10), sig)).result))
        .and(cases.foldMap { c =>
          val schnorr = run(b.jlvm, program, c._2).result
          val sigma = run(b.jlvm, oneLeaf, c._2).result
          expect(schnorr == sigma, s"${c._1}: schnorr_verify $schnorr, sigma_verify $sigma")
        })
    }
  }

  // ---- totality: hostile input to the signature, recovery and sigma operations gives a value or a typed error

  private given Show[Op] = Show.show(_.wire)
  private given Show[CanonicalValue] = Show.fromToString

  private val signatureOps: List[Op] =
    List(Op.Ed25519Verify, Op.Secp256k1Verify, Op.Bip340Verify, Op.BlsVerify, Op.BlsAggregateVerify, Op.SchnorrVerify, Op.SigmaVerify)

  /** The outcome's category: a boolean, a 20-byte address, or the error's kind. */
  private def outcome(r: Option[Either[JlvmError, CanonicalValue]]): String = r match {
    case scala.None                             => "unimplemented"
    case Some(Right(True)) | Some(Right(False)) => "boolean"
    case Some(Right(CBytes(a))) if a.size == 20 => "address"
    case Some(Right(other))                     => s"value $other"
    case Some(Left(e))                          => e.getClass.getSimpleName
  }

  private val errors = Set("Type", "Arity", "Crypto")

  /** Well-formed keys and signatures, so that generated arguments also get past decoding. */
  private val knownBytes: List[ByteVector] = List(
    hex"0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798",
    hex"03cb0d49e4eae7e57059a3da8ac52626d26fc11330af8fb093fa597d8b93deb7b1",
    ByteVector.low(33),
    hex"5866666666666666666666666666666666666666666666666666666666666666",
    ByteVector.fromValidHex(quorum.head.drop(2)),
    ByteVector.fromValidHex(quorumSignature.drop(2))
  )

  /** Exact widths of the schemes' keys, signatures, digests and proofs, with plausible first bytes. */
  private val biasedBytes: Gen[CanonicalValue] =
    for {
      n    <- Gen.oneOf(0, 1, 24, 31, 32, 33, 47, 48, 49, 55, 56, 57, 63, 64, 65, 66, 95, 96, 97, 128)
      head <- Gen.oneOf(0x00, 0x02, 0x03, 0x04, 0x1b, 0x1c, 0x80, 0xa0, 0xc0, 0xe0, 0xff).map(_.toByte)
      tail <- Gen.listOfN(math.max(n - 1, 0), Gen.choose(Byte.MinValue, Byte.MaxValue))
    } yield CBytes(if (n == 0) ByteVector.empty else ByteVector(head +: tail))

  private def array(xs: Seq[CanonicalValue]): CanonicalValue =
    CanonicalValue.array(xs.toVector).fold(e => sys.error(e.toString), identity)

  private val cryptoArg: Gen[CanonicalValue] = Gen.frequency(
    6 -> biasedBytes,
    2 -> Gen.oneOf(knownBytes).map(CBytes(_)),
    1 -> Gen.oneOf[CanonicalValue](CNull, True, CInt(BigInt(7)), text("0xabc")),
    1 -> Gen.choose(0, 4).flatMap(Gen.listOfN(_, biasedBytes)).map(array)
  )

  test("every signature, recovery and sigma op is total: random and exact-width arguments give a value or a typed error") { b =>
    val ops = Gen.oneOf(Op.Ecrecover :: signatureOps)
    forall(Gen.zip(ops, Gen.choose(0, 4).flatMap(Gen.listOfN(_, cryptoArg)).map(_.toVector))) {
      case (op, args) =>
        val o = outcome(b.ops(op, args))
        val allowed = if (op == Op.Ecrecover) errors + "address" else errors + "boolean"
        expect(allowed(o), s"${op.wire}: unexpected outcome $o")
    }
  }

  test("structured hostile input to the signature and sigma ops gives the typed error the specs define") { b =>
    IO {
      def is(op: Op, args: CanonicalValue*)(want: String) = {
        val o = outcome(b.ops(op, args.toVector))
        expect(o == want, s"${op.wire}(${args.map(_.toString.take(48)).mkString(", ")}): expected $want, got $o")
      }
      def arr(vs:   CanonicalValue*): CanonicalValue = array(vs)
      def node(kvs: (String, CanonicalValue)*): CanonicalValue = CanonicalValue.map(kvs.toList).fold(e => sys.error(e.toString), identity)
      def int(n:    Int): CanonicalValue = CInt(BigInt(n))
      val g = CBytes(hex"0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798")
      val identity33 = CBytes(ByteVector.low(33))
      val msg = CBytes(hex"00")
      val blsKey = CBytes(ByteVector.fromValidHex(quorum.head.drop(2)))
      val aggregate = CBytes(ByteVector.fromValidHex(quorumSignature.drop(2)))
      val proof56 = CBytes(ByteVector.fill(24)(1) ++ ByteVector.low(32))
      val zIsQ = CBytes(ByteVector.fill(24)(1) ++ Secp256k1Group.scalarBytes(Secp256k1Group.Order))
      def dlog(pk: CanonicalValue) = node("type" -> text("dlog"), "pk" -> pk)
      val deep = (0 until 80).foldLeft(dlog(g))((acc, _) => node("type" -> text("and"), "children" -> arr(acc)))
      val dhtuple = node("type" -> text("dhtuple"), "g" -> g, "h" -> g, "u" -> identity33, "v" -> identity33)
      val threshold = node("type" -> text("threshold"), "k" -> int(5), "children" -> arr(dlog(g), dlog(g)))
      val wide = node("type" -> text("or"), "children" -> arr(Vector.fill(256)(dlog(g))*))
      val bls =
        is(Op.BlsVerify)("Arity")
          .and(is(Op.BlsVerify, msg, msg)("Arity"))
          .and(is(Op.BlsVerify, int(1), int(2), int(3))("Type"))
          .and(is(Op.BlsVerify, text("0xabc"), msg, aggregate)("Type"))
          .and(is(Op.BlsVerify, msg, msg, msg)("Crypto"))
          .and(is(Op.BlsVerify, CBytes(ByteVector.fill(48)(0x11)), msg, CBytes(ByteVector.fill(96)(0x33)))("Crypto"))
          .and(is(Op.BlsAggregateVerify)("Arity"))
          .and(expect.same(Some(Right(False)), b.ops(Op.BlsAggregateVerify, Vector(arr(), msg, aggregate))))
          .and(is(Op.BlsAggregateVerify, msg, msg, aggregate)("Type"))
          .and(is(Op.BlsAggregateVerify, arr(Vector.fill(5000)(msg)*), msg, aggregate)("Crypto"))
          .and(is(Op.BlsAggregateVerify, arr(int(1)), int(2), int(3))("Type"))
          .and(is(Op.BlsAggregateVerify, arr(blsKey), msg, CBytes(ByteVector.fill(96)(0x33)))("Crypto"))
      val schnorr =
        is(Op.SchnorrVerify)("Arity")
          .and(is(Op.SchnorrVerify, g, msg)("Arity"))
          .and(is(Op.SchnorrVerify, int(1), int(2), int(3))("Type"))
          .and(is(Op.SchnorrVerify, CBytes(hex"04" ++ ByteVector.fill(32)(1)), msg, proof56)("Crypto"))
          .and(is(Op.SchnorrVerify, CBytes(hex"02" ++ ByteVector.high(32)), msg, proof56)("Crypto"))
          .and(is(Op.SchnorrVerify, identity33, msg, proof56)("Crypto"))
          .and(is(Op.SchnorrVerify, g, msg, CBytes(hex"dead"))("Crypto"))
          .and(is(Op.SchnorrVerify, g, msg, zIsQ)("Crypto"))
          .and(is(Op.SchnorrVerify, g, text("0xabc"), proof56)("Type"))
          .and(is(Op.SchnorrVerify, g, msg, proof56)("boolean"))
      val sigma =
        is(Op.SigmaVerify)("Arity")
          .and(is(Op.SigmaVerify, dlog(g), proof56)("Arity"))
          .and(is(Op.SigmaVerify, msg, proof56, msg)("Crypto"))
          .and(is(Op.SigmaVerify, node("type" -> text("xor"), "pk" -> g), proof56, msg)("Crypto"))
          .and(is(Op.SigmaVerify, node("type" -> text("dlog")), proof56, msg)("Crypto"))
          .and(is(Op.SigmaVerify, dlog(identity33), proof56, msg)("Crypto"))
          .and(is(Op.SigmaVerify, dhtuple, proof56, msg)("Crypto"))
          .and(is(Op.SigmaVerify, threshold, proof56, msg)("Crypto"))
          .and(is(Op.SigmaVerify, dlog(g), zIsQ, msg)("Crypto"))
          .and(is(Op.SigmaVerify, dlog(g), CBytes(ByteVector.low(200000)), msg)("Crypto"))
          .and(is(Op.SigmaVerify, deep, proof56, msg)("Crypto"))
          .and(is(Op.SigmaVerify, wide, proof56, msg)("Crypto"))
          .and(is(Op.SigmaVerify, dlog(g), proof56, int(1))("Type"))
          .and(is(Op.SigmaVerify, dlog(g), proof56, text("0xZZ"))("Type")) // not 0x-hex, so text, not bytes
          .and(is(Op.SigmaVerify, dlog(g), proof56, msg)("boolean"))
      bls.and(schnorr).and(sigma)
    }
  }

  /** Random, mostly malformed proposition trees: unknown types, missing and extra fields, any k, identity points. */
  private def sigmaNode(depth: Int): Gen[CanonicalValue] = {
    def m(kvs: List[(String, CanonicalValue)]) = CanonicalValue.map(kvs).fold(e => sys.error(e.toString), identity)
    val point = Gen.frequency(4 -> Gen.oneOf(knownBytes).map(CBytes(_): CanonicalValue), 3 -> biasedBytes)
    val tpe = Gen.oneOf("dlog", "dhtuple", "and", "or", "threshold", "xor", "", "DLOG").map(text)
    val leaf = for {
      t      <- tpe
      fields <- Gen.someOf("pk", "g", "h", "u", "v", "extra")
      pts    <- Gen.listOfN(fields.size, point)
    } yield m(("type" -> t) :: fields.toList.zip(pts))
    if (depth <= 0) leaf
    else
      Gen.frequency(
        3 -> leaf,
        2 -> (for {
          t  <- tpe
          k  <- Gen.choose(-2, 8)
          n  <- Gen.choose(0, 5)
          cs <- Gen.listOfN(n, sigmaNode(depth - 1))
        } yield m(List("type" -> t, "k" -> CInt(BigInt(k)), "children" -> array(cs))))
      )
  }

  /** Proofs at and around the length of small trees, including a response z = q. */
  private val sigmaProof: Gen[CanonicalValue] = Gen.frequency(
    3 -> (for {
      leaves <- Gen.choose(0, 8)
      delta  <- Gen.choose(-1, 1)
      bs     <- Gen.listOfN(math.max(24 + leaves * 56 + delta, 0), Gen.choose(Byte.MinValue, Byte.MaxValue))
    } yield CBytes(ByteVector(bs))),
    1 -> Gen.const(CBytes(ByteVector.fill(24)(1) ++ Secp256k1Group.scalarBytes(Secp256k1Group.Order))),
    1 -> biasedBytes
  )

  test("sigma_verify is total over random proposition trees and proofs") { b =>
    forall(Gen.zip(sigmaNode(4), sigmaProof, Gen.oneOf(biasedBytes, Gen.const(CInt(BigInt(1)): CanonicalValue)))) {
      case (prop, proof, msg) =>
        val o = outcome(b.ops(Op.SigmaVerify, Vector(prop, proof, msg)))
        expect(errors(o) || o == "boolean", s"unexpected outcome $o")
    }
  }

  // ---- sigma gates and gas (metakit's worked examples; jlvm.md §6-§7)

  test("gates: a 2-of-3 issuer threshold and a DH-tuple statement decide 'authorized' or 'rejected'") { b =>
    val issuers = Vector(BigInt(11), BigInt(12), BigInt(13))
    val message = ByteVector("mint 100 to carol, nonce 7".getBytes(UTF_8))
    val thresholdGate =
      """{"if": [{"sigma_verify": [
        |  {"type": "threshold", "k": 2, "children": {"map": [{"var": "issuers"}, {"type": "dlog", "pk": {"var": "$current"}}]}},
        |  {"var": "proof"}, {"var": "msg"}]}, "authorized", "rejected"]}""".stripMargin
    def issuerData(proof: ByteVector) =
      s"""{"issuers": [${issuers.map(w => h(pk(w))).mkString(", ")}], "proof": ${h(proof)}, "msg": ${h(message)}}"""
    val g = Secp256k1Group.Generator
    val base = g * BigInt(99)
    val w = BigInt(4242)
    val dhGate =
      """{"if": [{"sigma_verify": [{"type": "dhtuple", "g": {"var": "g"}, "h": {"var": "h"}, "u": {"var": "u"}, "v": {"var": "v"}},
        |  {"var": "proof"}, {"var": "msg"}]}, "authorized", "rejected"]}""".stripMargin
    def dhData(v: ByteVector, proof: ByteVector) =
      s"""{"g": ${h(g.bytes)}, "h": ${h(base.bytes)}, "u": ${h((g * w).bytes)}, "v": ${h(v)}, "proof": ${h(proof)}, "msg": ${h(message)}}"""
    (
      prove(b.random, SigmaProp.threshold(2, issuers.map(i => key(Secp256k1Group.base(i)))), Vector(issuers(0), issuers(2)), message),
      prove(b.random, Right(tuple(g, base, g * w, base * w)), Vector(w), message)
    ).mapN { (tp, dp) =>
      expect
        .same(Right(text("authorized")), run(b.jlvm, thresholdGate, issuerData(tp)).result)
        .and(expect.same(Right(text("rejected")), run(b.jlvm, thresholdGate, issuerData(tp.update(30, (tp(30) ^ 1).toByte))).result))
        .and(expect.same(Right(text("authorized")), run(b.jlvm, dhGate, dhData((base * w).bytes, dp)).result))
        .and(expect.same(Right(text("rejected")), run(b.jlvm, dhGate, dhData((base * (w + 1)).bytes, dp)).result))
    }
  }

  test("sigma_verify gas: a DH-tuple leaf costs about twice a dlog leaf; a limit short of the charge runs out first") { b =>
    val w = BigInt(4242)
    val g = Secp256k1Group.Generator
    val base = g * BigInt(99)
    val message = ByteVector.fill(32)(1)
    def dlogProgram(proof: ByteVector) = s"""{"sigma_verify": [{"type": "dlog", "pk": ${h(pk(w))}}, ${h(proof)}, ${h(message)}]}"""
    def dhProgram(proof: ByteVector) =
      s"""{"sigma_verify": [{"type": "dhtuple", "g": ${h(g.bytes)}, "h": ${h(base.bytes)}, "u": ${h((g * w).bytes)}, "v": ${h(
          (base * w).bytes
        )}}, ${h(proof)}, ${h(message)}]}"""
    def at(program: String, limit: Long) =
      b.jlvm
        .evaluate(Parser.parse(program).fold(e => sys.error(e.toString), identity), CNull, Map.empty, Config(limit = GasLimit(Gas(limit))))
    (
      prove(b.random, SigmaProp.dlog(Secp256k1Group.base(w)), Vector(w), message),
      prove(b.random, Right(tuple(g, base, g * w, base * w)), Vector(w), message)
    ).mapN { (dl, dh) =>
      val dlog = at(dlogProgram(dl), GasLimit.Default.amount.value)
      val dhtuple = at(dhProgram(dh), GasLimit.Default.amount.value)
      val used = dhtuple.gasUsed.value
      val ratio = used.toDouble / dlog.gasUsed.value.toDouble
      // the operation's size charge is the last before its work (jlvm.md §7), so one gas short stops before the curve
      val starved = at(dhProgram(dh), used - 1)
      expect
        .same(Right(True), dlog.result)
        .and(expect.same(Right(True), dhtuple.result))
        .and(expect(ratio > 1.6 && ratio < 2.4, s"dhtuple / dlog = $ratio (${dhtuple.gasUsed} / ${dlog.gasUsed})"))
        .and(expect.same(Left(JlvmError.OutOfGas(used, used - 1)), starved.result))
        .and(expect.same(Gas(used - 1), starved.gasUsed))
    }
  }
}
