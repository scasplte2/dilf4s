package xyz.kd5ujc.jlvm

import cats.effect.std.SecureRandom
import cats.effect.{IO, Resource}
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.canonical.CanonicalValue._
import xyz.kd5ujc.crypto.bls.Bls12381
import xyz.kd5ujc.crypto.sigma.{Secp256k1Group, SigmaProp, SigmaProver}
import xyz.kd5ujc.crypto.signing._

import scodec.bits.ByteVector
import weaver.{Expectations, IOSuite}

object CryptoOpsSuite extends IOSuite {

  final case class Backends(jlvm: Jlvm, secp: Secp256k1, bls: Bls12381, random: SecureRandom[IO])
  type Res = Backends
  def sharedResource: Resource[IO, Backends] =
    Resource.eval((Jlvm.load[IO], Secp256k1.load[IO], Bls12381.load[IO], SecureRandom.javaSecuritySecureRandom[IO]).mapN(Backends.apply))

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
        .and(expect(math.abs(separate.gasUsed.value - single.gasUsed.value) < 200, s"${separate.gasUsed} vs ${single.gasUsed}"))
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
    val ring = SigmaProp.or(ws.map(w => SigmaProp.Dlog(Secp256k1Group.base(w))))
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
      prove(b.random, Right(SigmaProp.Dlog(Secp256k1Group.base(bob))), Vector(bob), message),
      prove(b.random, Right(SigmaProp.Dlog(Secp256k1Group.base(alice))), Vector(alice), message)
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
    // Bob puts Alice's output (c2 = g^xy) second; his proof covers whichever order he chose
    val outputs = List(out(gy, gxy), out(gxy, gy))
    val prop = SigmaProp.or(
      Vector(
        SigmaProp.DhTuple(g, g * x, Secp256k1Group.decode(gy).toOption.get, Secp256k1Group.decode(gxy).toOption.get),
        SigmaProp.DhTuple(g, g * x, Secp256k1Group.decode(gxy).toOption.get, Secp256k1Group.decode(gy).toOption.get)
      )
    )
    prove(b.random, prop, Vector(y), message).map { proof =>
      expect
        .same(Right(True), run(b.jlvm, program, data(outputs, proof)).result)
        .and(expect.same(Right(False), run(b.jlvm, program, data(List(out(gy, gxy), out(gy, gxy)), proof)).result))
        .and(expect.same(Right(False), run(b.jlvm, program, data(List(out(gy, gxy), out(gxy, gy, value = 9)), proof)).result))
        .and(expect(crypto(run(b.jlvm, program, data(outputs, proof.drop(1))).result), "a short proof is malformed"))
    }
  }

  test("sigma_verify: malformed propositions are crypto errors") { b =>
    IO {
      val p = (prop: String) => run(b.jlvm, s"""{"sigma_verify": [$prop, "0x00", "0x00"]}""", "{}").result
      expect(crypto(p("""{"type": "or", "children": []}""")), "an empty or")
        .and(expect(crypto(p("""{"type": "dlog", "pk": "0x02"}""")), "a short key"))
        .and(expect(crypto(p("""{"type": "dlog", "pk": "0x00", "extra": 1}""")), "an extra field"))
        .and(expect(crypto(p("""{"type": "threshold", "k": 0, "children": [{"type": "dlog", "pk": "0x00"}]}""")), "k = 0"))
    }
  }
}
