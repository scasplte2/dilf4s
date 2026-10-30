package xyz.kd5ujc.crypto.sigma

import cats.Show
import cats.syntax.all._

import org.scalacheck.Gen
import scodec.bits.{ByteVector, hex}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object SigmaSuite extends SimpleIOSuite with Checkers {

  import SigmaProp._

  // ---- GF(2^192)

  private val element: Gen[Gf2_192] =
    Gen.listOfN(24, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(bs => Gf2_192.fromBytes(ByteVector(bs)).get)

  private given Show[Gf2_192] = Show.show(_.bytes.toHex)
  private given Show[(Gf2_192, Gf2_192, Gf2_192)] = Show.show { case (a, b, c) => s"${a.bytes.toHex}/${b.bytes.toHex}/${c.bytes.toHex}" }

  test("GF(2^192) is a field: associative, distributive, with inverses; bytes round-trip") {
    forall(Gen.zip(element, element, element)) {
      case (a, b, c) =>
        expect
          .same((a * b) * c, a * (b * c))
          .and(expect.same(a * (b + c), a * b + a * c))
          .and(expect.same(a * b, b * a))
          .and(expect.same(Gf2_192.One, if (a.isZero) Gf2_192.One else a * a.inverse))
          .and(expect.same(Option(a), Gf2_192.fromBytes(a.bytes)))
    }
  }

  pureTest("x^192 reduces by the pentanomial x^7 + x^2 + x + 1") {
    val x = Gf2_192.byte(2)
    val x192 = (1 to 192).foldLeft(Gf2_192.One)((p, _) => p * x)
    expect.same(Gf2_192.fromBytes(hex"87" ++ ByteVector.fill(23)(0)), Option(x192))
  }

  test("interpolation passes through its points and keeps the requested number of coefficients") {
    forall(Gen.zip(element, Gen.choose(0, 6), Gen.listOfN(6, element))) {
      case (at0, n, ys) =>
        val points = ys.take(n).zipWithIndex.map { case (y, i) => (i + 1, y) }.toVector
        val q = Gf2Poly.interpolate(at0, points)
        expect
          .same(n + 1, q.coefficients.size)
          .and(expect.same(at0, q(0)))
          .and(expect.same(points.map(_._2), points.map(p => q(p._1))))
    }
  }

  // ---- the group

  pureTest("group elements decode only in canonical form; the identity is 33 zero bytes") {
    val g = Secp256k1Group.Generator.bytes
    val uncompressed = ByteVector.view(Secp256k1Group.Generator.point.getEncoded(false))
    expect
      .same(Right(Secp256k1Group.Generator), Secp256k1Group.decode(g))
      .and(expect.same(Right(Secp256k1Group.Identity), Secp256k1Group.decode(ByteVector.fill(33)(0))))
      .and(expect.same(ByteVector.fill(33)(0), (Secp256k1Group.Generator * Secp256k1Group.Order).bytes))
      .and(expect(Secp256k1Group.decode(uncompressed).isLeft, "uncompressed"))
      .and(expect(Secp256k1Group.decode(hex"02" ++ ByteVector.fill(32)(0xff)).isLeft, "x beyond the field"))
      .and(expect(Secp256k1Group.decode(g.drop(1)).isLeft, "short"))
  }

  // ---- Ergo's known-answer vectors (sigmastate 5.0.13, SigningSpecification; cross-checked there with sigma-rust)

  private val msg = hex"1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00"
  private def dlog(sk: String): SigmaProp = Dlog(Secp256k1Group.base(BigInt(sk)))
  private val sk1 = "109749205800194830127901595352600384558037183218698112947062497909408298157746"
  private val sk2 = "50415569076448343263191022044468203756975150511337537963383000142821297891310"
  private val sk3 = "34648336872573478681093104997365775365807654884817677358848426648354905397359"

  private def dht(b: ByteVector): SigmaProp =
    b.grouped(33).toVector.traverse(Secp256k1Group.decode) match {
      case Right(Vector(g, h, u, v)) => DhTuple(g, h, u, v)
      case other                     => sys.error(s"bad DH tuple: $other")
    }

  private val dht1 = dht(
    hex"0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980280c66feee88d56e47bf3f47c4109d9218c60c373a472a0d9537507c7ee828c4802a96f19e97df31606183c1719400682d1d40b1ce50c9a1ed1b19845e2b1b551bf0255ac02191cb229891fb1b674ea9df7fc8426350131d821fc4a53f29c3b1cb21a"
  )
  private val dht2 = dht(
    hex"0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f817980214487635ebffa60b13a166bd0721c5f0ab603fc74168d7764d7ec5ef2107f5d40334c5b7efa5a4a22b83d102d2e6521eaa660fa911c5a213af63c8460f2327513b026a0be2a277291d42daad3830cb16a4ef20e4f1f7c36384f3fee065f0f143a355"
  )

  private def p(e: Either[String, SigmaProp]): SigmaProp = e.fold(sys.error, identity)

  private val vectors: List[(String, SigmaProp, ByteVector)] = List(
    (
      "dlog",
      dlog(sk1),
      hex"bcb866ba434d5c77869ddcbc3f09ddd62dd2d2539bf99076674d1ae0c32338ea95581fdc18a3b66789904938ac641eba1a66d234070207a2"
    ),
    (
      "dhtuple",
      dht1,
      hex"eba93a69b28cfdea261e9ea8914fca9a0b3868d50ce68c94f32e875730f8ca361bd3783c5d3e25802e54f49bd4fb9fafe51f4e8aafbf9815"
    ),
    (
      "and",
      p(SigmaProp.and(Vector(dlog(sk1), dlog(sk2)))),
      hex"9b2ebb226be42df67817e9c56541de061997c3ea84e7e72dbb69edb7318d7bb525f9c16ccb1adc0ede4700a046d0a4ab1e239245460c1ba45e5637f7a2d4cc4cc460e5895125be73a2ca16091db2dcf51d3028043c2b9340"
    ),
    (
      "or",
      p(SigmaProp.or(Vector(dlog(sk1), dlog(sk2)))),
      hex"ec94d2d5ef0e1e638237f53fd883c339f9771941f70020742a7dc85130aaee535c61321aa1e1367befb500256567b3e6f9c7a3720baa75ba6056305d7595748a93f23f9fc0eb9c1aaabc24acc4197030834d76d3c95ede60c5b59b4b306cd787d010e8217f34677d046646778877c669"
    ),
    (
      "or with a DH tuple",
      p(SigmaProp.or(Vector(dlog(sk1), dht2))),
      hex"a80daebdcd57874296f49fd9910ddaefbf517ca076b6e16b97678e96a20239978836e7ec5b795cf3a55616d394f07c004f85e0d3e71880d4734b57ea874c7eba724e8887280f1affadaad962ee916b39207af2d2ab2a69a2e6f4d652f7389cc4f582bbe6d7937c59aa64cf2965a8b36a"
    ),
    (
      "and with or",
      p(SigmaProp.or(Vector(dlog(sk2), dlog(sk3))).flatMap(o => SigmaProp.and(Vector(dlog(sk1), o)))),
      hex"397e005d85c161990d0e44853fbf14951ff76e393fe1939bb48f68e852cd5af028f6c7eaaed587f6d5435891a564d8f9a77288773ce5b526a670ab0278aa4278891db53a9842df6fba69f95f6d55cfe77dd7b4bdccc1a3378ac4524b51598cb813258f64c94e98c3ef891a6eb8cbfd2e527a9038ca50b5bb50058de55a859a169628e6ae5ba4cb0332c694e450782d6f"
    ),
    (
      "or with and",
      p(SigmaProp.and(Vector(dlog(sk2), dlog(sk3))).flatMap(a => SigmaProp.or(Vector(dlog(sk1), a)))),
      hex"a58b251be319a9656c21876b1136a59f42b18835dec6076c92f7a925ba28d2030218c177ab07563003eff5250cfafeb631ef610f4d710ab8e821bf632203adf23f4376580eaa17ddb36c0138f73a88551f45d92cde2b66dfbb5906c02e4d48106ff08be4a2fc29ec242f495468692f9ddeeb029dc5d8f38e2649cf09c44b67cbcfb3de4202026fb84d23ce2b4ff0f69b"
    ),
    (
      "2-of-3 threshold",
      p(
        SigmaProp.threshold(
          2,
          Vector(
            dlog("416167686186183758173232992934554728075978573242452195968805863126437865059"),
            dlog(sk3),
            dlog(sk2)
          )
        )
      ),
      hex"0b6bf9bc42c7b509ab56c76318c0891b2c8d44ef5fafb1379cc6b72b89c53cd43f8ef10158ce08646301d09b450ea83a1cdbbfc3dc7438ece4bbe934919069c50ec5857209b0dbf120b325c88667bc84580720ff4b3c371ec752bc6874c933f7fa53fae411e65ae07b647d365caac8c6744276c04c0240dd55e1f62c0e17a093dd91493c68104b1e01a4069017668d3f"
    )
  )

  pureTest("the dlog vector's public key matches Ergo's") {
    expect.same(hex"03cb0d49e4eae7e57059a3da8ac52626d26fc11330af8fb093fa597d8b93deb7b1", Secp256k1Group.base(BigInt(sk1)).bytes)
  }

  vectors.foreach {
    case (name, prop, proof) =>
      pureTest(s"Ergo vector: $name verifies; another message, a flipped byte or a changed length do not") {
        val flipped = proof.update(proof.size - 1, (proof(proof.size - 1) ^ 1).toByte)
        expect
          .same(Right(true), Sigma.verify(prop, proof, msg))
          .and(expect.same(Right(false), Sigma.verify(prop, proof, msg.reverse)))
          .and(expect(Sigma.verify(prop, flipped, msg) != Right(true), "a flipped response byte"))
          .and(expect(Sigma.verify(prop, proof ++ hex"00", msg).isLeft, "trailing bytes (Ergo ignores them)"))
          .and(expect(Sigma.verify(prop, proof.dropRight(1), msg).isLeft, "a short proof"))
      }
  }

  pureTest("trivial propositions are rejected: they would verify for anyone") {
    expect(SigmaProp.and(Vector.empty).isLeft, "empty and")
      .and(expect(SigmaProp.or(Vector.empty).isLeft, "empty or"))
      .and(expect(SigmaProp.threshold(0, Vector(dlog(sk1))).isLeft, "k = 0"))
      .and(expect(SigmaProp.threshold(2, Vector(dlog(sk1))).isLeft, "k > n"))
  }

  // ---- our prover

  private val secrets: Vector[BigInt] = Vector(sk1, sk2, sk3).map(BigInt(_))
  private val keys: Vector[SigmaProp] = secrets.map(w => Dlog(Secp256k1Group.base(w)))
  private val stranger: SigmaProp = Dlog(Secp256k1Group.base(BigInt(7)))

  // ZeroJoin (Ergo's MixExampleSpecification). Alice's half-mix box holds gX = x·G. Bob spends it with secret y into
  // two full-mix outputs holding (c1, c2) = (y·G, xy·G) in some order; the half-mix demands that he prove
  // dht(g, gX, c1, c2) or dht(g, gX, c2, c1), which hides which output is which.
  private val (x, y) = (BigInt(11), BigInt(13))
  private val g = Secp256k1Group.Generator
  private val (c1, c2) = (g * y, g * (x * y))
  private val halfMix = p(SigmaProp.or(Vector(DhTuple(g, g * x, c2, c1), DhTuple(g, g * x, c1, c2))))

  // Each full-mix output is spendable as dlog(c2) or dht(g, c1, gX, c2): Alice, knowing x, proves the second
  // disjunct for the output where c2 = xy·G; Bob, knowing y, proves the first for the other.
  private val fullMixAlice = p(SigmaProp.or(Vector(Dlog(c2), DhTuple(g, c1, g * x, c2))))
  private val fullMixBob = p(SigmaProp.or(Vector(Dlog(c1), DhTuple(g, c2, g * x, c1))))

  private val cases: List[(String, SigmaProp, Vector[BigInt])] = List(
    ("dlog", keys(0), Vector(secrets(0))),
    ("and", p(SigmaProp.and(keys.take(2))), secrets.take(2)),
    ("ring of three, signed by the second", p(SigmaProp.or(keys)), Vector(secrets(1))),
    ("ring with a stranger, signed by the last", p(SigmaProp.or(Vector(stranger, keys(2)))), Vector(secrets(2))),
    ("2-of-3 by the first and the last", p(SigmaProp.threshold(2, keys)), Vector(secrets(0), secrets(2))),
    ("3-of-4 nested in an and", p(SigmaProp.threshold(3, keys :+ stranger).flatMap(t => SigmaProp.and(Vector(keys(0), t)))), secrets),
    (
      "an or of thresholds",
      p((SigmaProp.threshold(2, keys), SigmaProp.threshold(1, Vector(stranger))).tupled.flatMap {
        case (a, b) => SigmaProp.or(Vector(b, a))
      }),
      secrets.take(2)
    ),
    ("ZeroJoin: Bob spends the half-mix", halfMix, Vector(y)),
    ("ZeroJoin: Alice spends her full-mix output", fullMixAlice, Vector(x)),
    ("ZeroJoin: Bob spends his", fullMixBob, Vector(y))
  )

  cases.foreach {
    case (name, prop, ws) =>
      pureTest(s"our prover: $name verifies; a fresh seed gives a different proof; another message does not verify") {
        val one = Sigma.verify(prop, SigmaProver.proveWith(prop, ws, msg, hex"01").toOption.get, msg)
        val proofs = List(hex"01", hex"02").map(seed => SigmaProver.proveWith(prop, ws, msg, seed))
        expect
          .same(Right(true), one)
          .and(expect(proofs.distinct.size == 2, "fresh randomness"))
          .and(expect.same(Right(false), proofs.head.flatMap(Sigma.verify(prop, _, msg.reverse))))
          .and(expect.same(Right(Sigma.proofLength(prop)), proofs.head.map(_.size)))
      }
  }

  pureTest("our prover refuses secrets that do not satisfy the proposition") {
    expect(SigmaProver.proveWith(p(SigmaProp.threshold(2, keys)), Vector(secrets(0)), msg, hex"01").isLeft, "one of two")
      .and(expect(SigmaProver.proveWith(keys(0), Vector(BigInt(7)), msg, hex"01").isLeft, "the wrong key"))
  }

  test("random rings: any member can sign, and the proof verifies") {
    forall(Gen.zip(Gen.choose(1, 6), Gen.choose(0, 5), Gen.choose(1L, Long.MaxValue))) {
      case (n, who, seed) =>
        val ws = (1 to n).toVector.map(i => BigInt(seed) * i + 1)
        val ring = p(SigmaProp.or(ws.map(w => Dlog(Secp256k1Group.base(w)))))
        val proof = SigmaProver.proveWith(ring, Vector(ws(who % n)), msg, ByteVector.fromLong(seed))
        expect.same(Right(true), proof.flatMap(Sigma.verify(ring, _, msg)))
    }
  }
}
