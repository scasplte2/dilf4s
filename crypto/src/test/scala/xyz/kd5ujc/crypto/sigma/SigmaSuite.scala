package xyz.kd5ujc.crypto.sigma

import cats.Show
import cats.syntax.all._

import scala.deriving.Mirror

import xyz.kd5ujc.core.canonical.CanonicalValue

import org.bouncycastle.crypto.ec.CustomNamedCurves
import org.scalacheck.Gen
import scodec.bits.{ByteVector, hex}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object SigmaSuite extends SimpleIOSuite with Checkers {

  private def key(pk: GroupElement): SigmaProp = SigmaProp.dlog(pk).fold(sys.error, identity)
  private def tuple(g: GroupElement, h: GroupElement, u: GroupElement, v: GroupElement): SigmaProp =
    SigmaProp.dhTuple(g, h, u, v).fold(sys.error, identity)

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
  private def dlog(sk: String): SigmaProp = key(Secp256k1Group.base(BigInt(sk)))
  private val sk1 = "109749205800194830127901595352600384558037183218698112947062497909408298157746"
  private val sk2 = "50415569076448343263191022044468203756975150511337537963383000142821297891310"
  private val sk3 = "34648336872573478681093104997365775365807654884817677358848426648354905397359"

  private def dht(b: ByteVector): SigmaProp =
    b.grouped(33).toVector.traverse(Secp256k1Group.decode) match {
      case Right(Vector(g, h, u, v)) => tuple(g, h, u, v)
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
  private val keys: Vector[SigmaProp] = secrets.map(w => key(Secp256k1Group.base(w)))
  private val stranger: SigmaProp = key(Secp256k1Group.base(BigInt(7)))

  // ZeroJoin (Ergo's MixExampleSpecification). Alice's half-mix box holds gX = x·G. Bob spends it with secret y into
  // two full-mix outputs holding (c1, c2) = (y·G, xy·G) in some order; the half-mix demands that he prove
  // dht(g, gX, c1, c2) or dht(g, gX, c2, c1), which hides which output is which.
  private val (x, y) = (BigInt(11), BigInt(13))
  private val g = Secp256k1Group.Generator
  private val (c1, c2) = (g * y, g * (x * y))
  private val halfMix = p(SigmaProp.or(Vector(tuple(g, g * x, c2, c1), tuple(g, g * x, c1, c2))))

  // Each full-mix output is spendable as dlog(c2) or dht(g, c1, gX, c2): Alice, knowing x, proves the second
  // disjunct for the output where c2 = xy·G; Bob, knowing y, proves the first for the other.
  private val fullMixAlice = p(SigmaProp.or(Vector(key(c2), tuple(g, c1, g * x, c2))))
  private val fullMixBob = p(SigmaProp.or(Vector(key(c1), tuple(g, c2, g * x, c1))))

  // (g, c1, x·g, x·c1): a DH tuple with witness x, as in Alice's full-mix disjunct; and two keys outside `keys`
  private val dhtX = tuple(g, c1, g * x, c2)
  private val more = Vector(BigInt(21), BigInt(22)).map(w => key(Secp256k1Group.base(w)))

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
    ("ZeroJoin: Bob spends his", fullMixBob, Vector(y)),
    ("an and of a key and a DH tuple", p(SigmaProp.and(Vector(keys(0), dhtX))), Vector(secrets(0), x)),
    (
      "(A or B) and (C or D), signed by B and C",
      p((SigmaProp.or(keys.take(2)), SigmaProp.or(more)).tupled.flatMap { case (ab, cd) => SigmaProp.and(Vector(ab, cd)) }),
      Vector(secrets(1), BigInt(21))
    ),
    (
      "an or of an and and a threshold, signed through the threshold",
      p((SigmaProp.and(Vector(stranger, keys(0))), SigmaProp.threshold(2, keys)).tupled.flatMap {
        case (a, t) => SigmaProp.or(Vector(a, t))
      }),
      Vector(secrets(1), secrets(2))
    ),
    (
      "a 2-of-3 of keys in an and with a DH tuple",
      p(SigmaProp.threshold(2, keys).flatMap(t => SigmaProp.and(Vector(t, dhtX)))),
      Vector(secrets(0), secrets(2), x)
    )
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
        val ring = p(SigmaProp.or(ws.map(w => key(Secp256k1Group.base(w)))))
        val proof = SigmaProver.proveWith(ring, Vector(ws(who % n)), msg, ByteVector.fromLong(seed))
        expect.same(Right(true), proof.flatMap(Sigma.verify(ring, _, msg)))
    }
  }

  pureTest("the identity is no statement point: dlog(O) would be provable by anyone, as w = 0") {
    val o = Secp256k1Group.Identity
    val g = Secp256k1Group.Generator
    expect(SigmaProp.dlog(o).isLeft, "dlog(O)")
      .and(expect(SigmaProp.dhTuple(g, g, o, o).isLeft, "dht with u = v = O (ZeroJoin with c1 = c2 = O)"))
      .and(expect(SigmaProp.dhTuple(o, g, g, g).isLeft, "an identity base"))
      .and(
        expect(
          SigmaProp
            .fromCanonical(
              CanonicalValue
                .map(List("type" -> CanonicalValue.text("dlog").toOption.get, "pk" -> CanonicalValue.CBytes(o.bytes)))
                .toOption
                .get
            )
            .isLeft,
          "decoded"
        )
      )
  }

  private given Show[(Int, Int, Vector[Int], Long)] = Show.fromToString

  test("random thresholds: any k-of-n, with k or more secrets known, proves and verifies") {
    val shape = for {
      n     <- Gen.choose(1, 6)
      k     <- Gen.choose(1, n)
      known <- Gen.choose(k, n)
      who   <- Gen.pick(known, 0 until n)
      seed  <- Gen.choose(1L, Long.MaxValue)
    } yield (n, k, who.toVector.sorted, seed)
    forall(shape) {
      case (n, k, who, seed) =>
        val ws = (1 to n).toVector.map(i => BigInt(seed) * i + 1)
        val prop = p(SigmaProp.threshold(k, ws.map(w => key(Secp256k1Group.base(w)))))
        val proof = SigmaProver.proveWith(prop, who.map(ws), msg, ByteVector.fromLong(seed))
        expect
          .same(Right(true), proof.flatMap(Sigma.verify(prop, _, msg)))
          .and(expect.same(Right(Sigma.proofLength(prop)), proof.map(_.size)))
    }
  }

  // ---- the bounds (sigma.md §3)

  pureTest("a proposition nests at most 64 deep, has 1 to 255 children per connective and at most 4096 leaves") {
    val leaf = keys(0)
    def nest(depth: Int): Either[String, SigmaProp] =
      (1 until depth).foldLeft(leaf.asRight[String])((acc, _) => acc.flatMap(c => SigmaProp.and(Vector(c))))
    def wide(n: Int) = SigmaProp.or(Vector.fill(n)(leaf))
    // groups of 241 leaves: 16 of them are 3856 leaves, 17 are 4097
    def leaves(groups: Int) = SigmaProp.and(Vector.fill(241)(leaf)).flatMap(g => SigmaProp.and(Vector.fill(groups)(g)))
    expect(nest(64).isRight, "64 deep")
      .and(expect(nest(65).isLeft, "65 deep"))
      .and(expect(wide(255).isRight, "255 children"))
      .and(expect(wide(256).isLeft, "256 children"))
      .and(expect(SigmaProp.threshold(1, Vector.fill(256)(leaf)).isLeft, "a threshold of 256 children"))
      .and(expect(leaves(16).isRight, "3856 leaves"))
      .and(expect(leaves(17).isLeft, "4097 leaves"))
  }

  pureTest("a 24-byte challenge read as a scalar is below q, so distinct challenges are distinct scalars") {
    val largest = BigInt(1, ByteVector.fill(Sigma.ChallengeSize.toLong)(0xff).toArray)
    expect
      .same(24, Sigma.ChallengeSize)
      .and(expect(largest < Secp256k1Group.Order, s"2^192 - 1 must be below q = ${Secp256k1Group.Order}"))
  }

  pureTest("a compressed point whose x has no y on secp256k1 does not decode") {
    // 5^3 + 7 = 132 is not a square modulo the field prime
    val x5 = ByteVector.fill(31)(0) :+ 5.toByte
    expect(Secp256k1Group.decode(hex"02" ++ x5).isLeft, "02, x = 5")
      .and(expect(Secp256k1Group.decode(hex"03" ++ x5).isLeft, "03, x = 5"))
  }

  // ---- soundness: the verifier against forgers (sigma.md §5-§7)

  pureTest("forgers who simulate what they cannot prove do not verify: a ring with no witness, a 2-of-3 with one") {
    def leaf(i: Int) = SigmaProp.dlog(Secp256k1Group.base(secrets(i))).toOption.get
    def e(b:    Int) = ByteVector.fill(Sigma.ChallengeSize.toLong)(b)
    def z(n:    Int) = BigInt(1000 + n)
    def zb(k:   BigInt) = Secp256k1Group.scalarBytes(k)

    // A ring of two with no witness: both challenges and responses chosen, then hashed. The verifier derives the last
    // challenge as root XOR e1, which is not the chosen e2.
    val ring = p(SigmaProp.or(keys.take(2)))
    val ringRoot =
      Sigma.challenge(Sigma.fiatShamir(ring, Vector(Sigma.commitment(leaf(0), e(1), z(1)), Sigma.commitment(leaf(1), e(2), z(2)))), msg)
    val ringForgery = ringRoot ++ e(1) ++ zb(z(1)) ++ zb(z(2))

    // 2-of-3 knowing only secrets(0): child 1 real with nonce r, children 2 and 3 simulated with chosen challenges.
    // Q has degree n - k = 1, so the root and child 2's challenge fix it, and Q(3) is not child 3's chosen challenge.
    val t = p(SigmaProp.threshold(2, keys))
    val r = BigInt(777)
    val e2 = Gf2_192.fromBytes(e(2)).get
    val e3 = Gf2_192.fromBytes(e(3)).get
    val commitments =
      Vector(Secp256k1Group.base(r).bytes, Sigma.commitment(leaf(1), e2.bytes, z(2)), Sigma.commitment(leaf(2), e3.bytes, z(3)))
    val root = Sigma.challenge(Sigma.fiatShamir(t, commitments), msg)
    val q = Gf2Poly.interpolate(Gf2_192.fromBytes(root).get, Vector(2 -> e2))
    val z1 = (r + BigInt(1, q(1).bytes.toArray) * secrets(0)).mod(Secp256k1Group.Order)
    val thresholdForgery = root ++ q.higherBytes ++ zb(z1) ++ zb(z(2)) ++ zb(z(3))

    // Controls: the same constructions with the witnesses they need verify, so the forgeries fail for want of them.
    def scalar(c: ByteVector) = BigInt(1, c.toArray)
    val honestRingRoot =
      Sigma.challenge(Sigma.fiatShamir(ring, Vector(Secp256k1Group.base(r).bytes, Sigma.commitment(leaf(1), e(2), z(2)))), msg)
    val e1 = honestRingRoot ^ e(2)
    val honestRing = honestRingRoot ++ e1 ++ zb((r + scalar(e1) * secrets(0)).mod(Secp256k1Group.Order)) ++ zb(z(2))
    val r2 = BigInt(888)
    val honestCommitments = Vector(Secp256k1Group.base(r).bytes, Secp256k1Group.base(r2).bytes, Sigma.commitment(leaf(2), e3.bytes, z(3)))
    val honestRoot = Sigma.challenge(Sigma.fiatShamir(t, honestCommitments), msg)
    val hq = Gf2Poly.interpolate(Gf2_192.fromBytes(honestRoot).get, Vector(3 -> e3))
    def answer(i: Int, nonce: BigInt) = zb((nonce + scalar(hq(i + 1).bytes) * secrets(i)).mod(Secp256k1Group.Order))
    val honestThreshold = honestRoot ++ hq.higherBytes ++ answer(0, r) ++ answer(1, r2) ++ zb(z(3))

    expect
      .same(Right(false), Sigma.verify(ring, ringForgery, msg))
      .and(expect(q(3) != e3, "the polynomial cannot also pass through the third chosen challenge"))
      .and(expect.same(Right(false), Sigma.verify(t, thresholdForgery, msg)))
      .and(expect.same(Right(true), Sigma.verify(ring, honestRing, msg)))
      .and(expect.same(Right(true), Sigma.verify(t, honestThreshold, msg)))
  }

  pureTest("a proof is bound to its proposition: another of the same length gives false, another length is malformed") {
    val dlogProof = SigmaProver.proveWith(keys(0), Vector(secrets(0)), msg, hex"01").toOption.get
    val andProof = SigmaProver.proveWith(p(SigmaProp.and(keys.take(2))), secrets.take(2), msg, hex"01").toOption.get
    expect
      .same(Sigma.proofLength(keys(0)), Sigma.proofLength(dhtX))
      .and(expect.same(Right(false), Sigma.verify(dhtX, dlogProof, msg)))
      .and(expect(Sigma.verify(p(SigmaProp.and(keys)), andProof, msg).isLeft, "a proof for two children against three"))
      .and(expect(Sigma.verify(keys(0), andProof, msg).isLeft, "an and proof against a single leaf"))
  }

  pureTest("a DH-tuple proof does not verify once any statement point changes, or for a tuple that is not DH") {
    val w = BigInt(4242)
    val h = g * BigInt(99)
    val good = tuple(g, h, g * w, h * w)
    val proof = SigmaProver.proveWith(good, Vector(w), msg, hex"01").toOption.get
    val other = g * BigInt(5)
    val notDh = tuple(g, h, g * w, h * (w + 1))
    val variants = List(
      "u and v swapped" -> tuple(g, h, h * w, g * w),
      "not a DH tuple"  -> notDh,
      "another base g"  -> tuple(other, h, g * w, h * w),
      "another base h"  -> tuple(g, other, g * w, h * w)
    )
    expect
      .same(Right(true), Sigma.verify(good, proof, msg))
      .and(variants.foldMap { case (name, prop) => expect(Sigma.verify(prop, proof, msg) == Right(false), name) })
      .and(expect(SigmaProver.proveWith(notDh, Vector(w), msg, hex"01").isLeft, "no proof of a tuple that is not DH"))
  }

  // ---- propositions built around the smart constructors (Scala 3 product mirrors) are checked at the boundary

  private val mirrorDlog = summon[Mirror.ProductOf[SigmaProp.Dlog]]
  private val mirrorAnd = summon[Mirror.ProductOf[SigmaProp.And]]
  private val mirrorThreshold = summon[Mirror.ProductOf[SigmaProp.Threshold]]

  /** A proof of `dlog(pk)` from `w`, made by hand so that nothing checks the statement: nonce `r`, `z = r + e·w`. */
  private def handProof(prop: SigmaProp, w: BigInt): ByteVector = {
    val r = BigInt(5)
    val e = Sigma.challenge(Sigma.fiatShamir(prop, Vector(Secp256k1Group.base(r).bytes)), msg)
    e ++ Secp256k1Group.scalarBytes((r + BigInt(1, e.toArray) * w).mod(Secp256k1Group.Order))
  }

  pureTest("a mirror-built dlog of the identity is rejected by the verifier, though its proof (w = 0) checks out") {
    val forged = mirrorDlog.fromProduct(Tuple1(Secp256k1Group.Identity))
    val proof = handProof(forged, BigInt(0))
    expect
      .same(56L, proof.size)
      .and(expect(Sigma.verify(forged, proof, msg).isLeft, "the identity is no statement"))
      .and(expect.same(Right(true), Sigma.verify(dlog(sk1), handProof(dlog(sk1), BigInt(sk1)), msg)))
  }

  pureTest("a mirror-built empty connective is rejected by the verifier and the prover; so is a bad threshold") {
    val empty = mirrorAnd.fromProduct(Tuple1(Vector.empty[SigmaProp]))
    val emptyProof = Sigma.challenge(Sigma.fiatShamir(empty, Vector.empty), msg)
    val zeroOfOne = mirrorThreshold.fromProduct((0, Vector(dlog(sk1))))
    val twoOfOne = mirrorThreshold.fromProduct((2, Vector(dlog(sk1))))
    expect(Sigma.verify(empty, emptyProof, msg).isLeft, "an empty and verifies for anyone")
      .and(expect(SigmaProver.proveWith(empty, Vector.empty, msg, hex"01").isLeft, "nor is one proven"))
      .and(expect(Sigma.verify(zeroOfOne, ByteVector.fill(Sigma.proofLength(zeroOfOne))(1), msg).isLeft, "k = 0"))
      .and(expect(SigmaProver.proveWith(zeroOfOne, Vector.empty, msg, hex"01").isLeft, "k = 0 proves with no secret"))
      .and(expect(Sigma.verify(twoOfOne, ByteVector.fill(Sigma.proofLength(twoOfOne).max(0L))(1), msg).isLeft, "k > n"))
  }

  pureTest("a mirror-built proposition too deep, too wide or with too many leaves is rejected, without overflowing") {
    val leaf = dlog(sk1)
    def nest(n: Int): SigmaProp = (1 to n).foldLeft(leaf)((c, _) => mirrorAnd.fromProduct(Tuple1(Vector(c))))
    val deep = nest(SigmaProp.MaxDepth)
    val proof = SigmaProver.proveWith(nest(SigmaProp.MaxDepth - 1), Vector(BigInt(sk1)), msg, hex"01")
    val wide = mirrorAnd.fromProduct(Tuple1(Vector.fill(SigmaProp.MaxChildren + 1)(leaf)))
    val many = mirrorAnd.fromProduct(Tuple1(Vector.fill(17)(mirrorAnd.fromProduct(Tuple1(Vector.fill(250)(leaf))))))
    expect(proof.flatMap(Sigma.verify(nest(SigmaProp.MaxDepth - 1), _, msg)) == Right(true), "64 deep is fine")
      .and(expect(proof.flatMap(Sigma.verify(deep, _, msg)).isLeft, "65 deep is not"))
      .and(expect(SigmaProver.proveWith(deep, Vector(BigInt(sk1)), msg, hex"01").isLeft, "nor is it proven"))
      .and(expect(Sigma.verify(nest(100000), ByteVector.empty, msg).isLeft, "a very deep tree is malformed, not a stack overflow"))
      .and(expect(Sigma.verify(wide, ByteVector.empty, msg).isLeft, "256 children"))
      .and(expect(Sigma.verify(many, ByteVector.empty, msg).isLeft, "4250 leaves"))
  }

  pureTest("a mirror-built point off secp256k1 is rejected by the smart constructors and at the boundary") {
    val other = CustomNamedCurves.getByName("secp256r1").getG.normalize()
    val alien = summon[Mirror.ProductOf[GroupElement]].fromProduct(Tuple1(other))
    val forged = mirrorDlog.fromProduct(Tuple1(alien))
    expect(SigmaProp.dlog(alien).isLeft, "dlog")
      .and(expect(SigmaProp.dhTuple(g, g, g, alien).isLeft, "dhTuple"))
      .and(expect(Sigma.verify(forged, ByteVector.fill(56L)(1), msg).isLeft, "verify"))
      .and(expect(SigmaProver.proveWith(forged, Vector(BigInt(1)), msg, hex"01").isLeft, "prove"))
  }
}
