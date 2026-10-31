package xyz.kd5ujc.crypto

import java.nio.charset.StandardCharsets.UTF_8

import cats.syntax.all._

import xyz.kd5ujc.crypto.bls.{Bls12381, BlsPop}
import xyz.kd5ujc.crypto.keys.{Ed25519Bip32, Secret}
import xyz.kd5ujc.crypto.signing._
import xyz.kd5ujc.crypto.vrf._

import cafe.cryptography.curve25519.CompressedEdwardsY
import io.circe.Json
import io.circe.syntax._
import scodec.bits.ByteVector

/**
 * Verification verdicts for docs/specs/signing.md and vrf.md, for every SDK to reproduce: well-formed signatures that
 * verify or not, and malformed keys and signatures, which are typed errors rather than `false`. The key is decoded
 * before the signature, so no case has both malformed.
 */
object SigningRejections {

  private def get[A](e: Either[?, A]): A = e.fold(err => sys.error(err.toString), identity)

  /** The verdict of a scheme's decoders and verifier, in that order. */
  private def verdict[S](vk: ByteVector, m: ByteVector, sig: ByteVector)(implicit C: SchemeCodec[S], V: Verifier[S]): String =
    C.verifyingKey(vk)
      .fold(
        _ => "malformedKey",
        k => C.signature(sig).fold(_ => "malformedSignature", s => if (V.verify(k, m, s)) "valid" else "invalid")
      )

  private def row[S](scheme: String, name: String, vk: ByteVector, m: ByteVector, sig: ByteVector, expected: String)(
    implicit C: SchemeCodec[S],
    V:          Verifier[S]
  ): Json = {
    val result = verdict[S](vk, m, sig)
    require(expected == result, s"$scheme $name: expected $expected, got $result")
    Json.obj(
      "scheme"       -> scheme.asJson,
      "name"         -> name.asJson,
      "verifyingKey" -> vk.toHex.asJson,
      "message"      -> m.toHex.asJson,
      "signature"    -> sig.toHex.asJson,
      "result"       -> result.asJson
    )
  }

  private def be32(i:  BigInt): ByteVector = ByteVector.view(i.toByteArray).takeRight(32).padLeft(32)
  private def le32(i:  BigInt): ByteVector = be32(i).reverse
  private def leInt(b: ByteVector): BigInt = BigInt(1, b.reverse.toArray)
  private def beInt(b: ByteVector): BigInt = BigInt(1, b.toArray)

  val L: BigInt = BigInt(2).pow(252) + BigInt("27742317777372353535851937790883648493")
  val p25519: BigInt = BigInt(2).pow(255) - 19
  val nP256: BigInt = BigInt("ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551", 16)
  val nSecp256k1: BigInt = BigInt("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)

  /**
   * The ZIP-215 test set: the 8 small-order points of edwards25519 in canonical form, then 6 non-canonical encodings
   * of them (`y >= p`, or the sign bit on `x = 0`). Every `(A, R)` pair with `S = 0` verifies under ZIP-215, since
   * `[8]([0]B - R - [k]A) = 0`, whatever `k` is; libsodium rejects them all.
   */
  val smallOrder: List[ByteVector] = List(
    "0100000000000000000000000000000000000000000000000000000000000000",
    "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
    "0000000000000000000000000000000000000000000000000000000000000000",
    "0000000000000000000000000000000000000000000000000000000000000080",
    "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc05",
    "26e8958fc2b227b045c3f489f2ef98f0d5dfac05d3c63339b13802886d53fc85",
    "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac037a",
    "c7176a703d4dd84fba3c0b760d10670f2a2053fa2c39ccc64ec7fd7792ac03fa",
    "0100000000000000000000000000000000000000000000000000000000000080",
    "ecffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
    "edffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
    "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff7f",
    "eeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
  ).map(ByteVector.fromValidHex(_))

  private def edwards(b: ByteVector) = Either.catchNonFatal(new CompressedEdwardsY(b.toArray).decompress())

  /** The smallest `y` whose 32-byte encoding decodes to no point. */
  private val notAPoint: ByteVector =
    LazyList.from(2).map(y => le32(BigInt(y))).find(edwards(_).isLeft).getOrElse(sys.error("unreachable"))

  /** The smallest `x >= 1` whose `size`-byte encoding `prefix || x` the scheme's codec rejects: no point has it. */
  private def offCurve(codec: ByteVector => Either[?, ?], prefix: ByteVector, size: Long): ByteVector =
    LazyList
      .from(1)
      .map(x => prefix ++ ByteVector.view(BigInt(x).toByteArray).dropWhile(_ == 0).padLeft(size - prefix.size))
      .find(codec(_).isLeft)
      .getOrElse(sys.error("unreachable"))

  private val zcash: ByteVector = ByteVector("Zcash".getBytes(UTF_8))
  private val abc: ByteVector = ByteVector("abc".getBytes(UTF_8))
  private val abd: ByteVector = ByteVector("abd".getBytes(UTF_8))

  private def ed25519(seed: ByteVector): List[Json] = {
    implicit val codec: SchemeCodec[Ed25519] = Ed25519.codec
    val libsodium: Verifier[Ed25519] = Ed25519.libsodiumVerifier
    val kp = get(Ed25519.seedDerivation.fromSeed(seed))
    val vk = kp.verifying.bytes
    val sig = Ed25519.signer.sign(kp.signing, abc).bytes
    val highS = sig.take(32) ++ le32(leInt(sig.drop(32)) + L)
    val badR = notAPoint ++ sig.drop(32)
    smallOrder.foreach(e => require(edwards(e).exists(_.multiplyByCofactor().isIdentity), s"${e.toHex} is not small order"))
    val zero = ByteVector.low(32)
    val zip215 = for {
      (a, i) <- smallOrder.zipWithIndex
      (r, j) <- smallOrder.zipWithIndex
    } yield (s"small-order-a$i-r$j", a, r ++ zero)
    def both(name: String, k: ByteVector, m: ByteVector, s: ByteVector, zip: String, sodium: String): List[Json] =
      List(
        row[Ed25519]("Ed25519", name, k, m, s, zip)(using codec, Ed25519.verifier),
        row[Ed25519]("Ed25519-libsodium", name, k, m, s, sodium)(using codec, libsodium)
      )
    both("valid", vk, abc, sig, "valid", "valid") ++
    both("wrong-message", vk, abd, sig, "invalid", "invalid") ++
    both("s-plus-l", vk, abc, highS, "invalid", "invalid") ++
    both("r-not-a-point", vk, abc, badR, "invalid", "invalid") ++
    both("key-not-a-point", notAPoint, abc, sig, "malformedKey", "malformedKey") ++
    both("short-key", vk.take(31), abc, sig, "malformedKey", "malformedKey") ++
    both("short-signature", vk, abc, sig.take(63), "malformedSignature", "malformedSignature") ++
    zip215.flatMap { case (name, a, s) => both(name, a, zcash, s, "valid", "invalid") }
  }

  private def ecdsa[S](scheme: String, n: BigInt, kp: KeyPair[S], sig: ByteVector)(
    implicit C: SchemeCodec[S],
    V:          Verifier[S]
  ): List[Json] = {
    val (r, s) = (beInt(sig.take(32)), beInt(sig.drop(32)))
    val vk = kp.verifying.bytes
    List(
      row[S](scheme, "valid", vk, abc, sig, "valid"),
      row[S](scheme, "wrong-message", vk, abd, sig, "invalid"),
      row[S](scheme, "high-s", vk, abc, be32(r) ++ be32(n - s), "malformedSignature"),
      row[S](scheme, "r-zero", vk, abc, be32(0) ++ be32(s), "malformedSignature"),
      row[S](scheme, "s-zero", vk, abc, be32(r) ++ be32(0), "malformedSignature"),
      row[S](scheme, "r-is-n", vk, abc, be32(n) ++ be32(s), "malformedSignature"),
      row[S](scheme, "key-off-curve", offCurve(C.verifyingKey, ByteVector(0x02), 33), abc, sig, "malformedKey"),
      row[S](scheme, "key-bad-prefix", ByteVector(0x05) ++ vk.drop(1), abc, sig, "malformedKey"),
      row[S](scheme, "uncompressed-length-key", vk ++ vk.drop(1), abc, sig, "malformedKey"),
      row[S](scheme, "short-signature", vk, abc, sig.take(63), "malformedSignature")
    )
  }

  def signing(secp: Secp256k1, bls: Bls12381): List[Json] = {
    val instances = new Secp256k1.Instances(secp)
    import instances._
    val seed = CryptoVectors.seeds.head

    val p256 = get(P256Ecdsa.seedDerivation.fromSeed(seed))
    val k1 = get(ecdsaSeedDerivation.fromSeed(seed))

    val bip340Verifier: Verifier[Bip340] = (k: VerifyingKey[Bip340], m: ByteVector, s: Signature[Bip340]) =>
      Message32.from(m).exists(secp.bip340Verify(k, _, s))
    val bip = get(bip340SeedDerivation.fromSeed(seed))
    val tagged = get(taggedSchnorrSeedDerivation.fromSeed(seed))
    val m32 = xyz.kd5ujc.core.hash.Hasher[xyz.kd5ujc.core.hash.Sha256].digest(abc).bytes
    val bipSig = get(Message32.from(m32).flatMap(secp.bip340SignWithAux(bip.signing, _, CryptoVectors.randomness))).bytes
    val taggedSig = get(secp.taggedSchnorrSignWithAux(tagged.signing, abc, CryptoVectors.randomness)).bytes
    val bipCases = {
      implicit val v: Verifier[Bip340] = bip340Verifier
      val vk = bip.verifying.bytes
      List(
        row[Bip340]("BIP-340", "valid", vk, m32, bipSig, "valid"),
        row[Bip340]("BIP-340", "wrong-message", vk, m32.update(0, (m32(0) ^ 1).toByte), bipSig, "invalid"),
        row[Bip340]("BIP-340", "tagged-signature", vk, m32, taggedSig, "invalid"),
        row[Bip340]("BIP-340", "s-not-below-n", vk, m32, bipSig.take(32) ++ be32(beInt(bipSig.drop(32)) + nSecp256k1), "invalid"),
        row[Bip340]("BIP-340", "key-off-curve", offCurve(bip340Codec.verifyingKey, ByteVector.empty, 32), m32, bipSig, "malformedKey"),
        row[Bip340]("BIP-340", "short-signature", vk, m32, bipSig.take(63), "malformedSignature")
      )
    }
    val taggedCases = {
      val vk = tagged.verifying.bytes
      List(
        row[Secp256k1TaggedSchnorr]("secp256k1-tagged-Schnorr", "valid", vk, abc, taggedSig, "valid"),
        row[Secp256k1TaggedSchnorr]("secp256k1-tagged-Schnorr", "wrong-message", vk, abd, taggedSig, "invalid"),
        // the BIP-340 signature of SHA-256("abc"): the same key, but neither scheme accepts the other's signatures
        row[Secp256k1TaggedSchnorr]("secp256k1-tagged-Schnorr", "bip340-signature", vk, abc, bipSig, "invalid"),
        row[Secp256k1TaggedSchnorr]("secp256k1-tagged-Schnorr", "bip340-signature-of-digest", vk, m32, bipSig, "invalid")
      )
    }

    val ristretto = {
      val kp = get(Ristretto255Schnorr.seedDerivation.fromSeed(seed))
      val vk = kp.verifying.bytes
      val sig = get(Ristretto255Schnorr.signWithRandomness(kp.signing, abc, CryptoVectors.randomness)).bytes
      val scheme = "ristretto255-Schnorr"
      List(
        row[Ristretto255Schnorr](scheme, "valid", vk, abc, sig, "valid"),
        row[Ristretto255Schnorr](scheme, "wrong-message", vk, abd, sig, "invalid"),
        row[Ristretto255Schnorr](scheme, "identity-key", ByteVector.low(32), abc, sig, "malformedKey"),
        row[Ristretto255Schnorr](scheme, "non-canonical-key", ByteVector(1) ++ ByteVector.low(31), abc, sig, "malformedKey"),
        row[Ristretto255Schnorr](scheme, "identity-r", vk, abc, ByteVector.low(32) ++ sig.drop(32), "malformedSignature"),
        row[Ristretto255Schnorr](scheme, "z-plus-l", vk, abc, sig.take(32) ++ le32(leInt(sig.drop(32)) + L), "malformedSignature"),
        row[Ristretto255Schnorr](scheme, "short-signature", vk, abc, sig.take(63), "malformedSignature")
      )
    }

    val blsCases = {
      import bls._
      val kp = get(bls.seedDerivation.fromSeed(seed))
      val other = get(bls.seedDerivation.fromSeed(CryptoVectors.seeds(1)))
      val vk = kp.verifying.bytes
      val sig = get(bls.sign(kp.signing, abc)).bytes
      val scheme = "BLS12-381-PoP"
      val identityKey = ByteVector(0xc0) ++ ByteVector.low(47)
      val identitySig = ByteVector(0xc0) ++ ByteVector.low(95)
      List(
        row[BlsPop](scheme, "valid", vk, abc, sig, "valid"),
        row[BlsPop](scheme, "wrong-message", vk, abd, sig, "invalid"),
        row[BlsPop](scheme, "other-key", other.verifying.bytes, abc, sig, "invalid"),
        row[BlsPop](scheme, "pop-as-signature", vk, vk, get(bls.popProve(kp.signing)).bytes, "invalid"),
        row[BlsPop](scheme, "identity-signature", vk, abc, identitySig, "invalid"),
        row[BlsPop](scheme, "identity-key", identityKey, abc, sig, "malformedKey"),
        row[BlsPop](scheme, "uncompressed-flag-key", vk.update(0, (vk(0) & 0x7f).toByte), abc, sig, "malformedKey"),
        row[BlsPop](scheme, "uncompressed-flag-signature", vk, abc, sig.update(0, (sig(0) & 0x7f).toByte), "malformedSignature"),
        row[BlsPop](scheme, "key-off-curve", offCurve(bls.codec.verifyingKey, ByteVector(0x80), 48), abc, sig, "malformedKey"),
        row[BlsPop](scheme, "short-signature", vk, abc, sig.take(95), "malformedSignature")
      )
    }

    ed25519(seed) ++
    ecdsa[P256Ecdsa]("P-256-ECDSA", nP256, p256, P256Ecdsa.signer.sign(p256.signing, abc).bytes) ++
    ecdsa[Secp256k1Ecdsa]("secp256k1-ECDSA", nSecp256k1, k1, ecdsaSigner.sign(k1.signing, abc).bytes) ++
    bipCases ++ taggedCases ++ ristretto ++ blsCases
  }

  // ---- key decoding: which typed error each decoder gives

  /** The decoder's verdict: `"accepted"`, or the [[CryptoError]] kind. */
  private def decoded(r: Either[CryptoError, ?]): String = r.fold(_.productPrefix, _ => "accepted")

  private def keyRow(scheme: String, name: String, part: String, bytes: ByteVector, expected: String)(
    decode: ByteVector => Either[CryptoError, ?]
  ): Json = {
    val result = decoded(decode(bytes))
    require(expected == result, s"$scheme $part $name: expected $expected, got $result")
    Json.obj(
      "scheme" -> scheme.asJson,
      "name"   -> name.asJson,
      "part"   -> part.asJson,
      "bytes"  -> bytes.toHex.asJson,
      "result" -> result.asJson
    )
  }

  /** Signing-key rows: a wrong length is `InvalidEncoding`; well-sized bytes that are no key are `InvalidKey`. */
  private def signingKeys[S](scheme: String, valid: ByteVector, invalid: List[(String, ByteVector)])(
    implicit C: SchemeCodec[S]
  ): List[Json] = {
    val row = (name: String, b: ByteVector, expected: String) => keyRow(scheme, name, "signingKey", b, expected)(C.signingKey)
    List(
      row("valid", valid, "accepted"),
      row("short", valid.dropRight(1), "InvalidEncoding"),
      row("long", valid :+ 0.toByte, "InvalidEncoding"),
      row("empty", ByteVector.empty, "InvalidEncoding")
    ) ++ invalid.map { case (name, b) => row(name, b, "InvalidKey") }
  }

  /** Verifying-key rows, as [[signingKeys]]. */
  private def verifyingKeys[S](scheme: String, valid: ByteVector, invalid: List[(String, ByteVector)])(
    implicit C: SchemeCodec[S]
  ): List[Json] = {
    val row = (name: String, b: ByteVector, expected: String) => keyRow(scheme, name, "verifyingKey", b, expected)(C.verifyingKey)
    List(
      row("valid", valid, "accepted"),
      row("short", valid.dropRight(1), "InvalidEncoding"),
      row("long", valid :+ 0.toByte, "InvalidEncoding")
    ) ++ invalid.map { case (name, b) => row(name, b, "InvalidKey") }
  }

  /** 33-byte keys over a valid key's x with a tag other than 02 or 03: SEC1 parsers take some of these tags. */
  private def badTags(vk: ByteVector): List[(String, ByteVector)] =
    List(0x00, 0x01, 0x04, 0x05, 0x06, 0x07, 0x82, 0xff).map(t => f"tag-$t%02x" -> (ByteVector(t.toByte) ++ vk.drop(1)))

  private val pSecp256k1: BigInt = BigInt("fffffffffffffffffffffffffffffffffffffffffffffffffffffffefffffc2f", 16)
  private val pP256: BigInt = BigInt("ffffffff00000001000000000000000000000000ffffffffffffffffffffffff", 16)
  private val rBls: BigInt = BigInt("73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001", 16)

  /** The 65-byte SEC1 uncompressed form (`04 || x || y`) of a compressed key. */
  private def uncompressed(curveName: String, compressed: ByteVector): ByteVector = {
    val curve = org.bouncycastle.crypto.ec.CustomNamedCurves.getByName(curveName)
    ByteVector.view(curve.getCurve.decodePoint(compressed.toArray).getEncoded(false))
  }

  private def ecdsaKeys[S](scheme: String, curveName: String, n: BigInt, p: BigInt, kp: KeyPair[S])(
    implicit C: SchemeCodec[S]
  ): List[Json] =
    signingKeys[S](scheme, kp.signing.bytes, List("zero" -> ByteVector.low(32), "n" -> be32(n), "above-n" -> be32(n + 1))) ++
    verifyingKeys[S](
      scheme,
      kp.verifying.bytes,
      List("off-curve" -> offCurve(C.verifyingKey, ByteVector(0x02), 33), "x-is-p" -> (ByteVector(0x02) ++ be32(p))) ++
      badTags(kp.verifying.bytes)
    ) :+
    keyRow(scheme, "uncompressed", "verifyingKey", uncompressed(curveName, kp.verifying.bytes), "InvalidEncoding")(C.verifyingKey)

  private def schnorrKeys[S](scheme: String, kp: KeyPair[?])(implicit C: SchemeCodec[S]): List[Json] =
    signingKeys[S](scheme, kp.signing.bytes, List("zero" -> ByteVector.low(32), "n" -> be32(nSecp256k1))) ++
    verifyingKeys[S](
      scheme,
      kp.verifying.bytes,
      List("off-curve" -> offCurve(C.verifyingKey, ByteVector.empty, 32), "x-is-p" -> be32(pSecp256k1))
    )

  /** How every scheme's decoders classify signing and verifying keys (signing.md §4). */
  def keyDecoding(secp: Secp256k1, bls: Bls12381): List[Json] = {
    val instances = new Secp256k1.Instances(secp)
    import instances._
    val seed = CryptoVectors.seeds.head
    val ed = get(Ed25519.seedDerivation.fromSeed(seed))
    val icarus = get(Ed25519Bip32.icarus(Secret(seed.take(16)))).key.bytes
    val rist = get(Ristretto255Schnorr.seedDerivation.fromSeed(seed))
    val blsKp = get(bls.seedDerivation.fromSeed(seed))
    val bip = get(bip340SeedDerivation.fromSeed(seed))
    signingKeys[Ed25519]("Ed25519", ed.signing.bytes, Nil)(using Ed25519.codec) ++
    verifyingKeys[Ed25519]("Ed25519", ed.verifying.bytes, List("not-a-point" -> notAPoint))(using Ed25519.codec) ++
    signingKeys[Ed25519Bip32](
      "Ed25519-BIP32",
      icarus,
      List(
        "kl-not-multiple-of-8" -> icarus.update(0, (icarus(0) | 0x01).toByte),
        "kl-top-bit"           -> icarus.update(31, (icarus(31) | 0x80).toByte)
      )
    )(using Ed25519Bip32.codec) ++
    ecdsaKeys[P256Ecdsa]("P-256-ECDSA", "secp256r1", nP256, pP256, get(P256Ecdsa.seedDerivation.fromSeed(seed)))(using P256Ecdsa.codec) ++
    ecdsaKeys[Secp256k1Ecdsa]("secp256k1-ECDSA", "secp256k1", nSecp256k1, pSecp256k1, get(ecdsaSeedDerivation.fromSeed(seed))) ++
    schnorrKeys[Bip340]("BIP-340", bip) ++
    schnorrKeys[Secp256k1TaggedSchnorr]("secp256k1-tagged-Schnorr", bip) ++
    signingKeys[Ristretto255Schnorr]("ristretto255-Schnorr", rist.signing.bytes, List("zero" -> ByteVector.low(32), "l" -> le32(L)))(
      using Ristretto255Schnorr.codec
    ) ++
    verifyingKeys[Ristretto255Schnorr](
      "ristretto255-Schnorr",
      rist.verifying.bytes,
      List("identity" -> ByteVector.low(32), "non-canonical" -> (ByteVector(1) ++ ByteVector.low(31)))
    )(using Ristretto255Schnorr.codec) ++
    signingKeys[BlsPop]("BLS12-381-PoP", blsKp.signing.bytes, List("zero" -> ByteVector.low(32), "r" -> be32(rBls)))(using bls.codec) ++
    verifyingKeys[BlsPop](
      "BLS12-381-PoP",
      blsKp.verifying.bytes,
      List(
        "identity"          -> (ByteVector(0xc0) ++ ByteVector.low(47)),
        "uncompressed-flag" -> blsKp.verifying.bytes.update(0, (blsKp.verifying.bytes(0) & 0x7f).toByte),
        "off-curve"         -> offCurve(bls.codec.verifyingKey, ByteVector(0x80), 48)
      )
    )(using bls.codec)
  }

  /** Proofs of possession for the shared seeds. */
  def blsPop(bls: Bls12381): List[Json] =
    CryptoVectors.seeds.map { seed =>
      val kp = get(bls.seedDerivation.fromSeed(seed))
      val proof = get(bls.popProve(kp.signing))
      require(bls.popVerify(kp.verifying, proof), "PopVerify accepts PopProve")
      Json.obj(
        "signingKey"   -> kp.signing.bytes.toHex.asJson,
        "verifyingKey" -> kp.verifying.bytes.toHex.asJson,
        "proof"        -> proof.bytes.toHex.asJson
      )
    }

  // ---- VRF

  private def vrfRow[S](name: String, vk: ByteVector, alpha: ByteVector, pi: ByteVector, expected: String)(implicit V: Vrf[S]): Json = {
    val result = V.verifyBytes(vk, alpha, pi).fold(_.productPrefix, _ => "valid")
    require(result == expected, s"${V.name} $name: expected $expected, got $result")
    Json.obj(
      "suite"        -> V.name.asJson,
      "name"         -> name.asJson,
      "verifyingKey" -> vk.toHex.asJson,
      "alpha"        -> alpha.toHex.asJson,
      "pi"           -> pi.toHex.asJson,
      "result"       -> result.asJson
    )
  }

  /** A non-canonical encoding (`y + p`) of a point of large order. */
  private val nonCanonicalPoint: ByteVector =
    (2 until 19)
      .map(y => (le32(BigInt(y)), le32(BigInt(y) + p25519)))
      .find { case (canonical, _) => edwards(canonical).exists(pt => !pt.multiplyByCofactor().isIdentity) }
      .map(_._2)
      .getOrElse(sys.error("unreachable"))

  private def vrfSuite[S](implicit V: Vrf[S]): List[Json] = {
    val kp = get(Vrf.fromSeed[S](CryptoVectors.seeds.head))
    val vk = kp.verifying.bytes
    val pi = get(V.prove(kp.signing, abc))._1.bytes
    val (gamma, c, s) = (pi.take(32), pi.slice(32, 48), pi.drop(48))
    List(
      vrfRow[S]("valid", vk, abc, pi, "valid"),
      vrfRow[S]("wrong-alpha", vk, abd, pi, "ChallengeMismatch"),
      vrfRow[S]("flipped-challenge", vk, abc, gamma ++ c.update(0, (c(0) ^ 1).toByte) ++ s, "ChallengeMismatch"),
      vrfRow[S]("s-plus-l", vk, abc, gamma ++ c ++ le32(leInt(s) + L), "NonCanonicalScalar"),
      vrfRow[S]("gamma-not-a-point", vk, abc, notAPoint ++ c ++ s, "InvalidGamma"),
      vrfRow[S]("key-not-a-point", notAPoint, abc, pi, "InvalidPublicKey"),
      vrfRow[S]("key-non-canonical", nonCanonicalPoint, abc, pi, "InvalidPublicKey"),
      vrfRow[S]("key-identity", smallOrder.head, abc, pi, "SmallOrderPublicKey"),
      vrfRow[S]("key-order-8", smallOrder(4), abc, pi, "SmallOrderPublicKey"),
      vrfRow[S]("short-key", vk.take(31), abc, pi, "Malformed"),
      vrfRow[S]("short-proof", vk, abc, pi.take(79), "Malformed")
    )
  }

  def vrf: List[Json] = vrfSuite[EcVrfTai] ++ vrfSuite[EcVrfEll2] ++ vrfSuite[EcVrfDraft03]
}
