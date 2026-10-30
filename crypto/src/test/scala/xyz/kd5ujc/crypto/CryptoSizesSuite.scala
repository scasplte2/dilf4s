package xyz.kd5ujc.crypto

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.core.canonical.{CanonicalValue, Codec}
import xyz.kd5ujc.crypto.bls.Bls12381
import xyz.kd5ujc.crypto.kes.CardanoSumKes
import xyz.kd5ujc.crypto.keys.{Ed25519Bip32, Secret}
import xyz.kd5ujc.crypto.signing._
import xyz.kd5ujc.crypto.vrf.{EcVrfDraft03, EcVrfTai, Vrf}
import xyz.kd5ujc.models.{crypto => C}

import io.circe.parser
import scodec.bits.{ByteVector, hex}
import weaver.{Expectations, SimpleIOSuite}

/**
 * Real outputs of every implementation have exactly the sizes of shared/schemas/crypto.cddl: each decodes through the
 * generated sized codec, and one byte less does not.
 */
object CryptoSizesSuite extends SimpleIOSuite {

  private def fits(name: String, codec: Codec[ByteVector], bytes: ByteVector): Expectations =
    expect(codec.decoder.decode(CanonicalValue.bytes(bytes)).isRight, s"$name: ${bytes.size} bytes fit the schema")
      .and(expect(codec.decoder.decode(CanonicalValue.bytes(bytes.drop(1))).isLeft, s"$name: one byte less does not"))

  private val seed = ByteVector.fill(32)(7)
  private val msg = hex"0102"

  test("signing keys, verifying keys and signatures of every scheme") {
    (Secp256k1.load[IO], Bls12381.load[IO]).tupled.map {
      case (secp, bls) =>
        val ed = Ed25519.seedDerivation.fromSeed(seed).toOption.get
        val p256 = P256Ecdsa.seedDerivation.fromSeed(seed).toOption.get
        val k1 = secp.ecdsaSeedDerivation.fromSeed(seed).toOption.get
        val bip340 = secp.bip340SeedDerivation.fromSeed(seed).toOption.get
        val r255 = Ristretto255Schnorr.seedDerivation.fromSeed(seed).toOption.get
        val blsKp = bls.seedDerivation.fromSeed(seed).toOption.get
        val icarus = Ed25519Bip32.icarus(Secret(seed.take(16))).toOption.get
        fits("ed25519 sk", C.Ed25519SigningKey.codec, ed.signing.bytes)
          .and(fits("ed25519 vk", C.Ed25519VerifyingKey.codec, ed.verifying.bytes))
          .and(fits("ed25519 sig", C.Ed25519Signature.codec, Ed25519.signer.sign(ed.signing, msg).bytes))
          .and(fits("bip32-ed25519 sk", C.Ed25519Bip32SigningKey.codec, icarus.key.bytes))
          .and(fits("chain code", C.ChainCode.codec, icarus.chainCode))
          .and(fits("p256 sk", C.EcdsaSigningKey.codec, p256.signing.bytes))
          .and(fits("p256 vk", C.EcdsaVerifyingKey.codec, p256.verifying.bytes))
          .and(fits("p256 sig", C.EcdsaSignature.codec, P256Ecdsa.signer.sign(p256.signing, msg).bytes))
          .and(fits("secp256k1 vk", C.EcdsaVerifyingKey.codec, k1.verifying.bytes))
          .and(fits("secp256k1 sig", C.EcdsaSignature.codec, secp.ecdsaSigner.sign(k1.signing, msg).bytes))
          .and(fits("bip340 vk", C.SchnorrVerifyingKey.codec, bip340.verifying.bytes))
          .and(
            fits(
              "bip340 sig",
              C.SchnorrSignature.codec,
              Message32.from(seed).flatMap(secp.bip340SignWithAux(bip340.signing, _, seed)).toOption.get.bytes
            )
          )
          .and(fits("ristretto255 vk", C.Ristretto255VerifyingKey.codec, r255.verifying.bytes))
          .and(
            fits(
              "ristretto255 sig",
              C.Ristretto255Signature.codec,
              Ristretto255Schnorr.signWithRandomness(r255.signing, msg, seed).toOption.get.bytes
            )
          )
          .and(fits("bls sk", C.BlsSigningKey.codec, blsKp.signing.bytes))
          .and(fits("bls vk", C.BlsVerifyingKey.codec, blsKp.verifying.bytes))
          .and(fits("bls sig", C.BlsSignature.codec, bls.sign(blsKp.signing, msg).toOption.get.bytes))
    }
  }

  pureTest("VRF, Cardano KES and zk formats") {
    val tai = Vrf.fromSeed[EcVrfTai](seed).toOption.get
    val d03 = Vrf.fromSeed[EcVrfDraft03](seed).toOption.get
    val (pi, beta) = Vrf.tai.prove(tai.signing, msg).toOption.get
    val kes = CardanoSumKes.Sum6.generate(seed).toOption.get
    val sp1 = parser
      .parse(new String(Files.readAllBytes(Paths.get("docs", "test-vectors", "zk", "sp1-groth16-premium.json")), UTF_8))
      .flatMap(_.hcursor.get[String]("proofBytes"))
      .map(h => ByteVector.fromValidHex(h.stripPrefix("0x")))
      .toOption
      .get
    fits("vrf sk", C.VrfSigningKey.codec, tai.signing.bytes)
      .and(fits("vrf draft-03 sk", C.VrfDraft03SigningKey.codec, d03.signing.bytes))
      .and(fits("vrf vk", C.VrfVerifyingKey.codec, tai.verifying.bytes))
      .and(fits("vrf proof", C.VrfProof.codec, pi.bytes))
      .and(fits("vrf output", C.VrfOutput.codec, beta.bytes))
      .and(fits("sum6 sk", C.Sum6KesSigningKey.codec, kes.bytes))
      .and(fits("sum6 vk", C.Sum6KesVerifyingKey.codec, CardanoSumKes.Sum6.verifyingKey(kes)))
      .and(fits("sum6 sig", C.Sum6KesSignature.codec, CardanoSumKes.Sum6.sign(kes, msg)))
      .and(fits("sp1 proof", C.Sp1Proof.codec, sp1))
      .and(fits("groth16 proof", C.Groth16Proof.codec, sp1.drop(4L + 32L * 3)))
      .and(fits("bip39 entropy (16)", C.Bip39Entropy.codec, seed.take(16)))
  }
}
