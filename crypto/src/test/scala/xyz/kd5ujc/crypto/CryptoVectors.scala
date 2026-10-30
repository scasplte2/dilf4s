package xyz.kd5ujc.crypto

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all._

import xyz.kd5ujc.crypto.keys._
import xyz.kd5ujc.crypto.signing._

import io.circe.Json
import io.circe.syntax._
import scodec.bits.{ByteVector, HexStringSyntax}
import weaver.SimpleIOSuite

/** Conformance vectors for docs/specs/signing.md: every scheme and derivation, from fixed inputs. */
object CryptoVectors {

  val path: Path = Paths.get("shared", "vectors", "signing_v1.json")

  val seeds: List[ByteVector] = List(
    ByteVector((0 until 32).map(_.toByte)),
    ByteVector.fill(32)(0x7f),
    hex"c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7"
  )

  val messages: List[ByteVector] = List(hex"", hex"616263", ByteVector((0 until 255).map(_.toByte)))

  /** Fixed nonce / auxiliary randomness for the randomised schemes. */
  val randomness: ByteVector = ByteVector.fill(32)(0x42)

  private def get[A](e: Either[_, A]): A = e.fold(err => sys.error(err.toString), identity)

  /** Standard BIP-340 signs exact 32-byte messages: the SHA-256 digests of [[messages]]. */
  val messages32: List[ByteVector] = messages.map(m => xyz.kd5ujc.core.hash.Hasher[xyz.kd5ujc.core.hash.Sha256].digest(m).bytes)

  private def scheme[S](name: String, sign: (SigningKey[S], ByteVector) => Signature[S], messages: List[ByteVector] = messages)(
    implicit D: SeedDerivation[S]
  ): Json =
    Json.obj(
      "scheme" -> name.asJson,
      "keys" -> seeds.map { seed =>
        val kp = get(D.fromSeed(seed))
        Json.obj(
          "seed"         -> seed.toHex.asJson,
          "signingKey"   -> kp.signing.bytes.toHex.asJson,
          "verifyingKey" -> kp.verifying.bytes.toHex.asJson,
          "signatures" -> messages
            .map(m => Json.obj("message" -> m.toHex.asJson, "signature" -> sign(kp.signing, m).bytes.toHex.asJson))
            .asJson
        )
      }.asJson
    )

  private def extended[S](k: ExtendedKey[S]): Json = Json.obj("key" -> k.key.bytes.toHex.asJson, "chainCode" -> k.chainCode.toHex.asJson)

  def build(secp: Secp256k1, english: WordList): Json = {
    val instances = new Secp256k1.Instances(secp)
    import instances._

    val bip39 = seeds.flatMap(s => List(s.take(16), s)).map { entropy =>
      val mnemonic = get(Bip39.toMnemonic(Secret(entropy), english))
      Json.obj(
        "entropy"    -> entropy.toHex.asJson,
        "mnemonic"   -> mnemonic.phrase.asJson,
        "passphrase" -> "dilf4s".asJson,
        "seed"       -> Bip39.seed(mnemonic, "dilf4s").bytes.toHex.asJson
      )
    }

    val hdSeed = Bip39.seed(get(Bip39.toMnemonic(Secret(seeds.head), english)), "")
    val slip10Path = "m/44'/501'/0'/0'"
    val bip32Path = "m/44'/0'/0'/0/7"
    val bip32 = new Bip32Secp256k1(secp)
    val bip32Key = get(ChildIndex.path(bip32Path).flatMap(bip32.derive(Secret(hdSeed.bytes), _)))

    val icarusRoot = get(Ed25519Bip32.icarus(Secret(seeds.head.take(16))))
    val accountPath = "m/1852'/1815'/0'"
    val account = get(ChildIndex.path(accountPath).flatMap(Ed25519Bip32.derive(icarusRoot, _)))
    val address = get(ChildIndex.path("m/0/0").flatMap(Ed25519Bip32.derive(account, _)))
    val icarusSign = (k: SigningKey[Ed25519Bip32], m: ByteVector) => Ed25519Bip32.signer.sign(k, m)

    Json.obj(
      "spec"       -> "docs/specs/signing.md".asJson,
      "version"    -> 1.asJson,
      "randomness" -> randomness.toHex.asJson,
      "signing" -> List(
        scheme[Ed25519]("Ed25519", Ed25519.signer.sign),
        scheme[P256Ecdsa]("P-256-ECDSA", P256Ecdsa.signer.sign),
        scheme[Secp256k1Ecdsa]("secp256k1-ECDSA", secp.ecdsaSigner.sign),
        scheme[Bip340]("BIP-340", (k, m) => get(Message32.from(m).flatMap(secp.bip340SignWithAux(k, _, randomness))), messages32),
        scheme[Secp256k1TaggedSchnorr]("secp256k1-tagged-Schnorr", (k, m) => get(secp.taggedSchnorrSignWithAux(k, m, randomness))),
        scheme[Ristretto255Schnorr]("ristretto255-Schnorr", (k, m) => get(Ristretto255Schnorr.signWithRandomness(k, m, randomness)))
      ).asJson,
      "bip39" -> bip39.asJson,
      "slip10Ed25519" -> Json.obj(
        "seed"   -> hdSeed.bytes.toHex.asJson,
        "path"   -> slip10Path.asJson,
        "master" -> extended(get(Slip10Ed25519.master(hdSeed))),
        "child"  -> extended(get(ChildIndex.path(slip10Path).flatMap(Slip10Ed25519.derive(hdSeed, _))))
      ),
      "bip32Secp256k1" -> Json.obj(
        "seed"         -> hdSeed.bytes.toHex.asJson,
        "path"         -> bip32Path.asJson,
        "master"       -> extended(get(bip32.master(hdSeed))),
        "child"        -> extended(bip32Key),
        "verifyingKey" -> secp.ecdsaPublicKeyOf.verifyingKey(bip32Key.key).bytes.toHex.asJson
      ),
      "ed25519Bip32" -> Json.obj(
        "entropy"             -> seeds.head.take(16).toHex.asJson,
        "passphrase"          -> "".asJson,
        "root"                -> extended(icarusRoot),
        "accountPath"         -> accountPath.asJson,
        "account"             -> extended(account),
        "accountVerifyingKey" -> Ed25519Bip32.publicKeyOf.verifyingKey(account.key).bytes.toHex.asJson,
        "addressPath"         -> "m/0/0".asJson,
        "address"             -> extended(address),
        "addressVerifyingKey" -> Ed25519Bip32.publicKeyOf.verifyingKey(address.key).bytes.toHex.asJson,
        "signatures" -> messages
          .map(m => Json.obj("message" -> m.toHex.asJson, "signature" -> icarusSign(address.key, m).bytes.toHex.asJson))
          .asJson
      )
    )
  }

  val render: IO[String] =
    (
      Secp256k1.load[IO],
      WordList.load[IO](Bip39.Language.English).flatMap(_.leftMap(e => new IllegalStateException(e.toString)).liftTo[IO])
    )
      .mapN(build)
      .map(_.spaces2 + "\n")
}

object CryptoVectorGen extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    CryptoVectors.render.flatMap(r => IO(Files.write(CryptoVectors.path, r.getBytes(UTF_8)))).as(ExitCode.Success)
}

object CryptoVectorsSuite extends SimpleIOSuite {

  test("committed signing vectors match the reference implementation") {
    (IO(new String(Files.readAllBytes(CryptoVectors.path), UTF_8)), CryptoVectors.render).mapN { (committed, rendered) =>
      expect(
        committed == rendered,
        s"${CryptoVectors.path} is stale; regenerate with: sbt \"crypto/Test/runMain xyz.kd5ujc.crypto.CryptoVectorGen\""
      )
    }
  }

  test("every committed signature verifies, and public derivation reproduces the address key") {
    Secp256k1.load[IO].map { secp =>
      val instances = new Secp256k1.Instances(secp)
      import instances._
      def check[S](sign: (SigningKey[S], ByteVector) => Signature[S])(implicit D: SeedDerivation[S], V: Verifier[S]) =
        CryptoVectors.seeds.foldMap { seed =>
          val kp = D.fromSeed(seed).toOption.get
          CryptoVectors.messages.foldMap(m => expect(V.verify(kp.verifying, m, sign(kp.signing, m))))
        }
      val root = Ed25519Bip32.icarus(Secret(CryptoVectors.seeds.head.take(16))).toOption.get
      val account = ChildIndex.path("m/1852'/1815'/0'").flatMap(Ed25519Bip32.derive(root, _)).toOption.get
      val address = ChildIndex.path("m/0/0").flatMap(Ed25519Bip32.derive(account, _)).toOption.get
      val viaPublic = ChildIndex.path("m/0/0").toOption.get.foldM((Ed25519Bip32.publicKeyOf.verifyingKey(account.key), account.chainCode)) {
        case ((vk, cc), i) => Ed25519Bip32.publicChild(vk, cc, i)
      }
      check[Secp256k1TaggedSchnorr]((k, m) => secp.taggedSchnorrSignWithAux(k, m, CryptoVectors.randomness).toOption.get)
        .and(check[Ristretto255Schnorr]((k, m) => Ristretto255Schnorr.signWithRandomness(k, m, CryptoVectors.randomness).toOption.get))
        .and(expect.same(Right((Ed25519Bip32.publicKeyOf.verifyingKey(address.key), address.chainCode)), viaPublic))
    }
  }
}
