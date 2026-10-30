package xyz.kd5ujc.crypto.keys

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.crypto.signing._

import io.circe.{ACursor, Json, parser}
import scodec.bits.{ByteVector, HexStringSyntax}
import weaver.SimpleIOSuite

object KeyDerivationSuite extends SimpleIOSuite {

  private val english: IO[WordList] = WordList.load[IO](Bip39.Language.English).flatMap(_.leftMap(e => new IllegalStateException(e.toString)).liftTo[IO])

  private def vectors(file: String): List[Json] =
    parser.parse(new String(Files.readAllBytes(Paths.get("docs", "test-vectors", file)), UTF_8)).flatMap(_.as[List[Json]]).toOption.get

  private def str(j: Json, path: String*): String = path.foldLeft(j.hcursor: ACursor)(_.downField(_)).as[String].toOption.get
  private def hx(j: Json, path: String*): ByteVector = ByteVector.fromValidHex(str(j, path: _*))

  // ------------------------------------------------------------------ BIP-39

  /** Trezor reference vectors (passphrase "TREZOR"). */
  private val trezor = List(
    (hex"00000000000000000000000000000000", "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about",
      hex"c55257c360c07c72029aebc1b53c05ed0362ada38ead3e3e9efa3708e53495531f09a6987599d18264c1e1c92f2cf141630c7a3c4ab7c81b2f001698e7463b04"),
    (hex"7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f", "legal winner thank year wave sausage worth useful legal winner thank yellow",
      hex"2e8905819b8723fe2c1d161860e5ee1830318dbf49a83bd451cfb8440c28bd6fa457fe1296106559a3c80937a1c1069be3a3a5bd381ee6260e8d9739fce1f607"),
    (hex"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo vote",
      hex"dd48c104698c30cfe2b6142103248622fb7bb0ff692eebb00089b32d22484e1613912f0a5b694407be899ffd31ed3992c456cdf60f5d4564b8ba3f05a69890ad")
  )

  test("BIP-39: Trezor vectors (entropy -> phrase -> entropy, seed)") {
    english.map { words =>
      trezor.foldMap { case (entropy, phrase, seed) =>
        val mnemonic = Bip39.toMnemonic(Secret(entropy), words)
        expect.same(Right(phrase), mnemonic.map(_.phrase))
          .and(expect.same(Right(Secret(entropy)), Bip39.toEntropy(phrase, words).map(_._2)))
          .and(expect.same(Right(seed), mnemonic.map(m => Bip39.seed(m, "TREZOR").bytes)))
      }
    }
  }

  test("BIP-39: bad checksum, unknown word, wrong length and bad entropy are typed errors") {
    english.map { words =>
      def mnemonicError(phrase: String) = Bip39.toEntropy(phrase, words) match {
        case Left(KeyError.InvalidMnemonic(_)) => true
        case _                                 => false
      }
      expect(mnemonicError(List.fill(12)("abandon").mkString(" ")), "checksum mismatch")
        .and(expect(mnemonicError(List.fill(11)("abandon").mkString(" ") + " abou"), "unknown word"))
        .and(expect(mnemonicError(List.fill(11)("abandon").mkString(" ")), "11 words"))
        .and(expect(Bip39.toMnemonic(Secret(ByteVector.fill(17)(0)), words).isLeft, "17 bytes of entropy"))
        .and(expect(Bip39.toEntropy("  abandon  abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon   about ", words).isRight, "whitespace is normalised"))
    }
  }

  test("BIP-39: every bundled word list loads and round-trips") {
    Bip39.Language.all.traverse(l => WordList.load[IO](l).map(l -> _)).map(_.foldMap { case (l, loaded) =>
      expect(loaded.isRight, s"${l.name} loads").and(
        expect(loaded.flatMap(w => Bip39.toMnemonic(Secret(hex"7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f"), w).flatMap(m => Bip39.toEntropy(m.phrase, w))).map(_._2) == Right(Secret(hex"7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f")), s"${l.name} round-trips")
      )
    })
  }

  pureTest("secrets and mnemonics are redacted") {
    expect(!Secret(hex"deadbeef").toString.contains("deadbeef"))
  }

  // ------------------------------------------------------------------ paths

  pureTest("paths parse hardened and soft segments, and reject malformed ones") {
    expect.same(Right(List(0x8000002cL, 0L, 7L)), ChildIndex.path("m/44'/0/7").map(_.map(_.serialized)))
      .and(expect.same(Right(List(0x80000000L)), ChildIndex.path("m/0h").map(_.map(_.serialized))))
      .and(expect(ChildIndex.path("44'/0").isLeft, "missing m"))
      .and(expect(ChildIndex.path("m/x").isLeft, "non-numeric"))
      .and(expect(ChildIndex.path("m/2147483648").isLeft, "index out of range"))
      .and(expect(ChildIndex.path("m/-1").isLeft, "negative"))
  }

  // ------------------------------------------------------------------ SLIP-10 Ed25519 (vector 1)

  pureTest("SLIP-10 Ed25519: test vector 1") {
    val seed = Secret(hex"000102030405060708090a0b0c0d0e0f")
    val m = Slip10Ed25519.derive(seed, Nil)
    val m0h = ChildIndex.path("m/0'").flatMap(Slip10Ed25519.derive(seed, _))
    expect.same(Right(hex"2b4be7f19ee27bbf30c667b642d5f4aa69fd169872f8fc3059c08ebae2eb19e7"), m.map(_.key.bytes))
      .and(expect.same(Right(hex"90046a93de5380a72b5e45010748567d5ea02bbf6522f979e05c0d8d8ca9fffb"), m.map(_.chainCode)))
      .and(expect.same(Right(hex"68e0fe46dfb67e368c75379acec591dad19df3cde26e63b93a8e704f1dade7a3"), m0h.map(_.key.bytes)))
      .and(expect.same(Right(hex"8b59aa11380b624e81507a27fedda59fea6d0b779a778918a2fd3590e16e9c69"), m0h.map(_.chainCode)))
      .and(expect(ChildIndex.path("m/0").flatMap(Slip10Ed25519.derive(seed, _)).isLeft, "soft indices are rejected"))
  }

  // ------------------------------------------------------------------ BIP32 secp256k1 (vector 1)

  test("BIP32 secp256k1: test vector 1") {
    Secp256k1.load[IO].map { backend =>
      val bip32 = new Bip32Secp256k1(backend)
      val seed = Secret(hex"000102030405060708090a0b0c0d0e0f")
      def at(p: String) = ChildIndex.path(p).flatMap(bip32.derive(seed, _))
      expect.same(Right(hex"e8f32e723decf4051aefac8e2c93c9c5b214313817cdb01a1494b917c8436b35"), at("m").map(_.key.bytes))
        .and(expect.same(Right(hex"873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d508"), at("m").map(_.chainCode)))
        .and(expect.same(Right(hex"edb2e14f9ee77d26dd93b4ecede8d16ed408ce149b6cd80b0715a2d911a0afea"), at("m/0'").map(_.key.bytes)))
        .and(expect.same(Right(hex"47fdacbd0f1097043b78c63c20c34ef4ed9a111d980047ad16282c7ae6236141"), at("m/0'").map(_.chainCode)))
        .and(expect.same(Right(hex"3c6cb8d0f6a264c91ea8b5030fadaa8e538b020f0a387421a12de9319dc93368"), at("m/0'/1").map(_.key.bytes)))
        .and(expect.same(Right(hex"2a7857631386ba23dacac34180dd1983734e444fdbf774041578e9b6adb37c19"), at("m/0'/1").map(_.chainCode)))
    }
  }

  // ------------------------------------------------------------------ BIP32-Ed25519

  private def extended(bytes: ByteVector): ExtendedKey[Ed25519Bip32] =
    Ed25519Bip32.codec.signingKey(bytes.take(64)).map(ExtendedKey(_, bytes.drop(64))).toOption.get

  /**
   * The BramblSc signing vectors use unclamped `kL`, which the codec rightly refuses; they exercise the signing equation
   * only. Upstream asserts only that signatures verify: vector 3's published signature is valid but was not produced by
   * the RFC 8032 nonce rule from the stated key (an independent reference computation agrees with ours), so for it we
   * check verification rather than bytes.
   */
  pureTest("BIP32-Ed25519: signing known answers (BramblSc vector file)") {
    vectors("ExtendedEd25519.json").zipWithIndex.foldMap { case (v, i) =>
      val sk = new SigningKey[Ed25519Bip32](hx(v, "inputs", "secretKey").take(64))
      val msg = hx(v, "inputs", "message")
      val sig = Ed25519Bip32.signer.sign(sk, msg)
      val vk = Ed25519Bip32.publicKeyOf.verifyingKey(sk)
      val published = Signature[Ed25519Bip32](hx(v, "outputs", "signature"))
      expect.same(hx(v, "outputs", "verificationKey").take(32), vk.bytes)
        .and(expect(Ed25519Bip32.verifier.verify(vk, msg, sig), "our signature verifies"))
        .and(
          if (i == 2) expect(Ed25519Bip32.verifier.verify(vk, msg, published), "the published signature verifies")
          else expect.same(published, sig)
        )
    }
  }

  test("BIP-39: mnemonic -> entropy (BramblSc vectors)") {
    english.map { words =>
      vectors("Bip39MnemonicToEntropy.json").foldMap { v =>
        expect.same(Right(Secret(hx(v, "outputs", "entropy"))), Bip39.toEntropy(str(v, "inputs", "mnemonic"), words).map(_._2))
      }
    }
  }

  pureTest("BIP32-Ed25519: child key derivation (Brambl CKD vectors), private and public") {
    vectors("Bip32Ed25519Ckd.json").foldMap { v =>
      val root = extended(hx(v, "inputs", "rootSecretKey"))
      val path = v.hcursor.downField("inputs").downField("path").as[List[(String, Long)]].toOption.get.traverse {
        case ("hard", i) => ChildIndex.hardened(i)
        case (_, i)      => ChildIndex.soft(i)
      }.toOption.get
      val child = Ed25519Bip32.derive(root, path)
      val expectedVk = hx(v, "outputs", "childVerificationKey")
      // public derivation from the deepest hardened ancestor through the trailing soft indices
      val (hard, soft) = (path.reverse.dropWhile(!_.hardened).reverse, path.reverse.takeWhile(!_.hardened).reverse)
      val viaPublic = Ed25519Bip32.derive(root, hard).flatMap { anchor =>
        soft.foldM((Ed25519Bip32.publicKeyOf.verifyingKey(anchor.key), anchor.chainCode)) { case ((vk, cc), i) => Ed25519Bip32.publicChild(vk, cc, i) }
      }
      expect.same(Right(hx(v, "outputs", "childSecretKey")), child.map(c => c.key.bytes ++ c.chainCode))
        .and(expect.same(Right(expectedVk), child.map(c => Ed25519Bip32.publicKeyOf.verifyingKey(c.key).bytes ++ c.chainCode)))
        .and(expect.same(Right(expectedVk), viaPublic.map { case (vk, cc) => vk.bytes ++ cc }))
    }
  }

  test("BIP32-Ed25519: Icarus master keys from mnemonics (Brambl vectors)") {
    english.map { words =>
      vectors("Bip32Ed25519Icarus.json").foldMap { v =>
        val key = Bip39.toEntropy(str(v, "inputs", "mnemonic"), words).flatMap { case (_, e) => Ed25519Bip32.icarus(e, str(v, "inputs", "password")) }
        expect.same(Right(hx(v, "outputs", "extendedEd25519")), key.map(k => k.key.bytes ++ k.chainCode))
      }
    }
  }

  pureTest("BIP32-Ed25519: hardened public derivation and unclamped keys are rejected") {
    val anyVk = Ed25519Bip32.publicKeyOf.verifyingKey(extended(hx(vectors("Bip32Ed25519Ckd.json").head, "inputs", "rootSecretKey")).key)
    expect(ChildIndex.hardened(0).flatMap(Ed25519Bip32.publicChild(anyVk, ByteVector.fill(32)(0), _)).isLeft)
      .and(expect(Ed25519Bip32.codec.signingKey(hx(vectors("ExtendedEd25519.json").head, "inputs", "secretKey").take(64)).isLeft, "unclamped kL"))
  }
}
