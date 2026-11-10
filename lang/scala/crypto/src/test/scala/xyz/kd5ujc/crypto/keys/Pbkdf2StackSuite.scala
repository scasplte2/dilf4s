package xyz.kd5ujc.crypto.keys

import java.nio.charset.StandardCharsets.UTF_8
import javax.crypto.SecretKeyFactory
import javax.crypto.spec.PBEKeySpec

import cats.effect.IO

import xyz.kd5ujc.test.stack.StackProbe

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/** PBKDF2 read whole at a normal stack, however many rounds: each round's sum is flat, never a view of the last. */
object Pbkdf2StackSuite extends SimpleIOSuite {

  private def jdk(password: String, salt: ByteVector, rounds: Int, bytes: Int): ByteVector =
    ByteVector(
      SecretKeyFactory
        .getInstance("PBKDF2WithHmacSHA512")
        .generateSecret(new PBEKeySpec(password.toCharArray, salt.toArray, rounds, bytes * 8))
        .getEncoded
    )

  test("BIP-39's seed (2048 rounds) is the reference vector") {
    WordList.load[IO](Bip39.Language.English).flatMap {
      case Left(e) => IO.pure(failure(s"the English word list: $e"))
      case Right(words) =>
        StackProbe.fits("the seed")(
          Bip39.toMnemonic(Secret(ByteVector.low(16)), words).flatMap(Bip39.seed(_, "TREZOR")).map(_.bytes.toHex)
        ) { seed =>
          expect.same(
            Right(
              "c55257c360c07c72029aebc1b53c05ed0362ada38ead3e3e9efa3708e53495531f09a6987599d18264c1e1c92f2cf141630c7a3c4ab7c81b2f001698e7463b04"
            ),
            seed
          )
        }
    }
  }

  test("Icarus's 96 bytes (4096 rounds, two blocks) are the JDK's PBKDF2") {
    val entropy = ByteVector.fromValidHex("0ccb74f36b7da1649a8144675522d4d8")
    StackProbe.fits("the Icarus master key")(Hmac.pbkdf2Sha512(ByteVector("dilf4s".getBytes(UTF_8)), entropy, 4096, 96).toHex) { key =>
      expect.same(jdk("dilf4s", entropy, 4096, 96).toHex, key)
    }
  }

  test("an Icarus root key reads whole") {
    StackProbe.fits("the Icarus root key")(Ed25519Bip32.icarus(Secret(ByteVector.low(16))).map(k => k.chainCode.toHex)) { chain =>
      expect(chain.exists(_.length == 64), s"a 32-byte chain code, got $chain")
    }
  }
}
