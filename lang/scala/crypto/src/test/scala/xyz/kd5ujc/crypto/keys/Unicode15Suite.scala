package xyz.kd5ujc.crypto.keys

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.syntax.all._

import xyz.kd5ujc.models.crypto_errors.KeyError

import scodec.bits.ByteVector
import weaver.SimpleIOSuite

/**
 * BIP-39 normalises as of Unicode 15.0 (signing.md section 2.1): a phrase, a passphrase or a word holding a code point
 * 15.0 does not assign is refused before it is normalised. The vectors pin the refusals a JSON text can carry; this
 * pins the table and what JSON cannot (a lone surrogate).
 */
object Unicode15Suite extends SimpleIOSuite {

  private val shared = Paths.get("shared", "unicode", "assigned-15.0.txt")

  test("the table is shared/unicode/assigned-15.0.txt") {
    IO.blocking(Files.readString(shared, UTF_8)).map { text =>
      val ranges = text.linesIterator.filterNot(l => l.startsWith("#") || l.isBlank).toList.map { l =>
        val Array(a, b) = l.split("\\.\\.")
        (Integer.parseInt(a, 16), Integer.parseInt(b, 16))
      }
      expect.same(ranges, Unicode15.ranges).and(expect(ranges.size == 707, s"707 ranges, got ${ranges.size}"))
    }
  }

  pureTest("assigned in 15.0, and not") {
    val in = List(0x0041, 0x1e030, 0x1d400, 0xfb01, 0xe000, 0x1f6dc, 0x10fffd)
    val out = List(0x1ccd6, 0xfdd0, 0xfffe, 0x10ffff, 0xd800, 0xdfff, -1, 0x110000)
    in.foldMap(cp => expect(Unicode15.assigned(cp), f"U+$cp%04X is assigned in 15.0"))
      .and(out.foldMap(cp => expect(!Unicode15.assigned(cp), f"U+$cp%04X is not assigned in 15.0")))
  }

  test("a lone surrogate in a phrase, a passphrase or a word is refused") {
    val lone = "\ud800"
    WordList.load[IO](Bip39.Language.English).map { loaded =>
      val english = loaded.fold(e => sys.error(e.toString), identity)
      val valid = (List.fill(11)("abandon") :+ "about").mkString(" ")
      val mnemonic = Bip39.toEntropy(valid, english).map(_._1)
      expect(Bip39.toEntropy(valid + lone, english).left.exists(_.kind == "InvalidMnemonic"), "the phrase")
        .and(expect(mnemonic.flatMap(Bip39.seed(_, s"dilf4s$lone")).left.exists(_.kind == "InvalidPassphrase"), "the passphrase"))
        .and(
          expect(
            WordList(Bip39.Language.English, english.words.updated(0, s"abandon$lone")).left.exists(_.kind == "InvalidWordList"),
            "a word"
          )
        )
    }
  }

  test("a caller's words are normalised, so a precomposed word matches a decomposed phrase") {
    WordList.load[IO](Bip39.Language.English).map { loaded =>
      val english = loaded.fold(e => sys.error(e.toString), identity)
      // word 0 given precomposed (U+00E9) is held decomposed (e, U+0301); zero entropy spells it eleven times, and a
      // phrase spelling it precomposed again reads back, as in every SDK
      val words = english.words.updated(0, "caf\u00e9")
      val entropy = Secret(ByteVector.low(16))
      val decomposed = WordList(Bip39.Language.English, words).flatMap { list =>
        Bip39.toMnemonic(entropy, list).flatMap(m => Bip39.toEntropy(m.words.mkString(" ").replace("cafe\u0301", "caf\u00e9"), list))
      }
      expect(words.size == 2048, "2048 words")
        .and(expect(decomposed.map(_._2.bytes) == Right(entropy.bytes), s"the phrase reads back its entropy: $decomposed"))
        .and(expect(WordList(Bip39.Language.English, words.updated(1, "caf\u00e9")).isLeft, "two words equal once normalised are refused"))
        .and(expect(WordList(Bip39.Language.English, words.updated(1, "ab andon")).isLeft, "a word holding a separator is refused"))
        .and(expect((WordList(Bip39.Language.English, words): Either[KeyError, WordList]).isRight, "the list is accepted"))
    }
  }
}
