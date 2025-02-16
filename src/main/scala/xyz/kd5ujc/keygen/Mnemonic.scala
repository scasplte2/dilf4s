package xyz.kd5ujc.keygen

import cats.data.{NonEmptyChain, ValidatedNec}
import cats.syntax.all._
import cats.MonadThrow
import cats.effect.std.Random
import cats.effect.{Resource, Sync}

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

trait Mnemonic[F[_]] {
  def entropyToPhrase(entropy: Entropy, language: Language): F[Phrase]
  def phraseToEntropy(phrase: String, language: Language): F[Entropy]
  def validatePhrase(phrase: String, language: Language): F[ValidatedNec[MnemonicError, Phrase]]
}

object Mnemonic {
  def make[F[_]: Sync: Random]: Resource[F, Mnemonic[F]] = {
    for {
      wordLoader <- WordListLoader.resource[F]
    } yield new Bip39Mnemonic[F](wordLoader)
  }
}

class Bip39Mnemonic[F[_]: MonadThrow](wordListLoader: WordListLoader[F]) extends Mnemonic[F] {

  def entropyToPhrase(entropy: Entropy, language: Language): F[Phrase] = {
    for {
      wordList <- wordListLoader.load(language)
      binaryString = entropyToBinary(entropy)
      checksum <- calculateChecksum(entropy)
      bitsWithChecksum = binaryString + checksum
      indices = bitsWithChecksum.grouped(11)
        .map(binary => Integer.parseInt(binary, 2))
        .toList
      words = indices.map(wordList)
      phrase = Phrase(words, language)
    } yield phrase
  }

  def phraseToEntropy(phrase: String, language: Language): F[Entropy] = {
    for {
      validated <- validatePhrase(phrase, language)
      entropy <- validated.fold(
        errors => MonadThrow[F].raiseError(MnemonicError.ValidationError(errors)),
        phrase => phraseToEntropyInternal(phrase)
      )
    } yield entropy
  }

  def validatePhrase(phrase: String, language: Language): F[ValidatedNec[MnemonicError, Phrase]] = {
    for {
      wordList <- wordListLoader.load(language)
      words = phrase.trim.split("\\s+").toList
      validation = validateWords(words, wordList, language)
    } yield validation
  }

  private def validateWords(
                             words: List[String],
                             wordList: List[String],
                             language: Language
                           ): ValidatedNec[MnemonicError, Phrase] = {
    val validations = List(
      validateWordCount(words),
      validateWordsInList(words, wordList),
      validateChecksum(words, wordList)
    )

    validations.sequence.map(_ => Phrase(words, language))
  }

  private def validateWordCount(words: List[String]): ValidatedNec[MnemonicError, Unit] = {
    val validCounts = Set(12, 15, 18, 21, 24)
    if (validCounts.contains(words.length)) ().validNec
    else MnemonicError.WordCountError(validCounts.min, words.length).invalidNec
  }

  private def validateWordsInList(
                                   words: List[String],
                                   wordList: List[String]
                                 ): ValidatedNec[MnemonicError, Unit] = {
    val invalidWords = words.filterNot(wordList.contains)
    if (invalidWords.isEmpty) ().validNec
    else MnemonicError.InvalidWordList(s"Invalid words: ${invalidWords.mkString(", ")}").invalidNec
  }

  private def validateChecksum(
                                words: List[String],
                                wordList: List[String]
                              ): ValidatedNec[MnemonicError, Unit] = {
    // Convert words to binary
    val indices = words.map(wordList.indexOf)
    val bits = indices.map(i => f"$i%11s".replace(' ', '0')).mkString

    // Split into entropy and checksum
    val checksumLength = words.length / 3
    val (entropyBits, checksumBits) = bits.splitAt(bits.length - checksumLength)

    // Calculate expected checksum
    val entropy = binaryStringToBytes(entropyBits)
    val expectedChecksum = calculateChecksumBits(entropy)

    if (checksumBits == expectedChecksum) ().validNec
    else MnemonicError.ChecksumError(expectedChecksum, checksumBits).invalidNec
  }

  private def calculateChecksum(entropy: Entropy): F[String] = MonadThrow[F].catchNonFatal {
    val hash = MessageDigest.getInstance("SHA-256").digest(entropy.bytes)
    val checksumLength = entropy.size.checksumBits
    val firstByte = hash(0)
    String.format(s"%8s", Integer.toBinaryString(firstByte & 0xFF))
      .replace(' ', '0')
      .take(checksumLength)
  }

  private def entropyToBinary(entropy: Entropy): String =
    entropy.bytes.map(b => String.format("%8s", Integer.toBinaryString(b & 0xFF)).replace(' ', '0')).mkString

  private def binaryStringToBytes(binary: String): Array[Byte] = {
    binary.grouped(8).map(b => Integer.parseInt(b, 2).toByte).toArray
  }

  private def phraseToEntropyInternal(phrase: Phrase): F[Entropy] = {
    for {
      wordList <- wordListLoader.load(phrase.language)
      indices = phrase.words.map(wordList.indexOf)
      binary = indices.map(i => f"$i%11s".replace(' ', '0')).mkString
      entropyBits = binary.take(binary.length - phrase.words.length / 3)
      entropyBytes = binaryStringToBytes(entropyBits)
      entropy <- Entropy.fromBytes[F](entropyBytes)
    } yield entropy
  }

  private def calculateChecksumBits(entropy: Array[Byte]): String = {
    val hash = MessageDigest.getInstance("SHA-256").digest(entropy)
    val checksumLength = entropy.length / 4
    String.format(s"%8s", Integer.toBinaryString(hash(0) & 0xFF))
      .replace(' ', '0')
      .take(checksumLength)
  }
}

sealed abstract class Language(val identifier: String, val wordCount: Int = 2048)

object Language {
  case object English extends Language("english")
}

/**
 * Represents a validated mnemonic phrase
 */
case class Phrase(words: List[String], language: Language)

sealed trait MnemonicError extends Exception
object MnemonicError {
  case class InvalidEntropy(reason: String) extends MnemonicError
  case class InvalidWordList(reason: String) extends MnemonicError
  case class ValidationError(errors: NonEmptyChain[MnemonicError]) extends MnemonicError
  case class ChecksumError(expected: String, actual: String) extends MnemonicError
  case class WordCountError(expected: Int, actual: Int) extends MnemonicError
}

sealed trait WordListLoader[F[_]] {
  def load(language: Language): F[List[String]]
}

object WordListLoader {
  // Known SHA-256 checksums for official BIP-39 word lists
  private val KnownChecksums: Map[Language, String] = Map(
    Language.English -> "ad90bf3beb7b0eb7e5acd74727dc0da96e0a280a258354e7293fb7e211ac03db"
    // Add other languages as needed
  )

  def resource[F[_]: Sync]: Resource[F, WordListLoader[F]] = {
    val acquire = Sync[F].delay(new ClasspathWordListLoader[F])
    Resource.eval(acquire)
  }

  private final class ClasspathWordListLoader[F[_]: MonadThrow] extends WordListLoader[F] {
    def load(language: Language): F[List[String]] = {
      for {
        words <- loadFromClasspath(language).adaptError {
          case e => WordListError.LoadError(e)
        }
        _ <- validateWordList(words, language)
        _ <- verifyChecksum(words, language)
      } yield words
    }

    private def loadFromClasspath(language: Language): F[List[String]] = {
      MonadThrow[F].catchNonFatal {
        val path = s"bip-0039/${language.identifier}.txt"
        val source = scala.io.Source.fromResource(path)
        try {
          source.getLines()
            .map(_.trim)
            .filter(_.nonEmpty)
            .toList
        } finally {
          source.close()
        }
      }
    }

    private def validateWordList(words: List[String], language: Language): F[Unit] = {
      MonadThrow[F].catchNonFatal {
        if (words.length != language.wordCount) {
          throw WordListError.InvalidWordList(
            language,
            s"Expected ${language.wordCount} words, got ${words.length}"
          )
        }

        val duplicates = words.groupBy(identity).collect {
          case (word, occurrences) if occurrences.length > 1 => word
        }
        if (duplicates.nonEmpty) {
          throw WordListError.InvalidWordList(
            language,
            s"Found duplicate words: ${duplicates.mkString(", ")}"
          )
        }

        val invalidWords = words.filter(word => !word.matches("^[a-z]+$"))
        if (invalidWords.nonEmpty) {
          throw WordListError.InvalidWordList(
            language,
            s"Found words with invalid characters: ${invalidWords.mkString(", ")}"
          )
        }
      }
    }

    private def verifyChecksum(words: List[String], language: Language): F[Unit] = {
      MonadThrow[F].catchNonFatal {
        KnownChecksums.get(language).foreach { expectedHash =>
          val actualHash = calculateHash(words)
          if (actualHash != expectedHash) {
            throw WordListError.ChecksumMismatch(language, expectedHash, actualHash)
          }
        }
      }
    }

    private def calculateHash(words: List[String]): String = {
      val preimage = words.mkString.getBytes(StandardCharsets.UTF_8)
      val digest = MessageDigest.getInstance("SHA-256").digest(preimage)
      digest.map("%02x".format(_)).mkString
    }
  }
}

sealed trait WordListError extends RuntimeException
object WordListError {
  case class LoadError(cause: Throwable) extends WordListError {
    override def getMessage: String = s"Failed to load word list: ${cause.getMessage}"
  }
  case class ChecksumMismatch(language: Language, expected: String, actual: String) extends WordListError {
    override def getMessage: String = s"Checksum mismatch for ${language.identifier}. Expected: $expected, Got: $actual"
  }
  case class InvalidWordList(language: Language, reason: String) extends WordListError {
    override def getMessage: String = s"Invalid word list for ${language.identifier}: $reason"
  }
}