package xyz.kd5ujc.signature.keygen

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

import cats.data.{NonEmptyChain, ValidatedNec}
import cats.effect.std.Random
import cats.effect.{Resource, Sync}
import cats.syntax.all._
import cats.{MonadThrow, Show}

trait Mnemonic[F[_]] {
  def entropyToPhrase(entropy: Entropy, language: Language): F[MnemonicPhrase]
  def phraseToEntropy(phrase:  String, language:  Language): F[Entropy]
  def validatePhrase(phrase:   String, language:  Language): F[ValidatedNec[MnemonicError, MnemonicPhrase]]
}

object Mnemonic {
  def make[F[_]: Sync: Random]: Resource[F, Mnemonic[F]] =
    WordListLoader.resource[F].map { loader =>
      new Bip39Mnemonic[F](loader)
    }

  def generatePhrase[F[_]: Sync: Random](
    wordCount: Int = 12,
    language:  Language = Language.English
  ): F[MnemonicPhrase] =
    for {
      size <- Sync[F].fromEither(
        EntropySize
          .fromWordCount(wordCount)
          .leftMap(err => new IllegalArgumentException(err.getMessage))
      )
      entropy <- Entropy.generate[F](size)
      mnemonic <- make[F].use { m =>
        m.entropyToPhrase(entropy, language)
      }
    } yield mnemonic
}

class Bip39Mnemonic[F[_]: Sync](wordListLoader: WordListLoader[F]) extends Mnemonic[F] {
  def validatePhrase(phrase: String, language: Language): F[ValidatedNec[MnemonicError, MnemonicPhrase]] =
    for {
      wordList <- wordListLoader.load(language)
      words = phrase.toLowerCase.trim.split("\\s+").toList
      validation = validateWords(words, wordList, language)
    } yield validation

  private def normalizePhrase(phrase: String, language: Language): String = {
    val normalized = language match {
      case Language.Japanese =>
        java.text.Normalizer.normalize(phrase, java.text.Normalizer.Form.NFKD)
      case _ => phrase
    }

    normalized.toLowerCase.trim
  }

  private def validateChecksum(words: List[String], wordList: List[String]): ValidatedNec[MnemonicError, Unit] = {
    val indices = words.map(wordList.indexOf)

    val bits = indices.map(i => String.format("%11s", Integer.toBinaryString(i)).replace(' ', '0')).mkString

    val checksumLength = words.length / 3 // Per BIP39 spec
    val (entropyBits, checksumBits) = bits.splitAt(bits.length - checksumLength)

    val entropy = entropyBits
      .grouped(8)
      .map(b => Integer.parseInt(b, 2).toByte)
      .toArray

    val expectedChecksum = {
      val hash = MessageDigest.getInstance("SHA-256").digest(entropy)
      String
        .format("%8s", Integer.toBinaryString(hash(0) & 0xff))
        .replace(' ', '0')
        .take(checksumLength)
    }

    // Constant time comparison
    var result = 0
    if (checksumBits.length == expectedChecksum.length) {
      for (i <- checksumBits.indices)
        result |= checksumBits.charAt(i) ^ expectedChecksum.charAt(i)
      if (result == 0) ().validNec
      else MnemonicError.ChecksumError(expectedChecksum, checksumBits).invalidNec
    } else {
      MnemonicError.ChecksumError(expectedChecksum, checksumBits).invalidNec
    }
  }

  def entropyToPhrase(entropy: Entropy, language: Language): F[MnemonicPhrase] =
    for {
      wordList     <- wordListLoader.load(language)
      binaryString <- entropyToBinary(entropy)
      checksum     <- calculateChecksum(entropy)
      bitsWithChecksum = binaryString + checksum
      indices = bitsWithChecksum
        .grouped(11)
        .map(binary => Integer.parseInt(binary, 2))
        .toList
      words = indices.map(wordList)
      phrase = MnemonicPhrase(words, language)
    } yield phrase

  def phraseToEntropy(phrase: String, language: Language): F[Entropy] =
    for {
      validated <- validatePhrase(phrase, language)
      entropy <- validated.fold(
        errors => MonadThrow[F].raiseError(MnemonicError.ValidationError(errors)),
        phrase => phraseToEntropyInternal(phrase)
      )
    } yield entropy

  private def validateWords(
    words:    List[String],
    wordList: List[String],
    language: Language
  ): ValidatedNec[MnemonicError, MnemonicPhrase] = {
    val validations = List(
      validateWordCount(words),
      validateWordsInList(words, wordList),
      validateChecksum(words, wordList)
    )

    validations.sequence.map(_ => MnemonicPhrase(words, language))
  }

  private def validateWordCount(words: List[String]): ValidatedNec[MnemonicError, Unit] = {
    val validCounts = Set(12, 15, 18, 21, 24)
    if (validCounts.contains(words.length)) ().validNec
    else MnemonicError.WordCountError(validCounts.min, words.length).invalidNec
  }

  private def validateWordsInList(
    words:    List[String],
    wordList: List[String]
  ): ValidatedNec[MnemonicError, Unit] = {
    val invalidWords = words.filterNot(wordList.contains)
    if (invalidWords.isEmpty) ().validNec
    else MnemonicError.InvalidWordList(s"Invalid words: ${invalidWords.mkString(", ")}").invalidNec
  }

  private def calculateChecksum(entropy: Entropy): F[String] = MonadThrow[F].catchNonFatal {
    val hash = MessageDigest.getInstance("SHA-256").digest(entropy.bytes)
    val checksumLength = entropy.size.checksumBits
    val firstByte = hash(0)
    String
      .format(s"%8s", Integer.toBinaryString(firstByte & 0xff))
      .replace(' ', '0')
      .take(checksumLength)
  }

  private def entropyToBinary(entropy: Entropy): F[String] =
    Sync[F].delay {
      entropy.bytes.map(b => String.format("%8s", Integer.toBinaryString(b & 0xff)).replace(' ', '0')).mkString
    }

  private def binaryStringToBytes(binary: String): Array[Byte] =
    binary.grouped(8).map(b => Integer.parseInt(b, 2).toByte).toArray

  private def phraseToEntropyInternal(phrase: MnemonicPhrase): F[Entropy] =
    for {
      wordList <- wordListLoader.load(phrase.language)
      indices = phrase.words.map(wordList.indexOf)
      binary = indices.map(i => f"$i%11s".replace(' ', '0')).mkString
      entropyBits = binary.take(binary.length - phrase.words.length / 3)
      entropyBytes = binaryStringToBytes(entropyBits)
      entropy <- Entropy.fromBytes[F](entropyBytes)
    } yield entropy
}

sealed abstract class Language(val identifier: String, val wordCount: Int = 2048)

object Language {
  case object Czech extends Language("czech")
  case object French extends Language("french")
  case object Korean extends Language("korean")
  case object English extends Language("english")
  case object Spanish extends Language("spanish")
  case object Italian extends Language("italian")
  case object Japanese extends Language("japanese")
  case object Portuguese extends Language("portuguese")
  case object ChineseSimplified extends Language("chinese_simplified")
  case object ChineseTraditional extends Language("chinese_traditional")
}

/**
 * Represents a validated mnemonic phrase
 */
case class MnemonicPhrase(words: List[String], language: Language)

object MnemonicPhrase {
  implicit val showMnemonicPhrase: Show[MnemonicPhrase] = Show.show { phrase =>
    val normalized = phrase.language match {
      case Language.Japanese =>
        phrase.words.map(word => java.text.Normalizer.normalize(word, java.text.Normalizer.Form.NFKD))
      case _ => phrase.words
    }

    normalized.mkString(" ")
  }
}

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
    Language.English            -> "ad90bf3beb7b0eb7e5acd74727dc0da96e0a280a258354e7293fb7e211ac03db",
    Language.Czech              -> "f9016943461800f7870363b4c301c814dbcb8f4de801e6c87d859eba840469d5",
    Language.French             -> "9cbdaadbd3ce9cbaee1b360fce45e935b21e3e2c56d9fcd56b3398ced2371866",
    Language.Korean             -> "f04f70b26cfef84474ff56582e798bcbc1a5572877d14c88ec66551272688c73",
    Language.Spanish            -> "a556a26c6a5bb36db0fb7d8bf579cb7465fcaeec03957c0dda61b569962d9da5",
    Language.Italian            -> "80d2e90d7436603fd6e57cd9af6f839391e64beac1a3e015804f094fcc5ab24c",
    Language.Japanese           -> "d9d1fde478cbeb45c06b93632a487eefa24f6533970f866ae81f136fbf810160",
    Language.Portuguese         -> "eed387d44cf8f32f60754527e265230d8019e8a2277937c71ef812e7a46c93fd",
    Language.ChineseSimplified  -> "bfd683b91db88609fabad8968c7efe4bf69606bf5a49ac4a4ba5e355955670cb",
    Language.ChineseTraditional -> "85b285c4e0e3eb1e52038e2cf4b4f8bba69fd814e1a09e063ce3609a1f67ad62"
  )

  def resource[F[_]: Sync]: Resource[F, WordListLoader[F]] = {
    val acquire = Sync[F].delay(new ClasspathWordListLoader[F])
    Resource.eval(acquire)
  }

  private final class ClasspathWordListLoader[F[_]: Sync] extends WordListLoader[F] {
    private val wordListCache = new java.util.concurrent.ConcurrentHashMap[Language, List[String]]()

    def load(language: Language): F[List[String]] =
      Sync[F].defer {
        Option(wordListCache.get(language)) match {
          case Some(cached) => cached.pure[F]
          case None         => loadAndCache(language)
        }
      }

    private def loadAndCache(language: Language): F[List[String]] =
      for {
        words <- loadFromClasspath(language).adaptError {
          case e => WordListError.LoadError(e)
        }
        _ <- validateWordList(words, language)
        _ <- verifyChecksum(words, language)
        _ <- Sync[F].delay(wordListCache.put(language, words))
      } yield words

    private def loadFromClasspath(language: Language): F[List[String]] =
      Resource
        .make(
          acquire = Sync[F].delay(
            scala.io.Source.fromResource(s"bip-0039/${language.identifier}.txt")
          )
        )(
          release = source => Sync[F].delay(source.close())
        )
        .use { source =>
          Sync[F].delay {
            source
              .getLines()
              .map(_.trim)
              .filter(_.nonEmpty)
              .toList
          }
        }

    private def validateWordList(words: List[String], language: Language): F[Unit] =
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

    private def verifyChecksum(words: List[String], language: Language): F[Unit] =
      MonadThrow[F].catchNonFatal {
        KnownChecksums.get(language).foreach { expectedHash =>
          val actualHash = calculateHash(words)
          if (actualHash != expectedHash) {
            throw WordListError.ChecksumMismatch(language, expectedHash, actualHash)
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
