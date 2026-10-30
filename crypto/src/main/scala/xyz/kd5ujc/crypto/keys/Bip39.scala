package xyz.kd5ujc.crypto.keys

import java.nio.charset.StandardCharsets.UTF_8
import java.text.Normalizer

import cats.effect.{Resource, Sync}
import cats.syntax.all._

import org.bouncycastle.crypto.digests.SHA256Digest
import scodec.bits.{BitVector, ByteVector}

sealed trait KeyError extends Product with Serializable

object KeyError {
  final case class InvalidEntropy(reason: String) extends KeyError
  final case class InvalidMnemonic(reason: String) extends KeyError
  final case class InvalidWordList(reason: String) extends KeyError
  final case class InvalidDerivation(reason: String) extends KeyError
}

/** Secret byte material (seeds, entropy): never printed. */
final class Secret private[crypto] (val bytes: ByteVector) {
  override def toString: String = s"Secret(<${bytes.size} bytes redacted>)"
  override def equals(other: Any): Boolean = other match {
    case that: Secret => java.security.MessageDigest.isEqual(bytes.toArray, that.bytes.toArray)
    case _            => false
  }
  override def hashCode: Int = bytes.size.toInt
}

object Secret {
  def apply(bytes: ByteVector): Secret = new Secret(bytes)
}

/** A BIP-39 mnemonic phrase (secret: never printed). */
final class Mnemonic private[keys] (val words: List[String], val language: Bip39.Language) {
  def phrase: String = words.mkString(language.separator)
  override def toString: String = s"Mnemonic(<${words.size} words redacted>)"
  override def equals(other: Any): Boolean = other match {
    case that: Mnemonic => phrase == that.phrase && language == that.language
    case _              => false
  }
  override def hashCode: Int = words.size
}

/** A validated BIP-39 word list: exactly 2048 distinct words. */
final class WordList private (val language: Bip39.Language, val words: Vector[String]) {
  private val index: Map[String, Int] = words.zipWithIndex.toMap
  def indexOf(word: String): Option[Int] = index.get(word)
}

object WordList {

  def apply(language: Bip39.Language, words: Vector[String]): Either[KeyError, WordList] =
    Either.cond(
      words.size == 2048 && words.distinct.size == 2048,
      new WordList(language, words),
      KeyError.InvalidWordList(s"${language.name}: expected 2048 distinct words, got ${words.size}")
    )

  /** Loads a bundled word list (`bip-0039/<name>.txt`); reading a resource is an effect. */
  def load[F[_]: Sync](language: Bip39.Language): F[Either[KeyError, WordList]] =
    Sync[F].blocking(Option(getClass.getResourceAsStream(s"/bip-0039/${language.name}.txt"))).flatMap {
      case None => (KeyError.InvalidWordList(s"missing resource for ${language.name}"): KeyError).asLeft[WordList].pure[F]
      case Some(stream) =>
        Resource.fromAutoCloseable(Sync[F].pure(stream)).use(in => Sync[F].blocking(new String(in.readAllBytes(), UTF_8))).map { text =>
          apply(language, text.split("\n").toVector.map(w => Normalizer.normalize(w.trim, Normalizer.Form.NFKD)).filter(_.nonEmpty))
        }
    }
}

/** BIP-39: entropy <-> mnemonic, and mnemonic -> 64-byte seed (PBKDF2-HMAC-SHA512, 2048 rounds). */
object Bip39 {

  sealed abstract class Language(val name: String, val separator: String) extends Product with Serializable

  object Language {
    case object English extends Language("english", " ")
    case object Japanese extends Language("japanese", "　")
    case object Korean extends Language("korean", " ")
    case object Spanish extends Language("spanish", " ")
    case object ChineseSimplified extends Language("chinese_simplified", " ")
    case object ChineseTraditional extends Language("chinese_traditional", " ")
    case object French extends Language("french", " ")
    case object Italian extends Language("italian", " ")
    case object Czech extends Language("czech", " ")
    case object Portuguese extends Language("portuguese", " ")

    val all: List[Language] =
      List(English, Japanese, Korean, Spanish, ChineseSimplified, ChineseTraditional, French, Italian, Czech, Portuguese)
  }

  private def sha256(b: ByteVector): ByteVector = {
    val d = new SHA256Digest(); val out = new Array[Byte](32); d.update(b.toArray, 0, b.size.toInt); d.doFinal(out, 0); ByteVector.view(out)
  }

  private def nfkd(s: String): String = Normalizer.normalize(s, Normalizer.Form.NFKD)

  /** 16, 20, 24, 28 or 32 bytes of entropy -> 12..24 words (ENT/32 checksum bits from SHA-256). */
  def toMnemonic(entropy: Secret, words: WordList): Either[KeyError, Mnemonic] = {
    val ent = entropy.bytes
    if (!Set(16L, 20L, 24L, 28L, 32L).contains(ent.size))
      KeyError.InvalidEntropy(s"entropy must be 16..32 bytes in steps of 4, got ${ent.size}").asLeft
    else {
      val bits = ent.bits ++ sha256(ent).bits.take(ent.size * 8 / 32)
      new Mnemonic(bits.grouped(11).map(group => words.words(group.toInt(signed = false))).toList, words.language).asRight
    }
  }

  /** Parses and validates a phrase (word count, membership, checksum), returning its entropy. */
  def toEntropy(phrase: String, words: WordList): Either[KeyError, (Mnemonic, Secret)] = {
    val tokens = nfkd(phrase).trim.split("[\\s　]+").toList.filter(_.nonEmpty)
    for {
      _ <- Either.cond(
        Set(12, 15, 18, 21, 24).contains(tokens.size),
        (),
        KeyError.InvalidMnemonic(s"expected 12, 15, 18, 21 or 24 words, got ${tokens.size}")
      )
      indices <- tokens.traverse(w => words.indexOf(w).toRight(KeyError.InvalidMnemonic("phrase contains a word not in the list")))
      bits = indices.foldLeft(BitVector.empty)((acc, i) => acc ++ BitVector.fromInt(i, 11))
      checksumBits = tokens.size * 11 / 33
      entropy = bits.dropRight(checksumBits.toLong).bytes
      _ <- Either.cond(
        sha256(entropy).bits.take(checksumBits.toLong) == bits.takeRight(checksumBits.toLong),
        (),
        KeyError.InvalidMnemonic("checksum mismatch")
      )
    } yield (new Mnemonic(tokens, words.language), Secret(entropy))
  }

  /** The 64-byte BIP-39 seed: PBKDF2-HMAC-SHA512(NFKD(mnemonic), "mnemonic" ++ NFKD(passphrase), 2048). */
  def seed(mnemonic: Mnemonic, passphrase: String = ""): Secret =
    Secret(
      Hmac.pbkdf2Sha512Block(
        ByteVector(nfkd(mnemonic.words.mkString(" ")).getBytes(UTF_8)),
        ByteVector(("mnemonic" + nfkd(passphrase)).getBytes(UTF_8)),
        2048
      )
    )
}
