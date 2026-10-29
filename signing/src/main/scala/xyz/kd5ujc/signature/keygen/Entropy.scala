package xyz.kd5ujc.signature.keygen

import cats.data.ValidatedNec
import cats.effect.std.Random
import cats.effect.{Resource, Sync}
import cats.syntax.all._

sealed trait Entropy {
  def bytes: Array[Byte]
  def size: EntropySize
}

object Entropy {
  private final case class EntropyImpl(bytes: Array[Byte], size: EntropySize) extends Entropy {
    private def constantTimeEquals(a: Array[Byte], b: Array[Byte]): Boolean = {
      if (a.length != b.length) return false
      var result = 0
      for (i <- a.indices)
        result |= a(i) ^ b(i)
      result == 0
    }

    override def equals(obj: Any): Boolean = obj match {
      case other: EntropyImpl => size == other.size && constantTimeEquals(bytes, other.bytes)
      case _                  => false
    }

    override def hashCode(): Int =
      31 * size.hashCode

    def wipe(): Unit =
      java.util.Arrays.fill(bytes, 0: Byte)

    def withBytes[A](f: Array[Byte] => A): A = {
      val copy = bytes.clone()
      try f(copy)
      finally java.util.Arrays.fill(copy, 0: Byte)
    }
  }

  def generate[F[_]: Sync: Random](size: EntropySize): F[Entropy] =
    Random[F].nextBytes(size.bytes).map(bytes => EntropyImpl(bytes, size))

  def fromBytes[F[_]: Sync](bytes: Array[Byte]): F[Entropy] =
    for {
      validated <- Sync[F].fromEither(
        EntropySize
          .validated(bytes.length * 8)
          .toEither
          .leftMap(errors => EntropyError.ValidationError(errors.iterator.mkString(", ")))
      )
      _ <- validateBytes(bytes).liftTo[F]
    } yield EntropyImpl(bytes.clone(), validated)

  def generateResource[F[_]: Sync: Random](size: EntropySize): Resource[F, Entropy] =
    Resource.make(
      acquire = generate[F](size)
    )(
      release = entropy =>
        Sync[F].delay {
          java.util.Arrays.fill(entropy.bytes, 0: Byte)
        }
    )

  private def validateBytes(bytes: Array[Byte]): Either[EntropyError, Unit] =
    if (Set(16, 20, 24, 28, 32).contains(bytes.length)) ().asRight
    else EntropyError.InvalidByteLength(bytes.length).asLeft

  private[keygen] def unsafe(bytes: Array[Byte], size: EntropySize): Entropy =
    EntropyImpl(bytes.clone(), size)
}

sealed trait EntropySize {
  def bits: Int
  def bytes: Int = bits / 8
  def checksumBits: Int = bits / 32
}

object EntropySize {
  case object Bits128 extends EntropySize { val bits = 128 }
  case object Bits160 extends EntropySize { val bits = 160 }
  case object Bits192 extends EntropySize { val bits = 192 }
  case object Bits224 extends EntropySize { val bits = 224 }
  case object Bits256 extends EntropySize { val bits = 256 }

  def validated(bits: Int): ValidatedNec[EntropyError, EntropySize] = {
    val validations = List(
      validateMultipleOf32(bits),
      validateKnownSize(bits)
    )

    validations.sequence.map(_ => fromBits(bits).getOrElse(Bits128))
  }

  def fromWordCount(wordCount: Int): Either[EntropyError, EntropySize] =
    wordCount match {
      case 12 => Right(Bits128)
      case 15 => Right(Bits160)
      case 18 => Right(Bits192)
      case 21 => Right(Bits224)
      case 24 => Right(Bits256)
      case _  => Left(EntropyError.InvalidSize(s"Invalid word count: $wordCount"))
    }

  private def validateMultipleOf32(bits: Int): ValidatedNec[EntropyError, Unit] =
    if (bits % 32 == 0) ().validNec
    else EntropyError.InvalidSize(s"Entropy size must be multiple of 32 bits, got: $bits").invalidNec

  private def validateKnownSize(bits: Int): ValidatedNec[EntropyError, Unit] =
    if (Set(128, 160, 192, 224, 256).contains(bits)) ().validNec
    else EntropyError.InvalidSize(s"Entropy size must be one of: 128, 160, 192, 224, 256 bits, got: $bits").invalidNec

  def fromBits(bits: Int): Either[EntropyError, EntropySize] = bits match {
    case 128 => Right(Bits128)
    case 160 => Right(Bits160)
    case 192 => Right(Bits192)
    case 224 => Right(Bits224)
    case 256 => Right(Bits256)
    case _   => Left(EntropyError.InvalidSize(s"Invalid entropy size: $bits bits"))
  }
}

sealed trait EntropyError extends Throwable
object EntropyError {
  case class InvalidSize(message: String) extends EntropyError {
    override def getMessage: String = message
  }
  case class InvalidByteLength(length: Int) extends EntropyError {
    override def getMessage: String = s"Invalid byte length: $length. Must correspond to a valid entropy size"
  }
  case class ValidationError(message: String) extends EntropyError {
    override def getMessage: String = message
  }
}
