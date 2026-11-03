package xyz.kd5ujc.crypto.signing

import java.nio.file.{Files, Path}
import java.util.Locale

import cats.effect.{Resource, Sync}
import cats.syntax.all._

import scala.util.control.NoStackTrace

import com.sun.jna.{Library, Native, NativeLibrary}
import scodec.bits.{ByteVector, hex}

/**
 * Ed25519 verification: the codec's verifying-key check and the two verification rules, from one backend. Both backends
 * give the same verdict on every input (the shared vectors and a differential test pin it), so the choice is one of cost
 * only. [[Ed25519Backend.reference]] is the pure-Scala code in [[Ed25519]]; [[Ed25519Native]] is dilf4s-signing, the
 * Rust that the SDK and the SP1 guest run and `ed25519_verify` is priced from, about five times faster.
 */
trait Ed25519Backend {
  def name: String

  /** Signing keys and signatures are checked for length; a verifying key must also decode under ZIP-215. */
  def codec: SchemeCodec[Ed25519]

  /** ZIP-215, the consensus rule ([[Ed25519.verifier]]). */
  def verifier: Verifier[Ed25519]

  /** libsodium's rule ([[Ed25519.libsodiumVerifier]]). */
  def libsodiumVerifier: Verifier[Ed25519]

  /** The key, then the signature, decoded by [[codec]], then verified by [[verifier]]: their verdict in one step. */
  def verifyBytes(key: ByteVector, message: ByteVector, signature: ByteVector): Either[CryptoError, Boolean]
}

object Ed25519Backend {

  /** The pure-Scala code in [[Ed25519]]: it needs no native library. */
  val reference: Ed25519Backend = new Ed25519Backend {
    val name: String = "pure Scala"
    val codec: SchemeCodec[Ed25519] = Ed25519.codec
    val verifier: Verifier[Ed25519] = Ed25519.verifier
    val libsodiumVerifier: Verifier[Ed25519] = Ed25519.libsodiumVerifier
    def verifyBytes(key: ByteVector, message: ByteVector, signature: ByteVector): Either[CryptoError, Boolean] =
      (codec.verifyingKey(key), codec.signature(signature)).tupled.map { case (k, s) => verifier.verify(k, message, s) }
  }

  /** dilf4s-signing through its native library ([[Ed25519Native.load]]); fails where the library is not bundled. */
  def native[F[_]: Sync]: F[Ed25519Backend] = Ed25519Native.load[F]
}

/**
 * dilf4s-signing's Ed25519 verification (`sdk/rust/jvm-ffi`), through JNA's direct mapping, which adds about 0.6 µs to
 * a call. The sbt build compiles the library with cargo and bundles it in this module's jar, one copy per platform;
 * [[load]] extracts it to a private temporary directory, opens it, and deletes the file. A missing or broken library
 * fails the load with a [[Ed25519Native.LoadError]]: there is no silent fallback to [[Ed25519Backend.reference]], whose
 * verdicts are the same but which costs five times more than `ed25519_verify` is priced at.
 *
 * Every load binds the natives again (JNI binds per class, JVM-wide), to its own copy of the library, so every backend
 * calls the copy bound last. A copy is bound only after it passes the ABI and known-answer checks through its own
 * function pointers, so a bad library never displaces a good one. Each copy is opened with `RTLD_NODELETE`, so none is
 * ever unmapped: a call already running in an earlier copy stays valid when a later load rebinds the class, and JNA
 * closing a library it no longer references unmaps nothing. The cost is one mapping (about 0.4 MB) per load, kept for
 * the life of the process, rejected copies included: load once, at the edge, as [[Ed25519Backend.native]]'s callers do.
 * Sharing one binding across loads instead would need process-global mutable state.
 */
object Ed25519Native {

  val LibraryName: String = "dilf4s_jvm_ffi"

  /** `ABI_VERSION` in sdk/rust/jvm-ffi: the result codes and signatures [[Ed25519Ffi]] declares. */
  val AbiVersion: Int = 1

  sealed abstract class LoadError(message: String) extends Exception(message) with NoStackTrace

  object LoadError {
    final case class UnsupportedPlatform(os: String, arch: String) extends LoadError(s"no $LibraryName build for $os on $arch")
    final case class NotBundled(resource: String) extends LoadError(s"$resource is not on the classpath")
    final case class LinkFailed(library: String, reason: String) extends LoadError(s"$library does not load: $reason")
    final case class AbiMismatch(found: Int) extends LoadError(s"$LibraryName has ABI version $found, expected $AbiVersion")
    final case class SelfTestFailed(check: String) extends LoadError(s"$LibraryName failed its self-test: $check")
  }

  /** A platform with a build: `os` is `linux` or `darwin`, `arch` is `x86_64` or `aarch64`. */
  final case class Platform private (os: String, arch: String) {
    def fileName: String = if (os == "darwin") s"lib$LibraryName.dylib" else s"lib$LibraryName.so"

    /** Where the sbt build bundles the library, relative to this package. */
    def resource: String = s"native/$os-$arch/$fileName"

    /** `RTLD_LAZY | RTLD_LOCAL | RTLD_NODELETE`, from `<dlfcn.h>` on Linux (glibc and musl; `RTLD_LOCAL` is 0) and macOS. */
    def openFlags: Int = if (os == "darwin") 0x1 | 0x4 | 0x80 else 0x1 | 0x1000
  }

  object Platform {

    /** From the JVM's `os.name` and `os.arch`. */
    def of(osName: String, osArch: String): Either[LoadError, Platform] =
      (
        Option(osName.toLowerCase(Locale.ROOT)).collect {
          case "linux"                                        => "linux"
          case n if n.startsWith("mac os x") || n == "darwin" => "darwin"
        },
        Option(osArch).collect {
          case "amd64" | "x86_64"  => "x86_64"
          case "aarch64" | "arm64" => "aarch64"
        }
      ).mapN(Platform(_, _)).toRight(LoadError.UnsupportedPlatform(osName, osArch))

    def current[F[_]: Sync]: F[Platform] =
      Sync[F]
        .delay((Option(System.getProperty("os.name")).getOrElse(""), Option(System.getProperty("os.arch")).getOrElse("")))
        .flatMap { case (os, arch) => of(os, arch).liftTo[F] }
  }

  /** The library bundled for this platform. */
  def load[F[_]: Sync]: F[Ed25519Backend] =
    Platform.current[F].flatMap(p => extracted[F](p).use(open[F](p, _)))

  /** A library built for this platform outside the jar (`cargo build --profile jvm-ffi -p dilf4s-jvm-ffi`). */
  def loadFrom[F[_]: Sync](library: Path): F[Ed25519Backend] =
    Platform.current[F].flatMap(open[F](_, library))

  /** The bundled library, copied to a fresh directory only this user can read; both are deleted on release. */
  private def extracted[F[_]: Sync](p: Platform): Resource[F, Path] =
    for {
      found <- Resource.make(Sync[F].blocking(Option(classOf[Ed25519Ffi].getResourceAsStream(p.resource))))(
        _.traverse_(in => Sync[F].blocking(in.close()))
      )
      in   <- Resource.eval(found.liftTo[F](LoadError.NotBundled(p.resource)))
      dir  <- Resource.make(Sync[F].blocking(Files.createTempDirectory(LibraryName)))(d => Sync[F].blocking(Files.deleteIfExists(d)).void)
      file <- Resource.make(Sync[F].pure(dir.resolve(p.fileName)))(f => Sync[F].blocking(Files.deleteIfExists(f)).void)
      _    <- Resource.eval(Sync[F].blocking(Files.copy(in, file)))
    } yield file

  /**
   * Opens the library and checks it through its own function pointers (every symbol, the ABI version, the known answers)
   * before [[Ed25519Ffi]] is bound to it, so a bad library never displaces a good one; then checks the binding.
   */
  private def open[F[_]: Sync](p: Platform, library: Path): F[Ed25519Backend] =
    for {
      lib <- linked[F, NativeLibrary](library)(
        NativeLibrary.getInstance(library.toString, java.util.Map.of(Library.OPTION_OPEN_FLAGS, Integer.valueOf(p.openFlags)))
      )
      unbound <- linked[F, Calls](library)(new Unbound(lib))
      _       <- checked[F](new Backend(unbound))
      _       <- linked[F, Unit](library)(Native.register(classOf[Ed25519Ffi], lib))
      bound = new Backend(Bound)
      _ <- checked[F](bound)
    } yield bound

  /** A library that does not load or lacks a symbol is a `LinkageError`, which an effect treats as fatal: caught by type. */
  private def linked[F[_]: Sync, A](library: Path)(thunk: => A): F[A] =
    Sync[F]
      .blocking(
        Either
          .catchOnly[LinkageError](thunk)
          .leftMap(e => LoadError.LinkFailed(library.toString, Option(e.getMessage).getOrElse(e.getClass.getName)))
      )
      .rethrow

  /** The ABI version, then the known answers. */
  private def checked[F[_]: Sync](b: Backend): F[Unit] =
    Sync[F]
      .delay(b.calls.abiVersion)
      .flatMap(v => Either.cond(v == AbiVersion, (), LoadError.AbiMismatch(v)).liftTo[F])
      .flatMap(_ => Sync[F].delay(selfTest(b)).rethrow)

  /** The library's four functions. */
  sealed private trait Calls {
    def abiVersion: Int
    def checkKey(key:        Array[Byte]): Int
    def verifyZip215(key:    Array[Byte], message: Array[Byte], signature: Array[Byte]): Int
    def verifyLibsodium(key: Array[Byte], message: Array[Byte], signature: Array[Byte]): Int
  }

  /** Through the natives JNA binds [[Ed25519Ffi]] to: the fast path, once a library has passed its checks. */
  private object Bound extends Calls {
    def abiVersion: Int = Ed25519Ffi.abiVersion()
    def checkKey(key:     Array[Byte]): Int = Ed25519Ffi.checkKey(key)
    def verifyZip215(key: Array[Byte], message: Array[Byte], signature: Array[Byte]): Int = Ed25519Ffi.verifyZip215(key, message, signature)
    def verifyLibsodium(key: Array[Byte], message: Array[Byte], signature: Array[Byte]): Int =
      Ed25519Ffi.verifyLibsodium(key, message, signature)
  }

  /**
   * Through one library's own function pointers, binding nothing: JNA's slower generic calls, to check a library before
   * it is bound. Each array is passed with its own length. Looking up a missing symbol throws `UnsatisfiedLinkError`.
   */
  final private class Unbound(lib: NativeLibrary) extends Calls {
    private val abi = lib.getFunction("dilf4s_jvm_ffi_abi_version")
    private val check = lib.getFunction("dilf4s_ed25519_check_key")
    private val zip215 = lib.getFunction("dilf4s_ed25519_verify_zip215")
    private val libsodium = lib.getFunction("dilf4s_ed25519_verify_libsodium")

    private def args(arrays: Array[Byte]*): Array[AnyRef] = arrays.flatMap(a => List[AnyRef](a, Integer.valueOf(a.length))).toArray

    def abiVersion: Int = abi.invokeInt(Array.empty[AnyRef])
    def checkKey(key:     Array[Byte]): Int = check.invokeInt(args(key))
    def verifyZip215(key: Array[Byte], message: Array[Byte], signature: Array[Byte]): Int = zip215.invokeInt(args(key, message, signature))
    def verifyLibsodium(key: Array[Byte], message: Array[Byte], signature: Array[Byte]): Int =
      libsodium.invokeInt(args(key, message, signature))
  }

  /** A result code of sdk/rust/jvm-ffi's `Verdict`. */
  sealed private[signing] trait Verdict

  private[signing] object Verdict {
    case object Invalid extends Verdict
    case object Valid extends Verdict
    case object KeyLength extends Verdict
    case object KeyNotAPoint extends Verdict
    case object SignatureLength extends Verdict
    case object BadArgument extends Verdict

    def of(code: Int): Option[Verdict] =
      code match {
        case 0  => Some(Invalid)
        case 1  => Some(Valid)
        case -1 => Some(KeyLength)
        case -2 => Some(KeyNotAPoint)
        case -3 => Some(SignatureLength)
        case -4 => Some(BadArgument)
        case _  => None
      }
  }

  private val KeyWhat = "Ed25519 verifying key"
  private val SignatureWhat = "Ed25519 signature"

  /**
   * The backend over a library's functions. Errors are built as [[Ed25519.codec]] builds them; a code that contradicts
   * the input, or that the ABI does not define, is a `BackendFailure`, never a verdict. The Boolean verifiers have no
   * error channel: they accept only on `Valid`, as the reference returns `false` for a key or signature that is malformed.
   */
  final private class Backend(val calls: Calls) extends Ed25519Backend {
    val name: String = "dilf4s-signing (Rust)"

    private def call(operation: String)(code: => Int): Either[CryptoError, Option[Verdict]] =
      Either
        .catchNonFatal(Verdict.of(code))
        .leftMap(e => CryptoError.BackendFailure(s"Ed25519 $operation", Option(e.getMessage).getOrElse(e.getClass.getName)))

    private def unexpected[A](operation: String, v: Option[Verdict]): Either[CryptoError, A] =
      CryptoError.BackendFailure(s"Ed25519 $operation", s"unexpected result ${v.fold("code")(_.toString)}").asLeft

    /** The length error [[SchemeCodec.sized]] gives `bytes`; a contradiction if `bytes` has the expected length. */
    private def lengthError[A](operation: String, what: String, expected: Long, bytes: ByteVector, v: Verdict): Either[CryptoError, A] =
      SchemeCodec.sized(what, expected, bytes).flatMap(_ => unexpected(operation, Some(v)))

    val codec: SchemeCodec[Ed25519] = new SchemeCodec[Ed25519] {
      val name: String = Ed25519.codec.name
      def signingKey(bytes: ByteVector): Either[CryptoError, SigningKey[Ed25519]] = Ed25519.codec.signingKey(bytes)
      def signature(bytes:  ByteVector): Either[CryptoError, Signature[Ed25519]] = Ed25519.codec.signature(bytes)
      def verifyingKey(bytes: ByteVector): Either[CryptoError, VerifyingKey[Ed25519]] =
        SchemeCodec
          .sized(KeyWhat, Ed25519.KeyLength, bytes)
          .flatMap(b => call("check_key")(calls.checkKey(b.toArray)))
          .flatMap {
            case Some(Verdict.Valid)        => VerifyingKey[Ed25519](bytes).asRight
            case Some(Verdict.KeyNotAPoint) => SchemeCodec.notAPoint(name).asLeft
            case other                      => unexpected("check_key", other)
          }
    }

    private def boolean(code: => Int): Boolean = call("verify")(code).exists(_.contains(Verdict.Valid))

    val verifier: Verifier[Ed25519] = (key: VerifyingKey[Ed25519], message: ByteVector, signature: Signature[Ed25519]) =>
      boolean(calls.verifyZip215(key.bytes.toArray, message.toArray, signature.bytes.toArray))

    val libsodiumVerifier: Verifier[Ed25519] = (key: VerifyingKey[Ed25519], message: ByteVector, signature: Signature[Ed25519]) =>
      boolean(calls.verifyLibsodium(key.bytes.toArray, message.toArray, signature.bytes.toArray))

    def verifyBytes(key: ByteVector, message: ByteVector, signature: ByteVector): Either[CryptoError, Boolean] =
      call("verify")(calls.verifyZip215(key.toArray, message.toArray, signature.toArray)).flatMap {
        case Some(Verdict.Valid)        => true.asRight
        case Some(Verdict.Invalid)      => false.asRight
        case Some(Verdict.KeyLength)    => lengthError("verify", KeyWhat, Ed25519.KeyLength, key, Verdict.KeyLength)
        case Some(Verdict.KeyNotAPoint) => SchemeCodec.notAPoint(codec.name).asLeft
        case Some(Verdict.SignatureLength) =>
          lengthError("verify", SignatureWhat, Ed25519.SignatureLength, signature, Verdict.SignatureLength)
        case other => unexpected("verify", other)
      }
  }

  /** Known answers from shared/vectors/signing_v1.json: a library that gets one wrong is not the one this code expects. */
  private def selfTest(b: Ed25519Backend): Either[LoadError, Unit] = {
    val key = hex"03a107bff3ce10be1d70dd18e74bc09967e4d6309ba50d5f1ddc8664125531b8"
    val signature =
      hex"9ca53579530654d5c3df77089ef45eda613e2fedf670e96bedac4639504e5845ef4b95d5793077233dd16817b2532e9c5525872a73a4ad74b759369a9e05c102"
    val notAPoint = hex"0200000000000000000000000000000000000000000000000000000000000000"
    val checks: List[(String, Boolean)] = List(
      "a valid signature verifies under ZIP-215" -> (b.verifyBytes(key, ByteVector.empty, signature) == Right(true)),
      "a valid signature verifies under libsodium's rule" ->
      b.libsodiumVerifier.verify(VerifyingKey[Ed25519](key), ByteVector.empty, Signature[Ed25519](signature)),
      "another message does not verify" -> (b.verifyBytes(key, hex"00", signature) == Right(false)),
      "a key off the curve is refused"  -> (b.codec.verifyingKey(notAPoint) == Ed25519Backend.reference.codec.verifyingKey(notAPoint)),
      "a short key is refused"          -> (b.verifyBytes(key.drop(1), ByteVector.empty, signature).isLeft),
      "a short signature is refused"    -> (b.verifyBytes(key, ByteVector.empty, signature.drop(1)).isLeft)
    )
    checks.collectFirst { case (check, false) => LoadError.SelfTestFailed(check) }.toLeft(())
  }
}
