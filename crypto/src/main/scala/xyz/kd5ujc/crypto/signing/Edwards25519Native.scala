package xyz.kd5ujc.crypto.signing

import java.nio.file.{Files, Path}
import java.util.Locale

import cats.effect.{Resource, Sync}
import cats.syntax.all._

import scala.util.control.NoStackTrace

import xyz.kd5ujc.crypto.vrf._

import com.sun.jna.{Library, Native, NativeLibrary}
import scodec.bits.{ByteVector, hex}

/**
 * Verification on edwards25519, from one backend: Ed25519 (the codec's verifying-key check and the two verification
 * rules) and ECVRF (the three suites). Both backends give the same verdict on every input (the shared vectors, the
 * libsodium corpora and differential tests pin it), so the choice is one of cost only. [[Edwards25519Backend.reference]]
 * is the pure-Scala code in [[Ed25519]] and [[Vrf]]; [[Edwards25519Native]] is dilf4s-signing, the Rust that the SDK
 * and the SP1 guest run and `ed25519_verify` and `ecvrf_verify` are priced from, about five times faster.
 */
trait Edwards25519Backend {
  def name: String

  /** Signing keys and signatures are checked for length; a verifying key must also decode under ZIP-215. */
  def codec: SchemeCodec[Ed25519]

  /** ZIP-215, the consensus rule ([[Ed25519.verifier]]). */
  def verifier: Verifier[Ed25519]

  /** libsodium's rule ([[Ed25519.libsodiumVerifier]]). */
  def libsodiumVerifier: Verifier[Ed25519]

  /** The key, then the signature, decoded by [[codec]], then verified by [[verifier]]: their verdict in one step. */
  def verifyBytes(key: ByteVector, message: ByteVector, signature: ByteVector): Either[CryptoError, Boolean]

  /** ECVRF-EDWARDS25519-SHA512-TAI: verification here, signing keys and proving in pure Scala ([[Vrf.tai]]). */
  def vrfTai: Vrf[EcVrfTai]

  /** ECVRF-EDWARDS25519-SHA512-ELL2, as [[vrfTai]] ([[Vrf.ell2]]). */
  def vrfEll2: Vrf[EcVrfEll2]

  /** draft-03 ECVRF, as libsodium (Cardano), as [[vrfTai]] ([[Vrf.draft03]]). */
  def vrfDraft03: Vrf[EcVrfDraft03]
}

object Edwards25519Backend {

  /** The pure-Scala code in [[Ed25519]] and [[Vrf]]: it needs no native library. */
  val reference: Edwards25519Backend = new Edwards25519Backend {
    val name: String = "pure Scala"
    val codec: SchemeCodec[Ed25519] = Ed25519.codec
    val verifier: Verifier[Ed25519] = Ed25519.verifier
    val libsodiumVerifier: Verifier[Ed25519] = Ed25519.libsodiumVerifier
    def verifyBytes(key: ByteVector, message: ByteVector, signature: ByteVector): Either[CryptoError, Boolean] =
      (codec.verifyingKey(key), codec.signature(signature)).tupled.map { case (k, s) => verifier.verify(k, message, s) }
    val vrfTai: Vrf[EcVrfTai] = Vrf.tai
    val vrfEll2: Vrf[EcVrfEll2] = Vrf.ell2
    val vrfDraft03: Vrf[EcVrfDraft03] = Vrf.draft03
  }

  /** dilf4s-signing through its native library ([[Edwards25519Native.load]]); fails where the library is not bundled. */
  def native[F[_]: Sync]: F[Edwards25519Backend] = Edwards25519Native.load[F]
}

/**
 * dilf4s-signing's Ed25519 and ECVRF verification (`sdk/rust/jvm-ffi`), through JNA's direct mapping, which adds about
 * 0.6 µs to a call. The sbt build compiles the library with cargo and bundles it in this module's jar, one copy per
 * platform; [[load]] extracts it to a private temporary directory, opens it, and deletes the file. A missing or broken
 * library fails the load with a [[Edwards25519Native.LoadError]]: there is no silent fallback to
 * [[Edwards25519Backend.reference]], whose verdicts are the same but which costs about five times more than the
 * operations are priced at.
 *
 * Every load binds the natives again (JNI binds per class, JVM-wide), to its own copy of the library, so every backend
 * calls the copy bound last. A copy is bound only after it passes the ABI and known-answer checks through its own
 * function pointers, so a bad library never displaces a good one. Each copy is opened with `RTLD_NODELETE`, so none is
 * ever unmapped: a call already running in an earlier copy stays valid when a later load rebinds the class, and JNA
 * closing a library it no longer references unmaps nothing. The cost is one mapping (about 0.4 MB) per load, kept for
 * the life of the process, rejected copies included: load once, at the edge, as [[Edwards25519Backend.native]]'s
 * callers do. Sharing one binding across loads instead would need process-global mutable state.
 */
object Edwards25519Native {

  val LibraryName: String = "dilf4s_jvm_ffi"

  /** `ABI_VERSION` in sdk/rust/jvm-ffi: the functions, signatures and result codes [[Edwards25519Ffi]] declares. */
  val AbiVersion: Int = 2

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
  def load[F[_]: Sync]: F[Edwards25519Backend] =
    Platform.current[F].flatMap(p => extracted[F](p).use(open[F](p, _)))

  /** A library built for this platform outside the jar (`cargo build --profile jvm-ffi -p dilf4s-jvm-ffi`). */
  def loadFrom[F[_]: Sync](library: Path): F[Edwards25519Backend] =
    Platform.current[F].flatMap(open[F](_, library))

  /** The bundled library, copied to a fresh directory only this user can read; both are deleted on release. */
  private def extracted[F[_]: Sync](p: Platform): Resource[F, Path] =
    for {
      found <- Resource.make(Sync[F].blocking(Option(classOf[Edwards25519Ffi].getResourceAsStream(p.resource))))(
        _.traverse_(in => Sync[F].blocking(in.close()))
      )
      in   <- Resource.eval(found.liftTo[F](LoadError.NotBundled(p.resource)))
      dir  <- Resource.make(Sync[F].blocking(Files.createTempDirectory(LibraryName)))(d => Sync[F].blocking(Files.deleteIfExists(d)).void)
      file <- Resource.make(Sync[F].pure(dir.resolve(p.fileName)))(f => Sync[F].blocking(Files.deleteIfExists(f)).void)
      _    <- Resource.eval(Sync[F].blocking(Files.copy(in, file)))
    } yield file

  /**
   * Opens the library and checks it through its own function pointers (every symbol, the ABI version, the known answers)
   * before [[Edwards25519Ffi]] is bound to it, so a bad library never displaces a good one; then checks the binding.
   */
  private def open[F[_]: Sync](p: Platform, library: Path): F[Edwards25519Backend] =
    for {
      lib <- linked[F, NativeLibrary](library)(
        NativeLibrary.getInstance(library.toString, java.util.Map.of(Library.OPTION_OPEN_FLAGS, Integer.valueOf(p.openFlags)))
      )
      unbound <- linked[F, Calls](library)(new Unbound(lib))
      _       <- checked[F](new Backend(unbound))
      _       <- linked[F, Unit](library)(Native.register(classOf[Edwards25519Ffi], lib))
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

  /** The library's functions: bound, unbound, or (in tests) a stub that answers with any code. */
  private[signing] trait Calls {
    def abiVersion: Int
    def checkKey(key:        Array[Byte]): Int
    def verifyZip215(key:    Array[Byte], message: Array[Byte], signature: Array[Byte]): Int
    def verifyLibsodium(key: Array[Byte], message: Array[Byte], signature: Array[Byte]): Int
    def vrfCheckKey(suite:   Int, key:             Array[Byte]): Int
    def vrfVerify(suite:     Int, key:             Array[Byte], alpha:     Array[Byte], proof: Array[Byte], beta: Array[Byte]): Int
  }

  /** Through the natives JNA binds [[Edwards25519Ffi]] to: the fast path, once a library has passed its checks. */
  private object Bound extends Calls {
    def abiVersion: Int = Edwards25519Ffi.abiVersion()
    def checkKey(key: Array[Byte]): Int = Edwards25519Ffi.checkKey(key)
    def verifyZip215(key: Array[Byte], message: Array[Byte], signature: Array[Byte]): Int =
      Edwards25519Ffi.verifyZip215(key, message, signature)
    def verifyLibsodium(key: Array[Byte], message: Array[Byte], signature: Array[Byte]): Int =
      Edwards25519Ffi.verifyLibsodium(key, message, signature)
    def vrfCheckKey(suite: Int, key: Array[Byte]): Int = Edwards25519Ffi.vrfCheckKey(suite, key)
    def vrfVerify(suite: Int, key: Array[Byte], alpha: Array[Byte], proof: Array[Byte], beta: Array[Byte]): Int =
      Edwards25519Ffi.vrfVerify(suite, key, alpha, proof, beta)
  }

  /**
   * Through one library's own function pointers, binding nothing: JNA's slower generic calls, to check a library before
   * it is bound. Each array is passed with its own length, and JNA copies `beta` back. Looking up a missing symbol
   * throws `UnsatisfiedLinkError`.
   */
  final private class Unbound(lib: NativeLibrary) extends Calls {
    private val abi = lib.getFunction("dilf4s_jvm_ffi_abi_version")
    private val check = lib.getFunction("dilf4s_ed25519_check_key")
    private val zip215 = lib.getFunction("dilf4s_ed25519_verify_zip215")
    private val libsodium = lib.getFunction("dilf4s_ed25519_verify_libsodium")
    private val vrfCheck = lib.getFunction("dilf4s_vrf_check_key")
    private val vrf = lib.getFunction("dilf4s_vrf_verify")

    private def args(arrays: Array[Byte]*): Array[AnyRef] = arrays.flatMap(a => List[AnyRef](a, Integer.valueOf(a.length))).toArray

    def abiVersion: Int = abi.invokeInt(Array.empty[AnyRef])
    def checkKey(key: Array[Byte]): Int = check.invokeInt(args(key))
    def verifyZip215(key: Array[Byte], message: Array[Byte], signature: Array[Byte]): Int =
      zip215.invokeInt(args(key, message, signature))
    def verifyLibsodium(key: Array[Byte], message: Array[Byte], signature: Array[Byte]): Int =
      libsodium.invokeInt(args(key, message, signature))
    def vrfCheckKey(suite: Int, key: Array[Byte]): Int = vrfCheck.invokeInt(Integer.valueOf(suite) +: args(key))
    def vrfVerify(suite: Int, key: Array[Byte], alpha: Array[Byte], proof: Array[Byte], beta: Array[Byte]): Int =
      vrf.invokeInt(Integer.valueOf(suite) +: args(key, alpha, proof, beta))
  }

  /** A result code of sdk/rust/jvm-ffi's `Verdict` (Ed25519). */
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

  /**
   * A result code of sdk/rust/jvm-ffi's `VrfVerdict`: `Valid`, a length, or a verification rejection. `KeyMismatch` is a
   * signing-key error that verification never returns, so it is kept out of [[VrfVerdict.Rejected]]: a backend that
   * answers with it fails rather than rejects.
   */
  sealed private[signing] trait VrfVerdict

  private[signing] object VrfVerdict {
    case object Valid extends VrfVerdict
    case object ProofLength extends VrfVerdict
    case object KeyLength extends VrfVerdict
    final case class Rejected(error: VrfError) extends VrfVerdict
    case object HashToCurveFailed extends VrfVerdict
    case object KeyMismatch extends VrfVerdict
    case object BadArgument extends VrfVerdict

    def of(code: Int): Option[VrfVerdict] =
      code match {
        case 1   => Some(Valid)
        case -10 => Some(ProofLength)
        case -11 => Some(KeyLength)
        case -12 => Some(Rejected(VrfError.InvalidPublicKey))
        case -13 => Some(Rejected(VrfError.SmallOrderPublicKey))
        case -14 => Some(Rejected(VrfError.InvalidGamma))
        case -15 => Some(Rejected(VrfError.NonCanonicalScalar))
        case -16 => Some(Rejected(VrfError.ChallengeMismatch))
        case -17 => Some(HashToCurveFailed)
        case -18 => Some(KeyMismatch)
        case -4  => Some(BadArgument)
        case _   => None
      }
  }

  private val KeyWhat = "Ed25519 verifying key"
  private val SignatureWhat = "Ed25519 signature"

  private def reason(e: Throwable): String = Option(e.getMessage).getOrElse(e.getClass.getName)

  /**
   * The backend over a library's functions. Errors are built as the pure-Scala code builds them; a code that contradicts
   * the input, or that the ABI does not define, is a `BackendFailure`, never a verdict. The Boolean Ed25519 verifiers
   * have no error channel: they accept only on `Valid`, as the reference returns `false` for a malformed key or
   * signature.
   */
  final private[signing] class Backend(val calls: Calls) extends Edwards25519Backend {
    val name: String = "dilf4s-signing (Rust)"

    private def call(operation: String)(code: => Int): Either[CryptoError, Option[Verdict]] =
      Either.catchNonFatal(Verdict.of(code)).leftMap(e => CryptoError.BackendFailure(s"Ed25519 $operation", reason(e)))

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

    val vrfTai: Vrf[EcVrfTai] = new NativeVrf[EcVrfTai](Vrf.tai, 0, Vrf.TaiNoPoint, calls)
    val vrfEll2: Vrf[EcVrfEll2] = new NativeVrf[EcVrfEll2](Vrf.ell2, 1, Vrf.Elligator2NoPoint, calls)
    val vrfDraft03: Vrf[EcVrfDraft03] = new NativeVrf[EcVrfDraft03](Vrf.draft03, 2, Vrf.Elligator2NoPoint, calls)
  }

  /**
   * One ECVRF suite (`suite` is its code in the C ABI): keys are checked and proofs verified natively; signing keys,
   * proof decoding and proving are `pure`'s. A length error is the one `pure` gives, so its message is identical.
   */
  final private[signing] class NativeVrf[S](pure: Vrf[S], suite: Int, noPoint: VrfError, calls: Calls) extends Vrf[S] {
    val name: String = pure.name
    def signingKey(bytes: ByteVector): Either[VrfError, SigningKey[S]] = pure.signingKey(bytes)
    def proof(bytes:      ByteVector): Either[VrfError, VrfProof[S]] = pure.proof(bytes)
    def publicKey(key:    SigningKey[S]): VerifyingKey[S] = pure.publicKey(key)
    def prove(key:        SigningKey[S], alpha: ByteVector): Either[VrfError, (VrfProof[S], VrfOutput[S])] = pure.prove(key, alpha)

    private def call(operation: String)(code: => Int): Either[VrfError, Option[VrfVerdict]] =
      Either.catchNonFatal(VrfVerdict.of(code)).leftMap(e => VrfError.BackendFailure(s"$name $operation", reason(e)))

    private def unexpected[A](operation: String, v: Option[VrfVerdict]): Either[VrfError, A] =
      VrfError.BackendFailure(s"$name $operation", s"unexpected result ${v.fold("code")(_.toString)}").asLeft

    /** `pure`'s `Malformed` for a wrong length; a contradiction if `pure` finds none. */
    private def malformed[A](operation: String, v: VrfVerdict, pureResult: Either[VrfError, ?]): Either[VrfError, A] =
      pureResult match {
        case Left(e: VrfError.Malformed) => e.asLeft
        case _                           => unexpected(operation, Some(v))
      }

    private def rejected[A](operation: String, v: Option[VrfVerdict]): Either[VrfError, A] =
      v match {
        case Some(VrfVerdict.Rejected(e))       => e.asLeft
        case Some(VrfVerdict.HashToCurveFailed) => noPoint.asLeft
        case other                              => unexpected(operation, other)
      }

    def verifyingKey(bytes: ByteVector): Either[VrfError, VerifyingKey[S]] =
      call("check_key")(calls.vrfCheckKey(suite, bytes.toArray)).flatMap {
        case Some(VrfVerdict.Valid)     => VerifyingKey[S](bytes).asRight
        case Some(VrfVerdict.KeyLength) => malformed("check_key", VrfVerdict.KeyLength, pure.verifyingKey(bytes))
        case Some(VrfVerdict.Rejected(e @ (VrfError.InvalidPublicKey | VrfError.SmallOrderPublicKey))) => e.asLeft
        case other                                                                                     => unexpected("check_key", other)
      }

    def verify(key: VerifyingKey[S], alpha: ByteVector, proof: VrfProof[S]): Either[VrfError, VrfOutput[S]] =
      verifyBytes(key.bytes, alpha, proof.bytes)

    def verifyBytes(key: ByteVector, alpha: ByteVector, proof: ByteVector): Either[VrfError, VrfOutput[S]] = {
      val beta = new Array[Byte](Vrf.OutputLength.toInt)
      call("verify")(calls.vrfVerify(suite, key.toArray, alpha.toArray, proof.toArray, beta)).flatMap {
        case Some(VrfVerdict.Valid)       => VrfOutput[S](ByteVector.view(beta)).asRight
        case Some(VrfVerdict.ProofLength) => malformed("verify", VrfVerdict.ProofLength, pure.proof(proof))
        case Some(VrfVerdict.KeyLength)   => malformed("verify", VrfVerdict.KeyLength, pure.proof(proof) *> pure.verifyingKey(key))
        case other                        => rejected("verify", other)
      }
    }
  }

  /** Known answers from shared/vectors/signing_v1.json and vrf_v1.json: a library that gets one wrong is not this one. */
  private def selfTest(b: Edwards25519Backend): Either[LoadError, Unit] = {
    val key = hex"03a107bff3ce10be1d70dd18e74bc09967e4d6309ba50d5f1ddc8664125531b8"
    val signature =
      hex"9ca53579530654d5c3df77089ef45eda613e2fedf670e96bedac4639504e5845ef4b95d5793077233dd16817b2532e9c5525872a73a4ad74b759369a9e05c102"
    val notAPoint = hex"0200000000000000000000000000000000000000000000000000000000000000"
    def vrf[S](v: Vrf[S], pi: ByteVector, beta: ByteVector): List[(String, Boolean)] =
      List(
        s"${v.name}: a proof verifies to its output"        -> (v.verifyBytes(key, ByteVector.empty, pi).map(_.bytes) == Right(beta)),
        s"${v.name}: another alpha is a challenge mismatch" -> (v.verifyBytes(key, hex"00", pi) == Left(VrfError.ChallengeMismatch)),
        s"${v.name}: a short proof is malformed" -> v.verifyBytes(key, ByteVector.empty, pi.drop(1)).left.exists {
          case _: VrfError.Malformed => true
          case _                     => false
        }
      )
    val checks: List[(String, Boolean)] = List(
      "a valid signature verifies under ZIP-215" -> (b.verifyBytes(key, ByteVector.empty, signature) == Right(true)),
      "a valid signature verifies under libsodium's rule" ->
      b.libsodiumVerifier.verify(VerifyingKey[Ed25519](key), ByteVector.empty, Signature[Ed25519](signature)),
      "another message does not verify" -> (b.verifyBytes(key, hex"00", signature) == Right(false)),
      "a key off the curve is refused"  -> (b.codec.verifyingKey(notAPoint) == Edwards25519Backend.reference.codec.verifyingKey(notAPoint)),
      "a short key is refused"          -> (b.verifyBytes(key.drop(1), ByteVector.empty, signature).isLeft),
      "a short signature is refused"    -> (b.verifyBytes(key, ByteVector.empty, signature.drop(1)).isLeft)
    ) ++ vrf(
      b.vrfTai,
      hex"550732dbbedcd46f4b4d96fba5c166d2cf4d0ce406a91a93d8412c5651ec0f1497ee218ad3a3c6ed40f6f429ab045d9003af0cbddd6ebc5c3e566c251fd79ddf1bff32e58ba6737f935df3b7b7be4302",
      hex"2c2e32764b3ff7bc2b08327ea863e9cde62d9f5f4e6d13a64c4ed65de9958abbfbf2371a0dee02d534dfeb604162cce68262f4521f225b202ef9a81bad05a624"
    ) ++ vrf(
      b.vrfEll2,
      hex"a91cd7eddc6aafe7fb2a7669d9a48ebf0c7804b25233ed55d630456d28fdbf42e3af87345af9123bb02eea8136a4d103619fadc367bb259fea2ce9b4d70de55317ca9934ed055044ac4c3bae7670d90f",
      hex"b6613f6d2cb9168869f1a67a894ca154d63a4504b62166be05feb84046b9d591115f2ce81046768fcbec4ea90114fef3b6bdf915579ce9c5fb00417bcde35e9a"
    ) ++ vrf(
      b.vrfDraft03,
      hex"9609e371832d51ed5a88cf74c03464e1eaaa1d7d69854e4f144d3d722b405c8ab46740ba09cde5a993e1cc9363fd377936d361b1c78e4c7ac56d6c79f6b7577dbaa197d715a51ba5f70b9b98ccc8410b",
      hex"2fa118653cd52bc75983f5276bf58eecd2f42e40ee0556749542993e710a53448928253486708a69b8a3dcd479e829a2d07191571d3b934218c347081a5b2959"
    )
    checks.collectFirst { case (check, false) => LoadError.SelfTestFailed(check) }.toLeft(())
  }
}
