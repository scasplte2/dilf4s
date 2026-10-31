package xyz.kd5ujc.crypto.zk

import cats.effect.Sync
import cats.syntax.all._

import xyz.kd5ujc.crypto.zk.Bn254.ZkError

import org.hyperledger.besu.nativelib.gnark.LibGnarkEIP196
import scodec.bits.ByteVector

/**
 * BN254 group arithmetic as the EVM precompiles define it (EIP-196, EIP-197), on exact-width bytes: G1 points are 64
 * bytes `x || y` with `(0, 0)` the identity, G2 points 128 bytes in EIP-197 order, scalars 32 bytes. An engine validates
 * every point it reads (canonical coordinates, on the curve, G2 in the order-r subgroup): malformed or invalid input is a
 * [[ZkError]], never an exception.
 */
trait Bn254Engine {

  /** `a + b`. */
  def add(a: ByteVector, b: ByteVector): Either[ZkError, ByteVector]

  /** `k · p`, for any 256-bit `k` (G1 has prime order r, so this is `(k mod r) · p`). */
  def mul(p: ByteVector, k: ByteVector): Either[ZkError, ByteVector]

  /** Whether the product of `e(g1, g2)` over the pairs is one; no pairs is true. */
  def pairing(pairs: Vector[(ByteVector, ByteVector)]): Either[ZkError, Boolean]
}

object Bn254Engine {

  private def width(b: ByteVector, n: Long, role: String): Either[ZkError, ByteVector] =
    Either.cond(b.size == n, b, ZkError.Malformed(s"$role must be $n bytes, got ${b.size}"))

  /** The pure-Scala implementation ([[Bn254]]): the reference the native engine is checked against. Slow. */
  val reference: Bn254Engine = new Bn254Engine {
    def add(a: ByteVector, b: ByteVector): Either[ZkError, ByteVector] =
      (Bn254.decodeG1(a), Bn254.decodeG1(b)).mapN((p, q) => Bn254.encodeG1(Bn254.addG1(p, q)))

    def mul(p: ByteVector, k: ByteVector): Either[ZkError, ByteVector] =
      (Bn254.decodeG1(p), width(k, 32, "a scalar")).mapN((q, s) => Bn254.encodeG1(Bn254.mulG1(q, BigInt(1, s.toArray).mod(Bn254.R))))

    def pairing(pairs: Vector[(ByteVector, ByteVector)]): Either[ZkError, Boolean] =
      pairs.traverse { case (g1, g2) => (Bn254.decodeG1(g1), Bn254.decodeG2(g2)).tupled }.map(Bn254.pairingProductIsOne)
  }

  /**
   * gnark-crypto (Consensys), through Besu's JNI build of the EIP-196/197 precompiles. Loading extracts and links the
   * native library, an effect at the edge; it fails on a platform the build does not cover (Linux and macOS, x86-64 and
   * aarch64). Widths are checked here: the precompiles would zero-pad short input.
   */
  def gnark[F[_]: Sync]: F[Bn254Engine] =
    Sync[F].delay(LibGnarkEIP196.ENABLED).flatMap { enabled =>
      if (enabled) Sync[F].pure(Gnark)
      else Sync[F].raiseError(new UnsupportedOperationException("gnark (Besu native) is not available on this platform"))
    }

  private object Gnark extends Bn254Engine {

    /** The precompile's error codes (LibGnarkEIP196). */
    private def failure(code: Int): ZkError = code match {
      case LibGnarkEIP196.EIP196_ERR_CODE_POINT_NOT_IN_FIELD          => ZkError.Malformed("a coordinate is not a canonical field element")
      case LibGnarkEIP196.EIP196_ERR_CODE_POINT_ON_CURVE_CHECK_FAILED => ZkError.InvalidPoint("a point is not on the curve")
      case LibGnarkEIP196.EIP196_ERR_CODE_POINT_IN_SUBGROUP_CHECK_FAILED => ZkError.InvalidPoint("G2 point is not in the order-r subgroup")
      case LibGnarkEIP196.EIP196_ERR_CODE_INVALID_INPUT_PAIRING_LENGTH   => ZkError.Malformed("pairing input is not whole pairs")
      case other                                                         => ZkError.Malformed(s"gnark failed with code $other")
    }

    /** One call into the native library, with a fresh output buffer that never escapes. */
    private def run(operation: Byte, input: ByteVector, outputSize: Int): Either[ZkError, ByteVector] = {
      val out = new Array[Byte](outputSize)
      Either
        .catchNonFatal(LibGnarkEIP196.eip196_perform_operation(operation, input.toArray, input.size.toInt, out))
        .leftMap(e => ZkError.Malformed(s"gnark: ${e.getMessage}"))
        .flatMap(code => Either.cond(code == LibGnarkEIP196.EIP196_ERR_CODE_SUCCESS, ByteVector.view(out), failure(code)))
    }

    def add(a: ByteVector, b: ByteVector): Either[ZkError, ByteVector] =
      (width(a, 64, "a G1 point"), width(b, 64, "a G1 point")).tupled.flatMap {
        case (x, y) => run(LibGnarkEIP196.EIP196_ADD_OPERATION_RAW_VALUE, x ++ y, LibGnarkEIP196.EIP196_PREALLOCATE_FOR_RESULT_BYTES)
      }

    def mul(p: ByteVector, k: ByteVector): Either[ZkError, ByteVector] =
      (width(p, 64, "a G1 point"), width(k, 32, "a scalar")).tupled.flatMap {
        case (x, s) => run(LibGnarkEIP196.EIP196_MUL_OPERATION_RAW_VALUE, x ++ s, LibGnarkEIP196.EIP196_PREALLOCATE_FOR_RESULT_BYTES)
      }

    def pairing(pairs: Vector[(ByteVector, ByteVector)]): Either[ZkError, Boolean] =
      if (pairs.isEmpty) true.asRight
      else
        pairs.traverse { case (g1, g2) => (width(g1, 64, "a G1 point"), width(g2, 128, "a G2 point")).mapN(_ ++ _) }
          .flatMap(ps =>
            run(
              LibGnarkEIP196.EIP196_PAIR_OPERATION_RAW_VALUE,
              ps.foldLeft(ByteVector.empty)(_ ++ _),
              LibGnarkEIP196.EIP196_PAIR_PREALLOCATE_FOR_RESULT_BYTES
            )
          )
          .map(out => out == ByteVector.low(31) :+ 1.toByte)
  }
}
