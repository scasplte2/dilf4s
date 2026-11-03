package xyz.kd5ujc.crypto.signing

import xyz.kd5ujc.crypto.vrf.{EcVrfTai, Vrf, VrfError}

import scodec.bits.ByteVector
import weaver.{Expectations, SimpleIOSuite}

/**
 * The native backend's reading of result codes, over a stub library that answers every call with one code: a code the
 * ABI does not define, one the operation cannot return, or one that contradicts the input is a `BackendFailure`, never a
 * verdict. Well-formed answers map to the errors the pure-Scala code gives.
 */
object NativeResultCodesSuite extends SimpleIOSuite {

  /** A library whose every function answers `code` (or throws), and writes no output. */
  private def stub(code: => Int): Edwards25519Native.Calls = new Edwards25519Native.Calls {
    def abiVersion: Int = code
    def checkKey(key:        Array[Byte]): Int = code
    def verifyZip215(key:    Array[Byte], message: Array[Byte], signature: Array[Byte]): Int = code
    def verifyLibsodium(key: Array[Byte], message: Array[Byte], signature: Array[Byte]): Int = code
    def vrfCheckKey(suite:   Int, key:             Array[Byte]): Int = code
    def vrfVerify(suite:     Int, key:             Array[Byte], alpha:     Array[Byte], proof: Array[Byte], beta: Array[Byte]): Int = code
  }

  private def vrf(code: => Int): Vrf[EcVrfTai] = new Edwards25519Native.NativeVrf[EcVrfTai](Vrf.tai, 0, Vrf.TaiNoPoint, stub(code))
  private def ed(code:  => Int): Edwards25519Backend = new Edwards25519Native.Backend(stub(code))

  private val key = ByteVector.fill(32)(1)
  private val proof = ByteVector.fill(80)(2)
  private val signature = ByteVector.fill(64)(3)
  private val alpha = ByteVector.empty

  private def backendFailure(r: Either[?, ?], what: String): Expectations =
    expect(
      r match {
        case Left(_: VrfError.BackendFailure | _: CryptoError.BackendFailure) => true
        case _                                                                => false
      },
      s"$what: expected a BackendFailure, got $r"
    )

  pureTest("ECVRF verification: codes outside verification's contract are backend failures") {
    List(
      -18 -> "KeyMismatch, which only a signing key can have",
      -4  -> "BadArgument, which a binding that passes arrays never earns",
      0   -> "an Ed25519 code",
      99  -> "an undefined code",
      -11 -> "KeyLength for a 32-byte key",
      -10 -> "ProofLength for an 80-byte proof"
    ).foldLeft(success) { case (acc, (code, what)) => acc.and(backendFailure(vrf(code).verifyBytes(key, alpha, proof), what)) }
      .and(backendFailure(vrf(throw new IllegalStateException("boom")).verifyBytes(key, alpha, proof), "a throwing call"))
  }

  pureTest("ECVRF verification: well-formed codes are the pure-Scala errors") {
    List(
      -12 -> VrfError.InvalidPublicKey,
      -13 -> VrfError.SmallOrderPublicKey,
      -14 -> VrfError.InvalidGamma,
      -15 -> VrfError.NonCanonicalScalar,
      -16 -> VrfError.ChallengeMismatch,
      -17 -> Vrf.TaiNoPoint
    ).foldLeft(success) { case (acc, (code, error)) => acc.and(expect.same(Left(error), vrf(code).verifyBytes(key, alpha, proof))) }
      .and(expect.same(Vrf.tai.proof(proof.drop(1)), vrf(-10).verifyBytes(key, alpha, proof.drop(1))))
      .and(expect.same(Vrf.tai.verifyingKey(key.drop(1)), vrf(-11).verifyBytes(key.drop(1), alpha, proof)))
  }

  pureTest("ECVRF key check: only the key's own rejections are verdicts") {
    expect
      .same(Left(VrfError.InvalidPublicKey), vrf(-12).verifyingKey(key))
      .and(expect.same(Left(VrfError.SmallOrderPublicKey), vrf(-13).verifyingKey(key)))
      .and(
        List(-14, -15, -16, -17, -18, -10, 0, 99).foldLeft(success)((acc, c) =>
          acc.and(backendFailure(vrf(c).verifyingKey(key), s"code $c"))
        )
      )
  }

  pureTest("Ed25519: codes outside the contract are backend failures; the Boolean verifiers accept only Valid") {
    val (vk, sig) = (VerifyingKey[Ed25519](key), Signature[Ed25519](signature))
    List(99, -4, -10, -1, -3)
      .foldLeft(success)((acc, c) => acc.and(backendFailure(ed(c).verifyBytes(key, alpha, signature), s"code $c")))
      .and(List(0, 99, -4, -1).foldLeft(success)((acc, c) => acc.and(backendFailure(ed(c).codec.verifyingKey(key), s"key check code $c"))))
      .and(
        expect(
          List(0, -1, -2, -3, -4, 99).forall(c => !ed(c).verifier.verify(vk, alpha, sig) && !ed(c).libsodiumVerifier.verify(vk, alpha, sig))
        )
      )
      .and(expect(ed(1).verifier.verify(vk, alpha, sig)))
      .and(backendFailure(ed(throw new IllegalStateException("boom")).verifyBytes(key, alpha, signature), "a throwing call"))
  }
}
