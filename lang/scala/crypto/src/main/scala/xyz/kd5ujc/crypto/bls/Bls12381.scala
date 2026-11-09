package xyz.kd5ujc.crypto.bls

import cats.effect.Sync
import cats.syntax.all._

import xyz.kd5ujc.crypto.signing._

import scodec.bits.ByteVector
import supranational.blst.{BLST_ERROR, P1, P1_Affine, P2, P2_Affine, PT, Pairing, SecretKey}

/**
 * BLS signatures over BLS12-381, Ethereum consensus profile: minimal-pubkey-size (keys in G1, 48 bytes; signatures
 * in G2, 96 bytes), proof-of-possession ciphersuite `BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_`. Byte-compatible
 * with metakit, tessellation-bls and the Eth2 test suite.
 */
sealed trait BlsPop

/**
 * blst (supranational) through Teku's `jblst` JNI build: constant-time for secret-key operations, with portable
 * natives. Loading the native library is an effect ([[Bls12381.load]]); afterwards every operation is a pure function
 * whose native objects are created per call and never escape. Every native call is captured, so a library failure
 * becomes a typed error or a rejection, never a throw.
 */
final class Bls12381 private () {
  import Bls12381._

  private def call[A](operation: String)(thunk: => A): Either[CryptoError, A] =
    Either
      .catchNonFatal(thunk)
      .leftMap(e => CryptoError.BackendFailure(s"blst $operation", Option(e.getMessage).getOrElse(e.getClass.getName)))

  private def secret(key: SigningKey[BlsPop]): Either[CryptoError, SecretKey] =
    call("from_bendian") { val sk = new SecretKey(); sk.from_bendian(key.bytes.toArray); sk }

  /** KeyValidate: a compressed G1 point in the subgroup, not the identity. */
  private def g1(bytes: ByteVector): Either[CryptoError, P1_Affine] =
    if (bytes.size != PublicKeyLength)
      CryptoError.InvalidEncoding("BLS public key", s"expected $PublicKeyLength bytes, got ${bytes.size}").asLeft
    else
      call("P1_Affine")(new P1_Affine(bytes.toArray))
        .leftMap(_ => CryptoError.InvalidKey("BLS public key does not decode"))
        .flatMap(p => Either.cond(!p.is_inf() && p.in_group(), p, CryptoError.InvalidKey("BLS public key is the identity or outside G1")))

  /** A compressed G2 point in the subgroup (the identity decodes, but never verifies). */
  private def g2(bytes: ByteVector): Either[CryptoError, P2_Affine] =
    if (bytes.size != SignatureLength)
      CryptoError.InvalidEncoding("BLS signature", s"expected $SignatureLength bytes, got ${bytes.size}").asLeft
    else
      call("P2_Affine")(new P2_Affine(bytes.toArray))
        .leftMap(_ => CryptoError.InvalidEncoding("BLS signature", "does not decode"))
        .flatMap(p => Either.cond(p.in_group(), p, CryptoError.InvalidEncoding("BLS signature", "outside G2")))

  implicit val codec: SchemeCodec[BlsPop] = new SchemeCodec[BlsPop] {
    val name: String = "BLS12-381-PoP"
    def signingKey(bytes: ByteVector): Either[CryptoError, SigningKey[BlsPop]] =
      SchemeCodec
        .sized("BLS signing key", 32, bytes)
        .flatMap(b =>
          Either.cond(
            BigInt(1, b.toArray) > 0 && BigInt(1, b.toArray) < R,
            new SigningKey[BlsPop](b),
            CryptoError.InvalidKey("BLS secret key must be a scalar in [1, r)")
          )
        )
    def verifyingKey(bytes: ByteVector): Either[CryptoError, VerifyingKey[BlsPop]] = g1(bytes).as(VerifyingKey[BlsPop](bytes))
    def signature(bytes:    ByteVector): Either[CryptoError, Signature[BlsPop]] = g2(bytes).as(Signature[BlsPop](bytes))
  }

  /** The public key of a secret key; a native failure is a typed error, never a substitute key. */
  def publicKey(key: SigningKey[BlsPop]): Either[CryptoError, VerifyingKey[BlsPop]] =
    secret(key).flatMap(sk => call("P1")(VerifyingKey[BlsPop](ByteVector.view(new P1(sk).compress()))))

  private def signWith(key: SigningKey[BlsPop], message: ByteVector, dst: String): Either[CryptoError, Signature[BlsPop]] =
    secret(key).flatMap(sk =>
      call("sign")(Signature[BlsPop](ByteVector.view(new P2().hash_to(message.toArray, dst).sign_with(sk).compress())))
    )

  def sign(key: SigningKey[BlsPop], message: ByteVector): Either[CryptoError, Signature[BlsPop]] = signWith(key, message, SignatureDst)

  private def verifyWith(key: ByteVector, message: ByteVector, signature: ByteVector, dst: String): Boolean =
    (for {
      pk  <- g1(key)
      sig <- g2(signature)
      ok  <- call("core_verify")(sig.core_verify(pk, true, message.toArray, dst) == BLST_ERROR.BLST_SUCCESS)
    } yield ok).getOrElse(false)

  implicit val verifier: Verifier[BlsPop] = (key: VerifyingKey[BlsPop], message: ByteVector, signature: Signature[BlsPop]) =>
    verifyWith(key.bytes, message, signature.bytes, SignatureDst)

  /** The sum of one or more signatures. */
  def aggregate(signatures: List[Signature[BlsPop]]): Either[CryptoError, Signature[BlsPop]] =
    signatures match {
      case Nil => CryptoError.InvalidEncoding("BLS aggregate", "at least one signature is required").asLeft
      case first :: rest =>
        for {
          head <- g2(first.bytes)
          tail <- rest.traverse(s => g2(s.bytes))
          sum <- call("aggregate")(tail.foldLeft(head.to_jacobian()) { (acc, p) =>
            acc.aggregate(p); acc
          })
        } yield Signature[BlsPop](ByteVector.view(sum.compress()))
    }

  /** One message signed by every key (keys must carry proofs of possession). */
  def fastAggregateVerify(keys: List[VerifyingKey[BlsPop]], message: ByteVector, signature: Signature[BlsPop]): Boolean =
    (keys match {
      case Nil => false.asRight[CryptoError]
      case first :: rest =>
        for {
          head <- g1(first.bytes)
          tail <- rest.traverse(k => g1(k.bytes))
          sum <- call("aggregate")(tail.foldLeft(head.to_jacobian()) { (acc, p) =>
            acc.aggregate(p); acc
          })
          sig <- g2(signature.bytes)
          ok  <- call("core_verify")(sig.core_verify(sum.to_affine(), true, message.toArray, SignatureDst) == BLST_ERROR.BLST_SUCCESS)
        } yield ok
    }).getOrElse(false)

  /** Distinct (key, message) pairs under one aggregate signature: `prod e(pk_i, H(m_i)) == e(g1, sig)`. */
  def aggregateVerify(pairs: List[(VerifyingKey[BlsPop], ByteVector)], signature: Signature[BlsPop]): Boolean =
    pairs.nonEmpty && (for {
      keys <- pairs.traverse { case (k, _) => g1(k.bytes) }
      sig  <- g2(signature.bytes)
      ok <- call("pairing") {
        val pairing = new Pairing(true, SignatureDst)
        keys.zip(pairs.map(_._2)).foreach {
          case (pk, m) => pairing.raw_aggregate(new P2().hash_to(m.toArray, SignatureDst).to_affine(), pk)
        }
        pairing.commit()
        !sig.is_inf() && pairing.finalverify(new PT(sig, P1_Affine.generator()))
      }
    } yield ok).getOrElse(false)

  /** Every (key, message, signature) triple verifies. */
  def batchVerify(triples: List[(VerifyingKey[BlsPop], ByteVector, Signature[BlsPop])]): Boolean =
    triples.nonEmpty && triples.forall { case (k, m, s) => verifier.verify(k, m, s) }

  /** Proof of possession: a signature over the public key under the PoP tag. */
  def popProve(key: SigningKey[BlsPop]): Either[CryptoError, Signature[BlsPop]] =
    publicKey(key).flatMap(pk => signWith(key, pk.bytes, PopDst))

  def popVerify(key: VerifyingKey[BlsPop], proof: Signature[BlsPop]): Boolean = verifyWith(key.bytes, key.bytes, proof.bytes, PopDst)

  /**
   * `hash_to_curve` into G2, uncompressed `x.c1 || x.c0 || y.c1 || y.c0` (192 bytes). The DST crosses JNI as a Java
   * string (modified UTF-8), so it must be 1..255 printable ASCII characters to hash the bytes other implementations do.
   */
  def hashToG2(message: ByteVector, dst: String): Either[CryptoError, ByteVector] =
    Either
      .cond(
        dst.nonEmpty && dst.length <= 255 && dst.forall(ch => ch >= 0x20 && ch < 0x7f),
        dst,
        CryptoError.InvalidEncoding("hash-to-curve DST", "must be 1..255 printable ASCII characters")
      )
      .flatMap(d => call("hash_to")(ByteVector.view(new P2().hash_to(message.toArray, d).serialize())))

  /** EIP-2333 master and child keys (the Ethereum BLS key tree). */
  def eip2333Master(seed: ByteVector): Either[CryptoError, SigningKey[BlsPop]] =
    Either.cond(seed.size >= 32L, seed, CryptoError.InvalidKey("EIP-2333 seeds are at least 32 bytes")).flatMap { s =>
      call("derive_master_eip2333") { val sk = new SecretKey(); sk.derive_master_eip2333(s.toArray); ByteVector.view(sk.to_bendian()) }
        .flatMap(codec.signingKey)
    }

  def eip2333Child(parent: SigningKey[BlsPop], index: Long): Either[CryptoError, SigningKey[BlsPop]] =
    Either.cond(index >= 0 && index < (1L << 32), index, CryptoError.InvalidKey("EIP-2333 indices are 32-bit")).flatMap { i =>
      secret(parent)
        .flatMap(p =>
          call("derive_child_eip2333") { val sk = new SecretKey(); sk.derive_child_eip2333(p, i); ByteVector.view(sk.to_bendian()) }
        )
        .flatMap(codec.signingKey)
    }

  /** Key pairs from a 32-byte (or longer) seed through EIP-2333's master derivation. */
  implicit val seedDerivation: SeedDerivation[BlsPop] = new SeedDerivation[BlsPop] {
    val seedLength: Long = 32
    def fromSeed(seed: ByteVector): Either[CryptoError, KeyPair[BlsPop]] =
      eip2333Master(seed).flatMap(sk => publicKey(sk).map(KeyPair(sk, _)))
  }
}

object Bls12381 {

  val PublicKeyLength: Long = 48
  val SignatureLength: Long = 96
  val SignatureDst: String = "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_"
  val PopDst: String = "BLS_POP_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_"
  val R: BigInt = BigInt("73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001", 16)

  /** Loads the native library (an effect, and it can fail on an unsupported platform) and checks it works. */
  def load[F[_]: Sync]: F[Bls12381] = Sync[F].delay(P1_Affine.generator().in_group()).flatMap { ok =>
    Sync[F].raiseUnless(ok)(new IllegalStateException("blst self-check failed")).as(new Bls12381())
  }
}
