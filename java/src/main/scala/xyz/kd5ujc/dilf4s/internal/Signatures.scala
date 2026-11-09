package xyz.kd5ujc.dilf4s.internal

import cats.syntax.all._

import scala.jdk.CollectionConverters._

import xyz.kd5ujc.core.canonical.CanonicalValue
import xyz.kd5ujc.core.hash.Hasher
import xyz.kd5ujc.crypto.bls.{Bls12381, BlsPop}
import xyz.kd5ujc.crypto.kes.{CardanoSumKes, KesProduct, KesSum}
import xyz.kd5ujc.crypto.keys._
import xyz.kd5ujc.crypto.sigma.{Secp256k1Group, Sigma, SigmaError, SigmaProp}
import xyz.kd5ujc.crypto.signing._
import xyz.kd5ujc.crypto.vrf.Vrf
import xyz.kd5ujc.dilf4s.error.{
  CryptoError => JCryptoError,
  KesError => JKesError,
  KeyError => JKeyError,
  SigmaError => JSigmaError,
  VrfError => JVrfError
}
import xyz.kd5ujc.dilf4s.{Result, Signing}

import scodec.bits.ByteVector

/**
 * Signatures, key derivation, VRFs, KES and sigma verification for the Java API. secp256k1 and BLS12-381 take the
 * backend the caller loaded (`Secp256k1.load[F]`, `Bls12381.load[F]`): given it, each call is pure. Byte arrays in are
 * copied; arrays out are fresh.
 */
object Signatures {

  private def bv(a: Array[Byte]): ByteVector = ByteVector(a)

  private def crypto[A](e: Either[CryptoError, A]): Result[A, JCryptoError] = Values.result(e.left.map(Errors.cryptoError))
  private def keys[A](e:   Either[KeyError, A]): Result[A, JKeyError] = Values.result(e.left.map(Errors.keyError))

  private def pair[S](p: KeyPair[S]): Signing.KeyPair = new Signing.KeyPair(p.signing.bytes.toArray, p.verifying.bytes.toArray)

  private def verifyWith[S](codec: SchemeCodec[S], verifier: Verifier[S])(
    key:       Array[Byte],
    message:   Array[Byte],
    signature: Array[Byte]
  ): Result[java.lang.Boolean, JCryptoError] =
    crypto(
      (codec.verifyingKey(bv(key)), codec.signature(bv(signature))).mapN((k, s) =>
        java.lang.Boolean.valueOf(verifier.verify(k, bv(message), s))
      )
    )

  private def signWith[S](codec: SchemeCodec[S], signer: Signer[S])(
    key:     Array[Byte],
    message: Array[Byte]
  ): Result[Array[Byte], JCryptoError] =
    crypto(codec.signingKey(bv(key)).map(k => signer.sign(k, bv(message)).bytes.toArray))

  private def publicKeyWith[S](codec: SchemeCodec[S], p: PublicKeyOf[S])(key: Array[Byte]): Result[Array[Byte], JCryptoError] =
    crypto(codec.signingKey(bv(key)).map(k => p.verifyingKey(k).bytes.toArray))

  private def checkKey[S](codec: SchemeCodec[S])(key: Array[Byte]): Result[Array[Byte], JCryptoError] =
    crypto(codec.verifyingKey(bv(key)).map(_.bytes.toArray))

  // ---- Ed25519 (RFC 8032 keys; ZIP-215, RFC 8032 strict and libsodium verification)

  def ed25519Verify(k: Array[Byte], m: Array[Byte], s: Array[Byte]): Result[java.lang.Boolean, JCryptoError] =
    verifyWith(Ed25519.codec, Ed25519.verifier)(k, m, s)
  def ed25519VerifyStrict(k: Array[Byte], m: Array[Byte], s: Array[Byte]): Result[java.lang.Boolean, JCryptoError] =
    verifyWith(Ed25519.codec, Ed25519.strictVerifier)(k, m, s)
  def ed25519VerifyLibsodium(k: Array[Byte], m: Array[Byte], s: Array[Byte]): Result[java.lang.Boolean, JCryptoError] =
    verifyWith(Ed25519.codec, Ed25519.libsodiumVerifier)(k, m, s)
  def ed25519Sign(k:      Array[Byte], m: Array[Byte]): Result[Array[Byte], JCryptoError] = signWith(Ed25519.codec, Ed25519.signer)(k, m)
  def ed25519PublicKey(k: Array[Byte]): Result[Array[Byte], JCryptoError] = publicKeyWith(Ed25519.codec, Ed25519.publicKeyOf)(k)
  def ed25519FromSeed(seed: Array[Byte]): Result[Signing.KeyPair, JCryptoError] = crypto(
    Ed25519.seedDerivation.fromSeed(bv(seed)).map(pair)
  )
  def ed25519CheckKey(k: Array[Byte]): Result[Array[Byte], JCryptoError] = checkKey(Ed25519.codec)(k)

  // ---- P-256 ECDSA over SHA-256, RFC 6979 nonces, low S

  def p256Verify(k: Array[Byte], m: Array[Byte], s: Array[Byte]): Result[java.lang.Boolean, JCryptoError] =
    verifyWith(P256Ecdsa.codec, P256Ecdsa.verifier)(k, m, s)
  def p256Sign(k:        Array[Byte], m: Array[Byte]): Result[Array[Byte], JCryptoError] = signWith(P256Ecdsa.codec, P256Ecdsa.signer)(k, m)
  def p256PublicKey(k:   Array[Byte]): Result[Array[Byte], JCryptoError] = publicKeyWith(P256Ecdsa.codec, P256Ecdsa.publicKeyOf)(k)
  def p256FromSeed(seed: Array[Byte]): Result[Signing.KeyPair, JCryptoError] = crypto(P256Ecdsa.seedDerivation.fromSeed(bv(seed)).map(pair))
  def p256CheckKey(k:    Array[Byte]): Result[Array[Byte], JCryptoError] = checkKey(P256Ecdsa.codec)(k)

  // ---- ristretto255 Schnorr (signing takes the caller's 32 bytes of nonce randomness)

  def ristrettoVerify(k: Array[Byte], m: Array[Byte], s: Array[Byte]): Result[java.lang.Boolean, JCryptoError] =
    verifyWith(Ristretto255Schnorr.codec, Ristretto255Schnorr.verifier)(k, m, s)
  def ristrettoSign(k: Array[Byte], m: Array[Byte], randomness: Array[Byte]): Result[Array[Byte], JCryptoError] =
    crypto(
      Ristretto255Schnorr.codec
        .signingKey(bv(k))
        .flatMap(Ristretto255Schnorr.signWithRandomness(_, bv(m), bv(randomness)))
        .map(_.bytes.toArray)
    )
  def ristrettoPublicKey(k: Array[Byte]): Result[Array[Byte], JCryptoError] =
    publicKeyWith(Ristretto255Schnorr.codec, Ristretto255Schnorr.publicKeyOf)(k)
  def ristrettoFromSeed(seed: Array[Byte]): Result[Signing.KeyPair, JCryptoError] =
    crypto(Ristretto255Schnorr.seedDerivation.fromSeed(bv(seed)).map(pair))
  def ristrettoCheckKey(k: Array[Byte]): Result[Array[Byte], JCryptoError] = checkKey(Ristretto255Schnorr.codec)(k)
  val RistrettoRandomnessLength: Int = Ristretto255Schnorr.RandomnessLength

  // ---- Ed25519-BIP32 (Icarus, Cardano's extended keys)

  def icarusVerify(k: Array[Byte], m: Array[Byte], s: Array[Byte]): Result[java.lang.Boolean, JCryptoError] =
    verifyWith(Ed25519Bip32.codec, Ed25519Bip32.verifier)(k, m, s)
  def icarusSign(k: Array[Byte], m: Array[Byte]): Result[Array[Byte], JCryptoError] =
    signWith(Ed25519Bip32.codec, Ed25519Bip32.signer)(k, m)
  def icarusPublicKey(k: Array[Byte]): Result[Array[Byte], JCryptoError] = publicKeyWith(Ed25519Bip32.codec, Ed25519Bip32.publicKeyOf)(k)

  private def extended[S](e: ExtendedKey[S]): Signing.ExtendedKey = new Signing.ExtendedKey(e.key.bytes.toArray, e.chainCode.toArray)

  private def icarusKey(k: Signing.ExtendedKey): Either[KeyError, ExtendedKey[Ed25519Bip32]] =
    Ed25519Bip32.codec
      .signingKey(bv(k.key()))
      .leftMap(e => KeyError.InvalidDerivation(e.toString))
      .flatMap(ExtendedKey.from(_, bv(k.chainCode())))

  private def index(i: Signing.ChildIndex): Either[KeyError, ChildIndex] =
    if (i.hardened()) ChildIndex.hardened(i.value()) else ChildIndex.soft(i.value())

  private def path(p: java.util.List[Signing.ChildIndex]): Either[KeyError, List[ChildIndex]] = p.asScala.toList.traverse(index)

  /**
   * The root from its 96-byte secret (`Ed25519Bip32.root`), clamped. Not `Ed25519Bip32.icarus`: its PBKDF2 builds a
   * lazy xor chain 4096 deep (`Hmac.pbkdf2Sha512`), whose first read overflows an ordinary thread's stack.
   */
  def icarusRoot(secret: Array[Byte]): Result[Signing.ExtendedKey, JKeyError] = keys(Ed25519Bip32.root(Secret(bv(secret))).map(extended))

  def icarusDerive(root: Signing.ExtendedKey, p: java.util.List[Signing.ChildIndex]): Result[Signing.ExtendedKey, JKeyError] =
    keys((icarusKey(root), path(p)).tupled.flatMap { case (r, cs) => Ed25519Bip32.derive(r, cs) }.map(extended))

  // ---- SLIP-10 Ed25519 and derivation paths

  def slip10Derive(seed: Array[Byte], p: java.util.List[Signing.ChildIndex]): Result[Signing.ExtendedKey, JKeyError] =
    keys(path(p).flatMap(Slip10Ed25519.derive(Secret(bv(seed)), _)).map(extended))

  def parsePath(p: String): Result[java.util.List[Signing.ChildIndex], JKeyError] =
    keys(ChildIndex.path(p).map(_.map(i => new Signing.ChildIndex(i.value, i.hardened)).asJava))

  // ---- BIP-39

  def language(l: xyz.kd5ujc.dilf4s.Bip39.Language): Bip39.Language =
    l match {
      case xyz.kd5ujc.dilf4s.Bip39.Language.ENGLISH             => Bip39.Language.English
      case xyz.kd5ujc.dilf4s.Bip39.Language.JAPANESE            => Bip39.Language.Japanese
      case xyz.kd5ujc.dilf4s.Bip39.Language.KOREAN              => Bip39.Language.Korean
      case xyz.kd5ujc.dilf4s.Bip39.Language.SPANISH             => Bip39.Language.Spanish
      case xyz.kd5ujc.dilf4s.Bip39.Language.CHINESE_SIMPLIFIED  => Bip39.Language.ChineseSimplified
      case xyz.kd5ujc.dilf4s.Bip39.Language.CHINESE_TRADITIONAL => Bip39.Language.ChineseTraditional
      case xyz.kd5ujc.dilf4s.Bip39.Language.FRENCH              => Bip39.Language.French
      case xyz.kd5ujc.dilf4s.Bip39.Language.ITALIAN             => Bip39.Language.Italian
      case xyz.kd5ujc.dilf4s.Bip39.Language.CZECH               => Bip39.Language.Czech
      case xyz.kd5ujc.dilf4s.Bip39.Language.PORTUGUESE          => Bip39.Language.Portuguese
    }

  def wordList(l: xyz.kd5ujc.dilf4s.Bip39.Language, words: java.util.List[String]): Result[WordList, JKeyError] =
    keys(WordList(language(l), words.asScala.toVector))

  def toMnemonic(entropy: Array[Byte], words: WordList): Result[Mnemonic, JKeyError] = keys(Bip39.toMnemonic(Secret(bv(entropy)), words))

  def toEntropy(phrase: String, words: WordList): Result[java.util.Map.Entry[Mnemonic, Array[Byte]], JKeyError] =
    keys(Bip39.toEntropy(phrase, words).map { case (m, s) => java.util.Map.entry(m, s.bytes.toArray) })

  def seed(m: Mnemonic, passphrase: String): Result[Array[Byte], JKeyError] = keys(Bip39.seed(m, passphrase).map(_.bytes.toArray))

  def mnemonicWords(m:  Mnemonic): java.util.List[String] = m.words.asJava
  def mnemonicPhrase(m: Mnemonic): String = m.phrase
  def sameMnemonic(a:   Mnemonic, b: Mnemonic): Boolean = a == b

  // ---- secp256k1 (libsecp256k1, loaded by the caller): ECDSA, BIP-340, dilf4s's tagged Schnorr and BIP-32

  def ecdsaVerify(b: Secp256k1, k: Array[Byte], m: Array[Byte], s: Array[Byte]): Result[java.lang.Boolean, JCryptoError] =
    verifyWith(b.ecdsaCodec, b.ecdsaVerifier)(k, m, s)
  def ecdsaSign(b: Secp256k1, k: Array[Byte], m: Array[Byte]): Result[Array[Byte], JCryptoError] =
    signWith(b.ecdsaCodec, b.ecdsaSigner)(k, m)
  def ecdsaPublicKey(b: Secp256k1, k: Array[Byte]): Result[Array[Byte], JCryptoError] = publicKeyWith(b.ecdsaCodec, b.ecdsaPublicKeyOf)(k)
  def ecdsaFromSeed(b: Secp256k1, seed: Array[Byte]): Result[Signing.KeyPair, JCryptoError] = crypto(
    b.ecdsaSeedDerivation.fromSeed(bv(seed)).map(pair)
  )
  def ecdsaCheckKey(b: Secp256k1, k: Array[Byte]): Result[Array[Byte], JCryptoError] = checkKey(b.ecdsaCodec)(k)

  /** Standard BIP-340: the message is exactly 32 bytes, as `bip340_verify` reads it. */
  def bip340Verify(b: Secp256k1, k: Array[Byte], m: Array[Byte], s: Array[Byte]): Result[java.lang.Boolean, JCryptoError] =
    crypto(
      (b.bip340Codec.verifyingKey(bv(k)), Message32.from(bv(m)), b.bip340Codec.signature(bv(s))).mapN((key, msg, sig) =>
        java.lang.Boolean.valueOf(b.bip340Verify(key, msg, sig))
      )
    )
  def bip340Sign(b: Secp256k1, k: Array[Byte], m: Array[Byte], aux: Array[Byte]): Result[Array[Byte], JCryptoError] =
    crypto(
      (b.bip340Codec.signingKey(bv(k)), Message32.from(bv(m))).tupled.flatMap { case (key, msg) => b.bip340SignWithAux(key, msg, bv(aux)) }
        .map(_.bytes.toArray)
    )
  def bip340PublicKey(b: Secp256k1, k: Array[Byte]): Result[Array[Byte], JCryptoError] =
    publicKeyWith(b.bip340Codec, b.bip340PublicKeyOf)(k)
  def bip340FromSeed(b: Secp256k1, seed: Array[Byte]): Result[Signing.KeyPair, JCryptoError] = crypto(
    b.bip340SeedDerivation.fromSeed(bv(seed)).map(pair)
  )
  def bip340CheckKey(b: Secp256k1, k: Array[Byte]): Result[Array[Byte], JCryptoError] = checkKey(b.bip340Codec)(k)

  def taggedVerify(b: Secp256k1, k: Array[Byte], m: Array[Byte], s: Array[Byte]): Result[java.lang.Boolean, JCryptoError] =
    verifyWith(b.taggedSchnorrCodec, b.taggedSchnorrVerifier)(k, m, s)
  def taggedSign(b: Secp256k1, k: Array[Byte], m: Array[Byte], aux: Array[Byte]): Result[Array[Byte], JCryptoError] =
    crypto(b.taggedSchnorrCodec.signingKey(bv(k)).flatMap(b.taggedSchnorrSignWithAux(_, bv(m), bv(aux))).map(_.bytes.toArray))
  def taggedPublicKey(b: Secp256k1, k: Array[Byte]): Result[Array[Byte], JCryptoError] =
    publicKeyWith(b.taggedSchnorrCodec, b.taggedSchnorrPublicKeyOf)(k)
  def taggedFromSeed(b: Secp256k1, seed: Array[Byte]): Result[Signing.KeyPair, JCryptoError] =
    crypto(b.taggedSchnorrSeedDerivation.fromSeed(bv(seed)).map(pair))

  def bip32Derive(b: Secp256k1, seed: Array[Byte], p: java.util.List[Signing.ChildIndex]): Result[Signing.ExtendedKey, JKeyError] =
    keys(path(p).flatMap(new Bip32Secp256k1(b).derive(Secret(bv(seed)), _)).map(extended))

  def bip32PublicChild(
    b:         Secp256k1,
    key:       Array[Byte],
    chainCode: Array[Byte],
    i:         Signing.ChildIndex
  ): Result[Signing.PublicChild, JKeyError] =
    keys(
      b.ecdsaCodec
        .verifyingKey(bv(key))
        .leftMap(e => KeyError.InvalidDerivation(e.toString): KeyError)
        .flatMap(vk => index(i).flatMap(new Bip32Secp256k1(b).publicChild(vk, bv(chainCode), _)))
        .map { case (vk, cc) => new Signing.PublicChild(vk.bytes.toArray, cc.toArray) }
    )

  // ---- BLS12-381, Ethereum's proof-of-possession profile (blst, loaded by the caller)

  def blsVerify(b: Bls12381, k: Array[Byte], m: Array[Byte], s: Array[Byte]): Result[java.lang.Boolean, JCryptoError] =
    verifyWith(b.codec, b.verifier)(k, m, s)
  def blsSign(b: Bls12381, k: Array[Byte], m: Array[Byte]): Result[Array[Byte], JCryptoError] =
    crypto(b.codec.signingKey(bv(k)).flatMap(b.sign(_, bv(m))).map(_.bytes.toArray))
  def blsPublicKey(b: Bls12381, k: Array[Byte]): Result[Array[Byte], JCryptoError] = crypto(
    b.codec.signingKey(bv(k)).flatMap(b.publicKey).map(_.bytes.toArray)
  )
  def blsFromSeed(b: Bls12381, seed: Array[Byte]): Result[Signing.KeyPair, JCryptoError] = crypto(
    b.seedDerivation.fromSeed(bv(seed)).map(pair)
  )
  def blsCheckKey(b: Bls12381, k: Array[Byte]): Result[Array[Byte], JCryptoError] = checkKey(b.codec)(k)

  private def blsSignatures(b: Bls12381, ss: java.util.List[Array[Byte]]): Either[CryptoError, List[Signature[BlsPop]]] =
    ss.asScala.toList.traverse(s => b.codec.signature(bv(s)))

  private def blsKeys(b: Bls12381, ks: java.util.List[Array[Byte]]): Either[CryptoError, List[VerifyingKey[BlsPop]]] =
    ks.asScala.toList.traverse(k => b.codec.verifyingKey(bv(k)))

  def blsAggregate(b: Bls12381, signatures: java.util.List[Array[Byte]]): Result[Array[Byte], JCryptoError] =
    crypto(blsSignatures(b, signatures).flatMap(b.aggregate).map(_.bytes.toArray))

  /** One message signed by every key: the keys must carry proofs of possession, checked by whoever admitted them. */
  def blsFastAggregateVerify(
    b:    Bls12381,
    keys: java.util.List[Array[Byte]],
    m:    Array[Byte],
    s:    Array[Byte]
  ): Result[java.lang.Boolean, JCryptoError] =
    crypto((blsKeys(b, keys), b.codec.signature(bv(s))).mapN((ks, sig) => java.lang.Boolean.valueOf(b.fastAggregateVerify(ks, bv(m), sig))))

  def blsAggregateVerify(
    b:     Bls12381,
    pairs: java.util.List[java.util.Map.Entry[Array[Byte], Array[Byte]]],
    s:     Array[Byte]
  ): Result[java.lang.Boolean, JCryptoError] =
    crypto(
      (pairs.asScala.toList.traverse(e => b.codec.verifyingKey(bv(e.getKey)).map(_ -> bv(e.getValue))), b.codec.signature(bv(s)))
        .mapN((ps, sig) => java.lang.Boolean.valueOf(b.aggregateVerify(ps, sig)))
    )

  def blsPopProve(b: Bls12381, k: Array[Byte]): Result[Array[Byte], JCryptoError] =
    crypto(b.codec.signingKey(bv(k)).flatMap(b.popProve).map(_.bytes.toArray))

  def blsPopVerify(b: Bls12381, k: Array[Byte], proof: Array[Byte]): Result[java.lang.Boolean, JCryptoError] =
    crypto((b.codec.verifyingKey(bv(k)), b.codec.signature(bv(proof))).mapN((key, p) => java.lang.Boolean.valueOf(b.popVerify(key, p))))

  def blsEip2333Master(b: Bls12381, seed: Array[Byte]): Result[Array[Byte], JCryptoError] = crypto(
    b.eip2333Master(bv(seed)).map(_.bytes.toArray)
  )

  def blsEip2333Child(b: Bls12381, parent: Array[Byte], i: Long): Result[Array[Byte], JCryptoError] =
    crypto(b.codec.signingKey(bv(parent)).flatMap(b.eip2333Child(_, i)).map(_.bytes.toArray))

  // ---- ecrecover (Ethereum)

  def ecrecover(digest: Array[Byte], signature: Array[Byte]): Result[java.util.Optional[Array[Byte]], JCryptoError] =
    crypto(
      Ecrecover
        .recover(bv(digest), bv(signature))
        .bimap(
          CryptoError.InvalidEncoding("ecrecover", _),
          _.fold(java.util.Optional.empty[Array[Byte]]())(a => java.util.Optional.of(a.toArray))
        )
    )

  def ethereumAddress(key: Array[Byte]): Result[Array[Byte], JCryptoError] =
    crypto(Ecrecover.address(bv(key)).bimap(CryptoError.InvalidEncoding("Ethereum public key", _), _.toArray))

  // ---- ECVRF

  private def vrfOf[R](s: xyz.kd5ujc.dilf4s.Vrf.Suite)(f: [S] => Vrf[S] => R): R =
    s match {
      case xyz.kd5ujc.dilf4s.Vrf.Suite.TAI     => f(Vrf.tai)
      case xyz.kd5ujc.dilf4s.Vrf.Suite.ELL2    => f(Vrf.ell2)
      case xyz.kd5ujc.dilf4s.Vrf.Suite.DRAFT03 => f(Vrf.draft03)
    }

  private def vrf[A](e: Either[xyz.kd5ujc.crypto.vrf.VrfError, A]): Result[A, JVrfError] = Values.result(e.left.map(Errors.vrfError))

  def vrfName(s: xyz.kd5ujc.dilf4s.Vrf.Suite): String = vrfOf(s)([S] => (v: Vrf[S]) => v.name)

  def vrfVerify(s: xyz.kd5ujc.dilf4s.Vrf.Suite, key: Array[Byte], alpha: Array[Byte], proof: Array[Byte]): Result[Array[Byte], JVrfError] =
    vrfOf(s)([S] => (v: Vrf[S]) => vrf(v.verifyBytes(bv(key), bv(alpha), bv(proof)).map(_.bytes.toArray)))

  def vrfProve(s: xyz.kd5ujc.dilf4s.Vrf.Suite, key: Array[Byte], alpha: Array[Byte]): Result[xyz.kd5ujc.dilf4s.Vrf.Proof, JVrfError] =
    vrfOf(s)(
      [S] =>
        (v: Vrf[S]) =>
          vrf(v.signingKey(bv(key)).flatMap(v.prove(_, bv(alpha))).map {
            case (pi, beta) => new xyz.kd5ujc.dilf4s.Vrf.Proof(pi.bytes.toArray, beta.bytes.toArray)
          })
    )

  def vrfPublicKey(s: xyz.kd5ujc.dilf4s.Vrf.Suite, key: Array[Byte]): Result[Array[Byte], JVrfError] =
    vrfOf(s)([S] => (v: Vrf[S]) => vrf(v.signingKey(bv(key)).map(v.publicKey(_).bytes.toArray)))

  def vrfFromSeed(s: xyz.kd5ujc.dilf4s.Vrf.Suite, seed: Array[Byte]): Result[Signing.KeyPair, JVrfError] =
    vrfOf(s)([S] => (v: Vrf[S]) => vrf(Vrf.fromSeed[S](bv(seed))(using v).map(pair)))

  def vrfCheckKey(s: xyz.kd5ujc.dilf4s.Vrf.Suite, key: Array[Byte]): Result[Array[Byte], JVrfError] =
    vrfOf(s)([S] => (v: Vrf[S]) => vrf(v.verifyingKey(bv(key)).map(_.bytes.toArray)))

  // ---- KES (docs/specs/kes.md): dilf4s's sum and product compositions over any hash, and Cardano's sum. Keys cross the
  // boundary as their bytes and period, so no key is tied to a scheme instance outside this object.

  private def kes[A](e: Either[xyz.kd5ujc.crypto.kes.KesError, A]): Result[A, JKesError] = Values.result(e.left.map(Errors.kesError))

  /** One KES construction, its key type closed over. */
  final private case class Kes(
    total:     Long,
    keyLength: Long,
    sigLength: Long,
    verify:    (ByteVector, Long, ByteVector, ByteVector) => Either[KesErr, Boolean],
    generate:  ByteVector => Either[KesErr, (ByteVector, Long, ByteVector)],
    sign:      (ByteVector, Long, ByteVector) => Either[KesErr, ByteVector],
    evolve:    (ByteVector, Long) => Either[KesErr, (ByteVector, Long, ByteVector)],
    load:      (ByteVector, Long) => Either[KesErr, (ByteVector, Long, ByteVector)]
  )

  private type KesErr = xyz.kd5ujc.crypto.kes.KesError

  private def sum[H](h: Hasher[H], height: Int): Either[KesErr, Kes] =
    KesSum[H](height)(using h).map { k =>
      def out(key: k.Key) = (key.bytes, key.period, k.verifyingKey(key))
      Kes(
        k.totalPeriods,
        k.keyLength,
        k.signatureLength,
        k.verify,
        seed => k.generate(seed).map(out),
        (raw, period, m) => k.load(raw, period).flatMap(k.sign(_, m)),
        (raw, period) => k.load(raw, period).flatMap(k.evolve).map(out),
        (raw, period) => k.load(raw, period).map(out)
      )
    }

  private def product[H](h: Hasher[H], superHeight: Int, subHeight: Int): Either[KesErr, Kes] =
    KesProduct[H](superHeight, subHeight)(using h).map { k =>
      def out(key: k.Key) = (key.bytes, key.period, k.verifyingKey(key))
      Kes(
        k.totalPeriods,
        k.keyLength,
        k.signatureLength,
        k.verify,
        seed => k.generate(seed).map(out),
        (raw, period, m) => k.load(raw, period).flatMap(k.sign(_, m)),
        (raw, period) => k.load(raw, period).flatMap(k.evolve).map(out),
        (raw, period) => k.load(raw, period).map(out)
      )
    }

  private def cardanoSum(depth: Int): Either[KesErr, Kes] =
    CardanoSumKes(depth).map { k =>
      def out(key: k.Key) = (key.bytes, key.period, k.verifyingKey(key))
      Kes(
        k.totalPeriods,
        k.signingKeyLength,
        k.signatureLength,
        k.verify,
        seed => k.generate(seed).map(out),
        (raw, period, m) => k.load(raw, period).map(k.sign(_, m)),
        (raw, period) => k.load(raw, period).flatMap(k.evolve).map(out),
        (raw, period) => k.load(raw, period).map(out)
      )
    }

  private def construction(c: xyz.kd5ujc.dilf4s.Kes.Construction): Either[KesErr, Kes] =
    c match {
      case s: xyz.kd5ujc.dilf4s.Kes.Construction.Sum        => sum(Hashes.hasher(s.hash), s.height)
      case p: xyz.kd5ujc.dilf4s.Kes.Construction.Product    => product(Hashes.hasher(p.hash), p.superHeight, p.subHeight)
      case d: xyz.kd5ujc.dilf4s.Kes.Construction.CardanoSum => cardanoSum(d.depth)
    }

  private def key(r: (ByteVector, Long, ByteVector)): xyz.kd5ujc.dilf4s.Kes.Key =
    new xyz.kd5ujc.dilf4s.Kes.Key(r._1.toArray, r._2, r._3.toArray)

  def kesShape(c: xyz.kd5ujc.dilf4s.Kes.Construction): Result[Array[Long], JKesError] =
    kes(construction(c).map(k => Array(k.total, k.keyLength, k.sigLength)))

  def kesVerify(
    c:         xyz.kd5ujc.dilf4s.Kes.Construction,
    vk:        Array[Byte],
    period:    Long,
    message:   Array[Byte],
    signature: Array[Byte]
  ): Result[java.lang.Boolean, JKesError] =
    kes(construction(c).flatMap(_.verify(bv(vk), period, bv(message), bv(signature))).map(java.lang.Boolean.valueOf))

  def kesGenerate(c: xyz.kd5ujc.dilf4s.Kes.Construction, seed: Array[Byte]): Result[xyz.kd5ujc.dilf4s.Kes.Key, JKesError] =
    kes(construction(c).flatMap(_.generate(bv(seed))).map(key))

  def kesSign(c: xyz.kd5ujc.dilf4s.Kes.Construction, raw: Array[Byte], period: Long, message: Array[Byte]): Result[Array[Byte], JKesError] =
    kes(construction(c).flatMap(_.sign(bv(raw), period, bv(message))).map(_.toArray))

  def kesEvolve(c: xyz.kd5ujc.dilf4s.Kes.Construction, raw: Array[Byte], period: Long): Result[xyz.kd5ujc.dilf4s.Kes.Key, JKesError] =
    kes(construction(c).flatMap(_.evolve(bv(raw), period)).map(key))

  def kesLoad(c: xyz.kd5ujc.dilf4s.Kes.Construction, raw: Array[Byte], period: Long): Result[xyz.kd5ujc.dilf4s.Kes.Key, JKesError] =
    kes(construction(c).flatMap(_.load(bv(raw), period)).map(key))

  // ---- sigma protocols over secp256k1 (docs/specs/sigma.md)

  private def sigma[A](e: Either[SigmaError, A]): Result[A, JSigmaError] = Values.result(e.left.map(Errors.sigmaError))

  /** `[proposition, proof, message]`: the proposition in its tagged-map form, as `sigma_verify` reads it. */
  def sigmaVerify(proposition: CanonicalValue, proof: Array[Byte], message: Array[Byte]): Result[java.lang.Boolean, JSigmaError] =
    sigma(SigmaProp.decode(proposition).flatMap(Sigma.check(_, bv(proof), bv(message))).map(java.lang.Boolean.valueOf))

  /** A single-key sigma proof (Ergo's P2PK signature), as `schnorr_verify` reads it. */
  def schnorrVerify(key: Array[Byte], message: Array[Byte], proof: Array[Byte]): Result[java.lang.Boolean, JSigmaError] =
    sigma(
      Secp256k1Group
        .decode(bv(key))
        .leftMap(reason => SigmaError.InvalidPoint("pk", reason): SigmaError)
        .flatMap(SigmaProp.dlog(_).flatMap(Sigma.check(_, bv(proof), bv(message))))
        .map(java.lang.Boolean.valueOf)
    )

  def sigmaProofLength(proposition: CanonicalValue): Result[java.lang.Long, JSigmaError] =
    sigma(SigmaProp.decode(proposition).map(p => java.lang.Long.valueOf(Sigma.proofLength(p))))
}
