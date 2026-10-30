package xyz.kd5ujc.crypto.kes

import cats.syntax.all._

import xyz.kd5ujc.core.hash.{Blake2b256, DomainTag, Hasher, Sha256, Sha3_512}

import org.scalacheck.Gen
import scodec.bits.{ByteVector, hex}
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

object MerkleKesSuite extends SimpleIOSuite with Checkers {

  private val seed: Gen[ByteVector] = Gen.listOfN(32, Gen.choose(Byte.MinValue, Byte.MaxValue)).map(ByteVector(_))
  implicit private val showBytes: cats.Show[ByteVector] = cats.Show.show(_.toHex)

  private def sumLaws[H: Hasher](name: String, height: Int) = {
    val kes = KesSum[H](height).toOption.get
    test(s"KesSum[$name] height $height: every period signs, verifies only there, and round-trips its encoding") {
      forall(seed) { s =>
        val k0 = kes.generate(s).toOption.get
        val vk = kes.verifyingKey(k0)
        val keys = List.unfold(Option(k0))(_.map(k => (k, kes.evolve(k).toOption)))
        expect
          .same(kes.totalPeriods.toInt, keys.size)
          .and(expect.same(Hasher[H].outputSize, vk.size))
          .and(keys.foldMap { k =>
            val sig = kes.sign(k, hex"beef").toOption.get
            expect
              .same(kes.signatureLength, sig.size)
              .and(expect.same(kes.keyLength, k.bytes.size))
              .and(expect.same(Right(true), kes.verify(vk, k.period, hex"beef", sig)))
              .and(
                expect(
                  (0L until kes.totalPeriods).filterNot(_ == k.period).forall(p => kes.verify(vk, p, hex"beef", sig) == Right(false)),
                  "other periods"
                )
              )
              .and(expect.same(Right(false), kes.verify(vk, k.period, hex"beee", sig)))
              .and(expect.same(Right(k.bytes), kes.load(k.bytes, k.period).map(_.bytes)))
              .and(expect.same(kes.evolve(k).map(_.bytes), kes.load(k.bytes, k.period).flatMap(kes.evolve).map(_.bytes)))
          })
          .and(expect.same(Left(KesError.Exhausted(kes.totalPeriods - 1)), kes.evolve(keys.last)))
      }
    }
  }

  sumLaws[Blake2b256]("Blake2b-256", 3)
  sumLaws[Sha256]("SHA-256", 2)
  sumLaws[Sha3_512]("SHA3-512", 2)

  private def productLaws[H: Hasher](name: String, sup: Int, sub: Int) = {
    val kes = KesProduct[H](sup, sub).toOption.get
    test(s"KesProduct[$name] ($sup, $sub): every period signs, verifies only there, round-trips, and erases the super leaf") {
      forall(seed) { s =>
        val k0 = kes.generate(s).toOption.get
        val vk = kes.verifyingKey(k0)
        val keys = List.unfold(Option(k0))(_.map(k => (k, kes.evolve(k).toOption)))
        expect
          .same(kes.totalPeriods.toInt, keys.size)
          .and(keys.foldMap { k =>
            val sig = kes.sign(k, hex"beef").toOption.get
            expect
              .same(kes.signatureLength, sig.size)
              .and(expect.same(kes.keyLength, k.bytes.size))
              .and(expect(keys.forall(o => kes.verifyingKey(o) == vk), "fixed root"))
              .and(expect.same(Right(true), kes.verify(vk, k.period, hex"beef", sig)))
              .and(
                expect(
                  (0L until kes.totalPeriods).filterNot(_ == k.period).forall(p => kes.verify(vk, p, hex"beef", sig) == Right(false)),
                  "other periods"
                )
              )
              .and(expect.same(Right(k.bytes), kes.load(k.bytes, k.period).map(_.bytes)))
              .and(expect(ops[H].sign(k.superTree, sup, k.period >> sub, hex"00").isEmpty, "the super leaf cannot sign again"))
          })
          .and(expect.same(Left(KesError.Exhausted(kes.totalPeriods - 1)), kes.evolve(keys.last)))
      }
    }
  }

  private def ops[H: Hasher] = new MerkleKesOps[H]

  productLaws[Blake2b256]("Blake2b-256", 2, 2)
  productLaws[Sha3_512]("SHA3-512", 1, 2)
  productLaws[Blake2b256]("Blake2b-256", 0, 2)
  productLaws[Blake2b256]("Blake2b-256", 2, 0)

  pureTest("a product signature cannot be replayed as a sum signature of the super tree (tagged sub-root)") {
    val prod = KesProduct[Blake2b256](2, 1).toOption.get
    val k = prod.generate(ByteVector.fill(32)(4)).toOption.get
    val sig = prod.sign(k, hex"").toOption.get
    val superSig = sig.take(ops[Blake2b256].signatureLength(2))
    val subRoot = sig.slice(superSig.size, superSig.size + 32)
    val asSum = KesSum[Blake2b256](2).toOption.get
    expect
      .same(Right(false), asSum.verify(prod.verifyingKey(k), 0, subRoot, superSig))
      .and(expect.same(Right(true), asSum.verify(prod.verifyingKey(k), 0, ops[Blake2b256].productMessage(subRoot), superSig)))
  }

  pureTest("corrupted keys are refused on load or when their right half activates") {
    val kes = KesSum[Blake2b256](2).toOption.get
    val k = kes.generate(ByteVector.fill(32)(7)).toOption.get
    val b = k.bytes
    // layout (height 2): [leaf(64) || seed1(32) || l1 || r1] || seed2(32) || l2 || r2; the top seed is at 160
    val badLeaf = b.update(0, (b(0) ^ 1).toByte)
    val badTopSeed = b.update(160L, (b(160L) ^ 1).toByte)
    val k1 = kes.load(badTopSeed, 0).flatMap(kes.evolve)
    expect(kes.load(badLeaf, 0).isLeft, "leaf seed/vk mismatch")
      .and(expect(kes.load(b, 2).isLeft, "pending seeds at a later period"))
      .and(expect(k1.flatMap(kes.evolve).isLeft, "a corrupted right seed is caught on activation"))
      .and(expect(KesSum[Blake2b256](31).isLeft))
      .and(expect(kes.verify(kes.verifyingKey(k), 4, hex"", kes.sign(k, hex"").toOption.get).isLeft))
  }

  pureTest("evolveTo jumps forward like repeated evolution and refuses the past") {
    val prod = KesProduct[Blake2b256](2, 2).toOption.get
    val k0 = prod.generate(ByteVector.fill(32)(3)).toOption.get
    val stepwise = (1 to 9).foldLeft(k0)((k, _) => prod.evolve(k).toOption.get)
    val sum = CardanoSumKes.Sum6
    val c0 = sum.generate(ByteVector.fill(32)(3)).toOption.get
    expect
      .same(Right(stepwise.bytes), prod.evolveTo(k0, 9).map(_.bytes))
      .and(expect(prod.evolveTo(stepwise, 3).isLeft, "backwards"))
      .and(expect(prod.evolveTo(k0, 16).isLeft, "beyond the last period"))
      .and(expect.same(Right(63L), sum.evolveTo(c0, 63).map(_.period)))
  }

  pureTest("KES tags satisfy the tag grammar") {
    MerkleKes.Tags.foldMap(t => expect(DomainTag(t).isRight, t))
  }
}
