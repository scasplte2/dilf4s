package xyz.kd5ujc.signing

import java.security.SecureRandom
import java.util.HexFormat

import weaver.SimpleIOSuite

import cats.effect.IO

object Ristretto255Suite extends SimpleIOSuite {

  private val hex = HexFormat.of()
  private val scheme = new Ristretto255

  // ========================================================================
  // FROST(ristretto255, SHA-512) test vectors from draft-irtf-cfrg-frost-15
  // ========================================================================

  test("FROST test vector: aggregate signature verifies") {
    IO {
      // From draft-irtf-cfrg-frost-15, section F.1 (FROST(ristretto255, SHA-512))
      val groupPublicKey = hex.parseHex("e2a62f39eede11269e3bd5a7d97554f5ca384f9f6d3dd9c3c0d05083c7254f57")
      val message = hex.parseHex("74657374") // "test"
      val signature = hex.parseHex(
        "fc45655fbc66bbffad654ea4ce5fdae253a49a64ace25d9adb62010dd9fb2555" +
          "2164141787162e5b4cab915b4aa45d94655dbb9ed7c378a53b980a0be220a802"
      )

      val pk = Ristretto255.PublicKey(groupPublicKey)
      expect(scheme.verify(signature, message, pk))
    }
  }

  test("FROST test vector: single-signer sign then verify with group_secret_key") {
    IO {
      // group_secret_key from draft-irtf-cfrg-frost-15 section F.1
      val groupSecretKey = hex.parseHex("1b25a55e463cfd15cf14a5d3acc3d15053f08da49c8afcf3ab265f2ebc4f970b")
      val sk = Ristretto255.SecretKey(groupSecretKey)
      val pk = scheme.getVerificationKey(sk)

      // Verify the derived public key matches the test vector
      val expectedPk = hex.parseHex("e2a62f39eede11269e3bd5a7d97554f5ca384f9f6d3dd9c3c0d05083c7254f57")
      expect(java.util.Arrays.equals(pk.bytes, expectedPk)) &&
      // Sign a message and verify it round-trips
      {
        val message = "test".getBytes("UTF-8")
        val sig = scheme.sign(sk, message)
        expect(scheme.verify(sig, message, pk))
      }
    }
  }

  // ========================================================================
  // Property-based tests (self-consistency)
  // ========================================================================

  test("sign then verify succeeds") {
    IO {
      val seed = randomBytes(32)
      val kp = scheme.deriveKeyPairFromSeed(seed)
      val msg = "Hello, Ristretto255!".getBytes("UTF-8")
      val sig = scheme.sign(kp.signingKey, msg)
      expect(scheme.verify(sig, msg, kp.verificationKey))
    }
  }

  test("wrong message fails verification") {
    IO {
      val seed = randomBytes(32)
      val kp = scheme.deriveKeyPairFromSeed(seed)
      val msg = "correct message".getBytes("UTF-8")
      val sig = scheme.sign(kp.signingKey, msg)
      expect(!scheme.verify(sig, "wrong message".getBytes("UTF-8"), kp.verificationKey))
    }
  }

  test("wrong key fails verification") {
    IO {
      val kp1 = scheme.deriveKeyPairFromSeed(randomBytes(32))
      val kp2 = scheme.deriveKeyPairFromSeed(randomBytes(32))
      val msg = "test message".getBytes("UTF-8")
      val sig = scheme.sign(kp1.signingKey, msg)
      expect(!scheme.verify(sig, msg, kp2.verificationKey))
    }
  }

  test("corrupted signature fails verification") {
    IO {
      val kp = scheme.deriveKeyPairFromSeed(randomBytes(32))
      val msg = "test message".getBytes("UTF-8")
      val sig = scheme.sign(kp.signingKey, msg)
      // Flip a bit in the signature
      sig(10) = (sig(10) ^ 0x01).toByte
      expect(!scheme.verify(sig, msg, kp.verificationKey))
    }
  }

  test("empty message signs and verifies") {
    IO {
      val kp = scheme.deriveKeyPairFromSeed(randomBytes(32))
      val msg = Array.emptyByteArray
      val sig = scheme.sign(kp.signingKey, msg)
      expect(scheme.verify(sig, msg, kp.verificationKey))
    }
  }

  test("large message signs and verifies") {
    IO {
      val kp = scheme.deriveKeyPairFromSeed(randomBytes(32))
      val msg = randomBytes(10000)
      val sig = scheme.sign(kp.signingKey, msg)
      expect(scheme.verify(sig, msg, kp.verificationKey))
    }
  }

  test("signature is 64 bytes") {
    IO {
      val kp = scheme.deriveKeyPairFromSeed(randomBytes(32))
      val msg = "test".getBytes("UTF-8")
      val sig = scheme.sign(kp.signingKey, msg)
      expect(sig.length == 64)
    }
  }

  test("public key is 32 bytes") {
    IO {
      val kp = scheme.deriveKeyPairFromSeed(randomBytes(32))
      expect(kp.verificationKey.bytes.length == 32)
    }
  }

  test("signatures are randomized (hedged nonces)") {
    IO {
      val seed = randomBytes(32)
      val kp = scheme.deriveKeyPairFromSeed(seed)
      val msg = "nonce test".getBytes("UTF-8")
      val sig1 = scheme.sign(kp.signingKey, msg)
      val sig2 = scheme.sign(kp.signingKey, msg)
      // Different signatures for same message (randomized nonces)
      expect(!java.util.Arrays.equals(sig1, sig2)) &&
      // But both verify
      expect(scheme.verify(sig1, msg, kp.verificationKey)) &&
      expect(scheme.verify(sig2, msg, kp.verificationKey))
    }
  }

  test("different seeds produce different public keys") {
    IO {
      val kp1 = scheme.deriveKeyPairFromSeed(randomBytes(32))
      val kp2 = scheme.deriveKeyPairFromSeed(randomBytes(32))
      expect(!java.util.Arrays.equals(kp1.verificationKey.bytes, kp2.verificationKey.bytes))
    }
  }

  // ========================================================================
  // Ristretto-specific tests
  // ========================================================================

  test("public key is valid CompressedRistretto point") {
    IO {
      import cafe.cryptography.curve25519.CompressedRistretto
      val kp = scheme.deriveKeyPairFromSeed(randomBytes(32))
      // Should not throw — valid canonical Ristretto encoding
      val point = new CompressedRistretto(kp.verificationKey.bytes).decompress()
      // Decompressed point should recompress to same bytes
      expect(java.util.Arrays.equals(point.compress().toByteArray, kp.verificationKey.bytes))
    }
  }

  test("signature R component is valid CompressedRistretto point") {
    IO {
      import cafe.cryptography.curve25519.CompressedRistretto
      val kp = scheme.deriveKeyPairFromSeed(randomBytes(32))
      val msg = "test".getBytes("UTF-8")
      val sig = scheme.sign(kp.signingKey, msg)
      val rBytes = sig.slice(0, 32)
      val rPoint = new CompressedRistretto(rBytes).decompress()
      expect(java.util.Arrays.equals(rPoint.compress().toByteArray, rBytes))
    }
  }

  test("no clamping — scalar is purely reduced mod L") {
    IO {
      import cafe.cryptography.curve25519.Scalar
      // Generate many keys and verify none have clamping artifacts
      val results = (0 until 100).map { _ =>
        val seed = randomBytes(32)
        val sk = scheme.deriveSecretKeyFromSeed(seed)
        val scalar = sk.bytes

        // Ed25519 clamping sets scalar[0] & 0xf8 and scalar[31] & 0x7f | 0x40
        // Ristretto does NOT do this — scalar is just reduced mod L
        // Verify the scalar is canonical (valid Scalar)
        try {
          Scalar.fromCanonicalBytes(scalar)
          true
        } catch {
          case _: Exception => false
        }
      }
      expect(results.forall(identity))
    }
  }

  test("identity point is not a valid public key for signing") {
    IO {
      import cafe.cryptography.curve25519.RistrettoElement
      // Identity compressed encoding
      val identityBytes = RistrettoElement.IDENTITY.compress().toByteArray
      val pk = Ristretto255.PublicKey(identityBytes)
      val kp = scheme.deriveKeyPairFromSeed(randomBytes(32))
      val msg = "test".getBytes("UTF-8")
      val sig = scheme.sign(kp.signingKey, msg)
      // Signature made by a different key should not verify with identity
      expect(!scheme.verify(sig, msg, pk))
    }
  }

  test("invalid public key bytes fail verification gracefully") {
    IO {
      val kp = scheme.deriveKeyPairFromSeed(randomBytes(32))
      val msg = "test".getBytes("UTF-8")
      val sig = scheme.sign(kp.signingKey, msg)
      // All-0xff is not a valid Ristretto encoding
      val badPk = Ristretto255.PublicKey(Array.fill(32)(0xff.toByte))
      expect(!scheme.verify(sig, msg, badPk))
    }
  }

  test("wrong-length signature fails verification gracefully") {
    IO {
      val kp = scheme.deriveKeyPairFromSeed(randomBytes(32))
      expect(!scheme.verify(Array.emptyByteArray, "test".getBytes("UTF-8"), kp.verificationKey)) &&
      expect(!scheme.verify(new Array[Byte](63), "test".getBytes("UTF-8"), kp.verificationKey))
    }
  }

  // ========================================================================
  // Cross-verification: multiple sign/verify rounds with different messages
  // ========================================================================

  test("batch: 50 random messages all verify") {
    IO {
      val kp = scheme.deriveKeyPairFromSeed(randomBytes(32))
      val results = (0 until 50).map { i =>
        val msg = s"message $i ${randomBytes(16).map("%02x".format(_)).mkString}".getBytes("UTF-8")
        val sig = scheme.sign(kp.signingKey, msg)
        scheme.verify(sig, msg, kp.verificationKey)
      }
      expect(results.forall(identity))
    }
  }

  // ========================================================================
  // Helpers
  // ========================================================================

  private val rng = new SecureRandom()

  private def randomBytes(n: Int): Array[Byte] = {
    val buf = new Array[Byte](n)
    rng.nextBytes(buf)
    buf
  }
}
