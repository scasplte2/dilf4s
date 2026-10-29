package xyz.kd5ujc.core.vectors

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

import cats.effect.IO

import weaver.SimpleIOSuite

object SharedVectorsSuite extends SimpleIOSuite {

  test("committed canonical-encoding vectors match the reference implementation") {
    IO(new String(Files.readAllBytes(CanonicalVectors.path), UTF_8)).map { committed =>
      expect(
        committed == CanonicalVectors.render,
        s"${CanonicalVectors.path} is stale; regenerate with: sbt \"core/Test/runMain xyz.kd5ujc.core.vectors.CanonicalVectorGen\""
      )
    }
  }

  pureTest("no rejection vector is accidentally accepted") {
    expect(!CanonicalVectors.render.contains("ACCEPTED"), "every cborRejections entry must fail to decode")
  }
}
