import java.nio.file.Files
import java.security.MessageDigest

import sbt._

/**
 * Artifacts pinned by SHA-256: native code on a consensus path (BN254 through gnark) and what loads it. `update` fails
 * if a pinned module resolves to another version or to different bytes, so neither a repository nor a mirror can
 * substitute them. The bytes come from `vendor/maven` (see its README).
 */
object Pinned {

  sealed trait Problem extends Product with Serializable
  object Problem {
    final case class VersionMismatch(module: ModuleID, expected: String) extends Problem
    final case class DigestMismatch(module: ModuleID, file: File, actual: String, expected: String) extends Problem
  }

  final case class Rejected(problems: List[Problem])
      extends RuntimeException(
        s"pinned artifacts do not match:\n  ${problems.map {
            case Problem.VersionMismatch(module, expected) => s"$module: pinned at $expected"
            case Problem.DigestMismatch(module, file, actual, expected) =>
              s"$module: ${file.getName} has SHA-256 $actual, pinned $expected"
          }.mkString("\n  ")}"
      )

  val artifacts: Map[(String, String), (String, String)] = Map(
    ("org.hyperledger.besu", "gnark")              -> ("2.0.0", "9632cf125816fda1968886ef2fa0a8620cdf2ebf8b193968b66c31796bc5095e"),
    ("org.hyperledger.besu", "besu-native-common") -> ("2.0.0", "8c29eecec101843f3350be63a0483aaa17f26ff2fc87c3e477258ab668d17829"),
    ("net.java.dev.jna", "jna")                    -> ("5.12.1", "91a814ac4f40d60dee91d842e1a8ad874c62197984403d0e3c30d39e55cf53b3")
  )

  private def sha256(f: File): String =
    MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(f.toPath)).map(b => f"${b & 0xff}%02x").mkString

  def check(report: UpdateReport): UpdateReport = {
    val problems = for {
      config              <- report.configurations
      module              <- config.modules
      (version, expected) <- artifacts.get((module.module.organization, module.module.name)).toList
      problem <-
        if (module.module.revision != version)
          List(Problem.VersionMismatch(module.module, version))
        else
          module.artifacts.flatMap {
            case (a, f) if a.`type` == "jar" =>
              val actual = sha256(f)
              if (actual == expected) Nil else List(Problem.DigestMismatch(module.module, f, actual, expected))
            case _ => Nil
          }
    } yield problem
    if (problems.nonEmpty) throw Rejected(problems.distinct.toList)
    report
  }
}
