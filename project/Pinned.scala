import java.nio.file.Files
import java.security.MessageDigest

import sbt._

/**
 * Artifacts pinned by SHA-256: native code on a consensus path (BN254 through gnark) and what loads it. `update` fails
 * if a pinned module resolves to another version or to different bytes, so neither a repository nor a mirror can
 * substitute them. The bytes come from `vendor/maven` (see its README).
 */
object Pinned {

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
          List(s"${module.module}: pinned at $version")
        else
          module.artifacts.collect {
            case (a, f) if a.`type` == "jar" && sha256(f) != expected =>
              s"${module.module}: ${f.getName} has SHA-256 ${sha256(f)}, pinned $expected"
          }
    } yield problem
    if (problems.nonEmpty) sys.error(s"pinned artifacts do not match:\n  ${problems.distinct.mkString("\n  ")}")
    report
  }
}
