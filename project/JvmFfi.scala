import scala.sys.process.{Process, ProcessLogger}
import scala.util.Try

import sbt._
import sbt.util.Logger

/**
 * The JVM's native edwards25519 library (`sdk/rust/jvm-ffi`: Ed25519 and ECVRF verification), compiled by cargo for this
 * machine and bundled in the crypto jar where `Edwards25519Native` looks for it:
 * `xyz/kd5ujc/crypto/signing/native/<os>-<arch>/`. The platform names here and in `Edwards25519Native.Platform` must agree.
 *
 * The Rust side is pinned: the toolchain (`sdk/rust/rust-toolchain.toml`), the dependencies (`--locked`), one codegen
 * unit, and the checkout's and cargo's paths remapped, so the library embeds no machine's paths and two builds on one
 * machine are byte-identical. The link is not: it uses the host's C toolchain and libc, so builders with different ones
 * produce different bytes, and the library needs a glibc at least as new as its builder's. A release artifact needs a
 * pinned build image (and one per platform); this build is for the machine that runs it and for CI.
 */
object JvmFfi {

  val crate = "dilf4s-jvm-ffi"
  val profile = "jvm-ffi"
  val library = "dilf4s_jvm_ffi"

  /** `(os, arch)` as `Edwards25519Native.Platform` names them, for the platforms with a build. */
  def platform: Option[(String, String)] = {
    val os = sys.props.getOrElse("os.name", "").toLowerCase match {
      case "linux"                                        => Some("linux")
      case n if n.startsWith("mac os x") || n == "darwin" => Some("darwin")
      case _                                              => None
    }
    val arch = sys.props.getOrElse("os.arch", "") match {
      case "amd64" | "x86_64"  => Some("x86_64")
      case "aarch64" | "arm64" => Some("aarch64")
      case _                   => None
    }
    for {
      o <- os
      a <- arch
    } yield (o, a)
  }

  def fileName(os: String): String = if (os == "darwin") s"lib$library.dylib" else s"lib$library.so"

  /** Whether a Rust target triple builds for this `(os, arch)`. */
  def matches(triple: String, os: String, arch: String): Boolean =
    triple.startsWith(s"$arch-") && (if (os == "darwin") triple.endsWith("-apple-darwin") else triple.contains("-linux-"))

  /** Runs `command` in `dir`, logging stderr (cargo's progress) and returning stdout's lines; fails on a non-zero exit. */
  private def run(command: Seq[String], dir: File, env: Seq[(String, String)], log: Logger): Vector[String] = {
    val out = Vector.newBuilder[String]
    val exit = Try(Process(command, dir, env: _*).!(ProcessLogger(line => out += line, log.info(_)))).fold(
      e => sys.error(s"$crate: cannot run ${command.head} (${e.getMessage}); install rustup, which reads sdk/rust/rust-toolchain.toml"),
      identity
    )
    if (exit != 0) sys.error(s"$crate: `${command.mkString(" ")}` failed with exit code $exit")
    out.result()
  }

  private val Filename = """"filenames":\["([^"]+)"""".r.unanchored

  /**
   * Runs cargo in `rustDir` and copies the library under `out`; nothing on a platform without a build. The target is the
   * pinned toolchain's host triple, named explicitly so no `CARGO_BUILD_TARGET` or `build.target` can redirect the build,
   * and the library's path is the one cargo reports for this build, so a stale file is never bundled.
   */
  def build(rustDir: File, out: File, log: Logger): Seq[File] =
    platform match {
      case None =>
        log.warn(s"$crate: no build for ${sys.props("os.name")} on ${sys.props("os.arch")}; Edwards25519Native will not load")
        Nil
      case Some((os, arch)) =>
        val host = run(Seq("rustc", "-vV"), rustDir, Nil, log).collectFirst { case l if l.startsWith("host: ") => l.stripPrefix("host: ") }
        val triple = host.getOrElse(sys.error(s"$crate: `rustc -vV` names no host triple"))
        if (!matches(triple, os, arch)) sys.error(s"$crate: rustc's host $triple does not build for this JVM ($os, $arch)")
        val cargoHome = sys.env.get("CARGO_HOME").map(file).getOrElse(file(sys.props("user.home")) / ".cargo")
        // CARGO_ENCODED_RUSTFLAGS separates flags with 0x1f, so a path may hold spaces; it replaces any RUSTFLAGS
        val rustFlags = Seq(
          s"--remap-path-prefix=${rustDir.getAbsolutePath}=/dilf4s/sdk/rust",
          s"--remap-path-prefix=${cargoHome.getAbsolutePath}=/cargo"
        ).mkString("\u001f")
        val command = Seq(
          "cargo",
          "build",
          "--locked",
          "--profile",
          profile,
          "-p",
          crate,
          "--target",
          triple,
          "--target-dir",
          (rustDir / "target").getAbsolutePath,
          "--message-format=json-render-diagnostics"
        )
        val artifacts = run(command, rustDir, Seq("CARGO_ENCODED_RUSTFLAGS" -> rustFlags), log).collect {
          case l @ Filename(path) if l.contains("\"reason\":\"compiler-artifact\"") && l.contains(s""""name":"$library"""") => file(path)
        }
        val built = artifacts match {
          case Vector(f) if f.getName == fileName(os) && f.isFile => f
          case other => sys.error(s"$crate: cargo reported ${other.mkString(", ")}, not one ${fileName(os)}")
        }
        val bundled = out / "xyz" / "kd5ujc" / "crypto" / "signing" / "native" / s"$os-$arch" / fileName(os)
        IO.copyFile(built, bundled)
        Seq(bundled)
    }
}
