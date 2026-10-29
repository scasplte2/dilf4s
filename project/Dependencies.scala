import sbt.*

object Dependencies {

  object V {
    val bouncyCastle = "1.86"
    val scodecBits = "1.2.5"
    val catsMtl = "1.7.0"
    val fs2 = "3.14.0"
    val rocksDb = "11.8.1"
    val cats = "2.13.0"
    val catsEffect = "3.7.1"
    val circe = "0.14.17"
    val curve25519 = "0.1.5"
    val levelDb = "0.12"
    val levelDbJni = "1.8"
    val log4cats = "2.8.0"
    val organizeImports = "0.6.0"
    val weaver = "0.13.0"
    val logback = "1.6.6"
    val betterMonadicFor = "0.3.1"
    val ed25519Elisabeth = "0.1.0"
    val curve25519Elisabeth = "0.1.0"
    val kindProjector = "0.13.4"
    val semanticDB = "4.12.7"
  }

  object Libraries {
    val bc = "org.bouncycastle"                   % "bcprov-jdk18on"           % V.bouncyCastle
    val scodecBits = "org.scodec"                %% "scodec-bits"              % V.scodecBits
    val catsMtl = "org.typelevel"                %% "cats-mtl"                 % V.catsMtl
    val fs2Core = "co.fs2"                       %% "fs2-core"                 % V.fs2
    val rocksDb = "org.rocksdb"                   % "rocksdbjni"               % V.rocksDb
    val catsKernelLaws = "org.typelevel"         %% "cats-kernel-laws"         % V.cats
    val cats = "org.typelevel"                   %% "cats-core"                % V.cats
    val catsEffect = "org.typelevel"             %% "cats-effect"              % V.catsEffect
    val catsEffectTestkit = "org.typelevel"      %% "cats-effect-testkit"      % V.catsEffect
    val circeCore = "io.circe"                   %% "circe-core"               % V.circe
    val circeGeneric = "io.circe"                %% "circe-generic"            % V.circe
    val circeParser = "io.circe"                 %% "circe-parser"             % V.circe
    val curve25519 = "com.weavechain"             % "curve25519-elisabeth"     % V.curve25519 % "runtime"
    val weaverCats = "org.typelevel"             %% "weaver-cats"              % V.weaver
    val weaverDiscipline = "org.typelevel"       %% "weaver-discipline"        % V.weaver
    val weaverScalaCheck = "org.typelevel"       %% "weaver-scalacheck"        % V.weaver
    val organizeImports = "com.github.liancheng" %% "organize-imports"         % V.organizeImports
    val levelDb = "org.iq80.leveldb"              % "leveldb"                  % V.levelDb
    val levelDbJni = "org.fusesource.leveldbjni"  % "leveldbjni-all"           % V.levelDbJni
    val log4cats = "org.typelevel"               %% "log4cats-slf4j"           % V.log4cats
    val logback = "ch.qos.logback"                % "logback-classic"          % V.logback
    val ed25519Elisabeth = "cafe.cryptography"    % "ed25519-elisabeth"        % V.ed25519Elisabeth
    val curve25519Elisabeth = "cafe.cryptography" % "curve25519-elisabeth"     % V.curve25519Elisabeth
    val jmhCore = "org.openjdk.jmh"               % "jmh-core"                 % "1.37"
    val jmhGen = "org.openjdk.jmh"                % "jmh-generator-annprocess" % "1.37"
  }

  object CompilerPlugin {

    val betterMonadicFor = compilerPlugin(
      "com.olegpy" %% "better-monadic-for" % V.betterMonadicFor
    )

    val kindProjector = compilerPlugin(
      ("org.typelevel" % "kind-projector" % V.kindProjector).cross(CrossVersion.full)
    )

    val semanticDB = compilerPlugin(
      ("org.scalameta" % "semanticdb-scalac" % V.semanticDB).cross(CrossVersion.full)
    )
  }
}
