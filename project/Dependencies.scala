import sbt.*

object Dependencies {

  object V {
    val bouncyCastle = "1.70"
    val cats = "2.9.0"
    val catsEffect = "3.4.2"
    val circe = "0.14.6"
    val levelDb = "0.12"
    val levelDbJni = "1.8"
    val log4cats = "2.5.0"
    val organizeImports = "0.5.0"
    val weaver = "0.8.1"
    val logback = "1.3.5"
    val betterMonadicFor = "0.3.1"
    val kindProjector = "0.13.2"
    val semanticDB = "4.8.15"
  }

  object Libraries {
    val bc = "org.bouncycastle" % "bcprov-jdk15on" % V.bouncyCastle
    val cats = "org.typelevel" %% "cats-core" % V.cats
    val catsEffect = "org.typelevel" %% "cats-effect" % V.catsEffect
    val catsEffectTestkit = "org.typelevel" %% "cats-effect-testkit" % V.catsEffect
    val circeCore = "io.circe" %% "circe-core" % V.circe
    val circeGeneric = "io.circe" %% "circe-generic" % V.circe
    val circeParser = "io.circe" %% "circe-parser" % V.circe
    val weaverCats = "com.disneystreaming" %% "weaver-cats" % V.weaver
    val weaverDiscipline = "com.disneystreaming" %% "weaver-discipline" % V.weaver
    val weaverScalaCheck = "com.disneystreaming" %% "weaver-scalacheck" % V.weaver
    val organizeImports = "com.github.liancheng" %% "organize-imports" % V.organizeImports
    val levelDb = "org.iq80.leveldb" % "leveldb" % V.levelDb
    val levelDbJni = "org.fusesource.leveldbjni" % "leveldbjni-all" % V.levelDbJni
    val log4cats = "org.typelevel" %% "log4cats-slf4j" % V.log4cats
    val logback = "ch.qos.logback" % "logback-classic" % V.logback
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
