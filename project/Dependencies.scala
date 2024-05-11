import sbt.*

object Dependencies {

  object V {
    val cats = "2.9.0"
    val catsEffect = "3.4.2"
    val circe = "0.14.6"
    val derevo = "0.13.0"
    val levelDb = "0.12"
    val levelDbJni = "1.8"
    val log4cats = "2.5.0"
    val organizeImports = "0.5.0"
    val scrypto = "2.3.0"
    val weaver = "0.8.1"
  }

  object Libraries {
    val cats = "org.typelevel" %% "cats-core" % V.cats
    val catsEffect = "org.typelevel" %% "cats-effect" % V.catsEffect
    val catsEffectTestkit = "org.typelevel" %% "cats-effect-testkit" % V.catsEffect
    val circeCore = "io.circe" %% "circe-core" % V.circe
    val circeGeneric = "io.circe" %% "circe-generic" % V.circe
    val circeParser = "io.circe" %% "circe-parser" % V.circe
    val derevoCore = "tf.tofu" %% s"derevo-core" % V.derevo
    val derevoCirce = "tf.tofu" %% s"derevo-circe-magnolia" % V.derevo
    val weaverCats = "com.disneystreaming" %% "weaver-cats" % V.weaver
    val weaverDiscipline = "com.disneystreaming" %% "weaver-discipline" % V.weaver
    val weaverScalaCheck = "com.disneystreaming" %% "weaver-scalacheck" % V.weaver
    val organizeImports = "com.github.liancheng" %% "organize-imports" % V.organizeImports
    val scrypto = "org.scorexfoundation" %% "scrypto" % V.scrypto
    val levelDb = "org.iq80.leveldb" % "leveldb" % V.levelDb
    val levelDbJni = "org.fusesource.leveldbjni" % "leveldbjni-all" % V.levelDbJni
    val log4cats = "org.typelevel" %% "log4cats-slf4j" % V.log4cats
  }

  object CompilerPlugin {

    val betterMonadicFor = compilerPlugin(
      "com.olegpy" %% "better-monadic-for" % "0.3.1"
    )

    val kindProjector = compilerPlugin(
      ("org.typelevel" % "kind-projector" % "0.13.2").cross(CrossVersion.full)
    )

    val semanticDB = compilerPlugin(
      ("org.scalameta" % "semanticdb-scalac" % "4.7.1").cross(CrossVersion.full)
    )
  }
}
