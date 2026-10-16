ThisBuild / scalaVersion := "3.3.7"

lazy val root = project
  .in(file("."))
  .settings(
    name := "MoMo3",

    // Run app in a dedicated JVM so interactive key input is delivered to the app.
    Compile / run / fork := true,
    Compile / run / connectInput := true,

    // --- SemanticDB (Scalafix + IntelliJ) ---
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,

    scalacOptions ++= Seq(
      "-Wunused:imports",
      "-Wunused:all"
    ),

    // --- Main dependencies ---
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.5.13",
      "com.github.pureconfig" %% "pureconfig-core" % "0.17.10",
      "com.github.pureconfig" %% "pureconfig-generic-scala3" % "0.17.10",
      "org.typelevel" %% "log4cats-core" % "2.8.0",
      "org.typelevel" %% "log4cats-slf4j" % "2.8.0",
      "org.jline" % "jline" % "3.29.0",
      "net.java.dev.jna" % "jna" % "5.14.0",
      "org.scala-lang" %% "scala3-compiler" % "3.3.7"
      ////> using dependency org.scala-lang::scala3-compiler:3.3.7

    ),

    // --- Tests ---
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit-scalacheck" % "1.3.0" % Test,
      "org.typelevel" %% "cats-effect-testkit" % "3.7.0" % Test,
      "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
    )
  )
