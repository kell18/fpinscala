
val commonSettings = Seq(
  scalaVersion := "2.12.7",
  scalacOptions ++= Seq(
    "-Ypartial-unification", // allow the compiler to unify type constructors of different arities
    "-deprecation",          // warn about use of deprecated APIs
    "-feature"               // warn about feature warnings
  )
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

val monocleVersion = "1.5.0" // 1.5.0-cats based on cats 1.0.x
resolvers += Resolver.bintrayRepo("dmbl","dinogroup")


lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.6.0",
      "org.typelevel" %% "cats-effect" % "1.2.0",

      "io.nats" % "java-nats-streaming" % "2.2.3",
      "co.fs2" %% "fs2-core" % "2.0.1",
      "dev.zio" %% "zio" % "1.0.0-RC15",

      "com.github.julien-truffaut" %%  "monocle-core"  % monocleVersion,
      "com.github.julien-truffaut" %%  "monocle-macro" % monocleVersion,
      "com.github.julien-truffaut" %%  "monocle-law"   % monocleVersion % "test"
    )
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )
