name := "evo-final"
scalaVersion := "2.13.3"

val catsVersion = "2.2.0"
val http4sVersion = "0.21.7"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.scalactic" %% "scalactic" % "3.2.2",
  "org.scalatest" %% "scalatest" % "3.2.2" % Test,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion
)
