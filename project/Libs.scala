import sbt._

object Libs {

  val zioVersion = "2.0.4"

  val libraryDependencies = Seq(
    "org.typelevel" %% "cats-core" % "2.9.0",
    "dev.zio" %% "zio" % zioVersion,
    "dev.zio" %% "zio-test" % zioVersion,
    "dev.zio" %% "zio-test-sbt" % zioVersion,
    "dev.zio" %% "zio-streams" % zioVersion,
    "dev.zio" %% "zio-interop-cats" % "22.0.0.0",
    "org.scalatest" %% "scalatest-wordspec" % "3.2.14" % Test
  )
}