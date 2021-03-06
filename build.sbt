import sbt.compilerPlugin


name := "eval"

scalaVersion := "2.12.12"

val derivingVersion = "3.0.0-M2"

lazy val eval: Project = (project in file(".")
  settings (resolvers ++= Seq(
    Resolver.sonatypeRepo("releases")
  ))

  settings(libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
    // the @deriving and @xderiving plugin and macro
    "org.scalaz"    %% "deriving-macro"              % derivingVersion,
    "org.scalaz"    %% "scalaz-deriving-magnolia"    % derivingVersion,
    "org.scalactic" %% "scalactic"                   % "3.0.8" % "test",
    "org.scalatest" %% "scalatest"                   % "3.0.8" % "test",
    compilerPlugin("org.scalaz" %% "deriving-plugin" % derivingVersion cross CrossVersion.full),
  ))
)

val jetty = "9.4.19.v20190610"

inThisBuild(List(
  organization := "com.github.stacycurl",
  homepage     := Some(url("https://github.com/stacycurl/eval")),
  licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers   := List(
    Developer("stacycurl", "Stacy Curl", "stacy.curl@gmail.com", url("https://github.com/stacycurl"))
  ),
  usePgpKeyHex("pimpathon ci"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
))
