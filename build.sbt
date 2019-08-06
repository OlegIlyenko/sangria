name := "sangria"
organization := "org.sangria-graphql"
version := "1.4.3-SNAPSHOT"

description := "Scala GraphQL implementation"
homepage := Some(url("http://sangria-graphql.org"))
licenses := Seq("Apache License, ASL Version 2.0" → url("http://www.apache.org/licenses/LICENSE-2.0"))

scalaVersion := "2.13.0"
crossScalaVersions := Seq("2.11.11", "2.12.7", scalaVersion.value)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-Xlint:-missing-interpolator,_")

scalacOptions ++= {
  if (scalaVersion.value startsWith "2.11")
    Seq("-target:jvm-1.7")
  else
    Seq.empty
}

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oF")

libraryDependencies ++= Seq(
  // AST Parser
  "org.parboiled" %% "parboiled" % "2.1.7",

  // AST Visitor
  "org.sangria-graphql" %% "macro-visit" % "0.1.2-SNAPSHOT",

  // Marshalling
  "org.sangria-graphql" %% "sangria-marshalling-api" % "2.0.0-SNAPSHOT",

  // Streaming
  "org.sangria-graphql" %% "sangria-streaming-api" % "1.0.1-SNAPSHOT",

  // Macros
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,

  // Testing
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.sangria-graphql" %% "sangria-marshalling-testkit" % "1.0.3-SNAPSHOT" % Test,
  "org.sangria-graphql" %% "sangria-spray-json" % "1.0.3-SNAPSHOT" % Test,
  "org.sangria-graphql" %% "sangria-argonaut" % "1.0.2-SNAPSHOT" % Test,
  "org.sangria-graphql" %% "sangria-ion" % "1.0.1-SNAPSHOT" % Test,
  "eu.timepit" %% "refined" % "0.9.9" % Test,

  // CATs
  "net.jcazevedo" %% "moultingyaml" % "0.4.1" % Test,
  "io.github.classgraph" % "classgraph" % "4.0.6" % Test
)

// Publishing

publishMavenStyle := true
publishArtifact in Test := false
pomIncludeRepository := (_ ⇒ false)
publishTo := Some(
  if (isSnapshot.value)
    "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  else
    "releases" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")

startYear := Some(2015)
organizationHomepage := Some(url("https://github.com/sangria-graphql"))
developers := Developer("OlegIlyenko", "Oleg Ilyenko", "", url("https://github.com/OlegIlyenko")) :: Nil
scmInfo := Some(ScmInfo(
  browseUrl = url("https://github.com/sangria-graphql/sangria.git"),
  connection = "scm:git:git@github.com:sangria-graphql/sangria.git"
))

// nice *magenta* prompt!

shellPrompt in ThisBuild := { state ⇒
  scala.Console.MAGENTA + Project.extract(state).currentRef.project + "> " + scala.Console.RESET
}
