/* basic project info */
name := "numval"

organization := "org.oxland"

version := "0.1.0-SNAPSHOT"

description := "Implicitly abstract numerics for Scala. Enables a single math expression to apply across number types."

homepage := Some(url("https://github.com/kabob/NumVal"))

startYear := Some(2012)

licenses := Seq(
  ("FreeBSD License", url("http://www.freebsd.org/copyright/freebsd-license.html"))
)

scmInfo := Some(
  ScmInfo(
    url("https://github.com/kabob/NumVal.git"),
    "scm:git:https://github.com/kabob/NumVal.git",
    Some("scm:git:git@github.com:kabob/NumVal.git")
  )
)

/* scala versions and options */
scalaVersion := "2.9.2"

crossScalaVersions := Seq(
  // "2.10.0",
  "2.9.3-RC1",
  "2.9.1", "2.9.1-1"
  /* "2.9.0", "2.9.0-1", */
  /* "2.8.0"  "2.8.1", "2.8.2" */
)

offline := false

// scalacOptions <+= scalaVersion { sv =>
//   if (sv.startsWith("2.10")) List(
//     "-feature",
//     // "-language:postfixOps",
//     // "-language:reflectiveCalls",
//     "-language:implicitConversions"
//     // "-language:higherKinds",
//     // "-language:existentials",
//     // "-language:experimental.macros",
//     // "-language:experimental.dynamics",
//   )
//   else Nil
// }

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked"
)

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")

/* dependencies */
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"
)

/* you may need these repos */
resolvers ++= Seq(
  // Resolver.sonatypeRepo("snapshots")
  // Resolver.typesafeIvyRepo("snapshots")
  // Resolver.typesafeIvyRepo("releases")
  // Resolver.typesafeRepo("releases")
  // Resolver.typesafeRepo("snapshots")
  // JavaNet2Repository,
  // JavaNet1Repository,
  // "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

// ivyXML := <dependencies>
//             <exclude module="logback-classic" />
//           </dependencies>

/* testing */
parallelExecution in Test := false

// testOptions += Tests.Argument(TestFrameworks.Specs2, "console", "junitxml")

// parallelExecution in Global := false //no parallelism between subprojects

/* sbt behavior */
logLevel in compile := Level.Warn

traceLevel := 5

/* publishing */
publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) Some(
    "snapshots" at nexus + "content/repositories/snapshots"
  )
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

mappings in (Compile, packageBin) ~= { (ms: Seq[(File, String)]) =>
  ms filter { case (file, toPath) =>
      toPath != "application.conf"
  }
}

publishArtifact in Test := false

// publishArtifact in (Compile, packageDoc) := false

// publishArtifact in (Compile, packageSrc) := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <developers>
    <developer>
      <id>kabob</id>
      <name>Robert Kohlenberger</name>
      <email>kohlenrw@bdumail.com</email>
    </developer>
  </developers>
)

// Josh Suereth's step-by-step guide to publishing on sonatype
// httpcom://www.scala-sbt.org/using_sonatype.html

/* assembly plugin */
assemblySettings

test in AssemblyKeys.assembly := {}
