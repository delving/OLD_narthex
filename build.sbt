import com.typesafe.sbt.packager.docker._

//===========================================================================
//    Copyright 2014, 2015, 2016 Delving B.V.
//
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
//===========================================================================

lazy val root = (project in file(".")).
  enablePlugins(play.sbt.PlayScala).
  enablePlugins(SbtWeb).
  enablePlugins(DockerPlugin).
  enablePlugins(GitVersioning).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "buildinfo"
  )
name := "narthex"

scalaVersion := "2.11.8"

buildInfoKeys ++= Seq[BuildInfoKey](
  resolvers,
  libraryDependencies in Test,
  "gitCommitSha" -> git.gitHeadCommit.value.getOrElse("nogit").substring(0, 5)
)

libraryDependencies ++= Seq(
  "org.webjars" %% "webjars-play" % "2.5.0-3",
  "org.webjars" % "bootstrap" % "3.1.1-2",
  "org.webjars" % "underscorejs" % "1.8.3",
  "org.webjars" % "jquery" % "2.1.1",
  "org.webjars" % "angularjs" % "1.3.17",
  "org.webjars" % "angular-file-upload" % "2.0.5",
  "org.webjars" % "angular-ui-bootstrap" % "0.11.2",
  "org.webjars" % "ng-grid" % "2.0.13",
  "org.webjars" % "ngStorage" % "0.3.0",
  "org.webjars" % "angular-sanitize" % "1.3.11",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "de.leanovate.play-mockws" %% "play-mockws" % "2.4.2" % "test",
  "org.scalautils" % "scalautils_2.11" % "2.1.3",
  "com.typesafe.akka" % "akka-testkit_2.11" % "2.4.8" % "test",
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % "test",
  "org.mockito" % "mockito-core" % "2.3.7" % "test",
  "commons-io" % "commons-io" % "2.4",
  "com.rockymadden.stringmetric" %% "stringmetric-core" % "0.27.4",
  "org.apache.poi" % "poi" % "3.10.1",
  "org.apache.poi" % "poi-ooxml" % "3.10.1",
  "org.apache.jena" % "jena-arq" % "3.1.0" exclude("log4j", "log4j"),
  "org.slf4j" % "log4j-over-slf4j" % "1.7.21",
  "org.easybatch" % "easybatch-apache-commons-csv" % "3.0.0",
  "com.typesafe.play" %% "play-mailer" % "5.0.0",
  "eu.delving" % "sip-core" % "1.2.0",
  "de.threedimensions" %% "metrics-play" % "2.5.13",
  "com.getsentry.raven" % "raven-logback" % "7.6.0" % "runtime",
  "nl.grons" %% "metrics-scala" % "3.5.5_a2.3",
  "org.coursera" % "metrics-datadog" % "1.1.6",
  "com.softwaremill.retry" %% "retry" % "0.3.0"
)

// Configure the steps of the asset pipeline (used in stage and dist tasks)
// rjs = RequireJS, uglifies, shrinks to one file, replaces WebJars with CDN
// digest = Adds hash to filename
// gzip = Zips all assets, Asset controller serves them automatically when client accepts them
pipelineStages := Seq(rjs, digest, gzip) // for processing static artifacts

// RequireJS with sbt-rjs (https://github.com/sbt/sbt-rjs#sbt-rjs)
// ~~~
RjsKeys.paths += ("jsRoutes" -> ("/narthex/jsRoutes" -> "empty:"))

RjsKeys.optimize := "none"

libraryDependencies ~= {
  _.map(_.exclude("commons-logging", "commons-logging"))
}

libraryDependencies ~= {
  _.map(_.exclude("org.slf4j", "slf4j-log4j12"))
}

libraryDependencies += cache

libraryDependencies += ws

resolvers += Resolver.mavenLocal

resolvers += "typesafe" at "http://repo.typesafe.com/typesafe/repo"

resolvers += Resolver.jcenterRepo

resolvers += "Delving" at "https://maven.pkg.github.com/delving/sip-creator"

resolvers += Resolver.file("local-ivy-repo", file(Path.userHome + "/.ivy2/local"))(Resolver.ivyStylePatterns)

publishMavenStyle := true

publishArtifact in(Compile, packageBin) := false

publishArtifact in(Compile, packageDoc) := false

publishArtifact in(Compile, packageSrc) := false

/* publishTo := Some("DelvingPublish" at "http://artifactory.delving.org/artifactory/delving") */

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

javaOptions += "-Djava.awt.headless=true"

PlayKeys.fileWatchService := play.runsupport.FileWatchService.sbt(pollInterval.value)

routesGenerator := InjectedRoutesGenerator

// sbt will generate a Dockerfile from the instructions below.
packageName in Docker := "delvingplatform/narthex"

dockerCommands := Seq(
  Cmd("FROM", "frolvlad/alpine-oraclejdk8:slim"),
  Cmd("MAINTAINER", "info@delving.eu"),
  Cmd("RUN", "apk update && apk add bash"),
  Cmd("WORKDIR", "/opt/docker"),
  Cmd("ADD", "opt /opt"),
  Cmd("RUN", "chown -R daemon:daemon ."),
  Cmd("EXPOSE", "9000"),
  Cmd("USER", "daemon"),
  ExecCmd("ENTRYPOINT", "bin/narthex", "-Dconfig.file=/opt/docker/conf/overrides.conf", "-Dlogger.file=/opt/docker/conf/logback.xml")
)

// Scala Compiler Options
scalacOptions in ThisBuild ++= Seq(
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-deprecation", // warning and location for usages of deprecated APIs
  "-feature", // warning and location for usages of features that should be imported explicitly
  "-unchecked", // additional warnings where generated code depends on assumptions
  "-Xlint", // recommended additional warnings
  "-Xcheckinit", // runtime error when a val is not initialized due to trait hierarchies (instead of NPE somewhere else)
  "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
  //"-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
  "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures
  "-Ywarn-dead-code", // Warn when dead code is identified
  "-Ywarn-unused", // Warn when local and private vals, vars, defs, and types are unused
  "-Ywarn-numeric-widen" // Warn when numerics are widened
)
