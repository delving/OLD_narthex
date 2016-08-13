//===========================================================================
//    Copyright 2014 Delving B.V.
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

lazy val root = (project in file(".")).enablePlugins(play.PlayScala)

name := "narthex"

version := "0.3.6-SNAPSHOT"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.webjars" %% "webjars-play" % "2.4.0-2",
  "org.webjars" % "bootstrap" % "3.1.1-2",
  "org.webjars" % "underscorejs" % "1.8.3",
  "org.webjars" % "jquery" % "2.1.1",
  "org.webjars" % "angularjs" % "1.3.17",
  "org.webjars" % "d3js" % "3.4.13",
  "org.webjars" % "nvd3" % "1.1.15-beta-2",
  "org.webjars" % "angularjs-nvd3-directives" % "0.0.7-1",
  "org.webjars" % "angular-file-upload" % "1.6.12",
  "org.webjars" % "angular-ui-bootstrap" % "0.11.2",
  "org.webjars" % "ng-grid" % "2.0.13",
  "org.webjars" % "ngStorage" % "0.3.0",
  "org.webjars" % "angular-sanitize" % "1.3.11",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "de.leanovate.play-mockws" %% "play-mockws" % "2.4.2" % "test",
  "org.scalautils" % "scalautils_2.11" % "2.1.3",
  "com.typesafe.akka" % "akka-testkit_2.11" % "2.4.8" % "test",
  "org.scalatestplus" %% "play" % "1.4.0" % "test",
  "commons-io" % "commons-io" % "2.4",
  "com.rockymadden.stringmetric" %% "stringmetric-core" % "0.27.4",
  "org.apache.poi" % "poi" % "3.10.1",
  "org.apache.poi" % "poi-ooxml" % "3.10.1",
  "org.apache.jena" % "jena-arq" % "2.12.1",
  "org.easybatch" % "easybatch-apache-commons-csv" % "3.0.0",
  "com.typesafe.play" %% "play-mailer" % "4.0.0",
  "eu.delving" % "sip-core" % "1.0.9"
)

libraryDependencies ~= {
  _.map(_.exclude("commons-logging", "commons-logging"))
}

libraryDependencies ~= {
  _.map(_.exclude("org.slf4j", "slf4j-log4j12"))
}

libraryDependencies += cache

libraryDependencies += ws

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += "typesafe" at "http://repo.typesafe.com/typesafe/repo"

resolvers += "Delving" at "http://artifactory.delving.org:8081/artifactory/delving"

resolvers += "Release" at "http://artifactory.delving.org:8081/artifactory/libs-release"

resolvers += "Snapshot" at "http://artifactory.delving.org:8081/artifactory/libs-snapshot"

resolvers += Resolver.file("local-ivy-repo", file(Path.userHome + "/.ivy2/local"))(Resolver.ivyStylePatterns)

publishMavenStyle := true

publishArtifact in(Compile, packageBin) := false

publishArtifact in(Compile, packageDoc) := false

publishArtifact in(Compile, packageSrc) := false

publishTo := Some("DelvingPublish" at "http://artifactory.delving.org/artifactory/delving")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

javaOptions += "-Djava.awt.headless=true"

PlayKeys.fileWatchService := play.runsupport.FileWatchService.sbt(pollInterval.value)
