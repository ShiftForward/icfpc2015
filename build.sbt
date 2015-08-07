import scalariform.formatter.preferences._

name := "icfpc2015"

organization := "eu.shiftforward"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.github.nscala-time"     %% "nscala-time"     % "2.0.0",
  "com.typesafe"                % "config"          % "1.3.0",
  "org.specs2"                 %% "specs2-core"     % "3.6.4"  % "test")

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value.
  setPreference(AlignParameters, true)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:implicitConversions",
  "-language:higherKinds")

Revolver.settings
