// jmhSettings

organization := "h.chan"

name := "selector"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.0.1"

scalacOptions in ThisBuild ++= Seq("-deprecation")
