name := "higher-order-substitution"

version := "0.1"

scalaVersion := "2.12.10"

val `higher-order-substitution` = (project in file("."))
  .settings(libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.1.2" % "test"
  ))