import sbt._
import Keys._


object SharedSettings {
  val buildSettings  = Defaults.defaultSettings ++ Seq(
    version := "0.1-SNAPSHOT",
    organization := "ch.usi.inf.l3",
    scalaVersion := "2.11.1",
    libraryDependencies ++= 
      List("org.scalatest" % "scalatest_2.11" % "2.2.4" % "test")

  )
}

object build extends Build {

  import SharedSettings._

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = buildSettings,
    aggregate = Seq(tiny, calcj, primj)
  )

  lazy val calcj = Project(
    id   = "calcj",
    settings = buildSettings,
    base = file("calcj")
  ) dependsOn(tiny)

  lazy val tiny = Project(
    id   = "tiny",
    settings = buildSettings,
    base = file("tiny")
  ) 


  lazy val primj = Project(
    id   = "primj",
    settings = buildSettings,
    base = file("primj")
  ) dependsOn(calcj)
}
