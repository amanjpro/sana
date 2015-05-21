import sbt._
import Keys._
import com.simplytyped.Antlr4Plugin._
import sbtassembly.Plugin._
import AssemblyKeys._


object SharedSettings {
  val buildSettings  = antlr4Settings ++ Defaults.defaultSettings ++ Seq(
    version := "0.1-SNAPSHOT",
    organization := "ch.usi.inf.l3",
    scalaVersion := "2.11.5",
    exportJars := true,
    javaSource in Antlr4 := (javaSource in Compile).value,
    antlr4GenListener in Antlr4 := false,
    antlr4GenVisitor in Antlr4 := true,
    scalacOptions ++= Seq("-unchecked", "-deprecation", 
      "-feature", "-Xlint", "-Xfatal-warnings"), 
    libraryDependencies ++= 
      List("org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
            "org.ow2.asm" % "asm-all" % "5.0.3", 
            "org.scalaz" %% "scalaz-core" % "7.1.1")
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

  lazy val tiny = Project(
    id   = "tiny",
    settings = buildSettings ++ Seq(
      antlr4PackageName in Antlr4 := Some("ch.usi.inf.l3.sana.tiny.antlr")),
    base = file("tiny")
  ) 

  lazy val calcj = Project(
    id   = "calcj",
    base = file("calcj")
  ) settings (
    artifact in (Compile, assembly) ~= { art =>
      art.copy(`classifier` = Some("assembly"))
    }
  ) settings (buildSettings ++ assemblySettings ++ Seq(
      antlr4PackageName in Antlr4 := Some("ch.usi.inf.l3.sana.calcj.antlr") 
    ) ++ addArtifact(artifact in (Compile, assembly), 
      assembly) : _*) dependsOn(tiny)

  lazy val primj = Project(
    id   = "primj",
    settings = buildSettings ++ Seq(
      antlr4PackageName in Antlr4 := Some("ch.usi.inf.l3.sana.primj.antlr")),
    base = file("primj")
  ) dependsOn(calcj)
}
