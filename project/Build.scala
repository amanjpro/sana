import sbt._
import Keys._
import com.simplytyped.Antlr4Plugin._
import sbtassembly.Plugin._
import AssemblyKeys._


object SharedSettings {
  def sourceURL(proj: String, branch: String = "master"): String = 
    s"https://github.com/amanjpro/sana/blob/$branch/$proj€{FILE_PATH}.scala#L1"
  val buildSettings  = antlr4Settings ++ Defaults.defaultSettings ++ Seq(
    version := "0.1-SNAPSHOT",
    organization := "ch.usi.inf.l3",
    scalaVersion := "2.11.5",
    apiURL := Some(url("http://sana.github.io")),
    exportJars := true,
    javaSource in Antlr4 := (javaSource in Compile).value,
    antlr4GenListener in Antlr4 := false,
    antlr4GenVisitor in Antlr4 := true,
    scalacOptions ++= Seq("-unchecked", "-deprecation", 
      "-feature", "-Xlint", "-Xfatal-warnings"), 
    scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits",
         "-diagrams", "-no-prefixes", "-author", "-explaintypes",
         "-language:implicitConversions,higherKinds"),
    // Help SBT to find Api docs for libraries that don't provide any in
    // their artifacts
    apiMappings ++= {
      val cp: Seq[Attributed[File]] = (fullClasspath in Compile).value
      def findManagedDependency(organization: String, name: String): File = {
        ( for {
            entry <- cp
            module <- entry.get(moduleID.key)
            if module.organization == organization
            if module.name.startsWith(name)
            jarFile = entry.data
          } yield jarFile
        ).head
      }
      Map(
          findManagedDependency("org.scalaz", "scalaz-core") -> 
               url("http://docs.typelevel.org/api/scalaz/stable/7.0.2/doc/")
      )
    },
    autoAPIMappings := true,
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
    base = file("tiny"),
    settings = buildSettings ++ Seq(
      scalacOptions in (Compile, doc) <++= (baseDirectory in LocalProject("tiny")).map {
        bd => Seq("-sourcepath", bd.getAbsolutePath, 
          "-doc-source-url", sourceURL("tiny"))
      },
      antlr4PackageName in Antlr4 := Some("ch.usi.inf.l3.sana.tiny.antlr"))
  ) 

  lazy val calcj = Project(
    id   = "calcj",
    base = file("calcj")
  ) settings (
    scalacOptions in (Compile, doc) <++= (baseDirectory in LocalProject("calcj")).map {
      bd => Seq("-sourcepath", bd.getAbsolutePath, "-doc-source-url", sourceURL("calcj"))
    },
    artifact in (Compile, assembly) ~= { art =>
      art.copy(`classifier` = Some("assembly"))
    }
  ) settings (buildSettings ++ assemblySettings ++ Seq(
      antlr4PackageName in Antlr4 := Some("ch.usi.inf.l3.sana.calcj.antlr") 
    ) ++ addArtifact(artifact in (Compile, assembly), 
      assembly) : _*) dependsOn(tiny)

  lazy val primj = Project(
    id   = "primj",
    base = file("primj"),
    settings = buildSettings ++ Seq(
      scalacOptions in (Compile, doc) <++= (baseDirectory in LocalProject("primj")).map {
        bd => Seq("-sourcepath", bd.getAbsolutePath, "-doc-source-url", sourceURL("primj"))
      },
      antlr4PackageName in Antlr4 := Some("ch.usi.inf.l3.sana.primj.antlr"))
  ) dependsOn(calcj)
}
