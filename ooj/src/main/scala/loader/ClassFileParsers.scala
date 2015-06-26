package ch.usi.inf.l3.sana.ooj.loader



import ch.usi.inf.l3.sana
import sana.tiny
import sana.ooj
import sana.primj

import tiny.io.ClassPathCatalog
import tiny.util.MonadUtils

import primj.contexts.{TreeContexts, TreeInfos}

import ooj.io.ClassFileParsers
import ooj.Global
import ooj.util.Definitions
import ooj.types.Types
import ooj.ast.{Trees, Constants}


import org.objectweb.asm._
import java.lang.{ClassLoader, Class => JClass}
import java.io.{File => JFile}


import scala.concurrent._
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

object Main {
  def main(args: Array[String]): Unit = {
    val cp = 
      "/Users/amanj/Documents/PhD/MyWork/Programming/SanaFM/jdk1.0/lib/classes2/"

    val f1: Future[ClassPathCatalog] = Future {
      new ClassPathCatalog(List(new JFile(cp)))
    }

    val f2: Future[ClassPathCatalog] = Future {
      val catalog = new ClassPathCatalog(List(new JFile(cp)))
      println(catalog.catalog)
      catalog
    }

    val r = for {
      t1 <- f1
      t2 <- f2
    } yield (t1.defines("java", false),
             t2.defines("java.lang.Object", true))

    r onSuccess {
      case r => println(r + "KLJLJ")
    }

    r onFailure {
      case r => println(r + "KLJLJ")
    }

    // f1 onComplete {
    //   case Success(tasks)    =>
    //     val r = for {
    //       t   <- tasks
    //     } yield t.defines("java.lang.Exception")
    //     println(r)
    //   case Failure(t)        => println(t)
    //
    // }
    //   .defines("java.lang.Exception")
    //
    // val reader = new ClassFileParsers with Trees with Types 
    //                   with TreeContexts with Definitions with
    //                   MonadUtils with Constants with TreeInfos
    //

    val compiler: ooj.Global = new ooj.Global with ClassFileParsers 
                                with Trees with Types with TreeContexts
                                with Definitions with MonadUtils with
                                Constants with TreeInfos {
      val classPaths: List[JFile] = List(
        new JFile(cp)
      )
      val isTest = true
    }

    // // val cr = new ClassReader("java.lang.String")
    // val cr = new ClassReader("
    // cr.accept(reader, 0)
    val clazz = compiler.loadClass("java.lang.Boolean")


    println(clazz)
  }
}






