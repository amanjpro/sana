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
      "/Users/amanj/Documents/PhD/MyWork/Programming/SanaFM/jdk1.0/lib/classes/"
    val f1: Future[ClassPathCatalog] = Future {
        new ClassPathCatalog(List(new JFile(cp)))
    }

    val f2: Future[ClassPathCatalog] = Future {
        new ClassPathCatalog(List(new JFile(cp)))
    }

    val r = for {
      t1 <- f1
      t2 <- f2
    } yield (t1.defines("java.lang.Exception"),
             t2.defines("java.lang.Object"))

    r onSuccess {
      case r => println(r)
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
      // .defines("java.lang.Exception")

    val reader = new ClassFileParsers with Trees with Types 
                      with TreeContexts with Definitions with
                      MonadUtils with Constants with TreeInfos {
      val classPaths: List[JFile] = List(
        new JFile(cp)
      )

      
        
      


            val global: ooj.Global = new ooj.Global {
        val isTest = true
      }
    }

    // // val cr = new ClassReader("java.lang.String")
    // val cr = new ClassReader("
    // cr.accept(reader, 0)
    val clazz = reader.loadClass("java.lang.Boolean")


    println(clazz)
  }
}






