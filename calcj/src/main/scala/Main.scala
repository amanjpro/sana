package ch.usi.inf.l3.sana.calcj

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.settings.{SanaConfig,CommandLineArgumentParser}
import tiny.passes.Phases
import tiny.debug.logger
import parser.Parsers
import typechecker.Typers
import java.lang.{System => OS}
import java.io.File

object Main { 
  def processOptions(args: Array[String],
                        ln: String, 
                        lv: String, 
                        fn: String): Either[String, SanaConfig] = {
        val config = new SanaConfig
        val processor = new CommandLineArgumentParser(config, ln, lv, fn)
        if(processor.parser.parse(args)) {
          Right(config)
        } else {
          Left("Bad set of arguments\n" ++ processor.parser.usage)
        }
      }

  def main(args: Array[String]): Unit = {


    val c = processOptions(args, langName, langVersion, 
          tiny.frameworkName) match {
        case Right(config) => config
        case Left(msg)     => 
          println(msg)
          System.exit(1)
          ???  // To satisfy the type checker
      }

    val ln = langName
    val lv = langVersion
    logger.setLevel(c.logLevel)

    val compiler = new Compiler with Parsers with Phases with Typers {
      type G = Global
      type ConfigType = SanaConfig
      val config = c
      val global: G = new Global {
        val isTest: Boolean = config.isTest
      }
      val langName: String = ln
      val langVersion: String = lv
    }
    val (errors, units) = compiler.start
    errors.foreach(Console.err.println)
    Console.err.println("")
    // TODO: Here it should go to codegen
    units.foreach(println)
  }
}
