package ch.usi.inf.l3.sana.calcj

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.settings.Configurations
import tiny.passes.Phases
import parser.Parsers
import typechecker.Typers

object Main { 
  def main(args: Array[String]): Unit = {
    val cnfgs = new Configurations {
      type ConfigType = SanaConfig
      def processOptions(args: Array[String],
                        ln: String, 
                        lv: String, 
                        fn: String): Either[String, ConfigType] = {
        val config = new SanaConfig
        val processor = new CommandLineArgumentParser(config, ln, lv, fn)
        if(processor.parser.parse(args)) {
          Right(config)
        } else {
          Left("Bad set of arguments\n" ++ processor.parser.usage)
        }
      }
    }


    val c: cnfgs.ConfigType = 
      cnfgs.processOptions(args, langName, langVersion, 
          tiny.frameworkName) match {
        case Right(config) => config
        case Left(msg)     => 
          println(msg)
          System.exit(1)
          ???  // To satisfy the type checker
      }

    val ln = langName
    val lv = langVersion
    val compiler = new Compiler with Parsers with Phases with Typers {
      type G = Global
      val global: G = new Global {
        val isTest: Boolean = c.isTest
      }
      val configurations: cnfgs.type = cnfgs
      val config: cnfgs.ConfigType = c
      val langName: String = ln
      val langVersion: String = lv
    }
    val (errors, units) = compiler.start
    errors.foreach(println)
    // TODO: Here it should go to codegen
    units.foreach(println)
  }
}
