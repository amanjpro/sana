package ch.usi.inf.l3.sana.calcj

import ch.usi.inf.l3.sana.tiny
import tiny.settings.Configurations

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

    val ln: String = "calcj"
    val lv: String = "1.0"

    val c: cnfgs.ConfigType = 
      cnfgs.processOptions(args, ln, lv, "Sana") match {
        case Right(config) => config
        case Left(msg)     => 
          println(msg)
          System.exit(1)
          ???
      }

    val compiler = new Compiler {
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
