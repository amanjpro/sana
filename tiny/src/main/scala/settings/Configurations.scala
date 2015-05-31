package ch.usi.inf.l3.sana.tiny.settings

import scopt.OptionParser

trait Configurations {



  class SanaConfig {
    /**
      * @group Testing and Debugging
      */
    private[this] var printTrees_ : Option[String] = None
    def printTrees: Option[String] = this.printTrees_
    protected[Configurations] def printTrees_=(v: Option[String]): Unit =
      this.printTrees_ = v

    /**
      * @group Testing and Debugging
      */
    private[this] var isTest_ : Boolean = false
    def isTest: Boolean = this.isTest_
    protected[Configurations] def isTest_=(v: Boolean): Unit =
      this.isTest_ = v

    /**
      * @group Testing and Debugging
      */
    private[this] var isVerbose_ : Boolean = false
    def isVerbose: Boolean = this.isVerbose_
    protected[Configurations] def isVerbose_=(v: Boolean): Unit =
      this.isVerbose_ = v


    /**
      * @group Plugins
      */
    private[this] var plugins_ : Vector[String] = Vector()
    def plugins: Vector[String] = this.plugins_
    protected[Configurations] def plugins_=(v: Vector[String]): Unit =
      this.plugins_ = v

    /**
      * @group Compilation Options
      */
    private[this] var classpath_ : Vector[String] = Vector()
    def classpath: Vector[String] = this.classpath_
    protected[Configurations] def classpath_=(v: Vector[String]): Unit =
      this.classpath_ = v

    /**
      * @group Compilation Options
      */
    private[this] var files_ : Vector[String] = Vector()
    def files: Vector[String] = this.files_
    protected[Configurations] def files_=(v: Vector[String]): Unit =
      this.files_ = v

    /**
      * @group Compilation Options
      */
    private[this] var destination_ : Option[String] = None
    def destination: Option[String] = this.destination_
    protected[Configurations] def destination_=(v: Option[String]): Unit =
      this.destination_ = v
  }

  type ConfigType <: SanaConfig

  class CommandLineArgumentParser(val config: ConfigType,
                                  val langName: String,
                                  val langVersion: String,
                                  val fmName: String) {

    def parser = new OptionParser[Unit]("scopt") {
        head(langName, langVersion)
        opt[Unit]("Stest") action { case _ =>
          config.isTest = true
        } text(s"To active testing mode for $fmName")
        opt[Unit]('v', "verbose") action { case _ =>
          config.isVerbose = true
        } text(s"Set verbose flag to $fmName")
        opt[Seq[String]]("SPlugin") action { case (plugins, _) =>
          config.plugins = config.plugins ++ plugins
        } valueName("<plugin1>, <plugin2>, ...") text(
          "Comma seperated plugin names to be used.")
        opt[String]('d', "destination") action { case (dest, _) =>
          config.destination = Some(dest)
        } text(s"Set the destination directory for the compiled classes")
        arg[String]("<file>...") required() unbounded() action { case (f, _) =>
          config.files = config.files ++ Vector(f) 
        } text("Unbounded filenames or paths to compile")
        opt[Seq[String]]("classpath") abbr("cp") action { case (cp, _) =>
          config.classpath = config.classpath ++ cp
          // config.copy(libName = k, maxCount = v)
        } valueName("<cp1>:<cp2>, ...") text(
          "Colon seperated classpath paths.")
        help("help") text("prints this usage text")
        // opt[Boolean]("-SPlugin") action { (x, c) =>
          // config.copy(foo = x) } text("To active testing mode for Sana")
      }
  }


  def processOptions(args: Array[String],
                  ln: String, 
                  lv: String, 
                  fn: String): Either[String, ConfigType]
}
