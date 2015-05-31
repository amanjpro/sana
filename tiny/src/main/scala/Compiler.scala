package ch.usi.inf.l3.sana.tiny


import ch.usi.inf.l3.sana.tiny
import source.SourceReader
import source.SourceFile
import tiny.parser.Parsers
import tiny.contexts.TreeContexts
import tiny.ast.Trees
import tiny.ast.TreeGen
import tiny.util._
import tiny.types.Types
import tiny.passes.Phases
import tiny.report._
import scopt.OptionParser


trait CompilerApi {
  self: Parsers with 
        CompilationUnits with 
        Phases with 
        TreeContexts with
        Reporting with
        MonadUtils =>


  val config: SanaConfig
  val fmName: String = "Sana"
  def langName: String
  def langVersion: String

  def commandLineArgumentProcessing(c: SanaConfig) = 
    new OptionParser[Unit]("scopt") {
      head(langName, langVersion)
      opt[Boolean]("Stest") action { case _ =>
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
  val standardPhases: List[Phase]

  def sourceReader: SourceReader

  protected def compileUnit(unit: CompilationUnit): 
          (Vector[Failure], CompilationUnit) = {
    standardPhases.foldLeft((Vector.empty[Failure], unit))((z, y) => {
      val (fst, snd) = z
      if(isErroneous(fst)) {
        (fst, ErroneousCompilationUnit(snd.state, snd.fileName))
      } else {
        y match {
          case p: TransformerPhase      => 
            val (w, cu) = p.startPhase(snd)
            (fst ++ w, cu)
          // Not all compiler phases return compilation units,
          // the ones that won't are checker phases, and they
          // are not allowed to change the compilation units.
          // that is why it is safe to return the previous one
          // and pass it to the next one.
          case p: CheckerPhase          => 
            val w = p.startPhase(snd)
            (fst ++ w, snd)
        }
      }
    })
  }

  protected def compile(cunits: List[CompilationUnit]):
    (Vector[Failure], List[CompilationUnit]) = {
    // FIXME: This is really broken, what about warnings? what about compiling
    // all compilation units and not giving up?
    val (w, _, cs) = cunits.foldLeft((Vector.empty[Failure], 
            EmptyContext: TreeContext,
            Vector.empty[CompilationUnit]))((z, y) => {
      val (w, s, cs) = z
      val cunit = y.withState(s)
      val (w2, c2) = compileUnit(cunit)
      (w ++ w2, c2.state, cs ++ Vector(c2))
    })
    (w, cs.toList)
  }

  protected def parse(files: List[String]): List[CompilationUnit] = {
    val sources: List[SourceFile] = sourceReader.readSources(files)
    sources.map(parse(_)) 
  }

  def start(files: List[String]): (Vector[Failure], List[CompilationUnit]) = {
    compile(parse(files))
  }
}




