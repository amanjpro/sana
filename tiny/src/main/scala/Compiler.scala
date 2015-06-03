package ch.usi.inf.l3.sana.tiny


import ch.usi.inf.l3.sana.tiny
import source.SourceReader
import source.SourceFile
import tiny.parser.Parsers
import tiny.contexts.TreeContexts
import tiny.ast.Trees
import tiny.ast.TreeGen
import tiny.util._
import tiny.settings.Configurations
import tiny.types.Types
import tiny.passes.Phases
import tiny.report._


trait CompilerApi {
  self: Phases with Parsers =>

  def langName: String
  def langVersion: String
  type ConfigType <: Configurations#SanaConfig
  val global: Global
  val configurations: Configurations
  val config: configurations.ConfigType

  lazy val isTest: Boolean = config.isTest

  val standardPhases: List[Phase]

  def sourceReader: SourceReader

  protected def compileUnit(unit: global.CompilationUnit): 
          (Vector[Report], global.CompilationUnit) = {
    standardPhases.foldLeft((Vector.empty[Report], unit))((z, y) => {
      global.logger.info(s"Entered phase ${y.name}")
      val (fst, snd) = z
      if(global.isErroneous(fst)) {
        (fst, global.ErroneousCompilationUnit(snd.state, snd.fileName))
      } else {
        y match {
          case p: TransformerPhase                   => 
            val (w, cu) = p.startPhase(snd)
            (fst ++ w, cu)
          // Not all compiler phases return compilation units, the type checker
          // phase ones, and they are not allowed to change the compilation
          // units. That is why it is safe to return the previous one and pass
          // it to the next one.
          case p: CheckerPhase                       => 
            val w = p.startPhase(snd)
            (fst ++ w, snd)
        }
      }
    })
  }

  protected def compile(cunits: List[global.CompilationUnit]):
    (Vector[Report], List[global.CompilationUnit]) = {
    // TODO: Check if this is still broken
    // FIXME: This is really broken, what about warnings? what about compiling
    // all compilation units and not giving up?
    val (w, _, cs) = cunits.foldLeft((Vector.empty[Report], 
            global.EmptyContext: global.TreeContext,
            Vector.empty[global.CompilationUnit]))((z, y) => {
      val (w, s, cs) = z
      val cunit = y.withState(s)
      val (w2, c2) = compileUnit(cunit)
      (w ++ w2, c2.state, cs ++ Vector(c2))
    })
    (w, cs.toList)
  }

  protected def parse(files: List[String]): List[global.CompilationUnit] = {
    val sources: List[SourceFile] = sourceReader.readSources(files)
    sources.map(parse(_)) 
  }

  def start: (Vector[Report], List[global.CompilationUnit]) = {
    compile(parse(config.files.toList))
  }
}




