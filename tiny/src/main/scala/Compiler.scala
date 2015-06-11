package ch.usi.inf.l3.sana.tiny


import ch.usi.inf.l3.sana.tiny
import source.SourceReader
import source.SourceFile
import tiny.parser.Parsers
import tiny.contexts.TreeContexts
import tiny.ast.Trees
import tiny.debug.logger
import tiny.ast.TreeGen
import tiny.util._
import tiny.settings.SanaConfig
import tiny.types.Types
import tiny.passes.Phases
import tiny.report._


trait CompilerApi {
  self: Phases with Parsers =>

  def langName: String
  def langVersion: String
  type ConfigType <: SanaConfig
  val global: Global
  val config: ConfigType

  lazy val isTest: Boolean = config.isTest

  val standardPhases: List[Phase]

  def sourceReader: SourceReader

  protected def compileUnit(context: global.Context,
          unit: global.CompilationUnit): 
          (Vector[Report], global.CompilationUnit, global.Context) = {
    standardPhases.foldLeft((Vector.empty[Report], unit, 
            context))((z, y) => {
      logger.info(s"Entered phase ${y.name}")
      val (report, unit, ctx) = z
      if(global.isErroneous(report)) {
        (report, global.ErroneousCompilationUnit(unit.fileName), ctx)
      } else {
        y match {
          case p: TransformerPhase                   => 
            val (w, cu, ctx2) = p.startPhase(ctx, unit)
            (report ++ w, cu, ctx2)
          // Not all compiler phases return compilation units, the type checker
          // phase ones, and they are not allowed to change the compilation
          // units. That is why it is safe to return the previous one and pass
          // it to the next one.
          case p: CheckerPhase                       => 
            val w = p.startPhase(ctx, unit)
            (report ++ w, unit, ctx)
        }
      }
    })
  }

  protected def compile(cunits: List[global.CompilationUnit]):
    (Vector[Report], List[global.CompilationUnit]) = {
    // TODO: Check if this is still broken
    // FIXME: This is really broken, what about warnings? what about compiling
    // all compilation units and not giving up?
    val (w, cs, _) = cunits.foldLeft((Vector.empty[Report], 
            Vector.empty[global.CompilationUnit],
            global.Context()))((z, y) => {
      val (w, cs, ctx) = z
      val (w2, u2, ctx2) = compileUnit(ctx, y)
      (w ++ w2, cs ++ Vector(u2), ctx2)
    })
    (w, cs.toList)
  }

  protected def parse(files: List[String]): List[global.CompilationUnit] = {
    val sources: List[SourceFile] = sourceReader.readSources(files)
    sources.map(parse(_)) 
  }



  def attach(units: List[global.CompilationUnit]): 
      (Vector[Report], List[global.CompilationUnit]) = {
    compile(units)
  }

  def start: (Vector[Report], List[global.CompilationUnit]) = {
    compile(parse(config.files.toList))
  }
}




