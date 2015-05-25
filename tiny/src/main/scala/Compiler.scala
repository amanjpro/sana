package ch.usi.inf.l3.sana.tiny


import ch.usi.inf.l3.sana.tiny
import source.SourceReader
import source.SourceFile
import tiny.parser.Parsers
import tiny.contexts.TreeContexts
import tiny.ast.Trees
import tiny.ast.TreeGen
import tiny.util.CompilationUnits
import tiny.util.MonadUtils
import tiny.types.Types
import tiny.passes.Phases
import tiny.report._


trait CompilerApi {
  self: Parsers with 
        CompilationUnits with 
        Phases with 
        TreeContexts with
        MonadUtils =>


  val phases: List[Phase]

  def sourceReader: SourceReader
  protected def compileUnit(unit: CompilationUnit): 
          (Vector[Failure], CompilationUnit) = {
    phases.foldLeft((Vector.empty[Failure], unit))((z, y) => {
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




