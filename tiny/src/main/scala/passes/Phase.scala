package ch.usi.inf.l3.sana.tiny.passes



import ch.usi.inf.l3.sana.tiny
import tiny.report._
import tiny.ast.Trees
import tiny.source.Position
import tiny.util.CompilationUnits
import tiny.contexts.TreeContexts

trait Phases {
  self: Reporting with 
        Trees with 
        CompilationUnits with 
        TreeContexts =>


  trait Phase {
    type R
    val description: Option[String] = None
    val name: String
    def runRightAfter: Option[String] = None
    def runAfter: List[String] = runRightAfter.toList
    def runBefore: List[String] = Nil
    def startPhase(unit: CompilationUnit): R
    def processOptions(options: List[String]): Unit = ()
  }

  trait TransformerPhase extends Phase {
    type R = (Vector[Failure], CompilationUnit)
  }

  trait CheckerPhase extends Phase {
    type R = Vector[Failure]
  }
}







