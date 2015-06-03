package ch.usi.inf.l3.sana.tiny.passes



import ch.usi.inf.l3.sana.tiny
import tiny.Global
import tiny.report.Failure

trait Phases {

  type G <: Global
  val global: G


  trait Phase {
    type R
    val description: Option[String] = None
    val name: String
    def runRightAfter: Option[String] = None
    def runAfter: List[String] = runRightAfter.toList
    def runBefore: List[String] = Nil
    def startPhase(unit: global.CompilationUnit): R
    def processOptions(options: List[String]): Unit = ()
  }

  trait TransformerPhase extends Phase {
    type R = (Vector[Failure], global.CompilationUnit)
  }

  trait CheckerPhase extends Phase {
    type R = Vector[Failure]
  }
}
