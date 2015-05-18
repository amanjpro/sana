package ch.usi.inf.l3.sana.tiny.passes



import ch.usi.inf.l3.sana.tiny
import tiny.report._
import tiny.ast.Trees
import tiny.source.Position
import tiny.util.CompilationUnits
import tiny.contexts.TreeContexts


trait Phases extends Reporting {
  self: Trees with CompilationUnits with TreeContexts =>


  trait Phase {
    def startPhase(unit: CompilationUnit): CompilationUnit
  }
}





