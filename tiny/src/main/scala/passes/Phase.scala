package ch.usi.inf.l3.sana.tiny.passes



import ch.usi.inf.l3.sana.tiny
import tiny.report._
import tiny.source.Position
import tiny.util.CompilationUnits


trait Phases extends Reporting {
  self: CompilationUnits =>


  trait Phase {
    def startPhase(unit: CompilationUnit): CompilationUnit
  }
}





