package ch.usi.inf.l3.sana.tiny.passes



import ch.usi.inf.l3.sana.tiny
import tiny.report._
import tiny.source.Position
import tiny.util.CompilationUnit


trait Phase {


  val TYPE_MISMATCH  = "Type mismatch"
  val UNEXPETED_TREE = "Unexpected tree"
  var result: Result = Success
  
  def startPhase(unit: CompilationUnit): CompilationUnit


  def error(msg: String, found: String, required: String,
    pos: Option[Position],
    t: Any): Unit = 
    result = result ++ Failure(msg)
}

