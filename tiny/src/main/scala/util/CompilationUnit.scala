package ch.usi.inf.l3.sana.tiny.util



import ch.usi.inf.l3.sana.tiny
import tiny.ast.Trees
import tiny.contexts.TreeContexts
import tiny.contexts.IDGen


trait CompilationUnits {
  self: Trees with TreeContexts =>


  trait CompilationUnit {
    def treeState: TreeState[Tree]
    def fileName: String
    val idGen = new IDGen

  }

  trait CompilationUnitFactory {
    class CompilationUnitImpl(val treeState: TreeState[Tree], 
      val fileName: String) extends CompilationUnit

    def apply(treeState: TreeState[Tree], 
      fileName: String): CompilationUnit = 
      new CompilationUnitImpl(treeState, fileName)
  }


  val CompilationUnit = new CompilationUnitFactory {}
}

