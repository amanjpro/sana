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

    def apply(t: TreeState[Tree], f: String): CompilationUnit = 
      new CompilationUnitImpl(t, f)
  }


  val CompilationUnit = new CompilationUnitFactory {}
}

