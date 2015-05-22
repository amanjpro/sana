package ch.usi.inf.l3.sana.tiny.util



import ch.usi.inf.l3.sana.tiny
import tiny.ast.Trees
import tiny.contexts.TreeContexts
import tiny.contexts.IDGen

import scalaz.Scalaz._
import scalaz.{Name => _, _}


trait CompilationUnits {
  self: Trees with TreeContexts =>


  // def toCUState[A <: Tree](t: A): State[TreeContext, A] = t.point[ContextState]
  // type CUState = State[TreeContext, Tree]

  trait CompilationUnit {
    def tree: Tree
    def state: TreeContext
    // def treeState: CUState
    def fileName: String
    val idGen = new IDGen
  }

  trait CompilationUnitFactory {
    class CompilationUnitImpl(val tree: Tree, val state: TreeContext,
      val fileName: String) extends CompilationUnit

    def apply(tree: Tree, state: TreeContext,
      fileName: String): CompilationUnit = 
      new CompilationUnitImpl(tree, state, fileName)
  }


  val CompilationUnit = new CompilationUnitFactory {}
}

