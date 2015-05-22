package ch.usi.inf.l3.sana.tiny.util



import ch.usi.inf.l3.sana.tiny
import tiny.ast.Trees
import tiny.contexts.TreeContexts
import tiny.contexts.IDGen

import scalaz.Scalaz._
import scalaz.{Name => _, _}


trait CompilationUnits {
  self: Trees with TreeContexts =>


  def toCUState[A <: Tree](t: A): State[TreeContext, A] = t.point[ContextState]
  type CUState = State[TreeContext, Tree]

  trait CompilationUnit {
    def treeState: CUState
    def fileName: String
    val idGen = new IDGen
  }

  trait CompilationUnitFactory {
    class CompilationUnitImpl(val treeState: CUState, 
      val fileName: String) extends CompilationUnit

    def apply(treeState: CUState,
      fileName: String): CompilationUnit = 
      new CompilationUnitImpl(treeState, fileName)
  }


  val CompilationUnit = new CompilationUnitFactory {}
}

