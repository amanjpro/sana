package ch.usi.inf.l3.sana.tiny.util



import ch.usi.inf.l3.sana.tiny
import tiny.ast.Trees
import tiny.contexts._

import scalaz.Scalaz._
import scalaz.{Name => _, _}


trait CompilationUnits {
  self: Trees with TreeContexts =>


  // def toCUState[A <: Tree](t: A): State[TreeContext, A] = t.point[ContextState]
  // type CUState = State[TreeContext, Tree]

  trait CompilationUnit {
    def tree: Tree
    def state: TreeContext
    def id: Int
    // def treeState: CUState
    def fileName: String

    def withState(state: TreeContext): CompilationUnit
    override def toString: String = s"compilation unit for $fileName"
  }

  trait CompilationUnitFactory {
    class CompilationUnitImpl(val id: Int, val tree: Tree, 
      val state: TreeContext,
      val fileName: String) extends CompilationUnit {


      def withState(st: TreeContext): CompilationUnit = 
        apply(id, tree, st, fileName)
    }

    def apply(id: Int, tree: Tree, state: TreeContext,
      fileName: String): CompilationUnit = 
      new CompilationUnitImpl(id, tree, state, fileName)

  }


  trait ErroneousCompilationUnitFactory {
    class ErroneousCompilationUnitImpl(val state: TreeContext,
              val fileName: String) extends CompilationUnit {
      override def toString: String = 
        s"erroneous compilation unit for $fileName"
      val tree: Tree = Empty
      val id: Int = NO_COMPILATION_UNIT_ID
      def withState(st: TreeContext): CompilationUnit = {
        apply(st, fileName)
      }
    }

    def apply(state: TreeContext,
      fileName: String): CompilationUnit = 
      new ErroneousCompilationUnitImpl(state, fileName)
  }

  val CompilationUnit = new CompilationUnitFactory {}
  val ErroneousCompilationUnit = new ErroneousCompilationUnitFactory {}
}

