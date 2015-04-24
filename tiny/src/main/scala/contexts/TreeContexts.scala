package ch.usi.inf.l3.sana.tiny.contexts

import ch.usi.inf.l3.sana.tiny
import tiny.ast.Trees
import tiny.util.CompilerMonad
import tiny.report._
import tiny.types.Types

import scalaz.{StateT, State, Monoid, Applicative, ReaderWriterStateT}
import scalaz.Scalaz._

trait TreeContexts {
  self: Trees with Types =>

  val compiler = new CompilerMonad {
    type R = List[String]
    type W = Result
    type S = TreeContext
  }

  
  type RWST[V] = compiler.RWST[V]

  type TreeState[T <: Tree] = RWST[T]

  type TypeState[T <: Type] = RWST[T]

  def newRWST[A](f: TreeContext => (TreeContext, A)): RWST[A] = ReaderWriterStateT { 
      (config: compiler.R, oldState: TreeContext) => 
        val (newState, t) = f(oldState)
        Applicative[Id].point((Nil, t, newState))
  }

  def point[A](t: A): RWST[A] = t.point[RWST]

  trait TreeContext {
    def compilationUnits: Map[Int, CompilationUnitContext]


    def getTpe(id: TreeId): Option[Type]
  }

  trait CompilationUnitContext {
    def definedTrees: Map[TreeId, Tree]
  }
  
  object EmptyContext extends TreeContext {
    def compilationUnits: Map[Int, CompilationUnitContext] = Map.empty


    def getTpe(id: TreeId): Option[Type] = None

  }
}




