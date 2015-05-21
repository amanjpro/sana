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
    // TODO: Why we need the Reader bit?
    type R = List[String]
    type W = Failure
    type S = TreeContext
  }

  
  type RWST[V] = compiler.RWST[V]

  type TreeState[T <: Tree] = RWST[T]


  def newRWST[A](f: TreeContext => (TreeContext, A)): RWST[A] = 
    ReaderWriterStateT { 
      (config: compiler.R, oldState: TreeContext) => 
        val (newState, t) = f(oldState)
        Applicative[Id].point((Nil, t, newState))
    }

  def run[A](m: RWST[A], s: TreeContext,
      r: List[String]): (List[Failure], A, TreeContext) = {
    m.run(r, s)
  }

  def point[A](t: A): RWST[A] = t.point[RWST]

  trait TreeContext {
    def compilationUnits: Map[Int, CompilationUnitContext]

    def unit(index: Int): CompilationUnitContext = 
      compilationUnits.get(index) match {
        case Some(cu) => cu
        case None     => MissingUnitContext
      }

    protected def newContext(cus: Map[Int, CompilationUnitContext]): TreeContext
    
    def defines(id: TreeId): Boolean = {
      compilationUnits.get(id.unitId) match {
        case None       => false
        case Some(unit) => unit.defines(id)
      }
    }


    def lookup(id: TreeId): TreeState[IdentifiedTree] //
    // = for {
    //   env     <- compiler.rwst.get
    //   tree    <- env.unit(id.unitId).decls.get(id) match  {
    //                case Some(t) => point(t)
    //                case None    => point(BadTree)
    //              }
    // } yield tree


    def extend(id: Int, 
      unitContext: CompilationUnitContext): TreeContext = {
      require(compilationUnits.get(id) == None, 
        "The index of a Compilation Unit Context must be unique.")
      update(id, unitContext)
    }

    def update(id: Int, 
      unitContext: CompilationUnitContext): TreeContext = {
      // val cuc: CompilationUnitContext = unitContext.extend(id, unitContext)
      newContext(compilationUnits + (id -> unitContext))
    }

    def getTpe(id: TreeId): Option[Type] = for {
      tree <- unit(id.unitId).decls.get(id)
    } yield {
      tree.tpe.run(this)._2
      // val (_, r, _) = run(tree.tpe, Nil, this)
      // r
    }
  }

  trait CompilationUnitContext {
    def decls: Map[TreeId, IdentifiedTree]
    def extend(id: TreeId, tree: IdentifiedTree): CompilationUnitContext
    def update(id: TreeId, tree: IdentifiedTree): CompilationUnitContext =
        extend(id, tree)
    def defines(id: TreeId): Boolean = decls.contains(id)
  }
  
  object MissingUnitContext extends CompilationUnitContext {
    def decls: Map[TreeId, IdentifiedTree] = Map.empty
    def extend(id: TreeId, tree: IdentifiedTree): CompilationUnitContext = 
        MissingUnitContext
  }

  object EmptyContext extends TreeContext {
    lazy val compilationUnits: Map[Int, CompilationUnitContext] = Map.empty
    def lookup(id: TreeId): TreeState[IdentifiedTree] = point(BadTree)
    override def getTpe(id: TreeId): Option[Type] = None
    protected def newContext(
      cus: Map[Int, CompilationUnitContext]): TreeContext = EmptyContext
  }
}




