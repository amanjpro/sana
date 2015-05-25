package ch.usi.inf.l3.sana.tiny.contexts

import ch.usi.inf.l3.sana.tiny
import tiny.ast.Trees
import tiny.util.MonadUtils
import tiny.report._
import tiny.types.Types

import scalaz.{StateT, State, Monoid, Applicative, ReaderWriterStateT}
import scalaz.Scalaz._

trait TreeContexts {
  self: Trees with Types with MonadUtils =>

  type ContextState[A] = State[TreeContext, A]
  

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


    def lookup(name: String): Option[TreeId] //
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
    def lookup(name: String): Option[TreeId] = None
    override def getTpe(id: TreeId): Option[Type] = None
    protected def newContext(
      cus: Map[Int, CompilationUnitContext]): TreeContext = EmptyContext
  }
}




