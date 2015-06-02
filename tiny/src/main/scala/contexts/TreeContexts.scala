package ch.usi.inf.l3.sana.tiny.contexts

import ch.usi.inf.l3.sana.tiny
import tiny.ast.Trees
import tiny.util.MonadUtils
import tiny.report._
import tiny.types.Types

import scalaz.{StateT, State, Monoid, Applicative, ReaderWriterStateT}
import scalaz.Scalaz._

/**
  * A trait that contains types that represent context of a tree in  the
  * `tiny` language.
  *
  * @groupdesc Tree\ Contexts                         
  * @groupdesc Compilation\ Units\ Contexts     
  *
  * @author Amanj Sherwany
  * @since 0.1
  * @version 0.1
  */
trait TreeContexts {
  self: Trees with Types with MonadUtils =>

  

  //////////////////////////////////////////////////////////////////
  // Contexts
  //////////////////////////////////////////////////////////////////
  /**
    * The base trait for tree contexts.
    *
    * @group Tree Contexts
    */
  trait TreeContext {
    /** A unique id generator */
    protected val idGen: IDGen 

    /**
      * A map from ids to the [[CompilationUnitContext]]s that they 
      * represent.
      * 
      * Each [[CompilationUnitContext]] in `Sana` is assigned a unique 
      * number to it to be distinguishable from all other unit contexts.
      *
      * @see [[CompilationUnitContext]]
      * @return A map from integer ids to compilation units.
      */
    def compilationUnits: Map[Int, CompilationUnitContext]

    /**
      * Given an id of a compilation unit, returns the compilation unit context 
      * that has the given id.
      *
      * @see [[CompilationUnitContext]]
      * @param index The id of the compilation unit
      * @return The compilation unit that has the given id. If there is none with
      *         this id, [[MissingUnitContext]] is returned.
      */
    def unit(index: Int): CompilationUnitContext = 
      compilationUnits.get(index) match {
        case Some(cu) => cu
        case None     => MissingUnitContext
      }

    /**
      * Factory method for creating a new context
      * 
      * @param idGen An id generator
      * @param cus a dictionary of already defined compilation unit contexts
      * @return a newly instanciated tree context, with the given idGen and 
      *         compilation unit contexts.
      */
    protected def newContext(idGen: IDGen,
        cus: Map[Int, CompilationUnitContext]): TreeContext
    
    /**
      * Checks if any tree with the given [[TreeId]] is already defined by
      * this context.
      *
      * @see [[contexts.TreeId]]
      * @param id The id that we want to check
      * @return True if the id is defined, and false otherwise
      */
    def defines(id: TreeId): Boolean = {
      compilationUnits.get(id.unitId) match {
        case None       => false
        case Some(unit) => unit.defines(id)
      }
    }

    /**
      * In the current context, looks up for a [[ast.Trees#IdentifiedTree]] that has
      * the given name.
      *
      * @see [[contexts.TreeId]]
      * @param name The name of the [[ast.Trees#IdentifiedTree]]
      * @return Optionally the id of the [[ast.Trees#IdentifiedTree]]
      */
    def lookup(name: String): Option[TreeId] //
    // = for {
    //   env     <- compiler.rwst.get
    //   tree    <- env.unit(id.unitId).decls.get(id) match  {
    //                case Some(t) => point(t)
    //                case None    => point(BadTree)
    //              }
    // } yield tree


    /**
      * Extends this context with a given [[CompilationUnitContext]].
      *
      * @see [[CompilationUnitContext]]
      * @param unitContext The compilation unit context to be added to this context
      * @return A new instance of tree context with the given compilation unit context
      *         added to it.
      */
    def extend(unitContext: CompilationUnitContext): TreeContext = {
      newContext(idGen, compilationUnits + (idGen.getNextId -> unitContext))
    }

    /**
      * Updates the bindings of the given id with a given 
      * [[CompilationUnitContext]].
      *
      * @see [[CompilationUnitContext]]
      * @param id The id of the compilation unit context to be updated
      * @param unitContext The compilation unit context to be updated
      * @return A new instance of tree context with the mapping of the given
      *         id updated to the given compilation context. In case the binding
      *         of the id was missing, `this` will be returned.
      */
    def update(id: Int, 
      unitContext: CompilationUnitContext): TreeContext = {
      compilationUnits.get(id) match {
        case None =>
          this
        case _    =>
          newContext(idGen, compilationUnits + (id -> unitContext))
      }
    }

    /**
      * Returns the type of the tree that has the given id
      *
      * @see [[types.Types.Type]]
      * @see [[contexts.TreeId]]
      * @param id the id of the tree which we want to query its type.
      * @return Optionally the type of the tree that has the given id.
      *         In case the current scope has no bindings for the given
      *         id, None is returned.
      */
    def getTpe(id: TreeId): Option[Type] = for {
      tree <- unit(id.unitId).decls.get(id)
    } yield {
      tree.tpe.run(this)._2
      // val (_, r, _) = run(tree.tpe, Nil, this)
      // r
    }
  }

  /**
    * This object represents an empty context for tree. This means it doesn't
    * define anything.
    *
    * @group Tree Contexts
    */
  object EmptyContext extends TreeContext {
    // We should never use this field
    protected lazy val idGen: IDGen = ???
    lazy val compilationUnits: Map[Int, CompilationUnitContext] = Map.empty
    def lookup(name: String): Option[TreeId] = None
    override def getTpe(id: TreeId): Option[Type] = None
    protected def newContext(idGen: IDGen,
      cus: Map[Int, CompilationUnitContext]): TreeContext = EmptyContext
  }

  /**
    * A trait to represent Compilation Unit Contexts. 
    *
    * @group Compilation Unit Contexts
    */
  trait CompilationUnitContext {
    /**
      * A map from ids to the [[ast.Trees#IdentifiedTree]]s that they 
      * represent.
      * 
      * @return A map from [[contexts.TreeId]] to AST trees.
      */
    def decls: Map[TreeId, IdentifiedTree]

    /**
      * Extends this context with a given [[ast.Trees#IdentifiedTree]].
      *
      * @param id the id of the tree to be added, needs to be unique
      * @param tree The compilation unit context to be added to this context
      * @return A new instance of [[CompilationUnitContext]] with the given
      *         tree added to it.
      */
    def extend(id: TreeId, tree: IdentifiedTree): CompilationUnitContext
    
    /**
      * Updates the bindings of the given id with a given 
      * [[ast.Trees#IdentifiedTree]].
      *
      * @param id The id of the tree to be updated
      * @param tree The tree to be updated
      * @return A new instance of compilation unit context with the 
      *         mapping of the given id updated to the given tree.
      *         In case the binding of the id was missing, `this` will be
      *         returned.
      */
    def update(id: TreeId, tree: IdentifiedTree): CompilationUnitContext =
        extend(id, tree)
    /**
      * Checks if any tree with the given [[TreeId]] is already defined by
      * this context.
      *
      * @see [[contexts.TreeId]]
      * @param id The id that we want to check
      * @return True if the id is defined, and false otherwise
      */
    def defines(id: TreeId): Boolean = decls.contains(id)
  }

  /**
    * An object to represent missing compilation units. 
    *
    * @group Compilation Unit Contexts
    */
  object MissingUnitContext extends CompilationUnitContext {
    def decls: Map[TreeId, IdentifiedTree] = Map.empty
    def extend(id: TreeId, tree: IdentifiedTree): CompilationUnitContext = 
        MissingUnitContext
  }
}




