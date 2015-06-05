package ch.usi.inf.l3.sana.tiny.contexts

import ch.usi.inf.l3.sana.tiny
import tiny.ast.Trees
import tiny.util.MonadUtils
import tiny.report._
import tiny.names.Name
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
    protected def idGen: IDGen

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
    protected def compilationUnits: Map[Int, CompilationUnitContext]

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
     * @param owner The owner of this name, if the tree is top-level 
     *              then the owner is None
     * @return Optionally the id of the [[ast.Trees#IdentifiedTree]]
     */
    def lookup(name: Name, owner: Option[TreeId]): Option[TreeId]

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
     * Returns the name of the tree that has the given id
     *
     * @see [[contexts.TreeId]]
     * @param id the id of the tree which we want to query its name.
     * @return Optionally the name of the tree that has the given id.
     *         In case the current scope has no bindings for the given
     *         id, None is returned.
     */
    def getName(id: TreeId): Option[Name] = for {
      tree <- unit(id.unitId).lookup(id)
    } yield {
      tree.name
      // val (_, r, _) = run(tree.tpe, Nil, this)
      // r
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
      tree <- unit(id.unitId).lookup(id)
    } yield {
      tree.tpe.run(this)._2
      // val (_, r, _) = run(tree.tpe, Nil, this)
      // r
    }
  }

  /**
   * This object represents an empty context for trees. 
   * 
   * This means it doesn't (and cannot) define tree definitions.
   *
   * @group Tree Contexts
   */
  case object EmptyContext extends TreeContext {
    // this field should never be used
    protected def idGen: IDGen = ???
    lazy val compilationUnits: Map[Int, CompilationUnitContext] = Map.empty
    def lookup(name: Name, id: Option[TreeId]): Option[TreeId] = None
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
    protected def idGen: IDGen

    /**
     * A map from ids to the [[ast.Trees#IdentifiedTree]]s that they 
     * represent.
     * 
     * @return A map from [[contexts.TreeId]] to AST trees.
     */
    protected def decls: Map[TreeId, IdentifiedTree]

    /**
     * Deletes the tree that has `id`, and all its children from this context
     *
     * @param id the id of the tree to be deleted, needs to be unique
     * @return a new compilation unit context with `id` and all 
     *         its children removed
     */
    def delete(id: TreeId): CompilationUnitContext

    /**
     * Extends this context with a given [[ast.Trees#IdentifiedTree]].
     *
     * @param id the id of the tree to be added, needs to be unique
     * @param tree The compilation unit context to be added to this context
     * @return A new instance of [[CompilationUnitContext]] with the given
     *         tree added to it. In case id is already already dfeined,
     *         `this` is returned.
     */
    def extend(id: TreeId, tree: IdentifiedTree): CompilationUnitContext =
      defines(id) match {
        case true   => this
        case false  => insert(id, tree)
      }

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
      defines(id) match {
        case false => this
        case true  => insert(id, tree)
      }

    /**
     * Inserts the bindings of the given id with a given 
     * [[ast.Trees#IdentifiedTree]].
     *
     * @param id The id of the tree to be updated/added
     * @param tree The tree to be updated/added
     * @return A new instance of compilation unit context with the 
     *         mapping of the given id updated/added to the given tree.
     */
    protected def insert(id: TreeId, 
      tree: IdentifiedTree): CompilationUnitContext

    /**
     * Checks if any tree with the given [[TreeId]] is already defined by
     * this context.
     *
     * @see [[contexts.TreeId]]
     * @param id The id that we want to check
     * @return True if the id is defined, and false otherwise
     */
    def defines(id: TreeId): Boolean = decls.contains(id)

    /**
     * Looks up for a tree with the given [[TreeId]]
     *
     * @see [[contexts.TreeId]]
     * @param id The id go be looked up
     * @return optionally the tree that has the given `id`
     */
    def lookup(id: TreeId): Option[IdentifiedTree] = 
      decls.get(id)

  }

  /**
   * An object to represent missing compilation units. 
   *
   * @group Compilation Unit Contexts
   */
  case object MissingUnitContext extends CompilationUnitContext {
    // this field should never be used
    protected def idGen: IDGen = ???
    protected def decls: Map[TreeId, IdentifiedTree] = Map.empty
    protected def insert(id: TreeId, 
      tree: IdentifiedTree): CompilationUnitContext = 
        MissingUnitContext
    def delete(id: TreeId): CompilationUnitContext =
      MissingUnitContext
  }

  private class CompilationUnitContextImpl(
    protected val idGen: IDGen,
    protected val decls: Map[TreeId, IdentifiedTree]) 
    extends CompilationUnitContext {
    
    def insert(id: TreeId, tree: IdentifiedTree): CompilationUnitContext = 
      new CompilationUnitContextImpl(idGen, decls + (id -> tree))

    def delete(id: TreeId): CompilationUnitContext =
      new CompilationUnitContextImpl(idGen, decls - id)
  }

  /**
   * A factory method for creating empty compilation unit contexts.
   *
   * Every time this method is called, a new instance is returned.
   * @group Factory Methods
   * @return a new instance of CompilationUnitContext.
   */
  def compilationUnitContext: CompilationUnitContext = 
    new CompilationUnitContextImpl(new IDGen, Map.empty)


  private class TreeContextImpl(val idGen: IDGen,
    val compilationUnits: Map[Int, CompilationUnitContext]) 
    extends TreeContext {

    protected def newContext(idGen: IDGen,
        cus: Map[Int, CompilationUnitContext]): TreeContext = 
      new TreeContextImpl(idGen, cus)

    // TODO: Implement this, and its logic
    def lookup(name: Name, id: Option[TreeId]): Option[TreeId] = None
  }


  /**
   * A factory method for creating empty tree contexts.
   *
   * Every time this method is called, a new instance is returned.
   * @group Factory Methods
   * @return a new instance of TreeContext.
   */
  def treeContext: TreeContext = 
    new TreeContextImpl(new IDGen, Map.empty)


}




