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
   * The base trait for all contexts.
   *
   * @group Tree Contexts
   */
  trait Context {
    /** A unique id generator */
    protected def idGen: IDGen

    /**
     * A map from ids to [[Context]]s.
     * 
     * Every instance of [[Context]] in `Sana` is assigned a unique *id* to it
     * to be distinguishable from all other instances.
     */
    protected def decls: Map[TreeId, Context]

    /**
     * Factory method for creating a new context
     * 
     * @param idGen An id generator
     * @param decls a dictionary of already defined members of this context
     * @return a newly instantiated tree context, with the given idGen and 
     *         context.
     */
    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context

    /**
     * Deletes the tree that has `id`, and all its children from this context
     *
     * @param id the id of the tree to be deleted, needs to be unique
     * @return a new compilation unit context with `id` and all 
     *         its children removed
     */
    def delete(id: TreeId): Context = id match {
      case NoId                => this
      case _: SimpleId         => newContext(idGen, decls - id)
      case _: CompositeId      =>
        val ctx = decls.get(id)
        ctx match {
          case None            => this
          case Some(ctx)       =>
            val newCtx = ctx.delete(id.forward)
            update(id, newCtx)
        }
    }

    /**
     * This thread-safe method to generate the next `id` in a context.
     *
     * @param owner The id of the context that we want to have its next id.
     * @return an *id* that is different from all the values that are produced
     *         so far by the context with the given `id`.
     */
    def nextId(owner: TreeId): TreeId = {
      if(owner == NoId) TreeId(NoId, idGen.nextId)
      else {
        decls.get(owner) match {
          case None                          => 
            // FIXME
            TreeId(NoId, idGen.nextId)
          case Some(ctx) if owner.isSimple   => 
            TreeId(owner, ctx.idGen.nextId)
          case Some(ctx)                     =>
            ctx.nextId(owner.forward)
        }
      }
    }

    /**
     * Checks if there is a binding for the given [[TreeId]] id.
     *
     * @see [[contexts.TreeId]]
     * @param id The id that we want to check
     * @return True if the id is defined, and false otherwise
     */
    def defines(id: TreeId): Boolean = {
      lazy val r = decls.get(id) match {
        case None                                => false
        case _: NamedContext if id.isSimple      => true
        case c: Context                          => id match {
          case _: SimpleId    => true
          case _              => c.defines(id.forward)
        }
      }

      id != NoId && r
    }

    /**
     * In the current context, looks up for a [[ast.Trees#NamedTree]] that has
     * the given name.
     *
     * @see [[contexts.TreeId]]
     * @param name The name of the [[ast.Trees#NamedTree]]
     * @param owner The owner of this name, if the tree is top-level 
     *              then the owner is the id of the compilation unit
     * @return The id of the [[ast.Trees#NamedTree]], or `NoId` if not found
     */
    def lookup(name: Name, owner: TreeId): TreeId = ???

    /**
     * Extends this context with a given [[Context]].
     *
     * @param decl The declaration to be added to this context
     * @return A tuple of the id of the new declaration and a new instance of
     *         a context with the given declaration added to it.
     */
    def extend(owner: TreeId, decl: Context): (TreeId, Context) = {
      owner match {
        case NoId        => 
          val id = TreeId(owner, idGen.nextId)
          val ctx = newContext(idGen, decls + (id -> decl))
          (id, ctx)
        case _          => decls.get(owner) match {
          case None          => (NoId, this)
          case Some(ctx)     =>
            val (id, ctx2) = ctx.extend(owner.forward, decl)
            val ctx3       = update(owner, ctx2)
            (id, ctx3)
        }
      }
    }

    /**
     * Updates the bindings of the given id with a [[tiny.ast.Trees#Tree]]
     *
     * Note that this method only performs if the *id* points to a 
     * [[TreeContexts#NamedContext]]
     *
     * @param id The id of the context to be updated
     * @param tree The new tree of the context
     * @return A new instance of this context with the mapping of the given
     *         id updated to the given tree. In case the binding of the id
     *         was missing, `this` will be returned.
     */
    def update(id: TreeId, tree: Tree): Context = {
      decls.get(id) match {
        case None                                     =>
          this
        case Some(ctx: NamedContext) if id.isSimple   =>
          val ctx2 = ctx.updateTree(tree)
          update(id, ctx2)
        case Some(ctx)                                =>
          val ctx2 = ctx.update(id.forward, tree)
          update(id, ctx2)
      }
    }
    /**
     * Updates the bindings of the given id with a new [[Context]].
     *
     * @param id The id of the context to be updated
     * @param bind The new value of the context
     * @return A new instance of this context with the mapping of the given
     *         id updated to the given context. In case the binding of the id
     *         was missing, `this` will be returned.
     */
    def update(id: TreeId, 
      bind: Context): Context = {
      decls.get(id) match {
        case None =>
          this
        case _    =>
          newContext(idGen, decls + (id -> bind))
      }
    }

    /**
     * Get the tree bound to the given `id`
     *
     * @param id the id of the tree
     * @return optionally returns the tree that is bound to this id
     */
    def getTree(id: TreeId): Option[Tree] = id match {
      case NoId              => None
      case _: SimpleId       => decls.get(id) match {
        case c: NamedContext        => Some(c.tree)
        case _                      => None
      }
      case _: CompositeId    => 
        decls.get(id).flatMap(_.getTree(id.forward)) 
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
      tree <- getTree(id)
      r    <- tree match {
        case n: NamedTree => Some(n.name)
        case _            => None
      }
    } yield r

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
      tree <- getTree(id)
    } yield {
      tree.tpe.run(this)._2
    }
  }

  /**
   * The base trait for contexts that represent a named tree
   *
   * @group Tree Contexts
   */
  trait NamedContext extends Context {
    def tree: Tree
    def updateTree(tree: Tree): NamedContext
  }
  

  /**
   * A class to represent a block of contexts, this can be
   * a programming block, or a compilation unit or any other
   * nameless context
   *
   * @group Compilation Unit Contexts
   */
  class BlockContext(protected val idGen: IDGen, 
    protected val decls: Map[TreeId, Context] = Map.empty) extends Context {

    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context = 
        new BlockContext(idGen, binds)
  }


  class AtomicContext(val tree: Tree) extends NamedContext {
    final protected val decls: Map[TreeId, Context] = Map.empty
    final protected def idGen: IDGen = ???
    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context = this
    def updateTree(tree: Tree): NamedContext = new AtomicContext(tree)
    override def defines(id: TreeId): Boolean = false
    override def lookup(name: Name, owner: TreeId): TreeId = NoId
    override def extend(owner: TreeId, decl: Context): (TreeId, Context) = 
      (NoId, this)
    override def update(id: TreeId, bind: Context): Context = this
    override def getTree(id: TreeId): Option[Tree] = None
    override def getName(id: TreeId): Option[Name] = None
    override def getTpe(id: TreeId): Option[Type] = None
    override def delete(id: TreeId): Context = this
  }

  class MethodContext(val tree: Tree, protected val idGen: IDGen, 
    val decls: Map[TreeId, Context] = Map.empty) extends NamedContext {
    
    
    def updateTree(tree: Tree): NamedContext = 
      new MethodContext(tree, idGen, decls)
    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context = 
        new MethodContext(tree, idGen, binds)
  }
  


  /**
   * An object to represent missing compilation units. 
   *
   * @group Compilation Unit Contexts
   */
  case object InvalidContext extends Context {
    // this field should never be used
    protected def idGen: IDGen = ???
    protected def decls: Map[TreeId, Context] = Map.empty
    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context = InvalidContext
    override def defines(id: TreeId): Boolean = false
    override def lookup(name: Name, owner: TreeId): TreeId = NoId
    override def extend(owner: TreeId, decl: Context): (TreeId, Context) = 
      (NoId, InvalidContext)
    override def update(id: TreeId, bind: Context): Context = InvalidContext
    override def getTree(id: TreeId): Option[Tree] = None
    override def getName(id: TreeId): Option[Name] = None
    override def getTpe(id: TreeId): Option[Type] = None
    override def delete(id: TreeId): Context =
      InvalidContext
  }

  private class ContextImpl(protected val idGen: IDGen,
    val decls: Map[TreeId, Context]) extends Context {
    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context = 
        new ContextImpl(idGen, binds)
  }

  object Context {
    def apply(idGen: IDGen, decls: Map[TreeId, Context]): Context = 
      new ContextImpl(idGen, decls)
    def apply(idGen: IDGen): Context = 
      new ContextImpl(idGen, Map.empty)
    def apply(): Context = 
      new ContextImpl(new IDGen, Map.empty)
  }

  def emptyContext: Context = Context.apply()
  def methodContext(tree: Tree): MethodContext = 
    new MethodContext(tree, new IDGen)
  def atomicContext(tree: Tree): AtomicContext = 
    new AtomicContext(tree)
  def blockContext: BlockContext = new BlockContext(new IDGen)
}




