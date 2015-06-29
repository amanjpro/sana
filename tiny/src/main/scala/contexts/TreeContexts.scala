package ch.usi.inf.l3.sana.tiny.contexts

import ch.usi.inf.l3.sana.tiny
import tiny.ast.Trees
import tiny.report._
import tiny.debug.logger
import tiny.names.Name
import tiny.types.Types


/**
 * A trait that contains types that represent context of a tree in  the
 * `tiny` language.
 *
 * @author Amanj Sherwany
 * @since 0.1
 * @version 0.1
 */
trait TreeContexts {
  self: TreeInfos with Trees with Types =>

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
        val ctx = decls.get(id.head)
        ctx match {
          case None            => this
          case Some(ctx)       =>
            val newCtx = ctx.delete(id.forward)
            update(id.head, newCtx)
        }
    }

    /**
     * Finds all bindings for the given Name directly in this Scope.
     *
     * @see [[contexts.TreeId]]
     * @param name The name that we want to check
     * @param p The predicate that the resulted tree should satisfy
     * @return list of ids of the trees, and Nil if none found
     */
    def findAllInThisContext(name: Name, 
      p: TreeInfo => Boolean): List[TreeId] = {
      decls.toList.foldLeft(Nil: List[TreeId])((z, y) => y._2 match {
        case n: NamedContext if (n.tree.name == name) && p(n.tree) =>
          y._1 :: z
        case _                                                     => z
      })
    }

    /**
     * Finds all bindings for the given Name in a given Scope up to
     * some other scope.
     *
     * @see [[contexts.TreeId]]
     * @param name The name that we want to check
     * @param p The predicate that the resulted tree should satisfy
     * @param from The context to start with
     * @param to The context to end at
     * @return list of ids of the trees, and Nil if none found
     */
    def boundedLookup(name: Name, p: TreeInfo => Boolean, 
            from: TreeId, to: TreeId): List[TreeId] = {
      if(from == to || to == NoId) Nil
      else {
        val ctx   = getContext(from)
        val infos = ctx.map(_.findAllInThisContext(name, p)).getOrElse(Nil)
        infos ++ boundedLookup(name, p, from.up, to) 
      }
    }

    /**
     * Finds a binding for the given Name directly in this Scope.
     *
     * @see [[contexts.TreeId]]
     * @param name The name that we want to check
     * @param p The predicate that the resulted tree should satisfy
     * @return id of the tree, if the it is defined, and NoId otherwise
     */
    def findInThisContext(name: Name, p: TreeInfo => Boolean): TreeId = {
      val id = decls.toList.foldLeft(NoId: TreeId)((z, y) => y._2 match {
        case n: NamedContext if (n.tree.name == name) && p(n.tree) =>
          y._1
        case _                                                     => z
      })
      id
    }
    /**
     * Checks if there is a binding for the given Name directly in this
     * Scope.
     *
     * @see [[contexts.TreeId]]
     * @param name The name that we want to check
     * @return True if the name is defined, and false otherwise
     */
    def directlyDefines(name: Name): Boolean = {
      if(findInThisContext(name, _ => true) == NoId) false
      else true
    }

    /**
     * Checks if there is a binding for the given [[TreeId]] id.
     *
     * @see [[contexts.TreeId]]
     * @param id The id that we want to check
     * @return True if the id is defined, and false otherwise
     */
    def defines(id: TreeId): Boolean = {
      require(id != NoId)
      decls.get(id.head) match {
        case None                                => false
        case Some(_)         if id.isSimple      => true
        case Some(c)                             => 
          c.defines(id.forward)
      }
    }

    /**
     * Get the context bound to the given `id`
     *
     * @param id the id of the tree
     * @return optionally returns the tree that is bound to this id
     */
    def getContext(id: TreeId): Option[Context] = decls.get(id.head) match {
      case None                           => 
        logger.warning(s"Undefined context ${id}")
        None
      case Some(c) if id.isSimple         => Some(c)
      case Some(c)                        => c.getContext(id.forward)
      //   ctx.getContext(id.forward)
      // for {
      //   ctx   <- decls.get(id.head)
      //   r     <- ctx.getContext(id.forward)
      // } yield r
    }

    /**
     * In the current context, looks up for a [[ast.Trees#NamedTree]] that has
     * the given name.
     *
     * @see [[contexts.TreeId]]
     * @param name The name of the [[ast.Trees#NamedTree]]
     * @param p The predicate that the resulted tree should satisfy
     * @param owner The owner of this name, if the tree is top-level 
     *              then the owner is the id of the compilation unit
     * @return The id of the [[ast.Trees#NamedTree]], or `NoId` if not found
     */
    def lookup(name: Name, p: TreeInfo => Boolean, 
      owner: TreeId): TreeId = getContext(owner) match {
      case Some(ctx)                                                     =>
        val id = ctx.findInThisContext(name, p)
        if(id == NoId) 
          lookup(name, p, owner.up)
        else {
          //TODO: Make sure this is correct
          owner.concat(id)
          // TreeId(owner, id)
          // id.merge(owner.head)
        }
      case None                                                          =>
        logger.warning("Name not found " + name.asString)
        NoId
    }

    protected def extendThisContext(decl: Context): (TreeId, Context) = {
      val (nid, idg) = idGen.nextId
      val id = TreeId(NoId, nid)
      val ctx = newContext(idg, decls + (id -> decl))
      logger.debug(s"Extend the current context with $id")
      (id, ctx)
    }

    /**
     * Extends this context with a given [[Context]].
     *
     * @param owner The owner of the context that we want to add 
     *              to this context.
     * @param decl The declaration to be added to this context
     * @return A tuple of the id of the new declaration and a new instance of
     *         a context with the given declaration added to it.
     */
    def extend(owner: TreeId, decl: Context): (TreeId, Context) = {
      owner match {
        case NoId                     =>
          extendThisContext(decl)
        case id                       => 
          decls.get(owner.head) match {
            case None            =>
              logger.warning("Undefined owner " + owner.head)
              (NoId, this)
            case Some(ctx1)       =>
              val (id, ctx2) = ctx1.extend(owner.forward, decl)
              val head       = owner.head
              val completeId = id.merge(head)
              logger.debug(s"Merging: ${id} with: ${head}")
              logger.debug(s"Result: $completeId")
              val ctx3       = update(head, ctx2)
              (completeId, ctx3)
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
    def update(id: TreeId, tree: TreeInfo): Context = {
      decls.get(id.head) match {
        case None                                     =>
          logger.warning(s"Undefined owner ${id.head} cannot be updated")
          this
        case Some(ctx: NamedContext) if id.isSimple   =>
          val ctx2 = ctx.updateTree(tree)
          update(id, ctx2)
        case Some(ctx)                                =>
          val ctx2 = ctx.update(id.forward, tree)
          update(id.head, ctx2)
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
    def update(id: TreeId, bind: Context): Context = {
      decls.get(id.head) match {
        case None                             =>
          logger.warning(s"Undefined owner ${id.head} cannot be found")
          this
        case Some(_)        if id.isSimple    =>
          newContext(idGen, decls + (id -> bind))
        case Some(_)                          =>
          val newBind = update(id.forward, bind)
          newContext(idGen, decls + (id.head -> newBind))
      }
    }

    


    /**
     * Get the tree bound to the given `id`
     *
     * @param id the id of the tree
     * @return optionally returns the tree that is bound to this id
     */
    def getTree(id: TreeId): Option[TreeInfo] = {
      // require(id != NoId)
      lazy val r = decls.get(id.head) match {
        case None                                         => 
          logger.warning(s"Undefined tree ${id}")
          None
        case Some(c: NamedContext) if id.isSimple         => 
          Some(c.tree)
        case Some(c)                                      => 
          c.getTree(id.forward)
      }
      if(TreeId.isBuiltIn(id)) None
      else r
    }
    // id match {
    //   case NoId              => None
    //   case _: SimpleId       => decls.get(id) match {
    //     case c: NamedContext        => Some(c.tree)
    //     case _                      => None
    //   }
    //   case _: CompositeId    =>
    //     decls.get(id.head).flatMap(_.getTree(id.forward)) 
    // }

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
      r    <- Some(tree.name)
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
    def getTpe(id: TreeId): TypeState[Type] = 
      getTree(id).map(_.tpe).getOrElse(toTypeState(ErrorType))
      
    //   for {
    //   tree <- getTree(id)
    // } yield tree.tpe.eval(this)
  }

  /**
   * The base trait for contexts that represent a named tree
   *
   * @group Tree Contexts
   */
  trait NamedContext extends Context {
    def tree: TreeInfo
    def updateTree(tree: TreeInfo): NamedContext
  }
  
  /**
   * The base trait for root contexts.
   *
   * A root context is where all built-in names and
   * types of a language are stored.
   *
   * @group Tree Contexts
   */
  trait RootContext extends Context {
    def definitions: Map[TreeId, TreeInfo]

    protected def findInDefinitions(name: Name, 
                p: TreeInfo => Boolean): TreeId =
      definitions.toList.foldLeft(NoId: TreeId)((z, y) => y._2 match {
        case info  if info.name == name && p(info)                 =>
          y._1
        case _                                                     => z
      })

    override def findInThisContext(name: Name, 
      p: TreeInfo => Boolean): TreeId = {
      super.findInThisContext(name, p) match {
        case NoId =>
          findInDefinitions(name, p)
        case id   => id
      }
    }

    override def lookup(name: Name, p: TreeInfo => Boolean, 
      owner: TreeId): TreeId = super.lookup(name, p, owner) match {
      case NoId => findInDefinitions(name, p)
      case id   => id
    }

    override def defines(id: TreeId): Boolean = {
      if(super.defines(id)) true
      else definitions.contains(id)
    }

    // override def lookup(name: Name, p: TreeInfo => Boolean, 
      // owner: TreeId): TreeId = getContext(owner) match {
    override def getTree(id: TreeId): Option[TreeInfo] = {
      super.getTree(id) match {
        case Some(info)       => Some(info)
        case None             => definitions.get(id)
      }
    }
  }

  /**
   * An object to represent missing compilation units. 
   *
   * @group Compilation Unit Contexts
   */
  protected case object InvalidContext extends Context {
    // this field should never be used
    protected def idGen: IDGen = ???
    protected def decls: Map[TreeId, Context] = Map.empty
    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context = InvalidContext
    override def defines(id: TreeId): Boolean = false
    override def lookup(name: Name, 
      p: TreeInfo => Boolean, owner: TreeId): TreeId = NoId
    override def extend(owner: TreeId, decl: Context): (TreeId, Context) = 
      (NoId, InvalidContext)
    override def update(id: TreeId, bind: Context): Context = InvalidContext
    override def update(id: TreeId, tree: TreeInfo): Context = InvalidContext
    override def getTree(id: TreeId): Option[TreeInfo] =
      None
    override def getName(id: TreeId): Option[Name] = None
    override def getTpe(id: TreeId): TypeState[Type] = toTypeState(ErrorType)
    override def delete(id: TreeId): Context =
      InvalidContext
  }

  private class ContextImpl(protected val idGen: IDGen,
    val decls: Map[TreeId, Context]) extends Context {
    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context = 
        new ContextImpl(idGen, binds)
  }

  protected class RootContextImpl(protected val idGen: IDGen,
    val decls: Map[TreeId, Context],
    val definitions: Map[TreeId, TreeInfo]) extends RootContext {
    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context = 
        new RootContextImpl(idGen, binds, definitions)
  }


  object Context {
    def apply(idGen: IDGen, decls: Map[TreeId, Context]): Context = 
      new ContextImpl(idGen, decls)
    def apply(idGen: IDGen): Context = 
      new ContextImpl(idGen, Map.empty)
    def apply(): Context = 
      new ContextImpl(new IDGen, Map.empty)
  }


  def invalidContext: Context = InvalidContext
  def emptyContext: Context = Context.apply()
  def rootContext: RootContext
}
