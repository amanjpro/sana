package ch.usi.inf.l3.sana.primj.contexts

import ch.usi.inf.l3.sana
import sana.calcj
import sana.primj
import sana.tiny
import tiny.report._
import tiny.contexts.{TreeId, NoId, IDGen}
import tiny.names.Name
import primj.types.Types
import primj.ast.Trees
import primj.util.Definitions


/**
 * A trait that contains types that represent context of a tree in  the
 * `primj` language.
 *
 *
 * @author Amanj Sherwany
 * @since 0.1
 * @version 0.1
 */
trait TreeContexts extends calcj.contexts.TreeContexts {
  self: TreeInfos with Trees with Types with Definitions =>

  class MethodContext(val tree: TreeInfo, 
    protected val idGen: IDGen, 
    val decls: Map[TreeId, Context] = Map.empty) extends NamedContext {
    
    
    def updateTree(tree: TreeInfo): NamedContext = 
      new MethodContext(tree, idGen, decls)
    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context = 
        new MethodContext(tree, idGen, binds)
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


  class AtomicContext(val tree: TreeInfo) extends NamedContext {
    final protected val decls: Map[TreeId, Context] = Map.empty
    final protected def idGen: IDGen = ???
    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context = this
    def updateTree(tree: TreeInfo): NamedContext = 
      new AtomicContext(tree)
    override def defines(id: TreeId): Boolean = false
    override def lookup(name: Name, 
      p: TreeInfo => Boolean, owner: TreeId): TreeId = NoId
    override def extend(owner: TreeId, decl: Context): (TreeId, Context) = 
      (NoId, this)
    override def update(id: TreeId, bind: Context): Context = this
    override def getTree(id: TreeId): Option[TreeInfo] =
      None
    override def getName(id: TreeId): Option[Name] = None
    override def getTpe(id: TreeId): Option[Type] = None
    override def delete(id: TreeId): Context = this
  }

  

  def atomicContext(tree: IdentifiedTree with NamedTree): AtomicContext = 
    new AtomicContext(newTreeInfo(tree, VariableKind))
  def blockContext: BlockContext = new BlockContext(new IDGen)
  def methodContext(tree: IdentifiedTree with NamedTree): MethodContext = 
    new MethodContext(newTreeInfo(tree, MethodKind), 
      new IDGen)


}




