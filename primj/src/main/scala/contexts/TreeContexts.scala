package ch.usi.inf.l3.sana.primj.contexts

import ch.usi.inf.l3.sana.tiny
import tiny.ast.Trees
import tiny.util.MonadUtils
import tiny.report._
import tiny.names.Name
import tiny.types.Types
import tiny.contexts.IDGen
import tiny.contexts.TreeId

import scalaz.{StateT, State, Monoid, Applicative, ReaderWriterStateT}
import scalaz.Scalaz._

/**
 * A trait that contains types that represent context of a tree in  the
 * `primj` language.
 *
 *
 * @author Amanj Sherwany
 * @since 0.1
 * @version 0.1
 */
trait TreeContexts extends tiny.contexts.TreeContexts {
  self: Trees with Types with MonadUtils =>

  class MethodContext(val tree: IdentifiedTree, protected val idGen: IDGen, 
    val decls: Map[TreeId, Context] = Map.empty) extends NamedContext {
    
    
    def updateTree(tree: IdentifiedTree): NamedContext = 
      new MethodContext(tree, idGen, decls)
    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context = 
        new MethodContext(tree, idGen, binds)
  }
  


  def methodContext(tree: IdentifiedTree): MethodContext = 
    new MethodContext(tree, new IDGen)
}




