package ch.usi.inf.l3.sana.primj.contexts

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import tiny.contexts.{TreeId}
import tiny.names.Name
import primj.Global
import primj.modifiers._





trait TreeContextApis {
  self: TreeContexts =>

  
  // TODO: Implement this class
  implicit class ImplicitContextApi(val ctx: Context) extends ContextApi {
    def isBlock(id: TreeId): Boolean = true
    def isFor(id: TreeId): Boolean = true
    def isMethodDef(id: TreeId): Boolean = true
    def isFinal(id: TreeId): Boolean = 
      true
      // ctx.getTree(id).map(_.mods.isFinal).getOrElse(false)
  }

  trait ContextApi {
    def ctx: Context
    def findVariable(name: Name, owner: TreeId): TreeId = {
      ctx.lookup(name, _ => true, owner)
    }

    def isBlock(id: TreeId): Boolean
    def isFor(id: TreeId): Boolean
    def isMethodDef(id: TreeId): Boolean

    // def enclosingMethod(id: TreeId): Option[TreeId] = for {
      // tree <- getTree(id)
      // _    
    // }
  }
}
