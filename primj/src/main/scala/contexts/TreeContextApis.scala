package ch.usi.inf.l3.sana.primj.contexts

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import tiny.contexts.{TreeId, NoId}
import tiny.names.Name
import primj.Global
import primj.modifiers._





trait TreeContextApis {
  self: TreeContexts with TreeInfos =>

  
  // TODO: Implement this class
  implicit class ImplicitContextApi(val ctx: Context) extends ContextApi {
    def isBlock(id: TreeId): Boolean = true
    def isFor(id: TreeId): Boolean = true
    def isMethodDef(id: TreeId): Boolean = 
      ctx.getTree(id).map(_.kind == MethodKind).getOrElse(false)
    def isVariable(id: TreeId): Boolean = 
      ctx.getTree(id).map(_.kind == VariableKind).getOrElse(false)
    def isFinal(id: TreeId): Boolean = 
      ctx.getTree(id).map(_.mods.isFinal).getOrElse(false)
    def enclosingMethod(id: TreeId): TreeId = {
      ctx.getTree(id) match {
        case Some(info) if info.kind == MethodKind  =>
          id
        case Some(info)                             =>
          enclosingMethod(id.up)
        case None                                   =>
          NoId
      }
    }
  }

  trait ContextApi {
    def ctx: Context

    def findVariable(name: Name, owner: TreeId): TreeId = {
      ctx.lookup(name, (x) => x.kind == VariableKind, owner)
    }

    def isBlock(id: TreeId): Boolean
    def isFor(id: TreeId): Boolean
    def isMethodDef(id: TreeId): Boolean
    def isVariable(id: TreeId): Boolean

    def enclosingMethod(id: TreeId): TreeId
    // = for {
      // tree <- getTree(id)
      // _    
    // }
  }
}
