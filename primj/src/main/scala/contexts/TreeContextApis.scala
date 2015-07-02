package ch.usi.inf.l3.sana.primj.contexts

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import tiny.contexts.{TreeId, NoId}
import tiny.names.Name
import primj.Global
import primj.modifiers.Ops._





trait TreeContextApis {
  self: TreeContexts with TreeInfos =>


  implicit class ImplicitContextApi(val ctx: Context) extends ContextApi {
    def isMethodDef(id: TreeId): Boolean =
      ctx.getTree(id).map(_.kind == MethodKind).getOrElse(false)
    def isVariable(id: TreeId): Boolean =
      ctx.getTree(id).map(_.kind == VariableKind).getOrElse(false)
    def isFinal(id: TreeId): Boolean =
      ctx.getTree(id).map(_.mods.isFinal).getOrElse(false)

    def enclosingMethod(id: TreeId): TreeId = {
      ctx.getContext(id) match {
        case Some(ctx: NamedContext) if ctx.tree.kind == MethodKind  =>
          id
        case Some(ctx)                                               =>
          enclosingMethod(id.up)
        case None                                                    =>
          NoId
      }
    }
  }

  trait ContextApi {
    def ctx: Context

    def findVariable(name: Name, owner: TreeId): TreeId = {
      ctx.lookup(name, (x) => x.kind == VariableKind, owner)
    }

    def isMethodDef(id: TreeId): Boolean
    def isVariable(id: TreeId): Boolean

    def enclosingMethod(id: TreeId): TreeId
    def enclosingMethodName(id: TreeId): Option[Name] =
      ctx.getTree(enclosingMethod(id)) match {
        case Some(c: NamedContext)      => Some(c.tree.name)
        case _                          => None
      }

  }
}
