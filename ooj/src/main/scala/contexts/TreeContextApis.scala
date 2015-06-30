package ch.usi.inf.l3.sana.ooj.contexts

import ch.usi.inf.l3.sana
import sana.primj
import sana.ooj
import sana.tiny
import tiny.contexts.{TreeId, NoId}
import tiny.names.Name
import ooj.Global
import ooj.modifiers.Ops._





trait TreeContextApis extends primj.contexts.TreeContextApis {
  self: TreeContexts with TreeInfos =>

  
  implicit class ImplicitContextApi(override val ctx: Context) extends 
      super.ImplicitContextApi(ctx) with ContextApi {
    def enclosingClass(id: TreeId): TreeId = 
      ctx.getTree(id) match {
        case Some(info) if info.kind == ClassKind    =>
          id
        case Some(info)                              =>
          enclosingMethod(id.up)
        case None                                    =>
          NoId
      }

    def enclosingPackage(id: TreeId): TreeId = 
      ctx.getTree(id) match {
        case Some(info) if info.kind == PackageKind  =>
          id
        case Some(info)                              =>
          enclosingMethod(id.up)
        case None                                    =>
          NoId
      }
  }


  trait ContextApi extends super.ContextApi {
    /**
     * Get all enclosing package names until it gets to the root,
     * 
     * The inner-most package is the head, and the outer most one
     * is the tail.
     * 
     * For this hierarchy:
     * ch.usi.inf.l3.sana
     *
     * The return value will be something like:
     *
     * List(sana's id, l3's id, inf's id, usi's, id, ch's id)
     */
    def enclosingPackages(id: TreeId): List[TreeId] = {
      enclosingPackage(id) match {
        case NoId         => Nil
        case pid          => pid::enclosingPackages(pid)
      }
    }

    def enclosingPackageNames(id: TreeId): List[Name] = {
      enclosingPackages(id).flatMap {
        case c: NamedContext            => List(c.tree.name)
        case _                          => Nil
      }
    }

    def enclosingPackage(id: TreeId): TreeId
    def enclosingPackageName(id: TreeId): Option[Name] = 
      ctx.getTree(enclosingPackage(id)) match {
        case Some(c: NamedContext)      => Some(c.tree.name)
        case _                          => None
      }

    def enclosingClass(id: TreeId): TreeId

    def enclosingClassName(id: TreeId): Option[Name] =
      ctx.getTree(enclosingClass(id)) match {
        case Some(c: NamedContext)      => Some(c.tree.name)
        case _                          => None
      }
  }
}
