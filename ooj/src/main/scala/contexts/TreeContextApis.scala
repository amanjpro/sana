package ch.usi.inf.l3.sana.ooj.contexts

import ch.usi.inf.l3.sana
import sana.primj
import sana.ooj
import sana.tiny
import tiny.contexts.{TreeId, NoId}
import tiny.names.Name
import ooj.Global
import ooj.modifiers.Ops._
import ooj.util.Definitions





trait TreeContextApis extends primj.contexts.TreeContextApis {
  self: TreeContexts with TreeInfos with Definitions =>


  implicit class ImplicitContextApi(override val ctx: Context) extends
      super.ImplicitContextApi(ctx) with ContextApi {

    def javaLangClass(name: Name): TreeId = {
      ctx.lookup(STRING_TYPE_NAME,
        _.kind.isInstanceOf[TypeKind], javaLangContext)
    }

    def javaLangContext: TreeId = {
      val javaPackage = ctx.lookup(JAVA_PACKAGE_NAME,
            _.kind == PackageKind, NoId)
      ctx.lookup(LANG_PACKAGE_NAME,
            _.kind == PackageKind, javaPackage)
    }

    def isStatic(id: TreeId): Boolean =
      ctx.getTree(id).map(_.mods.isStatic).getOrElse(false)


    def isInterface(id: TreeId): Boolean =
      ctx.getTree(id).map((x) => x.kind == ClassKind &&
                    x.mods.isInterface).getOrElse(false)

    def isClass(id: TreeId): Boolean =
      ctx.getTree(id).map((x) => x.kind == ClassKind &&
                    ! x.mods.isInterface).getOrElse(false)

    def isClassOrInterface(id: TreeId): Boolean =
      ctx.getTree(id).map((x) => x.kind == ClassKind).getOrElse(false)

    def enclosingClass(id: TreeId): TreeId =
      ctx.getContext(id) match {
        case Some(ctx: NamedContext) if ctx.tree.kind == ClassKind   =>
          id
        case Some(ctx: NamedContext) if ctx.tree.kind == PackageKind =>
          NoId
        case Some(ctx)                                               =>
          enclosingClass(id.up)
        case None                                                    =>
          NoId
      }


    def enclosingPackage(id: TreeId): TreeId =
      ctx.getContext(id) match {
        case Some(ctx: NamedContext) if ctx.tree.kind == PackageKind =>
          id
        case Some(ctx)                                               =>
          enclosingPackage(id.up)
        case None                                                    =>
          NoId
      }

    def enclosingNonLocal(id: TreeId): TreeId =
      ctx.getContext(id) match {
        case Some(ctx: NamedContext)
            if ctx.tree.kind == VariableKind && ctx.tree.mods.isField =>
          id
        case Some(ctx: NamedContext)                                  =>
          id
        case Some(ctx)                                                =>
          enclosingPackage(id.up)
        case None                                                     =>
          NoId
      }
  }


  trait ContextApi extends super.ContextApi {
    /**
     * Get all enclosing package ids until it gets to the root,
     *
     * The inner-most package is the head, and the outer most one
     * is the last.
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

    /**
     * Get all enclosing class ids until it gets to the first package,
     *
     * The inner-most package is the head, and the outer most one
     * is the last.
     */
    def enclosingClasses(id: TreeId): List[TreeId] = {
      enclosingClass(id) match {
        case NoId         => Nil
        case cid          => cid::enclosingClasses(cid)
      }
    }

    def enclosingNonLocal(id: TreeId): TreeId

    def topLevelClass(id: TreeId): TreeId = enclosingClasses(id) match {
      case Nil                  => NoId
      case classes              => classes.last
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


    def javaLangContext: TreeId
    def javaLangClass(name: Name): TreeId

    /**
     * Get the enclosing class of this id, in case it reached
     * a package before reaching a class, then stop searching
     * and return NoId
     */
    def enclosingClass(id: TreeId): TreeId

    def isStatic(id: TreeId): Boolean
    def isInterface(id: TreeId): Boolean
    def isClass(id: TreeId): Boolean
    def isClassOrInterface(id: TreeId): Boolean

    def enclosingClassName(id: TreeId): Option[Name] =
      ctx.getTree(enclosingClass(id)) match {
        case Some(c: NamedContext)      => Some(c.tree.name)
        case _                          => None
      }
  }
}
