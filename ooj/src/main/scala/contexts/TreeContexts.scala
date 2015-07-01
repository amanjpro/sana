package ch.usi.inf.l3.sana.ooj.contexts

import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.ooj

import tiny.report._
import tiny.contexts.{TreeId, NoId, IDGen}
import tiny.names.Name

import ooj.types.Types
import ooj.ast.Trees
import ooj.util.Definitions


trait TreeContexts extends primj.contexts.TreeContexts {
  self: TreeInfos with Trees with Types with Definitions =>

  protected class PackageContext(val tree: TreeInfo, 
    protected val idGen: IDGen, 
    val decls: Map[TreeId, Context] = Map.empty) extends NamedContext {
    
    
    def updateTree(tree: TreeInfo): NamedContext = 
      new PackageContext(tree, idGen, decls)
    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context = 
        new PackageContext(tree, idGen, binds)
  }

  protected class ClassContext(val tree: TreeInfo, 
    protected val idGen: IDGen, override val inheritedContexts: List[TreeId],
    val decls: Map[TreeId, Context] = Map.empty) extends NamedContext {
    
    def updateTree(tree: TreeInfo): NamedContext = tree match {
      case ci: ClassInfo  =>
        new ClassContext(tree, idGen, ci.parents, decls)
      case _              =>
        new ClassContext(tree, idGen, List(), decls)
    }



    protected def newContext(idGen: IDGen, 
      binds: Map[TreeId, Context]): Context = 
        new ClassContext(tree, idGen, inheritedContexts, binds)
  }

  def packageContext(treeInfo: TreeInfo): PackageContext = 
    new PackageContext(treeInfo, new IDGen)

  def packageContext(pkg: PackageDef): PackageContext = {
    val info = newPackageDefInfo(pkg.name)
    new PackageContext(info, new IDGen)
  }

  def classContext(treeInfo: ClassInfo): ClassContext = 
    new ClassContext(treeInfo, new IDGen, treeInfo.parents)

  def classContext(clazz: ClassDef): ClassContext = {
    val info = newClassDefInfo(clazz.mods, clazz.name, 
      clazz.parents.map(_.uses), clazz.tpe)
    new ClassContext(info, new IDGen, info.parents)
  }
}
