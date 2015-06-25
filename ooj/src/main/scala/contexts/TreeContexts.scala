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



  def packageContext(treeInfo: TreeInfo): PackageContext = 
    new PackageContext(treeInfo, new IDGen)


  // def packageContext(tree: PackageContext): PackageContext = 
  //   new MethodContext(newMethodDefInfo(tree.mods, tree.name, tree.tpe), 
  //     new IDGen)
  // trait RootContext extends super.RootContext {
  //   override protected def findInDefinitions(name: Name, 
  //               p: TreeInfo => Boolean): TreeId =
  //     definitions.toList.foldLeft(NoId: TreeId)((z, y) => y._2 match {
  //       case info  if info.name == name && p(info)                 =>
  //         y._1
  //       case _                                                     => z
  //     })
  //
  //   override def findInThisContext(name: Name, 
  //     p: TreeInfo => Boolean): TreeId = {
  //     super.findInThisContext(name, p) match {
  //       case NoId =>
  //         findInDefinitions(name, p)
  //       case id   => id
  //     }
  //   }
  //
  //   override def defines(id: TreeId): Boolean = {
  //     if(super.defines(id)) true
  //     else definitions.contains(id)
  //   }
  //
  //   // override def lookup(name: Name, p: TreeInfo => Boolean, 
  //     // owner: TreeId): TreeId = getContext(owner) match {
  //   override def getTree(id: TreeId): Option[TreeInfo] = {
  //     super.getTree(id) match {
  //       case Some(info)       => Some(info)
  //       case None             => definitions.get(id.forward)
  //     }
  //   }
  // }
}
