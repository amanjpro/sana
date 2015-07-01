package ch.usi.inf.l3.sana.ooj.contexts

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj

import ooj.types.Types
import ooj.ast.Trees
import tiny.names.Name
import tiny.modifiers.Flags
import tiny.modifiers.Ops._
import tiny.contexts.TreeId

import ooj.modifiers._

trait TreeInfos extends primj.contexts.TreeInfos {
  self: Trees with Types =>

  trait ClassInfo extends TreeInfo {
    def parents: List[TreeId]
  }

  protected class ClassInfoImpl(mods: Flags,
    info: Name, val parents: List[TreeId],
    tpe: TypeState[Type]) 
    extends TreeInfoImpl(mods, info, tpe, ClassKind) with ClassInfo



  def newPackageDefInfo(info: Name): TreeInfo =
    new TreeInfoImpl(noflags, info, toTypeState(notype), PackageKind)

  def newClassDefInfo(mods: Flags, info: Name, parents: List[TreeId],
      tpe: TypeState[Type]): ClassInfo =
    new ClassInfoImpl(mods, info, parents, tpe)


  object PackageKind extends TermKind
  object ClassKind extends TermKind
}
