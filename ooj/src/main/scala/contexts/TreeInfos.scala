package ch.usi.inf.l3.sana.ooj.contexts

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.ooj

import ooj.types.Types
import ooj.ast.Trees
import tiny.names.Name
import tiny.modifiers.Flags

import ooj.modifiers._

trait TreeInfos extends primj.contexts.TreeInfos {
  self: Trees with Types =>

  def newPackageDefInfo(mods: Flags, info: Name, 
      tpe: TypeState[Type]): TreeInfo =
    new TreeInfoImpl(mods, info, tpe, PackageKind)

  def newClassDefInfo(mods: Flags, info: Name, 
      tpe: TypeState[Type]): TreeInfo =
    new TreeInfoImpl(mods, info, tpe, ClassKind)


  object PackageKind extends TermKind
  object ClassKind extends TermKind
}
