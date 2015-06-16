package ch.usi.inf.l3.sana.primj.contexts

import ch.usi.inf.l3.sana
import sana.calcj
import sana.tiny
import tiny.types.Types
import tiny.ast.Trees
import tiny.modifiers._
import tiny.names.Name

trait TreeInfos extends calcj.contexts.TreeInfos {
  self: Trees with Types =>

  def newValDefInfo(mods: Flags, info: Name, tpe: TypeState[Type]): TreeInfo =
    new TreeInfoImpl(mods, info, tpe, VariableKind)

  def newMethodDefInfo(mods: Flags, info: Name, 
      tpe: TypeState[Type]): TreeInfo =
    new TreeInfoImpl(mods, info, tpe, MethodKind)


  object MethodKind extends TermKind
  object VariableKind extends TermKind
}

