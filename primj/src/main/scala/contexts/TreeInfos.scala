package ch.usi.inf.l3.sana.primj.contexts

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.calcj
import tiny.types.Types
import tiny.ast.Trees
import tiny.names.Name
import primj.modifiers._
import tiny.modifiers.Flags

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

