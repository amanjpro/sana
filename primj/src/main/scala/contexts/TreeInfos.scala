package ch.usi.inf.l3.sana.primj.contexts

import ch.usi.inf.l3.sana
import sana.calcj
import sana.tiny
import tiny.types.Types
import tiny.ast.Trees
import tiny.names.Name

trait TreeInfos extends calcj.contexts.TreeInfos {
  self: Trees with Types =>

  def newValDefInfo(info: Name, tpe: TypeState[Type]): TreeInfo =
    new TreeInfoImpl(info, tpe, VariableKind)

  def newMethodDefInfo(info: Name, tpe: TypeState[Type]): TreeInfo =
    new TreeInfoImpl(info, tpe, MethodKind)


  object MethodKind extends TermKind
  object VariableKind extends TermKind
}

