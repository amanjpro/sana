package ch.usi.inf.l3.sana.primj.util


import ch.usi.inf.l3.sana
import sana.calcj
import sana.tiny
import sana.primj
import tiny.contexts.TreeId
import primj.types.Types
import primj.ast.Trees
import primj.contexts.TreeInfos
import tiny.names._



trait Definitions extends calcj.util.Definitions {
  self: TreeInfos with Types with Trees =>

  override def builtInTypes: List[(Name, TypeState[Type])] = {
    val superTypes: List[(Name, TypeState[Type])] = super.builtInTypes 
    val langTypes:  List[(Name, TypeState[Type])] = List(
      Name("void")     -> toTypeState(VoidType)
    )

    superTypes ++ langTypes
  }
}

