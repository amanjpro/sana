package ch.usi.inf.l3.sana.calcj.contexts

import ch.usi.inf.l3.sana.tiny
import tiny.types.Types
import tiny.ast.Trees
import tiny.names.Name

trait TreeInfos extends tiny.contexts.TreeInfos {
  self: Trees with Types =>

  object BuiltInTypeKind extends TypeKind
}
