package ch.usi.inf.l3.sana.tiny.contexts

import ch.usi.inf.l3.sana.tiny.types.Types
import ch.usi.inf.l3.sana.tiny.ast.Trees
import ch.usi.inf.l3.sana.tiny.names.Name
import ch.usi.inf.l3.sana.tiny.modifiers._

trait TreeInfos {
  self: Trees with Types =>

  trait TreeInfo {
    def tpe: TypeState[Type]
    def name: Name
    def kind: TreeKind
    def mods: Flags
  }


  protected class TreeInfoImpl(val mods: Flags,
    val name: Name, val tpe: TypeState[Type],
    val kind: TreeKind) extends TreeInfo


  def newTreeInfo(tree: IdentifiedTree with NamedTree,
    kind: TreeKind): TreeInfo = tree match {
      case m: Modifiable =>
        new TreeInfoImpl(m.mods, tree.name, tree.tpe, kind)
      case _             =>
        new TreeInfoImpl(noflags, tree.name, tree.tpe, kind)
  }

  def newTreeInfo(mods: Flags, info: Name, 
        tpe: TypeState[Type], kind: TreeKind): TreeInfo =
    new TreeInfoImpl(mods, info, tpe, kind)

  trait TreeKind
  trait TypeKind extends TreeKind
  trait TermKind extends TreeKind
}




