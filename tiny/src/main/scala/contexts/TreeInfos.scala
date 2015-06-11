package ch.usi.inf.l3.sana.tiny.contexts

import ch.usi.inf.l3.sana.tiny.types.Types
import ch.usi.inf.l3.sana.tiny.ast.Trees
import ch.usi.inf.l3.sana.tiny.names.Name

trait TreeInfos {
  self: Trees with Types =>

  trait TreeInfo {
    def tpe: TypeState[Type]
    def name: Name
    def kind: TreeKind
  }


  protected class TreeInfoImpl(val name: Name, val tpe: TypeState[Type],
    val kind: TreeKind) extends TreeInfo


  def newTreeInfo(tree: IdentifiedTree with NamedTree,
    kind: TreeKind): TreeInfo = new TreeInfoImpl(tree.name, tree.tpe, kind)

  def newTreeInfo(info: Name, tpe: TypeState[Type], kind: TreeKind): TreeInfo =
    new TreeInfoImpl(info, tpe, kind)

  trait TreeKind
  trait TypeKind extends TreeKind
  trait TermKind extends TreeKind
}




