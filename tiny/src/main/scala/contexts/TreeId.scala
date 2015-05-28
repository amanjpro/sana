package ch.usi.inf.l3.sana.tiny.contexts



trait TreeId {
  // def kind: TreeType
  def unitId: Int
  def treeId: Int 
}

object TreeId {
  // class TreeIdImpl(val kind: TreeType,
  class TreeIdImpl(val unitId: Int, val treeId: Int) extends TreeId

  // def apply(kind: TreeType, unitId: Int, treeId: Int): TreeId =
  def apply(unitId: Int, treeId: Int): TreeId =
    new TreeIdImpl(unitId, treeId)
    // new TreeIdImpl(kind, unitId, treeId)
}

// abstract class TreeType
// object MethodId extends TreeType
// object PackageId extends TreeType
// object ClassId extends TreeType
// object InterfaceId extends TreeType
// object VarId extends TreeType
// object VoidId extends TreeType
//

object NoId extends TreeId {
  // val kind: TreeType = VoidId
  val unitId: Int = -1;
  val treeId: Int = -1;
}
