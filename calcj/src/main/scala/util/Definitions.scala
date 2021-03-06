package ch.usi.inf.l3.sana.calcj.util


import ch.usi.inf.l3.sana
import sana.calcj
import sana.tiny
import tiny.contexts.TreeId
import calcj.types.Types
import calcj.ast.Trees
import calcj.contexts.TreeInfos
import ch.usi.inf.l3.sana.tiny.modifiers.Ops._
import tiny.names._



trait Definitions extends tiny.util.Definitions {
  self: TreeInfos with Types with Trees =>
  // byte, short, int, long, char
  // boolean
  // float, double

  // protected def point(tpe: Type): TypeState[Type] = toTypeState(tpe)

  def builtInTypes: List[(Name, TypeState[Type])] = List(
    ByteType.name       -> toTypeState(ByteType),
    ShortType.name      -> toTypeState(ShortType),
    IntType.name        -> toTypeState(IntType),
    LongType.name       -> toTypeState(LongType),
    CharType.name       -> toTypeState(CharType),
    BooleanType.name    -> toTypeState(BooleanType),
    FloatType.name      -> toTypeState(FloatType),
    DoubleType.name     -> toTypeState(DoubleType)
  )



  def getBuiltInName(name: String): Option[Name] =
    builtInTypes.filter(_._1.asString == name).headOption.map(_._1)

  def getBuiltInType(name: String): Option[TypeState[Type]] =
    builtInTypes.filter(_._1.asString == name).headOption.map(_._2)


  def langDefinitions: Map[TreeId, TreeInfo] = {
    val builtins = builtInTypes
    val range: Range = 0 until builtins.size
    range.zip(builtins).foldLeft(Map.empty: Map[TreeId, TreeInfo])((z, y) => {
      val (id, (name, tpe)) = y
      val tid = TreeId.builtinId(id)
      z + (tid -> newTreeInfo(noflags, name, tpe, BuiltInTypeKind))
    })
  }


  val BYTE_TYPE_NAME: Name          = ByteType.name
  val SHORT_TYPE_NAME: Name         = ShortType.name
  val CHAR_TYPE_NAME: Name          = CharType.name
  val INT_TYPE_NAME: Name           = IntType.name
  val LONG_TYPE_NAME: Name          = LongType.name
  val FLOAT_TYPE_NAME: Name         = FloatType.name
  val DOUBLE_TYPE_NAME: Name        = DoubleType.name
  val BOOLEAN_TYPE_NAME: Name       = BooleanType.name


}

