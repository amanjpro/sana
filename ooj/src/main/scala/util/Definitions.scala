package ch.usi.inf.l3.sana.ooj.util


import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.ooj
import tiny.contexts.TreeId
import ooj.types.Types
import ooj.ast.Trees
import primj.contexts.TreeInfos
import tiny.names._



trait Definitions extends primj.util.Definitions {
  self: TreeInfos with Types with Trees =>

  override def builtInTypes: List[(Name, TypeState[Type])] = {
    val superTypes: List[(Name, TypeState[Type])] = super.builtInTypes
    val langTypes:  List[(Name, TypeState[Type])] = List(
      NullType.name      -> toTypeState(NullType)
    )

    superTypes ++ langTypes
  }


  val NO_PACKAGE_NAME: Name        = Name("")
  val JAVA_PACKAGE_NAME: Name      = Name("java")
  val LANG_PACKAGE_NAME: Name      = Name("java")
  val JAVA_LANG_PACKAGE_NAME: Name = Name("lang")
  val STRING_TYPE_NAME: Name       = Name("String")
  val OBJECT_TYPE_NAME: Name       = Name("Object")
  val ARRAY_TYPE_NAME: Name        = Name("<array>")

  val CONSTRUCTOR_NAME: Name       = Name("<init>")

  // to TypeUtils
  // def objectClassType(ctx: Context): ClassType =
  //   ctx.getTpe(OBJECT_TYPE_NAME, _ => true).eval(ctx)
  //
  // def stringClassType(ctx: Context): ClassType =
  //   ctx.getTpe(STRING_TYPE_NAME, _ => true).eval(ctx)
}
