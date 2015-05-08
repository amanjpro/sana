package ch.usi.inf.l3.sana.primj.types


import ch.usi.inf.l3.sana
import sana.calcj
import calcj.types


trait Types extends types.Types {


  trait MethodType extends Type {
    def ret: Type
    def params: List[Type]

    override def toString = s"(${params.mkString(", ")}) => ${ret}"
  }


  trait MethodTypeFactory {
    private class MethodTypeImpl(val ret: Type,
      val params: List[Type]) extends MethodType

    def apply(ret: Type, params: List[Type]): MethodType =
      new MethodTypeImpl(ret, params)
  }


  trait MethodTypeExtractor {
    def unapply(mt: MethodType): Option[(Type, List[Type])] = mt match {
      case null => None
      case mt   => Some((mt.ret, mt.params))
    }
  }


  val MethodType = new MethodTypeExtractor with MethodTypeFactory
}
