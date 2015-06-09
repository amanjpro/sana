package ch.usi.inf.l3.sana.primj.types


import ch.usi.inf.l3.sana
import sana.calcj
import calcj.types


trait Types extends types.Types {


  trait MethodType extends Type {
    def ret: Type
    def params: List[Type]

    def =:=(t: Type): Boolean = t match {
      case that: MethodType =>
        val pcheck = checkList[Type](params, that.params, _ =:= _)
        pcheck && this.ret =:= that.ret
      case _                => false
    }
    def =/=(t: Type): Boolean = !(this =:= t)
    // TODO: In Java-like languages, method subtyping is not like that!!
    def <:<(t: Type): Boolean = {
      val isEquiv = this =:= t
      lazy val isSub = t match {
        case that: MethodType =>
          val pcheck = checkList[Type](params, that.params, _ >:> _)
          pcheck && this.ret <:< that.ret
        case _                => false
      }
      isEquiv || isSub
    }
    def >:>(t: Type): Boolean = {
      val isEquiv = this =:= t
      lazy val isSup = t match {
        case that: MethodType =>
          val pcheck = checkList[Type](params, that.params, _ <:< _)
          pcheck && this.ret >:> that.ret
        case _                => false
      }
      isEquiv || isSup
    }


    

    def show: String = s"MethodType((${params.mkString(", ")}) => ${ret})"
  }

  def checkList[T](ts1: List[T], ts2: List[T], 
    f: (T, T) => Boolean): Boolean = {
    ts1.zip(ts2).foldLeft(true)((z, y) => {
      z && f(y._1, y._2)
    })
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
