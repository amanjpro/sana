package ch.usi.inf.l3.sana.calcj.types


import ch.usi.inf.l3.sana
import sana.tiny
import tiny.names.Name
import tiny.types


trait Types extends types.Types {

  
  /*
    A primitive type is one of the following:

    PrimitiveType:
      - boolean
      - NumericType:
        -- IntegralType:
           byte, short, int, long, char
        -- DoubleType: 
           float, double

  */

  trait PrimitiveType extends Type {
    def =:=(t: Type): Boolean = this == t
    def =/=(t: Type): Boolean = this != t
    def <:<(t: Type): Boolean = t =:= this
    def >:>(t: Type): Boolean = t =:= this
  }



  trait NumericType extends PrimitiveType


  trait IntegralType extends NumericType


  object ByteType extends IntegralType {
    override def show: String = "byte type"
    def name: Name = Name("byte")
  }

  object ShortType extends IntegralType {
    override def show: String = "short type"
    def name: Name = Name("short")
  }

  object IntType extends IntegralType {
    override def show: String = "int type"
    def name: Name = Name("int")
  }

  object LongType extends IntegralType {
    override def show: String = "long type"
    def name: Name = Name("long")
  }

  object CharType extends IntegralType {
    override def show: String = "char type"
    def name: Name = Name("char")
  }



  trait FloatingPointType extends NumericType
  object FloatType extends FloatingPointType {
    override def show: String = "float type"
    def name: Name = Name("float")
  }

  object DoubleType extends FloatingPointType {
    override def show: String = "double type"
    def name: Name = Name("double")
  }


  object BooleanType extends PrimitiveType {
    override def show: String = "boolean type"
    def name: Name = Name("boolean")
  }
  
  


  // Binary and Unary types
  trait BinaryType extends Type {
    def op1: Type
    def op2: Type
    def ret: Type
    def name: Name = Name("<binary-type>")

    def =:=(t: Type): Boolean = t match {
      case that: BinaryType =>
        this.op1 =:= that.op1 && this.op2 =:= that.op2 && this.ret =:= that.ret
      case _                => false
    }
    def =/=(t: Type): Boolean = !(this =:= t)
    def <:<(t: Type): Boolean = t =:= this
    def >:>(t: Type): Boolean = t =:= this
    override def show: String = s"BinaryType((${op1}, {$op2}) => ${ret})"
  }


  trait UnaryType extends Type {
    def op:   PrimitiveType
    def ret:  PrimitiveType

    def =:=(t: Type): Boolean = t match {
      case that: UnaryType =>
        this.op =:= that.op && this.ret =:= that.ret
      case _               => false
    }
    def name: Name = Name("<unary-type>")
    def =/=(t: Type): Boolean = !(this =:= t)
    def <:<(t: Type): Boolean = t =:= this
    def >:>(t: Type): Boolean = t =:= this
    override def show: String = s"UnaryType((${op}) => ${ret})"
  }



  trait BinaryTypeFactory {

    private class BinaryTypeImpl(val op1: Type,
      val op2: Type, val ret: Type) extends BinaryType

    def apply(op1: Type, op2: Type, 
      ret: Type): BinaryType =
      new BinaryTypeImpl(op1, op2, ret)

  }

  trait BinaryTypeExtractor {
    def unapply(bt: BinaryType): Option[(Type, Type, Type)] = bt match {
      case null => None
      case bt   => Some((bt.op1, bt.op2, bt.ret))
    }
  }


  trait UnaryTypeFactory {
    private class UnaryTypeImpl(val op: PrimitiveType,
      val ret: PrimitiveType) extends UnaryType

    def apply(op: PrimitiveType, 
      ret: PrimitiveType): UnaryType = new UnaryTypeImpl(op, ret)

  }

  trait UnaryTypeExtractor {
    def unapply(ut: UnaryType): Option[(PrimitiveType, 
      PrimitiveType)] = ut match {
      case null => None
      case bt   => Some((ut.op, ut.ret))
    }
  }


  val UnaryType  = new UnaryTypeExtractor with UnaryTypeFactory
  val BinaryType = new BinaryTypeExtractor with BinaryTypeFactory

}
