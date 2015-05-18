package ch.usi.inf.l3.sana.calcj.types


import ch.usi.inf.l3.sana
import sana.tiny
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
    override def toString = "byte"
  }

  object ShortType extends IntegralType {
    override def toString = "short"
  }

  object IntType extends IntegralType {
    override def toString = "int"
  }

  object LongType extends IntegralType {
    override def toString = "long"
  }

  object CharType extends IntegralType {
    override def toString = "char"
  }



  trait FloatingPointType extends NumericType
  object FloatType extends FloatingPointType {
    override def toString = "float"
  }

  object DoubleType extends FloatingPointType {
    override def toString = "double"
  }


  object BooleanType extends PrimitiveType {
    override def toString = "boolean"
  }
  
  object StringType extends PrimitiveType {
    override def toString = "String"
  }


  // Binary and Unary types
  trait BinaryType extends Type {
    def op1: PrimitiveType
    def op2: PrimitiveType
    def ret: PrimitiveType

    def =:=(t: Type): Boolean = t match {
      case that: BinaryType =>
        this.op1 =:= that.op1 && this.op2 =:= that.op2 && this.ret =:= that.ret
      case _                => false
    }
    def =/=(t: Type): Boolean = !(this =:= t)
    def <:<(t: Type): Boolean = t =:= this
    def >:>(t: Type): Boolean = t =:= this
    override def toString: String = s"BinaryType(${op1}, {$op2}) => ${ret}"
  }


  trait UnaryType extends Type {
    def op:   PrimitiveType
    def ret:  PrimitiveType

    def =:=(t: Type): Boolean = t match {
      case that: UnaryType =>
        this.op =:= that.op && this.ret =:= that.ret
      case _               => false
    }
    def =/=(t: Type): Boolean = !(this =:= t)
    def <:<(t: Type): Boolean = t =:= this
    def >:>(t: Type): Boolean = t =:= this
    override def toString: String = s"UnaryType(${op}) => ${ret}"
  }



  trait BinaryTypeFactory {

    private class BinaryTypeImpl(val op1: PrimitiveType,
      val op2: PrimitiveType, val ret: PrimitiveType) extends BinaryType

    def apply(op1: PrimitiveType, op2: PrimitiveType, 
      ret: PrimitiveType): BinaryType =
      new BinaryTypeImpl(op1, op2, ret)

  }

  trait BinaryTypeExtractor {
    def unapply(bt: BinaryType): Option[(PrimitiveType, 
      PrimitiveType, PrimitiveType)] = bt match {
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
