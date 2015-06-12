package ch.usi.inf.l3.sana.arrayj.types


import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.names.Name
import primj.types


trait Types extends types.Types{

  trait ArrayType extends Type

  trait NonEmptyArrayType extends ArrayType {
    def elemType: Type
    def =:=(t: Type): Boolean = t match {
      case other: NonEmptyArrayType => this.elemType =:= other.elemType
      case _                        => false
    }
    def =/=(t: Type): Boolean = t match {
      case other: NonEmptyArrayType => this.elemType =/= other.elemType
      case _                        => true
    }
    def <:<(t: Type): Boolean = t match {
      case other: NonEmptyArrayType => this.elemType <:< other.elemType
      case _                        => false
    }
    def >:>(t: Type): Boolean = t match {
      case other: NonEmptyArrayType => this.elemType >:> other.elemType
      case _                        => false
    }

    def show: String = "<array type"
    def name: Name = Name("array")

  }

  trait NonEmptyArrayTypeFactory {
    private class ArrayTypeImpl(val elemType: Type) 
            extends NonEmptyArrayType

    def apply(elemType: Type): ArrayType = new ArrayTypeImpl(elemType)
  }

  
  trait NonEmptyArrayTypeExtractor {
    def unapply(at: NonEmptyArrayType): Option[Type] = at match {
      case null => None
      case at   => Some(at.elemType)
    }
  }

  object EmptyArrayType extends ArrayType {
    def =:=(t: Type): Boolean = t match {
      case EmptyArrayType        => true
      case _                     => false
    }

    def =/=(t: Type): Boolean = t match {
      case EmptyArrayType        => false
      case _                     => true
    }

    def <:<(t: Type): Boolean = t match {
      case other: ArrayType => true
      case _                => false
    }
    def >:>(t: Type): Boolean = t match {
      case EmptyArrayType        => true
      case _                     => false
    }

    def show: String = "<empty array type"
    def name: Name = Name("empty-array")

  }


  val ArrayType = new NonEmptyArrayTypeFactory with 
                      NonEmptyArrayTypeExtractor {}
}
