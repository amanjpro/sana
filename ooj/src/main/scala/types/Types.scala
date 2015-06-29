package ch.usi.inf.l3.sana.ooj.types


import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import sana.ooj
import tiny.names.Name
import tiny.contexts.TreeId
import primj.types
import ooj.util.Definitions

import scala.collection.immutable.Set


trait Types extends types.Types {
  self: Definitions =>

  trait RefType extends Type


  trait ClassType extends RefType {
    def parents: Set[Type]
    def id: TreeId


    def allParents: Set[Type] = parents.flatMap { 
      case ctpe:ClassType  => ctpe.allParents
      case _               => Set.empty[Type]
    }

    def =:=(t: Type): Boolean = t match {
      case ct: ClassType      => id == ct.id
      case _                  => false
    }

    def =/=(t: Type): Boolean = !(this =:= t)
    def <:<(t: Type): Boolean = t match {
      case _: ObjectType      => true
      case ct: ClassType      =>
        this.allParents.filter(_ =:= ct).size != 0
      case _                  => false
    }
    def >:>(t: Type): Boolean = t match {
      case ct: ClassType      =>
        ct.allParents.filter(this =:= _).size != 0
      case NullType           => true
      case _                  => false
    }


    def show: String = name.asString
  }


  trait ArrayType extends RefType {
    def elemType: Type

    def =:=(t: Type): Boolean = t match {
      case at: ArrayType      => elemType =:= at.elemType
      case _                  => false
    }
    def =/=(t: Type): Boolean = !(this =:= t)
    def <:<(t: Type): Boolean = t match {
      case _: ObjectType      => true
      case _                  => false
    }

    // FIXME: Follow Java's specification
    def >:>(t: Type): Boolean = this =:= t

    def name: Name   = ARRAY_TYPE_NAME
    def show: String = name.asString
  }

  // object StringType extends ClassType {
  //   override def show: String = "String type"
  //   def name: Name = Name("java.lang.String")
  // }


  trait ObjectType extends ClassType {
    val parents: Set[Type] = Set.empty

    override def <:<(t: Type): Boolean = t match {
      case _: ObjectType      => true
      case _                  => false
    }

    override def >:>(t: Type): Boolean = t match {
      case _: RefType         => true
      case _                  => false
    }

    def name: Name = OBJECT_TYPE_NAME
  }

  object NullType extends RefType {
    def =:=(t: Type): Boolean = this == t
    def =/=(t: Type): Boolean = this != t
    def <:<(t: Type): Boolean = t match {
      case _: PrimitiveType   => false
      case _                  => true
    }
    def >:>(t: Type): Boolean = t =:= this


    def show: String = "bottom type"
    def name: Name = Name("NULL")

  }

  trait ObjectTypeExtractor {
    def unapply(ot: ObjectType): Option[TreeId] = ot match {
      case null         => None
      case _            => Some(ot.id)
    }
  }


  trait ObjectTypeFactory {
    private class ObjectTypeImpl(val id: TreeId) extends ObjectType

    def apply(id: TreeId): ObjectType = new ObjectTypeImpl(id)
  }

  trait ClassTypeExtractor {
    def unapply(ct: ClassType): Option[(TreeId, Name, Set[Type])] = ct match {
      case null         => None
      case _            => Some((ct.id, ct.name, ct.parents))
    }
  }


  trait ClassTypeFactory {
    private class ClassTypeImpl(val id: TreeId,
      val name: Name, val parents: Set[Type]) extends ClassType

    def apply(id: TreeId, name: Name, parents: Set[Type]): ClassType =
      new ClassTypeImpl(id, name, parents)
  }

  trait ArrayTypeExtractor {
      def unapply(at: ArrayType): Option[Type] = at match {
      case null         => None
      case _            => Some(at.elemType)
    }
  }


  trait ArrayTypeFactory {
    private class ArrayTypeImpl(val elemType: Type) extends ArrayType

    def apply(elemType: Type): ArrayType = new ArrayTypeImpl(elemType)
  }

  val ClassType  = new ClassTypeExtractor with ClassTypeFactory {}
  val ObjectType = new ObjectTypeExtractor with ObjectTypeFactory {}
}


