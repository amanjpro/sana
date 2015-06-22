package ch.usi.inf.l3.sana.ooj.types


import ch.usi.inf.l3.sana
import sana.primj
import sana.tiny
import tiny.names.Name
import primj.types

import scala.collection.immutable.Set


trait Types extends types.Types {

  trait RefType extends Type


  trait ClassType extends RefType {
    def parents: Set[ClassType]


    def allParents: Set[ClassType] = 
      parents.flatMap(_.allParents)

    def =:=(t: Type): Boolean = this == t
    def =/=(t: Type): Boolean = this != t
    def <:<(t: Type): Boolean = t match {
      case ObjectType         => true
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


  // object StringType extends ClassType {
  //   override def show: String = "String type"
  //   def name: Name = Name("java.lang.String")
  // }


  object ObjectType extends ClassType {
    val parents: Set[ClassType] = Set.empty

    override def <:<(t: Type): Boolean = t match {
      case ObjectType         => true
      case _                  => false
    }

    override def >:>(t: Type): Boolean = t match {
      case ct: ClassType      =>
        true
      case _                  => false
    }
    def name: Name = Name("java.lang.Object")
  }

  object NullType extends RefType {
    def =:=(t: Type): Boolean = this == t
    def =/=(t: Type): Boolean = this != t
    def <:<(t: Type): Boolean = t match {
      case _: PrimitiveType   => false
      case _                  => true
    }
    def >:>(t: Type): Boolean = t =:= this


    override def show: String = "bottom type"
    def name: Name = Name("NULL")

  }


  trait ClassTypeExtractor {
    def unapply(ct: ClassType): Option[(Name, Set[ClassType])] = ct match {
      case null         => None
      case _            => Some((ct.name, ct.parents))
    }
  }


  trait ClassTypeFactory {
    private class ClassTypeImpl(val name: Name, val parents: Set[ClassType])
      extends ClassType

    def apply(name: Name, parents: Set[ClassType]): ClassType =
      new ClassTypeImpl(name, parents)
  }


  val ClassType = new ClassTypeExtractor with ClassTypeFactory {}
}

