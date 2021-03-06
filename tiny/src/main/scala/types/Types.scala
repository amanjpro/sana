package ch.usi.inf.l3.sana.tiny.types


import ch.usi.inf.l3.sana.tiny.names.Name


trait Types {

  trait Type {
    def =:=(t: Type): Boolean 
    def =/=(t: Type): Boolean
    def <:<(t: Type): Boolean
    def >:>(t: Type): Boolean

    def show: String
    def name: Name

    override final def toString = show

  }

  object VoidType extends Type {
    def =:=(t: Type): Boolean = VoidType == t
    def =/=(t: Type): Boolean = VoidType != t
    def <:<(t: Type): Boolean = t =:= this
    def >:>(t: Type): Boolean = t =:= this

    def name: Name = Name("void")

    override def show: String = "void type"
  }

  object ErrorType extends Type {
    def =:=(t: Type): Boolean = false
    def =/=(t: Type): Boolean = true
    def <:<(t: Type): Boolean = t =:= this
    def >:>(t: Type): Boolean = t =:= this

    def name: Name = Name("<error>")
    override def show: String = "<type error>"
  }
  object NoType extends Type {
    def =:=(t: Type): Boolean = false
    def =/=(t: Type): Boolean = true
    def <:<(t: Type): Boolean = t =:= this
    def >:>(t: Type): Boolean = t =:= this

    def name: Name = Name("<no-type>")
    override def show: String = "<no type>"
  }


  def notype: Type = NoType

}
