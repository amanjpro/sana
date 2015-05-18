package ch.usi.inf.l3.sana.tiny.types


trait Types {

  trait Type {
    def =:=(t: Type): Boolean 
    def =/=(t: Type): Boolean
    def <:<(t: Type): Boolean
    def >:>(t: Type): Boolean
  }

  object VoidType extends Type {
    def =:=(t: Type): Boolean = VoidType == t
    def =/=(t: Type): Boolean = VoidType != t
    def <:<(t: Type): Boolean = t =:= this
    def >:>(t: Type): Boolean = t =:= this
    override def toString = "void"
  }

  object ErrorType extends Type {
    def =:=(t: Type): Boolean = false
    def =/=(t: Type): Boolean = true
    def <:<(t: Type): Boolean = t =:= this
    def >:>(t: Type): Boolean = t =:= this
    override def toString = "<type error>"
  }
  object NoType extends Type {
    def =:=(t: Type): Boolean = false
    def =/=(t: Type): Boolean = true
    def <:<(t: Type): Boolean = t =:= this
    def >:>(t: Type): Boolean = t =:= this
    override def toString = "Type is not computed yet"
  }


  def notype: Type = NoType

}
