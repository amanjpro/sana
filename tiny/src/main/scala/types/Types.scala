package ch.usi.inf.l3.sana.tiny.types


trait Types {

  trait Type {
    def =:=(t: Type): Boolean = this == t
    def =/=(t: Type): Boolean = this != t
  }

  object VoidType extends Type {
    override def toString = "void"
  }

  object ErrorType extends Type {
    override def toString = "<type error>"
  }
  object NoType extends Type {
    override def toString = "Type is not computed yet"
  }


  def notype: Type = NoType

}
