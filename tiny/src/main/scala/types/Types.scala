package ch.usi.inf.l3.sana.tiny.types


trait Types {

  trait Type {
    def =:=(t: Type): Boolean = this == t
  }

  object VoidType extends Type {
    override def toString = "void"
  }

}
