package ch.usi.inf.l3.sana.ooj.report

import ch.usi.inf.l3.sana.tiny
import tiny.report.ErrorCode

case object FIELD_ACCESS_IN_STATIC_CONTEXT extends ErrorCode {
  val message: String = "Accessing non-static field in static context"
}
case object AMBIGUOUS_METHOD_INVOCATION extends ErrorCode {
  val message: String = "Method invocation is ambigouus"
}

case object INSTANCE_METHOD_IN_STATIC_CONTEXT_INVOK extends ErrorCode {
  val message: String = "Invoking an instance method in a static context"
}

case object ACCESSING_THIS_IN_STATIC extends ErrorCode {
  val message: String = "Accessing this in a static context"
}


case object ACCESSING_SUPER_IN_STATIC extends ErrorCode {
  val message: String = "Accessing super in a static context"
}

case object ACCESSING_SUPER_IN_OBJECT extends ErrorCode {
  val message: String = "Accessing super in a Object class"
}


case object BAD_CONSTRUCTOR_NAME extends ErrorCode {
  val message: String = "Bad constructor name"
}


case object NONE_CONCRETE_CLASS_NEW extends ErrorCode {
  val message: String = "Instanciating a none concrete class"
}


case object CONSTRUCTOR_NOT_FOUND extends ErrorCode {
  val message: String = "No such constructor is found"
}
