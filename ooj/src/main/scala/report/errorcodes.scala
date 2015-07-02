package ch.usi.inf.l3.sana.ooj.report

import ch.usi.inf.l3.sana.tiny
import tiny.report.ErrorCode

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
