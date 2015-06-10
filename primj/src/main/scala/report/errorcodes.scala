package ch.usi.inf.l3.sana.primj.report

import ch.usi.inf.l3.sana.tiny
import tiny.report.ErrorCode

case object NAME_NOT_FOUND extends ErrorCode {
  val message: String = "Name not found"
}

case object VOID_VARIABLE_TYPE extends ErrorCode {
  val message: String = "Void type variable"
}


case object TYPE_NOT_FOUND extends ErrorCode {
  val message: String = "Type not found"
}
