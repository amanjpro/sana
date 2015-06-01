package ch.usi.inf.l3.sana.primj.report

import ch.usi.inf.l3.sana.tiny
import tiny.report.ErrorCode

case object VOID_TYPE_VARIABLE extends ErrorCode {
  val message: String = "Void type variable"
}
