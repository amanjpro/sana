package ch.usi.inf.l3.sana.brokenj.report

import ch.usi.inf.l3.sana.tiny
import tiny.report.ErrorCode

case object TOO_MANY_DEFAULT_CASES extends ErrorCode {
  val message: String = "Too many default cases found"
}
