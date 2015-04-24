package ch.usi.inf.l3.sana.tiny.report

import ch.usi.inf.l3.sana
import sana.tiny
import tiny.source.Position


trait Reporting {
  val BAD_STATEMENT  = 1
  val TYPE_MISMATCH  = 2
  val UNEXPETED_TREE = 3
  var result: Result = Success


  def errorCodeToMsg(n: Int): String = n match {
    case 1 => "Unexpected expressions here"
    case 2 => "Type mismatch"
    case 3 => "Unexpected tree"
  }
  


  def error(code: Int, found: String, required: String,
    pos: Option[Position],
    t: Any): Unit = 
    result = result ++ Failure(Error, errorCodeToMsg(code), 1, 0)
}
