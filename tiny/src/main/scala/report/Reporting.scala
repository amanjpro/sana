package ch.usi.inf.l3.sana.tiny.report

import ch.usi.inf.l3.sana
import sana.tiny
import tiny.source.Position
import tiny.ast.Trees
import tiny.contexts.TreeContexts

import scalaz.{Name => _, _}
import Scalaz._

trait ErrorCode {
  def message: String
  lazy val code: String = this.toString
}
case object BAD_STATEMENT extends ErrorCode {
  val message: String = "Unexpected expressions here"
}
case object TYPE_MISMATCH extends ErrorCode {
  val message: String = "Type mismatch"
}
case object UNEXPETED_TREE extends ErrorCode {
  val message: String = "Unexpected tree"
}

trait Reporting {
  self: Trees with TreeContexts =>

  
  val isTest: Boolean

  def errorCodeToMsg(n: ErrorCode): String = n.message  

  def isErroneous(v: Vector[Failure]): Boolean = 
    v.filter(_.isError) != Vector.empty

  protected def createMessage[T](code: ErrorCode, found: String,
    required: String, pos: Option[Position],
    t: T): String = {
      val msg = errorCodeToMsg(code)
      val col = pos match {
        case None    => 0
        case Some(p) => 4 + p.col
      }
      val caret = if(col == 0) {
        (col * ' ') + "^\n"
      } else ""
      s"""|$msg\n
      |${2 * ' '}$found\n
      |${2 * ' '}$required\n
      |$col$t\n
      |$caret""".stripMargin
  }

  // TODO: Read SanaConfig if we are testing the compiler or not
  protected def createMessageOrGetCode[T](code: ErrorCode, found: String,
    required: String, pos: Option[Position],
    t: T): String = if(isTest) code.code 
                    else 
                      createMessage(code, found, required, pos, t)


  def error[T](code: ErrorCode, found: String, required: String,
    pos: Option[Position],
    t: T): Writer[Vector[Failure], Unit] = 
      Vector(Failure(Error, 
                  createMessageOrGetCode(code, found, required, pos, t),
                  isTest)).tell

  def warning[T](code: ErrorCode, found: String, required: String,
    pos: Option[Position],
    t: T): Writer[Vector[Failure], Unit] = 
      Vector(Failure(Warning, 
                  createMessageOrGetCode(code, found, required, pos, t),
                  isTest)).tell

}
