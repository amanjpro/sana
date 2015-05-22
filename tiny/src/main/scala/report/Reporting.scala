package ch.usi.inf.l3.sana.tiny.report

import ch.usi.inf.l3.sana
import sana.tiny
import tiny.source.Position
import tiny.ast.Trees
import tiny.contexts.TreeContexts

import scalaz.{Name => _, _}
import Scalaz._

trait ErrorCode
case object BAD_STATEMENT extends ErrorCode
case object TYPE_MISMATCH extends ErrorCode
case object UNEXPETED_TREE extends ErrorCode

trait Reporting {
  self: Trees with TreeContexts =>

  
  def errorCodeToMsg(n: ErrorCode): String = n match {
    case BAD_STATEMENT => "Unexpected expressions here"
    case TYPE_MISMATCH => "Type mismatch"
    case UNEXPETED_TREE => "Unexpected tree"
  }
  

  def isErroneous(v: Vector[Failure]): Boolean = 
    v.filter(_.isError) == Vector.empty

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

  def error[T](code: ErrorCode, found: String, required: String,
    pos: Option[Position],
    t: T): Writer[Vector[Failure], Unit] = 
      Vector(Failure(Error, 
                  createMessage(code, found, required, pos, t))).tell

  def warning[T](code: ErrorCode, found: String, required: String,
    pos: Option[Position],
    t: T): Writer[Vector[Failure], Unit] = 
      Vector(Failure(Warning, 
                  createMessage(code, found, required, pos, t))).tell

}
