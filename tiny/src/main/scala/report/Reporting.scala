package ch.usi.inf.l3.sana.tiny.report

import ch.usi.inf.l3.sana
import sana.tiny
import tiny.source.Position
import tiny.ast.Trees
import tiny.contexts.TreeContexts

import scalaz.{Name => _, _}
import Scalaz._

trait Reporting {
  self: Trees with TreeContexts =>

  val BAD_STATEMENT  = 1
  val TYPE_MISMATCH  = 2
  val UNEXPETED_TREE = 3


  def errorCodeToMsg(n: Int): String = n match {
    case 1 => "Unexpected expressions here"
    case 2 => "Type mismatch"
    case 3 => "Unexpected tree"
  }
  

  protected def createMessage[T](code: Int, found: String,
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

  def error[T](code: Int, found: String, required: String,
    pos: Option[Position],
    t: T): Writer[Vector[Failure], Unit] = 
      Vector(Failure(Error, 
                  createMessage(code, found, required, pos, t))).tell

  def warning[T](code: Int, found: String, required: String,
    pos: Option[Position],
    t: T): Writer[Vector[Failure], Unit] = 
      Vector(Failure(Warning, 
                  createMessage(code, found, required, pos, t))).tell

}
