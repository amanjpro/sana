package ch.usi.inf.l3.sana.brokenj.ast

import ch.usi.inf.l3.sana
import sana.calcj
import sana.primj
import primj.ast

import primj.contexts.{TreeContexts, TreeContextApis}
import primj.types.Types

trait TreeUtils extends ast.TreeUtils {
  self: Trees with Types with TreeContexts with TreeContextApis =>

  override def isSimpleExpression(tree: Tree): Boolean = tree match {
    case _: Continue                                   => false
    case _: Break                                      => false
    case _: Case                                       => false
    case _: Switch                                     => false
    case _: Label                                      => false
    case _                                             =>
      super.isSimpleExpression(tree)
  }

  override def allPathsReturn(tree: Tree): Boolean = tree match {
    // brokenj
    case _: Continue | _: Break                        =>
      false
    case label: Label                                  =>
      allPathsReturn(label.stmt)
    case cse: Case                                     =>
      allPathsReturn(cse.body)
    case switch: Switch                                =>
      lazy val cases = switch.cases.foldLeft(true)((z, y) =>
        z || allPathsReturn(y)
      )
      allPathsReturn(switch.default) && cases
    case e                                              =>
      super.allPathsReturn(e)
  }

  def canHaveLabel(tree: Tree): Boolean = tree match {
    // INFO: Synchronize, Throw and Try to be added
    case _: Label | _: If | _: While | _: For | _: Block |
         _: Empty | _: Switch | _: Continue | _: Break   |
         _: Return                                         =>
      true
    case e                                                 =>
      isValidStatementExpression(e)
  }

  def isContinuable(tree: Tree): Boolean = isIterTree(tree)

  def isBreakable(tree: Tree): Boolean = tree match {
    case _: Switch | _: While | _: For    => true
    case _                                => false
  }

  def isIterTree(tree: Tree): Boolean = tree match {
    case _: While | _: For            => true
    case _                            => false
  }

}

