package ch.usi.inf.l3.sana.primj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.ast
import calcj.ast.JavaOps.{Inc, Dec}


trait TreeUtils extends ast.TreeUtils {
  self: Trees =>

  def isValidStatement(e: Tree): Boolean = e match {
    // Statements in primj: if, while, for, block, return, valdef
    // brokenj adds: Switch, continue, break
    case _: If | _: While | _: For | _: Block | 
         _: Return | _: ValDef | _: Empty =>
      true
    case _                                =>
      false
  }


  

  // INFO: Update this to Java as we go
  def isExpression(e: Tree): Boolean = e match {
    case _: Lit | _: Ident | _: Binary | _: Unary |
         _: Assign | _: Ternary | _: Apply              => true
    case _                                              => false
  }

  // expressions in primj
  // lit, ident, binary, unary, postfix, assign, ternary,apply
    


  // INFO: Needs to be extended once Select is introduced
  def pointsToUse(expr: Expr, 
            p: UseTree => Boolean): Boolean = expr match {
    case id: Ident         => p(id)
    case tuse: TypeUse     => p(tuse)
    case _                 =>
      false
  }

  def isValDefOrStatementExpression(v: Tree): Boolean = v match {
    case s: ValDef => true
    case e: Expr   => isValidStatementExpression(e)
    case _         => false
  }

  def isValidStatementExpression(e: Tree): Boolean = e match {
    case Unary(_, Inc, _)      => true
    case Unary(_, Dec, _)      => true
    case _: Apply              => true
    // case _: New                => true
    case _: Assign             => true
    case _                     => false
  }
}

