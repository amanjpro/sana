package ch.usi.inf.l3.sana.primj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import tiny.ast
import calcj.ast.JavaOps.{Inc, Dec}
import primj.modifiers._


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


  // TODO: Implement me
  def isSimpleExpression(tree: Tree): Boolean = ???
  
  def allPathsReturn(tree: Tree): Boolean = tree match {
    // tiny
    case BadTree | _: Empty | _: Ident | _: TypeUse    => false
    // calcj
    case _: Cast    | _: Lit   | _: Unary | _: Binary  => false
    // primj
    case _: Template | _: MethodDef                    =>
      // shouldn't happen
      false
    case _: Assign | _: Ternary | _: Apply | _: ValDef =>
      false
    case r: Return                                     =>
      true
    case ifelse: If                                    =>
      allPathsReturn(ifelse.thenp) && allPathsReturn(ifelse.elsep)
    case wile: While                                   =>
      if(wile.mods.isDoWhile ||
          isConstantExpression(wile.cond))
        allPathsReturn(wile.body)
      else false
    case forloop: For                                  =>
      isConstantExpression(forloop.cond) &&
        allPathsReturn(forloop.body)
    case block: Block                                  =>
      block.stmts match {
        case Nil         => false
        case stmts       => allPathsReturn(stmts.last)
      }
  }

  // TODO: Implement me
  // make sure that the guards are constant expressions Section 15.27
  def isConstantExpression(e: Expr): Boolean = ???

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

