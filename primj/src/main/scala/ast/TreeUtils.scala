package ch.usi.inf.l3.sana.primj.ast

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import tiny.ast
import calcj.ast.JavaOps.{Inc, Dec}
import primj.modifiers.Ops._
import primj.contexts.{TreeContexts, TreeContextApis}
import primj.types.Types


trait TreeUtils extends ast.TreeUtils {
  self: Trees with Types with TreeContexts with TreeContextApis =>

  def isValidStatement(e: Tree): Boolean = e match {
    // Statements in primj: if, while, for, block, return, valdef
    // brokenj adds: Switch, continue, break
    case _: If | _: While | _: For | _: Block |
         _: Return | _: ValDef | _: Empty =>
      true
    case _                                =>
      false
  }


  def isSimpleExpression(tree: Tree): Boolean = tree match {
      case _: While                 => false
      case _: For                   => false
      case _: ValDef                => false
      case _: MethodDef             => false
      case _: Template              => false
      case BadTree                  => false
      case _: Return                => false
      case _: If                    => false
      case _: Block                 => false
      case _: TypeUse               => false
      case _                        => true
  }

  def allPathsReturn(tree: Tree,
        ctx: Context): Boolean = tree match {
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
      allPathsReturn(ifelse.thenp, ctx) &&
        allPathsReturn(ifelse.elsep, ctx)
    case wile: While                                   =>
      if(wile.mods.isDoWhile ||
          isConstantExpression(wile.cond, ctx))
        allPathsReturn(wile.body, ctx)
      else false
    case forloop: For                                  =>
      isConstantExpression(forloop.cond, ctx) &&
        allPathsReturn(forloop.body, ctx)
    case block: Block                                  =>
      block.stmts match {
        case Nil         => false
        case stmts       => allPathsReturn(stmts.last, ctx)
      }
    case _                                             =>
      false
  }

  // make sure that the guards are constant expressions Section 15.27
  def isConstantExpression(e: Expr, ctx: Context): Boolean = e match {
    case lit: Lit                                     => true
    case Cast(tpt, e)                                 =>
      // permit casts to primitive and string
      // TODO: Change this in OOJ, to handle String too
      tpt.tpe.eval(ctx) match {
        case _: PrimitiveType      => isConstantExpression(e, ctx)
        case _                     => false
      }
    case u: Unary    if u.op != Inc && u.op != Dec    =>
      isConstantExpression(u.expr, ctx)
    case b: Binary                                    =>
      isConstantExpression(b.lhs, ctx) &&
        isConstantExpression(b.rhs, ctx)
    case id: Ident                                    =>
      ctx.isFinal(id.uses) && ctx.isVariable(id.uses)
    // TODO: Add qualified Select later in ooj
    // TypeName.Identifier only, and only when Identifier is already
    // a final variable
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
  def pointsToUse(tree: Tree,
            p: UseTree => Boolean): Boolean = tree match {
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

