package ch.usi.inf.l3.sana.primj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import tiny.util.CompilationUnits
import tiny.contexts.DefContext
import tiny.passes
import calcj.symbols
import primj.ast
import primj.types
import calcj.typechecker
import calcj.ast.JavaOps._
import scala.annotation.tailrec


// TODO: How long should we keep def information in our database?

// From Java Specification 1.0 - Sect: 5.2 - p61
// 1- Assignment Conversion
// 2- Method Conversion   Sect: 5.3 - p66
// 3- String Conversion   Sect: 5.4 - p67

trait Typer extends typechecker.Typers {
  self: ast.Trees with symbols.Symbols with types.Types with CompilationUnits =>

  trait Typer extends super.Typer {

    override def typeTree(tree: Tree, env: DefContext): (Tree, DefContext) = 
      tree match {
        case e: Expr       => typeExpr(e, env)
        case s: Statement  => typeStatement(s, env)
        case _             => 
          super.typeTree(tree, env)
      }


    def typeStatement(s: Statement, env: DefContext): (Statement, DefContext) = 
      s match {
        case iff: If       => typeIf(iff, env)
        case wile: While   => typeWhile(wile, env)
        case forloop: For  => typeFor(forloop, env)
      }

    def typeWhile(wile: While, env: DefContext): (While, DefContext) = {
      val (cond, _)  = typeExpr(wile.cond, env)
      val (body, _) = typeStatement(wile.body, env)
      if(cond.tpe != Some(BooleanType)) {
        error(TYPE_MISMATCH,
          cond.tpe.toString, "boolean", wile.cond.pos, wile.cond)
      }
      (While(wile.flags, cond, body, wile.pos), env)
    }

    def typeFor(forloop: For, env: DefContext): (For, DefContext) = {
      val (inits, env1) = typeList(typeStatement, forloop.inits, env)
      val (cond, _)     = typeExpr(forloop.cond, env1)
      val (steps, env2) = typeList(typeExpr, forloop.steps, env1)
      val (body, _)     = typeStatement(forloop.body, env1)
      if(cond.tpe != Some(BooleanType)) {
        error(TYPE_MISMATCH,
          cond.tpe.toString, "boolean", forloop.cond.pos, forloop.cond)
      }
      val noStatementExprInits = inits.filter(isValDeforStatementExpression(_))
      if(noStatementExprInits != Nil) {
        val h = noStatementExprInits.head
        error(BAD_STATEMENT, h.toString,
          "An expression statement, or variable declaration", h.pos, h)
      }

      val noStatementExprSteps = steps.filter(!isValidStatementExpression(_))
      if(noStatementExprSteps != Nil) {
        val h = noStatementExprSteps.head
        error(BAD_STATEMENT, h.toString,
          "An expression statement, or more", h.pos, h)
      }
      (For(inits, cond, steps, body, forloop.pos), env)
    }

    def typeIf(iff: If, env: DefContext): (If, DefContext) = {
      val (cond, _)  = typeExpr(iff.cond, env)
      val (thenp, _) = typeStatement(iff.thenp, env)
      val (elsep, _) = typeStatement(iff.elsep, env)
      if(cond.tpe != Some(BooleanType)) {
        error(TYPE_MISMATCH,
          cond.tpe.toString, "boolean", iff.cond.pos, iff.cond)
      }
      (If(cond, thenp, elsep, iff.pos), env)
    }

    override def typeExpr(e: Expr, 
      env: DefContext): (Expr, DefContext) = e match {
      case (_: Lit) | (_: Cast)   => (e, env)
      case _                      => 
        super.typeExpr(e, env)
    }


    def typeList[T <: Tree](f: (T, DefContext) => (T, DefContext),
      ls: List[T], env: DefContext): (List[T], DefContext) = 
        typeListCSP(f, ls, env, (xs: List[T]) => xs)

    @tailrec
    final def typeListCSP[T <: Tree](f: (T, DefContext) => (T, DefContext), 
      ls: List[T], env: DefContext, 
      cont: List[T] => List[T]): (List[T], DefContext) = ls match {
        case Nil    => (cont(Nil), env)
        case x::xs  => 
          val (t, env2) = f(x, env)
          typeListCSP(f, xs, env2, (ts: List[T]) => cont(t::ts))
      }

    

    def isValDeforStatementExpression(v: Statement): Boolean = v match {
      case s: ValDef => true
      case e: Expr   => isValidStatementExpression(e)
      case _         => false
    }
    def isValidStatementExpression(e: Expr): Boolean = e match {
      case _: Postfix    => true
      case Unary(Inc, _) => true
      case Unary(Dec, _) => true
      case _: Apply      => true
      // case _: New        => true
      case _: Assign     => true
    }
  }
}


