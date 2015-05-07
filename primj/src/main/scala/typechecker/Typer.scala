package ch.usi.inf.l3.sana.primj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import tiny.util.CompilationUnits
import tiny.passes
import tiny.contexts.TreeContexts
import calcj.typechecker
import calcj.ast.JavaOps._
import primj.ast
import primj.types

import scalaz.Scalaz._
import scalaz._

// TODO: How long should we keep def information in our database?

// From Java Specification 1.0 - Sect: 5.2 - p61
// 1- Assignment Conversion
// 2- Method Conversion   Sect: 5.3 - p66
// 3- String Conversion   Sect: 5.4 - p67

trait Typer extends typechecker.Typers {
  self: ast.Trees with TreeContexts with types.Types with CompilationUnits =>

  trait Typer extends super.Typer {

    override def typeTree(tree: Tree): TreeState[Tree] = tree match {
      case s: Expr  => for {
        ts <- typeExpr(s)
      } yield ts
      case _             => 
        super.typeTree(tree)
    }



    def typeWhile(wile: While): TreeState[While] = for {
      cond <- typeExpr(wile.cond)
      body <- typeExpr(wile.body)
      tpe  <- cond.tpe 
      _    <- (tpe =/= BooleanType) match {
        case true => 
          error(TYPE_MISMATCH,
            cond.tpe.toString, "boolean", wile.cond.pos, wile.cond)
          point(()) 
        case _    => point(())
      }
      tree <- point(While(wile.mods, cond, body, wile.pos))
    } yield tree

    def typeFor(forloop: For): TreeState[For] = for {
      inits <- typeList(typeExpr, forloop.inits).sequenceU
      cond  <- typeExpr(forloop.cond)
      steps <- typeList(typeExpr, forloop.steps).sequenceU
      body  <- typeExpr(forloop.body)
      tpe   <- cond.tpe
      _     <- (tpe =/= BooleanType) match {
        case true =>
          error(TYPE_MISMATCH,
            cond.tpe.toString, "boolean", forloop.cond.pos, forloop.cond)
          point(()) 
        case _    => point(())
      }
      _     <- inits.filter(isValDefOrStatementExpression(_)) match {
        case (x::xs) =>
          error(BAD_STATEMENT, x.toString,
            "An expression statement, or variable declaration", x.pos, x)
          point(())
        case _       => point(())
      }
      _     <- steps.filter(!isValidStatementExpression(_)) match {
        case (x::xs) =>
          error(BAD_STATEMENT, x.toString,
            "An expression statement, or more", x.pos, x)
          point(())
        case _       => point(())
      }
      tree  <- point(For(inits, cond, steps, body, forloop.pos))
    } yield tree

    def typeIf(iff: If): TreeState[If] = for {
      cond  <- typeExpr(iff.cond)
      thenp <- typeExpr(iff.thenp)
      elsep <- typeExpr(iff.elsep)
      tpe   <- cond.tpe
      _     <- (tpe =/= BooleanType) match {
        case true =>
          error(TYPE_MISMATCH,
            cond.tpe.toString, "boolean", iff.cond.pos, iff.cond)
          point(()) 
        case _    => point(())
      }
      tree  <- point(If(cond, thenp, elsep, iff.pos))
    } yield tree

    override def typeExpr(e: Expr): TreeState[Expr] = e match {
      case iff: If       => for {
        ti <- typeIf(iff)
      } yield ti
      case wile: While   => for {
        tw <- typeWhile(wile)
      } yield tw
      case forloop: For  => for {
        tf <- typeFor(forloop)
      } yield tf
      case (_: Lit) | (_: Cast)   => point(e)
      case _                      => 
        super.typeExpr(e)
    }


    def typeList[T <: Tree](f: T => TreeState[T], 
      ls: List[T]): List[TreeState[T]] = {
      val typedList = for {
        l    <- ls
      } yield f(l)
      typedList
    }

    // @tailrec
    // final def typeListCSP[T <: Tree](f: T => TreeState[T], 
    //   ls: List[T], cont: List[T] => List[T]): TreeState[List[T]] = ls match {
    //     case Nil    => (cont(Nil), env)
    //     case x::xs  => 
    //
    //       val t = f(x)
    //       typeListCSP(f, xs, env2, (ts: List[T]) => cont(t::ts))
    //   }
    //
    

    def isValDefOrStatementExpression(v: Expr): Boolean = v match {
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


