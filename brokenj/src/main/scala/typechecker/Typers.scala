package ch.usi.inf.l3.sana.brokenj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import tiny.util.{CompilationUnits, MonadUtils}
import tiny.passes
import tiny.report._
import tiny.contexts.TreeContexts
import calcj.ast.JavaOps._
import primj.report._
import primj.typechecker
import brokenj.Global

import scalaz.Scalaz._
import scalaz._

trait Typers extends typechecker.Typers {
  override type G <: Global
  import global._

  import rwst.{local => _, _}
  trait Typer extends super.Typer {


    override def typeExpr(e: Expr): TypeChecker[Expr] = e match {
      case iff: If                => for {
        ti <- typeIf(iff)
      } yield ti
      case wile: While            => for {
        tw <- typeWhile(wile)
      } yield tw
      case forloop: For           => for {
        tf <- typeFor(forloop)
      } yield tf
      case (_: Lit) | (_: Cast)   => point(e)
      case apply: Apply           => for {
        tapp <- typeApply(apply)
      } yield tapp
      case block: Block           => for {
        tblock <- typeBlock(block)
      } yield tblock
      case _                      =>
        super.typeExpr(e)
    }


    def typeCase(cse: Case): TypeChecker[Case] = for {
      // TODO:
      // make sure that the guards are constant expressions Section 15.27
      guards  <- cse.guards.map(typeExpr(_)).sequenceU
      body    <- typeTree(cse.body)
    } yield Case(guards, body, cse.pos, cse.owner)

    // TODO
    // rule-out duplicate constant guards Section 14.9
    def typeSwitch(switch: Switch): TypeChecker[Switch] = for {
      cond    <- typeExpr(switch.expr)
      ctpe    <- toTypeChecker(cond.tpe)
      _       <- if(ctpe =:= CharType  ||
                  ctpe =:= ByteType  ||
                  ctpe =:= ShortType ||
                  ctpe =:= IntType)
                    point(())
                 else
                    toTypeChecker(error(TYPE_MISMATCH,
                      cond.toString, "char, byte, short or int",
                      cond.pos, cond))
      cases   <- switch.cases.map(typeCase(_)).sequenceU
      ctx     <- get
      // _       <- cases.map {
      //   case cse  =>
      //     cse.guards.map {
      //       case guard =>
      //         val gtpe = guard.tpe.eval(ctx)
      //         if(gtpe <:< ctpe)
      //           point(())
      //         else
      //           toTypeChecker(error(TYPE_MISMATCH,
      //             gtpe.toString, ctpe.toString,
      //             guard.pos, guard))
      //     }
      // }
      default <- typeTree(switch.default)
    } yield Switch(cond, cases, default, switch.pos, switch.owner)
  }
}
