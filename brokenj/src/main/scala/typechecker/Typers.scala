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
import brokenj.report._

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


    def typeCase(cse: Case, tpe: Type): TypeChecker[Case] = for {
      // TODO:
      // make sure that the guards are constant expressions Section 15.27
      guards  <- cse.guards.map(typeExpr(_)).sequenceU
      ctx     <- get
      _       <- {
        val chks: List[TypeChecker[Unit]] = guards.map((guard) => {
          val gtpe = guard.tpe.eval(ctx)
          if(guard == Default || gtpe <:< tpe)
            point(())
          else
            toTypeChecker(error(TYPE_MISMATCH,
              gtpe.toString, tpe.toString,
                    guard.pos, guard))
        })
        chks.sequenceU
      }
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
      cases   <- switch.cases.map(typeCase(_, ctpe)).sequenceU
      _       <- switch.cases.flatMap((cse) =>
        cse.guards.filter(_ == Default)).size match {
          case 0 | 1                            => point(())
          case n                                =>
            toTypeChecker(error(TOO_MANY_DEFAULT_CASES,
              n.toString, "expected one or zero",
              switch.pos, cond))
        }
    } yield Switch(cond, cases, switch.pos, switch.owner)
  }
}
