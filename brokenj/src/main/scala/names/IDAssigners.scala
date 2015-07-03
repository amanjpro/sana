package ch.usi.inf.l3.sana.brokenj.names

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.brokenj

import tiny.source.Position
import tiny.contexts.TreeContexts
import tiny.util.{CompilationUnits,MonadUtils}
import tiny.contexts.{TreeId, NoId}
import tiny.passes
import tiny.debug.logger


import primj.report._
import primj.names

import brokenj.Global

import scalaz.{Name => _, Failure => _, _}
import scala.language.higherKinds
import Scalaz._


trait IDAssigners extends names.IDAssigners {
  override type G <: Global
  import global._

  trait IDAssigner extends super.IDAssigner {
    import rwst.{local => _, _}
    override def assign(tree: Tree): IDAssignerMonad[Tree] = tree match {
      case cse: Case                                 => for {
        r       <- assignCase(cse)
      } yield r
      case _                                         =>
        super.assign(tree)
    }


    def assignCase(cse: Case): IDAssignerMonad[Case] = for {
      owner  <- ask
      guards <- cse.guards.map(assignExpr(_)).sequenceU
      body   <- assign(cse.body)
    } yield Case(guards, body, cse.pos, owner)

    override def assignExpr(expr: Expr): IDAssignerMonad[Expr] = expr match {
      case brk: Break                         => for {
        owner <- ask
      } yield Break(brk.label, brk.pos, owner)
      case cnt: Continue                      => for {
        owner <- ask
      } yield Continue(cnt.label, cnt.pos, owner)
      case switch: Switch                     => for {
        owner <- ask
        expr  <- assignExpr(switch.expr)
        cases <- switch.cases.map(assignCase(_)).sequenceU
        dflt  <- assign(switch.default)
      } yield Switch(expr, cases, dflt, switch.pos, owner)
      case lbl: Label                         => for {
        owner <- ask
        stmt  <- assignExpr(lbl.stmt)
      } yield Label(lbl.name, stmt, lbl.pos, owner)
      case _                                  =>
        super.assignExpr(expr)
    }
  }
}

