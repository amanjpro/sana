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

trait Namers extends names.Namers {
  override type G <: Global
  import global._

  trait Namer extends super.Namer {
    override def nameTrees(tree: Tree): NamerMonad[Tree] = tree match {
      case cse: Case                                 => for {
        r       <- nameCases(cse)
      } yield r
      case _                                         =>
        super.nameTrees(tree)
    }


    def nameCases(cse: Case): NamerMonad[Case] = for {
      guards <- cse.guards.map(nameExprs(_)).sequenceU
      body   <- nameTrees(cse.body)
    } yield Case(guards, body, cse.pos, cse.owner)

    override def nameExprs(expr: Expr): NamerMonad[Expr] = expr match {
      case brk: Break                         => pointSW(brk)
      case cnt: Continue                      => pointSW(cnt)
      case switch: Switch                     => for {
        expr  <- nameExprs(switch.expr)
        cases <- switch.cases.map(nameCases(_)).sequenceU
        dflt  <- nameTrees(switch.default)
      } yield Switch(expr, cases, dflt, switch.pos, switch.owner)
      case lbl: Label                         => nameExprs(lbl.stmt)
      case _                                  =>
        super.nameExprs(expr)
    }
  }
}

