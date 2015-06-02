package ch.usi.inf.l3.sana.primj.names

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.source.Position
import tiny.contexts.TreeContexts
import tiny.util.{CompilationUnits,MonadUtils}
import tiny.contexts.TreeId
import tiny.report._
import tiny.passes
import tiny.names
import primj.ast.Trees
import primj.types.Types

 
import scalaz.{Name => _, Failure => _, _}
import scala.language.higherKinds
import Scalaz._

trait Namers extends names.Namers {
  self: Trees with 
        TreeContexts with 
        Types with 
        CompilationUnits with
        Reporting with
        MonadUtils =>

  trait Namer extends super.Namer {
    def canRedefine: Boolean

    def named(tree: Tree): NamerMonad[Tree] 
    def nameDef(defTree: TermTree): NamerMonad[TermTree]
    def bindUse(tree: Tree): NamerMonad[Tree] = tree match {
      case deftree: TermTree                          => for {
       r <- bindUseDefs(deftree)
      } yield r
      case tuse: TypeUse                              => for {
       r  <- bindUseType(tuse)
      } yield r
      case expr: Expr                                 => for {
        e <- bindUseExpr(expr)
      } yield e
    }

    def bindUseType(tuse: TypeUse): NamerMonad[TypeUse] = for {
      env  <- get
      tid  = env.lookup(tuse.name)
    } yield TypeUse(tid, tuse.name, tuse.owner, tuse.pos)

    def bindUseDefs(defTree: TermTree): NamerMonad[TermTree] 
    //= defTree match {
    //   case m:MethodDef                                => for {
    //      
    //   }
    // }

    def bindUseExpr(expr: Expr): NamerMonad[Expr] = expr match {
      case lit:Lit                                    => point(lit)
      case id: Ident                                  => for {
       env  <- get
       tid  = env.lookup(id.name)
      } yield Ident(tid, id.name, id.owner, id.pos)
      case cast:Cast                                  => for {
       tpt  <- bindUseType(cast.tpt)
       expr <- bindUseExpr(cast.expr) 
      } yield Cast(tpt, expr, cast.pos)
      case bin:Binary                                 => for {
        lhs <- bindUseExpr(bin.lhs)
        rhs <- bindUseExpr(bin.rhs)
      } yield Binary(lhs, bin.op, rhs, bin.tpe, bin.pos)
      case unary:Unary                                => for {
        expr <- bindUseExpr(unary.expr)
      } yield Unary(unary.op, expr, unary.tpe, unary.pos)
      case postfix:Postfix                            => for {
        expr <- bindUseExpr(postfix.expr)
      } yield Postfix(expr, postfix.op, postfix.tpe, postfix.pos)
      case assign:Assign                              => for {
        lhs <- bindUseExpr(assign.lhs)
        rhs <- bindUseExpr(assign.rhs)
      } yield Assign(lhs, rhs, assign.pos, assign.owner)
      case ifelse:If                                  => for {
        cond  <- bindUseExpr(ifelse.cond)
        thenp <- bindUseExpr(ifelse.thenp)
        elsep <- bindUseExpr(ifelse.elsep)
      } yield If(cond, thenp, elsep, ifelse.pos, ifelse.owner)
      case wile:While                                 => for {
        cond  <- bindUseExpr(wile.cond)
        body  <- bindUseExpr(wile.body)
      } yield While(wile.mods, cond, body, wile.pos, wile.owner)
      case block:Block                                => for {
        stmts <- block.stmts.map(bindUse(_)).sequenceU
        r     <- point(Block(stmts, block.tpe, block.pos, block.owner))
      } yield r
      case forloop:For                                => for {
        inits <- forloop.inits.map(bindUse(_)).sequenceU
        cond  <- bindUseExpr(forloop.cond)
        steps <- forloop.steps.map(bindUseExpr(_)).sequenceU
        body  <- bindUseExpr(forloop.body)
      } yield For(inits, cond, steps, body, forloop.pos, forloop.owner)
      case ternary:Ternary                            => for {
        cond  <- bindUseExpr(ternary.cond)
        thenp <- bindUseExpr(ternary.thenp)
        elsep <- bindUseExpr(ternary.elsep)
      } yield Ternary(cond, thenp, elsep, ternary.tpe, 
                      ternary.pos, ternary.owner)
      case apply:Apply                                => for {
        fun  <- bindUseExpr(apply.fun)
        args <- apply.args.map(bindUseExpr(_)).sequenceU
      } yield Apply(fun, args, apply.pos, apply.owner)
      case ret:Return      if ret.expr == None        => point(ret)
      case ret:Return                                 => for {
        expr <- bindUseExpr(ret.expr.get)
      } yield Return(expr, ret.pos, ret.owner)
    }
  }
}
