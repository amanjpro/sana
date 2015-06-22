package ch.usi.inf.l3.sana.primj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import tiny.util.{CompilationUnits, MonadUtils}
import tiny.passes
import tiny.report._
import tiny.contexts.TreeContexts
import calcj.typechecker
import calcj.ast.JavaOps._
import primj.report._
import primj.Global
import primj.modifiers._
import primj.modifiers.Ops._

import scalaz.Scalaz._
import scalaz._

// TODO: How long should we keep def information in our database?

// From Java Specification 1.0 - Sect: 5.2 - p61
// 1- Assignment Conversion
// 2- Method Conversion   Sect: 5.3 - p66
// 3- String Conversion   Sect: 5.4 - p67

trait Typers extends typechecker.Typers {
  override type G <: Global
  import global._

  trait Typer extends super.Typer {

    override def typeTree(tree: Tree): TypeChecker[Tree] = tree match {
      case tmpl: Template  => for {
          typedMembers <- tmpl.members.map(typeDefTree(_)).sequenceU
          r            <- pointSW(Template(typedMembers, tmpl.owner))
        } yield r
      case dtree: TermTree => for {
        ttree <- typeTermTree(dtree)
      } yield ttree
      case s: Expr         => for {
        ts <- typeExpr(s)
      } yield ts
      case _               => 
        super.typeTree(tree)
    }

    def typeDefTree(dtree: DefTree): TypeChecker[DefTree] = dtree match {
      case ttree: TermTree     => for {
        r <- typeTermTree(ttree)
      } yield r
      case _                   => pointSW(dtree)
    }

    def typeTermTree(dtree: TermTree): TypeChecker[TermTree] = dtree match {
      case vdef: ValDef     => for {
        ttree <- typeValDef(vdef)
      } yield ttree
      case mdef: MethodDef  => for {
        ttree <- typeMethodDef(mdef)
      } yield ttree
    }


    def typeMethodDef(mdef: MethodDef): TypeChecker[MethodDef] = for {
      params   <- mdef.params.map(typeValDef(_)).sequenceU
      body     <- typeExpr(mdef.body)
      rhsty    <- toTypeChecker(body.tpe)
      rty      <- toTypeChecker(mdef.ret.tpe)
      _        <- (rhsty <:< rty) match {
        case false if rty =/= VoidType =>
          toTypeChecker(error(TYPE_MISMATCH,
            rhsty.toString, rty.toString, body.pos, mdef))
        case _                         =>
          pointSW(())
      }
      _        <- if(rty =/= VoidType && !allPathsReturn(body))
                    toTypeChecker(error(MISSING_RETURN_STATEMENT,
                      body.toString, body.toString, body.pos, mdef))
                  else
                    pointSW(())
      tree     <- pointSW(MethodDef(mdef.mods, mdef.id, mdef.ret, mdef.name, 
                                  params, body, mdef.pos, mdef.owner))
    } yield tree

    def typeValDef(vdef: ValDef): TypeChecker[ValDef] = for {
      rhs      <- typeExpr(vdef.rhs)
      rhsty    <- toTypeChecker(rhs.tpe)
      ctx      <- getSW
      vty      <- toTypeChecker(vdef.tpt.tpe)
      _        <- if(vty =:= VoidType) {
        toTypeChecker(error(VOID_VARIABLE_TYPE,
            vty.toString, vty.toString, rhs.pos, vdef))
      } else if(vdef.mods.isFinal && !vdef.mods.isParam &&
                vdef.rhs == Empty) {
        toTypeChecker(error(UNINITIALIZED_FINAL_VARIABLE,
            vdef.toString, "", vdef.pos, vdef))

      } else (rhsty <:< vty) match {
          case false if !vdef.mods.isParam =>
            toTypeChecker(error(TYPE_MISMATCH,
              rhsty.toString, vty.toString, rhs.pos, vdef))
          case _                           =>
            pointSW(())
        }
      tree <- pointSW(ValDef(vdef.mods, vdef.id, vdef.tpt, vdef.name, 
                    rhs, vdef.pos, vdef.owner))
    } yield tree


    def typeAssign(assign: Assign): TypeChecker[Assign] = for {
      lhs  <- typeExpr(assign.lhs)
      ctx  <- getSW
      _    <- if(! pointsToUse(lhs, x => ctx.isVariable(x.uses)))
                toTypeChecker(error(ASSIGNING_NOT_TO_VARIABLE,
                  lhs.toString, lhs.toString, lhs.pos, lhs))
              else if(! pointsToUse(lhs, x => ctx.isFinal(x.uses)))
                toTypeChecker(error(REASSIGNING_FINAL_VARIABLE,
                  lhs.toString, lhs.toString, lhs.pos, lhs))
              else pointSW(())
      rhs  <- typeExpr(assign.rhs)
      ltpe <- toTypeChecker(lhs.tpe)
      rtpe <- toTypeChecker(rhs.tpe)
      _    <- if (ltpe <:< rtpe)
                pointSW(())
              else 
                toTypeChecker(error(TYPE_MISMATCH,
                  ltpe.toString, rtpe.toString, rhs.pos, assign))
    } yield Assign(lhs, rhs, assign.pos, assign.owner)


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
      case (_: Lit) | (_: Cast)   => pointSW(e)
      case apply: Apply           => for {
        tapp <- typeApply(apply)
      } yield tapp
      case block: Block           => for {
        tblock <- typeBlock(block)
      } yield tblock
      case assign: Assign         => for {
        tassign <- typeAssign(assign)
      } yield tassign
      case ret: Return         => for {
        tret    <- typeReturn(ret)
      } yield tret
      case _                      => 
        super.typeExpr(e)
    }


    def typeReturn(ret: Return): TypeChecker[Return] = for {
      ctx    <- getSW
      expr   <- ret.expr.map(typeExpr(_)).getOrElse(pointSW(Empty))
      mtpe   <- ctx.getTree(ctx.enclosingMethod(ret.owner)) match {
        case None                => pointSW(ErrorType)
        case Some(info)          => toTypeChecker(info.tpe)
      }
      ret2   <- ret.expr match {
        case None       => pointSW(Return(ret.pos, ret.owner))
        case Some(e)    => pointSW(Return(e, ret.pos, ret.owner))
      }
      rtpe   <- toTypeChecker(ret2.tpe)
      _      <- ret2.expr match {
        // add constructor later
        case None     if mtpe =/= VoidType    =>
          toTypeChecker(error(VOID_RETURN,
            ret.toString, ret.toString, ret.pos, ret))
        case Some(e)  if mtpe =:= VoidType    =>
          toTypeChecker(error(NON_VOID_RETURN,
            ret.toString, ret.toString, ret.pos, ret))
        case Some(e)                          =>
          if(rtpe <:< mtpe)
            pointSW(())
          else
            toTypeChecker(error(TYPE_MISMATCH,
              rtpe.toString, mtpe.toString, ret.pos, ret))

      }
    } yield ret2

    override def typeUnary(unary: Unary): TypeChecker[Unary] = for {
      ctx    <- getSW
      expr   = unary.expr
      _      <- if(unary.op == Inc || unary.op == Dec) {
                  if(! pointsToUse(expr, x => ctx.isVariable(x.uses)))
                     toTypeChecker(error(ASSIGNING_NOT_TO_VARIABLE,
                       expr.toString, expr.toString, expr.pos, expr))
                  else if(! pointsToUse(expr, x => ctx.isFinal(x.uses)))
                    toTypeChecker(error(REASSIGNING_FINAL_VARIABLE,
                      expr.toString, expr.toString, expr.pos, expr))
                  else pointSW(())
                } else pointSW(())
      res    <- super.typeUnary(unary)
    } yield res
      
      
      
    def typeBlock(block: Block): TypeChecker[Block] = for {
      stmts <- block.stmts.map(typeTree(_)).sequenceU
    } yield Block(block.id, stmts, block.pos, block.owner)

    def typeWhile(wile: While): TypeChecker[While] = for {
      cond <- typeExpr(wile.cond)
      body <- typeExpr(wile.body)
      tpe  <- toTypeChecker(cond.tpe)
      _    <- (tpe =/= BooleanType) match {
        case true => 
          toTypeChecker(error(TYPE_MISMATCH,
            tpe.toString, "boolean", wile.cond.pos, wile.cond))
        case _    => pointSW(())
      }
      tree <- pointSW(While(wile.mods, cond, body, wile.pos))
    } yield tree

    def typeFor(forloop: For): TypeChecker[For] = for {
      inits <- forloop.inits.map(typeTree(_)).sequenceU
      cond  <- typeExpr(forloop.cond)
      steps <- forloop.steps.map(typeExpr(_)).sequenceU
      body  <- typeExpr(forloop.body)
      tpe   <- toTypeChecker(cond.tpe)
      ctx   <- getSW
      _     <- (tpe =/= BooleanType && cond != Empty) match {
        case true =>
          toTypeChecker(error(TYPE_MISMATCH,
            tpe.toString, "boolean", forloop.cond.pos, forloop.cond))
        case _    => pointSW(())
      }
      // _     <- inits.filter(!isValDefOrStatementExpression(_)) match {
      //   case l@(x::xs) if l.size != inits.size =>
      //     toTypeChecker(error(BAD_STATEMENT, x.toString,
      //       "An expression statement, or variable declaration", x.pos, x))
      //   case _                                 => pointSW(())
      // }
      // _     <- steps.filter(!isValidStatementExpression(_)) match {
      //   case l@(x::xs) if l.size != steps.size =>
      //     toTypeChecker(error(BAD_STATEMENT, x.toString,
      //       "An expression statement, or more", x.pos, x))
      //   case _                                 => pointSW(())
      // }
      tree  <- pointSW(For(forloop.id, inits, cond, steps, body, forloop.pos))
    } yield tree

    def typeIf(iff: If): TypeChecker[If] = for {
      cond  <- typeExpr(iff.cond)
      thenp <- typeExpr(iff.thenp)
      elsep <- typeExpr(iff.elsep)
      tpe   <- toTypeChecker(cond.tpe)
      _     <- (tpe =/= BooleanType) match {
        case true =>
          toTypeChecker(error(TYPE_MISMATCH,
            tpe.toString, "boolean", iff.cond.pos, iff.cond))
        case _    => pointSW(())
      }
      tree  <- pointSW(If(cond, thenp, elsep, iff.pos))
    } yield tree

    // FIXME: Apply doesn't work with method overloading
    def typeApply(app: Apply): TypeChecker[Apply] = for {
      fun       <- typeExpr(app.fun)
      funty     <- toTypeChecker(fun.tpe)
      args      <- app.args.map(typeExpr(_)).sequenceU
      argtys    <- args.map((x) => toTypeChecker(x.tpe)).sequenceU
      _         <- funty match {
        case MethodType(r, ts) if checkList[Type](argtys, ts, _ <:< _) =>
          pointSW(())
        case t: MethodType                                             =>
          // TODO: Fix the error message
          toTypeChecker(error(TYPE_MISMATCH,
            "", "", app.pos, app))
        case t                                                         =>
          toTypeChecker(error(BAD_STATEMENT,
            t.toString, "function/method type", app.pos, app))
      }
      tree     <- pointSW(Apply(fun, args, app.pos, app.owner))
    } yield tree


    

  }
}
