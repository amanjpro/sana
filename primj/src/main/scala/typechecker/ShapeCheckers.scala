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

import scalaz.Scalaz._
import scalaz._

// TODO: How long should we keep def information in our database?

// From Java Specification 1.0 - Sect: 5.2 - p61
// 1- Assignment Conversion
// 2- Method Conversion   Sect: 5.3 - p66
// 3- String Conversion   Sect: 5.4 - p67

trait ShapeCheckers extends passes.Phases {
  type G <: Global
  import global._

  type ShapeChecker = StateWriter[Unit]

  def toShapeChecker(x: ContextState[Unit]): StateWriter[Unit] =
    toStateWriter(x)
  def toShapeChecker(x: ErrorReportingMonad[Unit]): StateWriter[Unit] =
    toStateWriter(x)

  trait ShapeCheckers extends CheckerPhase {
    
    val name: String = "shape-checkers"
    override val description: Option[String] = 
      Some("Check weather an AST tree is welformed.")
    override def runRightAfter: Option[String] = Some("typer")
    // def startPhase(state: Context, unit: CompilationUnit): 
    //      (Vector[Report], CompilationUnit, Context) = {
    //   val tree  = unit.tree
    //   val (w, (s, typedTree)) = typeTree(tree).run(state).run
    //   (w, CompilationUnit(unit.id, typedTree, unit.fileName), s)
    // }

    def checkTree(tree: Tree): ShapeChecker
    def checkBlock(block: Block): ShapeChecker = for {
      _ <- block.stmts.map(checkTree(_)).sequenceU
      _ <- block.stmts.map(checkStatement(_)).sequenceU
    } yield ()

    def checkIf(ifelse: If): ShapeChecker = for {
      _ <- checkTree(ifelse.cond)
      _ <- checkTree(ifelse.thenp)
      _ <- checkTree(ifelse.elsep)
      _ <- checkExpression(ifelse.cond)
      _ <- checkStatement(ifelse.thenp)
      _ <- checkStatement(ifelse.elsep)
    } yield ()

    def checkWhile(wile: While): ShapeChecker = for {
      _ <- checkTree(wile.cond)
      _ <- checkTree(wile.body)
      _ <- checkExpression(wile.cond)
      _ <- checkStatement(wile.body)
    } yield ()

    def checkFor(forloop: For): ShapeChecker = for {
      _ <- forloop.inits.map(checkTree(_)).sequenceU
      _ <- checkTree(forloop.cond)
      _ <- forloop.steps.map(checkTree(_)).sequenceU
      _ <- checkTree(forloop.body)
      _ <- isValidInitStatements(forloop.inits, forloop)
      _ <- forloop.cond match {
        case Empty => pointSW(())
        case cond  => checkExpression(cond)
      }
      _ <- forloop.steps.map(checkStatement(_)).sequenceU
      _ <- checkStatement(forloop.body)
    } yield ()


    // Return with expr can only appear in non-void methods
    // Return w/o expr can only appear in void methods or constructors
    def checkReturn(ret: Return): ShapeChecker

    def checkValDef(valdef: ValDef): ShapeChecker = for {
      ctx  <- getSW
      _    <- if((ctx.isBlock(valdef.owner) || ctx.isFor(valdef.owner)) &&
                  !(valdef.mods.isLocalVariable)) {
                // TODO: Better error message
                toShapeChecker(error(UNEXPETED_TREE,
                  valdef.toString, "an expression", valdef.pos, valdef))
              } else if(ctx.isMethodDef(valdef.owner) && 
                    !(valdef.mods.isParam)) {
                // TODO: Better error message
                toShapeChecker(error(UNEXPETED_TREE,
                  valdef.toString, "an expression", valdef.pos, valdef))
              } else if(!valdef.mods.isField) {
                // TODO: Better error message
                toShapeChecker(error(UNEXPETED_TREE,
                  valdef.toString, "an expression", valdef.pos, valdef))
              } else pointSW(())
    } yield ()

    def isExpression(e: Tree): Boolean = e match {
      case _: Lit | _: Ident | _: Binary | _: Unary |
           _: Postfix | _: Assign | _: Ternary | _: Apply => true
      case _                                              => false
    }

    // expressions in primj
    // lit, ident, binary, unary, postfix, assign, ternary,apply
    


    def checkExpression(e: Tree): ShapeChecker
    def checkStatement(e: Tree): ShapeChecker = {
      if(isValidStatementExpression(e) || isValidStatement(e))
        pointSW(())
      else
        toShapeChecker(error(BAD_STATEMENT,
          e.toString, "a statement", e.pos, e))
    }


    def isValidStatement(e: Tree): Boolean = e match {
      // Statements in primj: if, while, for, block, return, valdef
      // brokenj adds: Switch, continue, break
      case _: If | _: While | _: For | _: Block | 
           _: Return | _: ValDef | _: Empty =>
        true
      case _                                =>
        false
    }


    def isValidInitStatements(inits: List[Tree], 
            forloop: For): ShapeChecker = {
      if(!allValDefsOrNone(inits))
        // TODO: Better error message
        toShapeChecker(error(UNEXPETED_TREE,
          forloop.toString, "an expression", forloop.pos, forloop))
      else {
        val initsRes = inits.map(isValDefOrStatementExpression(_))
        val empty: Vector[Report] = Vector.empty
        val errors = inits.zip(initsRes).foldLeft(empty)((z, y) => {
          y match {
            case (init, false) =>
              z ++ Vector(genError(UNEXPETED_TREE, init.toString, 
                          "", init.pos, forloop))
            case (_, true)     =>
              z
          }
        })
        if(errors.size == 0) pointSW(())
        else toShapeChecker(errors.tell)
      }
    }

    def allValDefsOrNone(trees: List[Tree]): Boolean = {
      val valdefs = trees.filter(_.isInstanceOf[ValDef])
      valdefs.size == trees.size || valdefs.size == 0
    }

    def isValDefOrStatementExpression(v: Tree): Boolean = v match {
      case s: ValDef => true
      case e: Expr   => isValidStatementExpression(e)
      case _         => false
    }

    def isValidStatementExpression(e: Tree): Boolean = e match {
      case Postfix(_, Inc)    => true
      case Postfix(_, Dec)    => true
      case Unary(Inc, _)      => true
      case Unary(Dec, _)      => true
      case _: Apply           => true
      // case _: New             => true
      case _: Assign          => true
      case _                  => false
    }
    //   override def typeTree(tree: Tree): TypeChecker[Tree] = tree match {
  //     case tmpl: Template  => for {
  //         typedMembers <- tmpl.members.map(typeDefTree(_)).sequenceU
  //         r            <- pointSW(Template(typedMembers, tmpl.owner))
  //       } yield r
  //     case dtree: TermTree => for {
  //       ttree <- typeTermTree(dtree)
  //     } yield ttree
  //     case s: Expr         => for {
  //       ts <- typeExpr(s)
  //     } yield ts
  //     case _               => 
  //       super.typeTree(tree)
  //   }
  //
  //   def typeDefTree(dtree: DefTree): TypeChecker[DefTree] = dtree match {
  //     case ttree: TermTree     => for {
  //       r <- typeTermTree(ttree)
  //     } yield r
  //     case _                   => pointSW(dtree)
  //   }
  //
  //   def typeTermTree(dtree: TermTree): TypeChecker[TermTree] = dtree match {
  //     case vdef: ValDef     => for {
  //       ttree <- typeValDef(vdef)
  //     } yield ttree
  //     case mdef: MethodDef  => for {
  //       ttree <- typeMethodDef(mdef)
  //     } yield ttree
  //   }
  //
  //
  //   def typeMethodDef(mdef: MethodDef): TypeChecker[MethodDef] = for {
  //     params   <- mdef.params.map(typeValDef(_)).sequenceU
  //     body     <- typeExpr(mdef.body)
  //     rhsty    <- toTypeChecker(body.tpe)
  //     rty      <- toTypeChecker(mdef.ret.tpe)
  //     _        <- (rhsty <:< rty) match {
  //       case false =>
  //         toTypeChecker(error(TYPE_MISMATCH,
  //           rhsty.toString, rty.toString, body.pos, mdef))
  //       case true  =>
  //         pointSW(())
  //     }
  //     tree    <- pointSW(MethodDef(mdef.mods, mdef.id, mdef.ret, mdef.name, 
  //                                 params, body, mdef.pos, mdef.owner))
  //   } yield tree
  //
  //   def typeValDef(vdef: ValDef): TypeChecker[ValDef] = for {
  //     rhs      <- typeExpr(vdef.rhs)
  //     rhsty    <- toTypeChecker(rhs.tpe)
  //     ctx      <- getSW
  //     vty      <- toTypeChecker(vdef.tpt.tpe)
  //     _        <- if(vty =:= VoidType) {
  //       toTypeChecker(error(VOID_VARIABLE_TYPE,
  //           vty.toString, vty.toString, rhs.pos, vdef))
  //       // pointSW(())
  //     } else (rhsty <:< vty) match {
  //       case false if !vdef.mods.isParam =>
  //         toTypeChecker(error(TYPE_MISMATCH,
  //           rhsty.toString, vty.toString, rhs.pos, vdef))
  //         // pointSW(())
  //       case _                           =>
  //         pointSW(())
  //     }
  //     tree <- pointSW(ValDef(vdef.mods, vdef.id, vdef.tpt, vdef.name, 
  //                           rhs, vdef.pos, vdef.owner))
  //   } yield tree
  //
  //
  //   override def typeExpr(e: Expr): TypeChecker[Expr] = e match {
  //     case iff: If                => for {
  //       ti <- typeIf(iff)
  //     } yield ti
  //     case wile: While            => for {
  //       tw <- typeWhile(wile)
  //     } yield tw
  //     case forloop: For           => for {
  //       tf <- typeFor(forloop)
  //     } yield tf
  //     case (_: Lit) | (_: Cast)   => pointSW(e)
  //     case apply: Apply           => for {
  //       tapp <- typeApply(apply)
  //     } yield tapp
  //     case block: Block           => for {
  //       tblock <- typeBlock(block)
  //     } yield tblock
  //     case _                      => 
  //       super.typeExpr(e)
  //   }
  //
  //
    //
  //   def typeWhile(wile: While): TypeChecker[While] = for {
  //     cond <- typeExpr(wile.cond)
  //     body <- typeExpr(wile.body)
  //     tpe  <- toTypeChecker(cond.tpe)
  //     _    <- (tpe =/= BooleanType) match {
  //       case true => 
  //         toTypeChecker(error(TYPE_MISMATCH,
  //           tpe.toString, "boolean", wile.cond.pos, wile.cond))
  //       case _    => pointSW(())
  //     }
  //     tree <- pointSW(While(wile.mods, cond, body, wile.pos))
  //   } yield tree
  //
  //   def typeFor(forloop: For): TypeChecker[For] = for {
  //     inits <- forloop.inits.map(typeTree(_)).sequenceU
  //     cond  <- typeExpr(forloop.cond)
  //     steps <- forloop.steps.map(typeExpr(_)).sequenceU
  //     body  <- typeExpr(forloop.body)
  //     tpe   <- toTypeChecker(cond.tpe)
  //     ctx   <- getSW
  //     _     <- (tpe =/= BooleanType && cond != Empty) match {
  //       case true =>
  //         toTypeChecker(error(TYPE_MISMATCH,
  //           tpe.toString, "boolean", forloop.cond.pos, forloop.cond))
  //       case _    => pointSW(())
  //     }
  //     _     <- inits.filter(!isValDefOrStatementExpression(_)) match {
  //       case l@(x::xs) if l.size != inits.size =>
  //         toTypeChecker(error(BAD_STATEMENT, x.toString,
  //           "An expression statement, or variable declaration", x.pos, x))
  //       case _                                 => pointSW(())
  //     }
  //     _     <- steps.filter(!isValidStatementExpression(_)) match {
  //       case l@(x::xs) if l.size != steps.size =>
  //         toTypeChecker(error(BAD_STATEMENT, x.toString,
  //           "An expression statement, or more", x.pos, x))
  //       case _                                 => pointSW(())
  //     }
  //     tree  <- pointSW(For(forloop.id, inits, cond, steps, body, forloop.pos))
  //   } yield tree
  //
  //   def typeIf(iff: If): TypeChecker[If] = for {
  //     cond  <- typeExpr(iff.cond)
  //     thenp <- typeExpr(iff.thenp)
  //     elsep <- typeExpr(iff.elsep)
  //     tpe   <- toTypeChecker(cond.tpe)
  //     _     <- (tpe =/= BooleanType) match {
  //       case true =>
  //         toTypeChecker(error(TYPE_MISMATCH,
  //           tpe.toString, "boolean", iff.cond.pos, iff.cond))
  //       case _    => pointSW(())
  //     }
  //     tree  <- pointSW(If(cond, thenp, elsep, iff.pos))
  //   } yield tree
  //
  //   def typeApply(app: Apply): TypeChecker[Apply] = for {
  //     fun       <- typeExpr(app.fun)
  //     funty     <- toTypeChecker(fun.tpe)
  //     args      <- app.args.map(typeExpr(_)).sequenceU
  //     argtys    <- args.map((x) => toTypeChecker(x.tpe)).sequenceU
  //     _         <- funty match {
  //       case MethodType(r, ts) if checkList[Type](argtys, ts, _ <:< _) =>
  //         pointSW(())
  //       case t: MethodType                                             =>
  //         // TODO: Fix the error message
  //         toTypeChecker(error(TYPE_MISMATCH,
  //           "", "", app.pos, app))
  //       case t                                                         =>
  //         toTypeChecker(error(BAD_STATEMENT,
  //           t.toString, "function/method type", app.pos, app))
  //     }
  //     tree     <- pointSW(Apply(fun, args, app.pos, app.owner))
  //   } yield tree
  //
  //
    //
    
  }
}
