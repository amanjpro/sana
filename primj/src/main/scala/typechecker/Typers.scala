package ch.usi.inf.l3.sana.primj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import tiny.util.{CompilationUnits, MonadUtils}
import tiny.passes
import tiny.report._
import tiny.contexts.{TreeContexts, NoId}
import tiny.names.{Namers => _, _}
import calcj.typechecker
import calcj.ast.JavaOps._
import primj.report._
import primj.Global
import primj.modifiers._
import primj.modifiers.Ops._

import scalaz.{Name => _, Failure => _, _}
import scalaz.Scalaz._

// TODO: How long should we keep def information in our database?

// From Java Specification 1.0 - Sect: 5.2 - p61
// 1- Assignment Conversion
// 2- Method Conversion   Sect: 5.3 - p66
// 3- String Conversion   Sect: 5.4 - p67

trait Typers extends typechecker.Typers {

  override type G <: Global
  import global._

  import rwst.{local => _, _}
  trait Typer extends super.Typer {

    override def typeTree(tree: Tree): TypeChecker[Tree] = tree match {
      case tmpl: Template  => for {
          typedMembers <- tmpl.members.map(typeDefTree(_)).sequenceU
          r            <- point(Template(typedMembers, tmpl.owner))
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
      case _                   => point(dtree)
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
      body     <- local((_: Set[NamedTree]) => {
                   val pset: Set[NamedTree] = params.toSet
                   pset
                 })(typeExpr(mdef.body))
      rhsty    <- toTypeChecker(body.tpe)
      rty      <- toTypeChecker(mdef.ret.tpe)
      _        <- (rhsty <:< rty) match {
        case false if rty =/= VoidType =>
          toTypeChecker(error(TYPE_MISMATCH,
            rhsty.toString, rty.toString, body.pos, mdef))
        case _                         =>
          point(())
      }
      _        <- if(rty =/= VoidType && !allPathsReturn(body))
                    toTypeChecker(error(MISSING_RETURN_STATEMENT,
                      body.toString, body.toString, body.pos, mdef))
                  else
                    point(())
      tree     <- point(MethodDef(mdef.mods, mdef.id, mdef.ret, mdef.name, 
                                  params, body, mdef.pos, mdef.owner))
    } yield tree

    def typeValDef(vdef: ValDef): TypeChecker[ValDef] = for {
      // Name it's type-use, if it is not named yet
      isNamed    <- point(vdef.tpt.uses != NoId)
      tpt        <- if(isNamed) point(vdef.tpt) else typeUseTree(vdef.tpt)
      _          <- if(isNamed) {
        val info = newValDefInfo(vdef.mods, vdef.name, tpt.tpe)
        modify(_.update(vdef.id, info))
      } else point(())
      // Now name it
      rhs        <- typeExpr(vdef.rhs)
      rhsty      <- toTypeChecker(rhs.tpe)
      vty        <- toTypeChecker(tpt.tpe)
      _          <- if(vty =:= VoidType) {
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
            point(())
        }
      tree       <- point(ValDef(vdef.mods, vdef.id, tpt, vdef.name, 
                    rhs, vdef.pos, vdef.owner))
    } yield tree


    def typeAssign(assign: Assign): TypeChecker[Assign] = for {
      lhs  <- typeExpr(assign.lhs)
      ctx  <- get
      _    <- if(! pointsToUse(lhs, x => ctx.isVariable(x.uses)))
                toTypeChecker(error(ASSIGNING_NOT_TO_VARIABLE,
                  lhs.toString, lhs.toString, lhs.pos, lhs))
              else if(! pointsToUse(lhs, x => ctx.isFinal(x.uses)))
                toTypeChecker(error(REASSIGNING_FINAL_VARIABLE,
                  lhs.toString, lhs.toString, lhs.pos, lhs))
              else point(())
      rhs  <- typeExpr(assign.rhs)
      ltpe <- toTypeChecker(lhs.tpe)
      rtpe <- toTypeChecker(rhs.tpe)
      _    <- if (ltpe <:< rtpe)
                point(())
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
      case (_: Lit) | (_: Cast)   => point(e)
      case apply: Apply           => for {
        tapp <- typeApply(apply)
      } yield tapp
      case block: Block           => for {
        tblock <- typeBlock(block)
      } yield tblock
      case assign: Assign         => for {
        tassign <- typeAssign(assign)
      } yield tassign
      case ret: Return            => for {
        tret    <- typeReturn(ret)
      } yield tret
      case id: Ident              => for {
        tid     <- typeIdent(id)
      } yield tid match {
        case id: Expr   => id
        case _          => id
      }
      case _                      => 
        super.typeExpr(e)
    }


    def typeReturn(ret: Return): TypeChecker[Return] = for {
      ctx    <- get
      expr   <- ret.expr.map(typeExpr(_)).getOrElse(point(Empty))
      mtpe   <- ctx.getTree(ctx.enclosingMethod(ret.owner)) match {
        case None                => point(ErrorType)
        case Some(info)          => toTypeChecker(info.tpe)
      }
      ret2   <- ret.expr match {
        case None       => point(Return(ret.pos, ret.owner))
        case Some(e)    => point(Return(e, ret.pos, ret.owner))
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
            point(())
          else
            toTypeChecker(error(TYPE_MISMATCH,
              rtpe.toString, mtpe.toString, ret.pos, ret))

      }
    } yield ret2

    override def typeUnary(unary: Unary): TypeChecker[Unary] = for {
      ctx    <- get
      locals <- ask
      expr   <- typeExpr(unary.expr)
      _      <- if(unary.op == Inc || unary.op == Dec) {
                  if(! pointsToUse(expr, x => ctx.isVariable(x.uses)))
                    toTypeChecker(error(ASSIGNING_NOT_TO_VARIABLE,
                       expr.toString, expr.toString, expr.pos, expr))
                  else if(! pointsToUse(expr, x => ctx.isFinal(x.uses)))
                    toTypeChecker(error(REASSIGNING_FINAL_VARIABLE,
                      expr.toString, expr.toString, expr.pos, expr))
                  else point(())
                } else point(())
      res    <- super.typeUnary(unary)
    } yield res
      
      
      
    def typeBlock(block: Block): TypeChecker[Block] = for {
      locals <- ask
      zero   =  (Nil: List[TypeChecker[Tree]], locals)
      stmts2 =  block.stmts.foldLeft(zero)((z, y) => {
                  val locals = y match {
                    case v: ValDef => z._2 + v
                    case _         => z._2
                  }
                  val r = local((s: Set[NamedTree]) => 
                                locals)(typeTree(y))
                  (r::z._1, locals)
                })._1.reverse
      stmts  <- stmts2.sequenceU
      // r      <- point(Block(block.id, stmts, block.pos, block.owner))
      // stmts <- block.stmts.map(typeTree(_)).sequenceU
    } yield Block(block.id, stmts, block.pos, block.owner)

    def typeWhile(wile: While): TypeChecker[While] = for {
      cond <- typeExpr(wile.cond)
      body <- typeExpr(wile.body)
      tpe  <- toTypeChecker(cond.tpe)
      _    <- (tpe =/= BooleanType) match {
        case true => 
          toTypeChecker(error(TYPE_MISMATCH,
            tpe.toString, "boolean", wile.cond.pos, wile.cond))
        case _    => point(())
      }
      tree <- point(While(wile.mods, cond, body, wile.pos))
    } yield tree
    
    def typeFor(forloop: For): TypeChecker[For] = for {
      inits <- forloop.inits.map(typeTree(_)).sequenceU
      vals  =  inits.filter(_.isInstanceOf[ValDef])
      pset  = vals.asInstanceOf[List[NamedTree]].toSet
      cond  <- local((_: Set[NamedTree]) => pset)(typeExpr(forloop.cond))
      steps <- forloop.steps.map((step) => {
                 local((_: Set[NamedTree]) => {
                 pset
                 })(typeExpr(step))
               }).sequenceU
      body  <- local((_: Set[NamedTree]) => pset)(typeExpr(forloop.body))
      tpe   <- toTypeChecker(cond.tpe)
      ctx   <- get
      _     <- (tpe =/= BooleanType && cond != Empty) match {
        case true =>
          toTypeChecker(error(TYPE_MISMATCH,
            tpe.toString, "boolean", forloop.cond.pos, forloop.cond))
        case _    => point(())
      }
      // _     <- inits.filter(!isValDefOrStatementExpression(_)) match {
      //   case l@(x::xs) if l.size != inits.size =>
      //     toTypeChecker(error(BAD_STATEMENT, x.toString,
      //       "An expression statement, or variable declaration", x.pos, x))
      //   case _                                 => point(())
      // }
      // _     <- steps.filter(!isValidStatementExpression(_)) match {
      //   case l@(x::xs) if l.size != steps.size =>
      //     toTypeChecker(error(BAD_STATEMENT, x.toString,
      //       "An expression statement, or more", x.pos, x))
      //   case _                                 => point(())
      // }
      tree  <- point(For(forloop.id, inits, cond, steps, body, forloop.pos))
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
        case _    => point(())
      }
      tree  <- point(If(cond, thenp, elsep, iff.pos))
    } yield tree

    // FIXME: Apply doesn't work with method overloading
    def typeApply(app: Apply): TypeChecker[Apply] = for {
      fun       <- typeExpr(app.fun)
      funty     <- toTypeChecker(fun.tpe)
      args      <- app.args.map(typeExpr(_)).sequenceU
      argtys    <- args.map((x) => toTypeChecker(x.tpe)).sequenceU
      _         <- funty match {
        case MethodType(r, ts) if checkList[Type](argtys, ts, _ <:< _) =>
          point(())
        case t: MethodType                                             =>
          // TODO: Fix the error message
          toTypeChecker(error(TYPE_MISMATCH,
            "", "", app.pos, app))
        case t                                                         =>
          toTypeChecker(error(BAD_STATEMENT,
            t.toString, "function/method type", app.pos, app))
      }
      tree     <- point(Apply(fun, args, app.pos, app.owner))
    } yield tree




    // Type-checking (and name resolving) UseTrees
    protected def alreadyDefinedVariablePredicate(x: TreeInfo, 
          locals: Set[NamedTree]): Boolean = {
      // points to a local var? then it should be defined already
      val localPred = ((x.mods.isLocalVariable ||
        x.mods.isParam) && 
        !locals.filter(_.name == x.name).isEmpty)
      // it is global? Then, there should be no locals a long 
      // the way to the global
      val globalPred = x.mods.isField
      (x.kind == VariableKind) && (localPred || globalPred)
    }
    
    def typeIdent(id: Ident): TypeChecker[UseTree] = for {
      env    <- get
      name   <- point(id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME))
      locals <- ask
      tid    <- point(env.lookup(name, 
        alreadyDefinedVariablePredicate(_, locals), id.owner))
      _      <- tid match {
                case NoId    =>
                  toTypeChecker(error(NAME_NOT_FOUND,
                    id.toString, "a name", id.pos, id))
                case _     =>
                  point(())
              }
     } yield Ident(tid, id.owner, id.pos)


    // def nameMethodTreeUses(fun: UseTree): TypeChecker[UseTree] = fun match {
    //   case id: Ident                                => for {
    //     env    <- get
    //     name   <- point(id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME))
    //     tid    <- point(env.lookup(name, 
    //       _.kind == MethodKind, id.owner))
    //     // _      <- tid match {
    //     //           case NoId    =>
    //     //             toTypeChecker(error(NAME_NOT_FOUND,
    //     //               id.toString, "a method name", id.pos, id))
    //     //           case _     =>
    //     //             point(())
    //     //           }
    //    } yield Ident(tid, id.owner, id.pos)
    // }
    //
    // def nameMethodUses(fun: Expr): TypeChecker[Expr] = fun match {
    //   case use: UseTree                   => for {
    //     r <- nameMethodTreeUses(use)
    //   } yield r match {
    //     case e: Expr  => e
    //     case _        => use
    //   }
    //   case _                              =>
    //     nameExprs(fun)
    // }

    
    def typeUseTree(use: UseTree): TypeChecker[UseTree] = use match {
      case tuse: TypeUse                                => for {
        r <- typeTypeUse(tuse)
      } yield r
      case id: Ident                                    => for {
        r <- typeIdent(id)
      } yield r
      case _                                            => point(use)
    }

    def typeTypeUse(tuse: TypeUse): TypeChecker[TypeUse] = for {
      env  <- get
      name <- point(tuse.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME))
      tid  <- point(env.lookup(name, _.kind.isInstanceOf[TypeKind], 
              tuse.owner))
      _    <- tid match {
                case NoId    =>
                  toTypeChecker(error(TYPE_NOT_FOUND,
                    tuse.toString, "a type", tuse.pos, tuse))
                case tid     =>
                  point(())
              }
    } yield TypeUse(tid, tuse.owner, tuse.pos)
  }
}
