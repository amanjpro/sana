package ch.usi.inf.l3.sana.primj.names

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.source.Position
import tiny.util.{CompilationUnits,MonadUtils}
import tiny.contexts.{TreeId, NoId}
import tiny.passes
import tiny.debug.logger
import tiny.names.{Namers => _, _}
import tiny.names
import primj.modifiers.Ops._
import primj.Global
import primj.report._
import primj.contexts._

 
import scala.language.higherKinds
import scalaz.{Name => _, Failure => _, _}
import Scalaz._

trait Namers extends names.Namers {

  type G <: Global
  import global._

  trait Namer extends super.Namer {

    import rwst.{local => _, _}

    def nameTrees(tree: Tree): NamerMonad[Tree] = tree match {
      case tmpl: Template  => for {
        r       <- nameTemplates(tmpl)
      } yield r
      case deftree: DefTree                           => for {
       r <- nameDefTrees(deftree)
      } yield r
      case tuse: TypeUse                              => for {
       r  <- nameTypeUses(tuse)
      } yield r
      case expr: Expr                                 => for {
        e <- nameExprs(expr)
      } yield e
    }


    def nameTemplates(tmpl: Template): NamerMonad[Template] = for {
      members <- tmpl.members.map(nameDefTrees(_)).sequenceU
      r       <- point(Template(members, tmpl.owner))
    } yield r

    def nameUseTrees(use: UseTree): NamerMonad[UseTree] = use match {
      case tuse: TypeUse                                => for {
        r <- nameTypeUses(tuse)
      } yield r
      case id: Ident                                    => for {
        r <- nameIdents(id)
      } yield r
    }

    def nameTypeUses(tuse: TypeUse): NamerMonad[TypeUse] = for {
      env  <- get
      name <- point(tuse.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME))
      tid  <- point(env.lookup(name, _.kind.isInstanceOf[TypeKind], 
              tuse.owner))
      _    <- tid match {
                case NoId    =>
                  toNamerMonad(error(TYPE_NOT_FOUND,
                    tuse.toString, "a type", tuse.pos, tuse))
                case tid     =>
                  point(())
              }
    } yield TypeUse(tid, tuse.owner, tuse.pos)

    def nameDefTrees(defTree: DefTree): NamerMonad[DefTree] = defTree match {
      case ttree: TermTree                            => for {
        r  <- nameTermTrees(ttree)
      } yield r
    }

    def nameTermTrees(ttree: TermTree): NamerMonad[TermTree] = ttree match {
      case mtree: MethodDef                           => for {
        r  <- nameMethodDefs(mtree)
      } yield r
      case vtree: ValDef                              => for {
        r  <- nameValDefs(vtree)
      } yield r
    }

    def nameMethodDefs(meth: MethodDef): NamerMonad[MethodDef] = for {
      params  <- meth.params.map(nameValDefs(_)).sequenceU
      ret     <- nameUseTrees(meth.ret)
      body    <- local((_: Set[NamedTree]) => {
                   val pset: Set[NamedTree] = params.toSet
                   pset
                 })(nameExprs(meth.body))
      m       <- point(MethodDef(meth.mods, meth.id, ret, meth.name,
                params, body, meth.pos, meth.owner))
      info    =  newMethodDefInfo(m.mods, m.name, m.tpe)
      _       <- modify(_.update(meth.id, info))
    } yield m


    def nameValDefs(valdef: ValDef): NamerMonad[ValDef] = for {
      tpt     <- nameUseTrees(valdef.tpt)
      rhs     <- nameExprs(valdef.rhs)
      v       <- point(ValDef(valdef.mods, valdef.id, tpt, valdef.name,
                    rhs, valdef.pos, valdef.owner))
      info    =  newValDefInfo(v.mods, v.name, v.tpe)
      _       <- modify(_.update(v.id, info))
    } yield v

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

    def nameIdents(id: Ident): NamerMonad[UseTree] = for {
      env    <- get
      name   <- point(id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME))
      locals <- ask
      tid    <- point(env.lookup(name, 
        alreadyDefinedVariablePredicate(_, locals), id.owner))
      // _      <- tid match {
      //           case NoId    =>
      //             toNamerMonad(error(NAME_NOT_FOUND,
      //               id.toString, "a name", id.pos, id))
      //           case _     =>
      //             point(())
      //         }
     } yield Ident(tid, id.owner, id.pos)

    def nameMethodTreeUses(fun: UseTree): NamerMonad[UseTree] = fun match {
      case id: Ident                                => for {
        env    <- get
        name   <- point(id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME))
        tid    <- point(env.lookup(name, 
          _.kind == MethodKind, id.owner))
        // _      <- tid match {
        //           case NoId    =>
        //             toNamerMonad(error(NAME_NOT_FOUND,
        //               id.toString, "a method name", id.pos, id))
        //           case _     =>
        //             point(())
        //           }
       } yield Ident(tid, id.owner, id.pos)
    }

    def nameMethodUses(fun: Expr): NamerMonad[Expr] = fun match {
      case use: UseTree                   => for {
        r <- nameMethodTreeUses(use)
      } yield r match {
        case e: Expr  => e
        case _        => use
      }
      case _                              =>
        nameExprs(fun)
    }

    def nameExprs(expr: Expr): NamerMonad[Expr] = expr match {
      case lit:Lit                                    => point(lit)
      case id: Ident                                  => for {
        r   <- nameIdents(id)
      } yield r match {
        case id: Expr  => id
        case _         => id
      }
      case cast:Cast                                  => for {
       tpt  <- nameUseTrees(cast.tpt)
       expr <- nameExprs(cast.expr) 
      } yield Cast(tpt, expr, cast.pos)
      case bin:Binary                                 => for {
        lhs <- nameExprs(bin.lhs)
        rhs <- nameExprs(bin.rhs)
      } yield Binary(lhs, bin.op, rhs, bin.tpe, bin.pos)
      case unary:Unary                                => for {
        expr <- nameExprs(unary.expr)
      } yield Unary(unary.mods, unary.op, expr, unary.tpe, unary.pos)
      case assign:Assign                              => for {
        lhs <- nameExprs(assign.lhs)
        rhs <- nameExprs(assign.rhs)
      } yield Assign(lhs, rhs, assign.pos, assign.owner)
      case ifelse:If                                  => for {
        cond  <- nameExprs(ifelse.cond)
        thenp <- nameExprs(ifelse.thenp)
        elsep <- nameExprs(ifelse.elsep)
      } yield If(cond, thenp, elsep, ifelse.pos, ifelse.owner)
      case wile:While                                 => for {
        cond  <- nameExprs(wile.cond)
        body  <- nameExprs(wile.body)
      } yield While(wile.mods, cond, body, wile.pos, wile.owner)
      case block:Block                                => for {
        locals <- ask
        zero   =  (Nil: List[NamerMonad[Tree]], locals)
        stmts2 =  block.stmts.foldLeft(zero)((z, y) => {
                    val locals = y match {
                      case v: ValDef => z._2 + v
                      case _         => z._2
                    }
                    val r = local((s: Set[NamedTree]) => 
                                  locals)(nameTrees(y))
                    (r::z._1, locals)
                  })._1.reverse
        stmts  <- stmts2.sequenceU
        r      <- point(Block(block.id, 
          stmts, block.pos, block.owner))
      } yield r
      case forloop:For                                => for {
        inits <- forloop.inits.map(nameTrees(_)).sequenceU
        vals  =  inits.filter(_.isInstanceOf[ValDef])
        pset  = vals.asInstanceOf[List[NamedTree]].toSet
        cond  <- local((_: Set[NamedTree]) => {
                   pset
                 })(nameExprs(forloop.cond))
        steps <- forloop.steps.map((step) => {
                   local((_: Set[NamedTree]) => {
                   pset
                   })(nameExprs(step))
                 }).sequenceU
        body  <- local((_: Set[NamedTree]) => {
                   pset
                 })(nameExprs(forloop.body))
      } yield For(forloop.id, inits, cond, steps, 
                body, forloop.pos, forloop.owner)
      case ternary:Ternary                            => for {
        cond  <- nameExprs(ternary.cond)
        thenp <- nameExprs(ternary.thenp)
        elsep <- nameExprs(ternary.elsep)
      } yield Ternary(cond, thenp, elsep, ternary.tpe, 
                      ternary.pos, ternary.owner)
      case apply:Apply                                => for {
        fun  <- nameMethodUses(apply.fun)
        args <- apply.args.map(nameExprs(_)).sequenceU
      } yield Apply(fun, args, apply.pos, apply.owner)
      case ret:Return      if ret.expr == None        => point(ret)
      case ret:Return                                 => for {
        expr <- nameExprs(ret.expr.get)
      } yield Return(expr, ret.pos, ret.owner)
      case Empty                                      => for {
        r <- point(Empty)
      } yield {
        r
      }
    }
  }
}
