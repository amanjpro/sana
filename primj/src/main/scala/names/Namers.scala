package ch.usi.inf.l3.sana.primj.names

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.source.Position
import tiny.contexts.TreeContexts
import tiny.util.{CompilationUnits,MonadUtils}
import tiny.contexts.{TreeId, NoId}
import tiny.passes
import tiny.debug.logger
import tiny.names.{Namers => _, _}
import tiny.names
import primj.Global
import primj.report._

 
import scalaz.{Name => _, Failure => _, _}
import scala.language.higherKinds
import Scalaz._

trait Namers extends names.Namers {
  type G <: Global
  import global._

  trait Namer extends super.Namer {

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
      r       <- pointSW(Template(members, tmpl.owner))
    } yield r

      env  <- getSW
      name <- pointSW(tuse.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME))
      tid  <- pointSW(env.lookup(name, _.kind.isInstanceOf[TypeKind], 
              tuse.owner))
      _    <- tid match {
                case NoId    =>
                  toNamerMonad(error(TYPE_NOT_FOUND,
                    tuse.toString, "a type", tuse.pos, tuse))
                case tid     =>
                  pointSW(())
              }
      } yield tuse match {
        case tuse: TypeUse => TypeUse(tid, tuse.owner, tuse.pos)
        case _             => tuse
      }

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
      ret     <- nameTypeUses(meth.ret)
      body    <- nameExprs(meth.body)
      m       <- pointSW(MethodDef(meth.mods, meth.id, ret, meth.name,
                params, body, meth.pos, meth.owner))
      info    =  newMethodDefInfo(m.mods, m.name, m.tpe)
      _       <- modifySW(_.update(meth.id, info))
    } yield m


    def nameValDefs(valdef: ValDef): NamerMonad[ValDef] = for {
      tpt     <- nameTypeUses(valdef.tpt)
      rhs     <- nameExprs(valdef.rhs)
      v       <- pointSW(ValDef(valdef.mods, valdef.id, tpt, valdef.name,
                    rhs, valdef.pos, valdef.owner))
      info    =  newValDefInfo(v.mods, v.name, v.tpe)
      _       <- modifySW(_.update(v.id, info))
    } yield v

    def nameIdents(id: Ident): NamerMonad[Ident] = for {
      env  <- getSW
      name <- pointSW(id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME))
      tid  <- pointSW(env.lookup(name, 
                 _.kind.isInstanceOf[TermKind], id.owner))
      _    <- tid match {
                case NoId    =>
                  toNamerMonad(error(NAME_NOT_FOUND,
                    id.toString, "a name", id.pos, id))
                case _     =>
                  pointSW(())
              }
     } yield Ident(tid, id.owner, id.pos)

    def nameExprs(expr: Expr): NamerMonad[Expr] = expr match {
      case lit:Lit                                    => pointSW(lit)
      case id: Ident                                  => for {
        r   <- nameIdents(id)
      } yield r
      case cast:Cast                                  => for {
       tpt  <- nameTypeUses(cast.tpt)
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
        stmts <- block.stmts.map(nameTrees(_)).sequenceU
        r     <- pointSW(Block(block.id, 
          stmts, block.pos, block.owner))
      } yield r
      case forloop:For                                => for {
        inits <- forloop.inits.map(nameTrees(_)).sequenceU
        cond  <- nameExprs(forloop.cond)
        steps <- forloop.steps.map(nameExprs(_)).sequenceU
        body  <- nameExprs(forloop.body)
      } yield For(forloop.id, inits, cond, steps, 
                body, forloop.pos, forloop.owner)
      case ternary:Ternary                            => for {
        cond  <- nameExprs(ternary.cond)
        thenp <- nameExprs(ternary.thenp)
        elsep <- nameExprs(ternary.elsep)
      } yield Ternary(cond, thenp, elsep, ternary.tpe, 
                      ternary.pos, ternary.owner)
      case apply:Apply                                => for {
        fun  <- nameExprs(apply.fun)
        args <- apply.args.map(nameExprs(_)).sequenceU
      } yield Apply(fun, args, apply.pos, apply.owner)
      case ret:Return      if ret.expr == None        => pointSW(ret)
      case ret:Return                                 => for {
        expr <- nameExprs(ret.expr.get)
      } yield Return(expr, ret.pos, ret.owner)
      case Empty                                      => for {
        r <- pointSW(Empty)
      } yield {
        r
      }
    }
  }
}
