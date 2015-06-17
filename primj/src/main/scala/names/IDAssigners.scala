package ch.usi.inf.l3.sana.primj.names

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.source.Position
import tiny.report.Report
import tiny.contexts.{TreeId, NoId}
import tiny.passes
import tiny.debug.logger
import primj.report._
import tiny.names
import primj.Global

 
import scalaz._
import Scalaz._
import scala.language.{higherKinds,implicitConversions}

/**
 * This trait contains IDAssigner trait, where Sana assigns unique `id`
 * to all [[tiny.ast.Trees#IdentifiedTree]]s. This phase also assigns 
 * unique `id` to all [[tiny.util.CompilationUnits#CompilationUnit]]s.
 * Alongside assigning `id`s, this phase also assigns owners to each
 * tree.
 *
 * @author Amanj Sherwany
 * @since 0.1
 * @version 0.1
 */
trait IDAssigners extends passes.Phases {
  type G = Global
  import global._


  trait IDAssigner extends TransformerPhase {

    type IDAssignerMonad[T] = RWST[TreeId, T]
    lazy val rwst = RWST[TreeId]
    import rwst.{local => _, _}

    val name: String = "id-assigner"
    override val description: Option[String] = 
      Some("Add unique keys (ids) to definitions, and assign owners to trees.")
    override def runRightAfter: Option[String] = Some("parser")


    def startPhase(state: Context, unit: CompilationUnit): 
         (Vector[Report], CompilationUnit, Context) = {
      val tree  = unit.tree
      // Extend the tree context with an empty compilation unit context
      val (id, state2) = state.extend(NoId, state)
      val (w, namedTree, s) = assign(tree).run(id, state2)
      logger.debug(namedTree.show(s))
      (w, CompilationUnit(id, namedTree, unit.fileName), s)
    }

    def assign(tree: Tree): IDAssignerMonad[Tree] = tree match {
      case tmpl: Template  => for {
        members <- tmpl.members.map(assignDef(_)).sequenceU
        r       <- point(Template(members, tmpl.owner))
      } yield r
      case dtree: DefTree                            => for {
        r       <- assignDef(dtree)
      } yield r
      case tuse: TypeUse                             => for {
        r       <- assignTypeUse(tuse)
      } yield r
      case e: Expr                                   => for {
        e       <- assignExpr(e)
      } yield e
    }

    def assignDef(dtree: DefTree): IDAssignerMonad[DefTree] = dtree match {
      case meth: MethodDef                           => for {
        r       <- assignMethodDef(meth)
      } yield r
      case valdef: ValDef                            => for {
        r       <- assignValDef(valdef)
      } yield r
    }

    def assignMethodDef(meth: MethodDef): IDAssignerMonad[MethodDef] = for {
      owner   <- ask
      ctx1    <- get
      // _       <- ctx.getContext(owner) match {
      //           case
      //            }.directlyDefines(meth.name) match {
      //              case true =>
      //                error(TYPE_NOT_FOUND,
      //                  "", "a type", meth.pos, meth)
      //              case _    => point(())
      //            }
      id_ctx2 =  ctx1.extend(owner, methodContext(meth))
      id      =  id_ctx2._1
      ctx2    =  id_ctx2._2
      _       <- put(ctx2)
      params  <- meth.params.map {
        case v => local(const(id))(assignValDef(v))
      }.sequenceU
      ret     <- assignTypeUse(meth.ret)
      body    <- local(const(id))(assignExpr(meth.body))
      m       <- point(MethodDef(meth.mods, id, ret, meth.name, 
                params, body, meth.pos, owner))
      // INFO: This line seems to be useless, but we need it to satisfy the type
      // checker
      _       <- point(m)
    } yield m

    def assignValDef(valdef: ValDef): IDAssignerMonad[ValDef] = for {
      owner   <- ask
      ctx1    <- get
      tpt     <- assignTypeUse(valdef.tpt)
      rhs     <- assignExpr(valdef.rhs)
      id_ctx2 =  ctx1.extend(owner, atomicContext(valdef))
      id      =  id_ctx2._1
      ctx2    =  id_ctx2._2
      _       <- put(ctx2)
      v       <- point(ValDef(valdef.mods, id, tpt, valdef.name,
                    rhs, valdef.pos, owner))
      // INFO: This line seems to be useless, but we need it to satisfy the type
      // checker
      _       <- point(v)
    } yield v

    def assignTypeUse(tuse: TypeUse): IDAssignerMonad[TypeUse] = for {
      owner   <- ask
      r       <- point(TypeUse(tuse.uses, tuse.nameAtParser, owner, tuse.pos))
    } yield r

    def assignExpr(expr: Expr): IDAssignerMonad[Expr] = expr match {
      case lit: Lit                                  => for {
        r       <- point(lit)
      } yield r
      case id: Ident                                 => for {
        owner   <- ask
        r       <- point(Ident(id.uses, id.nameAtParser, owner, id.pos))
      } yield r
      case cast: Cast                                => for {
        tpt     <- assignTypeUse(cast.tpt)
        expr    <- assignExpr(cast.expr)
      } yield Cast(tpt, expr, cast.pos)
      case bin: Binary                               => for {
        lhs     <- assignExpr(bin.lhs)
        rhs     <- assignExpr(bin.rhs)
      } yield Binary(lhs, bin.op, rhs, bin.tpe, bin.pos)
      case unary: Unary                              => for {
        expr    <- assignExpr(unary.expr)
      } yield Unary(unary.mods, unary.op, expr, unary.tpe, unary.pos)
      case assign:Assign                             => for {
        owner   <- ask
        lhs     <- assignExpr(assign.lhs)
        rhs     <- assignExpr(assign.rhs)
      } yield Assign(lhs, rhs, assign.pos, owner)
      case ifelse:If                                 => for {
        owner   <- ask
        cond    <- assignExpr(ifelse.cond)
        thenp   <- assignExpr(ifelse.thenp)
        elsep   <- assignExpr(ifelse.elsep)
      } yield If(cond, thenp, elsep, ifelse.pos, owner)
      case wile:While                                => for {
        owner   <- ask
        cond    <- assignExpr(wile.cond)
        body    <- assignExpr(wile.body)
      } yield While(wile.mods, cond, body, wile.pos, owner)
      case block:Block                               => for {
        // INFO: 
        // Every block has a new scope, which means variables 
        // defined in it should have the block as its owner.
        // And since We resole owners by their IDs, that means
        // not all IDs need to have a name.
        // Another thing to notice is that, we need to extend
        // CompilationUnitContext to somehow store these kinds
        // of scoping too.
        // According to this design, method body has its own id
        // and the owner of the body is the method
        // the same thing applies for forloop
        owner   <- ask
        ctx1    <- get
        id_ctx2 =  ctx1.extend(owner, blockContext)
        id      =  id_ctx2._1
        ctx2    =  id_ctx2._2
        _       <- put(ctx2)
        stmts   <- block.stmts.map { case t =>
            local(const(id))(assign(t))
        }.sequenceU
        r       <- point(Block(id, stmts, block.pos, owner))
      } yield r
      case forloop:For                               => for {
        // INFO: 
        // The issue with for loops is similar to the one mentioned
        // in the Block, but here we also should include the variables
        // defined in the inits clause of a for-loop in the newly
        // created scope.
        owner   <- ask
        ctx1    <- get
        id_ctx2 =  ctx1.extend(owner, blockContext)
        id      =  id_ctx2._1
        ctx2    =  id_ctx2._2
        _       <- put(ctx2)
        inits   <- forloop.inits.map { case t =>
            local(const(id))(assign(t))
        }.sequenceU
        cond    <- local(const(id))(assignExpr(forloop.cond))
        steps   <- forloop.steps.map { case e => 
            local(const(id))(assignExpr(e))
        }.sequenceU
        body    <- local(const(id))(assignExpr(forloop.body))
      } yield For(id, inits, cond, steps, body, forloop.pos, owner)
      case ternary:Ternary                           => for {
        owner   <- ask
        cond    <- assignExpr(ternary.cond)
        thenp   <- assignExpr(ternary.thenp)
        elsep   <- assignExpr(ternary.elsep)
      } yield Ternary(cond, thenp, elsep, ternary.tpe, 
                      ternary.pos, owner)
      case apply:Apply                               => for {
        owner   <- ask
        fun     <- assignExpr(apply.fun)
        args    <- apply.args.map(assignExpr(_)).sequenceU
      } yield Apply(fun, args, apply.pos, owner)
      case ret:Return      if ret.expr == None       => for {
        owner   <- ask
        r       <- point(Return(ret.pos, owner))
      } yield r
      case ret:Return                                => for {
        owner   <- ask
        expr    <- assignExpr(ret.expr.get)
      } yield Return(expr, ret.pos, owner)
      case Empty                                     => for {
        r <- point(Empty)
      } yield {
        r
      }
    }
  }
}
