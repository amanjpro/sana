package ch.usi.inf.l3.sana.primj.names

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.source.Position
import tiny.report.Report
import tiny.contexts.{TreeId, NoId}
import tiny.passes
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


  // INFO: Making this implicit helps us having a better looking local
  // call, again thanks to lambda types
  implicit val factory = new StateReaderFactory[TreeId]
  type IDAssignerMonad[T] = factory.StateReader[T]

  trait IDAssigners extends TransformerPhase {

    val name: String = "id-assigner"
    override val description: Option[String] = 
      Some("Add unique keys (ids) to definitions, and assign owners to trees.")
    override def runRightAfter: Option[String] = Some("parser")


    def startPhase(unit: CompilationUnit): 
         (Vector[Report], CompilationUnit) = {
      val tree  = unit.tree
      // Extend the tree context with an empty compilation unit context
      val (id, state) = unit.state.extend(NoId, emptyContext)
      val (s, namedTree) = assign(tree).run(state).run(id)
      (Vector.empty, CompilationUnit(id, namedTree, s, unit.fileName))
    }

    def assign(tree: Tree): IDAssignerMonad[Tree] = tree match {
      case tmpl: Template  => for {
        members <- tmpl.members.map(assignDef(_)).sequenceU
        r       <- point(Template(members, tmpl.owner))
      } yield r
      case dtree: DefTree                            => for {
        r       <- assignDef(dtree)
      } yield r
      case tuse: TypeUse                             => point(tuse)
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
      ctx     <- get
      id_ctx2 <- point(ctx.extend(owner, methodContext(meth)))
      id      <- point(id_ctx2._1)
      ctx2    <- point(id_ctx2._2)
      _       <- put(ctx2)
      params  <- meth.params.map {
        (v: ValDef) => local[TreeId, ValDef](const(id))(assignValDef(v))
      }.sequenceU
      body    <- assignExpr(meth.body)
      m       <- point(MethodDef(meth.mods, id, meth.ret, meth.name, 
                meth.params, body, meth.pos, owner))
      ctx3    <- point(ctx2.update(id, m))
      _       <- put(ctx3)
      // INFO: This line seems to be useless, but we need it to satisfy the type
      // checker
      _       <- point(m)
    } yield m

    def assignValDef(valdef: ValDef): IDAssignerMonad[ValDef] = for {
      owner   <- ask
      ctx     <- get
      rhs     <- assignExpr(valdef.rhs)
      id_ctx2 <- point(ctx.extend(owner, atomicContext(valdef)))
      id      <- point(id_ctx2._1)
      ctx2    <- point(id_ctx2._2)
      _       <- put(ctx2)
      v       <- point(ValDef(valdef.mods, id, valdef.tpt, valdef.name,
                    rhs, valdef.pos, owner))
      ctx3    <- point(ctx2.update(id, v))
      _       <- put(ctx3)
      // INFO: This line seems to be useless, but we need it to satisfy the type
      // checker
      _       <- point(v)
    } yield v

    def assignTypeUse(tuse: TypeUse): IDAssignerMonad[TypeUse] = for {
        owner   <- ask
        r       <- point(TypeUse(tuse.uses, tuse.nameAtParser, owner, tuse.pos))
    } yield r

    def assignExpr(expr: Expr): IDAssignerMonad[Expr] = expr match {
      case lit: Lit                                  => point(lit)
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
      } yield Unary(unary.op, expr, unary.tpe, unary.pos)
      case postfix: Postfix                          => for {
        expr    <- assignExpr(postfix.expr)
      } yield Postfix(expr, postfix.op, postfix.tpe, postfix.pos)
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
        owner   <- ask
        ctx     <- get
        id_ctx2 <- point(ctx.extend(owner, blockContext))
        id      <- point(id_ctx2._1)
        ctx2    <- point(id_ctx2._2)
        _       <- put(ctx2)
        stmts   <- block.stmts.map((t: Tree) =>
            local(const(id))(assign(t))).sequenceU
        r       <- point(Block(stmts, block.tpe, block.pos, owner))
      } yield r
      case forloop:For                               => for {
        // INFO: 
        // The issue with for loops is similar to the one mentioned
        // in the Block, but here we also should include the variables
        // defined in the inits clause of a for-loop in the newly
        // created scope.
        owner   <- ask
        ctx     <- get
        id_ctx2 <- point(ctx.extend(owner, blockContext))
        id      <- point(id_ctx2._1)
        ctx2    <- point(id_ctx2._2)
        _       <- put(ctx2)
        inits   <- forloop.inits.map((t: Tree) =>
            local(const(id))(assign(t))).sequenceU
        cond    <- local(const(id))(assignExpr(forloop.cond))
        steps   <- forloop.steps.map((e: Expr) => 
            local(const(id))(assignExpr(e))).sequenceU
        body    <- local(const(id))(assignExpr(forloop.body))
      } yield For(inits, cond, steps, body, forloop.pos, owner)
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
    }
  }
}
