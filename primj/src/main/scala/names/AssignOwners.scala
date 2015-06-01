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
import scala.language.{higherKinds,implicitConversions}
import Scalaz._

trait AssignOwners extends passes.Phases {
  self: Trees with 
        TreeContexts with 
        Types with 
        CompilationUnits with
        Reporting with
        MonadUtils =>

  type Inner[A]                       = ReaderT[Id, Option[TreeId], A]
  type Outer[F[_], A]                 = StateT[F, TreeContext, A]
  type Stacked[A]                     = Outer[Inner, A]
  type OwnerAssignerMonad[T <: Tree]  = Stacked[T]

  implicit def toOwnerAssigner[A](x: Outer[Id, A]): Stacked[A] = 
    x.lift[Inner]
  implicit def outer2Inner[A](x: Stacked[A]): Inner[A] = x.lift[Id]

  trait OwnerAssigner extends TransformerPhase {
    private type ST[C, A] = StateT[Inner, C, A]
    private type RD[C, A] = ReaderT[Id, C, A]
    protected def point[A](t: A): Outer[Inner, A] = t.point[Stacked]
    protected def ask: Stacked[Option[TreeId]] = 
      MonadReader[RD, Option[TreeId]].ask.liftM[Outer]
    protected def local[A](f: Option[TreeId] => Option[TreeId])
        (fa: Inner[A]): Stacked[A] = {
      val r = MonadReader[RD, Option[TreeId]].local(f)(fa)
      r.liftM[Outer]
    }
    protected def get: Stacked[TreeContext] = 
      MonadState[ST, TreeContext].get
    protected def put(env: TreeContext): Stacked[Unit] = 
      MonadState[ST, TreeContext].put(env)
    protected def modify(f: TreeContext => TreeContext): Stacked[Unit] = 
      MonadState[ST, TreeContext].modify(f)
    


    val name: String = "owner-assigner"
    override val description: Option[String] = 
      Some("Identify tree owners and assign them.")
    override def runRightAfter: Option[String] = Some("parser")


    def startPhase(unit: CompilationUnit): 
         (Vector[Failure], CompilationUnit) = {
      val tree  = unit.tree
      val state = unit.state
      val (s, namedTree) = assign(tree).run(state).run(None)
      (Vector.empty, CompilationUnit(namedTree, s, unit.fileName))
    }

    def assign(tree: Tree): OwnerAssignerMonad[Tree] = tree match {
      case dtree: TermTree                           => for {
        r       <- assignDef(dtree)
      } yield r
      case tuse: TypeUse                             => for {
        r       <- assignTpt(tuse)
      } yield r
      case e: Expr                                   => for {
        e       <- assignExpr(e)
      } yield e
    }

    def assignDef(dtree: TermTree): OwnerAssignerMonad[TermTree] = dtree match {
      case meth: MethodDef                           => for {
        r       <- assignMethodDef(meth)
      } yield r
      case valdef: ValDef                            => for {
        r       <- assignValDef(valdef)
      } yield r
    }

    def assignMethodDef(meth: MethodDef): OwnerAssignerMonad[MethodDef] = for {
      owner   <- ask
      ret     <- assignTpt(meth.ret)
      params  <- meth.params.map(assignValDef(_)).sequenceU
      body    <- local((x) => Some(meth.id))(assignExpr(meth.body))
    } yield MethodDef(meth.mods, meth.id, ret, meth.name, 
                params, body, meth.pos, owner)

    def assignValDef(valdef: ValDef): OwnerAssignerMonad[ValDef] = for {
      owner   <- ask
      rhs     <- assignExpr(valdef.rhs)
      tpt     <- assignTpt(valdef.tpt)
    } yield ValDef(valdef.mods, valdef.id, valdef.tpt, valdef.name, 
                    rhs, valdef.pos, owner)

    def assignTpt(tuse: TypeUse): OwnerAssignerMonad[TypeUse] = for {
        owner   <- ask
        r       <- point(TypeUse(tuse.uses, tuse.name, owner, tuse.pos))
    } yield r

    def assignExpr(expr: Expr): OwnerAssignerMonad[Expr] = expr match {
      case lit: Lit                                  => point(lit)
      case id: Ident                                 => for {
        owner   <- ask
        r       <- point(Ident(id.uses, id.name, owner, id.pos))
      } yield r
      case cast: Cast                                => for {
        tpt     <- assignTpt(cast.tpt)
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
        owner   <- ask
        stmts   <- block.stmts.map(assign(_)).sequenceU
        r       <- point(Block(stmts, block.tpe, block.pos, owner))
      } yield r
      case forloop:For                               => for {
        owner   <- ask
        inits   <- forloop.inits.map(assignExpr(_)).sequenceU
        cond    <- assignExpr(forloop.cond)
        steps   <- forloop.steps.map(assignExpr(_)).sequenceU
        body    <- assignExpr(forloop.body)
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
