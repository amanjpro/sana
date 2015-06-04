package ch.usi.inf.l3.sana.primj.names

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.source.Position
import tiny.report.Report
import tiny.contexts.TreeId
import tiny.passes
import tiny.names
import primj.Global

 
import scalaz._
import Scalaz._
import scala.language.{higherKinds,implicitConversions}

trait IDAssigners extends passes.Phases {
  type G = Global
  import global._


  val factory = new StateReaderFactory[Int]
  type OwnerAssignerMonad[T] = factory.StateReader[T]

  trait IDAssigners extends TransformerPhase {

    val name: String = "id-assigner"
    override val description: Option[String] = 
      Some("Add unique keys (ids) to definitions.")
    override def runRightAfter: Option[String] = Some("parser")


    def startPhase(unit: CompilationUnit): 
         (Vector[Report], CompilationUnit) = {
      val tree  = unit.tree
      // Extend the tree context with an empty compilation unit context
      val state = unit.state.extend(compilationUnitContext)
      val (s, namedTree) = assign(tree).run(state).run(0)
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
      // owner   <- askSR
      ret     <- assignTpt(meth.ret)
      params  <- meth.params.map(assignValDef(_)).sequenceU
      body    <- localSR((_: Int) => 0)(assignExpr(meth.body))
    } yield MethodDef(meth.mods, meth.id, ret, meth.name, 
                params, body, meth.pos, meth.owner)

    def assignValDef(valdef: ValDef): OwnerAssignerMonad[ValDef] = for {
      // owner   <- askSR
      rhs     <- assignExpr(valdef.rhs)
      tpt     <- assignTpt(valdef.tpt)
    } yield ValDef(valdef.mods, valdef.id, valdef.tpt, valdef.name, 
                    rhs, valdef.pos, valdef.owner)

    def assignTpt(tuse: TypeUse): OwnerAssignerMonad[TypeUse] = for {
        owner   <- askSR
        r       <- pointSR(TypeUse(tuse.uses, tuse.name, tuse.owner, tuse.pos))
    } yield r

    def assignExpr(expr: Expr): OwnerAssignerMonad[Expr] = expr match {
      case lit: Lit                                  => pointSR(lit)
      case id: Ident                                 => for {
        owner   <- askSR
        r       <- pointSR(Ident(id.uses, id.name, id.owner, id.pos))
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
        owner   <- askSR
        lhs     <- assignExpr(assign.lhs)
        rhs     <- assignExpr(assign.rhs)
      } yield Assign(lhs, rhs, assign.pos, assign.owner)
      case ifelse:If                                 => for {
        owner   <- askSR
        cond    <- assignExpr(ifelse.cond)
        thenp   <- assignExpr(ifelse.thenp)
        elsep   <- assignExpr(ifelse.elsep)
      } yield If(cond, thenp, elsep, ifelse.pos, ifelse.owner)
      case wile:While                                => for {
        owner   <- askSR
        cond    <- assignExpr(wile.cond)
        body    <- assignExpr(wile.body)
      } yield While(wile.mods, cond, body, wile.pos, wile.owner)
      case block:Block                               => for {
        owner   <- askSR
        stmts   <- block.stmts.map(assign(_)).sequenceU
        r       <- pointSR(Block(stmts, block.tpe, block.pos, block.owner))
      } yield r
      case forloop:For                               => for {
        owner   <- askSR
        inits   <- forloop.inits.map(assign(_)).sequenceU
        cond    <- assignExpr(forloop.cond)
        steps   <- forloop.steps.map(assignExpr(_)).sequenceU
        body    <- assignExpr(forloop.body)
      } yield For(inits, cond, steps, body, forloop.pos, forloop.owner)
      case ternary:Ternary                           => for {
        owner   <- askSR
        cond    <- assignExpr(ternary.cond)
        thenp   <- assignExpr(ternary.thenp)
        elsep   <- assignExpr(ternary.elsep)
      } yield Ternary(cond, thenp, elsep, ternary.tpe, 
                      ternary.pos, ternary.owner)
      case apply:Apply                               => for {
        owner   <- askSR
        fun     <- assignExpr(apply.fun)
        args    <- apply.args.map(assignExpr(_)).sequenceU
      } yield Apply(fun, args, apply.pos, apply.owner)
      case ret:Return      if ret.expr == None       => for {
        owner   <- askSR
        r       <- pointSR(Return(ret.pos, ret.owner))
      } yield r
      case ret:Return                                => for {
        owner   <- askSR
        expr    <- assignExpr(ret.expr.get)
      } yield Return(expr, ret.pos, ret.owner)
    }
  }
}

