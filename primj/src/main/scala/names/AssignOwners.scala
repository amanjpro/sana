package ch.usi.inf.l3.sana.primj.names

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.report.Report
import tiny.contexts._
import tiny.passes
import tiny.names
import primj.Global

 
import scalaz.{Name => _, Failure => _, _}
import scala.language.{higherKinds,implicitConversions}
import Scalaz._

/**
 * This trait contains a compilation phase which adds contextual
 * owner information of trees, for example in `java` if we have 
 * the following example:
 * 
 * {{{ 
 * int m(int x, int y) {
 *   int z = x;
 *   while(z > 0 && y != x) {
 *     int v = 10;
 *     z--;
 *   }
 *   return x + y + z;
 * }
 * }}}
 * 
 * The owner of method `m` is the containing class that contains it,
 * the owner of variables `x`, `y`, `z`, `v` is method `m`. 
 * 
 * Since at this phase, the uses are not bound to the definitions (i.e. 
 * `uses` field is not assigned to the definitions they refer to), it is 
 * essential to keep the `nameAtParser` as is, otherwise the compiler 
 * cannot resolve them.
 *
 * @author Amanj Sherwany
 * @since 0.1
 * @version 0.1
 */
trait AssignOwners extends passes.Phases {
  type G = Global
  import global._

  val factory = new StateReaderFactory[TreeId]
  type OwnerAssignerMonad[T] = factory.StateReader[T]

  trait OwnerAssigner extends TransformerPhase {
    

    val name: String = "owner-assigner"
    override val description: Option[String] = 
      Some("Add contextual owners to trees.")
    override def runRightAfter: Option[String] = Some("id-assigner")


    def startPhase(unit: CompilationUnit): 
         (Vector[Report], CompilationUnit) = {
      val tree  = unit.tree
      val state = unit.state
      val (s, namedTree) = assign(tree).run(state).run(NoId)
      (Vector.empty, CompilationUnit(unit.id, namedTree, s, unit.fileName))
    }

    def assign(tree: Tree): OwnerAssignerMonad[Tree] = tree match {
      case tmpl: Template  => for {
        members <- tmpl.members.map(assignDef(_)).sequenceU
        r       <- pointSR(Template(members, tmpl.owner))
      } yield r
      case dtree: TermTree                           => for {
        r       <- assignDef(dtree)
      } yield r
      case tuse: TypeUse                             => for {
        r       <- assignTypeUse(tuse)
      } yield r
      case e: Expr                                   => for {
        e       <- assignExpr(e)
      } yield e
    }

    def assignDef(dtree: DefTree): OwnerAssignerMonad[DefTree]
    def assignTerm(dtree: TermTree): OwnerAssignerMonad[TermTree] = 
      dtree match {
        case meth: MethodDef                           => for {
          r       <- assignMethodDef(meth)
        } yield r
        case valdef: ValDef                            => for {
          r       <- assignValDef(valdef)
        } yield r
      }

    def assignMethodDef(meth: MethodDef): OwnerAssignerMonad[MethodDef] = for {
      owner   <- askSR
      ret     <- assignTypeUse(meth.ret)
      params  <- meth.params.map(assignValDef(_)).sequenceU
      body    <- localSR((_: TreeId) => meth.id)(assignExpr(meth.body))
    } yield MethodDef(meth.mods, meth.id, ret, meth.name, 
                params, body, meth.pos, owner)

    def assignValDef(valdef: ValDef): OwnerAssignerMonad[ValDef] = for {
      owner   <- askSR
      rhs     <- assignExpr(valdef.rhs)
      tpt     <- assignTypeUse(valdef.tpt)
    } yield ValDef(valdef.mods, valdef.id, tpt, valdef.name, 
                    rhs, valdef.pos, owner)

    def assignTypeUse(tuse: TypeUse): OwnerAssignerMonad[TypeUse] = for {
        owner   <- askSR
        r       <- pointSR(TypeUse(tuse.uses, tuse.nameAtParser, owner, tuse.pos))
    } yield r

    def assignExpr(expr: Expr): OwnerAssignerMonad[Expr] = expr match {
      case lit: Lit                                  => pointSR(lit)
      case id: Ident                                 => for {
        owner   <- askSR
        r       <- pointSR(Ident(id.uses, id.nameAtParser, owner, id.pos))
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
        owner   <- askSR
        lhs     <- assignExpr(assign.lhs)
        rhs     <- assignExpr(assign.rhs)
      } yield Assign(lhs, rhs, assign.pos, owner)
      case ifelse:If                                 => for {
        owner   <- askSR
        cond    <- assignExpr(ifelse.cond)
        thenp   <- assignExpr(ifelse.thenp)
        elsep   <- assignExpr(ifelse.elsep)
      } yield If(cond, thenp, elsep, ifelse.pos, owner)
      case wile:While                                => for {
        owner   <- askSR
        cond    <- assignExpr(wile.cond)
        body    <- assignExpr(wile.body)
      } yield While(wile.mods, cond, body, wile.pos, owner)
      case block:Block                               => for {
        // FIXME: 
        // Every block has a new scope, which means variables 
        // defined in it should have the block as its owner.
        // And since We resole owners by their IDs, that means
        // not all IDs need to have a name.
        // Another thing to notice is that, we need to extend
        // CompilationUnitContext to somehow store these kinds
        // of scoping too.
        owner   <- askSR
        stmts   <- block.stmts.map(assign(_)).sequenceU
        r       <- pointSR(Block(stmts, block.tpe, block.pos, owner))
      } yield r
      case forloop:For                               => for {
        // FIXME: 
        // The issue with for loops is similar to the one mentioned
        // in the Block, but here we also should include the variables
        // defined in the inits clause of a for-loop in the newly
        // created scope.
        owner   <- askSR
        inits   <- forloop.inits.map(assign(_)).sequenceU
        cond    <- assignExpr(forloop.cond)
        steps   <- forloop.steps.map(assignExpr(_)).sequenceU
        body    <- assignExpr(forloop.body)
      } yield For(inits, cond, steps, body, forloop.pos, owner)
      case ternary:Ternary                           => for {
        owner   <- askSR
        cond    <- assignExpr(ternary.cond)
        thenp   <- assignExpr(ternary.thenp)
        elsep   <- assignExpr(ternary.elsep)
      } yield Ternary(cond, thenp, elsep, ternary.tpe, 
                      ternary.pos, owner)
      case apply:Apply                               => for {
        owner   <- askSR
        fun     <- assignExpr(apply.fun)
        args    <- apply.args.map(assignExpr(_)).sequenceU
      } yield Apply(fun, args, apply.pos, owner)
      case ret:Return      if ret.expr == None       => for {
        owner   <- askSR
        r       <- pointSR(Return(ret.pos, owner))
      } yield r
      case ret:Return                                => for {
        owner   <- askSR
        expr    <- assignExpr(ret.expr.get)
      } yield Return(expr, ret.pos, owner)
    }
  }
}
