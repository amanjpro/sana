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

/**
 * This trait contains IDAssigner trait, where Sana assigns unique `id`
 * to all [[tiny.ast.Trees#DefTree]]s. This phase also assigns unique `id`
 * to all [[tiny.util.CompilationUnits#CompilationUnit]]s.
 *
 * @author Amanj Sherwany
 * @since 0.1
 * @version 0.1
 */
trait IDAssigners extends passes.Phases {
  type G = Global
  import global._


  val factory = new StateReaderFactory[Int]
  type IDAssignerMonad[T] = factory.StateReader[T]

  trait IDAssigners extends TransformerPhase {

    val name: String = "id-assigner"
    override val description: Option[String] = 
      Some("Add unique keys (ids) to definitions.")
    override def runRightAfter: Option[String] = Some("parser")


    def startPhase(unit: CompilationUnit): 
         (Vector[Report], CompilationUnit) = {
      val tree  = unit.tree
      // Extend the tree context with an empty compilation unit context
      val (id, state) = unit.state.extend(compilationUnitContext)
      val (s, namedTree) = assign(tree).run(state).run(id)
      (Vector.empty, CompilationUnit(id, namedTree, s, unit.fileName))
    }

    def assign(tree: Tree): IDAssignerMonad[Tree] = tree match {
      case tmpl: Template  => for {
        members <- tmpl.members.map(assignDef(_)).sequenceU
        r       <- pointSR(Template(members, tmpl.owner))
      } yield r
      case dtree: DefTree                            => for {
        r       <- assignDef(dtree)
      } yield r
      case tuse: TypeUse                             => pointSR(tuse)
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
      unitId  <- askSR
      ctx     <- getSR
      id      <- pointSR(TreeId(unitId, ctx.nextId(unitId)))
      params  <- meth.params.map(assignValDef(_)).sequenceU
      body    <- assignExpr(meth.body)
    } yield MethodDef(meth.mods, id, meth.ret, meth.name, 
                params, body, meth.pos, meth.owner)

    def assignValDef(valdef: ValDef): IDAssignerMonad[ValDef] = for {
      unitId  <- askSR
      ctx     <- getSR
      id      <- pointSR(TreeId(unitId, ctx.nextId(unitId)))
      rhs     <- assignExpr(valdef.rhs)
    } yield ValDef(valdef.mods, id, valdef.tpt, valdef.name, 
                    rhs, valdef.pos, valdef.owner)


    def assignExpr(expr: Expr): IDAssignerMonad[Expr] = expr match {
      case ifelse:If                                 => for {
        thenp   <- assignExpr(ifelse.thenp)
        elsep   <- assignExpr(ifelse.elsep)
      } yield If(ifelse.cond, thenp, elsep, ifelse.pos, ifelse.owner)
      case wile:While                                => for {
        body    <- assignExpr(wile.body)
      } yield While(wile.mods, wile.cond, body, wile.pos, wile.owner)
      case block:Block                               => for {
        stmts   <- block.stmts.map(assign(_)).sequenceU
        r       <- pointSR(Block(stmts, block.tpe, block.pos, block.owner))
      } yield r
      case forloop:For                               => for {
        inits   <- forloop.inits.map(assign(_)).sequenceU
        body    <- assignExpr(forloop.body)
      } yield For(inits, forloop.cond, forloop.steps, body, 
                  forloop.pos, forloop.owner)
      case _                                         => pointSR(expr)
    }
  }
}
