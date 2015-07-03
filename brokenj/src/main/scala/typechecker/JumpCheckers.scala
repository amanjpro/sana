package ch.usi.inf.l3.sana.brokenj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import tiny.util.{CompilationUnits, MonadUtils}
import tiny.passes
import tiny.modifiers.Flag
import tiny.report._
// import tiny.contexts.TreeContexts
import calcj.typechecker
import calcj.ast.JavaOps._
import primj.report._
import primj.modifiers._
import brokenj.Global
import scala.language.higherKinds

import scalaz.Scalaz._
import scalaz._

trait JumpCheckers extends passes.Phases {
  type G <: Global
  import global._

  type JumpChecker = ReaderWriter[List[Tree], Unit]

  def toJumpChecker(x: Reader[List[Tree], Unit]):
    ReaderWriter[List[Tree], Unit] = toReaderWriter(x)

  def toJumpChecker(x: ErrorReportingMonad[Unit]):
    ReaderWriter[List[Tree], Unit] = toReaderWriter(x)

  trait Checker extends CheckerPhase {

    val name: String = "jump-checkers"
    override val description: Option[String] =
      Some("Check validity of continue/break statements.")
    override def runRightAfter: Option[String] = Some("label-checkers")

    def startPhase(state: Context, unit: CompilationUnit):
         Vector[Report] = {
      val tree  = unit.tree
      val (w, _) = checkTree(tree).run(Nil).run
      w
    }

    def checkTree(tree: Tree): JumpChecker = tree match {
      // primj
      case tmpl: Template                   => for {
        _       <- tmpl.members.map(checkTree(_)).sequenceU
      } yield ()
      case mthd: MethodDef                  => for {
        _       <- mthd.params.map(checkTree(_)).sequenceU
        _       <- checkTree(mthd.body)
      } yield ()
      case valdef: ValDef                   => for {
        _       <- checkTree(valdef.rhs)
      } yield ()
      case Return(Some(expr))               => checkTree(expr)
      case _: Return                        => pointRW(())
      case apply: Apply                     => for {
        _       <- checkTree(apply.fun)
        _       <- apply.args.map(checkTree(_)).sequenceU
      } yield ()
      case ternary: Ternary                 => for {
        _       <- checkTree(ternary.cond)
        _       <- checkTree(ternary.thenp)
        _       <- checkTree(ternary.elsep)
      } yield ()
      case block: Block                     => for {
        _       <- block.stmts.map(checkTree(_)).sequenceU
      } yield ()
      case forloop: For                     => for {
        _       <- forloop.inits.map(checkTree(_)).sequenceU
        _       <- checkTree(forloop.cond)
        _       <- forloop.steps.map(checkTree(_)).sequenceU
        _       <-
          localRW[List[Tree], Unit](l => forloop::l)(checkTree(forloop.body))
      } yield ()
      case wile: While                      => for {
        _       <- checkTree(wile.cond)
        _       <- localRW[List[Tree], Unit](l => wile::l)(checkTree(wile.body))
      } yield ()
      case ifelse: If                       => for {
        _       <- checkTree(ifelse.cond)
        _       <- checkTree(ifelse.thenp)
        _       <- checkTree(ifelse.elsep)
      } yield ()
      case assign: Assign                   => for {
        _       <- checkTree(assign.lhs)
        _       <- checkTree(assign.rhs)
      } yield ()
      // calcj
      case lit: Lit                         => pointRW(())
      case bin: Binary                      => for {
        _       <- checkTree(bin.lhs)
        _       <- checkTree(bin.rhs)
      } yield ()
      case unary: Unary                     => for {
        _       <- checkTree(unary.expr)
      } yield ()
      case cast: Cast                       => for {
        _       <- checkTree(cast.expr)
      } yield ()
      // tiny
      case empty: Empty                     => pointRW(())
      case tuse: TypeUse                    => pointRW(())
      case id: Ident                        => pointRW(())
      case BadTree                          => pointRW(())
      // tiny
      case switch: Switch                   => for {
        _       <- checkTree(switch.expr)
        _       <- switch.cases.map {
          case x =>
            localRW[List[Tree], Unit](l => switch::l)(checkTree(x))
        }.sequenceU
      } yield ()
      case cse: Case                        => for {
        _       <- cse.guards.map(checkTree(_)).sequenceU
        _       <- checkTree(cse.body)
      } yield ()
      case lbl: Label                       => checkTree(lbl)
      case break: Break                     => checkBreak(break)
      case continue: Continue               => checkContinue(continue)
    }

    def checkContinue(cont: Continue): JumpChecker = for {
      encls   <- askRW
      _       <- if(encls.filter(isContinuable(_)) != Nil)
                   // TODO: Fix this
                   toJumpChecker(error(BAD_STATEMENT,
                     cont.toString, cont.toString, cont.pos, cont))
                 else
                   // TODO: Can you get rid of this?
                   pointRW[List[Tree], Unit](())
      res     <- pointRW(())
    } yield res



    def checkBreak(break: Break): JumpChecker = for {
      encls   <- askRW
      _       <- break.label match {
        case None if encls.filter(isBreakable(_)) != Nil  =>
          // TODO: Fix this
          toJumpChecker(error(BAD_STATEMENT,
            break.toString, break.toString, break.pos, break))
        case _                                            =>
          // TODO: Can you get rid of this?
          pointRW[List[Tree], Unit](())
      }
      res     <- pointRW(())
    } yield res





  }
}
