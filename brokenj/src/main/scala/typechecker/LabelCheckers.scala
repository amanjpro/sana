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
import scala.language.implicitConversions

import scalaz.Scalaz._
import scalaz._

trait LabelCheckers extends passes.Phases {
  type G <: Global
  import global._

  type LabelChecker = ReaderWriter[List[Label], Unit]

  def toLabelChecker(x: Reader[List[Label], Unit]): 
    ReaderWriter[List[Label], Unit] = toReaderWriter(x)

  def toLabelChecker(x: ErrorReportingMonad[Unit]): 
    ReaderWriter[List[Label], Unit] = toReaderWriter(x)


  trait Checker extends CheckerPhase {
    
    val name: String = "label-checkers"
    override val description: Option[String] = 
      Some("Check validity of lable statements.")
    override def runRightAfter: Option[String] = Some("shape-checkers")

    def startPhase(state: Context, unit: CompilationUnit): 
         Vector[Report] = {
      val tree  = unit.tree
      val (w, _) = checkTree(tree).run(Nil).run
      w
    }


    def checkTree(tree: Tree): LabelChecker = tree match {
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
        _       <- checkTree(forloop.body)
      } yield ()
      case wile: While                      => for {
        _       <- checkTree(wile.cond)
        _       <- checkTree(wile.body)
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
        _       <- switch.cases.map(checkTree(_)).sequenceU
        _       <- checkTree(switch.default)
      } yield ()
      case cse: Case                        => for {
        _       <- cse.guards.map(checkTree(_)).sequenceU
        _       <- checkTree(cse.body)
      } yield ()
      case lbl: Label                       => checkLabel(lbl)
      case break: Break                     => checkBreak(break)
      case continue: Continue               => checkContinue(continue)
    }

    def checkLabel(lbl: Label): LabelChecker = for {
      labels    <- askRW
      _         <- labels.filter(_.name == lbl.name) match {
        case x::xs                                 =>
          // TODO: Fix this
          toLabelChecker(error(BAD_STATEMENT,
            lbl.toString, lbl.toString, lbl.pos, lbl))
        case Nil                                   =>
          localRW((lbls: List[Label]) => lbl::lbls)(checkTree(lbl.stmt))
      }
    } yield ()

    def checkContinue(cont: Continue): LabelChecker = for {
      labels   <- askRW
      _        <- cont.label match {
        case None                                   =>
          pointRW[List[Label], Unit](())
        case Some(name)                             =>
          labels.filter(_.name == name) match {
            case Nil             =>
              // TODO: Fix this
              toLabelChecker(error(BAD_STATEMENT,
                cont.toString, cont.toString, cont.pos, cont))
            case x::xs                              =>
              if(isIterTree(x.stmt))
                pointRW[List[Label], Unit](())
              else
                // TODO: Fix this
                toLabelChecker(error(BAD_STATEMENT,
                  cont.toString, cont.toString, cont.pos, cont))
          }
      }
    } yield ()

    def checkBreak(break: Break): LabelChecker = for {
      labels   <- askRW
      _        <- break.label match {
        case None                                   =>
          pointRW[List[Label], Unit](())
        case Some(name)                             =>
          labels.filter(_.name == name) match {
            case Nil             =>
              // TODO: Fix this
              toLabelChecker(error(BAD_STATEMENT,
                break.toString, break.toString, break.pos, break))
            case x::xs                              =>
              pointRW[List[Label], Unit](())
          }
      }
    } yield ()


    
  }
}
