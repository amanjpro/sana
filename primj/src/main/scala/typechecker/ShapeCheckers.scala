package ch.usi.inf.l3.sana.primj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import tiny.util.{CompilationUnits, MonadUtils}
import tiny.passes
import tiny.report._
// import tiny.contexts.TreeContexts
import calcj.typechecker
import calcj.ast.JavaOps._
import primj.report._
import primj.Global
import primj.modifiers.Ops._

import scalaz.Scalaz._
import scalaz._

// TODO: How long should we keep def information in our database?

// From Java Specification 1.0 - Sect: 5.2 - p61
// 1- Assignment Conversion
// 2- Method Conversion   Sect: 5.3 - p66
// 3- String Conversion   Sect: 5.4 - p67

trait ShapeCheckers extends passes.Phases {
  type G <: Global
  import global._

  type ShapeChecker = StateWriter[Unit]

  def toShapeChecker(x: ContextState[Unit]): StateWriter[Unit] =
    toStateWriter(x)
  def toShapeChecker(x: ErrorReportingMonad[Unit]): StateWriter[Unit] =
    toStateWriter(x)

  trait Checker extends CheckerPhase {
    
    val name: String = "shape-checkers"
    override val description: Option[String] = 
      Some("Check weather an AST tree is welformed.")
    override def runRightAfter: Option[String] = Some("typer")
    // def startPhase(state: Context, unit: CompilationUnit): 
    //      (Vector[Report], CompilationUnit, Context) = {
    //   val tree  = unit.tree
    //   val (w, (s, typedTree)) = typeTree(tree).run(state).run
    //   (w, CompilationUnit(unit.id, typedTree, unit.fileName), s)
    // }

    def checkTree(tree: Tree): ShapeChecker
    def checkBlock(block: Block): ShapeChecker = for {
      _ <- block.stmts.map(checkTree(_)).sequenceU
      _ <- block.stmts.map(checkStatement(_)).sequenceU
    } yield ()

    def checkIf(ifelse: If): ShapeChecker = for {
      _ <- checkTree(ifelse.cond)
      _ <- checkTree(ifelse.thenp)
      _ <- checkTree(ifelse.elsep)
      _ <- checkExpression(ifelse.cond)
      _ <- checkStatement(ifelse.thenp)
      _ <- checkStatement(ifelse.elsep)
    } yield ()

    def checkWhile(wile: While): ShapeChecker = for {
      _ <- checkTree(wile.cond)
      _ <- checkTree(wile.body)
      _ <- checkExpression(wile.cond)
      _ <- checkStatement(wile.body)
    } yield ()

    def checkFor(forloop: For): ShapeChecker = for {
      _ <- forloop.inits.map(checkTree(_)).sequenceU
      _ <- checkTree(forloop.cond)
      _ <- forloop.steps.map(checkTree(_)).sequenceU
      _ <- checkTree(forloop.body)
      _ <- isValidInitStatements(forloop.inits, forloop)
      _ <- forloop.cond match {
        case Empty => pointSW(())
        case cond  => checkExpression(cond)
      }
      _ <- forloop.steps.map(checkStatement(_)).sequenceU
      _ <- checkStatement(forloop.body)
    } yield ()

    def checkCast(cast: Cast): ShapeChecker = for {
      _    <- if(pointsToUse(cast.tpt, _.isInstanceOf[TypeUse])) {
                // TODO: Better error message
                toShapeChecker(error(TYPE_NAME_EXPECTED,
                  cast.tpt.toString, "a type", cast.tpt.pos, cast.tpt))
              } else pointSW(())
      _    <- checkTree(cast.expr)
    } yield ()

    def checkMethodDef(meth: MethodDef): ShapeChecker = for {
      _    <- if(pointsToUse(meth.ret, _.isInstanceOf[TypeUse])) {
              // TODO: Better error message
              toShapeChecker(error(TYPE_NAME_EXPECTED,
                meth.ret.toString, "a type", meth.ret.pos, meth.ret))
            } else pointSW(())
      _    <- meth.params.map(checkValDef(_)).sequenceU
      _    <- checkTree(meth.body)
    } yield ()

    // Return with expr can only appear in non-void methods
    // Return w/o expr can only appear in void methods or constructors
    def checkReturn(ret: Return): ShapeChecker

    def checkValDef(valdef: ValDef): ShapeChecker = for {
      ctx  <- getSW
      _    <- if(pointsToUse(valdef.tpt, _.isInstanceOf[TypeUse])) {
                // TODO: Better error message
                toShapeChecker(error(TYPE_NAME_EXPECTED,
                  valdef.tpt.toString, "a type", valdef.tpt.pos, valdef.tpt))
              } else pointSW(())
      _    <- if((ctx.isBlock(valdef.owner) || ctx.isFor(valdef.owner)) &&
                  !(valdef.mods.isLocalVariable)) {
                // TODO: Better error message
                toShapeChecker(error(UNEXPETED_TREE,
                  valdef.toString, "an expression", valdef.pos, valdef))
              } else if(ctx.isMethodDef(valdef.owner) && 
                    !(valdef.mods.isParam)) {
                // TODO: Better error message
                toShapeChecker(error(UNEXPETED_TREE,
                  valdef.toString, "an expression", valdef.pos, valdef))
              } else if(!valdef.mods.isField) {
                // TODO: Better error message
                toShapeChecker(error(UNEXPETED_TREE,
                  valdef.toString, "an expression", valdef.pos, valdef))
              } else pointSW(())
      _     <- if(isSimpleExpression(valdef.rhs))
                 pointSW(())
               else
                 // TODO: Better error message
                 toShapeChecker(error(UNEXPETED_TREE,
                   valdef.toString, "an expression", valdef.pos, valdef))
    } yield ()

    
    // postfix flag can only be set if the operator is postfix
    def checkUnary(e: Unary): ShapeChecker = {
      if(e.mods.isPostfix && (e.op != Inc || e.op != Dec))
        toShapeChecker(error(BAD_STATEMENT,
          e.toString, "a postfix operation", e.pos, e))
      else pointSW(())
    }

    def checkExpression(e: Tree): ShapeChecker

    def checkStatement(e: Tree): ShapeChecker = {
      if(isValidStatementExpression(e) || isValidStatement(e))
        pointSW(())
      else
        toShapeChecker(error(BAD_STATEMENT,
          e.toString, "a statement", e.pos, e))
    }


    def isValidInitStatements(inits: List[Tree], 
          forloop: For): ShapeChecker = {
      if(!allValDefsOrNone(inits))
        // TODO: Better error message
        toShapeChecker(error(UNEXPETED_TREE,
          forloop.toString, "an expression", forloop.pos, forloop))
      else {
        val initsRes = inits.map(isValDefOrStatementExpression(_))
        val empty: Vector[Report] = Vector.empty
        val errors = inits.zip(initsRes).foldLeft(empty)((z, y) => {
          y match {
            case (init, false) =>
              z ++ Vector(genError(UNEXPETED_TREE, init.toString, 
                          "", init.pos, forloop))
            case (_, true)     =>
              z
          }
        })
        if(errors.size == 0) pointSW(())
        else toShapeChecker(errors.tell)
      }
    }

    def allValDefsOrNone(trees: List[Tree]): Boolean = {
      val valdefs = trees.filter(_.isInstanceOf[ValDef])
      valdefs.size == trees.size || valdefs.size == 0
    }

    
    
  }
}
