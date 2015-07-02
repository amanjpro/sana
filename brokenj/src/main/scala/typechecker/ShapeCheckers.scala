package ch.usi.inf.l3.sana.brokenj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import tiny.util.{CompilationUnits, MonadUtils}
import tiny.passes
import tiny.report._
import tiny.contexts.TreeContexts
import calcj.ast.JavaOps._
import primj.report._
import primj.modifiers._
import brokenj.Global

import scalaz.Scalaz._
import scalaz._

// TODO: How long should we keep def information in our database?

// From Java Specification 1.0 - Sect: 5.2 - p61
// 1- Assignment Conversion
// 2- Method Conversion   Sect: 5.3 - p66
// 3- String Conversion   Sect: 5.4 - p67

trait ShapeCheckers extends primj.typechecker.ShapeCheckers {

  override type G <: Global
  import global._

  trait Checker extends super.Checker {


    def checkLabel(lbl: Label): ShapeChecker =
      if(canHaveLabel(lbl.stmt))
        checkTree(lbl.stmt)
      else
        toShapeChecker(error(UNEXPETED_TREE,
          lbl.stmt.toString, "an expression", lbl.stmt.pos, lbl.stmt))
  }
}
