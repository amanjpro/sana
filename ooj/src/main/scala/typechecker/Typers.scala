package ch.usi.inf.l3.sana.ooj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.brokenj
import sana.ooj
import sana.primj
import sana.calcj
import primj.report._
import tiny.contexts._
import calcj.ast.JavaOps._
import ooj.Global

import scalaz.{Name => _, Failure => _, _}
import scalaz.Scalaz._

// From Java Specification 1.0 - Sect: 5.2 - p61
// 1- Assignment Conversion
// 2- Method Conversion   Sect: 5.3 - p66
// 3- String Conversion   Sect: 5.4 - p67

trait Typers extends brokenj.typechecker.Typers {
  type G <: Global
  import global._


  trait Typer extends super.Typer {
    override def binaryTyper(ltpe: Type, 
      rtpe: Type, bin: Binary): TypeChecker[Type] = bin.op match {
      case Add                                    =>
        (ltpe.name, rtpe.name) match {
          case (`STRING_TYPE_NAME`, _)            =>
            pointSW(BinaryType(ltpe, ltpe, ltpe))
          case (_, `STRING_TYPE_NAME`)            =>
            pointSW(BinaryType(rtpe, rtpe, rtpe))
          case _                                  =>
            super.binaryTyper(ltpe, rtpe, bin)
        }
      case Eq | Neq                               => 
        (ltpe, rtpe) match {
          case (_: RefType, _: RefType)           => 
            pointSW(BinaryType(ltpe, rtpe, BooleanType))
          case _                                  =>
            super.binaryTyper(ltpe, rtpe, bin)
        }
      case _                                      =>
        super.binaryTyper(ltpe, rtpe, bin)
    }
  }
}


