package ch.usi.inf.l3.sana.ooj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.brokenj
import sana.ooj
import sana.primj
import sana.calcj
import tiny.contexts._
import tiny.names.Name
import primj.report._
import calcj.ast.JavaOps._

import ooj.Global
import ooj.names.Namers

import scalaz.{Name => _, Failure => _, _}
import scalaz.Scalaz._

trait Typers extends brokenj.typechecker.Typers {
  // INFO:
  // To support method overloading, typer should take care of nameing
  // inner definitions (local definitions). 
  //
  // Take this an example:
  // class A {
  //   int k() { ... }
  // }
  // class B {
  //   String s() { ... }
  // }
  //
  // class Test {
  //   A m(int i) { ... }
  //   B m(String s) { ... }
  //  
  //   void test() {
  //     m("1").s();   // If we rely on Namer only, we wrongly resolve s to 
  //                   // the a non-existing method name, but typer knows 
  //                   // that this method application is actually correct,
  //                   // because it can always pick the correct method.
  //   }
  // }

  type G <: Global
  import global._

  import rwst.{local => _, _}

  trait Typer extends super.Typer {
    override def binaryTyper(ltpe: Type, 
      rtpe: Type, bin: Binary): TypeChecker[Type] = bin.op match {
      case Add                                    =>
        (ltpe.name, rtpe.name) match {
          case (`STRING_TYPE_NAME`, _)            =>
            point(BinaryType(ltpe, ltpe, ltpe))
          case (_, `STRING_TYPE_NAME`)            =>
            point(BinaryType(rtpe, rtpe, rtpe))
          case _                                  =>
            super.binaryTyper(ltpe, rtpe, bin)
        }
      case Eq | Neq                               => 
        (ltpe, rtpe) match {
          case (_: RefType, _: RefType)           => 
            point(BinaryType(ltpe, rtpe, BooleanType))
          case _                                  =>
            super.binaryTyper(ltpe, rtpe, bin)
        }
      case _                                      =>
        super.binaryTyper(ltpe, rtpe, bin)
    }


    override def typeApply(apply: Apply): TypeChecker[Apply] = for {
      args      <- apply.args.map(typeExpr(_)).sequenceU
    } yield null
  }
}


