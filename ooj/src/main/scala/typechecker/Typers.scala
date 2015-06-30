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

    override def typeIdent(id: Ident): TypeChecker[UseTree] = for {
      env   <- get
      lvars <- ask
      name  <- point(id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME))
      res   <- {
                 val enclosingMethod = env.enclosingMethod(id.owner)
                 val variable        = env.lookup(name,
                     alreadyDefinedVariablePredicate(_, lvars),
                     id.owner)
                 // Is there any local variables with the same name?
                 if(variable != NoId) { 
                   point(Ident(variable, id.owner, id.pos)) 
                 } else {
                   // OK, this seems to be ugly, but we don't want to
                   // re-compute operations
                   val tuse = env.lookup(name, _.isInstanceOf[TypeKind],
                     id.owner)
                   // compilation unit defines this name? bind it to that
                   // The way Context works, makes sure that it first searches
                   // for this compilation unit, then to this package and then
                   // falls back to other options.
                   // another compilation unit with the same package name as 
                   // this compilation unit defines this name? bind it to that
                   // it is type name
                   if(tuse != NoId) {
                     point(TypeUse(tuse, id.owner, id.pos)) 
                   } else {
                     // When we introduce import statements, we need to 
                     // implement the following resolution steps
                     // - an exactly one import statement, imports this name?
                     //   bind it to that, it is a type name
                     // - A wild-card import and a one-type import import this?
                     //   resolve to the one-type, and report a warning
                     //   and it is a type name
                     // - more than two import statements of the same kind import 
                     //   this name? report an error


                     // The fall back is: resolve it to a package name!
                     // XXX: For now I just add NoId to it
                     // FIXME: Don't we need to report an error now?
                     point(Ident(NoId, id.owner, id.pos)) 
                   }
                 }
               }
    } yield res

    // We override it, to support Method Overloading, this addresses Issue #1
    // override def nameMethodTreeUses(fun: UseTree): 
    //   NamerMonad[UseTree] = fun match {
    //   case id: Ident                                => for {
    //     env    <- get
    //     name   <- point(id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME))
    //     tid    <- point(env.lookup(name, 
    //       _.kind == MethodKind, id.owner))
    //     // _      <- tid match {
    //     //           case NoId    =>
    //     //             toNamerMonad(error(NAME_NOT_FOUND,
    //     //               id.toString, "a method name", id.pos, id))
    //     //           case _     =>
    //     //             point(())
    //     //           }
    //    } yield Ident(tid, id.owner, id.pos)
    //   case slct: Select                             =>
    //     // - if qual part points to Class, Then OK
    //     // - if it points to Interface, then report an error
    //     //   Java 1.0 doesn't have static methods in interfaces
    //     // - if qual is `super`, then search in the super class
    //     //   (Not interface, because interface cannot be accessed
    //     //   using super keyword). If `this` is interface, or
    //     //   Object class, then report an error, or if the context is
    //     //   a static context.
    //     // - Otherwise, qual should be an expression, and
    //     //   search in the context of the qual's uses
    //     ???
    // }


    override def typeApply(apply: Apply): TypeChecker[Apply] = for {
      args      <- apply.args.map(typeExpr(_)).sequenceU
    } yield null
  }
}


