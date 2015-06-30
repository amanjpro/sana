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
  self: Namers =>
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

  def namer: Namer

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

    // override def typeIdent(id: Ident): TypeChecker[UseTree] = for {
    //   env   <- get
    //   lvars <- ask
    //   name  <- point(id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME))
    //   res   <- {
    //              val enclosingMethod = env.enclosingMethod(id.owner)
    //              val variable        = env.lookup(name,
    //                  alreadyDefinedVariablePredicate(_, lvars),
    //                  id.owner)
    //              // Is there any local variables with the same name?
    //              if(variable != NoId) { 
    //                point(Ident(variable, id.owner, id.pos)) 
    //              } else {
    //                // OK, this seems to be ugly, but we don't want to
    //                // re-compute operations
    //                val tuse = env.lookup(name, _.isInstanceOf[TypeKind],
    //                  id.owner)
    //                // compilation unit defines this name? bind it to that
    //                // The way Context works, makes sure that it first searches
    //                // for this compilation unit, then to this package and then
    //                // falls back to other options.
    //                // another compilation unit with the same package name as 
    //                // this compilation unit defines this name? bind it to that
    //                // it is type name
    //                if(tuse != NoId) {
    //                  point(TypeUse(tuse, id.owner, id.pos)) 
    //                } else {
    //                  // When we introduce import statements, we need to 
    //                  // implement the following resolution steps
    //                  // - an exactly one import statement, imports this name?
    //                  //   bind it to that, it is a type name
    //                  // - A wild-card import and a one-type import import this?
    //                  //   resolve to the one-type, and report a warning
    //                  //   and it is a type name
    //                  // - more than two import statements of the same kind import 
    //                  //   this name? report an error
    //
    //
    //                  // The fall back is: resolve it to a package name!
    //                  // XXX: For now I just add NoId to it
    //                  // FIXME: Don't we need to report an error now?
    //                  point(Ident(NoId, id.owner, id.pos)) 
    //                }
    //              }
    //            }
    // } yield res
    //
    //
    //
    //

    // INFO
    // The following methods (typeTypeUse, typeIdent, typeSelect, typeUseTree),
    // Seem almost like a copy of (nameTypeUses, nameIdents, nameSelects, 
    // nameUseTrees), but they are not.
    //
    // These methods are aware of types, and they help us in method overloading,
    // Another difference is that, these work on the inner trees (types and
    // terms), not only the types of the global definitions.

    override def typeTypeUse(tuse: TypeUse): TypeChecker[TypeUse] = for {
      // Typing Type use, is simply naming the tree
      ctx       <- get
      tuse_ctx2 = {
        val (_, r, ctx2) = namer.nameTypeUses(tuse).run(Set(), ctx)
        (r, ctx2)
      }
      r         =  tuse_ctx2._1
      ctx2      =  tuse_ctx2._2
      _         <- put(ctx2)
    } yield r

    // Typing an Ident is a bit more involving than typing a type use,
    // we need to decide weather we resolve it to a local variable,
    // field, type name or a package name,
    //
    // This method is only called if the ident is not part of a select tree
    override def typeIdent(id: Ident): TypeChecker[UseTree] = for {
      env   <- get
      lvars <- ask
      owner     =  id.owner
      name      =  id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME)
      res   =  {
                 val enclosingMethod = env.enclosingMethod(id.owner)
                 val variable        = env.lookup(name,
                     alreadyDefinedVariablePredicate(_, lvars),
                     id.owner)
                 // Is there any local variables with the same name?
                 if(variable != NoId) { 
                   (Ident(variable, id.owner, id.pos), env)
                 } else {
                   // OK, this seems to be ugly, but we don't want to
                   // re-compute operations
                   val tuse = env.lookup(name, _.kind.isInstanceOf[TypeKind],
                     id.owner)
                   // compilation unit defines this name? Bind it to that
                   // The way Context works, makes sure that it first searches
                   // for this compilation unit, then to this package and then
                   // falls back to other options.
                   // another compilation unit with the same package name as 
                   // this compilation unit defines this name? Bind it to that
                   // it is type name
                   if(tuse != NoId) {
                     (TypeUse(tuse, id.owner, id.pos), env)
                   } else {
                     val pkg = env.lookup(name, _.kind == PackageKind,
                       id.owner)
                     if(pkg != NoId) {
                       (Ident(pkg, id.owner, id.pos), env) 
                     } else {
                       // Does the classpath defines this name
                       // in a package hierarchy similar to this
                       // compilation unit?
                       val pkgs = env.enclosingPackageNames(id.owner)
                       val fullName = pkgs.mkString(".") + "." + name
                       if(catalog.defines(fullName, true)) {
                         val (_, loadedClass, ctx2) = 
                           namer.loadFromClassPath(fullName, 
                             owner).run(Set(), env)
                         loadedClass match {
                           case cd: ClassDef =>
                             (TypeUse(cd.id, id.nameAtParser, 
                                 owner, id.pos), ctx2)
                           case _            =>
                             // This case should never happen
                             (Ident(NoId, id.nameAtParser, owner, id.pos), 
                                 ctx2)
                         }
                       } else if(self.catalog.defines(fullName, false)) { 
                         val info = newPackageDefInfo(name)
                         val (i, ctx2) = env.extend(owner, packageContext(info))
                           (Ident(i, id.nameAtParser, owner, id.pos), ctx2)
                       } else {
                          (Ident(NoId, id.owner, id.pos), env)
                       }
                     }
                     // When we introduce import statements, we need to 
                     // implement the following resolution steps
                     // - an exactly one import statement, imports this name?
                     //   bind it to that, it is a type name
                     // - A wild-card import and a one-type import import this?
                     //   resolve to the one-type, and report a warning
                     //   and it is a type name
                     // - more than two import statements of the same kind import 
                     //   this name? Report an error
                   }
                 }
               }
        tid  =  res._1
        env2 =  res._2
        _    <- tid.uses match {
          case NoId             =>
            toTypeChecker(error(NAME_NOT_FOUND,
              id.toString, "a name", id.pos, id))
          case _                 =>
            point(())
        }
        _    <- put(env2)
      } yield tid


    
    // This method is only called if the ident is not part of a select tree
    def typeQualifiedIdent(id: Ident): TypeChecker[SimpleUseTree] = for {
      env   <- get
      lvars <- ask
      owner     =  id.owner
      name      =  id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME)
      res   =  {
                  val qkind = env.getTree(owner).map(_.kind)
                  // At this point, we don't have 
                  if(qkind == Some(PackageKind)) {
                    val tuse = env.lookup(name, _.kind.isInstanceOf[TypeKind],
                              id.owner)
                    if(tuse != NoId) {
                      (TypeUse(tuse, id.owner, id.pos), env)
                    } else {
                      val tuse = env.lookup(name, 
                        _.kind == PackageKind, id.owner)
                      (Ident(tuse, id.owner, id.pos), env)
                    }
                  } else if(qkind != None){
                    val tuse = env.lookup(name, 
                      _.kind == VariableKind, id.owner)
                    (Ident(tuse, id.owner, id.pos), env)
                  } else {
                    (Ident(NoId, id.owner, id.pos), env)
                  }
                }
        tid  =  res._1
        env2 =  res._2
        _    <- tid.uses match {
          case NoId             =>
            toTypeChecker(error(NAME_NOT_FOUND,
              id.toString, "a name", id.pos, id))
          case _                 =>
            point(())
        }
        _    <- put(env2)
      } yield tid


    
    def typeSelect(select: Select): TypeChecker[UseTree] = for {
      qual    <- typeTree(select.qual)
      ctx     <- get
      // The following snippet only happens when we introduce inner classes
      // qtpe    <- toTypeChecker(qual.tpe)
      // id      =  qtpe match {
      //   // This should not happen yet, until we introduce inner classes
      //   case ct: ClassType => ct.id
      //   // INFO: What about arrays? They do not have anything to do with
      //   // type uses
      //   case _             => NoId
      // }
      qid     =  qual match {
        case qual: UseTree       => qual.uses
        case _                   => NoId
      }
      tree    <- select.tree match {
        case id: Ident      => 
          typeQualifiedIdent(Ident(id.uses, id.nameAtParser, qid, id.pos))
        case tuse: TypeUse  => 
          typeTypeUse(TypeUse(tuse.uses, tuse.nameAtParser, qid, tuse.pos))
      }
    } yield Select(qual, tree, select.pos, select.owner)

    override def typeUseTree(use: UseTree): TypeChecker[UseTree] = use match {
      case tuse: TypeUse                             => for {
        r       <- typeTypeUse(tuse)
      } yield r
      case id: Ident                                 => for {
        // Ident here? Then it should be a package name (at least
        // in OOJ!!)
        r       <- typeIdent(id)
      } yield r
      case select: Select                            => for {
        r  <- typeSelect(select)
      } yield r
    }
    
    // We override it, to support Method Overloading, this addresses Issue #1
    // override def nameMethodTreeUses(fun: UseTree): 
    //   TypeChecker[UseTree] = fun match {
    //   case id: Ident                                => for {
    //     env    <- get
    //     name   <- point(id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME))
    //     tid    <- point(env.lookup(name, 
    //       _.kind == MethodKind, id.owner))
    //     // _      <- tid match {
    //     //           case NoId    =>
    //     //             toTypeChecker(error(NAME_NOT_FOUND,
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


