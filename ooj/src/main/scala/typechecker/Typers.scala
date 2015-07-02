package ch.usi.inf.l3.sana.ooj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.brokenj
import sana.ooj
import sana.primj
import sana.calcj
import tiny.contexts._
import tiny.modifiers.Flags
import tiny.names.Name
import primj.report._
import calcj.ast.JavaOps._

import ooj.Global
import ooj.names.Namers
import ooj.modifiers.Ops._
import ooj.report._

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

    def typeSuper(spr: Super): TypeChecker[Super] = for {
      ctx        <- get
      enclClass  =  ctx.enclosingClass(spr.enclosingId)
      encl       =  ctx.enclosingNonLocal(spr.enclosingId)
      _          <- ctx.getTpe(enclClass) match {
        case ot: ObjectType             =>
          toTypeChecker(error(ACCESSING_SUPER_IN_OBJECT,
              spr.toString, "", spr.pos, spr))
        case _                          =>
          point(())
      }
      _          <- ctx.isStatic(encl) match {
        case true                   =>
          toTypeChecker(error(ACCESSING_SUPER_IN_STATIC,
              spr.toString, "", spr.pos, spr))
        case _                      => point(())
      }
    } yield Super(spr.enclosingId, spr.pos, enclClass)

    def typeThis(ths: This): TypeChecker[This] = for {
      ctx        <- get
      enclClass  =  ctx.enclosingClass(ths.enclosingId)
      encl       =  ctx.enclosingNonLocal(ths.enclosingId)
      _          <- ctx.isStatic(encl) match {
        case true                   =>
          toTypeChecker(error(ACCESSING_THIS_IN_STATIC,
              ths.toString, "", ths.pos, ths))
        case _                      => point(())
      }
    } yield This(ths.enclosingId, ths.pos, enclClass)

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


    def termIsVisible(id: TreeId, mods: Flags, from: TreeId,
      ctx: Context): Boolean = {

      def privateCheck(id: TreeId, from: TreeId): Boolean = {
        // Get the enclosing classes of this id
        val enclosingClasses   = ctx.enclosingClasses(from)
        val enclosingClassId   = ctx.enclosingClass(id)
        enclosingClasses.contains(enclosingClassId)
      }

      def packageCheck(id: TreeId, from: TreeId): Boolean = {
        val pkgFrom          = ctx.enclosingPackage(from)
        val pkgId            = ctx.enclosingPackage(id)
        pkgId == pkgFrom
      }
      if(mods.isPrivateAcc) {
        privateCheck(id, from)
      } else if(mods.isPackageAcc) {
        packageCheck(id, from)
      } else if(mods.isProtectedAcc) {
        val lowerCheck = packageCheck(id, from) || privateCheck(id, from)
        if(lowerCheck) true
        else {
          val enclosingClassFrom = ctx.enclosingClass(from)
          val enclosingClassId   = ctx.enclosingClass(id)
          val treeFrom           = ctx.getTree(enclosingClassFrom)
          treeFrom match {
            case Some(cf: ClassInfo)        =>
              cf.parents.contains(enclosingClassId)
            case _                          => false
          }
        }
      } else true
    }

    // Typing an Ident is a bit more involving than typing a type use,
    // we need to decide weather we resolve it to a local variable,
    // field, type name or a package name,
    //
    // This method is only called if the ident is not part of a select tree
    override def typeIdent(id: Ident): TypeChecker[UseTree] = for {
      env   <- get
      lvars <- ask
      owner =  id.owner
      encl  = id.enclosingId
      name  =  id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME)
      res   =  {
                 val enclosingMethod = env.enclosingMethod(id.owner)
                 val variable        = env.lookup(name,
                     alreadyDefinedVariablePredicate(_, lvars),
                     id.owner)
                 val visible         = env.getTree(variable) match {
                   case Some(t)          =>
                     !t.mods.isField ||
                       termIsVisible(variable, t.mods, encl, env)
                   case _                => false
                 }
                 // Is there any local variables with the same name?
                 if(variable != NoId && visible) {
                   (Ident(variable, id.pos, id.owner, encl), env)
                 } else {
                   // OK, this seems to be ugly, but we don't want to
                   // re-compute operations
                   val tuse = env.lookup(name, _.kind.isInstanceOf[TypeKind],
                     id.owner)

                   val visible         = env.getTree(tuse) match {
                     case Some(t)          =>
                       !t.mods.isField ||
                         namer.typeIsVisible(tuse, t.mods, encl, env)
                     case _                => false
                   }

                   // compilation unit defines this name? Bind it to that
                   // The way Context works, makes sure that it first searches
                   // for this compilation unit, then to this package and then
                   // falls back to other options.
                   // another compilation unit with the same package name as
                   // this compilation unit defines this name? Bind it to that
                   // it is type name
                   if(tuse != NoId && visible) {
                     (TypeUse(tuse, id.pos, id.owner, encl), env)
                   } else {
                     val pkg = env.lookup(name, _.kind == PackageKind,
                       id.owner)
                     if(pkg != NoId) {
                       (Ident(pkg, id.pos, id.owner, encl), env)
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
                           case cd: ClassDef if namer.typeIsVisible(cd.id,
                                       cd.mods, encl, ctx2)      =>
                             (TypeUse(cd.id, id.nameAtParser,
                                 id.pos, owner, encl), ctx2)
                           case _                                =>
                             // This case should never happen
                             (Ident(NoId, id.nameAtParser, id.pos, owner,
                               encl), ctx2)
                         }
                       } else if(self.catalog.defines(fullName, false)) {
                         val info = newPackageDefInfo(name)
                         val (i, ctx2) = env.extend(owner, packageContext(info))
                           (Ident(i, id.nameAtParser, id.pos,
                             owner, encl), ctx2)
                       } else {
                          (Ident(NoId, id.pos, id.owner, encl), env)
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
      owner =  id.owner
      encl  =  id.enclosingId
      name  =  id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME)
      res   =  {
                  val qkind = env.getTree(owner).map(_.kind)
                  // At this point, we don't have
                  if(qkind == Some(PackageKind)) {
                    val tuse = env.lookup(name, _.kind.isInstanceOf[TypeKind],
                              id.owner)
                    val visible         = env.getTree(tuse) match {
                      case Some(t)          =>
                        !t.mods.isField ||
                          namer.typeIsVisible(tuse, t.mods, encl, env)
                      case _                => false
                    }
                    if(tuse != NoId && visible) {
                      (TypeUse(tuse, id.pos, id.owner, encl), env)
                    } else {
                      val tuse = env.lookup(name,
                        _.kind == PackageKind, id.owner)
                      (Ident(tuse, id.pos, id.owner, encl), env)
                    }
                  } else if(qkind != None){
                    val variable = env.lookup(name,
                      _.kind == VariableKind, id.owner)
                    val visible         = env.getTree(variable) match {
                      case Some(t)          =>
                        !t.mods.isField ||
                          termIsVisible(variable, t.mods, encl, env)
                      case _                => false
                    }
                    if(visible)
                      (Ident(variable, id.pos, id.owner, encl), env)
                    else
                      (Ident(NoId, id.pos, id.owner, encl), env)
                  } else {
                    (Ident(NoId, id.pos, id.owner, encl), env)
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
      qtpe    <- toTypeChecker(qual.tpe)
      qid     =  qtpe match {
        // TODO: Array type????
        case c: ClassType           => c.id
        case _                      => NoId
      }
      tree    <- select.tree match {
        case id: Ident      =>
          typeQualifiedIdent(
            Ident(id.uses, id.nameAtParser, id.pos, qid, id.enclosingId))
        case tuse: TypeUse  =>
          typeTypeUse(TypeUse(tuse.uses, tuse.nameAtParser, tuse.pos,
            qid, tuse.enclosingId))
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


    def methodCanBeApplied(ptpes: List[Type],
                           atpes: List[Type]): Boolean = {
      if(ptpes.size != atpes.size) false
      else
        atpes.zip(ptpes).foldLeft(true)((z, y) => {
          z && (y._1 <:< y._2)
        })
    }


    /**
     * A method to filter out methods that cannot be applied within a scope
     * with a list of parameters.
     *
     * @param ids list of method id's that needs to be filtered out
     * @param enclClass the enclosing class of this method application
     * @param ctx the compilation context (the state of the whole program)
     * @param tpes the list of applied arguments
     */
    protected def qualifiedMethods(ids: List[TreeId],
      enclClass: TreeId, ctx: Context,
      tpes: List[Type]): List[(TreeId, MethodType)] = ids match {
      case Nil                                                           =>
        Nil
      case (id::rest)                                                    =>
        ctx.getTree(id) match {
          case Some(t)                if enclClass.contains(id)          =>
            t.tpe.eval(ctx) match {
              case mt: MethodType if methodCanBeApplied(mt.params, tpes) =>
                (id, mt)::qualifiedMethods(rest, enclClass, ctx, tpes)
              case _                                                     =>
                qualifiedMethods(rest, enclClass, ctx, tpes)
            }
          case Some(t)      if ! t.mods.isPrivateAcc                     =>
            t.tpe.eval(ctx) match {
              case mt: MethodType if methodCanBeApplied(mt.params, tpes) =>
                (id, mt)::qualifiedMethods(rest, enclClass, ctx, tpes)
              case _                                                     =>
                qualifiedMethods(rest, enclClass, ctx, tpes)
            }
          case _                                                         =>
            qualifiedMethods(rest, enclClass, ctx, tpes)
      }
    }


    /**
     * Taking a list of method id and method type pairs, finds the most
     * specific methods.
     *
     * According to Java's spec (1.0 ed) a method is more specific than the
     * other, if all its parameters can be passed to the latter.
     */
    def mostSpecificMethods(ids: List[(TreeId, MethodType)]):
        List[(TreeId, MethodType)] =
      ids.filter((x) => {
        ids.foldLeft(true)((z, y) =>
          if(x._1 == y._1) z
          else methodCanBeApplied(y._2.params, x._2.params)
        )
      })


    // This method addresses Issue #1
    def typeApplyFunIdent(fun: Ident,
      tpes: List[Type]): TypeChecker[Ident] = for {
      ctx        <- get
      owner      =  fun.owner           // Owner and enclId are the same
      enclId     =  fun.enclosingId     // if the call is not qualified
      mthdClass  =  ctx.enclosingClass(owner)
      enclClass  =  if(owner == enclId) mthdClass
                    else ctx.enclosingClass(enclId)
      name       =  fun.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME)
      methods    =  { // a list of method ids and their types
        ctx.getContext(mthdClass) match {
          case None                        => Nil
          case Some(mctx)                  =>
            val ids       =
              mctx.findAllInThisContextAndInherited(name,
                      _.kind == MethodKind)
            qualifiedMethods(ids, enclClass, ctx, tpes)
        }
      }
      specficis  = mostSpecificMethods(methods)
      _          <- specficis match {
        case List(id)                 => point(())
        case Nil                      =>
          toTypeChecker(error(NAME_NOT_FOUND,
              fun.toString, "a method name", fun.pos, fun))
      }
      _          <- specficis match {
        case List(id)                 => point(())
        case _                        =>
          toTypeChecker(error(AMBIGUOUS_METHOD_INVOCATION,
              fun.toString, "a method name", fun.pos, fun))

      }
      mid        =  methods.headOption.map(_._1).getOrElse(NoId)

      _          <- put(ctx)
    } yield Ident(mid, fun.pos, owner, enclClass)


    override def typeApply(apply: Apply): TypeChecker[Apply] = for {
      args      <- apply.args.map(typeExpr(_)).sequenceU
      ctx       <- get
      tpes      <- args.map(x => toTypeChecker(x.tpe)).sequenceU
      fun       <- apply.fun match {
        case id: Ident                    =>
          typeApplyFunIdent(id, tpes)
        case s@Select(qual, tree: Ident)  =>
          for {
            qual <- typeTree(qual)
            qtpe <- toTypeChecker(qual.tpe)
            qid  = qtpe match {
              // TODO: Arrays?
              case c: ClassType             => c.id
              case _                        => NoId
            }
            tree <- typeApplyFunIdent(Ident(tree.uses, qual.pos,
                                          qid, tree.enclosingId), tpes)
          } yield Select(qual, tree, s.pos, s.owner)
        case _                            =>
          typeExpr(apply.fun)
      }
      _           <- fun match {
        // A type is selected? then the call should be of a static method
        case s@Select(q, _) if pointsToUse(q,
            _.isInstanceOf[TypeUse]) && ! ctx.isStatic(s.uses)     =>

          toTypeChecker(error(INSTANCE_METHOD_IN_STATIC_CONTEXT_INVOK,
              fun.toString, "a method name", fun.pos, fun))
        case id: Ident                                             =>
          val enclId  = id.enclosingId
          val isStatic = ctx.getTree(ctx.enclosingNonLocal(enclId)) match {
            case Some(t)                     => t.mods.isStatic
            case _                           => false
          }
          val isMStatic = ctx.getTree(id.uses) match {
            case Some(t)                     => t.mods.isStatic
            case _                           => false
          }
          (isStatic, isMStatic) match {
            case (true, false)               =>
              toTypeChecker(error(INSTANCE_METHOD_IN_STATIC_CONTEXT_INVOK,
                fun.toString, "a method name", fun.pos, fun))
            case (_, _)                      => point(())
          }
          case _                                                    =>
        point(())
      }
    } yield Apply(fun, args, apply.pos, apply.owner)
  }
}


