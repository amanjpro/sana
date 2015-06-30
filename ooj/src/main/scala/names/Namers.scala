package ch.usi.inf.l3.sana.ooj.names

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import sana.brokenj
import sana.ooj

import tiny.source.Position
import tiny.util.{CompilationUnits,MonadUtils}
import tiny.contexts.{TreeId, NoId}
import tiny.passes
import tiny.names.Name
import tiny.debug.logger
import tiny.io.ClassPathCatalog


import primj.report._
import primj.modifiers.Ops.noflags

import ooj.Global
 
import scalaz.{Name => _, Failure => _, _}
import Scalaz._




trait Namers extends primj.names.Namers {
  self: IDAssigners =>

  // Namer only resolves type-uses and never idents
  override type G <: Global
  import global._


  def catalog: ClassPathCatalog
  def idassigner: IDAssigner

  trait Namer extends super.Namer {


    def loadFromClassPath(name: String, 
            owner: TreeId): NamerMonad[ClassDef] = {
      // INFO: Tree needs to have ID's assigned to
      // Namer needs to be able to talk to IDAssigners
      val clz =  loadClass(name)
      for { 
        ctx              <- getSW
        (_, clazz, ctx2) =  idassigner.assignClassDef(clz).run(owner, ctx)
        _                <- putSW(ctx2)
        r                <- nameClassDefs(clazz)
      } yield r
    }

    override def nameTrees(tree: Tree): NamerMonad[Tree] = tree match {
      case use: UseTree                              => for {
        r    <- nameUseTrees(use)
      } yield r
      case _                                         =>
        super.nameTrees(tree)
    }

    override def nameDefTrees(defTree: DefTree): 
          NamerMonad[DefTree] = defTree match {
      case ttree: TypeTree                           => for {
        r  <- nameTypeTrees(ttree)
      } yield r
      case _                                         => 
        super.nameDefTrees(defTree)
    }

    def nameTypeTrees(ttree: TypeTree): NamerMonad[TypeTree] = ttree match {
      case cdef: ClassDef                            => for {
        r <- nameClassDefs(cdef)
      } yield r
    }

    def nameClassDefs(clazz: ClassDef): NamerMonad[ClassDef] = for {
      parents <- clazz.parents.map(nameUseTrees(_)).sequenceU
      body    <- nameTemplates(clazz.body)
    } yield ClassDef(clazz.mods, clazz.id, clazz.name, parents,
                     body, clazz.pos, clazz.owner)


    def nameIdents(id: Ident): NamerMonad[Ident] = for {
      ctx       <- getSW
      name      =  id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME)
      // INFO
      // Ident here? then it should be package name (at least
      // in Java!!)
      // And the shape of the tree should be originally something like this:
      // java.lang.Object
      //
      // Creates the tree: Select(Select(Ident("java"), "lang"), "Object")
      // i.e. only the top level package is actually an Ident (this will
      // definitely change when we introduce import statement, but for now
      // this is all.
      tid_ctx2  <- ctx.findInThisContext(name, _.kind == PackageKind) match {
        case NoId                =>
          // We know that (in OOJ) only package names can be "Ident"
          catalog.defines(name.asString, false) match {
            case false              => pointSW((NoId, ctx))
            case true               =>
              // extend the context with a new package
              val pkg  = packageContext(newPackageDefInfo(name))
              // put it in the top-level
              val (id, ctx2) = ctx.extend(NoId, pkg)
              pointSW((id, ctx2))
          }
        case i                   => pointSW((i, ctx))
      }
      tid        =  tid_ctx2._1
      ctx2       =  tid_ctx2._2
      _          <- putSW(ctx2)
    } yield Ident(tid, id.owner, id.pos)


    def nameSelects(select: Select): NamerMonad[UseTree] = for {
      qual    <- nameTrees(select.qual)
      ctx     <- getSW
      name    =  select.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME)
      // qual is package name? then:
      // - if this package defines this name, then resolve this use 
      //   to that name, and it is a type use (or a package name)
      // - if it doesn't, see if this package (or probably type) is
      //   defined by the classpath
      
      // The following snippet only happens when we introduce inner classes
      // qtpe    <- toNamerMonad(qual.tpe)
      // id      =  qtpe match {
      //   // This should not happen yet, until we introduce inner classes
      //   case ct: ClassType => ct.id
      //   // INFO: What about arrays? They do not have anything to do with
      //   // type uses
      //   case _             => NoId
      // }
      id      =  qual match {
        case qual: UseTree       => qual.uses
        case _                   => NoId
      }
      // Do we have a Type with this name seeing from the qual?
      id_ctx2 =  ctx.lookup(name, _.kind.isInstanceOf[TypeKind], id) match {
        case NoId                                                    => 
          ctx.lookup(name, _.kind == PackageKind, id) match {
            case NoId             =>
              // Do we have the (package, or type) in classpath?
              val pkgs     = ctx.enclosingPackageNames(id)
              val fullName = pkgs.mkString(".") + "." + name
              // Is there a class in the classpath? with the same full name?
              // load it
              if(catalog.defines(fullName, true)) {
                val (_, (ctx2, loadedClass)) = 
                  loadFromClassPath(fullName, id).run(ctx).run
                loadedClass match {
                  case cd: ClassDef =>
                    (cd.id, ctx2)
                  case _            =>
                    // This case should never happen
                    (NoId, ctx) 
                }
              } else if(catalog.defines(fullName, false)) { 
                val info = newPackageDefInfo(name)
                ctx.extend(id, packageContext(info))
              } else {
                // Couldn't resolve the name, then don't resolve it
                (NoId, ctx) 
              }
            case id               =>
              // This name points to a package
              (id, ctx)
          }
        case id                                                      =>
          // This name points to a type
          (id, ctx)
      }
      tid     =  id_ctx2._1
      ctx2    =  id_ctx2._2
      _       <- putSW(ctx2)
    } yield Select(tid, qual, select.pos, select.owner)

    override def nameUseTrees(use: UseTree): NamerMonad[UseTree] = use match {
      case tuse: TypeUse                             => for {
        r       <- nameTypeUses(tuse)
      } yield r
      case id: Ident                                 => for {
        // Ident here? Then it should be a package name (at least
        // in OOJ!!)
        r       <- nameIdents(id)
      } yield r
      case select: Select                            => for {
        r  <- nameSelects(select)
      } yield r
    }
  }
}

