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

    import rwst.{local => _, _}

    def loadFromClassPath(name: String, 
            owner: TreeId): NamerMonad[ClassDef] = {
      // INFO: Tree needs to have ID's assigned to
      // Namer needs to be able to talk to IDAssigners
      val clz =  loadClass(name)
      for { 
        ctx              <- get
        (_, clazz, ctx2) =  idassigner.assignClassDef(clz).run(owner, ctx)
        _                <- put(ctx2)
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

    override def nameTypeUses(tuse: TypeUse): NamerMonad[TypeUse] = for {
      ctx       <- get
      owner     =  tuse.owner
      name      =  tuse.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME)
      // Do we have a Type with this name seeing from the owner?
      nuse_ctx2 =  ctx.lookup(name, 
        _.kind.isInstanceOf[TypeKind], owner) match {
        case NoId                   =>
          // Do we have the (package, or type) in classpath?
          val pkgs     = ctx.enclosingPackageNames(owner)
          val fullName = pkgs.mkString(".") + "." + name
          // Is there a class in the classpath? with the same full name?
          // load it
          if(catalog.defines(fullName, true)) {
            val (_, loadedClass, ctx2) = 
              loadFromClassPath(fullName, owner).run(Set(), ctx)
            loadedClass match {
              case cd: ClassDef =>
                (TypeUse(cd.id, tuse.nameAtParser, owner, tuse.pos), ctx2)
              case _            =>
                // This case should never happen
                (TypeUse(NoId, tuse.nameAtParser, owner, tuse.pos), ctx2)
            }
          } else {
            // Couldn't resolve the name, then don't resolve it
            (TypeUse(NoId, tuse.nameAtParser, owner, tuse.pos), ctx)
          }
        case i                      =>
          (TypeUse(i, tuse.nameAtParser, owner, tuse.pos), ctx)
      }
      nuse       =  nuse_ctx2._1
      ctx2       =  nuse_ctx2._2
      _          <- put(ctx2)
    } yield nuse

    def nameIdents(id: Ident): NamerMonad[SimpleUseTree] = for {
      ctx       <- get
      owner     =  id.owner
      name      =  id.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME)
      // INFO
      // Ident here? Then it should be package name (or probably
      // a type name, but start with a package name first)
      //
      // The following select:
      // java.lang.Object
      //
      // will produce:
      // Select(Select(Ident("java"), Ident("lang")), TypeUse("Object"))
      //
      // After we support inner classes (Java 1.1), we need to resolve
      // the types if needed, as an example:
      // 
      // pkg.OuterClass.InnerClass
      //
      // Will be parsed as:
      // Select(Select(Ident("pkg"), Ident("OuterClass")), 
      //           TypeUse("InnerClass"))
      // 
      // But after namer should be (or if it is a local use, after typer) to:
      // Select(Select(Ident("pkg"), TypeUse("OuterClass")), 
      //           TypeUse("InnerClass"))

      // Do we have a package with this name seeing from the owner?
      nid_ctx2  =  ctx.lookup(name, _.kind == PackageKind, owner) match {
        case NoId                   =>
          // Do we have a Type with this name seeing from the owner?
          ctx.lookup(name, _.kind.isInstanceOf[TypeKind], owner) match {
            case NoId                    =>
              // Do we have the (package, or type) in classpath?
              val pkgs     = ctx.enclosingPackageNames(owner)
              val fullName = pkgs.mkString(".") + "." + name
              // Is there a class in the classpath? with the same full name?
              // load it
              if(catalog.defines(fullName, true)) {
                val (_, loadedClass, ctx2) = 
                  loadFromClassPath(fullName, owner).run(Set(), ctx)
                loadedClass match {
                  case cd: ClassDef =>
                    (TypeUse(cd.id, id.nameAtParser, owner, id.pos), ctx2)
                  case _            =>
                    // This case should never happen
                    (Ident(NoId, id.nameAtParser, owner, id.pos), ctx2)
                }
              } else if(catalog.defines(fullName, false)) { 
                val info = newPackageDefInfo(name)
                val (i, ctx2) = ctx.extend(owner, packageContext(info))
                (Ident(i, id.nameAtParser, owner, id.pos), ctx2)
              } else {
                // Couldn't resolve the name, then don't resolve it
                (Ident(NoId, id.nameAtParser, owner, id.pos), ctx)
              }
            case i                       =>
              (TypeUse(i, id.nameAtParser, owner, id.pos), ctx)
          }
        case i                      =>
          (Ident(NoId, id.nameAtParser, owner, id.pos), ctx)
      }
      nid        =  nid_ctx2._1
      ctx2       =  nid_ctx2._2
      _          <- put(ctx2)
    } yield nid


    // Before Java 1.1:
    // qual is package name? then:
    // - if this package defines this name, then resolve this use 
    //   to that name, and it is a type use (or a package name)
    // - if it doesn't, see if this package (or probably type) is
    //   defined by the classpath
    //
    // At this stage, qual cannot be Type Name or Expression name
    def nameSelects(select: Select): NamerMonad[UseTree] = for {
      qual    <- nameTrees(select.qual)
      ctx     <- get
      // The following snippet only happens when we introduce inner classes
      // qtpe    <- toNamerMonad(qual.tpe)
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
          nameIdents(Ident(id.uses, id.nameAtParser, qid, id.pos))
        case tuse: TypeUse  => 
          nameTypeUses(TypeUse(tuse.uses, tuse.nameAtParser, qid, tuse.pos))
      }
    } yield Select(qual, tree, select.pos, select.owner)

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

