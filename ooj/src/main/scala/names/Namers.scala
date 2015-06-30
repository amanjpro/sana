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


    // Call this when needed and fix it
    def loadFromClassPath(name: String, 
            isClass: Boolean, owner: TreeId): NamerMonad[Tree] = {
      // INFO: Tree needs to have ID's assigned to
      // Namer needs to be able to talk to IDAssigners
      val clz =  loadClass(name)
      for { 
        ctx              <- getSW
        (_, clazz, ctx2) =  idassigner.assign(clz).run(owner, ctx)
        _                <- putSW(ctx2)
        r                <- nameTrees(clazz)
      } yield r
    }

    override def nameTrees(tree: Tree): NamerMonad[Tree] = tree match {
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


    
    override def nameUseTrees(use: UseTree): NamerMonad[UseTree] = use match {
      case tuse: TypeUse                             => for {
        r       <- nameTypeUses(tuse)
      } yield r
      case select: Select                            => for {
        qual    <- nameTrees(select.qual)
        ctx     <- getSW
        name    =  use.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME)
        // qual is package name? then:
        // - if this package defines this name, then resolve this use 
        //   to that name, and it is a type use
        // - if it doesn't, then resolve this to a package name.
        

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
        res     <- ctx.lookup(name, _.kind.isInstanceOf[TypeKind], id) match {
          case NoId                                                    => 
            ctx.lookup(name, _.kind == PackageKind, id) match {
              case NoId             =>
                // We know it has been wrongly resolved to a package name,
                // but for now just return a Select with NoId
                pointSW(Select(NoId, qual, select.pos, select.owner))
              case id               =>
                // This name points to a package
                pointSW(Select(id, qual, select.pos, select.owner))
            }
          case id                                                      =>
            // This name points to a type
            pointSW(Select(id, qual, select.pos, select.owner))
        }
      } yield res
    }
  }
}

