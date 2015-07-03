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
import tiny.report.Report
import tiny.names.Name
import tiny.debug.logger
import tiny.io.ClassPathCatalog


import primj.report._
import primj.modifiers.Ops.noflags

import ooj.Global

import scalaz.{Name => _, Failure => _, _}
import Scalaz._



trait IDAssigners extends brokenj.names.IDAssigners {
  override type G <: Global
  import global._


  trait IDAssigner extends super.IDAssigner {

    import rwst.{local => _, _}

    override def startPhase(state: Context, unit: CompilationUnit):
         (Vector[Report], CompilationUnit, Context) = unit.tree match {
      case pkg: PackageDef =>
        // the owner of a package name is the parent package!!
        val (pid, cid, state2) = {
          val pkgs = pkg.name.asString.split(".").toList
          extendPackages(state, NoId, pkgs)
        }
        // The owner of a package is the parent package, and this can
        // be found by going one step up from its id (path).
        val pkg2 = PackageDef(pkg.id, pkg.name, pkg.members, pkg.pos, pid.up)

        // The owner of the members of a package, is the compilation unit,
        // which in turn is owned by the package
        val (w, namedTree, s) = assignPackageDef(pkg).run(cid, state)
        logger.debug(namedTree.show(s))
        (w, CompilationUnit(cid, namedTree, unit.fileName), s)
      case _               =>
        super.startPhase(state, unit)
    }


    def assignNew(nw: New): IDAssignerMonad[New] = for {
      owner       <- ask
      tpt         <- assignUseTree(nw.tpt)
      args        <- nw.args.map(assignExpr(_)).sequenceU
    } yield New(tpt, args, nw.pos, owner)


    override def assignExpr(expr: Expr): IDAssignerMonad[Expr] = expr match {
      case nw: New                                             => for {
        r     <- assignNew(nw)
      } yield r
      case slct: Select                                        => for {
        r     <- assignSelect(slct)
      } yield r

      case _                                                   =>
        super.assignExpr(expr)
    }

    override def assign(tree: Tree): IDAssignerMonad[Tree] = tree match {
      case pkg: PackageDef                           => for {
        r       <- assignPackageDef(pkg)
      } yield r
      case clazz: ClassDef                           => for {
        r       <- assignClassDef(clazz)
      } yield r
      case select: Select                            => for {
        r       <- assignSelect(select)
      } yield r
      case _                                         =>
        super.assign(tree)
    }


    override def assignDef(dtree: DefTree): IDAssignerMonad[DefTree] =
      dtree match {
      case clazz: ClassDef                           => for {
        r       <- assignClassDef(clazz)
      } yield r
      case _                                         =>
        super.assignDef(dtree)
    }

    override def assignUseTree(use: UseTree): IDAssignerMonad[UseTree] =
      use match {
        case s: Select => for {
          r <- assignSelect(s)
        } yield r
        case _         => super.assignUseTree(use)
      }


    def assignSelect(select: Select): IDAssignerMonad[Select] = for {
      owner   <- ask
      qual    <- assign(select.qual)
      tree    <- assignUseTree(select.tree) match {
        case s: SimpleUseTree     => point(s)
        case _                    => point(select.tree)
      }
    } yield Select(qual, tree, select.pos, owner)

    /**
     * Assign id's the members of PackageDef. The package definition itself
     * should have an id already by the caller.
     */
    def assignPackageDef(pkg: PackageDef): IDAssignerMonad[PackageDef] = for {
      owner        <- ask
      members      <- pkg.members.map((x) =>
          local((_: TreeId) => owner)(assignDef(x))).sequenceU
    } yield {
      PackageDef(pkg.id, pkg.name, members, pkg.pos, pkg.owner)
    }

    /**
     * Extends the context with the given nested packages `pkgs`.
     * And returns the `id` for the inner most package, and
     * its compilation unit id, and the resulting context.
     */
    protected def extendPackages(ctx: Context, prev: TreeId,
        pkgs: List[String]): (TreeId, TreeId, Context) = pkgs match {
      case Nil              => (prev, NoId, ctx)
      case (x::xs)          =>
        val nme = Name(x)
        val id = ctx.getContext(prev) match {
          case None                =>
            NoId
          case Some(ctx)           =>
            ctx.findInThisContext(nme, _.kind == PackageKind)
        }
        val (id2, ctx2) = if(id != NoId) {
          (id, ctx)
        } else {
          val info        = newPackageDefInfo(nme)
          val pkgContext  = packageContext(info)
          ctx.extend(prev, pkgContext)
        }

        if(xs == Nil) {
          // TODO: make it compilation unit context???
          val (id3, ctx3) = ctx2.extend(id2, emptyContext)
          (id2, id3, ctx3)
        } else {
          extendPackages(ctx2, id2, xs)
        }
    }


    def assignClassDef(clazz: ClassDef): IDAssignerMonad[ClassDef] = for {
      owner   <- ask
      ctx1    <- get
      id_ctx2 =  ctx1.extend(owner, classContext(clazz))
      id      =  id_ctx2._1
      ctx2    =  id_ctx2._2
      body    <- assignTemplate(clazz.body)
    } yield ClassDef(clazz.mods, id, clazz.name, clazz.parents, body,
                     clazz.pos, owner)
  }
}
