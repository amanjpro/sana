package ch.usi.inf.l3.sana.primj.names

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.source.Position
import tiny.util.{CompilationUnits,MonadUtils}
import tiny.contexts.{TreeId, NoId}
import tiny.passes
import tiny.debug.logger
import tiny.names.{Namers => _, _}
import tiny.names
import primj.modifiers.Ops._
import primj.Global
import primj.report._
import primj.contexts._

 
import scala.language.higherKinds
import scalaz.{Name => _, Failure => _, _}
import Scalaz._

trait Namers extends names.Namers {

  // Namer should do what?
  // Well, it barely should resolve the type uses of the top-level
  // definitions. Methods, Fields, Classes! and that is all.
  // It also loads classes from classpath when needed.
  // We finish name resolution in the typer phase, that is to support
  // method overloading easier, without much re-computations.
  // Typer may also load classes from classpath.
  // Apply's fun, should be taken care of, it should accept either Select
  // or Ident.
  // Select's qual should be allowed to have whatever Tree, to support
  // all possible combinations, for example m().m().m() should be allowed.
  type G <: Global
  import global._

  trait Namer extends super.Namer {


    def nameTrees(tree: Tree): NamerMonad[Tree] = tree match {
      case tmpl: Template  => for {
        r       <- nameTemplates(tmpl)
      } yield r
      case _               => pointSW(tree)
    }


    def nameTemplates(tmpl: Template): NamerMonad[Template] = for {
      members <- tmpl.members.map(nameDefTrees(_)).sequenceU
      r       <- pointSW(Template(members, tmpl.owner))
    } yield r

    def nameUseTrees(use: UseTree): NamerMonad[UseTree] = use match {
      case tuse: TypeUse                                => for {
        r <- nameTypeUses(tuse)
      } yield r
      case _                                            => pointSW(use)
    }

    def nameTypeUses(tuse: TypeUse): NamerMonad[TypeUse] = for {
      env  <- getSW
      name <- pointSW(tuse.nameAtParser.map(Name(_)).getOrElse(ERROR_NAME))
      tid  <- pointSW(env.lookup(name, _.kind.isInstanceOf[TypeKind], 
              tuse.owner))
      _    <- tid match {
                case NoId    =>
                  toNamerMonad(error(TYPE_NOT_FOUND,
                    tuse.toString, "a type", tuse.pos, tuse))
                case tid     =>
                  pointSW(())
              }
    } yield TypeUse(tid, tuse.owner, tuse.pos)

    def nameDefTrees(defTree: DefTree): NamerMonad[DefTree] = defTree match {
      case ttree: TermTree                            => for {
        r  <- nameTermTrees(ttree)
      } yield r
    }

    def nameTermTrees(ttree: TermTree): NamerMonad[TermTree] = ttree match {
      case mtree: MethodDef                           => for {
        r  <- nameMethodDefs(mtree)
      } yield r
      case vtree: ValDef                              => for {
        r  <- nameValDefs(vtree)
      } yield r
    }

    def nameMethodDefs(meth: MethodDef): NamerMonad[MethodDef] = for {
      params  <- meth.params.map(nameValDefs(_)).sequenceU
      ret     <- nameUseTrees(meth.ret)
      m       <- pointSW(MethodDef(meth.mods, meth.id, ret, meth.name,
                params, meth.body, meth.pos, meth.owner))
      info    =  newMethodDefInfo(m.mods, m.name, m.tpe)
      _       <- modifySW(_.update(meth.id, info))
    } yield m


    def nameValDefs(valdef: ValDef): NamerMonad[ValDef] = for {
      tpt     <- nameUseTrees(valdef.tpt)
      v       <- pointSW(ValDef(valdef.mods, valdef.id, tpt, valdef.name,
                    valdef.rhs, valdef.pos, valdef.owner))
      info    =  newValDefInfo(v.mods, v.name, v.tpe)
      _       <- modifySW(_.update(v.id, info))
    } yield v

  }
}
