package ch.usi.inf.l3.sana.ooj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj

import tiny.source.Position
import tiny.contexts._
import tiny.names.Name
import tiny.modifiers.Flags
import tiny.util.MonadUtils

import calcj.ast.JavaOps._

import brokenj.ast

import ooj.modifiers._
import ooj.types


import scalaz.{Name => _, _}
import Scalaz._

trait Trees extends ast.Trees {
  self: types.Types with Constants with TreeContexts with MonadUtils
        with ooj.util.Definitions with ooj.contexts.TreeContextApis
        with ooj.contexts.TreeInfos =>

  /********************* AST Nodes *********************************/

  trait PackageDef extends NamedTree with IdentifiedTree {
    def members: List[DefTree]

    def tpe: TypeState[Type] = toTypeState(notype)
    def show(ctx: Context): String =
      s"""|PackageDef{
          |name=${name.asString},
          |members=${showList(members, ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

    def asString(ctx: Context): String =
      s"""|package ${name.asString}
          |${asStringList(members, ctx, "\n")}
          |""".stripMargin

  }

  trait ClassDef extends TypeTree {
    def name: Name
    def parents: List[UseTree]
    def body: Template

    def tpe: TypeState[Type] = for {
      ctx     <- get
      ptpes   <- parents.map(_.tpe).sequenceU
      ptpes2  =  ptpes.toSet
      // Is it java.lang.Object? use ObjectType then
      ty      <- if(name == OBJECT_TYPE_NAME) {
        val pckgLang = ctx.enclosingPackage(id)
        if(ctx.getName(pckgLang) == Some(Name("lang"))) {
          val pckgJava = ctx.enclosingPackage(pckgLang)
          if(ctx.getName(pckgJava) == Some(Name("java"))) {
            toTypeState(ObjectType(id))
          } else {
            toTypeState(ClassType(id, name, ptpes2))
          }
        } else {
          toTypeState(ClassType(id, name, ptpes2))
        }
      } else {
        toTypeState(ClassType(id, name, ptpes2))
      }
    } yield ty

    def show(ctx: Context): String =
      s"""|ClassDef{
          |mods=${mods.asString},
          |name=${name.asString},
          |parents=${showList(parents, ctx)},
          |body=${body.show(ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

    def asString(ctx: Context): String =
      s"""|${mods.asString}
          |class ${name.asString} extends ${asStringList(parents, ctx)}
          |${body.asString(ctx)}
          |}""".stripMargin
  }

  trait New extends Expr {
    def tpt: UseTree
    def args: List[Expr]
    def tpe: TypeState[Type] = tpt.tpe

    def asString(ctx: Context): String =
      s"new ${tpt.asString(ctx)}(${asStringList(args, ctx)})"

    def show(ctx: Context): String =
      s"""|New{
          |tpt=${tpt.show(ctx)}
          |args=${showList(args, ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin
  }

  trait Select extends UseTree with Expr {
    def qual: Tree
    def tree: SimpleUseTree

    override def name: ContextState[Name] = tree.name
    def uses: TreeId = tree.uses
    def nameAtParser: Option[String] = tree.nameAtParser

    def asString(ctx: Context): String =
      s"${qual.asString(ctx)}.${name(ctx)._2}"

    def show(ctx: Context): String =
      s"""|Select{
          |uses=$uses,
          |qual=${qual.show(ctx)},
          |nameAtParser=$nameAtParser,
          |owner=$owner,
          |pos=$pos,
          |tree=${tree.show(ctx)}
          |}""".stripMargin
  }

  trait This extends Expr {
    def enclosingId: TreeId

    def asString(ctx: Context): String = "this"

    def show(ctx: Context): String =
      s"""|This{
          |owner=$owner,
          |enclosingId=$enclosingId,
          |pos=$pos,
          |}""".stripMargin


    def tpe: TypeState[Type] = StateT {
      case ctx  =>
        ctx.getTpe(owner).run(ctx)
    }
  }

  trait Super extends Expr {
    def enclosingId: TreeId


    def asString(ctx: Context): String = "super"

    def tpe: TypeState[Type] = for {
      ctx       <- get
      ptpe      <- ctx.getTree(owner) match {
        case Some(t)          =>
          val ty = t.tpe.eval(ctx)
          ty match {
            case ct: ClassType     =>
              val pids = ct.parents.foldLeft(Nil: List[TreeId])((z, y) => {
                y match {
                  case ct: ClassType       => (ct.id)::z
                  case _                   => z
                }
              })
              pids.filter(ctx.isInterface(_)) match {
                case List(x)       => ctx.getTpe(x)
                case _             => toTypeState(notype)
              }
            case _                 => toTypeState(notype)
          }
        case None             => toTypeState(notype)
      }
    } yield ptpe

    def show(ctx: Context): String =
      s"""|Super{
          |owner=$owner,
          |enclosingId=$enclosingId,
          |pos=$pos,
          |}""".stripMargin
  }

  /***************************** Extractors **************************/

  trait PackageDefExctractor {

    def unapply(pdef: PackageDef): Option[(Name, List[DefTree])] = pdef match {
      case null            => None
      case _               => Some((pdef.name, pdef.members))
    }
  }

  trait ClassDefExtractor {
    def unapply(cd: ClassDef): Option[(Flags, Name,
                  List[UseTree], Template)] = cd match {
      case null     => None
      case _        => Some((cd.mods, cd.name, cd.parents, cd.body))
    }
  }


  trait NewExtractor {
    def unapply(nw: New): Option[(UseTree, List[Expr])] = nw match {
      case null     => None
      case _        => Some((nw.tpt, nw.args))
    }
  }

  trait SelectExtractor {
    def unapply(select: Select): Option[(Tree, SimpleUseTree)] = select match {
      case null            => None
      case _               => Some((select.qual, select.tree))
    }
  }

  /***************************** Factories **************************/

  trait PackageDefFactory {
    private class PackageDefImpl(val id: TreeId,
      val name: Name, val members: List[DefTree],
      val pos: Option[Position], val owner: TreeId) extends PackageDef

    def apply(id: TreeId, name: Name, members: List[DefTree],
      pos: Option[Position], owner: TreeId): PackageDef = {
      new PackageDefImpl(id, name, members, pos, owner)
    }
  }

  trait ClassDefFactory {
    private class ClassDefImpl(val mods: Flags, val id: TreeId,
      val name: Name, val parents: List[UseTree], val body: Template,
      val pos: Option[Position], val owner: TreeId) extends ClassDef

    def apply(mods: Flags, id: TreeId, name: Name, parents: List[UseTree],
      body: Template, pos: Option[Position], owner: TreeId): ClassDef = {
      new ClassDefImpl(mods, id, name, parents, body, pos, owner)
    }
  }


  trait NewFactory {
    private class NewImpl(val tpt: UseTree, val args: List[Expr],
      val pos: Option[Position], val owner: TreeId) extends New

    def apply(tpt: UseTree, args: List[Expr],
      pos: Option[Position], owner: TreeId): New =
      new NewImpl(tpt, args, pos, owner)
  }

  trait SelectFactory {
    private class SelectImpl(val qual: Tree, val tree: SimpleUseTree,
      val pos: Option[Position], val owner: TreeId) extends Select

    def apply(qual: Tree, tree: SimpleUseTree,
      pos: Option[Position], owner: TreeId): Select =
        new SelectImpl(qual, tree, pos, owner)
  }

  trait ThisFactory {
    private class ThisImpl(val enclosingId: TreeId,
      val pos: Option[Position], val owner: TreeId) extends This

    def apply(enclosingId: TreeId, pos: Option[Position],
      owner: TreeId): This = new ThisImpl(enclosingId, pos, owner)
  }

  trait SuperFactory {
    private class SuperImpl(val enclosingId: TreeId,
      val pos: Option[Position], val owner: TreeId) extends Super

    def apply(enclosingId: TreeId, pos: Option[Position],
      owner: TreeId): Super = new SuperImpl(enclosingId, pos, owner)
  }



  /******************* Factory and Extractor instances ***************/


  // TODO: Only let Extractors out, or none?

  val PackageDef  = new PackageDefExctractor with PackageDefFactory {}
  val ClassDef    = new ClassDefExtractor with ClassDefFactory {}
  val New         = new NewExtractor with NewFactory {}
  val Select      = new SelectFactory with SelectExtractor {}
  val This        = new ThisFactory {}
  val Super       = new SuperFactory {}
}
