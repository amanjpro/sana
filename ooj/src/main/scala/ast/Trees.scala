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
        with ooj.util.Definitions with ooj.contexts.TreeContextApis =>

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
    def tpe: TypeState[Type] = tpt.tpe

    def asString(ctx: Context): String =
      s"new ${tpt.asString(ctx)}"
    def show(ctx: Context): String = 
      s"""|New{
          |tpt=${tpt.show(ctx)}
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
    def enclosingClass: TreeId

    def asString(ctx: Context): String = "this"

    def show(ctx: Context): String = 
      s"""|This{
          |owner=$owner,
          |enclosingClass=$enclosingClass,
          |pos=$pos,
          |}""".stripMargin


    def tpe: TypeState[Type] = StateT {
      case ctx  =>
        ctx.getTpe(enclosingClass).run(ctx)
    }
  }

  trait Super extends Expr {
    def enclosingClass: TreeId


    def asString(ctx: Context): String = "super"
    // TODO:
    // Get parent's type, not this
    // XXX: BUT WHICH ONE?
    def tpe: TypeState[Type] = StateT {
      case ctx  =>
        ctx.getTpe(enclosingClass).run(ctx)
    }

    def show(ctx: Context): String = 
      s"""|Super{
          |owner=$owner,
          |enclosingClass=$enclosingClass,
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
    def unapply(nw: New): Option[UseTree] = nw match {
      case null     => None
      case _        => Some(nw.tpt)
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
    private class NewImpl(val tpt: UseTree, val pos: Option[Position],
      val owner: TreeId) extends New

    def apply(tpt: UseTree, pos: Option[Position], owner: TreeId): New =
      new NewImpl(tpt, pos, owner)
  }

  trait SelectFactory {
    private class SelectImpl(val qual: Tree, val tree: SimpleUseTree,
      val pos: Option[Position], val owner: TreeId) extends Select

    def apply(qual: Tree, tree: SimpleUseTree,
      pos: Option[Position], owner: TreeId): Select =
        new SelectImpl(qual, tree, pos, owner)
  }

  trait ThisFactory {
    private class ThisImpl(val enclosingClass: TreeId,
      val pos: Option[Position], val owner: TreeId) extends This

    def apply(enclosingClass: TreeId, pos: Option[Position], 
      owner: TreeId): This = new ThisImpl(enclosingClass, pos, owner)
  }

  trait SuperFactory {
    private class SuperImpl(val enclosingClass: TreeId,
      val pos: Option[Position], val owner: TreeId) extends Super

    def apply(enclosingClass: TreeId, pos: Option[Position], 
      owner: TreeId): Super = new SuperImpl(enclosingClass, pos, owner)
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
