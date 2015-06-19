package ch.usi.inf.l3.sana.ooj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import tiny.source.Position
import tiny.contexts._
import tiny.names.Name
import tiny.modifiers.Flag
import tiny.util.MonadUtils
import calcj.ast.JavaOps._
import primj.types
import brokenj.ast


import scalaz.{Name => _, _}
import Scalaz._

trait Trees extends ast.Trees {
  self: types.Types with Constants with TreeContexts with MonadUtils =>

  /********************* AST Nodes *********************************/

  trait ClassDef extends DefTree with Modifiable {
    def mods: Flag
    def name: Name
    def parents: List[TypeUse]
    def body: Template

    def show(ctx: Context): String = 
      s"""|ClassDef{
          |mods=${mods.asString}
          |name=${name.asString},
          |parents=${showList(parents, ctx)},
          |body=${body.show(ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

  } 

  trait New extends Expr {
    def tpt: UseTree
    def tpe: TypeState[Type] = tpt.tpe

    def show(ctx: Context): String = 
      s"""|New{
          |tpt=${tpt.show(ctx)}
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin
  }

  trait Select extends UseTree {
    def qual: UseTree

    def show(ctx: Context): String = 
      s"""|Select{
          |uses=$uses,
          |qual=${qual.show(ctx)},
          |nameAtParser=$nameAtParser,
          |owner=$owner,
          |pos=$pos,
          |name=${name(ctx)._2}
          |}""".stripMargin
  }

  trait This extends Expr {
    def enclosingClass: TreeId

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

  

  trait ClassDefExtractor {
    def unapply(cd: ClassDef): Option[(Flag, Name, 
                  List[TypeUse], Template)] = cd match {
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

  /***************************** Factories **************************/


  trait ClassDefFactory {
    private class ClassDefImpl(val mods: Flag, val id: TreeId, 
      val name: Name, val parents: List[TypeUse], val body: Template,
      val tpe: TypeState[Type], val pos: Option[Position], 
      val owner: TreeId) extends ClassDef

    def apply(mods: Flag, id: TreeId, name: Name, parents: List[TypeUse],
      body: Template, tpe: TypeState[Type], 
      pos: Option[Position], owner: TreeId): ClassDef =
      new ClassDefImpl(mods, id, name, parents, body, tpe, pos, owner)
  }


  trait NewFactory {
    private class NewImpl(val tpt: UseTree, val pos: Option[Position],
      val owner: TreeId) extends New

    def apply(tpt: UseTree, pos: Option[Position], owner: TreeId): New =
      new NewImpl(tpt, pos, owner)
  }

  trait SelectFactory {
    private class SelectImpl(val qual: UseTree, val uses: TreeId,
      val nameAtParser: Option[String],
      val pos: Option[Position], val owner: TreeId) extends Select

    def apply(qual: UseTree, uses: TreeId,
      nameAtParser: Option[String],
      pos: Option[Position], owner: TreeId): Select =
        new SelectImpl(qual, uses, nameAtParser, pos, owner)

    def apply(qual: UseTree, uses: TreeId,
      pos: Option[Position], owner: TreeId): Select =
        new SelectImpl(qual, uses, None, pos, owner)
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

  val ClassDef  = new ClassDefExtractor with ClassDefFactory {}
  val New       = new NewExtractor with NewFactory {}
  val Select    = new SelectFactory {}
  val This      = new ThisFactory {}
  val Super     = new SuperFactory {}
}
