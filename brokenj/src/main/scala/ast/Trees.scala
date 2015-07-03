package ch.usi.inf.l3.sana.brokenj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import tiny.source.Position
import tiny.contexts._
import tiny.names.Name
import tiny.util.MonadUtils
import calcj.ast.JavaOps._
import calcj.ast.Constants
import primj.types
import primj.ast
import tiny.modifiers.Flag


import scalaz.{Name => _, _}
import Scalaz._

trait Trees extends ast.Trees {
  self: types.Types with Constants with TreeContexts with MonadUtils
        with primj.util.Definitions =>

  /********************* AST Nodes *********************************/

  trait Label extends Expr with NamedTree {
    def name: Name
    def stmt: Expr
    def tpe: TypeState[Type] = stmt.tpe

    def show(ctx: Context): String =
      s"""|Label{
          |name=${name.asString},
          |stmt=${stmt.show(ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

    def asString(ctx: Context): String =
      s"${name.asString}: ${stmt.asString(ctx)}"

  }

  trait Break extends Expr {
    def label: Option[Name]
    def tpe: TypeState[Type] = toTypeState(VoidType)

    def show(ctx: Context): String =
      s"""|Break{
          |label=${label.map(_.asString).getOrElse("<empty>")},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

    def asString(ctx: Context): String = label match {
      case None            => "break"
      case Some(n)         => s"break ${n.asString}"
    }
  }

  trait Continue extends Expr {
    def label: Option[Name]
    def tpe: TypeState[Type] = toTypeState(VoidType)


    def asString(ctx: Context): String = label match {
      case None            => "continue"
      case Some(n)         => s"continue ${n.asString}"
    }
    def show(ctx: Context): String =
      s"""|Continue{
          |label=${label.map(_.asString).getOrElse("<empty>")},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

  }

  trait Case extends Tree {
    def guards: List[Expr]
    def body: Tree

    def asString(ctx: Context): String = {
      val cases = asStringList(guards, ctx, ":\n").map("case " + _)
      s"${cases}\n${body.asString(ctx)}"
    }

    def tpe: TypeState[Type] = toTypeState(VoidType)
    def show(ctx: Context): String =
      s"""|Case{
          |guards=${guards.map(_.show(ctx))},
          |guards=${body.show(ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

  }

  trait Switch extends Expr {
    def expr: Expr
    def cases: List[Case]
    def default: Tree

    def tpe: TypeState[Type] = toTypeState(VoidType)
    def asString(ctx: Context): String =
      s"""|switch(${expr.asString(ctx)}){
          |${asStringList(cases, ctx, "\n")}
          |default:
          |${default.asString(ctx)}""".stripMargin

    def show(ctx: Context): String =
      s"""|Switch{
          |expr=${expr.show(ctx)},
          |cases=${showList(cases, ctx)},
          |default=${default.show(ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

  }
  /***************************** Extractors **************************/

  trait LabelExtractor {
    def unapply(lbl: Label): Option[(Name, Expr)] = lbl match {
      case null     => None
      case _        => Some((lbl.name, lbl.stmt))
    }
  }

  trait SwitchExtractor {
    def unapply(switch: Switch): Option[(Expr, List[Case], Tree)] =
      switch match {
        case null     => None
        case _        => Some((switch.expr, switch.cases, switch.default))
      }
  }

  trait CaseExtractor {
    def unapply(cse: Case): Option[(List[Expr], Tree)] = cse match {
      case null     => None
      case _        => Some((cse.guards, cse.body))
    }
  }

  trait BreakExtractor {
    def unapply(brk: Break): Option[Option[Name]] = brk match {
      case null     => None
      case _        => Some(brk.label)
    }
  }

  trait ContinueExtractor {
    def unapply(cnt: Continue): Option[Option[Name]] = cnt match {
      case null     => None
      case _        => Some(cnt.label)
    }
  }

  /***************************** Factories **************************/

  trait LabelFactory {
    private class LabelImpl(val name: Name, val stmt: Expr,
      val pos: Option[Position], val owner: TreeId) extends Label

    def apply(name: Name, stmt: Expr,
      pos: Option[Position], owner: TreeId): Label =
      new LabelImpl(name, stmt, pos, owner)
  }


  trait SwitchFactory {
    private class SwitchImpl(val expr: Expr,
      val cases: List[Case], val default: Tree,
      val pos: Option[Position], val owner: TreeId) extends Switch


    def apply(expr: Expr, cases: List[Case], default: Tree,
        pos: Option[Position], owner: TreeId): Switch =
      new SwitchImpl(expr, cases, default, pos, owner)
  }

  trait CaseFactory {
    private class CaseImpl(val guards: List[Expr],
      val body: Tree,
      val pos: Option[Position], val owner: TreeId) extends Case


    def apply(guards: List[Expr], body: Tree,
        pos: Option[Position], owner: TreeId): Case =
      new CaseImpl(guards, body, pos, owner)
  }


  trait BreakFactory {
    private class BreakImpl(val label: Option[Name],
      val pos: Option[Position], val owner: TreeId) extends Break

    def apply(label: Option[Name],
      pos: Option[Position], owner: TreeId): Break =
      new BreakImpl(label, pos, owner)
  }

  trait ContinueFactory {
    private class ContinueImpl(val label: Option[Name],
      val pos: Option[Position], val owner: TreeId) extends Continue

    def apply(label: Option[Name],
      pos: Option[Position], owner: TreeId): Continue =
      new ContinueImpl(label, pos, owner)
  }


  /******************* Factory and Extractor instances ***************/


  // TODO: Only let Extractors out, or none?

  val Label     = new LabelExtractor with LabelFactory {}
  val Switch    = new SwitchExtractor with SwitchFactory {}
  val Case      = new CaseExtractor with CaseFactory {}
  val Break     = new BreakExtractor with BreakFactory {}
  val Continue  = new ContinueExtractor with ContinueFactory {}
}
