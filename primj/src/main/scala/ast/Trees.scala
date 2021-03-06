package ch.usi.inf.l3.sana.primj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import tiny.source.Position
import tiny.contexts._
import tiny.modifiers.Flags
import primj.modifiers._
import primj.modifiers.Ops._
import tiny.names.Name
import tiny.util.MonadUtils
import calcj.ast.JavaOps._
import calcj.ast.Constants
import calcj.ast
import primj.types


import scalaz.{Name => _, _}
import Scalaz._

trait Trees extends ast.Trees {
  self: types.Types with Constants with TreeContexts with MonadUtils
        with primj.util.Definitions =>

  /********************* AST Nodes *********************************/

  /**
    * Template is the AST node that represents the body of classes and
    * interfaces.
    *
    * [[primj]] has no class or interface, but having this node makes
    * going having multiple definitions in a single compilation unit.
    *
    * @group Api
    */
  trait Template extends Tree {
    /**
      * List of definitions defined in this template.
      */
    def members: List[DefTree]
    val tpe: TypeState[Type] = toTypeState(VoidType)
    def pos: Option[Position] = None

    def asString(ctx: Context): String =
      s"{\n${asStringList(members, ctx, "\n")}\n}"

    def show(ctx: Context): String =
      s"Template{${showList(members, ctx)}}"
  }

  // Variable and Method definitions
  trait MethodDef extends TermTree {
    def ret: UseTree
    def params: List[ValDef]
    def body: Expr
    val tpe: TypeState[Type] = {
      val tys = params.map(_.tpe).sequenceU
      for {
        r   <- ret.tpe
        tps <- tys
        ty  <- toTypeState(MethodType(r, tps))
      } yield ty
    }

    def asString(ctx: Context): String =
      s"""|${mods.asString}
          |${ret.asString(ctx)} ${name.asString}(${asStringList(params, ctx)}) {
          |${body.asString(ctx)}
          |}""".stripMargin

    def show(ctx: Context): String =
      s"""|MethodDef{
          |mods=${mods.asString},
          |id=${id},
          |ret=${ret.show(ctx)},
          |name=${name.toString},
          |params=${showList(params, ctx)},
          |body=${body.show(ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin


  }

  trait ValDef extends TermTree {
    def tpt: UseTree
    def rhs: Expr
    val tpe: TypeState[Type] = tpt.tpe

    def asString(ctx: Context): String =
      s"${mods.asString} ${tpt.asString(ctx)} $name = ${rhs.asString(ctx)}"

    def show(ctx: Context): String =
      s"""|ValDef{
          |mods=${mods.asString},
          |id=${id},
          |tpt=${tpt.show(ctx)},
          |name=${name.toString},
          |rhs=${rhs.show(ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin


  }



  trait Return extends Expr {
    val expr: Option[Expr]
    val tpe: TypeState[Type] = expr.map(_.tpe).getOrElse(toTypeState(VoidType))

    def isVoid: Boolean = expr == None


    def asString(ctx: Context): String = expr match {
      case None           => "return"
      case Some(e)        => s"return ${e.asString(ctx)}"
    }
    def show(ctx: Context): String =
      s"""|Return{
          |expr=${expr.map(_.show(ctx))},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin


  }

  trait Block extends Expr with IdentifiedTree {
    def stmts: List[Tree]

    val tpe: TypeState[Type] = stmts match {
      case Nil =>
        toTypeState(VoidType)
      case _   =>
        stmts.last.tpe
    }

    def asString(ctx: Context): String =
      s"""|{
          |${asStringList(stmts, ctx, ";\n")}
          |}""".stripMargin

    def show(ctx: Context): String =
      s"""|Block{
          |id=$id,
          |stmts=${showList(stmts, ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin
  }



  trait Assign extends Expr {
    def lhs: Expr
    def rhs: Expr
    val tpe: TypeState[Type] = lhs.tpe

    def asString(ctx: Context): String =
      s"${lhs.asString(ctx)} = ${rhs.asString(ctx)}"

    def show(ctx: Context): String =
      s"""|Assign{
          |lhs=${lhs.show(ctx)},
          |rhs=${rhs.show(ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

  }

  trait If extends Expr {
    def cond: Expr
    def thenp: Expr
    def elsep: Expr
    val tpe: TypeState[Type] = toTypeState(VoidType)

    def asString(ctx: Context): String =
      s"""|if(${cond.asString(ctx)})
          |${thenp.asString(ctx)}
          |else
          |${elsep.asString(ctx)}""".stripMargin

    def show(ctx: Context): String =
      s"""|If{
          |cond=${cond.show(ctx)},
          |thenp=${thenp.show(ctx)},
          |elsep=${elsep.show(ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

  }


  trait While extends Expr with Modifiable {
    def cond: Expr
    def body: Expr
    val tpe: TypeState[Type] = toTypeState(VoidType)

    def asString(ctx: Context): String = mods.isDoWhile match {
      case false                       =>
        s"""|while(${cond.asString(ctx)})
            |${body.asString(ctx)}""".stripMargin
      case true                        =>
        s"""|do
            |${body.asString(ctx)}
            |(${cond.asString(ctx)})""".stripMargin
    }

    def show(ctx: Context): String =
      s"""|While{
          |mods=${mods.asString}
          |cond=${cond.show(ctx)},
          |body=${body.show(ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

  }

  trait For extends Expr with IdentifiedTree {
    def inits: List[Tree]
    def cond: Expr
    def steps: List[Expr]
    def body: Expr
    val tpe: TypeState[Type] = toTypeState(VoidType)

    def asString(ctx: Context): String =
      s"""|for(${asStringList(inits, ctx)}; ${cond.asString(ctx)}; ${asStringList(steps, ctx)})
          |(${body.asString(ctx)})""".stripMargin
    def show(ctx: Context): String =
      s"""|For{
          |id=$id,
          |inits=${showList(inits, ctx)},
          |cond=${cond.show(ctx)},
          |steps=${showList(steps, ctx)},
          |body=${body.show(ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

  }

  // Ternary operator
  trait Ternary extends Expr {
    def cond: Expr
    def thenp: Expr
    def elsep: Expr

    def asString(ctx: Context): String =
      s"${cond.asString(ctx)}?${thenp.asString(ctx)}:${elsep.asString(ctx)}"

    def show(ctx: Context): String =
      s"""|Ternary{
          |cond=${cond.show(ctx)},
          |thenp=${thenp.show(ctx)},
          |elsep=${elsep.show(ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

  }

  // Apply
  trait Apply extends Expr {
    def fun: Expr
    def args: List[Expr]

    def asString(ctx: Context): String =
      s"${fun.asString(ctx)}(${asStringList(args, ctx)})"

    def show(ctx: Context): String =
      s"""|Apply{
          |fun=${fun.show(ctx)},
          |args=${showList(args, ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin

    val tpe: TypeState[Type] = for {
      funty <- fun.tpe
    } yield {
      funty match {
        case MethodType(r, _) => r
        case _                => NoType
      }
    }


  }





  /***************************** Extractors **************************/
  trait TemplateExtractor {
    def unapply(tmpl: Template): Option[List[DefTree]] = tmpl match {
      case null     => None
      case _        => Some(tmpl.members)
    }
  }

  trait AssignExtractor {
    def unapply(a: Assign): Option[(Expr, Expr)] = a match {
      case null => None
      case _    => Some((a.lhs, a.rhs))
    }
  }

  trait IfExtractor {
    def unapply(i: If): Option[(Expr, Expr, Expr)] = i match {
      case null => None
      case _    => Some((i.cond, i.thenp, i.elsep))
    }
  }


  trait WhileExtractor {
    def unapply(w: While): Option[(Flags, Expr, Expr)] = w match {
      case null => None
      case _    => Some((w.mods, w.cond, w.body))
    }
  }

  trait ForExtractor {
    def unapply(f: For):
      Option[(List[Tree], Expr, List[Expr], Expr)] = f match {
      case null => None
      case _    => Some((f.inits, f.cond, f.steps, f.body))
    }
  }
  trait BlockExtractor {
    def unapply(b: Block): Option[List[Tree]] = b match {
      case null => None
      case _    => Some(b.stmts)
    }
  }


  trait TernaryExtractor {
    def unapply(t: Ternary): Option[(Expr, Expr, Expr)] = t match {
      case null => None
      case _    => Some((t.cond, t.thenp, t.elsep))
    }
  }

  trait ApplyExtractor {
    def unapply(a: Apply): Option[(Expr, List[Expr])] = a match {
      case null => None
      case _    => Some((a.fun, a.args))
    }
  }

  trait MethodDefExtractor {
    def unapply(md: MethodDef):
      Option[(Flags, UseTree, Name, List[ValDef], Expr)] = md match {
        case null => None
        case _    => Some((md.mods, md.ret, md.name, md.params, md.body))
      }
  }

  trait ValDefExtractor {
    def unapply(vd: ValDef): Option[(Flags, UseTree, Name, Tree)] =
      vd match {
        case null => None
        case _    => Some((vd.mods, vd.tpt, vd.name, vd.rhs))
      }
  }
  trait ReturnExtractor {
    def unapply(r: Return): Option[Option[Expr]] = r match {
      case null => None
      case _    => Some(r.expr)
    }
  }

  // trait VoidReturnExtractor {
  //   def unapply(r: Return): Option[Expr] = r match {
  //     case null => None
  //     case _    => for(e <- r.expr) yield e
  //   }
  // }
  /***************************** Factories **************************/
  trait TemplateFactory {
    private class TemplateImpl(val members: List[DefTree],
      val owner: TreeId) extends Template

    def apply(members: List[DefTree],
      owner: TreeId = NoId): Template =
      new TemplateImpl(members, owner)
  }

  trait AssignFactory {
    private class AssignImpl(val lhs: Expr,
      val rhs: Expr, val pos: Option[Position],
      val owner: TreeId) extends Assign

    def apply(lhs: Expr, rhs: Expr, pos: Option[Position] = None,
      owner: TreeId = NoId): Assign =
      new AssignImpl(lhs, rhs, pos, owner)

  }
  trait IfFactory {
    private class IfImpl(val cond: Expr, val thenp: Expr,
      val elsep: Expr, val pos: Option[Position],
      val owner: TreeId) extends If


    def apply(cond: Expr, thenp: Expr, elsep: Expr,
      pos: Option[Position] = None, owner: TreeId = NoId): If =
      new IfImpl(cond, thenp, elsep, pos, owner)
  }


  trait WhileFactory {
    private class WhileImpl(val mods: Flags, val cond: Expr,
      val body: Expr, val pos: Option[Position],
      val owner: TreeId) extends While


    def apply(mods: Flags, cond: Expr, body: Expr,
      pos: Option[Position] = None, owner: TreeId = NoId): While =
      new WhileImpl(mods, cond, body, pos, owner)

  }

  trait BlockFactory {
    private class BlockImpl(val id: TreeId,
      val stmts: List[Tree], val pos: Option[Position],
      val owner: TreeId) extends Block


    def apply(id: TreeId, stmts: List[Tree],
      pos: Option[Position] = None, owner: TreeId = NoId): Block =
        new BlockImpl(id, stmts, pos, owner)

  }

  trait ForFactory {
    private class ForImpl(val id: TreeId, val inits: List[Tree],
      val cond: Expr, val steps: List[Expr], val body: Expr,
        val pos: Option[Position], val owner: TreeId) extends For

    def apply(id: TreeId, inits: List[Tree], cond: Expr, steps: List[Expr],
      body: Expr, pos: Option[Position] = None,
      owner: TreeId = NoId): For =
        new ForImpl(id, inits, cond, steps, body, pos, owner)
  }

  trait TernaryFactory {
    private class TernaryImpl(val cond: Expr, val thenp: Expr,
      val elsep: Expr, val tpe: TypeState[Type],
      val pos: Option[Position] = None, val owner: TreeId) extends Ternary


    def apply(cond: Expr, thenp: Expr, elsep: Expr, tpe: TypeState[Type],
      pos: Option[Position] = None, owner: TreeId): Ternary =
      new TernaryImpl(cond, thenp, elsep, tpe, pos, owner)
  }

  trait ApplyFactory {
    private class ApplyImpl(val fun: Expr, val args: List[Expr],
      val pos: Option[Position], val owner: TreeId) extends Apply


    def apply(fun: Expr, args: List[Expr], pos: Option[Position] = None,
      owner: TreeId = NoId): Apply =
        new ApplyImpl(fun, args, pos, owner)

  }

  trait ReturnFactory {
    private class ReturnImpl(val expr: Option[Expr],
    val pos: Option[Position], val owner: TreeId) extends Return


    def apply(expr: Expr, pos: Option[Position],
      owner: TreeId): Return = new ReturnImpl(Some(expr), pos, owner)

    def apply(pos: Option[Position], owner: TreeId): Return =
      new ReturnImpl(None, pos, owner)
  }


  trait MethodDefFactory {
    private class MethodDefImpl(val mods: Flags, val id: TreeId,
      val ret: UseTree, val name: Name, val params: List[ValDef],
      val body: Expr, val pos: Option[Position],
      val owner: TreeId) extends MethodDef

    def apply(mods: Flags, id: TreeId, ret: UseTree, name: Name,
      params: List[ValDef], body: Expr, pos: Option[Position] = None,
      owner: TreeId): MethodDef =
        new MethodDefImpl(mods, id, ret, name, params, body, pos, owner)
  }

  trait ValDefFactory {
    private class ValDefImpl(val mods: Flags, val id: TreeId,
      val tpt: UseTree, val name: Name, val rhs: Expr,
      val pos: Option[Position], val owner: TreeId) extends ValDef

    def apply(mods: Flags, id: TreeId, tpt: UseTree, name: Name,
      rhs: Expr, pos: Option[Position] = None,
      owner: TreeId = NoId): ValDef =
        new ValDefImpl(mods, id, tpt, name, rhs, pos, owner)
  }


  /******************* Factory and Extractor instances ***************/


  // TODO: Only let Extractors out, or none?

  val Template  = new TemplateExtractor with TemplateFactory {}
  val Assign    = new AssignExtractor with AssignFactory {}
  val If        = new IfExtractor with IfFactory {}
  val While     = new WhileExtractor with WhileFactory {}
  val For       = new ForExtractor with ForFactory {}
  val Block     = new BlockExtractor with BlockFactory {}
  val Ternary   = new TernaryExtractor with TernaryFactory {}
  val Apply     = new ApplyExtractor with ApplyFactory {}
  val Return    = new ReturnExtractor with ReturnFactory {}
  val MethodDef = new MethodDefExtractor with MethodDefFactory {}
  val ValDef    = new ValDefExtractor with ValDefFactory {}
}
