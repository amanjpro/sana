package ch.usi.inf.l3.sana.primj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import tiny.modifiers.Flags
import tiny.source.Position
import tiny.contexts._
import tiny.names.Name
import tiny.util.MonadUtils
import calcj.ast.JavaOps._
import calcj.ast.Constants
import calcj.ast
import primj.types


import scalaz.{Name => _, _}
import Scalaz._

/**
   TODO:
   Switch - Case
   Break
   Continue
 */

trait Trees extends ast.Trees {
  self: types.Types with Constants with TreeContexts with MonadUtils =>

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
    def tpe: TypeState[Type] = toTypeState(VoidType)
    def pos: Option[Position] = None
  }

  // Variable and Method definitions
  trait MethodDef extends TermTree {
    def ret: TypeUse
    def params: List[ValDef]
    def body: Expr
    def tpe: TypeState[Type] = {
      val tys = params.map(_.tpe).sequenceU
      for {
        r   <- ret.tpe
        tps <- tys
        ty  <- toTypeState(MethodType(r, tps))
      } yield ty
    }


    override def toString: String = 
      s"""|${mods} ${ret} ${name} (${params.mkString(", ")}) {
          |  ${body}
          |}""".stripMargin

  }
  
  trait ValDef extends TermTree {
    def tpt: TypeUse
    def rhs: Expr
    def tpe: TypeState[Type] = tpt.tpe
    override def toString: String = s"${mods} ${tpt} ${name} = ${rhs}"
  }
  


  trait Return extends Expr {
    val expr: Option[Expr]
    val tpe: TypeState[Type] = expr.map(_.tpe).getOrElse(toTypeState(NoType))

    def isVoid: Boolean = expr == None
    override def toString: String = s"return ${expr.getOrElse("")}"

  }

  trait Block extends Expr {
    def stmts: List[Tree]
    override def toString: String = 
      s"""|{
          |  ${stmts.map(_.toString).mkString("\n")}
          |}""".stripMargin
  }

  

  trait Assign extends Expr {
    def lhs: Expr
    def rhs: Expr
    def tpe: TypeState[Type] = toTypeState(NoType)
    override def toString: String = s"${lhs} = ${rhs}"
  }

  trait If extends Expr {
    def cond: Expr
    def thenp: Expr
    def elsep: Expr
    def tpe: TypeState[Type] = toTypeState(NoType)


    override def toString: String =
      s"""|if(${cond}) {
          |  ${thenp}
          |} else {
          |  ${elsep}
          |}""".stripMargin
  }


  // TODO: Add Do-While to the toString
  trait While extends Expr {
    def mods: Flags
    def cond: Expr
    def body: Expr
    def tpe: TypeState[Type] = toTypeState(NoType)

    override def toString: String =
      s"""|while(${cond}) {
          |  ${body}
          |}""".stripMargin

  }

  trait For extends Expr {
    def inits: List[Tree]
    def cond: Expr
    def steps: List[Expr]
    def body: Expr
    def tpe: TypeState[Type] = toTypeState(NoType)

    override def toString =
      s"""|for(${inits.mkString(", ")}; ${cond}; ${steps.mkString(", ")}) {
          |  ${body}
          |}""".stripMargin

  }

  // Ternary operator
  trait Ternary extends Expr {
    def cond: Expr
    def thenp: Expr
    def elsep: Expr


    override def toString: String =
      s"(${cond})?${thenp}:${elsep}"
  }

  // Apply
  trait Apply extends Expr {
    def fun: Expr
    def args: List[Expr]

    def tpe: TypeState[Type] = for {
      funty <- fun.tpe
    } yield {
      funty match {
        case MethodType(r, _) => r
        case _                => NoType
      }
    }


    override def toString: String = s"${fun}(${args.mkString(", ")})"
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
      Option[(Flags, TypeUse, Name, List[ValDef], Expr)] = md match {
        case null => None
        case _    => Some((md.mods, md.ret, md.name, md.params, md.body))
      }
  }

  trait ValDefExtractor {
    def unapply(vd: ValDef): Option[(Flags, TypeUse, Name, Tree)] = 
      vd match {
        case null => None
        case _    => Some((vd.mods, vd.tpt, vd.name, vd.rhs))
      }
  }
  trait ReturnExtractor {
    def unapply(r: Return): Option[Expr] = r match {
      case null => None
      case _    => for(e <- r.expr) yield e
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
      val owner: Option[TreeId]) extends Template

    def apply(members: List[DefTree],
      owner: Option[TreeId] = None): Template = 
      new TemplateImpl(members, owner)
  }

  trait AssignFactory {
    private class AssignImpl(val lhs: Expr, 
      val rhs: Expr, val pos: Option[Position],
      val owner: Option[TreeId]) extends Assign

    def apply(lhs: Expr, rhs: Expr, pos: Option[Position] = None,
      owner: Option[TreeId] = None): Assign = 
      new AssignImpl(lhs, rhs, pos, owner)

  }
  trait IfFactory {
    private class IfImpl(val cond: Expr, val thenp: Expr,
      val elsep: Expr, val pos: Option[Position],
      val owner: Option[TreeId]) extends If


    def apply(cond: Expr, thenp: Expr, elsep: Expr,
      pos: Option[Position] = None, owner: Option[TreeId] = None): If = 
      new IfImpl(cond, thenp, elsep, pos, owner)
  }


  trait WhileFactory {
    private class WhileImpl(val mods: Flags, val cond: Expr, 
      val body: Expr, val pos: Option[Position],
      val owner: Option[TreeId]) extends While


    def apply(mods: Flags, cond: Expr, body: Expr,
      pos: Option[Position] = None, owner: Option[TreeId] = None): While = 
      new WhileImpl(mods, cond, body, pos, owner)

  }

  trait BlockFactory {
    private class BlockImpl(val stmts: List[Tree], val tpe: TypeState[Type],
      val pos: Option[Position], val owner: Option[TreeId]) extends Block


    def apply(stmts: List[Tree], tpe: TypeState[Type], 
      pos: Option[Position] = None, owner: Option[TreeId] = None): Block = 
        new BlockImpl(stmts, tpe, pos, owner)

  }

  trait ForFactory {
    private class ForImpl(val inits: List[Tree],
      val cond: Expr, val steps: List[Expr], val body: Expr,
        val pos: Option[Position], val owner: Option[TreeId]) extends For

    def apply(inits: List[Tree], cond: Expr, steps: List[Expr], 
      body: Expr, pos: Option[Position] = None, 
      owner: Option[TreeId] = None): For = 
        new ForImpl(inits, cond, steps, body, pos, owner)
  }

  trait TernaryFactory {
    private class TernaryImpl(val cond: Expr, val thenp: Expr,
      val elsep: Expr, val tpe: TypeState[Type],
      val pos: Option[Position] = None, val owner: Option[TreeId]) extends Ternary


    def apply(cond: Expr, thenp: Expr, elsep: Expr, tpe: TypeState[Type], 
      pos: Option[Position] = None, owner: Option[TreeId]): Ternary = 
      new TernaryImpl(cond, thenp, elsep, tpe, pos, owner)
  }

  trait ApplyFactory {
    private class ApplyImpl(val fun: Expr, val args: List[Expr],
      val pos: Option[Position], val owner: Option[TreeId]) extends Apply


    def apply(fun: Expr, args: List[Expr], pos: Option[Position] = None, 
      owner: Option[TreeId] = None): Apply = 
        new ApplyImpl(fun, args, pos, owner)

  }

  trait ReturnFactory {
    private class ReturnImpl(val expr: Option[Expr], 
    val pos: Option[Position], val owner: Option[TreeId]) extends Return


    def apply(expr: Expr, pos: Option[Position],
      owner: Option[TreeId]): Return = new ReturnImpl(Some(expr), pos, owner)

    def apply(pos: Option[Position], owner: Option[TreeId]): Return = 
      new ReturnImpl(None, pos, owner)
  }


  trait MethodDefFactory {
    private class MethodDefImpl(val mods: Flags, val id: TreeId,
      val ret: TypeUse, val name: Name, val params: List[ValDef], 
      val body: Expr, val pos: Option[Position], 
      val owner: Option[TreeId]) extends MethodDef

    def apply(mods: Flags, id: TreeId, ret: TypeUse, name: Name, 
      params: List[ValDef], body: Expr, pos: Option[Position] = None, 
      owner: Option[TreeId]): MethodDef = 
        new MethodDefImpl(mods, id, ret, name, params, body, pos, owner)
  }

  trait ValDefFactory {
    private class ValDefImpl(val mods: Flags, val id: TreeId, 
      val tpt: TypeUse, val name: Name, val rhs: Expr, 
      val pos: Option[Position], val owner: Option[TreeId]) extends ValDef

    def apply(mods: Flags, id: TreeId, tpt: TypeUse, name: Name, 
      rhs: Expr, pos: Option[Position] = None, 
      owner: Option[TreeId] = None): ValDef = 
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
