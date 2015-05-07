package ch.usi.inf.l3.sana.primj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import tiny.ast.Flags
import tiny.source.Position
import tiny.contexts.TreeContexts
import tiny.names.Names._
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
  self: types.Types with Constants with TreeContexts =>

  /********************* AST Nodes *********************************/
  // Variable and Method definitions

  trait MethodDef extends DefTree {
    def ret: TypeUse
    def params: List[ValDef]
    def body: Expr
    def tpe: TypeState[Type] = {
      val tys = params.map(_.tpe).sequenceU
      for {
        r   <- ret.tpe
        tps <- tys
        ty  <- point(MethodType(r, tps))
      } yield ty
    }


    override def toString: String = 
      s"""|${mods} ${ret} ${name} (${params.mkString(", ")}) {
          |  ${body}
          |}""".stripMargin

  }
  
  trait ValDef extends DefTree {
    def tpt: TypeUse
    def rhs: Tree
    def tpe: TypeState[Type] = tpt.tpe
    override def toString: String = s"${mods} ${tpt} ${name} = ${rhs}"
  }
  


  trait Return extends Expr {
    val expr: Option[Expr]
    val tpe: TypeState[Type] = expr.map(_.tpe).getOrElse(point(NoType))

    def isVoid: Boolean = expr == None
    override def toString: String = s"return ${expr.getOrElse("")}"

  }

  trait Block extends Expr {
    def stmts: List[Expr]
    override def toString: String = 
      s"""|{
          |  ${stmts.map(_.toString).mkString("\n")}
          |}""".stripMargin
  }

  

  trait Assign extends Expr {
    def lhs: Expr
    def rhs: Expr
    def tpe: TypeState[Type] = point(NoType)
    override def toString: String = s"${lhs} = ${rhs}"
  }

  trait If extends Expr {
    def cond: Expr
    def thenp: Expr
    def elsep: Expr
    def tpe: TypeState[Type] = point(NoType)


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
    def tpe: TypeState[Type] = point(NoType)

    override def toString: String =
      s"""|while(${cond}) {
          |  ${body}
          |}""".stripMargin

  }

  trait For extends Expr {
    def inits: List[Expr]
    def cond: Expr
    def steps: List[Expr]
    def body: Expr
    def tpe: TypeState[Type] = point(NoType)

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



    override def toString: String = s"${fun}(${args.mkString(", ")})"
  }





  /***************************** Extractors **************************/
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
      Option[(List[Expr], Expr, List[Expr], Expr)] = f match {
      case null => None
      case _    => Some((f.inits, f.cond, f.steps, f.body))
    }
  }
  trait BlockExtractor {
    def unapply(b: Block): Option[List[Expr]] = b match {
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

  trait AssignFactory {
    private class AssignImpl(val lhs: Expr, 
      val rhs: Expr, val pos: Option[Position],
      val owner: Option[TreeId]) extends Assign

    def apply(l: Expr, r: Expr, p: Option[Position] = None,
      o: Option[TreeId] = None): Assign = 
      new AssignImpl(l, r, p, o)

  }
  trait IfFactory {
    private class IfImpl(val cond: Expr, val thenp: Expr,
      val elsep: Expr, val pos: Option[Position],
      val owner: Option[TreeId]) extends If


    def apply(c: Expr, t: Expr, e: Expr,
      p: Option[Position] = None, o: Option[TreeId] = None): If = 
      new IfImpl(c, t, e, p, o)
  }


  trait WhileFactory {
    private class WhileImpl(val mods: Flags, val cond: Expr, 
      val body: Expr, val pos: Option[Position],
      val owner: Option[TreeId]) extends While


    def apply(f: Flags, c: Expr, b: Expr,
      p: Option[Position] = None, o: Option[TreeId] = None): While = 
      new WhileImpl(f, c, b, p, o)

  }

  trait BlockFactory {
    private class BlockImpl(val stmts: List[Expr], val tpe: TypeState[Type],
      val pos: Option[Position], val owner: Option[TreeId]) extends Block


    def apply(ss: List[Expr], t: TypeState[Type], 
      p: Option[Position] = None, o: Option[TreeId] = None): Block = 
        new BlockImpl(ss, t, p, o)

  }

  trait ForFactory {
    private class ForImpl(val inits: List[Expr],
      val cond: Expr, val steps: List[Expr], val body: Expr,
        val pos: Option[Position], val owner: Option[TreeId]) extends For

    def apply(is: List[Expr], c: Expr, ss: List[Expr], 
      b: Expr, p: Option[Position] = None, o: Option[TreeId] = None): For = 
        new ForImpl(is, c, ss, b, p, o)
  }

  trait TernaryFactory {
    private class TernaryImpl(val cond: Expr, val thenp: Expr,
      val elsep: Expr, val tpe: TypeState[Type],
      val pos: Option[Position] = None, val owner: Option[TreeId]) extends Ternary


    def apply(c: Expr, t: Expr, e: Expr, tp: TypeState[Type], 
      p: Option[Position] = None, o: Option[TreeId]): Ternary = 
      new TernaryImpl(c, t, e, tp, p, o)
  }

  trait ApplyFactory {
    private class ApplyImpl(val fun: Expr, val args: List[Expr],
      val tpe: TypeState[Type], val pos: Option[Position],
      val owner: Option[TreeId]) extends Apply


    def apply(f: Expr, as: List[Expr], t: TypeState[Type], 
      p: Option[Position] = None, o: Option[TreeId] = None): Apply = 
        new ApplyImpl(f, as, t, p, o)

  }

  trait MethodDefFactory {
    private class MethodDefImpl(val mods: Flags, val id: TreeId,
      val ret: TypeUse, val name: Name, val params: List[ValDef], 
      val body: Expr, val pos: Option[Position], 
      val owner: Option[TreeId]) extends MethodDef

    def apply(f: Flags, i: TreeId, r: TypeUse, n: Name, ps: List[ValDef], 
      b: Expr, p: Option[Position] = None, o: Option[TreeId]): MethodDef = 
        new MethodDefImpl(f, i, r, n, ps, b, p, o)
  }

  trait ReturnFactory {
    private class ReturnImpl(val expr: Option[Expr], 
    val pos: Option[Position], val owner: Option[TreeId]) extends Return


    def apply(e: Expr, p: Option[Position],
      o: Option[TreeId]): Return = new ReturnImpl(Some(e), p, o)

    def apply(p: Option[Position], o: Option[TreeId]): Return = 
      new ReturnImpl(None, p, o)
  }

  trait ValDefFactory {
    private class ValDefImpl(val mods: Flags, val id: TreeId, 
      val tpt: TypeUse, val name: Name, val rhs: Tree, 
      val pos: Option[Position], val owner: Option[TreeId]) extends ValDef

    def apply(f: Flags, i: TreeId, r: TypeUse, n: Name, 
      b: Tree, p: Option[Position] = None, o: Option[TreeId] = None): ValDef = 
        new ValDefImpl(f, i, r, n, b, p, o)
  }


  /******************* Factory and Extractor instances ***************/


  // TODO: Only let Extractors out, or none?

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
