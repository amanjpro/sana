package ch.usi.inf.l3.sana.primj.ast


import ch.usi.inf.l3.sana
import sana.tiny.ast
import sana.tiny.ast.Flags
import sana.tiny.source.Position
import sana.tiny.names.Names._
import sana.tiny.types
import sana.tiny.symbols
import sana.calcj.ast.JavaOps._
import sana.calcj.ast.Constants



/**
   TODO:
   Switch - Case
   Break
   Continue
 */

trait Trees extends ast.Trees {
  self: types.Types with symbols.Symbols with Constants =>

  /********************* AST Nodes *********************************/
  // Variable and Method definitions

  trait MethodDef extends DefTree {
    def ret: TypeUse
    def params: List[ValDef]
    def body: Block

    override def toString: String = 
      s"""|${flags} ${ret} ${name} (${params.mkString(", ")}) {
          |  ${body}
          |}""".stripMargin

  }
  
  trait ValDef extends DefTree {
    def tpt: TypeUse
    def rhs: Tree
    override def toString: String = s"${flags} ${tpt} ${name} = ${rhs}"
  }
  
  // Statements and Blocks

  // FIXME: What is a block?
  trait Block extends Statement

  trait Assign extends Statement {
    def lhs: Expr
    def rhs: Expr
    def tpe: Option[Type] = None
    def symbol: Option[Symbol] = None
    override def toString: String = s"${lhs} = ${rhs}"
  }

  trait If extends Statement {
    def cond: Expr
    def thenp: Statement
    def elsep: Statement
    def tpe: Option[Type] = None
    def symbol: Option[Symbol] = None


    override def toString: String =
      s"""|if(${cond}) {
          |  ${thenp}
          |} else {
          |  ${elsep}
          |}""".stripMargin
  }


  // TODO: Add Do-While to the toString
  trait While extends Statement {
    def flags: Flags
    def cond: Expr
    def body: Statement
    def tpe: Option[Type] = None
    def symbol: Option[Symbol] = None

    override def toString: String =
      s"""|while(${cond}) {
          |  ${body}
          |}""".stripMargin

  }

  trait For extends Statement {
    def init: Statement
    def cond: Expr
    def step: Expr
    def body: Statement
    def tpe: Option[Type] = None
    def symbol: Option[Symbol] = None

    override def toString =
      s"""|for(${init}; ${cond}; ${step}) {
          |  ${body}
          |}""".stripMargin

  }

  // Ternary operator
  trait Ternary extends Expr {
    def cond: Expr
    def thenp: Expr
    def elsep: Expr


    def symbol: Option[Symbol] = None
    override def toString: String =
      s"(${cond})?${thenp}:${elsep}"
  }

  // Apply
  trait Apply extends Expr {
    def fun: Expr
    def args: List[Expr]


    def symbol: Option[Symbol] = fun.symbol

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
    def unapply(i: If): Option[(Expr, Statement, Statement)] = i match {
      case null => None
      case _    => Some((i.cond, i.thenp, i.elsep))
    }
  }


  trait WhileExtractor {
    def unapply(w: While): Option[(Flags, Expr, Statement)] = w match {
      case null => None
      case _    => Some((w.flags, w.cond, w.body))
    }
  }

  trait ForExtractor {
    def unapply(f: For): Option[(Statement, Expr, Expr, Statement)] = f match {
      case null => None
      case _    => Some((f.init, f.cond, f.step, f.body))
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
      Option[(Flags, TypeUse, Name, List[ValDef], Block)] = md match {
        case null => None
        case _    => Some((md.flags, md.ret, md.name, md.params, md.body))
      }
  }

  trait ValDefExtractor {
    def unapply(vd: ValDef): Option[(Flags, TypeUse, Name, Tree)] = 
      vd match {
        case null => None
        case _    => Some((vd.flags, vd.tpt, vd.name, vd.rhs))
      }
  }

  

  /***************************** Factories **************************/

  trait AssignFactory {
    private class AssignImpl(val lhs: Expr, 
      val rhs: Expr, val pos: Option[Position]) extends Assign

    def apply(l: Expr, r: Expr, p: Option[Position] = None): Assign = 
      new AssignImpl(l, r, p)

  }
  trait IfFactory {
    private class IfImpl(val cond: Expr, val thenp: Statement,
      val elsep: Statement, val pos: Option[Position]) extends If


    def apply(c: Expr, t: Statement, e: Statement,
      p: Option[Position] = None): If = new IfImpl(c, t, e, p)
  }


  trait WhileFactory {
    private class WhileImpl(val flags: Flags, val cond: Expr, 
      val body: Statement, val pos: Option[Position]) extends While


    def apply(f: Flags, c: Expr, b: Statement,
      p: Option[Position] = None): While = new WhileImpl(f, c, b, p)

  }

  trait ForFactory {
    private class ForImpl(val init: Statement,
      val cond: Expr, val step: Expr, val body: Statement,
        val pos: Option[Position]) extends For

    def apply(i: Statement, c: Expr, s: Expr, 
      b: Statement, p: Option[Position] = None): For = 
        new ForImpl(i, c, s, b, p)
  }

  trait TernaryFactory {
    private class TernaryImpl(val cond: Expr, val thenp: Expr,
      val elsep: Expr, val tpe: Option[Type],
      val pos: Option[Position]) extends Ternary


    def apply(c: Expr, t: Expr, e: Expr, tp: Option[Type], 
      p: Option[Position]): Ternary = new TernaryImpl(c, t, e, tp, p)
  }

  trait ApplyFactory {
    private class ApplyImpl(val fun: Expr, val args: List[Expr],
      val tpe: Option[Type], val pos: Option[Position]) extends Apply


    def apply(f: Expr, as: List[Expr], t: Option[Type], 
      p: Option[Position]): Apply = new ApplyImpl(f, as, t, p)

  }

  trait MethodDefFactory {
    private class MethodDefImpl(val flags: Flags, val ret: TypeUse, 
      val name: Name, val params: List[ValDef], val body: Block, 
      val symbol: Option[Symbol], val pos: Option[Position]) extends MethodDef

    def apply(f: Flags, r: TypeUse, n: Name, ps: List[ValDef], 
      b: Block, s: Option[Symbol], p: Option[Position]): MethodDef = 
        new MethodDefImpl(f, r, n, ps, b, s, p)
  }

  trait ValDefFactory {
    private class ValDefImpl(val flags: Flags, val tpt: TypeUse, 
      val name: Name, val rhs: Tree, val symbol: Option[Symbol],
      val pos: Option[Position]) extends ValDef

    def apply(f: Flags, r: TypeUse, n: Name, 
      b: Tree, s: Option[Symbol], p: Option[Position]): ValDef = 
        new ValDefImpl(f, r, n, b, s, p)
  }


  /******************* Factory and Extractor instances ***************/


  // TODO: Only let Extractors out, or none?

  val Assign    = new AssignExtractor with AssignFactory {}
  val If        = new IfExtractor with IfFactory {}
  val While     = new WhileExtractor with WhileFactory {}
  val For       = new ForExtractor with ForFactory {}
  val Ternary   = new TernaryExtractor with TernaryFactory {}
  val Apply     = new ApplyExtractor with ApplyFactory {}
  val MethodDef = new MethodDefExtractor with MethodDefFactory {}
  val ValDef    = new ValDefExtractor with ValDefFactory {}
}
