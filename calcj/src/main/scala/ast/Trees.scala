package ch.usi.inf.l3.sana.calcj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.ast
import tiny.ast.Flags
import tiny.source.Position
import tiny.contexts.TreeContexts
import tiny.names.Names._
import tiny.types
import calcj.ast.JavaOps._


import scalaz.Scalaz._
import scalaz.{Name => _, _}

trait Trees extends ast.Trees {
  self: types.Types with TreeContexts with Constants =>
  
  
  /********************* AST Nodes *********************************/

  // Conversions and Casts
  trait Cast extends Expr {
    def tpt: Expr // TODO: What should be here?
    def expr: Expr
    def owner: Option[TreeId] = None
    override def tpe: TypeState[Type] = tpt.tpe
  }

  // Binary and Unary trees
  trait Binary extends Expr {
    def lhs: Expr
    def op: BOp
    def rhs: Expr

    def owner: Option[TreeId] = None
    override def toString: String = 
      s"(${lhs.toString} ${op.toString} ${rhs.toString})"
  
  }

  trait Postfix extends Expr {
    def op: POp
    def expr: Expr

    def owner: Option[TreeId] = None
    override def toString: String = s"(${op.toString}${expr.toString})"
  }

  trait Unary extends Expr {
    def op: UOp
    def expr: Expr

    def owner: Option[TreeId] = None
    override def toString: String = s"(${expr.toString}${op.toString})"
  }

  // Literals
  trait Lit extends Expr {
    def const: Constant
    def owner: Option[TreeId] = None
    override def tpe: TypeState[Type] = point(const.tpe)
  }

  /***************************** Extractors **************************/

  trait CastExtractor {
    def unapply(c: Cast): Option[(Expr, Expr)] = c match {
      case null => None
      case _    => Some((c.tpt, c.expr))
    }
  }

  trait LitExtractor {
    def unapply(b: Lit): Option[Constant] = b match {
      case null => None
      case _    => Some((b.const))
    }
  }

  trait BinaryExtractor {
    def unapply(b: Binary): Option[(Expr, BOp, Expr)] = b match {
      case null => None
      case _    => Some((b.lhs, b.op, b.rhs))
    }
  }

  trait UnaryExtractor {
    def unapply(u: Unary): Option[(UOp, Expr)] = u match {
      case null => None
      case _    => Some((u.op, u.expr))
    }
  }

  trait PostfixExtractor {
    def unapply(u: Postfix): Option[(Expr, POp)] = u match {
      case null => None
      case _    => Some((u.expr, u.op))
    }
  }

  /***************************** Factories **************************/

  trait CastFactory {
    private class CastImpl(val tpt: Expr, val expr: Expr,
      val pos: Option[Position]) extends Cast

    def apply(tpt: Expr, expr: Expr, pos: Option[Position]): Cast = 
      new CastImpl(tpt, expr, pos)
  }

  trait LitFactory {
    private class LitImpl(val const: Constant, 
      val pos: Option[Position]) extends Lit

    def apply(const: Constant, pos: Option[Position]): Lit = 
      new LitImpl(const, pos)
  }


  trait BinaryFactory {
    private class BinaryImpl(val lhs: Expr,
      val op: BOp, val rhs: Expr, val tpe: TypeState[Type],
      val pos: Option[Position]) extends Binary

    def apply(lhs: Expr, op: BOp, rhs: Expr, tpe: TypeState[Type], 
      pos: Option[Position]): Binary = new BinaryImpl(lhs, op, rhs, tpe, pos)
  }

  trait UnaryFactory {
    private class UnaryImpl(val op: UOp,
      val expr: Expr, val tpe: TypeState[Type],
      val pos: Option[Position]) extends Unary

    def apply(op: UOp, expr: Expr, tpe: TypeState[Type], 
      pos: Option[Position]): Unary =
      new UnaryImpl(op, expr, tpe, pos)
  }

  trait PostfixFactory {
    private class PostfixImpl(val expr: Expr, val op: POp,
      val tpe: TypeState[Type], 
      val pos: Option[Position]) extends Postfix

    def apply(expr: Expr, op: POp, tpe: TypeState[Type], 
      pos: Option[Position]): Postfix =
      new PostfixImpl(expr, op, tpe, pos)

  }

  /******************* Factory and Extractor instances ***************/


  // TODO: Only let Extractors out, or none?

  val Cast      = new CastExtractor with CastFactory {}
  val Lit       = new LitExtractor with LitFactory {}
  val Binary    = new BinaryExtractor with BinaryFactory {}
  val Unary     = new UnaryExtractor with UnaryFactory {}
  val Postfix   = new PostfixExtractor with PostfixFactory {}
}
