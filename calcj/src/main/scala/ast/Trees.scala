package ch.usi.inf.l3.sana.calcj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.ast
import tiny.modifiers.Flags
import tiny.source.Position
import tiny.contexts._
import tiny.names._
import tiny.types
import tiny.util.MonadUtils
import calcj.ast.JavaOps._


import scalaz.Scalaz._
import scalaz.{Name => _, _}

trait Trees extends ast.Trees {
  self: types.Types with TreeContexts with Constants with MonadUtils =>
  
  
  /********************* AST Nodes *********************************/

  // Conversions and Casts
  trait Cast extends Expr {
    def tpt: TypeUse
    def expr: Expr
    val owner: TreeId = expr.owner
    override def tpe: TypeState[Type] = tpt.tpe

    def show(ctx: Context): String = 
      s"""|Cast{
          |tpt=${tpt.show(ctx)},
          |expr=${expr.show(ctx)},
          |pos=$pos
          |}""".stripMargin
  }

  // Binary and Unary trees
  trait Binary extends Expr {
    def lhs: Expr
    def op: BOp
    def rhs: Expr

    val owner: TreeId = lhs.owner


    def show(ctx: Context): String = 
      s"""|Binary{
          |lhs=${lhs.show(ctx)},
          |op=${op.toString},
          |rhs=${rhs.show(ctx)},
          |tpe=${tpe.eval(ctx)},
          |pos=${pos}
          |}""".stripMargin

  
  }

  trait Postfix extends Expr {
    def op: POp
    def expr: Expr

    val owner: TreeId = expr.owner

    def show(ctx: Context): String = 
      s"""|Postfix{
          |op=${op.toString},
          |expr=${expr.show(ctx)},
          |tpe=${tpe.eval(ctx)},
          |pos=${pos}
          |}""".stripMargin


  }

  trait Unary extends Expr {
    def op: UOp
    def expr: Expr

    val owner: TreeId = expr.owner

    def show(ctx: Context): String = 
      s"""|Unary{
          |expr=${expr.show(ctx)},
          |op=${op.toString},
          |tpe=${tpe.eval(ctx)},
          |pos=${pos}
          |}""".stripMargin

  }

  // Literals
  trait Lit extends Expr {
    def const: Constant
    val owner: TreeId = NoId
    override def tpe: TypeState[Type] = toTypeState(const.tpe)

    def show(ctx: Context): String = 
      s"Lit($const)"
  }

  /***************************** Extractors **************************/

  trait CastExtractor {
    def unapply(c: Cast): Option[(TypeUse, Expr)] = c match {
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
    private class CastImpl(val tpt: TypeUse, val expr: Expr,
      val pos: Option[Position]) extends Cast

    def apply(tpt: TypeUse, expr: Expr, pos: Option[Position]): Cast = 
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
