package ch.usi.inf.l3.sana.calcj.parser

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.source.SourceFile
import tiny.source.Position
import tiny.util.{CompilationUnits, MonadUtils}
import tiny.contexts.TreeContexts
import tiny.parser
import calcj.Global
import calcj.ast.Trees
import calcj.ast.Constants
import calcj.ast.JavaOps._
import calcj.types.Types
import calcj.antlr._

import org.antlr.v4.runtime.misc.NotNull
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.AbstractParseTreeVisitor


import scalaz.Scalaz._
import scalaz.{Name => _, _}

trait Parsers extends parser.Parsers {

  type G <: Global
  import global._
  
  def parse(source: SourceFile): CompilationUnit = {
    val tree = new CalcjVisitor(source.name).visit(source.content)
    CompilationUnit(tree, EmptyContext, source.name)
  }

  class CalcjVisitor(val source: String) extends CalcjBaseVisitor[Tree] {

    def pos(ctx: ParserRuleContext): Option[Position] = {
      val token = ctx.getStart
      Some(Position(source, token.getLine, token.getCharPositionInLine + 1))
    }

    def createUnaryOrPostfix[T <: ParserRuleContext](isPostfix: Boolean,
      exp: T, trm: String, ctx: ParserRuleContext): Expr = {

      val e1 = visitChildren(exp)
      val op = trm match {
        case "-"     => Neg
        case "+"     => Pos
        case "++"    => Inc
        case "--"    => Dec
        case "~"     => BCompl
        case "!"     => Not
      }
      (e1, op) match {
        case (e: Expr, op: POp) if isPostfix => 
          Postfix(e, op, toTypeState(notype), pos(ctx))
        case (e: Expr, op: UOp) => 
          Unary(op, e, toTypeState(notype), pos(ctx))
        case _                  =>
          // TODO: report an error
          throw new Exception("Expression is expected")
      }
    }
    def createBinary[T <: ParserRuleContext](es: java.util.List[T], 
      trm: String, ctx: ParserRuleContext): Expr = {
      val e1 = visitChildren(es.get(0))
      val op = trm match {
        case "*"     => Mul
        case "/"     => Div
        case "%"     => Mod
        case "+"     => Add
        case "-"     => Sub
        case "<<"    => SHL
        case ">>"    => SHR
        case ">>>"   => USHR
        case "<"     => Lt
        case ">"     => Gt
        case "<="    => Le
        case ">="    => Ge
        case "=="    => Eq
        case "!="    => Neq
        case "&"     => BAnd
        case "^"     => BXor
        case "|"     => BOr
        case "&&"    => And
        case "||"    => Or
      }
      val e2 = visitChildren(es.get(1))
      (e1, e2) match {
        case (x: Expr, y: Expr) => 
          Binary(x, op, y, toTypeState(notype), pos(ctx))
        case _                  =>
          // TODO: report an error
          throw new Exception("Expression is expected")
      }
    }
    
    override def visitUnaryNum(@NotNull ctx: CalcjParser.UnaryNumContext): Tree = { 
      createUnaryOrPostfix(false, ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitUnaryElse(@NotNull ctx: CalcjParser.UnaryElseContext): Tree = {
      createUnaryOrPostfix(false, ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitPostfix(@NotNull ctx: CalcjParser.PostfixContext): Tree = { 
      createUnaryOrPostfix(true, ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitPrimitiveType(
      @NotNull ctx: CalcjParser.PrimitiveTypeContext): Tree = { 
      TypeUse(None, ctx.getText, None, pos(ctx))
    }

    override def visitCast(@NotNull ctx: CalcjParser.CastContext): Tree = { 
      val e = visitChildren(ctx.expression)
      val tpt = visitChildren(ctx.primitiveType)
      (tpt, e) match {
        case (tpt: TypeUse, e: Expr) =>
          Cast(tpt, e, pos(ctx))
        case _               =>
          // TODO: report an error
          throw new Exception("(TypeUse) Expression is expected")
      }
    }
    
    
    // Binary visitors

    override def visitMul(@NotNull ctx: CalcjParser.MulContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx) 
    }
       
    override def visitAdd(@NotNull ctx: CalcjParser.AddContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitShifts(@NotNull ctx: CalcjParser.ShiftsContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitRel(@NotNull ctx: CalcjParser.RelContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitEqu(@NotNull ctx: CalcjParser.EquContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitBAnd(@NotNull ctx: CalcjParser.BAndContext): Tree = {
      createBinary(ctx.expression, "&", ctx) 
    }

    override def visitBXor(@NotNull ctx: CalcjParser.BXorContext): Tree = {
      createBinary(ctx.expression, "^", ctx) 
    }
    
    override def visitBOr(@NotNull ctx: CalcjParser.BOrContext): Tree = {
      createBinary(ctx.expression, "|", ctx) 
    }

    override def visitAnd(@NotNull ctx: CalcjParser.AndContext): Tree =  { 
      createBinary(ctx.expression, "&&", ctx) 
    }

    override def visitOr(@NotNull ctx: CalcjParser.OrContext): Tree =  { 
      createBinary(ctx.expression, "||", ctx) 
    }



    
    // Literal visitors

    override def visitIntLit(@NotNull ctx: CalcjParser.IntLitContext): Tree = { 
      val txt = ctx.getText
      (txt.endsWith("l") || txt.endsWith("L")) match {
        case true  => Lit(LongConstant(ctx.getText.toInt), pos(ctx)) 
        case false => Lit(IntConstant(ctx.getText.toInt), pos(ctx)) 
      }
    }
    
    override def visitFloatLit(@NotNull ctx: CalcjParser.FloatLitContext): Tree = { 
      val txt = ctx.getText
      (txt.endsWith("f") || txt.endsWith("F")) match {
        case true  => Lit(FloatConstant(ctx.getText.toInt), pos(ctx)) 
        case false => Lit(DoubleConstant(ctx.getText.toInt), pos(ctx)) 
      }
    }
   
    override def visitCharLit(@NotNull ctx: CalcjParser.CharLitContext): Tree = { 
      Lit(CharConstant(ctx.getText.head), pos(ctx)) 
    }

    override def visitStrLit(@NotNull ctx: CalcjParser.StrLitContext): Tree = { 
      Lit(StringConstant(ctx.getText), pos(ctx)) 
    }

  
    override def visitBoolLit(@NotNull ctx: CalcjParser.BoolLitContext): Tree = { 
      Lit(BooleanConstant(ctx.getText.toBoolean), pos(ctx)) 
    }
  }
}





