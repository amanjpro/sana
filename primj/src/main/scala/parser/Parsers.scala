package ch.usi.inf.l3.sana.primj.parser

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import tiny.contexts.TreeContexts
import tiny.source.SourceFile
import tiny.source.Position
import tiny.contexts._
import tiny.names.Name
import tiny.util.{CompilationUnits, MonadUtils}
import tiny.parser
import calcj.ast.Constants
import calcj.ast.JavaOps._
import primj.Global
import primj.antlr._
import primj.modifiers._



import org.antlr.v4.runtime.misc.NotNull
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.AbstractParseTreeVisitor


import scalaz.Scalaz._
import scalaz.{Name => _, _}

import scala.collection.JavaConverters._

trait Parsers extends parser.Parsers {

  type G = Global
  import global._
 
  def parse(source: SourceFile): CompilationUnit = {
    val tree = new PrimjVisitor(source.name).visit(source.content)
    CompilationUnit(NO_COMPILATION_UNIT_ID, tree, EmptyContext, source.name)
  }

  class PrimjVisitor(val source: String) extends PrimjBaseVisitor[Tree] {
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
    
    def createVarDecls(ctx: 
      PrimjParser.VariableDeclarationContext,
      mods: Flags): List[ValDef] = {
      val tpe    = visitChildren(ctx.`type`)
      val names  = ctx.Identifier.asScala.toList.map(_.getText)
      tpe match {
        case tu: TypeUse =>
          names.map {
            case name =>
              ValDef(mods, NoId, tu, Name(name), Empty, pos(ctx), NoId)
          }
        case _           =>
          // TODO: report an error
          throw new Exception("Expression is expected")
      }
    }

    def createVarDefs(ctx: 
      PrimjParser.VariableDefinitionContext,
      mods: Flags): List[ValDef] = {
      val tpe    = visitChildren(ctx.`type`)
      val names  = ctx.Identifier.asScala.toList.map(_.getText)
      val exprs  = ctx.expression.asScala.toList.map {
        case es => visitChildren(es).asInstanceOf[Expr]
      }
      tpe match {
        case tu: TypeUse =>
          for {
            name  <- names
            expr  <- exprs
          } yield ValDef(mods, NoId, tu, Name(name), expr, pos(ctx), NoId)
        case _           =>
          // TODO: report an error
          throw new Exception("Expression is expected")
      }
    }
    override def visitProgram(ctx: PrimjParser.ProgramContext): Tree = { 
      val defs = ctx.defDecleration.asScala.toList.flatMap { (kid) => 
        if(kid.methodDeclaration != null)
          List(visitChildren(kid.methodDeclaration).asInstanceOf[DefTree])
        else if(kid.variableDeclaration != null)
          createVarDecls(kid.variableDeclaration, Flags(FlagSet.FIELD))
        else
          createVarDefs(kid.variableDefinition, Flags(FlagSet.FIELD))
      }
      Template(defs, NoId)
    }

		override def visitMethodDeclaration(ctx: 
      PrimjParser.MethodDeclarationContext): Tree = {
      val tpe    = visitChildren(ctx.`type`)
      val name   = ctx.Identifier.getText
      val params = ctx.formalParameters match {
        case null         => List()
        case ps           =>
        ps.formalParameterList.formalParameter.asScala.toList.map {
            case kid => visitChildren(kid).asInstanceOf[ValDef]
          }
      }
      val body   = visitChildren(ctx.methodBody)
      (tpe, body) match {
        case (tu: TypeUse, b: Block) =>
          MethodDef(NoFlags, NoId, tu, Name(name), params, b,
            pos(ctx), NoId)
        case _                       =>
          // TODO: report an error
          throw new Exception("Bad tree shape")
      }
    }
		
		override def visitVoidType(ctx: PrimjParser.VoidTypeContext): Tree = { 
      TypeUse(NoId, Some(ctx.getText), NoId, pos(ctx))
    }
		override def visitBlock(ctx: PrimjParser.BlockContext): Tree = { 
      val stmts   = ctx.blockStatement.asScala.toList.map {
        case kid => visitChildren(kid)
      }
      Block(stmts, toTypeState(notype), pos(ctx), NoId)
    }
		override def visitIf(ctx: PrimjParser.IfContext): Tree = { 
      val cond  = visitChildren(ctx.parExpression)
      val thenp = visitChildren(ctx.statement.get(0))
      val elsep = ctx.statement.size match {
        case 2 => visitChildren(ctx.statement.get(1))
        case 1 => Empty
      }
      (cond, thenp, elsep) match {
        case (c: Expr, t: Expr, e: Expr) =>
          If(c, t, e, pos(ctx), NoId)
        case _                           =>
          // TODO: report an error
          throw new Exception("Bad tree shape")
      }
    }
		override def visitFor(ctx: PrimjParser.ForContext): Tree = { 
      val inits = ctx.forControl.forInit match {
        case null  => Nil
        case inits => 
          if(inits.expressionList != null)
            inits.expressionList.expression.asScala.toList.map {
              case kid => visitChildren(kid).asInstanceOf[Expr]
            }
          else 
            createVarDefs(inits.variableDefinition, 
              Flags(FlagSet.LOCAL_VARIABLE))
      }
      val cond  = ctx.forControl.expression match {
        case null => Empty
        case e    => visitChildren(e)
      }
      val steps = ctx.forControl.forUpdate match {
        case null  => Nil
        case steps =>
          steps.expressionList.expression.asScala.toList.map {
            case kid => visitChildren(kid).asInstanceOf[Expr]
          }
      }
      val body  = visitChildren(ctx.statement)
      (cond, body) match {
        case (c: Expr, b: Expr) =>
          For(inits, c, steps, b, pos(ctx), NoId)
        case _                  =>
          // TODO: report an error
          throw new Exception("Bad tree shape")
      }
    }
		override def visitWhile(ctx: PrimjParser.WhileContext): Tree = {
      val cond = visitChildren(ctx.parExpression)
      val body = visitChildren(ctx.statement)
      (cond, body) match {
        case (c: Expr, b: Expr) =>
          While(NoFlags, c, b, pos(ctx), NoId)
        case _                  =>
          // TODO: report an error
          throw new Exception("Bad tree shape")
      }
    }
		override def visitDoWhile(ctx: PrimjParser.DoWhileContext): Tree = {
      val cond = visitChildren(ctx.parExpression)
      val body = visitChildren(ctx.statement)
      (cond, body) match {
        case (c: Expr, b: Expr) =>
          While(Flags(FlagSet.DO_WHILE), c, b, pos(ctx), NoId)
        case _                  =>
          // TODO: report an error
          throw new Exception("Bad tree shape")
      }
    }

		override def visitReturn(ctx: PrimjParser.ReturnContext): Tree = {
      ctx.expression match {
        case null                => 
          Return(pos(ctx), NoId)
        case expr                =>
          val e = visitChildren(expr)
          e match {
            case e: Expr         =>
              Return(e, pos(ctx), NoId)
          case _                 =>
            // TODO: report an error
            throw new Exception("Bad tree shape")
          }
      }
    }

		override def visitAssign(ctx: PrimjParser.AssignContext): Tree = {
      val name   = ctx.Identifier.getText
      val id     = Ident(NoId, Some(name), NoId, pos(ctx))
      val rhs    = visitChildren(ctx.expression)
      rhs match {
        case e: Expr          =>
          Assign(id, e, pos(ctx), NoId)
        case _                 =>
          // TODO: report an error
          throw new Exception("Bad tree shape")
      }
    }
    override def visitEmpty(ctx: PrimjParser.EmptyContext): Tree = {
      Empty
    }
		override def visitTernary(ctx: PrimjParser.TernaryContext): Tree = {
      val cond  = visitChildren(ctx.parExpression)
      val thenp = visitChildren(ctx.expression.get(0))
      val elsep = visitChildren(ctx.expression.get(1))
      (cond, thenp, elsep) match {
        case (c: Expr, t: Expr, e: Expr) =>
          Ternary(c, t, e, toTypeState(notype), pos(ctx), NoId)
        case _                           =>
          // TODO: report an error
          throw new Exception("Bad tree shape")
      }
    }
		override def visitApply(ctx: PrimjParser.ApplyContext): Tree = {
      val name   = ctx.Identifier.getText
      val id     = Ident(NoId, Some(name), NoId, pos(ctx))
      val args   = ctx.arguments.expressionList.expression match {
        case null           => Nil
        case es             => es.asScala.toList.map {
          case kid => visitChildren(kid).asInstanceOf[Expr]
        }
      }
      Apply(id, args, pos(ctx), NoId)
    }


    override def visitUnaryNum(@NotNull ctx: PrimjParser.UnaryNumContext): Tree = { 
      createUnaryOrPostfix(false, ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitUnaryElse(@NotNull ctx: PrimjParser.UnaryElseContext): Tree = {
      createUnaryOrPostfix(false, ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitPostfix(@NotNull ctx: PrimjParser.PostfixContext): Tree = { 
      createUnaryOrPostfix(true, ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitPrimitiveType(
      @NotNull ctx: PrimjParser.PrimitiveTypeContext): Tree = { 
      TypeUse(NoId, Some(ctx.getText), NoId, pos(ctx))
    }

    override def visitCast(@NotNull ctx: PrimjParser.CastContext): Tree = { 
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

    override def visitMul(@NotNull ctx: PrimjParser.MulContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx) 
    }
       
    override def visitAdd(@NotNull ctx: PrimjParser.AddContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitShifts(@NotNull ctx: PrimjParser.ShiftsContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitRel(@NotNull ctx: PrimjParser.RelContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitEqu(@NotNull ctx: PrimjParser.EquContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx) 
    }

    override def visitBAnd(@NotNull ctx: PrimjParser.BAndContext): Tree = {
      createBinary(ctx.expression, "&", ctx) 
    }

    override def visitBXor(@NotNull ctx: PrimjParser.BXorContext): Tree = {
      createBinary(ctx.expression, "^", ctx) 
    }
    
    override def visitBOr(@NotNull ctx: PrimjParser.BOrContext): Tree = {
      createBinary(ctx.expression, "|", ctx) 
    }

    override def visitAnd(@NotNull ctx: PrimjParser.AndContext): Tree =  { 
      createBinary(ctx.expression, "&&", ctx) 
    }

    override def visitOr(@NotNull ctx: PrimjParser.OrContext): Tree =  { 
      createBinary(ctx.expression, "||", ctx) 
    }



    
    // Literal visitors

    override def visitIntLit(@NotNull ctx: PrimjParser.IntLitContext): Tree = { 
      val txt = ctx.getText
      (txt.endsWith("l") || txt.endsWith("L")) match {
        case true  => Lit(LongConstant(ctx.getText.toInt), pos(ctx)) 
        case false => Lit(IntConstant(ctx.getText.toInt), pos(ctx)) 
      }
    }
    
    override def visitFloatLit(@NotNull ctx: PrimjParser.FloatLitContext): Tree = { 
      val txt = ctx.getText
      (txt.endsWith("f") || txt.endsWith("F")) match {
        case true  => Lit(FloatConstant(ctx.getText.toInt), pos(ctx)) 
        case false => Lit(DoubleConstant(ctx.getText.toInt), pos(ctx)) 
      }
    }
   
    override def visitCharLit(@NotNull ctx: PrimjParser.CharLitContext): Tree = { 
      Lit(CharConstant(ctx.getText.head), pos(ctx)) 
    }

    override def visitStrLit(@NotNull ctx: PrimjParser.StrLitContext): Tree = { 
      Lit(StringConstant(ctx.getText), pos(ctx)) 
    }

  
    override def visitBoolLit(@NotNull ctx: PrimjParser.BoolLitContext): Tree = { 
      Lit(BooleanConstant(ctx.getText.toBoolean), pos(ctx)) 
    }
  }
}
