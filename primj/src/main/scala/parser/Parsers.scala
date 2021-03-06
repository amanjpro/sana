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
import tiny.modifiers.Flags
import tiny.util.{CompilationUnits, MonadUtils}
import tiny.parser
import tiny.debug.logger
import calcj.ast.Constants
import calcj.ast.JavaOps._
import primj.Global
import primj.antlr._
import primj.modifiers._
import primj.modifiers.Ops._
import calcj.modifiers.{Ops => _, _}



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
    logger.debug(tree.show(emptyContext))
    CompilationUnit(NO_COMPILATION_UNIT_ID, tree, source.name)
  }

  class PrimjVisitor(val source: String) extends PrimjBaseVisitor[Tree] {
    def pos(ctx: ParserRuleContext): Option[Position] = {
      val token = ctx.getStart
      Some(Position(source, token.getLine, token.getCharPositionInLine + 1))
    }

    def createUnaryOrPostfix[T <: ParserRuleContext](isPostfix: Boolean,
      exp: T, trm: String, ctx: ParserRuleContext): Expr = {

      val e1 = visit(exp)
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
          Unary(Flags(POSTFIX), op, e, toTypeState(notype), pos(ctx))
        case (e: Expr, op: UOp) =>
          Unary(noflags, op, e, toTypeState(notype), pos(ctx))
        case _                  =>
          // TODO: report an error
          throw new Exception("Expression is expected, but got " + e1 + " " + op)
      }
    }

    def createBinary[T <: ParserRuleContext](es: java.util.List[T],
      trm: String, ctx: ParserRuleContext): Expr = {
      val e1 = visit(es.get(0))
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
      val e2 = visit(es.get(1))
      (e1, e2) match {
        case (x: Expr, y: Expr) =>
          Binary(x, op, y, toTypeState(notype), pos(ctx))
        case _                  =>
          // TODO: report an error
          throw new Exception("Expression is expected but got: " + e1 + " " + e2)
      }
    }

    override def visitAssign(@NotNull ctx: PrimjParser.AssignContext): Tree = {
      val name   = ctx.Identifier.getText
      val id     = Ident(NoId, Some(name), pos(ctx), NoId)
      val e2     = visit(ctx.expression).asInstanceOf[Expr]
      val op: Option[BOp] = ctx.op.getText match {
        case "+="   => Some(Add)
        case "-="   => Some(Sub)
        case "*="   => Some(Mul)
        case "/="   => Some(Div)
        case "%="   => Some(Mod)
        case "&="   => Some(BAnd)
        case "|="   => Some(BOr)
        case "^="   => Some(Xor)
        case "<<="  => Some(SHL)
        case ">>="  => Some(SHR)
        case ">>>=" => Some(USHR)
        case "="    => None
      }
      op match {
        case None     =>
          Assign(id, e2, pos(ctx), NoId)
        case Some(op) =>
          val rhs = Binary(id, op, e2, toTypeState(notype), pos(ctx))
          Assign(id, rhs, pos(ctx), NoId)
      }
    }



    def createVarDecls(@NotNull ctx:
      PrimjParser.VariableDeclarationContext,
      mods: Flags): List[ValDef] = {
      val mods1   = if(ctx.mods != null)
                      mods | FINAL
                    else mods
      val tpe    = visit(ctx.`type`)
      val names  = ctx.Identifier.asScala.toList.map(_.getText)
      val exprs  = ctx.varRHS.asScala.toList.map {
        case null => Empty
        case e    => visit(e).asInstanceOf[Expr]
      }
      tpe match {
        case tu: TypeUse =>
          names.zip(exprs).map {
            case (name, expr) =>
              ValDef(mods1, NoId, tu, Name(name), expr, pos(ctx), NoId)
          }
        case _           =>
          // TODO: report an error
          throw new Exception("TypeUse is expected")
      }
    }

    def createVarDefs(@NotNull ctx:
      PrimjParser.VariableDefinitionContext,
      mods: Flags): List[ValDef] = {

      val mods1   = if(ctx.mods != null)
                      mods | FINAL
                    else
                      mods
      val tpe    = visit(ctx.`type`)
      val names  = ctx.Identifier.asScala.toList.map(_.getText)
      val exprs  = ctx.expression.asScala.toList.map {
        case es => visit(es).asInstanceOf[Expr]
      }
      tpe match {
        case tu: TypeUse =>
          names.zip(exprs).map {
            case (name, expr) =>
              ValDef(mods1, NoId, tu, Name(name), expr, pos(ctx), NoId)
          }
        case _           =>
          // TODO: report an error
          throw new Exception("Expression is expected")
      }
    }

    override def visitProgram(@NotNull ctx: PrimjParser.ProgramContext): Tree = {
      val defs = ctx.defDecleration.asScala.toList.flatMap { (kid) =>
        if(kid.methodDeclaration != null)
          List(visit(kid).asInstanceOf[DefTree])
        else //if (kid.variableDeclaration != null)
          createVarDecls(kid.variableDeclaration, Flags(FIELD))
      }
      Template(defs, NoId)
    }

    override def visitFormalParameter(@NotNull ctx:
      PrimjParser.FormalParameterContext): Tree = {
      val mods    = if(ctx.mods != null)
                      PARAM | FINAL
                    else
                      Flags(PARAM)
      val tpe = TypeUse(NoId, Some(ctx.`type`.getText),pos(ctx), NoId)
      val name = Name(ctx.Identifier.getText)
      ValDef(mods, NoId, tpe, name, Empty, pos(ctx), NoId)
    }

		override def visitMethodDeclaration(@NotNull ctx:
      PrimjParser.MethodDeclarationContext): Tree = {
      val tpe    = visit(ctx.`type`)
      val name   = ctx.Identifier.getText
      val params = ctx.formalParameters.formalParameterList match {
        case null                                      => List()
        case ps if ps.formalParameter != null          =>
          ps.formalParameter.asScala.toList.map {
            case e  => visit(e).asInstanceOf[ValDef]
          }
        case _                                         => List()
      }
      val body   = visit(ctx.methodBody)
      (tpe, body) match {
        case (tu: TypeUse, b: Block) =>
          MethodDef(noflags, NoId, tu, Name(name), params, b,
            pos(ctx), NoId)
        case _                       =>
          // TODO: report an error
          throw new Exception("Bad tree shape")
      }
    }

    override def visitVarRHS(@NotNull ctx: PrimjParser.VarRHSContext): Tree = {
      if(ctx.expression == null) Empty
      else visit(ctx.expression)
    }


		override def visitVoidType(@NotNull ctx: PrimjParser.VoidTypeContext): Tree = {
      TypeUse(NoId, Some(ctx.getText), pos(ctx), NoId)
    }

		override def visitBlock(@NotNull ctx: PrimjParser.BlockContext): Tree = {
      val stmts   = ctx.statement match {
        case null    => Nil
        case stmts   => stmts.asScala.toList.map(visit(_))
      }
      Block(NoId, stmts, pos(ctx), NoId)
    }

		override def visitIf(@NotNull ctx: PrimjParser.IfContext): Tree = {
      val cond  = visit(ctx.parExpression)
      val thenp = visit(ctx.statement.get(0))
      val elsep = ctx.statement.size match {
        case 2 => visit(ctx.statement.get(1))
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
		override def visitFor(@NotNull ctx: PrimjParser.ForContext): Tree = {
      val inits = ctx.forControl.forInit match {
        case null  => Nil
        case inits =>
          if(inits.expressionList != null)
            visit(inits.expressionList)
                .asInstanceOf[java.util.List[Tree]].asScala.toList
          else
            createVarDefs(inits.variableDefinition,
              Flags(LOCAL_VARIABLE))
      }
      val cond  = ctx.forControl.expression match {
        case null => Empty
        case e    => visit(e)
      }
      val steps = ctx.forControl.forUpdate match {
        case null  => Nil
        case es    =>
          es.expressionList.expression.asScala.toList map {
            case e => visit(e).asInstanceOf[Expr]
          }
      }
      val body  = visit(ctx.statement)
      (cond, body) match {
        case (c: Expr, b: Expr) =>
          For(NoId, inits, c, steps, b, pos(ctx), NoId)
        case _                  =>
          // TODO: report an error
          throw new Exception("Bad tree shape")
      }
    }
		override def visitWhile(@NotNull ctx: PrimjParser.WhileContext): Tree = {
      val cond = visit(ctx.parExpression)
      val body = visit(ctx.statement)
      (cond, body) match {
        case (c: Expr, b: Expr) =>
          While(noflags, c, b, pos(ctx), NoId)
        case _                  =>
          // TODO: report an error
          throw new Exception("Bad tree shape")
      }
    }
		override def visitDoWhile(@NotNull ctx: PrimjParser.DoWhileContext): Tree = {
      val cond = visit(ctx.parExpression)
      val body = visit(ctx.statement)
      (cond, body) match {
        case (c: Expr, b: Expr) =>
          While(Flags(DO_WHILE), c, b, pos(ctx), NoId)
        case _                  =>
          // TODO: report an error
          throw new Exception("Bad tree shape")
      }
    }

		override def visitReturn(@NotNull ctx: PrimjParser.ReturnContext): Tree = {
      ctx.expression match {
        case null                =>
          Return(pos(ctx), NoId)
        case expr                =>
          val e = visit(expr)
          e match {
            case e: Expr         =>
              Return(e, pos(ctx), NoId)
          case _                 =>
            // TODO: report an error
            throw new Exception("Bad tree shape")
          }
      }
    }

    override def visitBlockStmt(@NotNull ctx:
      PrimjParser.BlockStmtContext): Tree = {
      visit(ctx.block)
    }

    override def visitExprStmt(@NotNull ctx: PrimjParser.ExprStmtContext): Tree = {
      visit(ctx.expression)
    }

    override def visitVarStmt(@NotNull ctx: PrimjParser.VarStmtContext): Tree = {
      visit(ctx.variableDeclaration)
    }

    override def visitAssignStmt(@NotNull ctx:
      PrimjParser.AssignStmtContext): Tree = {
      visit(ctx.assign)
    }

    override def visitEmpty(@NotNull ctx: PrimjParser.EmptyContext): Tree = {
      Empty
    }
		override def visitTernary(@NotNull ctx: PrimjParser.TernaryContext): Tree = {
      val cond  = visit(ctx.parExpression)
      val thenp = visit(ctx.expression.get(0))
      val elsep = visit(ctx.expression.get(1))
      (cond, thenp, elsep) match {
        case (c: Expr, t: Expr, e: Expr) =>
          Ternary(c, t, e, toTypeState(notype), pos(ctx), NoId)
        case _                           =>
          // TODO: report an error
          throw new Exception("Bad tree shape")
      }
    }
		override def visitApply(@NotNull ctx: PrimjParser.ApplyContext): Tree = {
      val name   = ctx.Identifier.getText
      val id     = Ident(NoId, Some(name), pos(ctx), NoId)
      val args   = ctx.arguments.expressionList match {
        case null           => Nil
        case args           =>
          args.expression.asScala.toList.map {
            case e => visit(e).asInstanceOf[Expr]
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

    override def visitId(@NotNull ctx: PrimjParser.IdContext): Tree = {
      Ident(NoId, Some(ctx.getText), pos(ctx), NoId)
    }

    override def visitPrimitiveType(
      @NotNull ctx: PrimjParser.PrimitiveTypeContext): Tree = {
      TypeUse(NoId, Some(ctx.getText), pos(ctx), NoId)
    }

    override def visitCast(@NotNull ctx: PrimjParser.CastContext): Tree = {
      val e = visit(ctx.expression)
      val tpt = visit(ctx.primitiveType)
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

    // override def visitStrLit(@NotNull ctx: PrimjParser.StrLitContext): Tree = {
    //   Lit(StringConstant(ctx.getText), pos(ctx))
    // }


    override def visitBoolLit(@NotNull ctx: PrimjParser.BoolLitContext): Tree = {
      Lit(BooleanConstant(ctx.getText.toBoolean), pos(ctx))
    }
  }
}
