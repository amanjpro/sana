package ch.usi.inf.l3.sana.ooj.parser

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.brokenj
import sana.ooj


import tiny.source.SourceFile
import tiny.source.Position
import tiny.util.{CompilationUnits, MonadUtils}
import tiny.contexts._
import tiny.parser
import tiny.modifiers.{Flags, Flag}
import tiny.names.Name


import calcj.modifiers._
import primj.modifiers._
import ooj.modifiers._
import ooj.modifiers.Ops._

import ooj.Global
import ooj.ast.Trees
import ooj.ast.Constants
import calcj.ast.JavaOps._
import ooj.types.Types
import ooj.antlr._

import org.antlr.v4.runtime.misc.NotNull
import org.antlr.v4.runtime.{ParserRuleContext, Token}
import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.{AbstractParseTreeVisitor, TerminalNode}


import scalaz.Scalaz._
import scalaz.{Name => _, _}

import scala.collection.JavaConverters._

trait Parsers extends parser.Parsers {

  type G <: Global
  import global._

  def parse(source: SourceFile): CompilationUnit = {
    val tree = new OOJVisitor(source.name).visit(source.content)
    CompilationUnit(NO_COMPILATION_UNIT_ID, tree, source.name)
  }

  class OOJVisitor(val source: String) extends Java1BaseVisitor[Tree] {

    def pos(token: Token): Option[Position] = {
      Some(Position(source, token.getLine, token.getCharPositionInLine + 1))
    }

    def pos(ctx: ParserRuleContext): Option[Position] = {
      val token = ctx.getStart
      Some(Position(source, token.getLine, token.getCharPositionInLine + 1))
    }

    def localVariableDeclaration(
      ctx: Java1Parser.LocalVariableDeclarationContext): List[ValDef] = {
      // Java1 does not allow modifiers on local variables,
      // Because there were no inner class back then
      val mods       = noflags
      val tpt        = visit(ctx.`type`()).asInstanceOf[TypeUse]
      ctx.variableDeclarators.variableDeclarator.asScala.toList.map {
        case ctx =>
          val tpt2   =
            dimsToArrayType(tpt, ctx.variableDeclaratorId.dims)
          val name   = Name(ctx.variableDeclaratorId.Identifier.getText)
          val rhs    = ctx.variableInitializer match {
            case null          => Empty
            case child         => visit(child).asInstanceOf[Expr]
          }
          ValDef(mods, NoId, tpt2, name, rhs, pos(ctx), NoId)
      }
    }


    def modifierToFlag(ctx: Java1Parser.ModifierContext): Flag =
      ctx.getText match {
        case "public"                     => ACC_PUBLIC
        case "protected"                  => ACC_PROTECTED
        case "private"                    => ACC_PRIVATE
        case "static"                     => STATIC
        case "abstract"                   => ABSTRACT
        case "final"                      => FINAL
        // TODO: Uncomment them when you support them
        // case "native"                     => NATIVE
        // case "synchronized"               => SYNCHRONIZED
        // case "transient"                  => TRANSIENT
        // case "volatile"                   => VOLATILE
      }

    def interfacesContextToTypeUses(interfaceContext:
        Java1Parser.ExtendsInterfacesContext): List[TypeUse] =
      interfaceContext match {
        case null                       => Nil
        case ctx                        =>
          interfacesContextToTypeUses(ctx.classOrInterfaceTypeList)
    }


    def interfacesContextToTypeUses(interfaceContext:
      Java1Parser.InterfacesContext): List[TypeUse] =
      interfaceContext match {
        case null                     => Nil
        case ctx                      =>
          interfacesContextToTypeUses(ctx.classOrInterfaceTypeList)
      }

    def interfacesContextToTypeUses(interfaces:
      Java1Parser.ClassOrInterfaceTypeListContext):
        List[TypeUse] = interfaces
                        .classOrInterfaceType
                        .asScala
                        .toList
                        .map {
                          case ctx =>
                            TypeUse(NoId, Some(ctx.name().getText),
                              pos(ctx.name()), NoId)
                        }

    def dimsToArrayType(use: UseTree,
        dims: Java1Parser.DimsContext): UseTree = dims match {
        case null                           => use
        case n                              =>
          dimsToArrayType(use, n.dim.size)
      }

    def dimsToArrayType(use: UseTree, n: Int): UseTree =
      use
      // (1 to n).foldLeft(use)((z, y) => {
      //   // TODO: Make this a type array, when we have array
      //   z
      // })

    def modifiersToFlags(modifiers:
      java.util.List[Java1Parser.ModifierContext],
      packageIsDefault: Boolean = false): Flags = modifiers match {
        case null if packageIsDefault    => Flags(ACC_PACKAGE)
        case null                        => noflags
        case mods                        =>
          mods.asScala.toList match {
            case Nil  if packageIsDefault => Flags(ACC_PACKAGE)
            case mods                     =>
              mods.foldLeft(noflags)((z, y) =>
                  z | modifierToFlag(y))
          }
      }


    def namesToTree(names: List[TerminalNode]): Tree =
      names match {
      case Nil                                   => BadTree
      case List(n)                               =>
        Ident(NoId, Some(n.getText), pos(n.getSymbol), NoId)
      case (x::xs)                               =>
        val qual = namesToTree(xs)
        val sym  = x.getSymbol
        val nme  = x.getText
        val p    = pos(sym)
        Select(qual, Ident(NoId, Some(nme), p, NoId), p, qual.owner)
    }

    def createUnaryOrPostfix[T <: ParserRuleContext](isPostfix: Boolean,
      exp: T, trm: String, ctx: ParserRuleContext): Expr = {

        val e1 = visitChildren(exp).asInstanceOf[Expr]
        val op = trm match {
          case "-"     => Neg
          case "+"     => Pos
          case "++"    => Inc
          case "--"    => Dec
          case "~"     => BCompl
          case "!"     => Not
        }
        if(isPostfix)
          Unary(Flags(POSTFIX), op, e1, toTypeState(notype), pos(ctx))
        else
          Unary(noflags, op, e1, toTypeState(notype), pos(ctx))
    }

    def createBinary[T <: ParserRuleContext](es: java.util.List[T],
      trm: String, ctx: ParserRuleContext): Expr = {
      val e1 = visitChildren(es.get(0)).asInstanceOf[Expr]
      val e2 = visitChildren(es.get(1)).asInstanceOf[Expr]
      createBinary(e1, e2, trm, ctx)
    }
    def createBinary(e1: Expr, e2: Expr,
      trm: String, ctx: ParserRuleContext): Expr = {
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
        Binary(e1, op, e2, toTypeState(notype), pos(ctx))
    }


    override def visitCompilationUnit(ctx:
        Java1Parser.CompilationUnitContext): Tree = {
      visitChildren(ctx)
    }

    override def visitIntLit(ctx: Java1Parser.IntLitContext): Tree = {
      Lit(IntConstant(ctx.getText.toInt), pos(ctx))
    }

    override def visitCharLit(ctx: Java1Parser.CharLitContext): Tree = {
      Lit(CharConstant(ctx.getText.head), pos(ctx))
    }

    override def visitDoubleLit(ctx: Java1Parser.DoubleLitContext): Tree = {
      Lit(DoubleConstant(ctx.getText.toDouble), pos(ctx))
    }


    override def visitBoolLit(ctx: Java1Parser.BoolLitContext): Tree = {
      val b = ctx.getText match {
        case "true"           => true
        case _                => false
      }
      Lit(BooleanConstant(b), pos(ctx))
    }


    override def visitStringLit(ctx: Java1Parser.StringLitContext): Tree = {
      Lit(StringConstant(ctx.getText), pos(ctx))
    }

    override def visitNullLit(ctx: Java1Parser.NullLitContext): Tree = {
      Lit(NullConstant, pos(ctx))
    }


    override def visitType(ctx: Java1Parser.TypeContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitPrimitiveType(ctx:
      Java1Parser.PrimitiveTypeContext): Tree = ctx.getText match {
      case "byte"                =>
        TypeUse(NoId, Some(BYTE_TYPE_NAME.asString), pos(ctx), NoId)
      case "short"               =>
        TypeUse(NoId, Some(SHORT_TYPE_NAME.asString), pos(ctx), NoId)
      case "char"                =>
        TypeUse(NoId, Some(CHAR_TYPE_NAME.asString), pos(ctx), NoId)
      case "int"                 =>
        TypeUse(NoId, Some(INT_TYPE_NAME.asString), pos(ctx), NoId)
      case "long"                =>
        TypeUse(NoId, Some(LONG_TYPE_NAME.asString), pos(ctx), NoId)
      case "float"               =>
        TypeUse(NoId, Some(FLOAT_TYPE_NAME.asString), pos(ctx), NoId)
      case "double"              =>
        TypeUse(NoId, Some(DOUBLE_TYPE_NAME.asString), pos(ctx), NoId)
      case "boolean"             =>
        TypeUse(NoId, Some(CHAR_TYPE_NAME.asString), pos(ctx), NoId)
    }

    override def visitReferenceType(ctx:
      Java1Parser.ReferenceTypeContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }



    override def visitClassOrInterfaceType(ctx:
      Java1Parser.ClassOrInterfaceTypeContext): Tree = {
      visitChildren(ctx) match {
        case id: Ident                                     =>
          TypeUse(id.uses, id.nameAtParser, id.pos, id.owner)
        case s@Select(qual, id: Ident)                     =>
          val tuse = TypeUse(id.uses, id.nameAtParser, id.pos, id.owner)
          Select(qual, tuse, s.pos, s.owner)
        case t                                             =>
          t
      }
    }


    override def visitArrayType(ctx: Java1Parser.ArrayTypeContext): Tree = {
      val use = visitChildren(ctx).asInstanceOf[UseTree]
      dimsToArrayType(use, 1)
    }


    override def visitName(ctx: Java1Parser.NameContext): Tree = {
      namesToTree(ctx.Identifier.asScala.toList)
    }

    override def visitPackageDeclaration(ctx:
      Java1Parser.PackageDeclarationContext): Tree = {
      val name = ctx.Identifier.asScala.toList.mkString(".")
      Ident(NoId, Some(name), pos(ctx), NoId)
    }

    override def visitImportDeclaration(ctx:
      Java1Parser.ImportDeclarationContext): Tree = {
      // TODO: Implement this when you introduce import statements
      visitChildren(ctx)
    }

    override def visitSingleTypeImportDeclaration(ctx:
      Java1Parser.SingleTypeImportDeclarationContext): Tree = {
      // TODO: Implement this when you introduce import statements
      visitChildren(ctx)
    }


    override def visitTypeImportOnDemandDeclaration(ctx:
      Java1Parser.TypeImportOnDemandDeclarationContext): Tree = {
      // TODO: Implement this when you introduce import statements
      visitChildren(ctx)
    }

    override def visitTypeDeclaration(ctx:
      Java1Parser.TypeDeclarationContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitClassDeclaration(ctx:
      Java1Parser.ClassDeclarationContext): Tree = {
      val mods       = modifiersToFlags(ctx.modifier, true)
      val name       = Name(ctx.Identifier.getText)
      val parent     = ctx.parent() match {
        case null                  =>
          val ps      = pos(ctx.Identifier.getSymbol)
          // an Identifier to point to java package
          val javaPkg = Ident(NoId, Some(JAVA_PACKAGE_NAME.asString),
                              ps, NoId)
          // an Identifier to point to lang package
          val langPkg = Ident(NoId, Some(LANG_PACKAGE_NAME.asString),
                              ps, NoId)
          // an Identifier to point to Object type
          val objType = TypeUse(NoId, Some(OBJECT_TYPE_NAME.asString),
                                ps, NoId)
          // Create a Select out of it
          Select(Select(javaPkg, langPkg, ps, NoId),
                objType, ps, NoId)
        case _                     =>
          TypeUse(NoId, Some(ctx.parent().getText),
            pos(ctx.parent()), NoId)
      }
      val interfaces = interfacesContextToTypeUses(ctx.interfaces())
      val body       = visitChildren(ctx.classBody()).asInstanceOf[Template]
      ClassDef(mods, NoId, name, parent::interfaces, body, pos(ctx), NoId)
    }


    override def visitClassBody(ctx: Java1Parser.ClassBodyContext): Tree = {
      ctx.classBodyDeclaration() match {
        case null                     =>
          Template(Nil, NoId)
        case body                     =>
          val members = body.asScala.toList.flatMap { (x) =>
            if(x.classMemberDeclaration != null &&
                x.classMemberDeclaration.fieldDeclaration != null) {
              val ctx = x.classMemberDeclaration.fieldDeclaration
              val mods       = modifiersToFlags(ctx.modifier, true)
              val tpt        = visit(ctx.`type`()).asInstanceOf[TypeUse]
              ctx.variableDeclarators.variableDeclarator.asScala.toList.map {
                case ctx =>
                  val tpt2   =
                    dimsToArrayType(tpt, ctx.variableDeclaratorId.dims)
                  val name   = Name(ctx.variableDeclaratorId.Identifier.getText)
                  val rhs    = ctx.variableInitializer match {
                    case null          => Empty
                    case child         => visit(child).asInstanceOf[Expr]
                  }
                  ValDef(mods, NoId, tpt2, name, rhs, pos(ctx), NoId)
              }
            } else {
              List(visit(x).asInstanceOf[DefTree])
            }
          }
          Template(members, NoId)
      }
    }

    override def visitClassBodyDeclaration(ctx:
      Java1Parser.ClassBodyDeclarationContext): Tree = {
      // INFO: Don't implement this
      visitChildren(ctx)
    }


    override def visitClassMemberDeclaration(ctx:
      Java1Parser.ClassMemberDeclarationContext): Tree = {
      // INFO: Don't implement this
      visitChildren(ctx)
    }

    override def visitFieldDeclaration(ctx:
      Java1Parser.FieldDeclarationContext): Tree = {
      // INFO: Don't implement this
      visitChildren(ctx)
    }

    override def visitVariableDeclarators(ctx:
      Java1Parser.VariableDeclaratorsContext): Tree = {
      // INFO: Don't implement this
      visitChildren(ctx)
    }

    override def visitVariableDeclarator(ctx:
      Java1Parser.VariableDeclaratorContext): Tree = {
      // INFO: Don't implement this
      visitChildren(ctx)
    }

    override def visitVariableDeclaratorId(ctx:
      Java1Parser.VariableDeclaratorIdContext): Tree = {
      // INFO: Don't implement this
      visitChildren(ctx)
    }

    override def visitVariableInitializer(ctx:
      Java1Parser.VariableInitializerContext): Tree = {
      // INFO: Don't implement this
      visitChildren(ctx)
    }

    override def visitMethodDeclaration(ctx:
      Java1Parser.MethodDeclarationContext): Tree = {
      visit(ctx.methodHeader()) match {
        case md: MethodDef                        =>
          val body = visit(ctx.methodBody).asInstanceOf[Expr]
          MethodDef(md.mods, md.id, md.ret, md.name,
            md.params, body, md.pos, md.owner)
        case t                                    =>
          t
      }
    }

    def formalParameterListToValDefs(ctx:
      Java1Parser.FormalParameterListContext): List[ValDef] = {
      ctx match {
        case null                     => Nil
        case ctx                      =>
          ctx.formalParameter().asScala.toList.map ((ctx) => {
            val tpt  = {
              val tpt = visit(ctx.`type`()).asInstanceOf[UseTree]
              val dims = ctx.variableDeclaratorId.dims
              dimsToArrayType(tpt, dims)
            }
            val name = Name(ctx.variableDeclaratorId.Identifier.getText)
            ValDef(Flags(PARAM), NoId, tpt, name, Empty, pos(ctx), NoId)
          })
      }
    }

    override def visitTypedMethodHeader(ctx:
      Java1Parser.TypedMethodHeaderContext): Tree = {
      val mods       = modifiersToFlags(ctx.modifier, true)
      val tpt        = {
        val use  = visit(ctx.`type`()).asInstanceOf[UseTree]
        val dims = ctx.methodDeclarator.dims
        dimsToArrayType(use, dims)
      }
      val name       =
        Name(ctx.methodDeclarator.methodDeclaratorNoDims.Identifier.getText)
      val params     = formalParameterListToValDefs(
        ctx.methodDeclarator.methodDeclaratorNoDims.formalParameterList)
      // TODO: support throws clause, when we have them
      MethodDef(mods, NoId, tpt, name, params, Empty, pos(ctx), NoId)
    }


    override def visitVoidMethodHeader(ctx:
      Java1Parser.VoidMethodHeaderContext): Tree = {
      val mods       = modifiersToFlags(ctx.modifier, true)
      val tpt        = TypeUse(NoId, Some(VOID_TYPE_NAME.asString),
                              pos(ctx), NoId)
      val name       = Name(ctx.methodDeclaratorNoDims().Identifier.getText)
      val params     = formalParameterListToValDefs(
        ctx.methodDeclaratorNoDims().formalParameterList)
      // TODO: support throws clause, when we have them
      MethodDef(mods, NoId, tpt, name, params, Empty, pos(ctx), NoId)
    }


    override def visitThrowsClause(ctx:
      Java1Parser.ThrowsClauseContext): Tree = {
      // TODO: Implement this when you support it
      visitChildren(ctx)
    }

    override def visitClassOrInterfaceTypeList(ctx:
      Java1Parser.ClassOrInterfaceTypeListContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitMethodBody(ctx:
      Java1Parser.MethodBodyContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }


    override def visitStaticInitializer(ctx:
      Java1Parser.StaticInitializerContext): Tree = {
      // TODO: Implement this when you support it
      visitChildren(ctx)
    }

    override def visitConstructorDeclaration(ctx:
      Java1Parser.ConstructorDeclarationContext): Tree = {
      val mods       = modifiersToFlags(ctx.modifier, true) | CONSTRUCTOR
      val declCtx    = ctx.constructorDeclarator.methodDeclaratorNoDims()
      val tpt        = {
        val id = declCtx.Identifier
        TypeUse(NoId, Some(id.getText), pos(id.getSymbol), NoId)
      }
      val name       = CONSTRUCTOR_NAME
      val params     = formalParameterListToValDefs(declCtx.formalParameterList)
      val body       = visit(ctx.constructorBody).asInstanceOf[Expr]
      MethodDef(mods, NoId, tpt, name, params, body, pos(ctx), NoId)
    }


    override def visitConstructorDeclarator(ctx:
      Java1Parser.ConstructorDeclaratorContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }


    override def visitConstructorBody(ctx:
      Java1Parser.ConstructorBodyContext): Tree = {
      val call  = ctx.explicitConstructorInvocation match {
        case null                           => Nil
        case ctx                            => List(visit(ctx))
      }
      val stmts = ctx.blockStatement match {
        case null                           => Nil
        case list                           =>
          list.asScala.toList.map { (x) =>
            visit(ctx)
          }
      }
      Block(NoId, call ++ stmts, pos(ctx), NoId)
    }

    override def visitExplicitConstructorInvocation(ctx:
      Java1Parser.ExplicitConstructorInvocationContext): Tree = {
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      val ps   = pos(ctx)
      val init = Ident(NoId, Some(CONSTRUCTOR_NAME.asString),
        ps, NoId)
      val qual = ctx.qual.getText match {
        case "super"             =>
          Super(NoId, ps, NoId)
        case "this"              =>
          This(NoId, ps, NoId)
      }
      val fun  = Select(qual, init, ps, NoId)
      Apply(fun, args, ps, NoId)

      visitChildren(ctx)
    }


    override def visitInterfaceDeclaration(ctx:
      Java1Parser.InterfaceDeclarationContext): Tree = {
      val mods       = modifiersToFlags(ctx.modifier, true) | INTERFACE
      val name       = Name(ctx.Identifier.getText)
      val interfaces = interfacesContextToTypeUses(
        ctx.extendsInterfaces())
      val body       =
        visitChildren(ctx.interfaceBody()).asInstanceOf[Template]
      ClassDef(mods, NoId, name, interfaces, body, pos(ctx), NoId)
    }

    override def visitExtendsInterfaces(ctx:
      Java1Parser.ExtendsInterfacesContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitInterfaceBody(ctx:
      Java1Parser.InterfaceBodyContext): Tree = {
      ctx.interfaceMemberDeclaration() match {
        case null                     =>
          Template(Nil, NoId)
        case body                     =>
          val members = body.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[DefTree]
          }
          Template(members, NoId)
      }
      visitChildren(ctx)
    }

    override def visitInterfaceMemberDeclaration(ctx:
      Java1Parser.InterfaceMemberDeclarationContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitConstantDeclaration(ctx:
      Java1Parser.ConstantDeclarationContext): Tree = {
      visit(ctx.fieldDeclaration) match {
        case vd: ValDef             =>
          ValDef(vd.mods | FINAL,
            vd.id, vd.tpt, vd.name, vd.rhs, vd.pos, vd.owner)
        case t                      =>
          t
      }
      visitChildren(ctx)
    }

    override def visitAbstractMethodDeclaration(ctx:
      Java1Parser.AbstractMethodDeclarationContext): Tree = {
      visit(ctx.methodHeader) match {
        case md: MethodDef          =>
          MethodDef(md.mods | ABSTRACT,
            md.id, md.ret, md.name, md.params, md.body,
            md.pos, md.owner)
        case t                      =>
          t
      }
    }


    override def visitArrayInitializer(ctx:
      Java1Parser.ArrayInitializerContext): Tree = {
      // TODO: Implement this when you introduce arrays
      visitChildren(ctx)
    }


    override def visitVariableInitializers(ctx:
      Java1Parser.VariableInitializersContext): Tree = {
      // TODO: Implement this when you introduce arrays
      visitChildren(ctx)
    }

    override def visitBlock(ctx:
      Java1Parser.BlockContext): Tree = {
      val stmts = ctx.blockStatement match {
        case null                           => Nil
        case list                           =>
          list.asScala.toList.flatMap { (x) =>
            if(x.localVariableDeclarationStatement != null) {
              val ctx = x.localVariableDeclarationStatement
                         .localVariableDeclaration
              localVariableDeclaration(ctx)
            } else List(visit(ctx))
          }
      }
      Block(NoId, stmts, pos(ctx), NoId)
    }

    override def visitBlockStatement(ctx:
      Java1Parser.BlockStatementContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitLocalVariableDeclarationStatement(ctx:
      Java1Parser.LocalVariableDeclarationStatementContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitLocalVariableDeclaration(ctx:
      Java1Parser.LocalVariableDeclarationContext): Tree = {
      // TODO: it is much like in primj, see how I have done it
      // In Java1 modifiers couldn't appear here (There were no
      // inner classes)
      // val mods       = modifiersToFlags(ctx.modifier, false)
      val tpt        = visit(ctx.`type`()).asInstanceOf[TypeUse]
      visitChildren(ctx)
    }

    override def visitStatement(ctx:
      Java1Parser.StatementContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitStatementNoShortIf(ctx:
      Java1Parser.StatementNoShortIfContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitStatementWithoutTrailingSubstatement(ctx:
      Java1Parser.StatementWithoutTrailingSubstatementContext): Tree = {
      // INFO: Do not use this method
        visitChildren(ctx)
    }

    override def visitEmptyStatement(ctx:
      Java1Parser.EmptyStatementContext): Tree = {
      Empty
    }

    override def visitLabeledStatement(ctx:
      Java1Parser.LabeledStatementContext): Tree = {
      val id   = Name(ctx.Identifier.getText)
      val expr = visit(ctx.statement).asInstanceOf[Expr]
      Label(id, expr, pos(ctx), NoId)
    }

    override def visitLabeledStatementNoShortIf(ctx:
      Java1Parser.LabeledStatementNoShortIfContext): Tree = {
      val id   = Name(ctx.Identifier.getText)
      val expr = visit(ctx.statementNoShortIf).asInstanceOf[Expr]
      Label(id, expr, pos(ctx), NoId)
    }

    override def visitExpressionStatement(ctx:
      Java1Parser.ExpressionStatementContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitStatementExpression(ctx:
      Java1Parser.StatementExpressionContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitIfThenStatement(ctx:
      Java1Parser.IfThenStatementContext): Tree = {
      val cond    = visit(ctx.expression).asInstanceOf[Expr]
      val thenp   = visit(ctx.statement).asInstanceOf[Expr]
      val elsep   = Empty
      If(cond, thenp, elsep, pos(ctx), NoId)
    }

    override def visitIfThenElseStatement(ctx:
      Java1Parser.IfThenElseStatementContext): Tree = {
      visitChildren(ctx)
      val cond    = visit(ctx.expression).asInstanceOf[Expr]
      val thenp   = visit(ctx.statementNoShortIf).asInstanceOf[Expr]
      val elsep   = visit(ctx.statement).asInstanceOf[Expr]
      If(cond, thenp, elsep, pos(ctx), NoId)
    }

    override def visitIfThenElseStatementNoShortIf(ctx:
      Java1Parser.IfThenElseStatementNoShortIfContext): Tree = {
      val cond    = visit(ctx.expression).asInstanceOf[Expr]
      // first branch
      val thenp   = visit(ctx.statementNoShortIf.get(0)).asInstanceOf[Expr]
      // second branch
      val elsep   = visit(ctx.statementNoShortIf.get(1)).asInstanceOf[Expr]
      If(cond, thenp, elsep, pos(ctx), NoId)
    }

    override def visitSwitchStatement(ctx:
      Java1Parser.SwitchStatementContext): Tree = {
      val expr      = visit(ctx.expression).asInstanceOf[Expr]
      val bodyCtx   = ctx.switchBlock
      val body      = {
        val cases1  = bodyCtx.switchBlockStatementGroups match {
          case null                           => Nil
          case groups                         =>
            groups.switchBlockStatementGroup.asScala.toList.map { (x) =>
              visit(x).asInstanceOf[Case]
            }
        }
        val cases2  = bodyCtx.switchLabel match {
          case null                           => Nil
          case labels                         =>
            val lbls = labels.asScala.toList.map { (ctx) =>
              if(ctx.defaultCase == null) {
                visit(ctx.caseLabel.constantExpression).asInstanceOf[Expr]
              } else Default
            }
            List(Case(lbls, Empty, pos(bodyCtx), NoId))
        }
        cases1 ++ cases2
      }
      Switch(expr, body, pos(ctx), NoId)
    }

    override def visitSwitchBlock(ctx: Java1Parser.SwitchBlockContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitSwitchBlockStatementGroups(ctx:
      Java1Parser.SwitchBlockStatementGroupsContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitSwitchBlockStatementGroup(ctx:
      Java1Parser.SwitchBlockStatementGroupContext): Tree = {
      // INFO: Do not use this method
      val guards = ctx.switchLabel.asScala.toList.map { (ctx) =>
        if(ctx.defaultCase == null) {
          visit(ctx.caseLabel.constantExpression).asInstanceOf[Expr]
        } else Default
      }
      val body  = ctx.blockStatement match {
        case null                                => Empty
        case stmts                               =>
          val body = stmts.asScala.toList.map(visit(_))
          Block(NoId, body, pos(ctx), NoId)
      }
      Case(guards, body, pos(ctx), NoId)
    }


    override def visitSwitchLabel(ctx:
      Java1Parser.SwitchLabelContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitCaseLabel(ctx:
      Java1Parser.CaseLabelContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitDefaultCase(ctx:
      Java1Parser.DefaultCaseContext): Tree = {
      // INFO: Do not use this method
      visitChildren(ctx)
    }

    override def visitWhileStatement(ctx:
      Java1Parser.WhileStatementContext): Tree = {
      val expr      = visit(ctx.expression).asInstanceOf[Expr]
      val body      = visit(ctx.statement).asInstanceOf[Expr]
      While(noflags, expr, body, pos(ctx), NoId)
    }

    override def visitWhileStatementNoShortIf(ctx:
      Java1Parser.WhileStatementNoShortIfContext): Tree = {
      val expr      = visit(ctx.expression).asInstanceOf[Expr]
      val body      = visit(ctx.statementNoShortIf).asInstanceOf[Expr]
      While(noflags, expr, body, pos(ctx), NoId)
    }

    override def visitDoStatement(ctx:
      Java1Parser.DoStatementContext): Tree = {
      val expr      = visit(ctx.expression).asInstanceOf[Expr]
      val body      = visit(ctx.statement).asInstanceOf[Expr]
      While(Flags(DO_WHILE), expr, body, pos(ctx), NoId)
    }


    override def visitForStatement(ctx:
      Java1Parser.ForStatementContext): Tree = {
      val inits    = ctx.forInit match {
        case null                                            => Nil
        case init   if init.statementExpressionList == null  =>
          localVariableDeclaration(init.localVariableDeclaration)
        case inits                                           =>
          inits
            .statementExpressionList
            .statementExpression
            .asScala.toList.map {(x) =>
              visit(x).asInstanceOf[Expr]
            }
      }
      val steps    = ctx.forUpdate match {
        case null                                            => Nil
        case steps                                           =>
          steps
            .statementExpressionList
            .statementExpression
            .asScala.toList.map {(x) => visit(x).asInstanceOf[Expr]}
      }
      val cond     = ctx.expression match {
        case null                          => Empty
        case expr                          => visit(expr).asInstanceOf[Expr]
      }
      val body     = visit(ctx.statement).asInstanceOf[Expr]
      For(NoId, inits, cond, steps, body, pos(ctx), NoId)
    }

    override def visitForStatementNoShortIf(ctx:
      Java1Parser.ForStatementNoShortIfContext): Tree = {
      val inits    = ctx.forInit match {
        case null                                            => Nil
        case init   if init.statementExpressionList == null  =>
          localVariableDeclaration(init.localVariableDeclaration)
        case inits                                           =>
          inits
            .statementExpressionList
            .statementExpression
            .asScala.toList.map {(x) =>
              visit(x).asInstanceOf[Expr]
            }
      }
      val steps    = ctx.forUpdate match {
        case null                                            => Nil
        case steps                                           =>
          steps
            .statementExpressionList
            .statementExpression
            .asScala.toList.map {(x) => visit(x).asInstanceOf[Expr]}
      }
      val cond     = ctx.expression match {
        case null                          => Empty
        case expr                          => visit(expr).asInstanceOf[Expr]
      }
      val body     = visit(ctx.statementNoShortIf).asInstanceOf[Expr]
      For(NoId, inits, cond, steps, body, pos(ctx), NoId)
    }

    override def visitForInit(ctx:
      Java1Parser.ForInitContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitForUpdate(ctx:
      Java1Parser.ForUpdateContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitStatementExpressionList(ctx:
      Java1Parser.StatementExpressionListContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitBreakStatement(ctx:
      Java1Parser.BreakStatementContext): Tree = {
      val id      = ctx.Identifier match {
        case null                        => None
        case name                        => Some(Name(name.getText))
      }
      Break(id, pos(ctx), NoId)
    }

    override def visitContinueStatement(ctx:
      Java1Parser.ContinueStatementContext): Tree = {
      val id      = ctx.Identifier match {
        case null                        => None
        case name                        => Some(Name(name.getText))
      }
      Continue(id, pos(ctx), NoId)
    }

    override def visitReturnStatement(ctx:
      Java1Parser.ReturnStatementContext): Tree = {
      ctx.expression match {
        case null                        =>
          Return(pos(ctx), NoId)
        case expr                        =>
          val e = visit(expr).asInstanceOf[Expr]
          Return(e, pos(ctx), NoId)
      }
    }

    override def visitThrowStatement(ctx:
      Java1Parser.ThrowStatementContext): Tree = {
      // TODO: Implement this when you support exceptions
      visitChildren(ctx)
    }

    override def visitSynchronizedStatement(ctx:
      // TODO: Implement this when you support synchronized
      Java1Parser.SynchronizedStatementContext): Tree = {
      visitChildren(ctx)
    }

    override def visitTryStatement(ctx:
      Java1Parser.TryStatementContext): Tree = {
      // TODO: Implement this when you support exceptions
      visitChildren(ctx)
    }

    override def visitCatches(ctx:
      Java1Parser.CatchesContext): Tree = {
      // TODO: Implement this when you support exceptions
      visitChildren(ctx)
    }

    override def visitCatchClause(ctx:
      Java1Parser.CatchClauseContext): Tree = {
      // TODO: Implement this when you support exceptions
      visitChildren(ctx)
    }

    override def visitFinallyClause(ctx:
      Java1Parser.FinallyClauseContext): Tree = {
      // TODO: Implement this when you support exceptions
      visitChildren(ctx)
    }

    override def visitPrimary(ctx:
      Java1Parser.PrimaryContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitArrayAccess(ctx:
      Java1Parser.ArrayAccessContext): Tree = {
      // TODO: Implement this when you support arrays
      visitChildren(ctx)
    }

    override def visitPrimaryNoNewArray(ctx:
      Java1Parser.PrimaryNoNewArrayContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitClassInstanceCreationExpression(ctx:
      Java1Parser.ClassInstanceCreationExpressionContext): Tree = {
      val qual    = visit(ctx.classOrInterfaceType)
      val ps      = pos(ctx)
      val init    = Ident(NoId, Some(CONSTRUCTOR_NAME.asString),
                          ps, NoId)
      val fun     = Select(qual, init, ps, NoId)
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      Apply(fun, args, ps, NoId)
    }

    override def visitArgumentList(ctx:
      Java1Parser.ArgumentListContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitArrayCreationExpression(ctx:
      Java1Parser.ArrayCreationExpressionContext): Tree = {
      // TODO: Implement this when you support arrays
      visitChildren(ctx)
    }

    override def visitDimExpr(ctx:
      Java1Parser.DimExprContext): Tree = {
      // TODO: Implement this when you support arrays
      visitChildren(ctx)
    }

    override def visitDims(ctx:
      Java1Parser.DimsContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitQualifiedFieldAccess(ctx:
      Java1Parser.QualifiedFieldAccessContext): Tree = {
      val id   = Ident(NoId, Some(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol), NoId)
      val qual = visit(ctx.primary)
      Select(qual, id, pos(ctx), NoId)
    }

    override def visitSuperFieldAccess(ctx:
      Java1Parser.SuperFieldAccessContext): Tree = {
      val id   = Ident(NoId, Some(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol), NoId)
      val qual = Super(NoId, pos(ctx), NoId)
      Select(qual, id, pos(ctx), NoId)
    }

    override def visitSimpleMethodInvocation(ctx:
      Java1Parser.SimpleMethodInvocationContext): Tree = {
      val ps   = pos(ctx)
      visitChildren(ctx)
      val fun = visit(ctx.name).asInstanceOf[Expr]
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      Apply(fun, args, ps, NoId)
    }

    override def visitQualifiedMethodInvocation(ctx:
      Java1Parser.QualifiedMethodInvocationContext): Tree = {
      val ps   = pos(ctx)
      val id   = Ident(NoId, Some(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol), NoId)
      val qual = visit(ctx.primary)
      val fun  = Select(qual, id, pos(ctx), NoId)
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      Apply(fun, args, ps, NoId)
    }

    override def visitSuperMethodInvocation(ctx:
      Java1Parser.SuperMethodInvocationContext): Tree = {
      val ps   = pos(ctx)
      val id   = Ident(NoId, Some(ctx.Identifier.getText),
                        pos(ctx.Identifier.getSymbol), NoId)
      val qual = Super(NoId, pos(ctx), NoId)
      val fun  = Select(qual, id, pos(ctx), NoId)
      val args = ctx.argumentList match {
        case null                       => Nil
        case list                       =>
          list.expression.asScala.toList.map { (x) =>
            visit(x).asInstanceOf[Expr]
          }
      }
      Apply(fun, args, ps, NoId)
    }

    override def visitPrimaryExpression(ctx:
      Java1Parser.PrimaryExpressionContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitNameExpression(ctx:
      Java1Parser.NameExpressionContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitCastExpression(ctx:
      Java1Parser.CastExpressionContext): Tree = {
      // INFO: Do not implement this
      val tpt  = visit(ctx.`type`()).asInstanceOf[UseTree]
      val expr = visit(ctx.expression).asInstanceOf[Expr]
      Cast(tpt, expr, pos(ctx))
    }

    override def visitPostfixExpression(ctx:
      Java1Parser.PostfixExpressionContext): Tree = {
      createUnaryOrPostfix(true, ctx.expression, ctx.op.getText, ctx)
    }

    override def visitUnaryExpression(ctx:
      Java1Parser.UnaryExpressionContext): Tree = {
      createUnaryOrPostfix(false, ctx.expression, ctx.op.getText, ctx)
    }

    override def visitBitwiseUnaryExpression(ctx:
      Java1Parser.BitwiseUnaryExpressionContext): Tree = {
      createUnaryOrPostfix(false, ctx.expression, ctx.op.getText, ctx)
    }


    override def visitAssignment(ctx:
      Java1Parser.AssignmentContext): Tree = {
      val lhs      = visit(ctx.leftHandSide).asInstanceOf[Expr]
      val rhs      = visit(ctx.expression).asInstanceOf[Expr]
      Assign(lhs, rhs, pos(ctx), NoId)
    }

    override def visitLeftHandSide(ctx:
      Java1Parser.LeftHandSideContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }

    override def visitMulBinaryExpression(ctx:
      Java1Parser.MulBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
    }

    override def visitAddBinaryExpression(ctx:
      Java1Parser.AddBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
    }

    override def visitShiftBinaryExpression(ctx:
      Java1Parser.ShiftBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
    }

    override def visitRelBinaryExpression(ctx:
      Java1Parser.RelBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
    }

    override def visitInstanceOfExpression(ctx:
      Java1Parser.InstanceOfExpressionContext): Tree = {
      // TODO: Implement this, when you support casting (all of it)
      visitChildren(ctx)
    }

    override def visitEquBinaryExpression(ctx:
      Java1Parser.EquBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, ctx.op.getText, ctx)
    }

    override def visitBitAndBinaryExpression(ctx:
      Java1Parser.BitAndBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, "&", ctx)
    }

    override def visitBitOrBinaryExpression(ctx:
      Java1Parser.BitOrBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, "|", ctx)
    }

    override def visitBitXOrBinaryExpression(ctx:
      Java1Parser.BitXOrBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, "^", ctx)
    }

    override def visitAndBinaryExpression(ctx:
      Java1Parser.AndBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, "&&", ctx)
    }

    override def visitOrBinaryExpression(ctx:
      Java1Parser.OrBinaryExpressionContext): Tree = {
      createBinary(ctx.expression, "||", ctx)
    }

    override def visitAssignExpression(ctx:
      Java1Parser.AssignExpressionContext): Tree = {
      val lhs      = visit(ctx.leftHandSide).asInstanceOf[Expr]
      val rhs      = visit(ctx.expression).asInstanceOf[Expr]
      ctx.op.getText match {
        case "="                         =>
          Assign(lhs, rhs, pos(ctx), NoId)
        case op                          =>
          val op2      = op.take(op.length - 1)
          val expr     = createBinary(lhs, rhs, op2, ctx)
          Assign(lhs, expr, pos(ctx), NoId)
      }
    }

    override def visitTernaryExpression(ctx:
      Java1Parser.TernaryExpressionContext): Tree = {
      val cond   = visit(ctx.expression.get(0)).asInstanceOf[Expr]
      val thenp  = visit(ctx.expression.get(1)).asInstanceOf[Expr]
      val elsep  = visit(ctx.expression.get(2)).asInstanceOf[Expr]
      Ternary(cond, thenp, elsep, toTypeState(notype), pos(ctx), NoId)
    }

    override def visitConstantExpression(ctx:
      Java1Parser.ConstantExpressionContext): Tree = {
      // INFO: Do not implement this
      visitChildren(ctx)
    }
  }
}
