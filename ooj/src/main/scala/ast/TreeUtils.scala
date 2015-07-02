package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana
import sana.ooj
import sana.brokenj
import brokenj.ast

import ooj.contexts.{TreeContexts, TreeContextApis}
import ooj.types.Types
import ooj.util.Definitions

trait TreeUtils extends ast.TreeUtils {
  self: Trees with Types with TreeContexts with TreeContextApis
        with Definitions =>

  override def isSimpleExpression(tree: Tree): Boolean = tree match {
    case _: ClassDef                                   => false
    case _: PackageDef                                 => false
    case _                                             =>
      super.isSimpleExpression(tree)
  }


  override def isValidStatementExpression(e: Tree): Boolean = e match {
    case _: New               => true
    case _                    => super.isValidStatementExpression(e)
  }

  override def pointsToUse(tree: Tree,
            p: UseTree => Boolean): Boolean = tree match {
    case id: Ident         => p(id)
    case tuse: TypeUse     => p(tuse)
    case slct: Select      => pointsToUse(slct.tree, p)
    case _                 =>
      false
  }


  // make sure that the guards are constant expressions Section 15.27
  override def isConstantExpression(e: Expr,
          ctx: Context): Boolean = e match {
    case Cast(tpt, e)                                 =>
      tpt.tpe.eval(ctx) match {
        case _: PrimitiveType      => isConstantExpression(e, ctx)
        case ct: ClassType         =>
          val javaLangString = ctx.javaLangClass(STRING_TYPE_NAME)
          if(javaLangString == ct.id)
            isConstantExpression(e, ctx)
          else false
        case _                     => false
      }
    case id: Ident                                    =>
      ctx.isFinal(id.uses) && ctx.isVariable(id.uses)
    case Select(qual, id: Ident)                      =>
      pointsToUse(qual, (q) => ctx.isClassOrInterface(q.uses)) &&
        isConstantExpression(id, ctx)
    case _                                            =>
      super.isConstantExpression(e, ctx)
  }

}

