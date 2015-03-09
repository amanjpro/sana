package ch.usi.inf.l3.sana.calcj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import tiny.util.CompilationUnit
import tiny.passes
import sana.calcj.symbols
import sana.calcj.ast
import sana.calcj.types
import sana.calcj.ast.JavaOps._


// From Java Specification 1.0 - Sect: 5.2 - p61
// 1- Assignment Conversion
// 2- Method Conversion   Sect: 5.3 - p66
// 3- String Conversion   Sect: 5.4 - p67

trait Typer extends passes.Phase {
  self: ast.Trees with symbols.Symbols with types.Types =>


  def startPhase(unit: CompilationUnit): CompilationUnit = ???

  def typeTree(tree: Tree): Tree = {
    tree match {
      case e: Expr       => typeExpr(e)
      case _             => 
        error(UNEXPETED_TREE,
          tree.toString, "an expression", tree.pos, tree)
        tree
    }
  }

  def typeUnary(unary: Unary): Unary = {
    val etree = typeExpr(unary.expr)
    val r = for {
      etpe   <- etree.tpe
      r      <- unaryTyper(etpe, unary)
      esym   <- getSymbolByType(r.op)
      utree2 <- if(etpe =:= r.op) Some(etree)
                else {
                  val p = etree.pos
                  Some(Cast(Ident(esym, p), etree, p))
                }
    } yield {
      Unary(unary.op, unary.expr, Some(r), unary.pos)
    }
    r.getOrElse(unary)
  }


  def typeExpr(e: Expr): Expr = e match {
    case bin: Binary            => typeBinary(bin)
    case unary: Unary           => typeUnary(unary)
    case (_: Lit) | (_: Cast)   => e
    case _                      => 
      error(UNEXPETED_TREE,
        e.toString, "an expression", e.pos, e)
      e
  }

  def typeBinary(bin: Binary): Binary = {
    val ltree = typeExpr(bin.lhs)
    val rtree = typeExpr(bin.lhs)
    val r     = for {
      ltpe   <- ltree.tpe
      rtpe   <- rtree.tpe
      r      <- binaryTyper(ltpe, rtpe, bin)
      lsym   <- getSymbolByType(r.op1)
      rsym   <- getSymbolByType(r.op2)
      ltree2 <- if(ltpe =:= r.op1) Some(ltree) 
                else {
                  val p = ltree.pos
                  Some(Cast(Ident(lsym , p), ltree, p))
                }
      rtree2 <- if(rtpe =:= r.op2) Some(rtree)
                else {
                  val p = rtree.pos
                  Some(Cast(Ident(rsym , p), rtree, p))
                }
    } yield {
      Binary(ltree2, bin.op, rtree2, Some(r), bin.pos)
    }
    r.getOrElse(bin)
  }



  def unaryTyper(tpe: Type, unary: Unary): Option[UnaryType] = {
    (unary.op, tpe)  match {
      case (Not, BooleanType)                              => 
        Some(UnaryType(BooleanType, BooleanType))
      case (Pos, x: NumericType)                           => 
        val t = unaryNumericPromotion(x)
        Some(UnaryType(x, x))
      case (Neg, x: NumericType)                           => 
        val t = unaryNumericPromotion(x)
        Some(UnaryType(x, x))
      case (BCompl, x: IntegralType)                       => 
        val t = unaryNumericPromotion(x)
        Some(UnaryType(t, t))
      case (Inc, x: NumericType)                           => 
        Some(UnaryType(x, x))
      case (Dec, x: NumericType)                           => 
        Some(UnaryType(x, x))
      case (Not, _)                                        => 
        error(TYPE_MISMATCH,
          tpe.toString, "boolean", unary.expr.pos, unary.expr)
        None
      case (Pos, _) | (Neg, _) | (Inc, _) | (Dec, _)       => 
        error(TYPE_MISMATCH,
          tpe.toString, "a numeric type", unary.expr.pos, unary.expr)
        None
      case _                                               => 
        error(TYPE_MISMATCH,
          tpe.toString, "an integral type", unary.expr.pos, unary.expr)
        None
    }
  }

  def binaryTyper(ltpe: Type, rtpe: Type, 
    bin: Binary): Option[BinaryType] = bin.op match {
      case Gt | Lt | Le | Ge                      => 
        (ltpe, rtpe) match {
          case (x: NumericType, y: NumericType)   =>
            val t = binaryNumericPromotion(x, y)
            Some(BinaryType(t, t, BooleanType))
          case (_: NumericType, _)                => 
            error(TYPE_MISMATCH,
              rtpe.toString, "a numerical type", bin.rhs.pos, bin.rhs)
            None
          case _                                  => 
            error(TYPE_MISMATCH,
              ltpe.toString, "a numerical type", bin.lhs.pos, bin.lhs)
            None
        }
      case Eq | Neq                               => 
        (ltpe, rtpe) match {
          case (BooleanType, BooleanType)         =>
            Some(BinaryType(BooleanType, BooleanType, BooleanType))
          case (x: NumericType, y: NumericType)   =>
            val t = binaryNumericPromotion(x, y)
            Some(BinaryType(t, t, BooleanType))
          case (StringType, StringType)           => 
            Some(BinaryType(StringType, StringType, BooleanType))
          case _                                  => 
            error(TYPE_MISMATCH,
              ltpe.toString, "a primitive type", bin.pos, bin)
            None
        }
      case And | Or | Amp | Pipe | Xor            => 
        (ltpe, rtpe) match {
          case (BooleanType, BooleanType)         =>
            Some(BinaryType(BooleanType, BooleanType, BooleanType))
          case (BooleanType, _)                   => 
            error(TYPE_MISMATCH,
              rtpe.toString, "bolean", bin.rhs.pos, bin.rhs)
            None
          case _                                  =>
            error(TYPE_MISMATCH,
              ltpe.toString, "bolean", bin.lhs.pos, bin.lhs)
            None
        } 
      case Add                                    =>
        (ltpe, rtpe) match {
          case (x: NumericType, y: NumericType)   =>
            val t = binaryNumericPromotion(x, y)
            Some(BinaryType(t, t, t))
          case (StringType, _: PrimitiveType) | 
                (_: PrimitiveType, StringType)    =>
            Some(BinaryType(StringType, StringType, StringType))
          case (_: NumericType, _)                => 
            error(TYPE_MISMATCH,
              rtpe.toString, "a numerical type", bin.rhs.pos, bin.rhs)
            None
          case _                                  => 
            error(TYPE_MISMATCH,
              ltpe.toString, "a numerical type", bin.lhs.pos, bin.lhs)
            None
        }
      case Sub | Mul | Div | Mod                  => 
        (ltpe, rtpe) match {
          case (x: NumericType, y: NumericType)   =>
            val t = binaryNumericPromotion(x, y)
            Some(BinaryType(t, t, t))
          case (_: NumericType, _)                => 
            error(TYPE_MISMATCH,
              rtpe.toString, "a numerical type", bin.rhs.pos, bin.rhs)
            None
          case _                                  => 
            error(TYPE_MISMATCH,
              ltpe.toString, "a numerical type", bin.lhs.pos, bin.lhs)
            None

        }

      case BAnd | BOr | BXor                      =>
        (ltpe, rtpe) match {
          case (x: IntegralType, y: IntegralType) =>
            val t = binaryNumericPromotion(x, y)
            Some(BinaryType(t, t, t))
          case _                                  => None
        }
      case SHL | SHR | USHR                       => 
        (ltpe, rtpe) match {
          case (x: IntegralType, y: IntegralType) =>
            val t1 = unaryNumericPromotion(x)
            val t2 = unaryNumericPromotion(y)
            Some(BinaryType(t1, t2, t1))
          case (_: IntegralType, _)                => 
            error(TYPE_MISMATCH,
              rtpe.toString, "an integral type", bin.rhs.pos, bin.rhs)
            None
          case _                                  => 
            error(TYPE_MISMATCH,
              ltpe.toString, "an integral type", bin.lhs.pos, bin.lhs)
            None
        }

  }

  def binaryNumericPromotion(t1: NumericType, 
    t2: NumericType): PrimitiveType = (t1, t2) match {
    case (DoubleType, _) => DoubleType
    case (_, DoubleType) => DoubleType
    case (FloatType, _)  => FloatType
    case (_, FloatType)  => FloatType
    case (LongType, _)   => LongType
    case (_, LongType)   => LongType
    case (_, _)          => IntType
  }

  def unaryNumericPromotion(t1: NumericType): NumericType = t1 match {
    case LongType        => LongType
    case x: IntegralType => IntType
    case _               => t1
  }
}
