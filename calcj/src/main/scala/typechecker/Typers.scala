package ch.usi.inf.l3.sana.calcj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.passes
import tiny.report._
import tiny.contexts._
import calcj.Global
import calcj.ast.JavaOps._

import scalaz.{Name => _, Failure => _, _}
import scalaz.Scalaz._

// From Java Specification 1.0 - Sect: 5.2 - p61
// 1- Assignment Conversion
// 2- Method Conversion   Sect: 5.3 - p66
// 3- String Conversion   Sect: 5.4 - p67

trait Typers extends passes.Phases {
  type G <: Global
  import global._

  type TypeChecker[T] = StateWriter[T]

  def toTypeChecker[A](x: ContextState[A]): StateWriter[A] =
    toStateWriter(x)
  def toTypeChecker[A](x: ErrorReportingMonad[A]): StateWriter[A] =
    toStateWriter(x)
  // def toTypeChecker = toStateWriter(_: CompilerErrorMonad[_])
  // def toStateWriter[A](x: ContextStateT[Id, A]): StateWriter[A] =
  //   x.lift[CompilerErrorMonad]
  // implicit def toStateWriter[A](x: CompilerErrorMonad[A]): StateWriter[A] = 
  //   x.liftM[ContextStateT]
  //
  // val toTypeChecker = toStateWriter
  // def toStateWriter[A](x: CompilerErrorMonad[A]): StateWriter[A] = 
  //   x.liftM[ContextStateT]
  //
  trait Typer extends TransformerPhase {
    
    
    val name: String = "typer"
    override val description: Option[String] = 
      Some("The main type-checking phase.")
    override def runRightAfter: Option[String] = Some("namer")


    def startPhase(state: Context, unit: CompilationUnit): 
         (Vector[Report], CompilationUnit, Context) = {
      val tree  = unit.tree
      val (w, (s, typedTree)) = typeTree(tree).run(state).run
      (w, CompilationUnit(unit.id, typedTree, unit.fileName), s)
    }

    def typeTree(tree: Tree): TypeChecker[Tree] = tree match {
      case e: Expr       => for {
        te <- typeExpr(e)
      } yield te
      case _             => 
        error(UNEXPETED_TREE,
          tree.toString, "an expression", tree.pos, tree)
        pointSW(tree)
    }

    def typeExpr(e: Expr): TypeChecker[Expr] = e match {
      case bin: Binary            => for {
        te <- typeBinary(bin)
      } yield te
      case unary: Unary           => for {
        te <- typeUnary(unary)
      } yield te
      case (_: Lit) | (_: Cast)   => pointSW(e)
      case _                      => 
        error(UNEXPETED_TREE,
          e.toString, "an expression", e.pos, e)
        pointSW(e)
    }

    
    def typeUnary(unary: Unary): TypeChecker[Unary] = {
      for {
        etree      <- typeExpr(unary.expr)              
        etpe       <- toTypeChecker(etree.tpe)
        utpe       <- unaryTyper(etpe, unary)
        expr       <- pointSW {                           
          utpe match {
            case u: UnaryType => castIfNeeded(etree, u.op, utpe)
            case u            => 
              etree
          }
        }
        // TODO:
        // Pos unary operator, should ideally perform the cast and return
        // the containing expression not the whole unary expression (the
        // operation is redundant). But this will reproduce the same problem
        // that Scala has, when type checker can return a different tree 
        // type. What should we do here?
        // res        <- unary.op match {
        //   case Pos    => pointSW(expr)
        //   case _      => pointSW(Unary(unary.op, expr, pointSW(utpe), unary.pos))
        // }
        res        <- pointSW(Unary(unary.op, expr,           
                            toTypeState(utpe), unary.pos))
      } yield res 
    }


    

    def typeBinary(bin: Binary): TypeChecker[Binary] = {
      for {
        ltree                <- typeExpr(bin.lhs)
        ltpe                 <- toTypeChecker(ltree.tpe)
        rtree                <- typeExpr(bin.rhs)
        rtpe                 <- toTypeChecker(rtree.tpe)
        // INFO:
        // We do pointSW, and cpointSW the following call, because Scala
        // cannot infer the type of ``let-binding'' well.
        // i.e. I cannot say btpe = binaryTyper(...) without
        // ascribing btpe's type, which results in a warning
        btpe                 <- binaryTyper(rtpe, ltpe, bin)
        // INFO:
        // We cannot pattern match against the tuple here,
        // due to a bug in the Scala Compiler #SI-1336
        // Scala says tuple pattern matching is not irrefutable
        // and it requires the monad (namely our StateMonad) to
        // have a filter method, which does not make much sense
        // for it to have it.
        es                   <- pointSW {
          btpe match {
            case b: BinaryType =>
              val l = castIfNeeded(ltree, b.op1, ltpe)
              val r = castIfNeeded(rtree, b.op2, rtpe)
              (l, r)
            case _             => (ltree, rtree)
          }
        }
        res                  <- 
          pointSW(Binary(es._1, bin.op, es._2, toTypeState(btpe), bin.pos))
      } yield res
    }

    private def castIfNeeded(e: Expr, t1: Type, t2: Type): Expr = {
      if(t1 =:= t2) e
      else {
        val pos = e.pos
        // TODO: Assign TreeId to primitive types
        // TODO: Type names
        val id = TypeUse(NoId, e.owner, pos)
        Cast(id, e, pos)
      }
    }


    def unaryTyper(tpe: Type, unary: Unary): TypeChecker[Type] = {
      (unary.op, tpe)  match {
        case (Not, BooleanType)                              => 
          pointSW(UnaryType(BooleanType, BooleanType))
        case (Pos, x: NumericType)                           => 
          val t = unaryNumericPromotion(x)
          pointSW(UnaryType(x, x))
        case (Neg, x: NumericType)                           => 
          val t = unaryNumericPromotion(x)
          pointSW(UnaryType(x, x))
        case (BCompl, x: IntegralType)                       => 
          val t = unaryNumericPromotion(x)
          pointSW(UnaryType(t, t))
        case (Inc, x: NumericType)                           => 
          pointSW(UnaryType(x, x))
        case (Dec, x: NumericType)                           => 
          pointSW(UnaryType(x, x))
        case (Not, _)                                        => for {
          _ <- toTypeChecker(error(TYPE_MISMATCH,
            tpe.toString, "boolean", unary.expr.pos, unary.expr))
          r <- pointSW(ErrorType)
        } yield r
        case (Pos, _) | (Neg, _) | (Inc, _) | (Dec, _)       => for {
          _ <- toTypeChecker(error(TYPE_MISMATCH,
            tpe.toString, "a numeric type", unary.expr.pos, unary.expr))
          r <- pointSW(ErrorType)
        } yield r
        case _                                               => for {
          _ <- toTypeChecker(error(TYPE_MISMATCH,
            tpe.toString, "an integral type", unary.expr.pos, unary.expr))
          r <- pointSW(ErrorType)
        } yield r
      }
    }

    def binaryTyper(ltpe: Type, 
      rtpe: Type, bin: Binary): TypeChecker[Type] = bin.op match {
        case Gt | Lt | Le | Ge                      => 
          (ltpe, rtpe) match {
            case (x: NumericType, y: NumericType)   =>
              val t = binaryNumericPromotion(x, y)
              pointSW(BinaryType(t, t, BooleanType))
            case (_: NumericType, _)                => for {
              _ <- toTypeChecker(error(TYPE_MISMATCH,
                rtpe.toString, "a numerical type", bin.rhs.pos, bin.rhs))
              r <- pointSW(ErrorType)
            } yield r
            case _                                  => for {
              _ <- toTypeChecker(error(TYPE_MISMATCH,
                ltpe.toString, "a numerical type", bin.lhs.pos, bin.lhs))
              r <- pointSW(ErrorType)
            } yield r
          }
        case Eq | Neq                               => 
          (ltpe, rtpe) match {
            case (BooleanType, BooleanType)         =>
              pointSW(BinaryType(BooleanType, BooleanType, BooleanType))
            case (x: NumericType, y: NumericType)   =>
              val t = binaryNumericPromotion(x, y)
              pointSW(BinaryType(t, t, BooleanType))
            case (StringType, StringType)           => 
              pointSW(BinaryType(StringType, StringType, BooleanType))
            case _                                  => for {
              _ <- toTypeChecker(error(TYPE_MISMATCH,
                ltpe.toString, "a primitive type", bin.pos, bin))
              r <- pointSW(ErrorType)
            } yield r
          }
        case And | Or | Amp | Pipe | Xor            => 
          (ltpe, rtpe) match {
            case (BooleanType, BooleanType)         =>
              pointSW(BinaryType(BooleanType, BooleanType, BooleanType))
            case (BooleanType, _)                   => for {
              _ <- toTypeChecker(error(TYPE_MISMATCH,
                rtpe.toString, "bolean", bin.rhs.pos, bin.rhs))
              r <- pointSW(ErrorType)
            } yield r
            case _                                  => for {
              _ <- toTypeChecker(error(TYPE_MISMATCH,
                ltpe.toString, "bolean", bin.lhs.pos, bin.lhs))
              r <- pointSW(ErrorType)
            } yield r

          } 
        case Add                                    =>
          (ltpe, rtpe) match {
            case (x: NumericType, y: NumericType)   =>
              val t = binaryNumericPromotion(x, y)
              pointSW(BinaryType(t, t, t))
            case (StringType, _: PrimitiveType) | 
                  (_: PrimitiveType, StringType)    =>
              pointSW(BinaryType(StringType, StringType, StringType))
            case (_: NumericType, _)                => for {
              _ <- toTypeChecker(error(TYPE_MISMATCH,
                rtpe.toString, "a numerical type", bin.rhs.pos, bin.rhs))
              r <- pointSW(ErrorType)
            } yield r
            case _                                  => for {
              _ <- toTypeChecker(error(TYPE_MISMATCH,
                ltpe.toString, "a numerical type", bin.lhs.pos, bin.lhs))
              r <- pointSW(ErrorType)
            } yield r
          }
        case Sub | Mul | Div | Mod                  => 
          (ltpe, rtpe) match {
            case (x: NumericType, y: NumericType)   =>
              val t = binaryNumericPromotion(x, y)
              pointSW(BinaryType(t, t, t))
            case (_: NumericType, _)                => for {
              _ <- toTypeChecker(error(TYPE_MISMATCH,
                rtpe.toString, "a numerical type", bin.rhs.pos, bin.rhs))
              r <- pointSW(ErrorType)
            } yield r
            case _                                  => for {
              _ <- toTypeChecker(error(TYPE_MISMATCH,
                ltpe.toString, "a numerical type", bin.lhs.pos, bin.lhs))
              r <- pointSW(ErrorType)
            } yield r

          }

        case BAnd | BOr | BXor                      =>
          (ltpe, rtpe) match {
            case (x: IntegralType, y: IntegralType) =>
              val t = binaryNumericPromotion(x, y)
              pointSW(BinaryType(t, t, t))
            case _                                  => 
              pointSW(ErrorType)
          }
        case SHL | SHR | USHR                       => 
          (ltpe, rtpe) match {
            case (x: IntegralType, y: IntegralType) =>
              val t1 = unaryNumericPromotion(x)
              val t2 = unaryNumericPromotion(y)
              pointSW(BinaryType(t1, t2, t1))
            case (_: IntegralType, _)               => for {
              _ <- toTypeChecker(error(TYPE_MISMATCH,
                rtpe.toString, "an integral type", bin.rhs.pos, bin.rhs))
              r <- pointSW(ErrorType)
            } yield r
            case _                                  => for {
              _ <- toTypeChecker(error(TYPE_MISMATCH,
                ltpe.toString, "an integral type", bin.lhs.pos, bin.lhs))
              r <- pointSW(ErrorType)
            } yield r
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
}
