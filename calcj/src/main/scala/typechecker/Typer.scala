package ch.usi.inf.l3.sana.calcj.typechecker

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.util.CompilationUnits
import tiny.contexts.TreeContexts
import tiny.passes
import tiny.report._
import tiny.util.MonadUtils
import calcj.ast
import calcj.types
import calcj.ast.JavaOps._

import scalaz.{Name => _, Failure => _, _}
import scalaz.Scalaz._
import scala.language.{higherKinds, implicitConversions}

// From Java Specification 1.0 - Sect: 5.2 - p61
// 1- Assignment Conversion
// 2- Method Conversion   Sect: 5.3 - p66
// 3- String Conversion   Sect: 5.4 - p67

trait Typers extends passes.Phases {
  self: ast.Trees with 
        TreeContexts with 
        types.Types with 
        CompilationUnits with
        MonadUtils =>


  type Inner[A]              = WriterT[Id, Vector[Failure], A]
  type Outer[F[_], A]        = StateT[F, TreeContext, A]
  type Stacked[A]             = Outer[Inner, A]
  type TypeChecker[T <: Tree] = Stacked[T]

  protected def point[A](t: A): Outer[Inner, A] = t.point[Stacked]

  def toTypeChecker[A](x: Outer[Id, A]): Stacked[A] = x.lift[Inner]
  // def toTypeChecker[A <: Tree](st: State[TreeContext, A]): TypeChecker[A] = {
//     
  // }

  

  // protected def toState[A](rwst: RWST[A]): State[TreeContext, A] = 
  //   State {
  //     (ctx: TreeContext) => {
  //       val r = rwst.run(Nil, ctx)._2
  //       (ctx, r)
  //     }
  //   }
  //

  trait Typer extends TransformerPhase {
    val name: String = "typer"
    override val description: Option[String] = 
      Some("The main type-checking phase.")
    override def runRightAfter: Option[String] = Some("namer")


    def startPhase(unit: CompilationUnit): 
         (Vector[Failure], CompilationUnit) = {
      val tree  = unit.tree
      val state = unit.state
      val (w, (s, typedTree)) = typeTree(tree).run(state).run
      (w, CompilationUnit(typedTree, s, unit.fileName))
    }

    def typeTree(tree: Tree): TypeChecker[Tree] = tree match {
      case e: Expr       => for {
        te <- typeExpr(e)
      } yield te
      case _             => 
        error(UNEXPETED_TREE,
          tree.toString, "an expression", tree.pos, tree)
        point(tree)
    }

    def typeExpr(e: Expr): TypeChecker[Expr] = e match {
      case bin: Binary            => for {
        te <- typeBinary(bin)
      } yield te
      case unary: Unary           => for {
        te <- typeUnary(unary)
      } yield te
      case (_: Lit) | (_: Cast)   => point(e)
      case _                      => 
        error(UNEXPETED_TREE,
          e.toString, "an expression", e.pos, e)
        point(e)
    }

    
    def typeUnary(unary: Unary): TypeChecker[Unary] = {
      for {
        etree      <- typeExpr(unary.expr)              
        etpe       <- toTypeChecker(etree.tpe)
        utpe       <- point(unaryTyper(etpe, unary))
        expr       <- point {                           
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
        //   case Pos    => point(expr)
        //   case _      => point(Unary(unary.op, expr, point(utpe), unary.pos))
        // }
        res        <- point(Unary(unary.op, expr,           
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
        // We do point, and cpoint the following call, because Scala
        // cannot infer the type of ``let-binding'' well.
        // i.e. I cannot say btpe = binaryTyper(...) without
        // ascribing btpe's type, which results in a warning
        btpe                 <- point(binaryTyper(rtpe, ltpe, bin))
        // INFO:
        // We cannot pattern match against the tuple here,
        // due to a bug in the Scala Compiler #SI-1336
        // Scala says tuple pattern matching is not irrefutable
        // and it requires the monad (namely our StateMonad) to
        // have a filter method, which does not make much sense
        // for it to have it.
        es                   <- point {
          btpe match {
            case b: BinaryType =>
              val l = castIfNeeded(ltree, b.op1, ltpe)
              val r = castIfNeeded(rtree, b.op2, rtpe)
              (l, r)
            case _             => (ltree, rtree)
          }
        }
        res                  <- 
          point(Binary(es._1, bin.op, es._2, toTypeState(btpe), bin.pos))
      } yield res
    }

    private def castIfNeeded(e: Expr, t1: Type, t2: Type): Expr = {
      if(t1 =:= t2) e
      else {
        val pos = e.pos
        // TODO: Assign TreeId to primitive types
        // TODO: Type names
        val id = Ident(None, t1.toString, e.owner, pos)
        Cast(id, e, pos)
      }
    }


    def unaryTyper(tpe: Type, unary: Unary): Type = {
      (unary.op, tpe)  match {
        case (Not, BooleanType)                              => 
          UnaryType(BooleanType, BooleanType)
        case (Pos, x: NumericType)                           => 
          val t = unaryNumericPromotion(x)
          UnaryType(x, x)
        case (Neg, x: NumericType)                           => 
          val t = unaryNumericPromotion(x)
          UnaryType(x, x)
        case (BCompl, x: IntegralType)                       => 
          val t = unaryNumericPromotion(x)
          UnaryType(t, t)
        case (Inc, x: NumericType)                           => 
          UnaryType(x, x)
        case (Dec, x: NumericType)                           => 
          UnaryType(x, x)
        case (Not, _)                                        => 
          error(TYPE_MISMATCH,
            tpe.toString, "boolean", unary.expr.pos, unary.expr)
          ErrorType
        case (Pos, _) | (Neg, _) | (Inc, _) | (Dec, _)       => 
          error(TYPE_MISMATCH,
            tpe.toString, "a numeric type", unary.expr.pos, unary.expr)
          ErrorType
        case _                                               => 
          error(TYPE_MISMATCH,
            tpe.toString, "an integral type", unary.expr.pos, unary.expr)
          ErrorType
      }
    }

    def binaryTyper(ltpe: Type, rtpe: Type, bin: Binary): Type = bin.op match {
        case Gt | Lt | Le | Ge                      => 
          (ltpe, rtpe) match {
            case (x: NumericType, y: NumericType)   =>
              val t = binaryNumericPromotion(x, y)
              BinaryType(t, t, BooleanType)
            case (_: NumericType, _)                => 
              error(TYPE_MISMATCH,
                rtpe.toString, "a numerical type", bin.rhs.pos, bin.rhs)
              ErrorType
            case _                                  => 
              error(TYPE_MISMATCH,
                ltpe.toString, "a numerical type", bin.lhs.pos, bin.lhs)
              ErrorType
          }
        case Eq | Neq                               => 
          (ltpe, rtpe) match {
            case (BooleanType, BooleanType)         =>
              BinaryType(BooleanType, BooleanType, BooleanType)
            case (x: NumericType, y: NumericType)   =>
              val t = binaryNumericPromotion(x, y)
              BinaryType(t, t, BooleanType)
            case (StringType, StringType)           => 
              BinaryType(StringType, StringType, BooleanType)
            case _                                  => 
              error(TYPE_MISMATCH,
                ltpe.toString, "a primitive type", bin.pos, bin)
              ErrorType
          }
        case And | Or | Amp | Pipe | Xor            => 
          (ltpe, rtpe) match {
            case (BooleanType, BooleanType)         =>
              BinaryType(BooleanType, BooleanType, BooleanType)
            case (BooleanType, _)                   => 
              error(TYPE_MISMATCH,
                rtpe.toString, "bolean", bin.rhs.pos, bin.rhs)
              ErrorType
            case _                                  =>
              error(TYPE_MISMATCH,
                ltpe.toString, "bolean", bin.lhs.pos, bin.lhs)
              ErrorType
          } 
        case Add                                    =>
          (ltpe, rtpe) match {
            case (x: NumericType, y: NumericType)   =>
              val t = binaryNumericPromotion(x, y)
              BinaryType(t, t, t)
            case (StringType, _: PrimitiveType) | 
                  (_: PrimitiveType, StringType)    =>
              BinaryType(StringType, StringType, StringType)
            case (_: NumericType, _)                => 
              error(TYPE_MISMATCH,
                rtpe.toString, "a numerical type", bin.rhs.pos, bin.rhs)
              ErrorType
            case _                                  => 
              error(TYPE_MISMATCH,
                ltpe.toString, "a numerical type", bin.lhs.pos, bin.lhs)
              ErrorType
          }
        case Sub | Mul | Div | Mod                  => 
          (ltpe, rtpe) match {
            case (x: NumericType, y: NumericType)   =>
              val t = binaryNumericPromotion(x, y)
              BinaryType(t, t, t)
            case (_: NumericType, _)                => 
              error(TYPE_MISMATCH,
                rtpe.toString, "a numerical type", bin.rhs.pos, bin.rhs)
              ErrorType
            case _                                  => 
              error(TYPE_MISMATCH,
                ltpe.toString, "a numerical type", bin.lhs.pos, bin.lhs)
              ErrorType

          }

        case BAnd | BOr | BXor                      =>
          (ltpe, rtpe) match {
            case (x: IntegralType, y: IntegralType) =>
              val t = binaryNumericPromotion(x, y)
              BinaryType(t, t, t)
            case _                                  => 
              ErrorType
          }
        case SHL | SHR | USHR                       => 
          (ltpe, rtpe) match {
            case (x: IntegralType, y: IntegralType) =>
              val t1 = unaryNumericPromotion(x)
              val t2 = unaryNumericPromotion(y)
              BinaryType(t1, t2, t1)
            case (_: IntegralType, _)                => 
              error(TYPE_MISMATCH,
                rtpe.toString, "an integral type", bin.rhs.pos, bin.rhs)
              ErrorType
            case _                                  => 
              error(TYPE_MISMATCH,
                ltpe.toString, "an integral type", bin.lhs.pos, bin.lhs)
              ErrorType
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
