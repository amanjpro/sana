package ch.usi.inf.l3.sana.arrayj.ast


import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import sana.primj
import sana.arrayj
import tiny.source.Position
import tiny.contexts._
import tiny.names.Name
import tiny.util.MonadUtils
import calcj.ast.Constants
import primj.ast
import primj.modifiers.Flags
import arrayj.types


import scalaz.{Name => _, _}
import Scalaz._

trait Trees extends ast.Trees {
  self: types.Types with Constants with TreeContexts with MonadUtils =>


  //////////////////////////////////////////////////////////////////
  // Api
  //////////////////////////////////////////////////////////////////
  trait ArrayValue extends Expr {
    def elements: List[Expr]
    def tpe: TypeState[Type] = elements match {
      case Nil        => toTypeState(EmptyArrayType)
      case _          => 
        StateT {
          case ctx => {
            val head     = elements.head.tpe.eval(ctx)
            val elemType = elements.tail.foldLeft(head)((z, y) => {
              val tpNext = y.tpe.eval(ctx)
              if (z <:< tpNext) tpNext
              else if (z >:> tpNext) z
              else ErrorType
            })
            elemType match {
              case ErrorType    => (ctx, ErrorType)
              case _            => (ctx, ArrayType(elemType))
            }
          }
        }
    }

    def show(ctx: Context): String = 
      s"""|ArrayValue{
          |elements=${showList(elements, ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin
  }


  trait ArrayAccess extends Expr {
    def array: ArrayValue
    def index: Expr
    def tpe: TypeState[Type] = StateT {
      case ctx =>
        val t = array.tpe.eval(ctx) match {
          case x: NonEmptyArrayType => x.elemType
          //case EmptyArrayType       => WHAT?
          case _                    => ErrorType
        }
        (ctx, t)
    }


    def show(ctx: Context): String = 
      s"""|ArrayAccess{
          |array=${array.show(ctx)},
          |index=${index.show(ctx)},
          |owner=${owner},
          |pos=${pos}
          |}""".stripMargin
  }


  //////////////////////////////////////////////////////////////////
  // Extractors
  //////////////////////////////////////////////////////////////////
  trait ArrayValueExtractor {
    def unapply(r: ArrayValue): Option[Seq[Expr]] = r match {
      case null => None
      case _    => Some(r.elements)
    }
  }

  trait ArrayAccessExtractor {
    def unapply(r: ArrayAccess): Option[(ArrayValue, Expr)] = r match {
      case null => None
      case _    => Some((r.array, r.index))
    }
  }

  //////////////////////////////////////////////////////////////////
  // Factories
  //////////////////////////////////////////////////////////////////

  trait ArrayValueFactory {
    private class ArrayValueImpl(val elements: List[Expr], 
      val pos: Option[Position],
      val owner: TreeId) extends ArrayValue

    def apply(elements: List[Expr], pos: Option[Position],
      owner: TreeId): ArrayValue = 
      new ArrayValueImpl(elements, pos, owner)
  }

  trait ArrayAccessFactory {
    private class ArrayAccessImpl(val array: ArrayValue, val index: Expr,
      val pos: Option[Position],
      val owner: TreeId) extends ArrayAccess

    def apply(array: ArrayValue, index: Expr, pos: Option[Position],
      owner: TreeId): ArrayAccess = 
      new ArrayAccessImpl(array, index, pos, owner)
  }
  
  //////////////////////////////////////////////////////////////////
  // Factory and Extractor instances
  //////////////////////////////////////////////////////////////////
  // TODO: Only let Extractors out, or none?

  val ArrayValue  = new ArrayValueExtractor with ArrayValueFactory {}
  val ArrayAccess = new ArrayAccessExtractor with ArrayAccessFactory {}
}
