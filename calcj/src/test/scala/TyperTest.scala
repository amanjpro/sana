import ch.usi.inf.l3.sana
import sana.calcj
import sana.tiny
import tiny.util._
import tiny.report._
import tiny.contexts.TreeContexts
import calcj.Global
import calcj.ast.Trees
import calcj.ast.Constants
import calcj.types.Types
import calcj.ast.JavaOps._
import calcj.typechecker.Typers
import org.scalatest._

class TyperTest extends FlatSpec with Matchers with Typers {

  type G = Global
  val global: G = new Global {
    val isTest: Boolean = true
  }

  import global._
  val isTest: Boolean = true
  def getTpe(ts: TypeChecker[Tree]): Type = {
    val (_, (_, tree)) = ts.run(EmptyContext).run
    tree.tpe.eval(EmptyContext)
  }
  
  val typer = new Typer {}
  "1L >> 2" should "be long" in {
    val b = Binary(
              Lit(LongConstant(1), None), 
              SHR,
              Lit(IntConstant(2), None), 
              toTypeState(NoType),
              None)
    val tpe = getTpe(typer.typeTree(b))
    tpe =:= LongType
  }

  "(short) 1 >>> 1L" should "be int" in {
    val b = Binary(
              Lit(ShortConstant(1), None), 
              USHR,
              Lit(LongConstant(1), None), 
              toTypeState(NoType),
              None)
    val tpe = getTpe(typer.typeTree(b))
    tpe =:= IntType
  }

  "1 << 1.0f" should "not type check" in {
    val b = Binary(
              Lit(IntConstant(1), None), 
              SHL,
              Lit(FloatConstant(1.0f), None), 
              toTypeState(NoType),
              None)
    val tpe = getTpe(typer.typeTree(b))
    tpe =:= ErrorType
  }

  "(short) 1 >>> true" should "not type check" in {
    val b = Binary(
              Lit(ShortConstant(1), None), 
              USHR,
              Lit(BooleanConstant(true), None), 
              toTypeState(NoType),
              None)
    val tpe = getTpe(typer.typeTree(b))
    tpe =:= ErrorType
  }

  "(short) 1 + -((byte) 2) * 4" should "be int" in {
    val b = Binary(
              Lit(ShortConstant(1), None), 
              Add,
              Binary(
                Unary(Neg,
                  Lit(ByteConstant(2), None), 
                  toTypeState(NoType),
                  None),
                Mul, 
                Lit(IntConstant(4), None), 
                toTypeState(NoType),
                None),
              toTypeState(NoType),
              None)
    val tpe = getTpe(typer.typeTree(b))
    tpe =:= IntType
  }


  "-('a') * 2.2D" should "be double" in {
    val b = Binary(
              Unary(
                Neg,
                Lit(CharConstant('a'), None),
                toTypeState(NoType),
                None),
              Mul,
              Lit(DoubleConstant(2.2D), None),
              toTypeState(NoType),
              None
            )
    val tpe = getTpe(typer.typeTree(b))
    tpe =:= DoubleType
  }

  "(short) 1 + (byte) 2 + \"4\"" should "be String" in {
    val b = Binary(
              Lit(ShortConstant(1), None), 
              Add,
              Binary(
                Lit(ByteConstant(2), None), 
                Add, 
                Lit(StringConstant("4"), None), 
                toTypeState(NoType),
                None),
              toTypeState(NoType),
              None)
    val tpe = getTpe(typer.typeTree(b))
    tpe =:= DoubleType
  }

  "+((short) 1)" should "be int" in {
    val b = Unary(
              Pos,
              Lit(ByteConstant(1), None),
              toTypeState(NoType),
              None)
    val tpe = getTpe(typer.typeTree(b))
    tpe =:= IntType
  }

}
