import ch.usi.inf.l3.sana
import sana.calcj
import sana.tiny
import calcj.ast.Trees
import calcj.ast.Constants
import calcj.types.Types
import calcj.symbols.Symbols
import calcj.misc.JavaOps._
import calcj.typechecker.Typer
import org.scalatest._

class TyperTest extends FlatSpec with Matchers with Trees with 
  Constants with Types with Typer with Symbols {
  "1L >> 2" should "be long" in {
    val b = Binary(
              Lit(LongConstant(1), None), 
              SHR,
              Lit(IntConstant(2), None), 
              None,
              None)
    typeTree(b).tpe.map(_ == LongType).getOrElse(false)
  }

  "(short) 1 >>> 1L" should "be int" in {
    val b = Binary(
              Lit(ShortConstant(1), None), 
              USHR,
              Lit(LongConstant(1), None), 
              None,
              None)
    typeTree(b).tpe.map(_ == IntType).getOrElse(false)
  }

  "1 << 1.0f" should "not type check" in {
    val b = Binary(
              Lit(IntConstant(1), None), 
              SHL,
              Lit(FloatConstant(1.0f), None), 
              None,
              None)
    typeTree(b).tpe == None
  }

  "(short) 1 >>> true" should "not type check" in {
    val b = Binary(
              Lit(ShortConstant(1), None), 
              USHR,
              Lit(BooleanConstant(true), None), 
              None,
              None)
    typeTree(b).tpe == None
  }

  "(short) 1 + -((byte) 2) * 4" should "be int" in {
    val b = Binary(
              Lit(ShortConstant(1), None), 
              Add,
              Binary(
                Unary(Neg,
                  Lit(ByteConstant(2), None), 
                  None,
                  None),
                Mul, 
                Lit(IntConstant(4), None), 
                None,
                None),
              None,
              None)
    typeTree(b).tpe.map(_ == IntType).getOrElse(false)
  }


  "-('a') * 2.2D" should "be double" in {
    val b = Binary(
              Unary(
                Neg,
                Lit(CharConstant('a'), None),
                None,
                None),
              Mul,
              Lit(DoubleConstant(2.2D), None),
              None,
              None
            )
    typeTree(b).tpe.map(_ == DoubleType).getOrElse(false)
  }

  "(short) 1 + (byte) 2 + \"4\"" should "be String" in {
    val b = Binary(
              Lit(ShortConstant(1), None), 
              Add,
              Binary(
                Lit(ByteConstant(2), None), 
                Add, 
                Lit(StringConstant("4"), None), 
                None,
                None),
              None,
              None)
    typeTree(b).tpe.map(_ == StringType).getOrElse(false)
  }
  
}
