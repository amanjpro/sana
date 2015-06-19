package ch.usi.inf.l3.sana.calcj.ast



/**
 Literal default values:
 byte: (byte)0
 short: (short)0
 int: 0
 long: 0L
 float: 0.0f
 double: 0.0d
 char: '\u0000'
 boolean: false
 */

import ch.usi.inf.l3.sana
import sana.calcj.types._
import sana.calcj.contexts._

trait Constants {
  self: Types with Trees with TreeContexts =>

  trait Constant {
    type VType
    def value: VType
    def tpe: TypeState[Type]

    final def show(ctx: Context): String = 
      s"Constant(Type=${tpe.eval(ctx).show}, value=${value}"
    override final def toString: String = show(emptyContext)
  }

  /**************************** Extractors **************************/
  trait ConstantExtractor {
    def unapply(const: Constant): Option[(const.VType)] = Some(const.value)
  }

  /***************************** Factories **************************/
  trait ByteFactory {
    private class ByteConstImpl(val value: Byte, 
      val tpe: TypeState[Type]) extends Constant {
        type VType = Byte
      }

    def apply(value: Byte): Constant = {
      new ByteConstImpl(value, toTypeState(ByteType))
    }
  }

  trait ShortFactory {
    private class ShortConstImpl(val value: Short,
      val tpe: TypeState[Type]) extends Constant {
        type VType = Short
      }

    def apply(value: Short): Constant = {
      new ShortConstImpl(value, toTypeState(ShortType))
    }
  }

  trait CharFactory {
    private class CharConstImpl(val value: Char,
      val tpe: TypeState[Type]) extends Constant {
        type VType = Char
      }

    def apply(value: Char): Constant = {
      new CharConstImpl(value, toTypeState(CharType))
    }
  }
  trait IntFactory {
    private class IntConstImpl(val value: Int,
      val tpe: TypeState[Type]) extends Constant {
        type VType = Int
      }

    def apply(value: Int): Constant = {
      new IntConstImpl(value, toTypeState(IntType))
    }
  }

  trait LongFactory {
    private class LongConstImpl(val value: Long,
      val tpe: TypeState[Type]) extends Constant {
        type VType = Long
      }

    def apply(value: Long): Constant = {
      new LongConstImpl(value, toTypeState(LongType))
    }
  }

  trait FloatFactory {
    private class FloatConstImpl(val value: Float,
      val tpe: TypeState[Type]) extends Constant {
        type VType = Float
      }

    def apply(value: Float): Constant = {
      new FloatConstImpl(value, toTypeState(FloatType))
    }
  }

  trait DoubleFactory {
    private class DoubleConstImpl(val value: Double,
      val tpe: TypeState[Type]) extends Constant {
        type VType = Double
      }

    def apply(value: Double): Constant = {
      new DoubleConstImpl(value, toTypeState(DoubleType))
    }
  }

  trait BooleanFactory {
    private class BooleanConstImpl(val value: Boolean,
      val tpe: TypeState[Type]) extends Constant {
        type VType = Boolean
      }

    def apply(value: Boolean): Constant = {
      new BooleanConstImpl(value, toTypeState(BooleanType))
    }
  }

  

  /******************* Factory and Extractor instances ***************/
  val ByteConstant    = new ByteFactory with ConstantExtractor {}
  val ShortConstant   = new ShortFactory with ConstantExtractor {}
  val CharConstant    = new CharFactory with ConstantExtractor {}
  val IntConstant     = new IntFactory with ConstantExtractor {}
  val LongConstant    = new LongFactory with ConstantExtractor {}
  val FloatConstant   = new FloatFactory with ConstantExtractor {}
  val DoubleConstant  = new DoubleFactory with ConstantExtractor {}
  val BooleanConstant = new BooleanFactory with ConstantExtractor {}

  val DEFAULT_BYTE    = ByteConstant(0.toByte)
  val DEFAULT_CHAR    = CharConstant('\u0000')
  val DEFAULT_SHORT   = ShortConstant(0.toShort)
  val DEFAULT_INT     = IntConstant(0)
  val DEFAULT_LONG    = LongConstant(0L)
  val DEFAULT_FLOAT   = FloatConstant(0.0f)
  val DEFAULT_DOUBLE  = DoubleConstant(0.0d)
  val DEFAULT_BOOLEAN = BooleanConstant(false)
}
