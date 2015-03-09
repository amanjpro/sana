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

trait Constants {
  self: Types =>

  trait Constant {
    type VType
    def value: VType
    def tpe: Type
  }

  trait ByteFactory {
    private class ByteConstImpl(val value: Byte, 
      val tpe: Type) extends Constant {
        type VType = Byte
      }

    def apply(v: Byte): Constant = {
      new ByteConstImpl(v, ByteType)
    }
  }

  trait ShortFactory {
    private class ShortConstImpl(val value: Short,
      val tpe: Type) extends Constant {
        type VType = Short
      }

    def apply(v: Short): Constant = {
      new ShortConstImpl(v, ShortType)
    }
  }

  trait CharFactory {
    private class CharConstImpl(val value: Char,
      val tpe: Type) extends Constant {
        type VType = Char
      }

    def apply(v: Char): Constant = {
      new CharConstImpl(v, CharType)
    }
  }
  trait IntFactory {
    private class IntConstImpl(val value: Int,
      val tpe: Type) extends Constant {
        type VType = Int
      }

    def apply(v: Int): Constant = {
      new IntConstImpl(v, IntType)
    }
  }

  trait LongFactory {
    private class LongConstImpl(val value: Long,
      val tpe: Type) extends Constant {
        type VType = Long
      }

    def apply(v: Long): Constant = {
      new LongConstImpl(v, LongType)
    }
  }

  trait FloatFactory {
    private class FloatConstImpl(val value: Float,
      val tpe: Type) extends Constant {
        type VType = Float
      }

    def apply(v: Float): Constant = {
      new FloatConstImpl(v, FloatType)
    }
  }

  trait DoubleFactory {
    private class DoubleConstImpl(val value: Double,
      val tpe: Type) extends Constant {
        type VType = Double
      }

    def apply(v: Double): Constant = {
      new DoubleConstImpl(v, DoubleType)
    }
  }

  trait BooleanFactory {
    private class BooleanConstImpl(val value: Boolean,
      val tpe: Type) extends Constant {
        type VType = Boolean
      }

    def apply(v: Boolean): Constant = {
      new BooleanConstImpl(v, BooleanType)
    }
  }

  trait StringFactory {
    private class StringConstImpl(val value: String,
      val tpe: Type) extends Constant {
        type VType = String
      }

    def apply(v: String): Constant = {
      new StringConstImpl(v, StringType)
    }
  }

  val ByteConstant    = new ByteFactory {}
  val ShortConstant   = new ShortFactory {}
  val CharConstant    = new CharFactory {}
  val IntConstant     = new IntFactory {}
  val LongConstant    = new LongFactory {}
  val FloatConstant   = new FloatFactory {}
  val DoubleConstant  = new DoubleFactory {}
  val BooleanConstant = new BooleanFactory {}
  val StringConstant  = new StringFactory {}

  val DEFAULT_BYTE    = ByteConstant(0.toByte)
  val DEFAULT_CHAR    = CharConstant('\u0000')
  val DEFAULT_SHORT   = ShortConstant(0.toShort)
  val DEFAULT_INT     = IntConstant(0)
  val DEFAULT_LONG    = LongConstant(0L)
  val DEFAULT_FLOAT   = FloatConstant(0.0f)
  val DEFAULT_DOUBLE  = DoubleConstant(0.0d)
  val DEFAULT_BOOLEAN = BooleanConstant(false)
}
