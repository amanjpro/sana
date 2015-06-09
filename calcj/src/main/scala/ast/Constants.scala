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

    final def show: String = s"Constant(Type=${tpe.show}, value=${value}"
    override final def toString: String = show
  }

  /**************************** Extractors **************************/
  trait ConstantExtractor {
    def unapply(const: Constant): Option[(const.VType)] = Some(const.value)
  }

  /***************************** Factories **************************/
  trait ByteFactory {
    private class ByteConstImpl(val value: Byte, 
      val tpe: Type) extends Constant {
        type VType = Byte
      }

    def apply(value: Byte): Constant = {
      new ByteConstImpl(value, ByteType)
    }
  }

  trait ShortFactory {
    private class ShortConstImpl(val value: Short,
      val tpe: Type) extends Constant {
        type VType = Short
      }

    def apply(value: Short): Constant = {
      new ShortConstImpl(value, ShortType)
    }
  }

  trait CharFactory {
    private class CharConstImpl(val value: Char,
      val tpe: Type) extends Constant {
        type VType = Char
      }

    def apply(value: Char): Constant = {
      new CharConstImpl(value, CharType)
    }
  }
  trait IntFactory {
    private class IntConstImpl(val value: Int,
      val tpe: Type) extends Constant {
        type VType = Int
      }

    def apply(value: Int): Constant = {
      new IntConstImpl(value, IntType)
    }
  }

  trait LongFactory {
    private class LongConstImpl(val value: Long,
      val tpe: Type) extends Constant {
        type VType = Long
      }

    def apply(value: Long): Constant = {
      new LongConstImpl(value, LongType)
    }
  }

  trait FloatFactory {
    private class FloatConstImpl(val value: Float,
      val tpe: Type) extends Constant {
        type VType = Float
      }

    def apply(value: Float): Constant = {
      new FloatConstImpl(value, FloatType)
    }
  }

  trait DoubleFactory {
    private class DoubleConstImpl(val value: Double,
      val tpe: Type) extends Constant {
        type VType = Double
      }

    def apply(value: Double): Constant = {
      new DoubleConstImpl(value, DoubleType)
    }
  }

  trait BooleanFactory {
    private class BooleanConstImpl(val value: Boolean,
      val tpe: Type) extends Constant {
        type VType = Boolean
      }

    def apply(value: Boolean): Constant = {
      new BooleanConstImpl(value, BooleanType)
    }
  }

  trait StringFactory {
    private class StringConstImpl(val value: String,
      val tpe: Type) extends Constant {
        type VType = String
      }

    def apply(value: String): Constant = {
      new StringConstImpl(value, StringType)
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
  val StringConstant  = new StringFactory with ConstantExtractor {}

  val DEFAULT_BYTE    = ByteConstant(0.toByte)
  val DEFAULT_CHAR    = CharConstant('\u0000')
  val DEFAULT_SHORT   = ShortConstant(0.toShort)
  val DEFAULT_INT     = IntConstant(0)
  val DEFAULT_LONG    = LongConstant(0L)
  val DEFAULT_FLOAT   = FloatConstant(0.0f)
  val DEFAULT_DOUBLE  = DoubleConstant(0.0d)
  val DEFAULT_BOOLEAN = BooleanConstant(false)
}
