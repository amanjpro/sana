package ch.usi.inf.l3.sana.calcj.symbols



import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.symbols
import calcj.types.Types
import tiny.source.Position
import tiny.names.Names
import Names._

trait Symbols extends symbols.Symbols {
  self: Types =>


  trait TypeSymbol extends Symbol
  trait PrimitiveTypeSymbol extends TypeSymbol 

  object ByteSymbol extends PrimitiveTypeSymbol {
    def name: Name = "byte"
    override def toString: String = name + " symbol"
    def tpe: Option[Type] = Some(ByteType)
  }

  object ShortSymbol extends PrimitiveTypeSymbol {
    def name: Name = "short"
    override def toString: String = name + " symbol"
    def tpe: Option[Type] = Some(ShortType)
  }

  object CharSymbol extends PrimitiveTypeSymbol {
    def name: Name = "char"
    override def toString: String = name + " symbol"
    def tpe: Option[Type] = Some(CharType)
  }

  object IntSymbol extends PrimitiveTypeSymbol {
    def name: Name = "int"
    override def toString: String = name + " symbol"
    def tpe: Option[Type] = Some(IntType)
  }

  object LongSymbol extends PrimitiveTypeSymbol {
    def name: Name = "long"
    override def toString: String = name + " symbol"
    def tpe: Option[Type] = Some(LongType)
  }

  object FloatSymbol extends PrimitiveTypeSymbol {
    def name: Name = "float"
    override def toString: String = name + " symbol"
    def tpe: Option[Type] = Some(FloatType)
  }

  object DoubleSymbol extends PrimitiveTypeSymbol {
    def name: Name = "double"
    override def toString: String = name + " symbol"
    def tpe: Option[Type] = Some(DoubleType)
  }

  object BooleanSymbol extends PrimitiveTypeSymbol {
    def name: Name = "boolean"
    override def toString: String = name + " symbol"
    def tpe: Option[Type] = Some(BooleanType)
  }

  object StringSymbol extends Symbol {
    def name: Name = "String"
    override def toString: String = name + " symbol"
    def tpe: Option[Type] = Some(StringType)
  }


  def getSymbolByType(tpe: Type): Option[Symbol] = {
    tpe match {
      case ByteType    => Some(ByteSymbol)
      case ShortType   => Some(ShortSymbol)
      case CharType    => Some(CharSymbol)
      case IntType     => Some(IntSymbol)
      case LongType    => Some(LongSymbol)
      case FloatType   => Some(FloatSymbol)
      case DoubleType  => Some(DoubleSymbol)
      case BooleanType => Some(BooleanSymbol)
      case StringType  => Some(StringSymbol)
      case _           => None
    }
  }
}
