package ch.usi.inf.l3.sana.tiny.ast


import ch.usi.inf.l3.sana.tiny
import tiny.source.Position
import tiny.names.Names
import Names._
import tiny.types._
import tiny.symbols._


/**
 TODO:
 Literal
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
trait Trees {
  self: Types with Symbols =>


  /********************* AST Nodes *********************************/

  // FIXME: Trees should have traverse methods
  trait Tree {
    def tpe: Option[Type]
    def symbol: Option[Symbol]
    def pos: Option[Position] 
  }


  trait DefTree extends Tree {
    def flags: Flags
    def name: Name
    def tpe: Option[Type] = symbol.flatMap(_.tpe)
  }


  trait TypeTree extends Tree {
    def mods: Flags
  }
  

  trait Expr extends Statement 

  trait Statement extends Tree 


  // Really common ASTs, I cannot imagine a compiler without them
  // FIXME: Do we really need TypeUse?
  trait TypeUse extends Tree {
    def tpe: Option[Type] = symbol.flatMap(_.tpe)
    def name: Name
    override def toString: String = name
  }
  trait Ident extends Expr {
    def tpe: Option[Type] = symbol.flatMap(_.tpe)
    def name: Name
    override def toString: String = name
  }

  

  /***************************** Factories **************************/
  trait TypeUseFactory {
    private class TypeUseImpl(val name: Name, val symbol: Option[Symbol],
      val pos: Option[Position]) extends TypeUse {}

    def apply(n: Name, s: Option[Symbol], p: Option[Position]): TypeUse = 
        new TypeUseImpl(n, s, p)

    def apply(s: Symbol, p: Option[Position]): TypeUse = 
        new TypeUseImpl(s.name, Some(s), p)
  }

  trait IdentFactory {
    private class IdentImpl(val name: Name, val symbol: Option[Symbol],
      val pos: Option[Position]) extends Ident {}

    def apply(n: Name, s: Option[Symbol], p: Option[Position]): Ident = 
        new IdentImpl(n, s, p)

    def apply(s: Symbol, p: Option[Position]): Ident = 
        new IdentImpl(s.name, Some(s), p)
  }



  /**************************** Extractors **************************/
  // trait TypeUseExtractor {
  //   def apply(s: Symbol): TypeUse = new TypeUseImpl(s)
  // }
  //
  // trait IdentExtractor {
  //   def unapply(i: Ident): Option[Symbol] = 
  // }
  /******************* Factory and Extractor instances ***************/

  val Ident   = new IdentFactory {}
  val TypeUse = new TypeUseFactory {}
}

trait Flags
