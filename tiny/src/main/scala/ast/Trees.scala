package ch.usi.inf.l3.sana.tiny.ast


import ch.usi.inf.l3.sana.tiny
import tiny.source.Position
import tiny.names.Names
import tiny.contexts.TreeContexts
import tiny.contexts.TreeId
import Names._
import tiny.types._

import scalaz.Scalaz._
import scalaz.{Name => _, _}

trait Trees {
  self: Types with TreeContexts =>


  /********************* AST Nodes *********************************/

  trait TreeId

  

  trait Tree extends TreeTraversers {
    def tpe: TypeState[Type]
    def owner: Option[TreeId]
    def pos: Option[Position] 
  }

  trait IdentifiedTree extends Tree {
    def id: TreeId
  }

  trait NamedTree extends Tree {
    def name: Name
  }

  trait DefTree extends IdentifiedTree with NamedTree {
    def mods: Flags
  }


  trait TypeTree extends IdentifiedTree with NamedTree {
    def mods: Flags
  }
  
  trait Statement extends Tree 

  trait Expr extends Statement 


  trait Empty extends Statement {
    def tpe: TypeState[Type]     = point(notype)
    def owner: Option[TreeId]    = None
    def pos: Option[Position]    = None


    override def toString: String = ";"


    // Traverser functions
    // def fold[K](f: (Tree, K) => K, acc: K): K = acc
    // def apply(f: Tree => Tree): Tree = this
    // def filter(p: Tree => Boolean): List[Tree] = Nil
  }

  // Really common ASTs, I cannot imagine a compiler without them
  // FIXME: Do we really need TypeUse?
  trait TypeUse extends Tree {
    def uses: Option[TreeId]
    def tpe: TypeState[Type] = {
      newRWST {
        (ctx: TreeContext) => {
          val r = for {
            i <- uses
            r <- ctx.getTpe(i)
          } yield r
          (ctx, r.getOrElse(ErrorType))
        }
      }
    }
    def name: Name
    override def toString: String = name


    // Traverser functions
    // def fold[K](f: (Tree, K) => K, acc: K): K = f(this, acc)
    // def apply(f: Tree => Tree): Tree = f(this)
    // def filter(p: Tree => Boolean): List[Tree] = if(p(this)) List(this) else Nil
  }
  trait Ident extends Expr {
    def uses: Option[TreeId]
    def tpe: TypeState[Type] = {
       newRWST {
        (ctx: TreeContext) => {
          val r = for {
            i <- uses
            r <- ctx.getTpe(i)
          } yield r
          (ctx, r.getOrElse(ErrorType))
        }
      }
    }
    def name: Name
    override def toString: String = name


    // // Traverser functions
    // def fold[K](f: (Tree, K) => K, acc: K): K = f(this, acc)
    // def apply(f: Tree => Tree): Tree = f(this)
    // def filter(p: Tree => Boolean): List[Tree] = if(p(this)) List(this) else Nil
  }

  

  /***************************** Factories **************************/
  trait TypeUseFactory {
    private class TypeUseImpl(val uses: Option[TreeId], val name: Name, 
      val owner: Option[TreeId], val pos: Option[Position]) extends TypeUse

    def apply(u: Option[TreeId], n: Name, 
      o: Option[TreeId], p: Option[Position]): TypeUse = 
        new TypeUseImpl(u, n, o, p)
  }

  trait IdentFactory {
    private class IdentImpl(val uses: Option[TreeId], val name: Name, 
      val owner: Option[TreeId], val pos: Option[Position]) extends Ident 

    def apply(u: Option[TreeId], n: Name, 
      o: Option[TreeId], p: Option[Position]): Ident = 
        new IdentImpl(u, n, o, p)
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
  val Empty     = new Empty {}



  /******************** Tree Traverses ******************************/
  trait TreeTraversers {
    // def fold[K](f: (Tree, K) => K, acc: K): K
    //
    // def apply(f: Tree => Tree): Tree 
    //
    // def filter(p: Tree => Boolean): List[Tree]
    //   
    //
    // def foreach(f: Tree => Unit): Unit = {
    //   fold[Unit]((x: Tree, y: Unit) => {
    //     f(x)
    //     ()
    //   }, ())
    // } 
  }
}

trait Flags
