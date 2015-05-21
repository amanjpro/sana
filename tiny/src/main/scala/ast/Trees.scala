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

  // FIXME: Can I get rid of this ContextState?
  private type ContextState[A] = State[TreeContext, A]
  type TypeState[T <: Type] = State[TreeContext, T]

  def toTypeState[A <: Type](t: A): State[TreeContext, A] = t.point[ContextState]


  /********************* AST Nodes *********************************/

  trait IdKind
  object NoKind extends IdKind

  trait TreeId {
    val unitId: Int
    val id: Int
    val kind: IdKind
  }
  object NoId extends TreeId {
    val unitId: Int = -1
    val id: Int = -1
    val kind: IdKind = NoKind
  }

  

  trait Tree extends TreeTraversers {
    def tpe: TypeState[Type]
    def owner: Option[TreeId]
    def pos: Option[Position] 
  }

  object BadTree extends IdentifiedTree {
    val id: TreeId = NoId
    val tpe: TypeState[Type] = toTypeState(notype)
    val owner: Option[TreeId] = None
    val pos: Option[Position] = None
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
  

  trait Expr extends Tree 


  trait Empty extends Expr {
    def tpe: TypeState[Type]     = toTypeState(notype)
    def owner: Option[TreeId]    = None
    def pos: Option[Position]    = None


    override def toString: String = ";"


    // Traverser functions
    // def fold[K](f: (Tree, K) => K, acc: K): K = acc
    // def apply(f: Tree => Tree): Tree = this
    // def filter(p: Tree => Boolean): List[Tree] = Nil
  }

  trait UseTree extends Tree

  // Really common ASTs, I cannot imagine a compiler without them
  // FIXME: Do we really need TypeUse?
  trait TypeUse extends UseTree {
    def uses: Option[TreeId]
    def tpe: TypeState[Type] = {
      State {
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
  trait Ident extends Expr with UseTree {
    def uses: Option[TreeId]
    def tpe: TypeState[Type] = {
       State {
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

    def apply(uses: Option[TreeId], name: Name, 
      owner: Option[TreeId], pos: Option[Position]): TypeUse = 
        new TypeUseImpl(uses, name, owner, pos)
  }

  trait IdentFactory {
    private class IdentImpl(val uses: Option[TreeId], val name: Name, 
      val owner: Option[TreeId], val pos: Option[Position]) extends Ident 

    def apply(uses: Option[TreeId], name: Name, 
      owner: Option[TreeId], pos: Option[Position]): Ident = 
        new IdentImpl(uses, name, owner, pos)
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
    // type T = this.type
    // def fold[K](acc: K)(f: (T, K) => K): K
    //
    //
    // def map(f: T => T): T = fold(this)((z: T, y: T){
    //   f(y)
    // })
    //
    // def foreach(f: T => Unit): Unit = {
    //   fold[Unit](())((x: T, y: Unit) => {
    //     f(x)
    //     ()
    //   })
    // } 
  }
}

trait Flags
