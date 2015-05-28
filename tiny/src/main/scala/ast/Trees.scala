package ch.usi.inf.l3.sana.tiny.ast


import ch.usi.inf.l3.sana.tiny
import tiny.source.Position
import tiny.names.Names
import tiny.contexts._
import Names._
import tiny.types._

import scalaz.Scalaz._
import scalaz.{Name => _, _}

/**
  * A trait that contains AST trees for the `tiny` language.
  *
  * The trait provides AST node factories and extractors only, and new
  * types can be defined by extending the trait.
  *
  * == Defined AST types ==
  *  - [[Tree]]: The parent of all trees
  *  - [[BadTree]]: An AST node that represents a bad tree
  *  - [[IdentifiedTree]]: Trees that have a ''unique'' id, like methods.
  *  - [[NamedTree]]: Trees that have a name, like variables.
  *  - [[DefTree]]: All the trees that define a new name.
  *  - [[Empty]]: An AST node that represents absent of expressions.
  *  - [[Expr]]: An AST node that represents an expression, in tiny 
  *              there is no Statement.
  *  - [[UseTree]]: Trees that use a [[DefTree]]
  *  - [[TypeUse]]: Trees that use a defined type.
  *  - [[Ident]]: Trees that use a defined term.
  *
  * @groupdesc Trees                AST trees to be exported
  * @groupdesc Factories            AST tree factories
  * @groupdesc Extractors           AST tree extractors (views)
  * @groupdesc Api                  AST tree Api
  * @groupdesc Utilities            Utility classes
  *
  * @author Amanj Sherwany
  * @since 0.1
  * @version 0.1
  */
trait Trees {
  self: Types with TreeContexts =>

  type TypeState[T <: Type] = State[TreeContext, T]
  def toTypeState[A <: Type](t: A): State[TreeContext, A] = t.point[ContextState]

 
  //////////////////////////////////////////////////////////////////
  // Api
  //////////////////////////////////////////////////////////////////
  /**
    * Base trait for all AST trees.
    *
    * @group Api
    */
  trait Tree extends TreeTraversers {
    /**
      * Computes the type of this AST node.
      *
      * @see [[TypeState]]
      * @return A computation to compute the type of this AST node in
      *         a typing context.
      */
    def tpe: TypeState[Type]

    /**
      * Returns the owner of this AST node. Owner of a tree is the DefTree that 
      * syntactically encloses this tree.
      * 
      *
      * @see [[contexts.TreeId]]
      * @return Optionally the owner of this AST node.
      */
    def owner: Option[TreeId]

    /**
      * Returns the position of this AST node in the parsed source file.
      *
      * @see [[source.Position]]
      * @return Optionally the position of this AST node.
      */
    def pos: Option[Position] 
  }

  /**
    * The base trait for all AST nodes that have a ''unique id''.
    *
    * @group Api
    */
  trait IdentifiedTree extends Tree {
    /**
      * The ''unique id'' of this AST node. 
      *
      * This ''id'' is used in `owner` computing, and also later in name 
      * resolution.
      *
      * @see [[contexts.TreeId]]
      * @return The id for this AST node.
      */
    def id: TreeId
  }

  /**
    * The base trait for all trees that have a name.
    *
    * @group Api
    */
  trait NamedTree extends Tree {
    /**
      * @return The name of this tree.
      */
    def name: Name
  }

  
  /**
    * The base trait for all trees that introduce a new name.
    *
    * @group Api
    */
  trait DefTree extends IdentifiedTree with NamedTree with Modifiable

  /**
    * The base trait for all trees that introduce a new type.
    *
    * @group Api
    */
  trait TypeTree extends DefTree
      
  /**
    * The base trait for all trees that introduce a new term.
    *
    * @group Api
    */
  trait TermTree extends DefTree

  /**
    * The base trait for all expressions.
    *
    * @group Api
    */
  trait Expr extends Tree 

  /**
    * A trait to represent empty statements.
    *
    * @group Api
    */
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

  /**
    * The base trait for all trees that use a [[DefTree]].
    *
    * @group Api
    */
  trait UseTree extends Tree {
    /**
      * The ''id'' of the [[DefTree]] that this tree points too.
      *
      * @see [[contexts.TreeId]]
      * @return Optionally the id for the [[DefTree]] that this tree uses.
      */
    def uses: Option[TreeId]
  }

  // Really common ASTs, I cannot imagine a compiler without them
  // FIXME: Do we really need TypeUse?
  /**
    * A trait that represents identifiers that point to a [[TypeTree]]
    *
    * @group Api
    */
  trait TypeUse extends UseTree with NamedTree {
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
    override def toString: String = name


    // Traverser functions
    // def fold[K](f: (Tree, K) => K, acc: K): K = f(this, acc)
    // def apply(f: Tree => Tree): Tree = f(this)
    // def filter(p: Tree => Boolean): List[Tree] = if(p(this)) List(this) else Nil
  }
  
  /**
    * A trait that represents identifiers that point to a [[TermTree]]
    *
    * @group Api
    */
  trait Ident extends Expr with UseTree with NamedTree {
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
    override def toString: String = name


    // // Traverser functions
    // def fold[K](f: (Tree, K) => K, acc: K): K = f(this, acc)
    // def apply(f: Tree => Tree): Tree = f(this)
    // def filter(p: Tree => Boolean): List[Tree] = if(p(this)) List(this) else Nil
  }

  

  //////////////////////////////////////////////////////////////////
  // Factories
  //////////////////////////////////////////////////////////////////
  /**
    * The factory for [[TypeUse]] instances
    *
    * @group Factories
    */
  trait TypeUseFactory {
    private class TypeUseImpl(val uses: Option[TreeId], val name: Name, 
      val owner: Option[TreeId], val pos: Option[Position]) extends TypeUse

    /**
      * Creates a [[TypeUse]] instance.
      *
      * @param uses the id of the [[DefTree]] that this instance uses
      * @param name the name of this identifier
      * @param owner the owner of this tree
      * @param pos the position of this tree
      * @return a new [[TypeUse]] instance.
      */
    def apply(uses: Option[TreeId], name: Name, 
      owner: Option[TreeId], pos: Option[Position]): TypeUse = 
        new TypeUseImpl(uses, name, owner, pos)
  }

  /**
    * The factory for [[Ident]] instances
    *
    * @group Factories
    */
  trait IdentFactory {
    private class IdentImpl(val uses: Option[TreeId], val name: Name, 
      val owner: Option[TreeId], val pos: Option[Position]) extends Ident 

    /**
      * Creates a [[Ident]] instance.
      *
      * @param uses the id of the [[DefTree]] that this instance uses
      * @param name the name of this identifier
      * @param owner the owner of this tree
      * @param pos the position of this tree
      * @return a new [[Ident]] instance.
      */
    def apply(uses: Option[TreeId], name: Name, 
      owner: Option[TreeId], pos: Option[Position]): Ident = 
        new IdentImpl(uses, name, owner, pos)
  }



  //////////////////////////////////////////////////////////////////
  // Extractors
  //////////////////////////////////////////////////////////////////
  // trait TypeUseExtractor {
  //   def apply(s: Symbol): TypeUse = new TypeUseImpl(s)
  // }
  //
  // trait IdentExtractor {
  //   def unapply(i: Ident): Option[Symbol] = 
  // }

  //////////////////////////////////////////////////////////////////
  // Factory and Extractor instances
  //////////////////////////////////////////////////////////////////
  /**
    * AST tree for identifiers that point to terms
    * 
    * @group Trees
    */
  val Ident   = new IdentFactory {}
  /**
    * AST tree for identifiers that point to types
    * 
    * @group Trees
    */
  val TypeUse = new TypeUseFactory {}
  /**
    * AST tree for empty statements and trees
    *
    * @group Trees
    */
  val Empty     = new Empty {}
  /**
    * AST tree to represent erroneous trees
    *
    * @group Trees
    */
  case object BadTree extends IdentifiedTree {
    val id: TreeId = NoId
    val tpe: TypeState[Type] = toTypeState(notype)
    val owner: Option[TreeId] = None
    val pos: Option[Position] = None
  }

  //////////////////////////////////////////////////////////////////
  // Utilities
  //////////////////////////////////////////////////////////////////
  /**
    * The base trait for all trees that can have modifier, like `private`,
    * `public` and others.
    *
    * @group Utilities
    */
  trait Modifiable {
    /**
      * @see [[Flags]]
      * @return the modifiers of this node.
      */
    def mods: Flags
  }

  //////////////////////////////////////////////////////////////////
  // Tree Traverses
  //////////////////////////////////////////////////////////////////
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
