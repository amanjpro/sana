package ch.usi.inf.l3.sana.tiny.ast


import ch.usi.inf.l3.sana.tiny
import tiny.source.Position
import tiny.names._
import tiny.contexts._
import tiny.modifiers._
import tiny.util._
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
  self: Types with TreeContexts with MonadUtils with Definitions =>

  type TypeState[T <: Type] = State[Context, T]
  def toTypeState[A <: Type](t: A): TypeState[A] = t.point[ContextState]


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
      * @see [[tiny.contexts.TreeId]]
      * @return The owner of this AST node, or returns NoId if not found.
      */
    def owner: TreeId

    /**
      * Returns the position of this AST node in the parsed source file.
      *
      * @see [[tiny.source.Position]]
      * @return Optionally the position of this AST node.
      */
    def pos: Option[Position]


    /**
      * Returns the string representation of this AST node in a given context.
      *
      * This method may not be used for pretty printing, but for debugging
      * purposes.
      */
    def show(ctx: Context): String

    /**
      * Returns the string representation of this AST node in a given context.
      *
      * This method shows a more concise representation than `show`, but shows
      * less information.
      */

    def asString(ctx: Context): String

    override final def toString: String = asString(emptyContext)
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
      * @see [[tiny.contexts.TreeId]]
      * @return The id for this AST node.
      */
    def id: TreeId

  }

  /**
   * The base trait for all AST nodes that have a nam
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
    * The base trait for all trees that can have modifier, like `private`,
    * `public` and others.
    *
    * @group Utilities
    */
  trait Modifiable extends Tree {
    /**
      * @see [[tiny.modifiers.Flags]]
      * @return the modifiers of this node.
      */
    def mods: Flags
  }

  /**
   * The base trait for all trees that introduce a new name.
   *
   * @group Api
   */
  trait DefTree extends IdentifiedTree with Modifiable with NamedTree

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
    val tpe: TypeState[Type]     = toTypeState(notype)
    val owner: TreeId            = NoId
    val pos: Option[Position]    = None


    def show(ctx: Context): String = "<empty-tree>"
    def asString(ctx: Context): String = ";"


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
      * @see [[tiny.contexts.TreeId]]
      * @return Optionally the id for the [[DefTree]] that this tree uses.
      */
    def uses: TreeId

    def nameAtParser: Option[String]

    val tpe: TypeState[Type] = {
       State {
        (ctx: Context) => {
          ctx.getTpe(uses).run(ctx)
        }
      }
    }

    val name: ContextState[Name] = {
      State {
        (ctx: Context) => {
          val n = for {
            r <- ctx.getName(uses)
          } yield r
          val r = (n, nameAtParser) match {
            case (None, None)    => ERROR_NAME
            case (None, Some(n)) => Name(n)
            case (Some(n), _)    => n
          }
          (ctx, r)
        }
      }
    }
  }

  /**
    * A trait that represents simple use trees, that compose of
    * one tee exactly. An example is Ident and TypeUse, but not
    * Select.
    *
    * @group Api
    */
  trait SimpleUseTree extends UseTree {
    /**
     * The id (or the owner) of the directly enclosing tree of this
     * tree.
     *
     * SimpleTree is the only Tree that needs this field, and it is
     * because they can become part of Select, and be owned by the
     * Select tree, while geographically be inside another tree.
     */
    def enclosingId: TreeId
  }

  // Really common ASTs, I cannot imagine a compiler without them
  /**
    * A trait that represents identifiers that point to a [[TypeTree]]
    *
    * @group Api
    */
  trait TypeUse extends SimpleUseTree {
    def asString(ctx: Context): String = name(ctx)._2.asString

    def show(ctx: Context): String =
      s"""|TypeUse{
          |uses=$uses,
          |nameAtParser=$nameAtParser,
          |owner=$owner,
          |pos=$pos,
          |name=${name(ctx)._2}
          |}""".stripMargin


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
  trait Ident extends Expr with SimpleUseTree {


    def asString(ctx: Context): String = name(ctx)._2.asString
    def show(ctx: Context): String =
      s"""|Ident{
          |uses=$uses,
          |nameAtParser=$nameAtParser,
          |owner=$owner,
          |pos=$pos,
          |name=${name(ctx)._2}
          |}""".stripMargin

    // override def toString: String = name


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
    private class TypeUseImpl(val uses: TreeId,
      val nameAtParser: Option[String], val pos: Option[Position],
      val owner: TreeId, val enclosingId: TreeId) extends TypeUse

    /**
      * Creates a [[TypeUse]] instance.
      *
      * You don't need to use this constructor after parsing
      *
      * @param uses the id of the [[DefTree]] that this instance uses
      * @param nameAtParser the name of this type-use when parsed
      * @param pos the position of this tree
      * @param owner the owner of this tree
      * @return a new [[TypeUse]] instance.
      */
    def apply(uses: TreeId, nameAtParser: Option[String],
      pos: Option[Position], owner: TreeId): TypeUse =
        new TypeUseImpl(uses, nameAtParser, pos, owner, owner)

    /**
      * Creates a [[TypeUse]] instance.
      *
      * Please be reminded, you almost always need this this constructor
      * after parser phase.
      *
      * @param uses the id of the [[DefTree]] that this instance uses
      * @param pos the position of this tree
      * @param owner the owner of this tree
      * @return a new [[TypeUse]] instance.
      */
    def apply(uses: TreeId, pos: Option[Position], owner: TreeId): TypeUse =
        new TypeUseImpl(uses, None, pos, owner, owner)

    /**
      * Creates a [[TypeUse]] instance.
      *
      * Please be reminded, you almost always need this this constructor
      * after parser phase, when dealing with Select and TypeUse
      *
      * @param uses the id of the [[DefTree]] that this instance uses
      * @param pos the position of this tree
      * @param owner the owner of this tree
      * @param enclosingId the id of the enclosing tree or its owner
      * @return a new [[TypeUse]] instance.
      */
    def apply(uses: TreeId,
      pos: Option[Position], owner: TreeId, enclosingId: TreeId): TypeUse =
        new TypeUseImpl(uses, None, pos, owner, enclosingId)



    /**
      * Creates a [[TypeUse]] instance.
      *
      * You don't need to use this constructor after parsing
      *
      * @param uses the id of the [[DefTree]] that this instance uses
      * @param nameAtParser the name of this type-use when parsed
      * @param pos the position of this tree
      * @param owner the owner of this tree
      * @param enclosingId the id of the enclosing tree or its owner
      * @return a new [[TypeUse]] instance.
      */
    def apply(uses: TreeId, nameAtParser: Option[String],
      pos: Option[Position], owner: TreeId, enclosingId: TreeId): TypeUse =
        new TypeUseImpl(uses, nameAtParser, pos, owner, enclosingId)


  }

  /**
    * The factory for [[Ident]] instances
    *
    * @group Factories
    */
  trait IdentFactory {
    private class IdentImpl(val uses: TreeId,
      val nameAtParser: Option[String], val pos: Option[Position],
      val owner: TreeId, val enclosingId: TreeId) extends Ident

    /**
      * Creates a [[Ident]] instance.
      *
      * You don't need to use this constructor after parsing
      *
      * @param uses the id of the [[DefTree]] that this instance uses
      * @param nameAtParser the name of this identifier when parsed
      * @param pos the position of this tree
      * @param owner the owner of this tree
      * @param enclosingId the id of the enclosing tree or its owner
      * @return a new [[Ident]] instance.
      */
    def apply(uses: TreeId, nameAtParser: Option[String], pos: Option[Position],
      owner: TreeId, enclosingId: TreeId): Ident =
        new IdentImpl(uses, nameAtParser, pos, owner, enclosingId)

    /**
      * Creates a [[Ident]] instance.
      *
      * Please be reminded, you almost always need this this constructor
      * after parser phase, when dealing with Select and Ident.
      *
      * @param uses the id of the [[DefTree]] that this instance uses
      * @param pos the position of this tree
      * @param owner the owner of this tree
      * @param enclosingId the id of the enclosing tree or its owner
      * @return a new [[Ident]] instance.
      */
    def apply(uses: TreeId, pos: Option[Position],
      owner: TreeId, enclosingId: TreeId): Ident =
        new IdentImpl(uses, None, pos, owner, enclosingId)



    /**
      * Creates a [[Ident]] instance.
      *
      * You don't need to use this constructor after parsing
      *
      * @param uses the id of the [[DefTree]] that this instance uses
      * @param nameAtParser the name of this identifier when parsed
      * @param pos the position of this tree
      * @param owner the owner of this tree
      * @return a new [[Ident]] instance.
      */
    def apply(uses: TreeId, nameAtParser: Option[String], pos: Option[Position],
      owner: TreeId): Ident =
        new IdentImpl(uses, nameAtParser, pos, owner, owner)


    /**
      * Creates a [[Ident]] instance.
      *
      * Please be reminded, you almost always need this this constructor
      * after parser phase.
      *
      * @param uses the id of the [[DefTree]] that this instance uses
      * @param pos the position of this tree
      * @param owner the owner of this tree
      * @return a new [[Ident]] instance.
      */
    def apply(uses: TreeId, pos: Option[Position], owner: TreeId): Ident =
        new IdentImpl(uses, None, pos, owner, owner)

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
    val id: TreeId            = NoId
    val tpe: TypeState[Type]  = toTypeState(notype)
    val owner: TreeId         = NoId
    val pos: Option[Position] = None
    val name: Name            = ERROR_NAME

    def asString(ctx: Context): String = "<bad-tree>"
    def show(ctx: Context): String = "<bad-tree>"
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


  def showList(trees: List[Tree], ctx: Context): String =
    s"List{\n${trees.map(_.show(ctx)).mkString(",\n")}\n}"

  def asStringList(trees: List[Tree], ctx: Context, sep: String = ", "): String =
    s"${trees.map(_.asString(ctx)).mkString(sep)}"
}

