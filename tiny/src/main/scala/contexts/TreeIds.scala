package ch.usi.inf.l3.sana.tiny.contexts

import ch.usi.inf.l3.sana.tiny.debug.logger


/**
 * A TreeId represents a path to a context.
 * 
 * For example for a context with the following shape:
 * {{{
 *         root (TreeId = path = _; id = 1)
 *           |
 * +--------------------------------------+
 * |     | TreeId: path = 1; id = 2  |    |
 * +--------------------------------------+
 *               |
 * +-------------------------------------------+
 * |       | TreeId: path = 1, 2; id = 3  |    |
 * +-------------------------------------------+
 * }}}
 *
 */
trait TreeId extends Any {
  /**
   * Walks the id one level forward
   *
   * For example a path like:
   * {{{
   * 1 -> 2 -> 3 -> 4 -> 5
   * }}}
   *
   * Walks to:
   * {{{
   * 2 -> 3 -> 4 -> 5
   * }}}
   */
  def forward: TreeId

  /**
   * Returns the first rout in the path
   *
   * For the following path:
   * {{{
   * 1 -> 2 -> 3 -> 4 -> 5
   * }}}
   *
   * Walks to:
   * {{{
   * 1
   * }}}
   *
   * One invariant to notice here is: 
   * {{{
   * val hd      = treeId.head
   * val tl      = treeId.forward
   * tl.merge(hd)   // Should return true
   * }}}
   */
  def head: TreeId

  /**
   * Concatenates two ids together:
   *
   * {{{
   * val tid1 = 1 -> 2 -> 3
   * val tid2 = 4 -> 5 -> 6
   * 
   * tid1.concat(tid2) // 1 -> 2 -> 3 -> 4 -> 5 -> 6
   * }}}
   */
  def concat(other: TreeId): TreeId = other match {
    case sid: SimpleId         => TreeId(this, sid.id)
    case cid: CompositeId      => TreeId(this.concat(cid.path), cid.id)
    case NoId                  => this
  }

  /**
   * Tests contains the other path
   *
   * {{{
   * val tid1 = 1 -> 2 -> 3
   * val tid2 = 1 -> 2 -> 3 -> 4 -> 5
   * 
   * tid1.contains(tid2) // true
   * tid2.contains(tid1) // false
   * }}}
   */
  def contains(other: TreeId): Boolean

  /**
   * Tests contains the other path
   *
   * {{{
   * val tid1 = 1 -> 2 -> 3
   * val tid2 = 1 -> 2 -> 3 -> 4 -> 5
   * 
   * tid2.isContainedIn(tid1) // true
   * tid1.isContainedIn(tid2) // false
   * }}}
   */

  def isContainedIn(other: TreeId): Boolean = other.contains(this)
  /**
   * Returns the path to the parent context
   *
   * For the following path:
   * {{{
   * 1 -> 2 -> 3 -> 4 -> 5
   * }}}
   *
   * Walks to:
   * {{{
   * 1 -> 2 -> 3 -> 4
   * }}}
   */
  def up: TreeId

  /**
   * Merges two paths together
   *
   * For the following two paths:
   * {{{
   * this: 2 -> 3 -> 4 -> 5
   * other: 1
   * }}}
   *
   * Walks to:
   * {{{
   * 1 -> 2 -> 3 -> 4 -> 5
   * }}}
   *
   * @param other the head of the path to be merged with this path
   */
  def merge(other: TreeId): TreeId

  /** Weather the path is simple or composite */
  def isComposite: Boolean

  /** Weather the path is simple or composite */
  def isSimple: Boolean
}

object TreeId {
  def apply(owner: TreeId, id: Long): TreeId = owner match {
    case NoId => new SimpleId(id)
    case _    => new CompositeId(owner, id)
  }


  def builtinId(id: Long): TreeId = TreeId(TreeId(NoId, -1), id)
  def isBuiltIn(id: TreeId): Boolean = id.head == TreeId(NoId, -1)
}



class SimpleId protected[contexts](val id: Long) extends AnyVal with TreeId {
  def forward: TreeId = NoId
  def head: TreeId = this
  def up: TreeId = NoId
  def contains(other: TreeId): Boolean = false
  def merge(other: TreeId): TreeId = TreeId(other, id)
  def isComposite: Boolean = false
  def isSimple: Boolean    = true
  override def toString: String = s"SimpleId($id)"
}


class CompositeId protected[contexts](val path: TreeId, 
  val id: Long) extends TreeId {
  def forward: TreeId = path.forward match {
    case NoId      => TreeId(NoId, id)
    case p         => TreeId(p, id)
  }
  def head: TreeId = path.head
  // match {
  //   case NoId                              => TreeId(NoId, id)
  //   case _                                 => path.head
  // }

  def contains(other: TreeId): Boolean = {
    lazy val r = other match {
      case _: SimpleId | NoId        => false
      case _                         => this.contains(other.up)
    }
    this == other || r
  }
  def up: TreeId = path
  def merge(other: TreeId): TreeId = other match {
    case NoId                      => this
    case _                         =>
      TreeId(path.merge(other), id)
  } 

  def isComposite: Boolean = true
  def isSimple: Boolean    = false
  override def toString: String = s"CompositId($path, $id)"


  override val hashCode = 41 * (41 + path.hashCode) + id.hashCode
  override def equals(other: Any) = {
    lazy val result = other match {
      case that: CompositeId =>
        (this.path == that.path) && (this.id == that.id)
      case _                 =>
        false
    }
    other != null && result
  }
}

case object NoId extends TreeId {
  def forward: TreeId = NoId
  def contains(other: TreeId): Boolean = false
  def head: TreeId = NoId
  def up: TreeId = NoId
  def merge(other: TreeId): TreeId = other
  def isComposite: Boolean = false
  def isSimple: Boolean    = false
  override def toString: String = "NoId"
}
