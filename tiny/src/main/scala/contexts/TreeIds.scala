package ch.usi.inf.l3.sana.tiny.contexts



trait TreeId extends Any {
  def forward: TreeId
  def isComposite: Boolean
  def isSimple: Boolean
}

object TreeId {
  def apply(owner: TreeId, id: Int): TreeId = owner match {
    case NoId => new SimpleId(id)
    case _    => new CompositeId(owner, id)
  }
}



class SimpleId(val id: Int) extends AnyVal with TreeId {
  def forward: TreeId = NoId
  def isComposite: Boolean = false
  def isSimple: Boolean    = true
}


class CompositeId(val path: TreeId, val id: Int) extends TreeId {
  def forward: TreeId = path.forward match {
    case NoId      => TreeId(NoId, id)
    case p         => TreeId(p, id)
  }
  def isComposite: Boolean = true
  def isSimple: Boolean    = false
}

object NoId extends TreeId {
  def forward: TreeId = NoId
  def isComposite: Boolean = false
  def isSimple: Boolean    = false
}
