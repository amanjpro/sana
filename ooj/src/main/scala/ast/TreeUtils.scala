package ch.usi.inf.l3.sana.ooj.ast

import ch.usi.inf.l3.sana
import sana.brokenj
import brokenj.ast


trait TreeUtils extends ast.TreeUtils {
  self: Trees =>

  override def pointsToUse(tree: Tree,
            p: UseTree => Boolean): Boolean = tree match {
    case id: Ident         => p(id)
    case tuse: TypeUse     => p(tuse)
    case slct: Select      => pointsToUse(slct.tree, p)
    case _                 =>
      false
  }

}

