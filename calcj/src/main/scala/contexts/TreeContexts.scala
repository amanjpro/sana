package ch.usi.inf.l3.sana.calcj.contexts

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.ast.Trees
import tiny.report._
import tiny.names.Name
import tiny.types.Types
import tiny.contexts.{IDGen, TreeId, NoId}
import calcj.util.Definitions


/**
 * A trait that contains types that represent context of a tree in  the
 * `calcj` language.
 *
 *
 * @author Amanj Sherwany
 * @since 0.1
 * @version 0.1
 */
trait TreeContexts extends tiny.contexts.TreeContexts {
  self: TreeInfos with Trees with Types with Definitions =>

  
  def rootContext: RootContext = 
    new RootContextImpl(new IDGen, Map.empty, langDefinitions)

}




