package ch.usi.inf.l3.sana.tiny.names

import ch.usi.inf.l3.sana.tiny
import tiny.source.Position
import tiny.contexts.TreeContexts
import tiny.contexts.TreeId
import tiny.ast.Trees
import Names._
import tiny.types._

 
import scalaz.{Name => _, _}
import Scalaz._

trait Namers {
  self: Trees with TreeContexts =>

  trait Namer {

    def canRedefine: Boolean

    // def bind(id: TreeId, tree: IdentifiedTree): TreeState[IdentifiedTree] =
    //   for {
    //     env    <- compiler.rwst.get
    //     env2   <- if(env.defines(id) == false || canRedefine) {
    //                 val unit2 = env.unit(id.unitId).extend(id, tree)
    //                 point(env.update(id.unitId, unit2))
    //               } else {
    //                 point(env)
    //               }
    //     _      <- compiler.rwst.put(env2)
    //   } yield tree
    //
    
    def gensym(pre: String): State[Int, String] = for {
      i <- get
      _ <- put(i+1)
    } yield pre + i.toString
  }
}
