package ch.usi.inf.l3.sana.tiny.names

import ch.usi.inf.l3.sana.tiny
import tiny.source.Position
import tiny.contexts.TreeContexts
import tiny.util.CompilationUnits
import tiny.contexts.TreeId
import tiny.ast.Trees
import tiny.types.Types
import tiny.report._
import tiny.passes
import Names._

 
import scalaz.{Name => _, Failure => _, _}
import Scalaz._

trait Namers extends passes.Phases {
  self: Trees with TreeContexts with Types with CompilationUnits =>

  trait Namer extends TransformerPhase {
    val name: String = "namer"
    override val description: Option[String] = 
      Some("The main type-checking phase.")
    override def runRightAfter: Option[String] = Some("parser")


    def startPhase(unit: CompilationUnit): 
         (Vector[Failure], CompilationUnit) = {
      val tree  = unit.tree
      val state = unit.state
      // val (w, typedTree, s) = run(typeTree(tree), state, Nil)
      // (w, CompilationUnit(typedTree, s, unit.fileName))
      ???
    }
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
