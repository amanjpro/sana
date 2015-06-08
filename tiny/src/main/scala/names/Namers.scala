package ch.usi.inf.l3.sana.tiny.names

import ch.usi.inf.l3.sana.tiny
import tiny.source.Position
import tiny.contexts.TreeContexts
import tiny.util.{CompilationUnits, MonadUtils}
import tiny.contexts.TreeId
import tiny.ast.Trees
import tiny.types.Types
import tiny.report._
import tiny.passes

 
import scalaz.{Name => _, Failure => _, _}
import scala.language.higherKinds
import Scalaz._

trait Namers extends passes.Phases {

  import global._

  type NamerMonad[T <: Tree]  = StateWriter[T]

  def toNamerMonad[A](x: ContextState[A]): StateWriter[A] =
    toStateWriter(x)
  def toNamerMonad[A](x: ErrorReportingMonad[A]): StateWriter[A] =
    toStateWriter(x)
  trait Namer extends TransformerPhase {
    val name: String = "namer"
    override val description: Option[String] = 
      Some("The main namer phase, bind uses to definitions.")
    override def runRightAfter: Option[String] = Some("id-assigner")


    def startPhase(state: Context, unit: CompilationUnit): 
         (Vector[Report], CompilationUnit, Context) = {
      val tree  = unit.tree
      val (w, (s, namedTree)) = nameTrees(tree).run(state).run
      (w, CompilationUnit(unit.id, namedTree, unit.fileName), s)
    }

    def nameTrees(tree: Tree): NamerMonad[Tree]
    // def nameDefs(defTree: DefTree): NamerMonad[DefTree]
    // def bindUses(tree: Tree): NamerMonad[Tree]
    //
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
    
    // def gensym(pre: String): State[Int, String] = for {
    //   i <- get
    //   _ <- put(i+1)
    // } yield pre + i.toString
  }
}
