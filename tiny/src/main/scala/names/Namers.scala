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
import Names._

 
import scalaz.{Name => _, Failure => _, _}
import scala.language.{higherKinds,implicitConversions}
import Scalaz._

trait Namers extends passes.Phases {
  self: Trees with 
        TreeContexts with 
        Types with 
        CompilationUnits with
        MonadUtils =>

  trait Namer extends TransformerPhase {
    type Inner[A]               = WriterT[Id, Vector[Failure], A]
    type Outer[F[_], A]         = StateT[F, TreeContext, A]
    type Stacked[A]             = Outer[Inner, A]
    type NamerMonad[T <: Tree]  = Stacked[T]

    private type ST[C, A] = StateT[Inner, C, A]
    protected def point[A](t: A): Outer[Inner, A] = t.point[Stacked]
    protected def get = {
      MonadState[ST, TreeContext].get
    }
    protected def put(env: TreeContext) = {
      MonadState[ST, TreeContext].put(env)
    }
    protected def modify(f: TreeContext => TreeContext) = {
      MonadState[ST, TreeContext].modify(f)
    }

    implicit def toNamerMonad[A](x: Outer[Id, A]): Stacked[A] = x.lift[Inner]

    val name: String = "namer"
    override val description: Option[String] = 
      Some("The main namer phase, bind uses to definitions.")
    override def runRightAfter: Option[String] = Some("parser")


    def startPhase(unit: CompilationUnit): 
         (Vector[Failure], CompilationUnit) = {
      val tree  = unit.tree
      val state = unit.state
      val (w, (s, namedTree)) = named(tree).run(state).run
      (w, CompilationUnit(namedTree, s, unit.fileName))
    }
    def canRedefine: Boolean

    def named(tree: Tree): NamerMonad[Tree]
    def nameDefs(defTree: DefTree): NamerMonad[DefTree]
    def bindUses(tree: Tree): NamerMonad[Tree]

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
