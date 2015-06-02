package ch.usi.inf.l3.sana.tiny.util

import ch.usi.inf.l3.sana
import sana.tiny
import tiny.types.Types
import tiny.contexts.{TreeContexts,TreeId}
import tiny.ast.Trees
import tiny.report._

// Exclude Success and Failure in scalaz
import scalaz.{Name => _, Failure => _, _}
import scalaz.Scalaz._

import scala.language.{higherKinds,implicitConversions}

trait MonadUtils {
  self: Trees with TreeContexts with Types =>
  
  type CompilerErrorMonad[A]      = WriterT[Id, Vector[Failure], A]
  type ContextStateT[F[_], A]     = StateT[F, TreeContext, A]
  type TreeIdReader[A]            = ReaderT[Id, Option[TreeId], A]
  private type SW[C, A]           = StateT[CompilerErrorMonad, C, A]


  type ContextState[A]            = ContextStateT[Id, A]
  type StateWriter[A]             = ContextStateT[CompilerErrorMonad, A]
  type StateReader[A]             = ContextStateT[TreeIdReader, A]


  implicit def toStateReader[A](x: ContextStateT[Id, A]): StateReader[A] = 
    x.lift[TreeIdReader]
  implicit def toStateReader[A](x: TreeIdReader[A]): StateReader[A] =
    x.liftM[ContextStateT]
  implicit def outer2Inner[A](x: StateReader[A]): TreeIdReader[A] = x.lift[Id]


  implicit def toStateWriter[A](x: ContextStateT[Id, A]): StateWriter[A] =
    x.lift[CompilerErrorMonad]
  implicit def toStateWriter[A](x: CompilerErrorMonad[A]): StateWriter[A] = 
    x.liftM[ContextStateT]

  implicit def pointSW[A](t: A): StateWriter[A] = 
    Monad[StateWriter].point(t)
  implicit def pointSR[A](t: A): ContextStateT[TreeIdReader, A] =
      t.point[StateReader]
  def askSR: StateReader[Option[TreeId]] = 
    MonadReader[Reader, Option[TreeId]].ask.liftM[ContextStateT]
  def localSR[A](f: Option[TreeId] => Option[TreeId])
      (fa: TreeIdReader[A]): StateReader[A] = {
    val r = MonadReader[Reader, Option[TreeId]].local(f)(fa)
    r.liftM[ContextStateT]
  }

  def getSW: StateWriter[TreeContext] = 
    MonadState[SW, TreeContext].get
  def putSW(env: TreeContext): StateWriter[Unit] = 
    MonadState[SW, TreeContext].put(env)
  def modifySW(f: TreeContext => TreeContext): StateWriter[Unit] = 
    MonadState[SW, TreeContext].modify(f)
    

  // implicit def tree2Writer[T](t: T): Stacked[T] =
      // point(t)


  // // TODO: This trait is not needed at all, we can get rid of it completely
  // // (at least so far)
  // /*
  //  * R: Abstract type for the Reader part in ReaderWriterState
  //  * W: Abstract type for the Writer part in ReaderWriterState
  //  * S: Abstract type for the State part in ReaderWriterState
  //  */
  // type R = List[String]
  // type W = Vector[Failure]
  // type S = TreeContext
  //
  // type RWST[V] = ReaderWriterState[R, W, S, V]
  //
  // // Move the following two functions to some Monad utility class
  // protected def toRWST[A](st: State[S, A]): RWST[A] = {
  //   st.rwst[W, R]
  // }
  //
  //
  // def newRWST[A](f: S => (S, A)): RWST[A] = 
  //   ReaderWriterStateT { 
  //     (config: R, oldState: S) => 
  //       val (newState, t) = f(oldState)
  //       Applicative[Id].point((Vector.empty, t, newState))
  //   }
  //
  // def run[A](m: RWST[A], s: TreeContext,
  //     r: R): (W, A, S) = {
  //   m.run(r, s)
  // }
  //
  // // point is equivalent to ``pure'' and ``return'' in Haskell
  // // def point[A](t: A): RWST[A] = t.point[RWST]
  //
  //
  // // Stolen from: http://git.io/vf4L6
  // // Bring the RWST syntax into scope.
  // // This will bring into scope methods which return things of type RWST[_].
  // // example syntax methods:
  // // get    -- gets the current state
  // // put    -- replaces the current state
  // // modify -- alter the current state
  // // tell   -- append to the writer
  // // ask    -- read from the reader
  // val rwst = ReaderWriterStateT.rwstMonad[Id, R, W, S]
  // import rwst._
}
