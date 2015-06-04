package ch.usi.inf.l3.sana.tiny.util

import ch.usi.inf.l3.sana
import sana.tiny
import tiny.types.Types
import tiny.contexts.{TreeContexts,TreeId}
import tiny.report._

import scalaz._
import scalaz.Scalaz._

import scala.language.{higherKinds,implicitConversions}

/*!!! CAUTION: This trait is meant to blow your mind! It heavily employs
types.

Google "Type Lambdas in Scala"
*/
trait MonadUtils {
  self: TreeContexts =>
  
  /*
    INFO:
    Type Lambdas in Scala are not as useful as it seems. It is basically 
    broken on the type system level. One cannot mix implicit conversion
    type lambdas (that is why you cannot make use of this feature with
    Scalaz which heavily uses implicit.

    For more information see this bug:
      https://issues.scala-lang.org/browse/SI-2712


    For now, you should create new state-reader monads using StateReaderFactory.

    Until the bugs gets fixed, we mark StateReader as private, and 
    StateReaderFactory as public
  */
  class StateReaderFactory[μ] {
    type ρ[α] = Reader[μ, α]
    type λ[α] = ContextStateT[ρ, α]
    type StateReader[α] = λ[α]
  }

  // private[this] def reader[P] = new RF[P]()

  type ErrorReportingMonad[A]     = WriterT[Id, Vector[Report], A]
  type ContextStateT[F[_], A]     = StateT[F, TreeContext, A]

  type ContextState[A]            = ContextStateT[Id, A]
  type StateWriter[A]             = ContextStateT[ErrorReportingMonad, A]

  // type RD[A] = Reader[Option[TreeId], A]
  // type OwnerAssignerMonad[T] = StateT[RD, TreeContext, T]
  private type StateReader[R, A]          = StateReaderFactory[R]#λ[A]

    
  //   trait[R] RF{
  //   type τ[a] = Reader[R, a]
  //   type λ   = ContextStateT[τ, A]
  // })#λ
  //
    // StateT[({type l[a] = Reader[R, a]})#l, TreeContext, A]

  // type `SanaReader[R]`[A]         = Reader[R, A]
  // type StateReader[R, A]          = 
    // StateT[`SanaReader[R]`, TreeContext, A]

  def toStateReader[A, R](x: ContextStateT[Id, A]): StateReader[R, A] = {
    type RD[a] = Reader[R, a]
    x.lift[RD]
  }

  def toStateReader[R, A](x: Reader[R, A]): StateReader[R, A] =
    x.liftM[ContextStateT]


  def toStateWriter[A](x: ContextStateT[Id, A]): StateWriter[A] =
    x.lift[ErrorReportingMonad]

  def toStateWriter[A](x: ErrorReportingMonad[A]): StateWriter[A] = 
    x.liftM[ContextStateT]

  def pointSW[A](t: A): StateWriter[A] = 
    Monad[StateWriter].point(t)

  def pointSR[R, A](t: A): StateReader[R, A] = {
    type RD[a] = Reader[R, a]
    type SR[a] = ContextStateT[RD, a]
    Monad[SR].point(t)
  }
                              
  def askSR[R]: StateReader[R, R] = 
    MonadReader[Reader, R].ask.liftM[ContextStateT]

  def localSR[R, RT <: R, A](f: R => RT)(fa: Reader[R, A]): 
      StateReader[R, A] = {
    val r = MonadReader[Reader, R].local(f)(fa)
    r.liftM[ContextStateT]
  }
  

  private type SW[C, A]           = StateT[ErrorReportingMonad, C, A]
  def getSW: StateWriter[TreeContext] = 
    MonadState[SW, TreeContext].get

  def putSW(env: TreeContext): StateWriter[Unit] = 
    MonadState[SW, TreeContext].put(env)

  def modifySW(f: TreeContext => TreeContext): StateWriter[Unit] = 
    MonadState[SW, TreeContext].modify(f)
    

  implicit def stateR2R[R, A](x: StateReader[R, A]): Reader[R, A] =
    x.lift[Id]

  implicit def stateW2W[A](x: StateWriter[A]): Writer[Vector[Report], A] =
    x.lift[Id]


  class Dummy[A](dummy: A)
  // implicit val dummyInt: Dummy[Int] = new Dummy(1)
  // implicit val dummyId: Dummy[Option[TreeId]] = new Dummy(None)

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
  // type W = Vector[Report]
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
