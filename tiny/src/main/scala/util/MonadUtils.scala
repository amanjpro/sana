package ch.usi.inf.l3.sana.tiny.util

import ch.usi.inf.l3.sana
import sana.tiny
import tiny.types.Types
import tiny.contexts.{TreeContexts,TreeId}
import tiny.report._

import scalaz._
import scalaz.Scalaz._
import scalaz.Kleisli._

import scala.language.{higherKinds,implicitConversions}

/*!!! CAUTION: This trait is meant to blow your mind! It heavily employs
types.

Google "Type Lambdas in Scala"
*/
trait MonadUtils {
  self: TreeContexts =>
  
  

  // private[this] def reader[P] = new RF[P]()

  type ErrorReportingMonad[A]     = WriterT[Id, Vector[Report], A]
  type ContextStateT[F[_], A]     = StateT[F, Context, A]

  type ContextState[A]            = ContextStateT[Id, A]
  type StateWriter[A]             = ContextStateT[ErrorReportingMonad, A]


  type ReaderWriter[R, A]  = ReaderT[ErrorReportingMonad, R, A]


  def tellRW[R](x: Vector[Report]): ReaderWriter[R, Unit] = 
    toReaderWriter(x.tell)

  def askRW[R]: ReaderWriter[R, R] = 
    MonadReader[Reader, R].ask.lift[ErrorReportingMonad]

  def localRW[R, A](f: R => R)(fa: ReaderWriter[R, A]):
      ReaderWriter[R, A] = {
    // type RD[f[_], a] = ReaderT[f, R, a]
    // val r = MonadReader[Reader, R].local(f)(fa.liftReader[R])
    // r.lift[ErrorReportingMonad]
    // toReaderWriter(r)
    // r

    Kleisli[ErrorReportingMonad, R, A] {
      r => fa.run(f(r))
    }
  }

  def pointRW[R, A](t: A): ReaderWriter[R, A] = {
    type RW[a] = ReaderWriter[R, a]
    Monad[RW].point(t)
  }


  

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
  class StateReaderFactory[μ]() {
    type ρ[α] = Reader[μ, α]
    type λ[α] = ContextStateT[ρ, α]
    type StateReader[α] = λ[α]
  }


  // type RD[A] = Reader[Option[TreeId], A]
  // type OwnerAssignerMonad[T] = StateT[RD, Context, T]
  private type StateReader[R, A]          = StateReaderFactory[R]#λ[A]

    
  //   trait[R] RF{
  //   type τ[a] = Reader[R, a]
  //   type λ   = ContextStateT[τ, A]
  // })#λ
  //
    // StateT[({type l[a] = Reader[R, a]})#l, Context, A]

  // type `SanaReader[R]`[A]         = Reader[R, A]
  // type StateReader[R, A]          = 
    // StateT[`SanaReader[R]`, Context, A]

  def toStateReader[A, R](x: ContextStateT[Id, A]): StateReader[R, A] = {
    type RD[a] = Reader[R, a]
    x.lift[RD]
  }

  def toStateReader[R, A](x: Reader[R, A]): StateReader[R, A] =
    x.liftM[ContextStateT]

  def toReaderWriter[R, A](x: Reader[R, A]): ReaderWriter[R, A] = {
    x.lift[ErrorReportingMonad]
  }

  def toReaderWriter[R, A](x: 
    ErrorReportingMonad[A]): ReaderWriter[R, A] = {
    type RW[f[_], a] = ReaderT[f, R, a]
    x.liftM[RW]
  }



  def toStateWriter[A](x: ContextStateT[Id, A]): StateWriter[A] =
    x.lift[ErrorReportingMonad]

  def toStateWriter[A](x: ErrorReportingMonad[A]): StateWriter[A] = 
    x.liftM[ContextStateT]

  def pointSW[A](t: A): StateWriter[A] = 
    Monad[StateWriter].point(t)

  // def point[R, A](t: A)
  //   (implicit factory: StateReaderFactory[R]): factory.StateReader[A] = {
  //   Monad[factory.StateReader].point(t)
  // }
  //                             
  // def ask[R](implicit factory: StateReaderFactory[R]): 
  //     factory.StateReader[R] = 
  //   MonadReader[Reader, R].ask.liftM[ContextStateT]
  //
  // def local[R, A](f: R => R)(fa: StateReader[R, A])(
  //   implicit factory: StateReaderFactory[R]): 
  //     factory.StateReader[A] = {
  //   // val factory = new StateReaderFactory[R]
  //   // StateReader((r, s) => fa.run(f(r), s))
  //   // val r = MonadReader[Reader, R].local(f)(fa)
  //   // r.liftM[ContextStateT]
  //   ???
  // }
  //
  // def get[R](implicit factory: StateReaderFactory[R]): 
  //     factory.StateReader[Context] = {
  //   type RD[a] = Reader[R, a]
  //   type SR[c, a] = StateT[RD, c, a]
  //   MonadState[SR, Context].get
  // }
  //
  // def put[R](env: Context)
  //   (implicit factory: StateReaderFactory[R]): factory.StateReader[Unit] = {
  //   type RD[a] = Reader[R, a]
  //   type SR[c, a] = StateT[RD, c, a]
  //   MonadState[SR, Context].put(env)
  // }
  //
  // def modify[R](f: Context => Context)
  //   (implicit factory: StateReaderFactory[R]): factory.StateReader[Unit] = {
  //   type RD[a] = Reader[R, a]
  //   type SR[c, a] = StateT[RD, c, a]
  //   MonadState[SR, Context].modify(f)
  // }


  private type SW[C, A]           = StateT[ErrorReportingMonad, C, A]
  def getSW: StateWriter[Context] = 
    MonadState[SW, Context].get

  def putSW(env: Context): StateWriter[Unit] = 
    MonadState[SW, Context].put(env)

  def modifySW(f: Context => Context): StateWriter[Unit] = 
    MonadState[SW, Context].modify(f)
    

  def const[T, B](a: T)(b: B): T = a

  // implicit def stateR2R[R, A](x: StateReaderFactory[R]#StateReader[A]): Reader[R, A] = {
    // type RD[a] = Reader[R, a]
    // x.lift[RD]
    // x.liftM[ContextStateT]
  // }
    // x.liftM[ContextStateT]
    // x.lift[Id]

  // implicit def stateW2W[A](x: StateWriter[A]): Writer[Vector[Report], A] =
    // x.lift[Id]


  // class Dummy[A](dummy: A)
  // implicit val dummyInt: Dummy[Int] = new Dummy(1)
  // implicit val dummyId: Dummy[Option[TreeId]] = new Dummy(None)

  // implicit def tree2Writer[T](t: T): Stacked[T] =
      // point(t)


  // TODO: This trait is not needed at all, we can get rid of it completely
  // (at least so far)
  // /*
  //  * R: Abstract type for the Reader part in ReaderWriterState
  //  * W: Abstract type for the Writer part in ReaderWriterState
  //  * S: Abstract type for the State part in ReaderWriterState
  //  */
  // type R = List[String]
  // type W = Vector[Report]
  // type S = Context

  type RWST[R, A] = ReaderWriterState[R, Vector[Report], Context, A]

  // Move the following two functions to some Monad utility class
  implicit def toRWST[A, R](st: StateT[Id, Context, A]): RWST[R, A] = {
    st.rwst[Vector[Report], R]
  }

  implicit def toRWST[A, R](wm: ErrorReportingMonad[A]): RWST[R, A] = {
    ReaderWriterState[R, Vector[Report], Context, A] ((r, s) => {
      val (ws, a) = wm.run
      (ws, a, s)
    })
    // type M[F[_], a] = WriterT[F, Vector[Report], a]
    // ReaderWriterState.liftM[M]
    // wm.liftM[M](wm)
    //[Context, R]
  }


  // implicit def toRWST[A, R](st: StateT[Id, Context, A]): RWST[R, A] = {
  //   st.rwst[Vector[Report], R]
  // }

  // def point[A, R](a: => A): RWST[R, A] = {
  //   Monad[RWST].point(f)
  // }
    // Applicative[Id].point((Vector.empty, t, newState))

  // def run[A](m: RWST[A], s: Context,
  //     r: R): (W, A, S) = {
  //   m.run(r, s)
  // }

  // point is equivalent to ``pure'' and ``return'' in Haskell
  // def point[A](t: A): RWST[A] = t.point[RWST]


  // Stolen from: http://git.io/vf4L6
  // Bring the RWST syntax into scope.
  // This will bring into scope methods which return things of type RWST[_].
  // example syntax methods:
  // get    -- gets the current state
  // put    -- replaces the current state
  // modify -- alter the current state
  // tell   -- append to the writer
  // ask    -- read from the reader
  // local  -- locally run the reader
  def RWST[R] = ReaderWriterStateT.rwstMonad[Id, R, Vector[Report], Context]

  def local[A, R](f: R => R)(fa: RWST[R, A]): RWST[R, A] =
    ReaderWriterStateT((r, s) => fa.run(f(r), s))

  // def mapM[A,M[_]:Monad](xs:List[A])(f:A=>M[A]) : M[List[A]] =
    // xs.map(f).sequence 
}
