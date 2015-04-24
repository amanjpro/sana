package ch.usi.inf.l3.sana.tiny.util

// Exclude Success and Failure in scalaz
import scalaz.{Success => _, Failure => _, _}
import scalaz.Scalaz._
import ch.usi.inf.l3.sana
import sana.tiny
import tiny.report._

trait CompilerMonad {
  // trait StateMonad [S, +T] extends Function1[S, (T, S)] {
  //   outer =>
  //
  //   def apply(tc: S): (T, S)
  //
  //
  //   def get: StateMonad[S, S] = new StateMonad[S, S] {
  //       def apply(s: S): (S, S) = (s, s)
  //     }
  //
  //   def put[A](newState: A): StateMonad[A, Unit] = new StateMonad[A, Unit] {
  //       def apply(st: A): (Unit, A) = ((), st)
  //     }
  //
  //
  //   def map[B >: T](f: T => B): StateMonad[S, B] = {
  //     new StateMonad[S, B] {
  //       def apply(st: S): (B, S) = {
  //         val (t, s) = outer(st)
  //         (f(t), s)
  //       }
  //     }
  //   }
  //
  //   def flatMap[B](f: T => StateMonad[S, B]): StateMonad[S, B] = {
  //     new StateMonad[S, B] {
  //       def apply(givenContext: S): (B, S) = {
  //         val (tree, newContext) = outer(givenContext)
  //         val newSt = f(tree)
  //         newSt(newContext)
  //       }
  //     }
  //   }
  // }


  /**
   * Abstract type for the Reader part in ReaderWriterState
   */
  type R
  /**
   * Abstract type for the Writer part in ReaderWriterState
   */
  type W 
  /**
   * Abstract type for the State part in ReaderWriterState
   */
  type S

  // Stolen from: http://git.io/vf4L6
  // Bring the RWST syntax into scope.
  // This will bring into scope methods which return things of type RWST[_].
  // example syntax methods:
  // get    -- gets the current state
  // put    -- replaces the current state
  // modify -- alter the current state
  // tell   -- append to the writer
  // ask    -- read from the reader
  val rwst = ReaderWriterStateT.rwstMonad[Id, R, List[W], S]
  import rwst._


  // point is equivalent to ``pure'' and ``return'' in Haskell
  type RWST[V] = ReaderWriterState[R, List[W], S, V]

  // def compiler(n: Int, st: Compiler): Compiler = {
  //   for {
  //     env <- ask
  //     env1 <- get
  //     _ <- put(env)
  //     _ <- tell(List(Success))
  //   } yield None
  // }
}
