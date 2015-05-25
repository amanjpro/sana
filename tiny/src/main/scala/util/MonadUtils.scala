package ch.usi.inf.l3.sana.tiny.util

// Exclude Success and Failure in scalaz
import scalaz.{Success => _, Failure => _, _}
import scalaz.Scalaz._
import ch.usi.inf.l3.sana
import sana.tiny
import tiny.contexts.TreeContexts
import tiny.ast.Trees
import tiny.report._

trait MonadUtils {
  self: Trees with TreeContexts =>
  
  /*
   * R: Abstract type for the Reader part in ReaderWriterState
   * W: Abstract type for the Writer part in ReaderWriterState
   * S: Abstract type for the State part in ReaderWriterState
   */
  type R = List[String]
  type W = Vector[Failure]
  type S = TreeContext
  
  type RWST[V] = ReaderWriterState[R, W, S, V]

  // Move the following two functions to some Monad utility class
  protected def toRWST[A](st: State[S, A]): RWST[A] = {
    st.rwst[W, R]
  }


  def newRWST[A](f: S => (S, A)): RWST[A] = 
    ReaderWriterStateT { 
      (config: R, oldState: S) => 
        val (newState, t) = f(oldState)
        Applicative[Id].point((Vector.empty, t, newState))
    }

  def run[A](m: RWST[A], s: TreeContext,
      r: R): (W, A, S) = {
    m.run(r, s)
  }

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
  val rwst = ReaderWriterStateT.rwstMonad[Id, R, W, S]
  import rwst._
}
