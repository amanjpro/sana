package ch.usi.inf.l3.sana.primj



import ch.usi.inf.l3.sana.tiny
import tiny.modifiers.Flag

package object modifiers {

  val FlagSet = new FlagSet {}

  val noflags = tiny.modifiers.noflags


  implicit class FlagOps(mods: Flag) extends tiny.modifiers.FlagOps(mods) {
    val isDoWhile: Boolean       = hasFlag(FlagSet.DO_WHILE)
    val isParam:   Boolean       = hasFlag(FlagSet.PARAM)
    val isLocalVariable: Boolean = hasFlag(FlagSet.LOCAL_VARIABLE)
    val isField: Boolean         = hasFlag(FlagSet.FIELD)
    val isFinal: Boolean         = hasFlag(FlagSet.FINAL)
  }
}
