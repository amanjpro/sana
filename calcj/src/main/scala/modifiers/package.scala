package ch.usi.inf.l3.sana.calcj



import ch.usi.inf.l3.sana.tiny
import tiny.modifiers.Flag

package object modifiers {

  val FlagSet = new FlagSet {}

  val noflags = tiny.modifiers.noflags


  implicit class FlagOps(mods: Flag) extends tiny.modifiers.FlagOps(mods) {
    val isPostfix: Boolean       = hasFlag(FlagSet.POSTFIX)
  }
}
