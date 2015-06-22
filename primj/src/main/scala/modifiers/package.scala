package ch.usi.inf.l3.sana.primj



import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.modifiers.Flag

package object modifiers {

  val FlagSet = new FlagSet {}

  val noflags = tiny.modifiers.noflags


  implicit class FlagOps(mods: Flag) extends calcj.modifiers.FlagOps(mods) {
    val isDoWhile: Boolean       = hasFlag(FlagSet.DO_WHILE)
    val isParam:   Boolean       = hasFlag(FlagSet.PARAM)
    val isLocalVariable: Boolean = hasFlag(FlagSet.LOCAL_VARIABLE)
    val isField: Boolean         = hasFlag(FlagSet.FIELD)
    val isFinal: Boolean         = hasFlag(FlagSet.FINAL)


    override def ops: List[(Boolean, String)] = 
      List((isDoWhile,       "DO_WHILE"),
           (isParam,         "PARAM"),
           (isLocalVariable, "LOCAL_VARIABLE"),
           (isField,         "FIELD"),
           (isFinal,         "FINAL")) ++ super.ops
  }
}
