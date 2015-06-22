package ch.usi.inf.l3.sana.primj.modifiers



import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.modifiers.Flags

object Ops {

  val noflags = tiny.modifiers.Ops.noflags


  implicit class FlagOps(mask: Flags) 
      extends calcj.modifiers.Ops.FlagOps(mask) {
    val isDoWhile: Boolean       = mask.hasFlag(DO_WHILE)
    val isParam:   Boolean       = mask.hasFlag(PARAM)
    val isLocalVariable: Boolean = mask.hasFlag(LOCAL_VARIABLE)
    val isField: Boolean         = mask.hasFlag(FIELD)
    val isFinal: Boolean         = mask.hasFlag(FINAL)
  }
}
