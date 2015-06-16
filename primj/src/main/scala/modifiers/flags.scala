package ch.usi.inf.l3.sana.primj.modifiers

import ch.usi.inf.l3.sana.tiny
import tiny.modifiers.Flag

trait Flags extends tiny.modifiers.Flags {
  val isDoWhile: Boolean       = hasFlag(FlagSet.DO_WHILE)
  val isParam:   Boolean       = hasFlag(FlagSet.PARAM)
  val isLocalVariable: Boolean = hasFlag(FlagSet.LOCAL_VARIABLE)
  val isField: Boolean         = hasFlag(FlagSet.FIELD)
  val isFinal: Boolean         = hasFlag(FlagSet.FINAL)
}

private class FlagsImpl(val mask: Flag) extends Flags

trait FlagSet {
  val DO_WHILE: Flag        = Flag(1 << 1)
  val PARAM: Flag           = Flag(1 << 2)
  val LOCAL_VARIABLE: Flag  = Flag(1 << 3)
  val FIELD: Flag           = Flag(1 << 4)
  val FINAL: Flag           = Flag(1 << 5)
}
