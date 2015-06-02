package ch.usi.inf.l3.sana.primj.modifiers

import ch.usi.inf.l3.sana.tiny
import tiny.modifiers.Flag

trait Flags extends tiny.modifiers.Flags {
  val isDoWhile: Boolean = hasFlag(FlagSet.DO_WHILE)
}

private class FlagsImpl(val mask: Flag) extends Flags {
}

trait FlagSet {
  val DO_WHILE: Flag = Flag(0 << 1)
}
