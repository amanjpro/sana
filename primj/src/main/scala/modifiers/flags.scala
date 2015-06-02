package ch.usi.inf.l3.sana.primj.modifiers

import ch.usi.inf.l3.sana.tiny

trait Flags extends tiny.modifiers.Flags {
  val isDoWhile: Boolean = hasFlag(FlagSet.DO_WHILE)
}

private class FlagsImpl(val mask: Int) extends Flags {
}

trait FlagSet {
  val DO_WHILE: Int  = 0 << 1
}
