package ch.usi.inf.l3.sana.primj.modifiers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.calcj
import tiny.modifiers.Flag





trait FlagSet extends calcj.modifiers.FlagSet {
  val DO_WHILE: Flag        = Flag(1 << 3)
  val PARAM: Flag           = Flag(1 << 4)
  val LOCAL_VARIABLE: Flag  = Flag(1 << 5)
  val FIELD: Flag           = Flag(1 << 6)
  val FINAL: Flag           = Flag(1 << 7)
}

