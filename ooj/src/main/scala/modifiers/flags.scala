package ch.usi.inf.l3.sana.ooj.modifiers

import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.modifiers.Flag





trait FlagSet extends primj.modifiers.FlagSet {
  val ACC_PUBLIC: Flag      = Flag(1 << 8)
  val ACC_PROTECTED: Flag   = Flag(1 << 9)
  val ACC_PRIVATE: Flag     = Flag(1 << 10)
  val ACC_PACKAGE: Flag     = Flag(1 << 11)
  val STATIC: Flag          = Flag(1 << 12)
  val ABSTRACT: Flag        = Flag(1 << 13)
  val INTERFACE: Flag       = Flag(1 << 14)
  val CONSTRUCTOR: Flag     = Flag(1 << 15)
}
