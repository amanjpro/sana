package ch.usi.inf.l3.sana.tiny.modifiers

trait Flags {
  val mask: Int

  def hasAnyFlags: Boolean = mask != FlagSet.NO_FLAGS

  def hasFlag(flag: Int): Boolean =
    (mask & flag) == flag
}

private class FlagsImpl extends Flags {
  val mask: Int = FlagSet.NO_FLAGS
}

trait FlagSet {
  val NO_FLAGS: Int  = 0
}


