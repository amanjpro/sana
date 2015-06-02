package ch.usi.inf.l3.sana.tiny.modifiers

class Flag(val mask: Int) extends AnyVal {
  def &(flag: Flag): Flag = Flag(flag.mask & mask)
  def |(flag: Flag): Flag = Flag(flag.mask | mask)
  def >>>(flag: Flag): Flag = Flag(flag.mask >>> mask)
  def >>(flag: Flag): Flag = Flag(flag.mask >> mask)
  def <<(flag: Flag): Flag = Flag(flag.mask << mask)
  def ^(flag: Flag): Flag = Flag(flag.mask ^ mask)
}

object Flag {
  def apply(mask: Int): Flag = new Flag(mask)
}

trait Flags {
  val mask: Flag

  def hasAnyFlags: Boolean = mask != FlagSet.NO_FLAGS

  def hasFlag(flag: Flag): Boolean =
    (mask & flag) == flag
}

private class FlagsImpl extends Flags {
  val mask: Flag = FlagSet.NO_FLAGS
}

trait FlagSet {
  val NO_FLAGS: Flag = Flag(0)
}


