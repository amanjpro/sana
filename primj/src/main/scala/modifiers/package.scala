package ch.usi.inf.l3.sana.primj



import ch.usi.inf.l3.sana.tiny.modifiers.Flag

package object modifiers {
  val FlagSet = new FlagSet {}

  val noflags = Flags(Flag(0))

  def Flags(flags: Flag): Flags = new FlagsImpl(flags)
}
