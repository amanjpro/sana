package ch.usi.inf.l3.sana.primj




package object modifiers {
  val FlagSet = new FlagSet {}

  val NoFlags = ch.usi.inf.l3.sana.tiny.modifiers.NoFlags

  def Flags(flags: Int): Flags = new FlagsImpl(flags)
}
