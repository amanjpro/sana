package ch.usi.inf.l3.sana.tiny




package object modifiers {
  val FlagSet = new FlagSet {}
  val noflags: Flag = Flag(0)


  implicit class FlagOps(mask: Flag) {

    def hasAnyFlags: Boolean = mask != FlagSet.NO_FLAGS
    def hasNoFlags: Boolean  = !hasAnyFlags
    val isCompiled: Boolean  = hasFlag(FlagSet.COMPILED)

    def hasFlag(flag: Flag): Boolean =
      (mask & flag) == flag

    def asString: String = "IMPLEMENT ME"
  }
}
