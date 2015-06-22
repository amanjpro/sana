package ch.usi.inf.l3.sana.calcj.modifiers



import ch.usi.inf.l3.sana.tiny

object Ops {

  val noflags = tiny.modifiers.Ops.noflags
  
  implicit class FlagOps(mask: tiny.modifiers.Flags) 
      extends tiny.modifiers.Ops.FlagOps(mask) {
    def isPostfix: Boolean = mask.hasFlag(POSTFIX)
  }
}
