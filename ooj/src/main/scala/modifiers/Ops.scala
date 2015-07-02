package ch.usi.inf.l3.sana.ooj.modifiers



import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.modifiers.Flags

object Ops {


  val noflags = tiny.modifiers.Ops.noflags


  implicit class FlagOps(mask: Flags)
      extends primj.modifiers.Ops.FlagOps(mask) {

    val isPublicAcc: Boolean     = mask.hasFlag(ACC_PUBLIC)
    val isPrivateAcc: Boolean    = mask.hasFlag(ACC_PRIVATE)
    val isProtectedAcc: Boolean  = mask.hasFlag(ACC_PROTECTED)
    val isPackageAcc: Boolean    = mask.hasFlag(ACC_PACKAGE)
    val isStatic: Boolean        = mask.hasFlag(STATIC)
    val isInterface: Boolean     = mask.hasFlag(INTERFACE)
    val isAbstract: Boolean      = mask.hasFlag(ABSTRACT)
    val isConstructor: Boolean   = mask.hasFlag(CONSTRUCTOR)
  }
}
