package ch.usi.inf.l3.sana.ooj



import ch.usi.inf.l3.sana
import sana.tiny
import sana.primj
import tiny.modifiers.Flag

package object modifiers {

  val FlagSet = new FlagSet {}

  val noflags = tiny.modifiers.noflags


  implicit class FlagOps(mods: Flag) extends primj.modifiers.FlagOps(mods) {
    
    val isPublicAcc: Boolean     = hasFlag(FlagSet.ACC_PUBLIC)
    val isPrivateAcc: Boolean    = hasFlag(FlagSet.ACC_PRIVATE)
    val isProtectedAcc: Boolean  = hasFlag(FlagSet.ACC_PROTECTED)
    val isPackageAcc: Boolean    = hasFlag(FlagSet.ACC_PACKAGE)
    val isStatic: Boolean        = hasFlag(FlagSet.STATIC)
    val isInterface: Boolean     = hasFlag(FlagSet.INTERFACE)
    val isAbstract: Boolean      = hasFlag(FlagSet.ABSTRACT)
    val isConstructor: Boolean   = hasFlag(FlagSet.CONSTRUCTOR)


    override def ops: List[(Boolean, String)] = 
      List((isPublicAcc,      "ACC_PUBLIC"),
           (isPrivateAcc,     "ACC_PRIVATE"),
           (isProtectedAcc,   "ACC_PROTECTED"),
           (isPackageAcc,     "ACC_PACKAGE"),
           (isStatic,         "STATIC"),
           (isInterface,      "INTERFACE"),
           (isAbstract,       "ABSTRACT"),
           (isConstructor,    "CONSTRUCTOR")) ++ super.ops

  }
}
