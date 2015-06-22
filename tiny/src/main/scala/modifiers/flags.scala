package ch.usi.inf.l3.sana.tiny.modifiers

import scala.collection.immutable.Set

trait Flag {
  def |(flag: Flag): Flags = Flags(Set(this, flag))
  def |(flags: Flags): Flags = flags | this
}
case object COMPILED extends Flag




sealed class Flags(private val flags: Set[Flag]) {
  def |(flag: Flag): Flags   = Flags(flags + flag)
  def |(other: Flags): Flags = Flags(other.flags ++ flags)

  def hasAnyFlags: Boolean = flags != Nil
  def hasNoFlags: Boolean  = !hasAnyFlags

  def hasFlag(flag: Flag): Boolean = flags.contains(flag)

  final def asString: String = flags.mkString(" | ")
}
object Flags {
  def apply(flag: Flag): Flags = new Flags(Set(flag))
  def apply(flags: Set[Flag]): Flags = new Flags(flags)

  def unapplySeq(flags: Flags): Option[Set[Flag]] = flags match {
    case null                 => None
    case flags                => Some(flags.flags)
  }
}

object NoFlags extends Flags(Set.empty[Flag])

// /**
//   * This is a Value class to represent a flag.
//   *
//   * Using this class protects us from arbitrary integer
//   * values to be assigned as a flag.
//   *
//   * @constructor creates a new Flag with the given mask value
//   * @param mask the value of the flag
//   */
// class Flag(val mask: Long) extends AnyVal {
//   /** @see [[scala.Int.&(x:Int):*]] */
//   def &(flag: Flag): Flag = Flag(flag.mask & mask)
//   /** @see [[scala.Int.|(x:Int):*]] */
//   def |(flag: Flag): Flag = Flag(flag.mask | mask)
//   // /** @see [[scala.Int.>>>(x:Int):*]] */
//   // def >>>(flag: Flag): Flag = Flag(flag.mask >>> mask)
//   // /** @see [[scala.Int.>>(x:Int):*]] */
//   // def >>(flag: Flag): Flag = Flag(flag.mask >> mask)
//   // /** @see [[scala.Int.<<(x:Int):*]] */
//   // def <<(flag: Flag): Flag = Flag(flag.mask << mask)
//   // /** @see [[scala.Int.^(x:Int):*]] */
//   // def ^(flag: Flag): Flag = Flag(flag.mask ^ mask)
// }
// /**
//   * Companion object of class Flag
//   */
// object Flag {
//   /** 
//     * A factory method for [[Flag]]
//     *
//     * @param mask the value of the flag
//     * @return a new instance of Flag with the given mask value
//     */
//   def apply(mask: Long): Flag = new Flag(mask)
// }




// trait FlagSet {
//   val NO_FLAGS: Flag = Flag(0)
//   val COMPILED: Flag = Flag(1 << 1)
// }
