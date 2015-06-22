package ch.usi.inf.l3.sana.tiny.modifiers

/**
  * This is a Value class to represent a flag.
  *
  * Using this class protects us from arbitrary integer
  * values to be assigned as a flag.
  *
  * @constructor creates a new Flag with the given mask value
  * @param mask the value of the flag
  */
class Flag(val mask: Long) extends AnyVal {
  /** @see [[scala.Int.&(x:Int):*]] */
  def &(flag: Flag): Flag = Flag(flag.mask & mask)
  /** @see [[scala.Int.|(x:Int):*]] */
  def |(flag: Flag): Flag = Flag(flag.mask | mask)
  // /** @see [[scala.Int.>>>(x:Int):*]] */
  // def >>>(flag: Flag): Flag = Flag(flag.mask >>> mask)
  // /** @see [[scala.Int.>>(x:Int):*]] */
  // def >>(flag: Flag): Flag = Flag(flag.mask >> mask)
  // /** @see [[scala.Int.<<(x:Int):*]] */
  // def <<(flag: Flag): Flag = Flag(flag.mask << mask)
  // /** @see [[scala.Int.^(x:Int):*]] */
  // def ^(flag: Flag): Flag = Flag(flag.mask ^ mask)
}
/**
  * Companion object of class Flag
  */
object Flag {
  /** 
    * A factory method for [[Flag]]
    *
    * @param mask the value of the flag
    * @return a new instance of Flag with the given mask value
    */
  def apply(mask: Long): Flag = new Flag(mask)
}




trait FlagSet {
  val NO_FLAGS: Flag = Flag(0)
  val COMPILED: Flag = Flag(1 << 1)
}
