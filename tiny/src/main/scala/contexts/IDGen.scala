package ch.usi.inf.l3.sana.tiny.contexts

import java.util.concurrent.atomic.AtomicInteger

/**
  * A class to generate unique keys.
  *
  * @author Amanj Sherwany
  * @since 0.1
  * @version 0.1
  *
  * @constructor Creates a new instance of IDGen
  */
class IDGen {
  private val id: AtomicInteger = new AtomicInteger(0)

  /**
    * A this thread-safe method to generate the next `id`. 
    
    * Generated ''id''s are of type integer.
    * 
    * @return an integer value that is different from all
    *         the values that are produced so far by this
    *         instance.
    */
  def getNextId(): Int = id.getAndIncrement
}

