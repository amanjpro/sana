package ch.usi.inf.l3.sana.tiny.contexts

import java.util.concurrent.atomic.AtomicInteger

class IDGen {
  private val id: AtomicInteger = new AtomicInteger(0)

  def getNextId: Int = id.getAndIncrement
}

