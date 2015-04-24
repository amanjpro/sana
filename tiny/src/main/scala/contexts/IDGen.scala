package ch.usi.inf.l3.sana.tiny.contexts


class IDGen {
  private var id: Int = -1

  def getNextId: Int = {
    this.synchronized {
      id = id + 1
      id
    }
  }
}

