package ch.usi.inf.l3.sana.tiny.report


trait Result {
  def ++(r: Result): Result
}
object Success extends Result {
  def ++(r: Result): Result = r  
}
case class Failure(msg: String) extends Result {
  def ++(r: Result): Result = r match {
    case Failure(msg2) => 
      Failure(s"${msg}\n${msg2}")  
    case _            =>
      this
  }
}

