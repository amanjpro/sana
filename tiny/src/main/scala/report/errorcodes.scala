package ch.usi.inf.l3.sana.tiny.report


trait ErrorCode {
  def message: String
  lazy val code: String = this.toString
}
case object BAD_STATEMENT extends ErrorCode {
  val message: String = "Unexpected expressions here"
}
case object TYPE_MISMATCH extends ErrorCode {
  val message: String = "Type mismatch"
}
case object UNEXPETED_TREE extends ErrorCode {
  val message: String = "Unexpected tree"
}


