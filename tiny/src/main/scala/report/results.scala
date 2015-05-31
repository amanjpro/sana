package ch.usi.inf.l3.sana.tiny.report


// trait Result {
//   def ++(r: Result): Result
// }
//
// object Success extends Result {
//   def ++(r: Result): Result = r  
//   override def toString: String = "Compilation successful"
// }

// Fix error reporting
case class Failure(kind: ErrorKind, msg: String, isTest: Boolean) { // extends Result 
  // def ++(r: Result): Result = r match {
  //   case f: Failure   => 
  //     Failure(f.kind, s"${msg}\n${f.kind}: ${f.msg}", 
  //       errorNo + f.errorNo, warningNo + f.warningNo)
  //   case _            =>
  //     this
  // }

  def isError: Boolean = kind == Error

  // def errorFound(wno: Int, eno: Int): String = {
  //   def text(no: Int, msg: String): String = no match {
  //     case 0   => ""
  //     case 1   => s"one ${msg} found"
  //     case 2   => s"two ${msg}s found"
  //     case 3   => s"three ${msg}s found"
  //     case 4   => s"four ${msg}s found"
  //     case _   => s"${no} ${msg}s found"
  //   }
  //   s"${text(wno, "warning")}\n${text(eno, "error")}"
  // } 
  // TODO: Read SanaConfig if we are testing the compiler or not
  override def toString: String = 
    if(isTest) msg 
    else s"$kind: $msg"
    
    // msg + s"\n${errorFound(warningNo, errorNo)}\nCompilation Unsuccessful"
}

trait ErrorKind
case object Error extends ErrorKind {
  override def toString: String = "[error]"
}
case object Warning extends ErrorKind {
  override def toString: String = "[warning]"
}
