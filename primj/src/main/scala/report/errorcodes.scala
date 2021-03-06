package ch.usi.inf.l3.sana.primj.report

import ch.usi.inf.l3.sana.tiny
import tiny.report.ErrorCode

case object DOUBLE_DEF extends ErrorCode {
  val message: String = "Name is already defined"
}
case object NAME_NOT_FOUND extends ErrorCode {
  val message: String = "Name not found"
}

case object VOID_VARIABLE_TYPE extends ErrorCode {
  val message: String = "Void type variable"
}

case object UNINITIALIZED_FINAL_VARIABLE extends ErrorCode {
  val message: String = "Final variable is uninitialized"
}

case object TYPE_NOT_FOUND extends ErrorCode {
  val message: String = "Type not found"
}


case object REASSIGNING_FINAL_VARIABLE extends ErrorCode {
  val message: String = "Reassigining to a final variable"
}

case object ASSIGNING_NOT_TO_VARIABLE extends ErrorCode {
  val message: String = "Assigning to a tree that is not a variable"
}


case object MISSING_RETURN_STATEMENT extends ErrorCode {
  val message: String = "Missing return statement"
}

case object VOID_RETURN extends ErrorCode {
  val message: String = "Missing expression"
}

case object NON_VOID_RETURN extends ErrorCode {
  val message: String = "Return cannot have an expression here"
}

case object TYPE_NAME_EXPECTED extends ErrorCode {
  val message: String = "A type name is expected"
}
