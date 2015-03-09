package ch.usi.inf.l3.sana.calcj.util


import ch.usi.inf.l3.sana
import sana.tiny.util


trait FlagSet extends util.FlagSet {
  val LOCAL_VARIABLE       = 0x00000001L
  val PARAM                = 0x00000002L
  val TERNARY              = 0x00000004L
  val FINAL                = 0x00000008L

  // val TERNARY              = 0x00000004L

}
