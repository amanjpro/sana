import ch.usi.inf.l3.sana
import sana.tiny.contexts._
import org.scalatest._

class TreeIdTester extends FlatSpec with Matchers {

  val (path, hd, up) = {
    val p1 = TreeId(NoId, 1)
    val p2 = TreeId(p1, 2)
    val p3 = TreeId(p2, 3)
    val p4 = TreeId(p3, 4)
    val p5 = TreeId(p4, 5)
    (p5, p1, p4)
  }
  
  val tl = {
    val p2 = TreeId(NoId, 2)
    val p3 = TreeId(p2, 3)
    val p4 = TreeId(p3, 4)
    val p5 = TreeId(p4, 5)
    p5
  }

  "path.head" should s"$hd" in {
    path.head == hd
  }

  "path.forward" should s"$tl" in {
    path.forward == tl
  }

  "path.up" should s"$up" in {
    path.up == up
  }


  "tl.merge(hd)" should s"$path" in {
    tl.merge(hd) == path
  }
}


