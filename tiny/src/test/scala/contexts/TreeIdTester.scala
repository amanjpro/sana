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
    path.head should be (hd)
  }

  "path.forward" should s"$tl" in {
    path.forward should be (tl)
  }

  "path.up" should s"$up" in {
    path.up should be (up)
  }


  "tl.merge(hd)" should s"$path" in {
    tl.merge(hd) should be (path)
  }


  val path1 = TreeId(TreeId(NoId, 1), 4)
  val path2 = TreeId(NoId, 1)
  val res   = TreeId(TreeId(TreeId(NoId, 1), 1), 4)
  s"$path1 merge ($path2)" should s"$res" in {
    path1.merge(path2) should be (res)
  }



  val p1 = TreeId(TreeId(TreeId(NoId, 1), 2), 3)
  val p2 = TreeId(TreeId(TreeId(NoId, 4), 5), 6)
  val r2   = TreeId(TreeId(TreeId(
                      TreeId(TreeId(TreeId(NoId, 1), 2), 3), 4), 5), 6)

  s"$p1 concat ($p2)" should s"$r2" in {
    p1.concat(p2) should be (r2)
  }

  val parent = TreeId(TreeId(TreeId(NoId, 1), 2), 3)
  val kid    = 
    TreeId(TreeId(TreeId(TreeId(TreeId(TreeId(NoId, 1), 2), 3), 4), 5), 6)

  s"$parent contains $kid" should "true" in {
    parent.contains(kid) should be (true)
  }

  s"$kid contains $parent" should "false" in {
    kid.contains(parent) should be (false)
  }

  s"$parent isContainedIn $kid" should "false" in {
    parent.isContainedIn(kid) should be (false)
  }

  s"$kid isContainedIn $parent" should "true" in {
    kid.isContainedIn(parent) should be (true)
  }
}



