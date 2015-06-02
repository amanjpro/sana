package ch.usi.inf.l3.sana.tiny.passes

trait Scheduler {
  self: Phases =>


  
  class PhaseGraph

  // TODO: Finish implementing this
  // protected trait PTree {
  //   def insert(p: Phase): Either[String, List[Phase]]
  //   def toList: List[Phase]
  // }
  //

  protected case class PList(v: Phase, kid: Option[PList]) {

    def saneDependency(dep: String, plist: PList): Boolean = {
      plist.v.runRightAfter match {
        case Some(s) if dep == s => false
        case _                   => 
          plist.kid match {
            case None     => true
            case Some(k)  => saneDependency(dep, k)
          }
      }
    }

    def insert(o: Phase): Option[PList] = {
      o.runRightAfter match {
        case Some(dep)      =>
          if(! saneDependency(dep, this))
            None
          else if(dep == v.name)
            Some(PList(v, Some(PList(o, kid))))
          else if(v.runRightAfter == Some(o.name))
            Some(PList(o, Some(this)))
          else {
            val r = kid match {
              case None    => PList(v, Some(PList(o, None)))
              case Some(k) => PList(v, k.insert(o))
            }
            Some(r)
          }
        case _              => None
      }
    }

    private def insertAll(phases: List[Phase]): Option[PList] = {
      phases match {
        case Nil    => Some(this)
        case x::xs  =>
          insert(x) match {
            case None          => None
            case Some(r)       => r.insertAll(xs)
          }
      }
    }
    def merge(other: PList): Option[PList] = {
      insertAll(other.toList)
    }
    private def names: List[String] = toList.map(_.name)
    private def runBefores: List[String] = 
      toList.flatMap((x) => x.runRightAfter.toList)
    private def shouldRunRightBefore(dep: String): Boolean = 
      names.contains(dep)
    private def shouldRunRightAfter(dep: String): Boolean = 
      runBefores.contains(dep)
    def shouldContain(dep: String): Boolean =
      shouldRunRightBefore(dep) || shouldRunRightAfter(dep)
    def toList: List[Phase] = {
      v::(kid.toList.flatMap(_.toList))
    }
  }
  // protected case class Graph(v: Cluster, kids: List[PList]) {


    // def insert(p: Phase): Either[String, List[Cluster]] = {
    //   if(p.runRightAfter == v.name &&
    //     kids.filter(_.runRightAfter != v.name) == Nil) {
    //     Graph(v, List(Graph(p, kids)))
    //   } else if(p.runAfter == v.name) {
    //     Graph(v, p::kids)
    //
    // }

    // def toList: List[Phase] = {
    //   v::(kids.flatMap(_.toList))
    // }
  // }
  // protected class Leave(val v: Phase) extends PTree {
  //   def insert(p: Phase): Either[String, List[Phase]] = {
  //     
  //   }
  //   def toList: List[Phase] = List(v)
  // }
}



