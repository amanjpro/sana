package ch.usi.inf.l3.sana.tiny.plugins



import ch.usi.inf.l3.sana.tiny
import tiny.Global
import tiny.report.Failure
import tiny.passes.Phases

abstract class Plugin(val global: Global) extends Phases {
  type G = Global

  def components: List[Phase]
}
