package ch.usi.inf.l3.sana.tiny.symbols



import ch.usi.inf.l3.sana.tiny
import tiny.types.Types
import tiny.names.Names
import tiny.source.Position
import Names._

trait Symbols {
  self: Types =>

  type Type = self.Type

  trait Symbol {
    def name: Name
    def tpe: Option[Type]
  }
}
