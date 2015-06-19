package ch.usi.inf.l3.sana.ooj.ast



import ch.usi.inf.l3.sana
import sana.ooj.types._
import sana.calcj
import sana.primj
import sana.ooj
import ooj.types.Types
import primj.contexts.TreeContexts
import ooj.util.Definitions


import scalaz.{Name => _, _}
import Scalaz._

trait Constants extends calcj.ast.Constants {
  self: Types with Trees with TreeContexts with Definitions =>

  /**************************** Extractors **************************/
  trait ConstantExtractor {
    def unapply(const: Constant): Option[(const.VType)] = Some(const.value)
  }

  /***************************** Factories **************************/
  trait NullFactory {
    private object NullConstImpl extends Constant {
      type VType                  = Null
      val value: VType            = null
      val tpe: TypeState[Type]    = toTypeState(NullType)
    }

    def instance: Constant = NullConstImpl
  }

  trait StringFactory {
    private class StringConstImpl(val value: String,
      val tpe: TypeState[Type]) extends Constant {
        type VType = String
      }

    def apply(value: String): Constant = {
      val tpe: TypeState[Type] = State {
        case ctx =>
          val id = ctx.findInThisContext(STRING_TYPE_NAME, _ => true)
          ctx.getTpe(id).run(ctx)
      }
      new StringConstImpl(value, tpe)
    }
  }


  val NullConstant: Constant = new NullFactory {}.instance
  val StringConstant  = new StringFactory with ConstantExtractor {}


  val DEFAULT_REFERENCE  = NullConstant

}


